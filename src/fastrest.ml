open Core
open Async
open Httpaf

module Ezjsonm_encoding = struct
  include Json_encoding.Make(Json_repr.Ezjsonm)

  let destruct_safe encoding value =
    try destruct encoding value with exn ->
      Format.eprintf "%a@."
        (Json_encoding.print_error ?print_unknown:None) exn ;
      raise exn
end

let src = Logs.Src.create "fastrest"

type get
type post_form
type post_json

type _ meth =
  | Get : get meth
  | PostForm : post_form meth
  | PostJson : post_json meth

type 'a error =
  | Http of Client_connection.error
  | App of 'a

type auth = {
  key : string ;
  secret : string ;
}

type auth_result = {
  params : (string * string list) list ;
  headers : Headers.t ;
}

let ezjsonm_of_params params =
  let fields = List.map params ~f:begin fun (k, vs) ->
      k, `String (String.concat vs)
    end in
  `O fields

type ('meth, 'ok, 'error) service = {
  meth : 'meth meth ;
  url : Uri.t ;
  req : Request.t ;
  encoding : ('ok, 'error) result Json_encoding.encoding ;
  params : (string * string list) list ;
  auth : ('meth, 'ok, 'error) authf option ;
}

and ('meth, 'ok, 'error) authf =
  (('meth, 'ok, 'error) service -> auth -> auth_result)

let get ?auth ?(params=[]) encoding url =
  let target = Uri.path_and_query url in
  let req = Request.create `GET target in
  { meth = Get ; url ; req ; encoding ; params ; auth }

let post_form ?auth ?(params=[]) encoding url =
  let target = Uri.path_and_query url in
  let req = Request.create `POST target in
  { meth = PostForm ; url ; req ; encoding ; params ; auth }

let post_json ?auth ?(params=[]) encoding url =
  let target = Uri.path_and_query url in
  let req = Request.create `POST target in
  { meth = PostJson ; url ; req ; encoding ; params ; auth }

let write_iovec w iovec =
  List.fold_left iovec ~init:0 ~f:begin fun a { Faraday.buffer ; off ; len } ->
    Writer.write_bigstring w buffer ~pos:off ~len ;
    a+len
  end

let request (type meth) ?auth (service : (meth, 'ok, 'error) service) =
  let error_iv = Ivar.create () in
  let resp_iv = Ivar.create () in
  let error_handler err =
    Ivar.fill error_iv (Error (Http err))
  in
  let response_handler response body =
    Logs.debug ~src (fun m -> m "%a" Response.pp_hum response) ;
    match response with
    | { Response.status = `OK ; _ } ->
      let buffer = Buffer.create 32 in
      let on_eof () =
        let buf_str = Buffer.contents buffer in
        Logs.debug ~src (fun m -> m "%s" buf_str) ;
        let resp_json = Ezjsonm.from_string buf_str in
        match Ezjsonm_encoding.destruct_safe
                service.encoding resp_json with
        | Error e -> Ivar.fill error_iv (Error (App e))
        | Ok v -> Ivar.fill resp_iv (Ok v)
      in
      let rec on_read buf ~off ~len =
        Buffer.add_string buffer (Bigstringaf.substring buf ~off ~len) ;
        Body.schedule_read body ~on_eof ~on_read
      in
      Body.schedule_read body ~on_eof ~on_read
    | _ ->
      Logs.err ~src (fun m -> m "Error response")
  in
  let params, headers = match service.meth, service.auth, auth with
    | _, _, None -> service.params, service.req.headers
    | Get, _, _ -> service.params, service.req.headers
    | _, None, _ -> service.params, service.req.headers
    | _, Some authf, Some auth ->
      let { params ; headers } = authf service auth in
      params @ service.params,
      Headers.(add_list service.req.headers (to_list headers)) in
  let headers = Headers.add_list headers [
      "User-Agent", "ocaml-fastrest" ;
      "Host", Uri.host_with_default ~default:"" service.url ;
    ] in
  let headers, params_str = match service.meth with
    | Get -> headers, None
    | PostForm ->
      let params_str = Uri.encoded_of_query params in
      Headers.add_list headers [
        "Content-Type", "application/x-www-form-urlencoded" ;
        "Content-Length", string_of_int (String.length params_str) ;
      ], Some params_str
    | PostJson ->
      let params_str = Ezjsonm.to_string (ezjsonm_of_params params) in
      Headers.add_list headers [
        "Content-Type", "application/json" ;
        "Content-Length", string_of_int (String.length params_str) ;
      ], Some params_str
  in
  let req = { service.req with headers } in
  Conduit_async.V3.with_connection_uri service.url begin fun _ r w ->
    let body, conn =
      Client_connection.request req ~error_handler ~response_handler in
    let rec flush_req () =
      match Client_connection.next_write_operation conn with
      | `Write iovec ->
        let nb_read = write_iovec w iovec in
        Client_connection.report_write_result conn (`Ok nb_read) ;
        flush_req ()
      | `Yield ->
        Client_connection.yield_writer conn flush_req ;
      | `Close _ -> () in
    let rec read_response () =
      match Client_connection.next_read_operation conn with
      | `Close -> Deferred.unit
      | `Read -> begin
          Reader.read_one_chunk_at_a_time r
            ~handle_chunk:begin fun buf ~pos ~len ->
              let nb_read = Client_connection.read conn buf ~off:pos ~len in
              return (`Stop_consumed ((), nb_read))
            end >>= function
          | `Eof -> Deferred.unit
          | `Eof_with_unconsumed_data _ -> Deferred.unit
          | `Stopped () -> read_response ()
        end in
    Logs_async.debug ~src
      (fun m -> m "%a" Request.pp_hum req) >>= fun () ->
    begin
      match params_str with
      | Some ps ->
        Logs.debug ~src (fun m -> m "%s" ps) ;
        Body.write_string body ps
      | _ -> ()
    end ;
    flush_req () ;
    don't_wait_for (read_response ()) ;
    Deferred.any [Ivar.read resp_iv ;
                  Ivar.read error_iv]
  end
