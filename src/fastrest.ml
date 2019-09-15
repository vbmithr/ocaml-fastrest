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
module Log = (val Logs.src_log src : Logs.LOG)
module Log_async = (val Logs_async.src_log src : Logs_async.LOG)

type 'a error =
  | Http of Client_connection.error
  | App of 'a

let pp_cc_error ppf = function
  | `Exn e ->
    Format.fprintf ppf "Exn %a" Exn.pp e
  | `Invalid_response_body_length r ->
    Format.fprintf ppf "Invalid_response_body_length %a" Response.pp_hum r
  | `Malformed_response s ->
    Format.fprintf ppf "Malformed_response %s" s

let pp_print_error pp_error ppf = function
  | Http e -> Format.fprintf ppf "Http %a" pp_cc_error e
  | App e -> Format.fprintf ppf "App %a" pp_error e

type auth = {
  key : string ;
  secret : string ;
  meta : (string * string) list ;
}

let auth ?(meta=[]) ~key ~secret () = { meta ; key ; secret }

let json_of_params enc v = Json_encoding.construct enc v

type form
type json
type _ params =
  | Form : (string * string list) list -> form params
  | Json : 'a Json_encoding.encoding * 'a -> json params

type 'a auth_result = {
  params : 'a params ;
  headers : Headers.t ;
}

type ('params, 'ok, 'error) service = {
  meth : Method.t ;
  url : Uri.t ;
  encoding : ('ok, 'error) result Json_encoding.encoding ;
  params : 'params params ;
  auth : ('params, 'ok, 'error) authf option ;
}

and ('params, 'ok, 'error) authf =
  (('params, 'ok, 'error) service -> auth -> 'params auth_result)

let get ?auth encoding url =
  { meth = `GET ; url ; encoding ; params = Form (Uri.query url) ; auth }

let delete ?auth encoding url =
  { meth = `DELETE ; url ; encoding ; params = Form (Uri.query url) ; auth }

let post_form ?auth ?(params=[]) encoding url =
  { meth = `POST ; url ; encoding ; params = Form params; auth }

let post_json ?auth ~params:(enc, v) encoding url =
  { meth = `POST ; url ; encoding ; params = Json (enc, v); auth }

let put_form ?auth ?(params=[]) encoding url =
  { meth = `PUT ; url ; encoding ; params = Form params; auth }

let put_json ?auth ~params:(enc, v) encoding url =
  { meth = `PUT ; url ; encoding ; params = Json (enc, v); auth }

let body_hdrs_of_service :
  type a. (a, 'ok, 'error) service -> (Headers.t * string) option = fun srv ->
  match srv.meth with
  | `POST | `PUT -> begin
      match srv.params with
      | Form params ->
        let str = Uri.encoded_of_query params in
        let hdrs =
          Headers.of_list [
            "Content-Type", "application/x-www-form-urlencoded" ;
            "Content-Length", string_of_int (String.length str) ;
          ] in
        Some (hdrs, str)
      | Json (e, a) ->
        let str = Ezjsonm.value_to_string (json_of_params e a) in
        let hdrs =
          Headers.of_list [
            "Content-Type", "application/json" ;
            "Content-Length", string_of_int (String.length str) ;
          ] in
        Some (hdrs, str) end
  | #Method.t -> None

let write_iovec w iovec =
  List.fold_left iovec ~init:0 ~f:begin fun a { Faraday.buffer ; off ; len } ->
    Writer.write_bigstring w buffer ~pos:off ~len ;
    a+len
  end

let rec flush_req conn w =
  match Client_connection.next_write_operation conn with
  | `Write iovec ->
    let nb_read = write_iovec w iovec in
    Client_connection.report_write_result conn (`Ok nb_read) ;
    flush_req conn w
  | `Yield ->
    Client_connection.yield_writer conn (fun () -> flush_req conn w) ;
  | `Close _ -> ()

let read_response conn r =
  Log_async.debug (fun m -> m "read_response") >>= fun () ->
  match Client_connection.next_read_operation conn with
  | `Close -> Deferred.unit
  | `Read -> begin
      Reader.read_one_chunk_at_a_time r
        ~handle_chunk:begin fun buf ~pos ~len ->
          let nb_read = Client_connection.read conn buf ~off:pos ~len in
          return (`Consumed (nb_read, `Need_unknown))
        end >>= function
      | `Stopped () -> assert false (* cannot happen *)
      | `Eof -> Deferred.unit (* normal case *)
      | `Eof_with_unconsumed_data _ ->
        Log_async.err (fun m -> m "Got EOF with unconsumed data")
    end

let request :
  type params.
  ?version:Async_ssl.Version.t ->
  ?options:Async_ssl.Opt.t list ->
  ?auth:auth ->
  (params, 'ok, 'error) service ->
  ('ok, 'error error) result Deferred.t =
  fun ?version ?options ?auth service ->
  let error_iv = Ivar.create () in
  let resp_iv = Ivar.create () in
  let error_handler err =
    Ivar.fill error_iv (Error (Http err))
  in
  let response_handler response body =
    Log.debug (fun m -> m "%a" Response.pp_hum response) ;
    let buffer = Bigbuffer.create 32 in
    let on_eof () =
      let buf_str = Bigbuffer.contents buffer in
      Log.debug (fun m -> m "%s" buf_str) ;
      let resp_json = Ezjsonm.from_string buf_str in
      match Ezjsonm_encoding.destruct_safe
              service.encoding resp_json with
      | Error e -> Ivar.fill error_iv (Error (App e))
      | Ok v -> Ivar.fill resp_iv (Ok v)
    in
    let rec on_read buf ~off ~len =
      let str = Bigstringaf.sub buf ~off ~len in
      Bigbuffer.add_bigstring buffer str ;
      Body.schedule_read body ~on_eof ~on_read
    in
    Body.schedule_read body ~on_eof ~on_read
  in
  let req = Request.create service.meth (Uri.path_and_query service.url) in
  let req, service = match req.meth, service.auth, auth with
    | _, _, None -> req, service
    | _, None, _ -> req, service
    | `PUT, Some authf, Some auth
    | `POST, Some authf, Some auth ->
      let { params ; headers } = authf service auth in
      { req with headers = Headers.(add_list req.headers (to_list headers)) },
      { service with params }
    | `DELETE, Some authf, Some auth
    | `GET, Some authf, Some auth ->
      let { params ; headers } = authf service auth in
      let ps = match params with
        | Form ps -> ps
        | _ -> assert false in
      { req with
        headers = Headers.(add_list req.headers (to_list headers)) ;
        target = Uri.(path_and_query (with_query service.url ps)) ;
      }, service
    | _ -> invalid_arg "unsupported method"
  in
  let headers = Headers.add_list req.headers [
      "User-Agent", "ocaml-fastrest" ;
      "Host", Uri.host_with_default ~default:"" service.url ;
    ] in
  let headers, params_str =
    match body_hdrs_of_service service with
    | None -> headers, None
    | Some (hdrs, params_str) ->
      Headers.(add_list headers (to_list hdrs)), Some params_str in
  let req = { req with headers } in
  Log_async.debug (fun m -> m "%a" Request.pp_hum req) >>= fun () ->
  Async_uri.with_connection ?version ?options service.url begin fun _sock _conn r w ->
    let body, conn =
      Client_connection.request req ~error_handler ~response_handler in
    begin
      match params_str with
      | Some ps ->
        Log.debug (fun m -> m "%s" ps) ;
        Body.write_string body ps
      | _ -> ()
    end ;
    flush_req conn w ;
    don't_wait_for (read_response conn r) ;
    Deferred.any [Ivar.read resp_iv ;
                  Ivar.read error_iv]
  end

let error_handler = function
  | `Exn exn ->
    Log.err (fun m -> m "%a" Exn.pp exn)
  | `Invalid_response_body_length r ->
    Log.err (fun m -> m "Invalid_response_body_length %a" Response.pp_hum r)
  | `Malformed_response r ->
    Log.err (fun m -> m "Malformed response %s" r)

let simple_call ?(headers=Headers.empty) ?body ~meth url =
  let resp_iv = Ivar.create () in
  let pr, pw = Pipe.create () in
  let response_handler response body =
    Log.debug (fun m -> m "%a" Response.pp_hum response) ;
    Ivar.fill_if_empty resp_iv response ;
    let on_eof () = Pipe.close pw in
    let rec on_read buf ~off ~len =
      Pipe.write_without_pushback_if_open pw (Bigstringaf.substring buf ~off ~len) ;
      Body.schedule_read body ~on_eof ~on_read
    in
    Body.schedule_read body ~on_eof ~on_read in
  let headers = Headers.add_list headers [
      "User-Agent", "ocaml-fastrest" ;
      "Host", Uri.host_with_default ~default:"" url ;
    ] in
  let headers = match body with
    | None -> headers
    | Some body ->
      Headers.add headers "Content-Length"
        (Int.to_string (String.(length body))) in
  Async_uri.connect url >>= fun (_sock, _conn, r, w) ->
  let req = Request.create ~headers meth (Uri.path_and_query url) in
  let body_writer, conn =
    Client_connection.request req ~error_handler ~response_handler in
  Log_async.debug (fun m -> m "%a" Request.pp_hum req) >>= fun () ->
  begin
    match body with
    | None -> ()
    | Some body ->
      Log.debug (fun m -> m "%s" body) ;
      Body.write_string body_writer body
  end ;
  flush_req conn w ;
  don't_wait_for (read_response conn r) ;
  don't_wait_for begin Pipe.closed pw >>= fun () ->
    Reader.close r >>= fun () ->
    Writer.close w
  end ;
  Ivar.read resp_iv >>= fun resp ->
  return (resp, pr)
