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

module Client_connection = struct
  include Client_connection
  let pp_error ppf = function
    | `Exn e ->
      Format.fprintf ppf "Exn %a" Exn.pp e
    | `Invalid_response_body_length r ->
      Format.fprintf ppf "Invalid_response_body_length %a" Response.pp_hum r
    | `Malformed_response s ->
      Format.fprintf ppf "Malformed_response %s" s
end

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

type ('params, 'a) service = {
  meth : Method.t ;
  url : Uri.t ;
  encoding : ('a, Error.t) result Json_encoding.encoding ;
  params : 'params params ;
  auth : ('params, 'a) authf option ;
}

and ('params, 'a) authf =
  (('params, 'a) service -> auth -> 'params auth_result)

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
  type a. (a, 'b) service -> (Headers.t * string) option = fun srv ->
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
  match Client_connection.next_read_operation conn with
  | `Close -> ()
  | `Read ->
    Reader.read_one_chunk_at_a_time r ~handle_chunk:begin fun buf ~pos ~len ->
      let nb_read = Client_connection.read conn buf ~off:pos ~len in
      return (`Consumed (nb_read, `Need_unknown))
    end >>> function
    | `Stopped () -> assert false
    | `Eof ->
      ignore (Client_connection.read_eof conn (Bigstring.of_string "") ~off:0 ~len:0)
    | `Eof_with_unconsumed_data buf ->
      ignore (Client_connection.read_eof conn
                (Bigstring.of_string buf) ~off:0 ~len:(String.length buf))

let read_body ?(bufsize=512) body =
  let buf = Bigbuffer.create bufsize in
  let read_finished = Ivar.create () in
  let on_eof () =
    Ivar.fill read_finished () in
  let rec on_read b ~off:pos ~len =
    Bigbuffer.add_bigstring buf (Bigstring.sub_shared b ~pos ~len) ;
    Body.schedule_read body ~on_eof ~on_read
  in
  Body.schedule_read body ~on_eof ~on_read ;
  Ivar.read read_finished >>| fun () ->
  Bigbuffer.contents buf

let error_handler error_iv err =
  Ivar.fill error_iv
    (Error.of_string
       (Format.asprintf "%a" Client_connection.pp_error err))

let request :
  type params.
  ?version:Async_ssl.Version.t ->
  ?options:Async_ssl.Opt.t list ->
  ?buffer_age_limit:[ `At_most of Time.Span.t | `Unlimited ] ->
  ?interrupt:unit Deferred.t ->
  ?reader_buffer_size:int ->
  ?writer_buffer_size:int ->
  ?timeout:Time.Span.t ->
  ?config:Config.t ->
  ?auth:auth ->
  (params, 'a) service -> 'a Deferred.t =
  fun ?version ?options ?buffer_age_limit ?interrupt
    ?reader_buffer_size ?writer_buffer_size ?timeout ?config ?auth service ->
    let resp_iv = Ivar.create () in
    let error_iv = Ivar.create () in

    let response_handler service ~error_iv ~resp_iv response body =
      Log.debug (fun m -> m "%a" Response.pp_hum response) ;
      read_body body >>> fun buf_str ->
      Log.debug (fun m -> m ">>> %s" buf_str) ;
      let resp_json = Ezjsonm.from_string buf_str in
      match Ezjsonm_encoding.destruct_safe service.encoding resp_json with
      | Error e -> Ivar.fill error_iv e
      | Ok v -> Ivar.fill resp_iv v in

    let headers = Headers.of_list ["accept", "application/json"] in
    let req = Request.create ~headers service.meth (Uri.path_and_query service.url) in
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
        "Connection", "close" ;
      ] in
    let headers, params_str =
      match body_hdrs_of_service service with
      | None -> headers, None
      | Some (hdrs, params_str) ->
        Headers.(add_list headers (to_list hdrs)), Some params_str in
    let req = { req with headers } in
    Log_async.debug (fun m -> m "%a" Request.pp_hum req) >>= fun () ->
    Async_uri.with_connection
      ?version ?options ?buffer_age_limit ?interrupt
      ?reader_buffer_size ?writer_buffer_size ?timeout
      service.url begin fun _sock _conn r w ->
      let body, conn =
        Client_connection.request ?config req
          ~error_handler:(error_handler error_iv)
          ~response_handler:(response_handler service ~resp_iv ~error_iv) in
      begin
        match params_str with
        | Some ps ->
          Log.debug (fun m -> m "%s" ps) ;
          Body.write_string body ps
        | _ -> ()
      end ;
      flush_req conn w ;
      read_response conn r ;
      Deferred.any [(Ivar.read resp_iv >>= Deferred.Or_error.return) ;
                    (Ivar.read error_iv >>= Deferred.Or_error.fail)] |>
      Deferred.Or_error.ok_exn
    end

let error_handler = function
  | `Exn exn ->
    Log.err (fun m -> m "%a" Exn.pp exn)
  | `Invalid_response_body_length r ->
    Log.err (fun m -> m "Invalid_response_body_length %a" Response.pp_hum r)
  | `Malformed_response r ->
    Log.err (fun m -> m "Malformed response %s" r)

let simple_call
    ?version ?options ?socket ?buffer_age_limit ?interrupt
    ?reader_buffer_size ?writer_buffer_size ?timeout
    ?config ?(headers=Headers.empty) ?body ~meth url =
  let resp_iv = Ivar.create () in
  let pr, pw = Pipe.create () in

  let response_handler response body =
    Log.debug (fun m -> m "%a" Response.pp_hum response) ;
    Ivar.fill_if_empty resp_iv response ;
    let on_eof () = Pipe.close pw in
    let on_read buf ~off ~len =
      Pipe.write_without_pushback_if_open pw
        (Bigstringaf.substring buf ~off ~len) in
    Body.schedule_read body ~on_eof ~on_read in

  let headers = Headers.add_list headers [
      "Host", Uri.host_with_default ~default:"" url ;
      "User-Agent", "ocaml-fastrest" ;
    ] in
  let headers = match body with
    | None -> headers
    | Some body ->
      Headers.add headers "Content-Length"
        (Int.to_string (String.(length body))) in
  Async_uri.connect
    ?version ?options ?socket ?buffer_age_limit ?interrupt
    ?reader_buffer_size ?writer_buffer_size ?timeout url >>= fun (_sock, _conn, r, w) ->
  let req = Request.create ~headers meth (Uri.path_and_query url) in
  let body_writer, conn =
    Client_connection.request ?config req ~error_handler ~response_handler in
  Log_async.debug (fun m -> m "%a" Request.pp_hum req) >>= fun () ->
  Option.iter body ~f:begin fun body ->
    Log.debug (fun m -> m "%s" body) ;
    Body.write_string body_writer body
  end ;
  Body.close_writer body_writer ;
  flush_req conn w ;
  read_response conn r ;
  don't_wait_for begin Pipe.closed pw >>= fun () ->
    Reader.close r >>= fun () ->
    Writer.close w
  end ;
  Ivar.read resp_iv >>= fun resp ->
  return (resp, pr)

let simple_call_string
    ?version ?options ?buffer_age_limit ?interrupt
    ?reader_buffer_size ?writer_buffer_size ?timeout
    ?config ?(headers=Headers.empty) ?body ~meth url =
  let resp_iv = Ivar.create () in
  let response_handler response body =
    Log.debug (fun m -> m "%a" Response.pp_hum response) ;
    read_body body >>> fun body ->
    Log.debug (fun m -> m ">>> %s" body) ;
    Ivar.fill resp_iv (response, body) in
  let headers = Headers.add_list headers [
      "Host", Uri.host_with_default ~default:"" url ;
      "User-Agent", "ocaml-fastrest" ;
      "Connection", "close" ;
    ] in
  let headers = match body with
    | None -> headers
    | Some body ->
      Headers.add headers "Content-Length"
        (Int.to_string (String.(length body))) in
  Async_uri.with_connection
    ?version ?options ?buffer_age_limit ?interrupt
    ?reader_buffer_size ?writer_buffer_size ?timeout url begin fun _sock _conn r w ->
    let req = Request.create ~headers meth (Uri.path_and_query url) in
    let body_writer, conn =
      Client_connection.request ?config req ~error_handler ~response_handler in
    Log_async.debug (fun m -> m "%a" Request.pp_hum req) >>= fun () ->
    Option.iter body ~f:begin fun body ->
      Log.debug (fun m -> m "%s" body) ;
      Body.write_string body_writer body
    end ;
    Body.close_writer body_writer ;
    flush_req conn w ;
    read_response conn r ;
    Ivar.read resp_iv
  end
