open Core
open Async

let wrap_request
    ?(timeout=Time.Span.of_int_sec 1)
    ?(speed=`Quick) n service =
  Alcotest_async.test_case ~timeout n speed begin fun () ->
    (Fastrest.request service) |>
    Deferred.Or_error.ignore |>
    Deferred.Or_error.ok_exn
  end

let wrap_request_light
    ?(timeout=Time.Span.of_int_sec 1)
    ?(speed=`Quick) n f =
  Alcotest_async.test_case ~timeout n speed begin fun () ->
    f () |>
    Deferred.Or_error.ignore |>
    Deferred.Or_error.ok_exn
  end

let dummy_service =
  Fastrest.get Json_encoding.(conv (fun _ -> assert false) (fun _ -> Ok ()) unit)
    (Uri.of_string "https://api.bitfinex.com/v1/symbols")

let basic = [
  wrap_request_light "simple" begin fun () ->
    Fastrest.simple_call_string ~meth:`GET
      (Uri.make ~scheme:"http" ~host:"www.google.com" ()) >>= fun _ ->
    Deferred.Or_error.return ()
  end ;
  (* wrap_request "dummy_service" dummy_service *)
]

let () =
  Logs.set_level (Some Debug) ;
  Logs.set_reporter (Logs_async_reporter.reporter ()) ;
  Alcotest.run ~and_exit:false "fastrest" [
    "basic", basic ;
  ]
