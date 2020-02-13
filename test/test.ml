open Core
open Async
open Alcotest_async

let wrap_request
    ?(timeout=Time.Span.of_int_sec 1)
    ?(speed=`Quick) n service =
  test_case ~timeout n speed begin fun () ->
    (Fastrest.request service)
  end

let wrap_request_light
    ?(timeout=Time.Span.of_int_sec 1)
    ?(speed=`Quick) n f =
  test_case ~timeout n speed begin fun () ->
    f ()
  end

let symbols =
  Fastrest.get Json_encoding.(conv (fun _ -> assert false) (fun _ -> Ok ()) unit)
    (Uri.of_string "https://api.bitfinex.com/v1/symbols")

let tickers =
  Fastrest.get Json_encoding.(conv (fun _ -> assert false) (fun _ -> Ok ()) unit)
    (Uri.of_string "https://api-pub.bitfinex.com/v2/tickers?symbols=ALL")

let basic = [
  wrap_request_light "simple" begin fun () ->
    Fastrest.simple_call_string ~meth:`GET
      (Uri.make ~scheme:"http" ~host:"www.google.com" ()) |>
    Deferred.ignore_m
  end ;
  wrap_request "bfx_symbols" symbols ;
  wrap_request "bfx_tickers" tickers ;
]

let main () =
  run "fastrest" [
    "basic", basic ;
  ]

let () =
  don't_wait_for (main ()) ;
  never_returns (Scheduler.go ())
