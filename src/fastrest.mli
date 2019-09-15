open Async
open Httpaf

module Ezjsonm_encoding : sig
  include module type of Json_encoding.Make(Json_repr.Ezjsonm)
  val destruct_safe : 'a Json_encoding.encoding -> Ezjsonm.value -> 'a
end

type auth = {
  key : string ;
  secret : string ;
  meta : (string * string) list ;
}

val auth :
  ?meta:(string * string) list ->
  key:string -> secret:string -> unit -> auth

type 'a error =
  | Http of Client_connection.error
  | App of 'a

val pp_print_error :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a error -> unit

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

val body_hdrs_of_service :
  ('params, 'ok, 'error) service -> (Headers.t * string) option

val get :
  ?auth:(form, 'ok, 'error) authf ->
  ('ok, 'error) result Json_encoding.encoding -> Uri.t ->
  (form, 'ok, 'error) service

val delete :
  ?auth:(form, 'ok, 'error) authf ->
  ('ok, 'error) result Json_encoding.encoding -> Uri.t ->
  (form, 'ok, 'error) service

val post_form :
  ?auth:(form, 'ok, 'error) authf ->
  ?params:(string * string list) list ->
  ('ok, 'error) result Json_encoding.encoding -> Uri.t ->
  (form, 'ok, 'error) service

val post_json :
  ?auth:(json, 'ok, 'error) authf ->
  params:'a Json_encoding.encoding * 'a ->
  ('ok, 'error) result Json_encoding.encoding -> Uri.t ->
  (json, 'ok, 'error) service

val put_form :
  ?auth:(form, 'ok, 'error) authf ->
  ?params:(string * string list) list ->
  ('ok, 'error) result Json_encoding.encoding -> Uri.t ->
  (form, 'ok, 'error) service

val put_json :
  ?auth:(json, 'ok, 'error) authf ->
  params:'a Json_encoding.encoding * 'a ->
  ('ok, 'error) result Json_encoding.encoding -> Uri.t ->
  (json, 'ok, 'error) service

val request :
  ?version:Async_ssl.Version.t ->
  ?options:Async_ssl.Opt.t list ->
  ?auth:auth ->
  ('params, 'ok, 'error) service ->
  ('ok, 'error error) result Deferred.t

val simple_call :
  ?headers:Headers.t ->
  ?body:string ->
  meth:Method.t -> Uri.t -> (Response.t * string Pipe.Reader.t) Deferred.t
