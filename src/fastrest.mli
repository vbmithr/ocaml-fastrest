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

type params = (string * string list) list

type auth_result = {
  params : params ;
  headers : Headers.t ;
}

type get
type post_form
type post_json
type put_form
type put_json
type delete

type _ meth =
  | Get : get meth
  | PostForm : post_form meth
  | PostJson : post_json meth
  | PutForm : put_form meth
  | PutJson : put_json meth
  | Delete : delete meth

type 'a error =
  | Http of Client_connection.error
  | App of 'a

val pp_print_error :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a error -> unit

type ('meth, 'ok, 'error) service = {
  meth : 'meth meth ;
  url : Uri.t ;
  encoding : ('ok, 'error) result Json_encoding.encoding ;
  params : params ;
  json_of_params : (params -> Ezjsonm.t) option ;
  auth : ('meth, 'ok, 'error) authf option ;
}

and ('meth, 'ok, 'error) authf =
  (('meth, 'ok, 'error) service -> auth -> auth_result)

val body_hdrs_of_service :
  ('meth, 'ok, 'error) service -> (Headers.t * string) option

val get :
  ?auth:(get, 'ok, 'error) authf ->
  ('ok, 'error) result Json_encoding.encoding -> Uri.t ->
  (get, 'ok, 'error) service

val post_form :
  ?auth:(post_form, 'ok, 'error) authf ->
  ?params:params ->
  ('ok, 'error) result Json_encoding.encoding -> Uri.t ->
  (post_form, 'ok, 'error) service

val post_json :
  ?auth:(post_json, 'ok, 'error) authf ->
  ?params:params ->
  ?json_of_params:(params -> Ezjsonm.t) ->
  ('ok, 'error) result Json_encoding.encoding -> Uri.t ->
  (post_json, 'ok, 'error) service

val put_form :
  ?auth:(put_form, 'ok, 'error) authf ->
  ?params:params ->
  ('ok, 'error) result Json_encoding.encoding -> Uri.t ->
  (put_form, 'ok, 'error) service

val put_json :
  ?auth:(put_json, 'ok, 'error) authf ->
  ?params:params ->
  ?json_of_params:(params -> Ezjsonm.t) ->
  ('ok, 'error) result Json_encoding.encoding -> Uri.t ->
  (put_json, 'ok, 'error) service

val delete :
  ?auth:(delete, 'ok, 'error) authf ->
  ('ok, 'error) result Json_encoding.encoding -> Uri.t ->
  (delete, 'ok, 'error) service

val request :
  ?auth:auth ->
  ('meth, 'ok, 'error) service ->
  ('ok, 'error error) result Deferred.t

val simple_call :
  ?headers:Headers.t ->
  ?body:string ->
  meth:Method.t -> Uri.t -> (Response.t * string Pipe.Reader.t) Deferred.t
