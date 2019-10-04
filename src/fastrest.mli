open Core
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

val body_hdrs_of_service : (_, _) service -> (Headers.t * string) option

val get :
  ?auth:(form, 'a) authf ->
  ('a, Error.t) result Json_encoding.encoding -> Uri.t ->
  (form, 'a) service

val delete :
  ?auth:(form, 'a) authf ->
  ('a, Error.t) result Json_encoding.encoding -> Uri.t ->
  (form, 'a) service

val post_form :
  ?auth:(form, 'a) authf ->
  ?params:(string * string list) list ->
  ('a, Error.t) result Json_encoding.encoding -> Uri.t ->
  (form, 'a) service

val post_json :
  ?auth:(json, 'a) authf ->
  params:'a Json_encoding.encoding * 'a ->
  ('a, Error.t) result Json_encoding.encoding -> Uri.t ->
  (json, 'a) service

val put_form :
  ?auth:(form, 'a) authf ->
  ?params:(string * string list) list ->
  ('a, Error.t) result Json_encoding.encoding -> Uri.t ->
  (form, 'a) service

val put_json :
  ?auth:(json, 'a) authf ->
  params:'a Json_encoding.encoding * 'a ->
  ('a, Error.t) result Json_encoding.encoding -> Uri.t ->
  (json, 'a) service

val request :
  ?version:Async_ssl.Version.t ->
  ?options:Async_ssl.Opt.t list ->
  ?auth:auth ->
  ('params, 'a) service ->
  'a Deferred.Or_error.t

val simple_call :
  ?headers:Headers.t ->
  ?body:string ->
  meth:Method.t -> Uri.t ->
  (Response.t * string Pipe.Reader.t) Deferred.t
