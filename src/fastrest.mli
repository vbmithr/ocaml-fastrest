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

type auth_result = {
  params : (string * string list) list ;
  headers : Headers.t ;
}

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

val body_hdrs_of_service :
  ('meth, 'ok, 'error) service -> (Headers.t * string) option

val get :
  ?auth:(get, 'ok, 'error) authf ->
  ?params:(string * string list) list ->
  ('ok, 'error) result Json_encoding.encoding -> Uri.t ->
  (get, 'ok, 'error) service

val post_form :
  ?auth:(post_form, 'ok, 'error) authf ->
  ?params:(string * string list) list ->
  ('ok, 'error) result Json_encoding.encoding -> Uri.t ->
  (post_form, 'ok, 'error) service

val post_json :
  ?auth:(post_json, 'ok, 'error) authf ->
  ?params:(string * string list) list ->
  ('ok, 'error) result Json_encoding.encoding -> Uri.t ->
  (post_json, 'ok, 'error) service

val request :
  ?auth:auth ->
  ('meth, 'ok, 'error) service ->
  ('ok, 'error error) result Deferred.t

val simple_call :
  ?headers:Headers.t ->
  ?body:string ->
  meth:Method.t -> Uri.t -> (Response.t * string option) Deferred.t
