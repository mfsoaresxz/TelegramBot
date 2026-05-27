open Lwt
open Cohttp
open Cohttp\_lwt\_unix
open Yojson.Basic.Util

(\* ========================================== \*)
(\* 1. LÓGICA DE VALIDAÇÃO DE CPF              \*)
(\* ========================================== \*)

let char\_to\_int c = int\_of\_char c - int\_of\_char '0'

(\* Verifica se todos os números são iguais (ex: 111.111.111-11 é inválido) \*)
let all\_same s =
&#x20; let first = s.\[0] in
&#x20; let rec loop i =
&#x20;   if i = String.length s then true
&#x20;   else if s.\[i] <> first then false
&#x20;   else loop (i + 1)
&#x20; in loop 1