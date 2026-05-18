open Lwt
open Cohttp
open Cohttp_lwt_unix
open Yojson.Basic.Util

(* ========================================== *)
(* 1. LÓGICA DE VALIDAÇÃO DE CPF              *)
(* ========================================== *)

let char_to_int c = int_of_char c - int_of_char '0'

(* Verifica se todos os números são iguais (ex: 111.111.111-11 é inválido) *)
let all_same s =
  let first = s.[0] in
  let rec loop i =
    if i = String.length s then true
    else if s.[i] <> first then false
    else loop (i + 1)
  in loop 1

let validate_cpf cpf_input =
  (* Filtra apenas os números da string (ignora pontos e traços) *)
  let clean = 
    String.to_seq cpf_input 
    |> Seq.filter (fun c -> c >= '0' && c <= '9') 
    |> String.of_seq 
  in
  
  if String.length clean <> 11 || all_same clean then false
  else
    let digits = Array.init 11 (fun i -> char_to_int clean.[i]) in
    
    (* Função auxiliar para calcular o dígito verificador *)
    let calc_digit max_weight =
      let rec loop i weight sum =
        if weight < 2 then
          let rem = sum mod 11 in
          if rem < 2 then 0 else 11 - rem
        else loop (i + 1) (weight - 1) (sum + digits.(i) * weight)
      in loop 0 max_weight 0
    in
    
    let d1 = calc_digit 10 in
    let d2 = calc_digit 11 in
    digits.(9) = d1 && digits.(10) = d2


(* ========================================== *)
(* 2. LÓGICA DO BOT NO TELEGRAM               *)
(* ========================================== *)

let token = "8619111069:AAG4X9IIhdzc67xAmGQJ3cYcTgwuj-5hUtM"
let api_url = Printf.sprintf "t.me/TCamlBot" token

(* Função para enviar mensagem de volta ao usuário *)
let send_message chat_id text =
  let uri_str = Printf.sprintf "%s/sendMessage?chat_id=%d&text=%s" 
      api_url chat_id (Uri.pct_encode text) in
  Client.get (Uri.of_string uri_str) >>= fun (_, body) ->
  Cohttp_lwt.Body.drain_body body

(* Processa a mensagem recebida *)
let process_message msg =
  try
    let message = msg |> member "message" in
    let chat_id = message |> member "chat" |> member "id" |> to_int in
    let text = message |> member "text" |> to_string in
    
    (* Verifica o texto recebido e responde de acordo *)
    let reply = 
      if validate_cpf text then 
        "✅ O CPF informado é VÁLIDO." 
      else 
        "❌ O CPF informado é INVÁLIDO." 
    in
    send_message chat_id reply
  with 
  | _ -> Lwt.return_unit (* Ignora erros como mensagens de áudio, stickers, etc. *)

(* Loop contínuo (Long Polling) para buscar novas mensagens *)
let rec poll offset =
  let uri_str = Printf.sprintf "%s/getUpdates?offset=%d&timeout=10" api_url offset in
  Client.get (Uri.of_string uri_str) >>= fun (_, body) ->
  Cohttp_lwt.Body.to_string body >>= fun body_str ->
  
  let json = Yojson.Basic.from_string body_str in
  let results = json |> member "result" |> to_list in
  
  let next_offset = List.fold_left (fun acc msg ->
    let update_id = msg |> member "update_id" |> to_int in
    (* Usa Lwt.async para processar a mensagem sem travar o loop *)
    Lwt.async (fun () -> process_message msg);
    max acc (update_id + 1)
  ) offset results in
  
  poll next_offset

(* Ponto de entrada do programa *)
let () =
  print_endline "Bot iniciado e escutando mensagens...";
  Lwt_main.run (poll 0)