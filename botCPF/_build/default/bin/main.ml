open Lwt
open Cohttp
open Cohttp_lwt_unix

let token = "8930331882:AAHaFcQINepocSFCarfME8ybpUSx4flVEOA"
let base_url = "https://api.telegram.org/bot" ^ token

let apenas_digitos s =
  String.concat "" (List.map (String.make 1)
    (List.filter (fun c -> c >= '0' && c <= '9')
      (List.init (String.length s) (String.get s))))

let valida_cpf cpf_raw =
  let cpf = apenas_digitos cpf_raw in
  if String.length cpf <> 11 then `Invalido "CPF deve ter 11 digitos"
  else
    let todos_iguais =
      let c = cpf.[0] in
      String.for_all (fun x -> x = c) cpf
    in
    if todos_iguais then `Invalido "Digitos todos iguais"
    else
      let digito i = Char.code cpf.[i] - 48 in
      (* Primeiro digito verificador *)
      let soma1 = ref 0 in
      for i = 0 to 8 do
        soma1 := !soma1 + (digito i) * (10 - i)
      done;
      let d1 = ((!soma1 * 10) mod 11) mod 10 in
      (* Segundo digito verificador *)
      let soma2 = ref 0 in
      for i = 0 to 9 do
        soma2 := !soma2 + (digito i) * (11 - i)
      done;
      let d2 = ((!soma2 * 10) mod 11) mod 10 in
      if d1 = digito 9 && d2 = digito 10 then `Valido
      else `Invalido "Digitos verificadores incorretos"
      
let formata_cpf cpf =
  let d = apenas_digitos cpf in
  if String.length d = 11 then
    Printf.sprintf "%s.%s.%s-%s"
      (String.sub d 0 3)
      (String.sub d 3 3)
      (String.sub d 6 3)
      (String.sub d 9 2)
  else d

let get_updates offset =
  let url = Uri.of_string (base_url ^ "/getUpdates?timeout=10&offset=" ^ string_of_int offset) in
  Client.get url >>= fun (_, body) ->
  Cohttp_lwt.Body.to_string body

let send_message chat_id text =
  let url = Uri.of_string (base_url ^ "/sendMessage") in
  let params = `Assoc [
    ("chat_id", `Int chat_id);
    ("text", `String text)
  ] in
  let body = Yojson.Safe.to_string params in
  let headers = Header.init_with "Content-Type" "application/json" in
  Client.post ~headers ~body:(Cohttp_lwt.Body.of_string body) url >>= fun (_, body) ->
  Cohttp_lwt.Body.to_string body

let handle_message message =
  let open Yojson.Safe.Util in
  let chat_id = message |> member "chat" |> member "id" |> to_int in
  let text =
    match message |> member "text" with
    | `String t -> t
    | _ -> ""
  in
  Printf.printf "Mensagem recebida: %s\n%!" text;
  let resposta =
    match text with
    | "/start" ->
        "Ola! Eu sou o ValCPF Bot\n\nEnvie um CPF para validar!\n\nExemplos:\n- 123.456.789-09\n- 12345678909\n- 123-456-789-09"
    | "/ajuda" ->
        "Como usar:\nEnvie qualquer CPF (com ou sem pontuacao) e eu digo se e valido!\n\n/start - Mensagem inicial\n/ajuda - Esta mensagem"
    | cpf ->
        let digitos = apenas_digitos cpf in
        if String.length digitos = 0 then
          "Nao entendi. Envie um CPF ou use /ajuda"
        else
          match valida_cpf cpf with
          | `Valido ->
              Printf.sprintf "CPF VALIDO!\n\n%s" (formata_cpf cpf)
          | `Invalido motivo ->
              Printf.sprintf "CPF INVALIDO!\n\nMotivo: %s\nInformado: %s" motivo (formata_cpf cpf)
  in
  send_message chat_id resposta

let rec poll offset =
  get_updates offset >>= fun body ->
  let json = Yojson.Safe.from_string body in
  let open Yojson.Safe.Util in
  let ok = json |> member "ok" |> to_bool in
  if not ok then (
    print_endline "Erro na API do Telegram";
    Lwt_unix.sleep 5.0 >>= fun () -> poll offset
  ) else (
    let updates = json |> member "result" |> to_list in
    let new_offset =
      List.fold_left (fun acc update ->
        let update_id = update |> member "update_id" |> to_int in
        let _ =
          match update |> member "message" with
          | `Null -> Lwt.return ""
          | msg   -> handle_message msg
        in
        max acc (update_id + 1)
      ) offset updates
    in
    poll new_offset
  )

let () =
  print_endline "Bot iniciado!";
  Lwt_main.run (poll 0)