(** MCP HTTP Session ID management
    MCP Spec 2025-03-26: Session IDs must be visible ASCII (0x21-0x7E) *)

(** Base62 character set for compact, ASCII-safe IDs *)
let base62_chars = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

(** Encode integer to base62 string *)
let encode_base62 n =
  let rec aux acc n =
    if n = 0 then acc
    else aux (base62_chars.[n mod 62] :: acc) (n / 62)
  in
  if n = 0 then "0"
  else String.init (List.length (aux [] n)) (fun i -> List.nth (aux [] n) i)

(** Validate session ID per MCP spec: visible ASCII only (0x21-0x7E) *)
let is_valid id =
  String.length id > 0 &&
  String.for_all (fun c ->
    let code = Char.code c in
    code >= 0x21 && code <= 0x7E
  ) id

(** Generate unique session ID (MCP spec format: visible ASCII 0x21-0x7E) *)
let generate () =
  let timestamp = int_of_float (Unix.gettimeofday () *. 1000.0) in
  let pid = Unix.getpid () in
  let random = Random.int 1000000 in
  Printf.sprintf "mcp_%s_%s_%s"
    (encode_base62 timestamp)
    (encode_base62 pid)
    (encode_base62 random)

(** Get or generate a valid session ID from optional header value *)
let get_or_generate = function
  | Some id when is_valid id -> id
  | Some _ -> generate ()  (* Invalid session ID format, generate new *)
  | None -> generate ()
