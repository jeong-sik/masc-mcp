(** Cancellation Tokens - MCP 2025-11-25 Spec Compliance

    Implements client-side cancellation support for long-running operations.
    Based on CancellationToken pattern.

    MCP Spec MAY: Support for client request cancellation
*)

(** Cancellation token state *)
type token = {
  id: string;
  mutable cancelled: bool;
  mutable reason: string option;
  mutable callbacks: (unit -> unit) list;
  created_at: float;
}

(** Token store *)
module TokenStore = struct
  let tokens : (string, token) Hashtbl.t = Hashtbl.create 64

  (** Generate token ID *)
  let generate_id () : string =
    let bytes = Mirage_crypto_rng.generate 8 in
    let buf = Buffer.create 16 in
    for i = 0 to String.length bytes - 1 do
      Buffer.add_string buf (Printf.sprintf "%02x" (Char.code (String.get bytes i)))
    done;
    Printf.sprintf "cancel_%s" (Buffer.contents buf)

  (** Create new cancellation token *)
  let create () : token =
    let token = {
      id = generate_id ();
      cancelled = false;
      reason = None;
      callbacks = [];
      created_at = Unix.gettimeofday ();
    } in
    Hashtbl.add tokens token.id token;
    token

  (** Get token by ID *)
  let get (id : string) : token option =
    Hashtbl.find_opt tokens id

  (** Remove token *)
  let remove (id : string) : unit =
    Hashtbl.remove tokens id

  (** List all tokens *)
  let list_all () : token list =
    Hashtbl.fold (fun _ t acc -> t :: acc) tokens []

  (** Cleanup old tokens (older than max_age seconds) *)
  let cleanup ~(max_age : float) : int =
    let now = Unix.gettimeofday () in
    let old_tokens = Hashtbl.fold (fun id t acc ->
      if now -. t.created_at > max_age then id :: acc else acc
    ) tokens [] in
    List.iter (Hashtbl.remove tokens) old_tokens;
    List.length old_tokens
end

(** Check if token is cancelled *)
let is_cancelled (token : token) : bool =
  token.cancelled

(** Cancel a token - triggers all callbacks *)
let cancel ?(reason : string option) (token : token) : unit =
  if not token.cancelled then begin
    token.cancelled <- true;
    token.reason <- reason;
    (* Execute callbacks in reverse order (LIFO) *)
    List.iter (fun cb ->
      try cb () with exn ->
        Log.Cancel.error "Callback failed: %s" (Printexc.to_string exn)
    ) token.callbacks
  end

(** Register cancellation callback *)
let on_cancel (token : token) (callback : unit -> unit) : unit =
  token.callbacks <- callback :: token.callbacks

(** Cancel token by ID *)
let cancel_by_id ?(reason : string option) (id : string) : bool =
  match TokenStore.get id with
  | Some token ->
    cancel ?reason token;
    true
  | None -> false

(** Create a token linked to a task *)
let create_for_task ~(task_id : string) : token =
  let token = TokenStore.create () in
  (* Store task_id as metadata - we could extend token type later *)
  on_cancel token (fun () ->
    Log.Cancel.info "Task %s cancelled (token: %s)" task_id token.id
  );
  token

(** Token to JSON *)
let token_to_json (t : token) : Yojson.Safe.t =
  `Assoc [
    ("id", `String t.id);
    ("cancelled", `Bool t.cancelled);
    ("reason", match t.reason with None -> `Null | Some r -> `String r);
    ("created_at", `Float t.created_at);
    ("callback_count", `Int (List.length t.callbacks));
  ]

(** MCP tool handler for cancellation *)
let handle_cancellation_tool (arguments : Yojson.Safe.t) : (bool * string) =
  let get_string key =
    match Yojson.Safe.Util.member key arguments with
    | `String s -> Some s
    | _ -> None
  in
  match get_string "action" with
  | Some "create" ->
    let token = TokenStore.create () in
    (true, Yojson.Safe.pretty_to_string (token_to_json token))

  | Some "cancel" ->
    (match get_string "token_id" with
     | Some id ->
       let reason = get_string "reason" in
       if cancel_by_id ?reason id then
         (true, Printf.sprintf "Token '%s' cancelled" id)
       else
         (false, Printf.sprintf "Token '%s' not found" id)
     | None -> (false, "token_id required"))

  | Some "check" ->
    (match get_string "token_id" with
     | Some id ->
       (match TokenStore.get id with
        | Some token -> (true, Yojson.Safe.pretty_to_string (token_to_json token))
        | None -> (false, Printf.sprintf "Token '%s' not found" id))
     | None -> (false, "token_id required"))

  | Some "list" ->
    let tokens = TokenStore.list_all () in
    let json = `Assoc [
      ("count", `Int (List.length tokens));
      ("tokens", `List (List.map token_to_json tokens));
    ] in
    (true, Yojson.Safe.pretty_to_string json)

  | Some "cleanup" ->
    let removed = TokenStore.cleanup ~max_age:3600.0 in
    (true, Printf.sprintf "Cleaned up %d old tokens" removed)

  | Some other -> (false, Printf.sprintf "Unknown action: %s" other)
  | None -> (false, "action required: create, cancel, check, list, cleanup")
