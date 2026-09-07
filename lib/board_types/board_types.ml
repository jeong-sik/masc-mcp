(** Board - MASC Internal Board (Mastodon-style federation ready)

    Zero-tolerance implementation:
    - ID validation (no path traversal)
    - TTL optional (0 = permanent, default)
    - Explicit persistence errors
    - Cryptographic IDs (no prediction)
    - Atomic writes (no corruption)
    - Automatic sweeper (no manual cleanup)

    Eio Best Practices:
    - Switch.on_release for cleanup (not Fun.protect)
    - Structured concurrency

    @since 0.5.0 - Replaces social.ml with hardened implementation
*)

(** {1 Error Types - No Silent Failures} *)

type board_error =
  | Invalid_id of string
  | Post_not_found of string
  | Comment_not_found of string
  | Io_error of string
  | Validation_error of string
  | Already_voted of string
  | Already_exists of string
  | Unauthorized of string
    (** Actor attempted an owner-gated mutation (e.g. editing a post they
        do not own). Distinct from [Validation_error] so callers can map it
        to a 403-class rejection rather than a generic input error. *)
  [@@deriving show]

(** {1 Safe ID Module - Parse Don't Validate} *)

(* Shared regex for alphanumeric ID validation (Post_id, Board_id, Sub_board_id).
   Single [Re.compile] DFA build instead of 3 identical copies. [Comment_id]
   is stricter: it accepts only the shape its own [generate] mints. *)
let alphanumeric_id_re = Re.Pcre.re {|^[a-zA-Z0-9_-]+$|} |> Re.compile

module Post_id : sig
  type t
  val of_string : string -> (t, board_error) result
  val to_string : t -> string
  val generate : unit -> t
end = struct
  type t = string

  (* Only alphanumeric, dash, underscore. Max 64 chars. *)
  let valid_pattern = alphanumeric_id_re

  let of_string s =
    let s = String.trim s in
    let len = String.length s in
    if len >= 1 && len <= 64 && Re.execp valid_pattern s then Ok s
    else Error (Invalid_id (Printf.sprintf "Invalid post_id: %s" s))

  let to_string t = t

  let generate () = Random_id.prefixed ~prefix:"p-" ~bytes:16
end

module Comment_id : sig
  type t
  val of_string : string -> (t, board_error) result
  val to_string : t -> string
  val generate : unit -> t
  val accepted_format : string
  val json_schema_pattern : string
end = struct
  type t = string

  (* [generate] is the only minter, so the parser accepts exactly what it
     mints: the ["c-"] prefix followed by [2 * random_bytes] lowercase hex
     characters. A keeper that invents a comment id (["c-placeholder"],
     ["BUILDER_A_DONE"]: 109 of the 134 masc_board_comment_vote calls in
     August 2026, #29457) is refused here with the accepted shape instead of
     reaching the store as a lookup that can only miss. *)
  let prefix = "c-"
  let random_bytes = 16
  let hex_length = 2 * random_bytes
  let accepted_format = Printf.sprintf "%s<%d lowercase hex>" prefix hex_length
  let json_schema_pattern = Printf.sprintf "^%s[0-9a-f]{%d}$" prefix hex_length
  let valid_pattern = Re.Pcre.re json_schema_pattern |> Re.compile

  let of_string s =
    let s = String.trim s in
    if Re.execp valid_pattern s then Ok s
    else
      Error
        (Invalid_id
           (Printf.sprintf
              "Invalid comment_id %S; expected %s, the id masc_board_post_get \
               and masc_board_comment return"
              s accepted_format))

  let to_string t = t

  let generate () = Random_id.prefixed ~prefix ~bytes:random_bytes
end

module Agent_id : sig
  type t
  val parse : string -> (t, board_error) result
  val of_string : string -> (t, board_error) result
  val to_string : t -> string
end = struct
  type t = string

  (* SSOT: [Validation.Id_shape] owns the agent id shape. This module carried
     its own copy and drifted twice -- #8625 raised the length cap by hand to
     match, #8633 re-widened the pattern by hand to match -- each time catching
     up after the fact. It now delegates, so the two cannot disagree again, and
     board inherits the path-separator and traversal checks it never had. *)
  let from_shape = function
    | Ok id -> Ok (Validation.Id_shape.to_string id)
    | Error reason -> Error (Validation_error (Printf.sprintf "Invalid agent_id: %s" reason))

  let parse s =
    Validation.Id_shape.parse (String.trim s) |> from_shape

  let of_string s =
    Validation.Id_shape.validate (String.trim s) |> from_shape

  let to_string t = t
end

(** {1 Types with Mandatory TTL} *)

type visibility =
  | Public      (* Visible to federation *)
  | Unlisted    (* Not in feeds, but accessible *)
  | Internal    (* This MASC instance only *)
  | Direct      (* Mentioned agents only *)

type audience =
  | Targets of Agent_id.t list
  | Broadcast
  | Thread_participants
  | Discoverable
(** Closed routing authority for Board mutations. [Targets] contains exact
    typed Board identities, not Keeper identities; MASC may project those
    identities into Keeper lanes without making Board depend on Keeper. *)

type post_kind =
  | Human_post [@tla.symbol "human_post"]
  | Automation_post [@tla.symbol "automation_post"]
  | System_post [@tla.symbol "system_post"]
[@@deriving tla]

(* RFC-0233 §7: typed provenance of a board post — which keeper turn produced
   it and through which channel. Replaces the fusion [meta_json] [run_id]
   smuggle with a first-class field that a real index can key on (no
   meta_json substring scan, RFC §7.6 guard #2).

   [source] is the channel's [Surface_ref.lane_label] string, NOT a typed
   [Surface_ref.t]: Surface_ref lives in the [masc] umbrella, which depends on
   [masc_board], so referencing the typed variant here would form a cycle.

   [turn_ref] and [fusion_run_id] are distinct (RFC §7.6 guard #5): turn_ref is
   the turn-level join key, fusion_run_id correlates a fusion deliberation run.
   All sub-fields optional; an all-[None] origin is represented as [origin = None]. *)
type post_origin = {
  turn_ref: Ids.Turn_ref.t option;
  source: string option;
  fusion_run_id: string option;
}

(* RFC-0233 §7: constructor for a keeper-authored post's origin. A keeper post
   is the output of a specific keeper turn, so [turn_ref] is the turn-level join
   key and [source] names the producing channel (e.g. "keeper_speech",
   "keeper_alert"). [fusion_run_id] is always [None] here: fusion's
   server-root-switch fork has its own constructor at the fusion sink. [turn_ref]
   stays optional so callers that cannot reach a mint-once-safe turn reference
   still set [source] (origin present, turn_ref absent) rather than fabricating
   one. *)
let keeper_authored_origin ?turn_ref ~source () : post_origin =
  { turn_ref; source = Some source; fusion_run_id = None }

type post = {
  id: Post_id.t;
  author: Agent_id.t;
  title: string;
  body: string;
  post_kind: post_kind;
  meta_json: Yojson.Safe.t option;
  visibility: visibility;
  created_at: float;
  updated_at: float;   (* Last activity: vote, comment, edit *)
  expires_at: float;   (* MANDATORY - no eternal posts *)
  votes_up: int;
  votes_down: int;
  reply_count: int;
  pinned: bool;              (* Operator-curated pin (owner-gated): floats the post to the top of its category *)
  hearth: string option;     (* Topic category within the Board *)
  thread_id: string option;  (* Linked Conversation thread *)
  origin: post_origin option; (* RFC-0233 §7: originating turn / channel provenance *)
}

type comment = {
  id: Comment_id.t;
  post_id: Post_id.t;
  parent_id: Comment_id.t option;
  author: Agent_id.t;
  content: string;
  created_at: float;
  expires_at: float;   (* MANDATORY *)
  votes_up: int;
  votes_down: int;
}

type post_creation = {
  post: post;
  audience: audience;
}

type comment_creation = {
  comment: comment;
  audience: audience;
}

type reaction_target_type =
  | Reaction_post
  | Reaction_comment

type reaction = {
  target_type: reaction_target_type;
  target_id: string;
  user_id: Agent_id.t;
  emoji: string;
  created_at: float;
}

type reaction_summary = {
  emoji: string;
  count: int;
  reacted: bool;
  recent_user_ids: string list;
}

type reaction_toggle_result = {
  target_type: reaction_target_type;
  target_id: string;
  user_id: string;
  emoji: string;
  reacted: bool;
  summary: reaction_summary list;
}

(** {1 SubBoard — Named spaces within the board} *)

module Sub_board_id : sig
  type t
  val of_string : string -> (t, board_error) result
  val to_string : t -> string
  val generate : unit -> t
end = struct
  type t = string

  let valid_pattern = alphanumeric_id_re

  let of_string s =
    let s = String.trim s in
    let len = String.length s in
    if len >= 1 && len <= 64 && Re.execp valid_pattern s then Ok s
    else Error (Invalid_id (Printf.sprintf "Invalid sub_board_id: %s" s))

  let to_string t = t

  let generate () = Random_id.prefixed ~prefix:"sb-" ~bytes:16
end

type sub_board_access =
  | Open
  | Members_only
  | Owner_only

type sub_board = {
  id: Sub_board_id.t;
  slug: string;
  name: string;
  description: string;
  owner: Agent_id.t;
  members: Agent_id.t list;
  access: sub_board_access;
  created_at: float;
  post_count: int;
}

(** {1 Read pagination and sweeper defaults} *)

module Limits = struct
  let env_int name default = Env_config_core.get_int ~default name

  let default_comment_page_limit = 50
  let max_comment_page_limit = 100
  let default_ttl_hours = 0    (* 0 = permanent (no expiry) *)
  let sweeper_interval_sec = env_int "MASC_BOARD_SWEEPER_INTERVAL_SEC" 10
  let sweeper_batch_size = env_int "MASC_BOARD_SWEEPER_BATCH_SIZE" 100
end

(** {1 Vote Direction} *)

type vote_direction = Up | Down

(** {1 In-Memory Store} *)

type flusher_msg =
  | Flush
  | Sweep

(** {1 Karma Ledger Contract} *)

(** A single attributed karma event.  One event is generated per upvote
    received by an agent.  Downvotes do not generate karma events
    (scoring rule: [Up] = +1, [Down] = 0). *)
type karma_event = {
  recipient : string;
  (** Agent who earned the karma — author of the upvoted post or comment. *)
  voter : string;
  (** Agent who cast the upvote. *)
  target_kind : string;
  (** Content kind: ["post"] or ["comment"]. *)
  target_id : string;
  (** Identifier of the upvoted post or comment. *)
  delta : int;
  (** Karma delta.  Always [+1] per upvote under the current scoring
      contract.  Stored explicitly so future rule changes are backward
      compatible — older events keep their original delta value. *)
  ts : float;
  (** Unix timestamp at which the upvote was cast (seconds since epoch). *)
}

type store = {
  posts: (string, post) Hashtbl.t;
  comments: (string, comment) Hashtbl.t;
  (* #10086: value carries [(direction, cast_ts)] so
     [rewrite_vote_log] persists the original vote timestamp on
     every flush instead of overwriting it with the wall clock.
     The float is Unix seconds at which the vote was first cast,
     or the flip time on a direction change. *)
  vote_log: (string, vote_direction * float) Hashtbl.t;
  post_count: int ref;
  mutable last_sweep: float;
  mutex: Eio.Mutex.t;
  persist_mutex: Eio.Mutex.t;
  origin_create_mutex: Eio.Mutex.t;
  (* Phase 2 caches *)
  mutable karma_cache: (string * int) list option;       (** None = stale *)
  mutable sorted_posts_cache: post list option;           (** None = stale *)
  comments_by_post: (string, string list) Hashtbl.t;      (** post_id -> comment_id list *)
  reactions: (string, reaction) Hashtbl.t;                 (** unique target/user/emoji reactions *)
  mutable dirty_posts: bool;                               (** Deferred flush flag *)
  mutable dirty_comments: bool;                            (** Deferred flush flag *)
  dirty_post_ids: (string, unit) Hashtbl.t;                 (** Deferred post snapshots *)
  dirty_comment_ids: (string, unit) Hashtbl.t;              (** Deferred comment snapshots *)
  mutable last_flush: float;
  flusher_inbox: flusher_msg Eio.Stream.t;                               (** Last deferred flush time *)
  sub_boards: (string, sub_board) Hashtbl.t;               (** sub_board_id -> sub_board *)
  sub_boards_by_slug: (string, string) Hashtbl.t;          (** slug -> sub_board_id *)
  (* RFC-0233 §7 guard #2: real secondary indexes for origin lookup, mirroring
     [sub_boards_by_slug]. Maintained on create and rebuilt on load (derive-on-
     load, no separately-persisted SSOT). Never a meta_json substring scan. *)
  posts_by_turn_ref: (string, string) Hashtbl.t;           (** Turn_ref.to_string -> post_id *)
  posts_by_run_id: (string, string) Hashtbl.t;             (** fusion_run_id -> post_id *)
}
