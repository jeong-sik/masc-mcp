(** RFC 8866 SDP - Session Description Protocol

    Pure OCaml implementation of SDP for WebRTC.
*)

(* ============================================
   Types
   ============================================ *)

type net_type = IN
type addr_type = IP4 | IP6

type media_type =
  | Audio
  | Video
  | Application
  | Text
  | Message

type transport =
  | UDP
  | TCP
  | DTLS_SCTP
  | UDP_DTLS_SCTP
  | RTP_AVP
  | RTP_SAVP
  | RTP_SAVPF

type connection = {
  net_type: net_type;
  addr_type: addr_type;
  address: string;
  ttl: int option;
  num_addresses: int option;
}

type origin = {
  username: string;
  sess_id: string;
  sess_version: string;
  net_type: net_type;
  addr_type: addr_type;
  unicast_address: string;
}

type bandwidth = {
  bw_type: string;
  bandwidth: int;
}

type ice_candidate = {
  foundation: string;
  component: int;
  transport: string;
  priority: int;
  address: string;
  port: int;
  cand_type: string;
  rel_addr: string option;
  rel_port: int option;
  extensions: (string * string) list;
}

type setup_role =
  | Active
  | Passive
  | Actpass
  | Holdconn

type fingerprint = {
  hash_func: string;
  fingerprint: string;
}

type direction =
  | Sendrecv
  | Sendonly
  | Recvonly
  | Inactive

type sctp_port = int
type max_message_size = int

type media = {
  media_type: media_type;
  port: int;
  proto: transport;
  fmt: string list;
  connection: connection option;
  bandwidth: bandwidth list;
  ice_ufrag: string option;
  ice_pwd: string option;
  ice_options: string list;
  ice_candidates: ice_candidate list;
  setup: setup_role option;
  fingerprint: fingerprint option;
  rtcp: (int * string) option;
  rtcp_mux: bool;
  sctp_port: sctp_port option;
  max_message_size: max_message_size option;
  direction: direction;
  mid: string option;
  attributes: (string * string option) list;
}

type session = {
  version: int;
  origin: origin;
  name: string;
  info: string option;
  uri: string option;
  email: string list;
  phone: string list;
  connection: connection option;
  bandwidth: bandwidth list;
  timing: (int * int);
  ice_lite: bool;
  ice_ufrag: string option;
  ice_pwd: string option;
  ice_options: string list;
  fingerprint: fingerprint option;
  bundle_group: string list option;
  media: media list;
  attributes: (string * string option) list;
}

(* ============================================
   String conversions
   ============================================ *)

let string_of_net_type = function IN -> "IN"
let net_type_of_string = function "IN" -> IN | _ -> IN

let string_of_addr_type = function IP4 -> "IP4" | IP6 -> "IP6"
let addr_type_of_string = function
  | "IP4" -> IP4
  | "IP6" -> IP6
  | _ -> IP4

let string_of_media_type = function
  | Audio -> "audio"
  | Video -> "video"
  | Application -> "application"
  | Text -> "text"
  | Message -> "message"

let media_type_of_string = function
  | "audio" -> Audio
  | "video" -> Video
  | "application" -> Application
  | "text" -> Text
  | "message" -> Message
  | _ -> Application

let string_of_transport = function
  | UDP -> "UDP"
  | TCP -> "TCP"
  | DTLS_SCTP -> "DTLS/SCTP"
  | UDP_DTLS_SCTP -> "UDP/DTLS/SCTP"
  | RTP_AVP -> "RTP/AVP"
  | RTP_SAVP -> "RTP/SAVP"
  | RTP_SAVPF -> "RTP/SAVPF"

let transport_of_string = function
  | "UDP" -> UDP
  | "TCP" -> TCP
  | "DTLS/SCTP" -> DTLS_SCTP
  | "UDP/DTLS/SCTP" -> UDP_DTLS_SCTP
  | "RTP/AVP" -> RTP_AVP
  | "RTP/SAVP" -> RTP_SAVP
  | "RTP/SAVPF" -> RTP_SAVPF
  | _ -> UDP

let string_of_direction = function
  | Sendrecv -> "sendrecv"
  | Sendonly -> "sendonly"
  | Recvonly -> "recvonly"
  | Inactive -> "inactive"

let direction_of_string = function
  | "sendrecv" -> Sendrecv
  | "sendonly" -> Sendonly
  | "recvonly" -> Recvonly
  | "inactive" -> Inactive
  | _ -> Sendrecv

(* Export for potential external use *)
let _ = direction_of_string

let string_of_setup = function
  | Active -> "active"
  | Passive -> "passive"
  | Actpass -> "actpass"
  | Holdconn -> "holdconn"

let setup_of_string = function
  | "active" -> Active
  | "passive" -> Passive
  | "actpass" -> Actpass
  | "holdconn" -> Holdconn
  | _ -> Actpass

(* ============================================
   Parsing helpers
   ============================================ *)

let split_first c s =
  match String.index_opt s c with
  | None -> (s, None)
  | Some i -> (String.sub s 0 i, Some (String.sub s (i + 1) (String.length s - i - 1)))

let split_spaces s =
  String.split_on_char ' ' s |> List.filter (fun x -> x <> "")

(* ============================================
   ICE Candidate parsing
   ============================================ *)

let parse_ice_candidate line =
  (* candidate:foundation component transport priority address port typ type [raddr addr] [rport port] *)
  let parts = split_spaces line in
  match parts with
  | foundation :: comp :: trans :: prio :: addr :: port :: "typ" :: ctype :: rest ->
    let (rel_addr, rel_port, exts) =
      let rec find_rel ra rp acc = function
        | [] -> (ra, rp, List.rev acc)
        | "raddr" :: v :: rest -> find_rel (Some v) rp acc rest
        | "rport" :: v :: rest -> find_rel ra (Some (int_of_string v)) acc rest
        | k :: v :: rest -> find_rel ra rp ((k, v) :: acc) rest
        | [k] -> find_rel ra rp ((k, "") :: acc) []
      in
      find_rel None None [] rest
    in
    Ok {
      foundation;
      component = int_of_string comp;
      transport = trans;
      priority = int_of_string prio;
      address = addr;
      port = int_of_string port;
      cand_type = ctype;
      rel_addr;
      rel_port;
      extensions = exts;
    }
  | _ -> Error "Invalid ICE candidate format"

let ice_candidate_to_string c =
  let base = Printf.sprintf "%s %d %s %d %s %d typ %s"
    c.foundation c.component c.transport c.priority
    c.address c.port c.cand_type
  in
  let rel = match c.rel_addr, c.rel_port with
    | Some ra, Some rp -> Printf.sprintf " raddr %s rport %d" ra rp
    | Some ra, None -> Printf.sprintf " raddr %s" ra
    | None, Some rp -> Printf.sprintf " rport %d" rp
    | None, None -> ""
  in
  let exts = List.fold_left (fun acc (k, v) ->
    acc ^ Printf.sprintf " %s %s" k v
  ) "" c.extensions in
  base ^ rel ^ exts

(* ============================================
   Fingerprint parsing
   ============================================ *)

let parse_fingerprint line =
  match split_first ' ' line with
  | (hash_func, Some fp) -> Ok { hash_func; fingerprint = fp }
  | _ -> Error "Invalid fingerprint format"

let fingerprint_to_string fp =
  Printf.sprintf "%s %s" fp.hash_func fp.fingerprint

(* ============================================
   Line parsing
   ============================================ *)

let parse_origin line =
  match split_spaces line with
  | [username; sess_id; sess_version; nt; at; addr] ->
    Ok {
      username;
      sess_id;
      sess_version;
      net_type = net_type_of_string nt;
      addr_type = addr_type_of_string at;
      unicast_address = addr;
    }
  | _ -> Error "Invalid origin line"

let parse_connection line =
  match split_spaces line with
  | [nt; at; addr] ->
    let (address, rest) = split_first '/' addr in
    let (ttl, num) = match rest with
      | None -> (None, None)
      | Some r ->
        match split_first '/' r with
        | (t, None) -> (Some (int_of_string t), None)
        | (t, Some n) -> (Some (int_of_string t), Some (int_of_string n))
    in
    Ok {
      net_type = net_type_of_string nt;
      addr_type = addr_type_of_string at;
      address;
      ttl;
      num_addresses = num;
    }
  | _ -> Error "Invalid connection line"

let parse_bandwidth line =
  match split_first ':' line with
  | (bw_type, Some bw) ->
    (try Ok { bw_type; bandwidth = int_of_string bw }
     with _ -> Error "Invalid bandwidth value")
  | _ -> Error "Invalid bandwidth line"

let parse_media_line line =
  match split_spaces line with
  | media :: port :: proto :: fmt ->
    let (port_int, _num_ports) =
      match split_first '/' port with
      | (p, None) -> (int_of_string p, 1)
      | (p, Some n) -> (int_of_string p, int_of_string n)
    in
    Ok (media_type_of_string media, port_int, transport_of_string proto, fmt)
  | _ -> Error "Invalid media line"

(* ============================================
   Session Parsing
   ============================================ *)

let default_media = {
  media_type = Application;
  port = 0;
  proto = UDP;
  fmt = [];
  connection = None;
  bandwidth = [];
  ice_ufrag = None;
  ice_pwd = None;
  ice_options = [];
  ice_candidates = [];
  setup = None;
  fingerprint = None;
  rtcp = None;
  rtcp_mux = false;
  sctp_port = None;
  max_message_size = None;
  direction = Sendrecv;
  mid = None;
  attributes = [];
}

let default_session = {
  version = 0;
  origin = {
    username = "-";
    sess_id = "0";
    sess_version = "0";
    net_type = IN;
    addr_type = IP4;
    unicast_address = "0.0.0.0";
  };
  name = "-";
  info = None;
  uri = None;
  email = [];
  phone = [];
  connection = None;
  bandwidth = [];
  timing = (0, 0);
  ice_lite = false;
  ice_ufrag = None;
  ice_pwd = None;
  ice_options = [];
  fingerprint = None;
  bundle_group = None;
  media = [];
  attributes = [];
}

let parse_attribute line =
  match split_first ':' line with
  | (name, value) -> (name, value)

let rec parse_media (lines : string list) (media : media) : (media * string list, string) result =
  match lines with
  | [] -> Ok (media, [])
  | line :: rest when String.length line > 0 ->
    let typ = line.[0] in
    let value = if String.length line > 2 then String.sub line 2 (String.length line - 2) else "" in
    (match typ with
     | 'm' -> Ok (media, lines)  (* Next media section *)
     | 'c' ->
       (match parse_connection value with
        | Ok c -> parse_media rest { media with connection = Some c }
        | Error e -> Error e)
     | 'b' ->
       (match parse_bandwidth value with
        | Ok b -> parse_media rest { media with bandwidth = b :: media.bandwidth }
        | Error e -> Error e)
     | 'a' ->
       let (name, value_opt) = parse_attribute value in
       let media' = match name with
         | "ice-ufrag" -> { media with ice_ufrag = value_opt }
         | "ice-pwd" -> { media with ice_pwd = value_opt }
         | "ice-options" ->
           { media with ice_options = match value_opt with
               | Some v -> split_spaces v
               | None -> [] }
         | "candidate" ->
           (match value_opt with
            | Some v ->
              (match parse_ice_candidate v with
               | Ok c -> { media with ice_candidates = c :: media.ice_candidates }
               | Error _ -> media)
            | None -> media)
         | "setup" ->
           { media with setup = Option.map setup_of_string value_opt }
         | "fingerprint" ->
           (match value_opt with
            | Some v ->
              (match parse_fingerprint v with
               | Ok fp -> { media with fingerprint = Some fp }
               | Error _ -> media)
            | None -> media)
         | "mid" -> { media with mid = value_opt }
         | "rtcp-mux" -> { media with rtcp_mux = true }
         | "sctp-port" ->
           { media with sctp_port = Option.map int_of_string value_opt }
         | "max-message-size" ->
           { media with max_message_size = Option.map int_of_string value_opt }
         | "sendrecv" -> { media with direction = Sendrecv }
         | "sendonly" -> { media with direction = Sendonly }
         | "recvonly" -> { media with direction = Recvonly }
         | "inactive" -> { media with direction = Inactive }
         | _ -> { media with attributes = (name, value_opt) :: media.attributes }
       in
       parse_media rest media'
     | _ -> parse_media rest media)  (* Skip unknown lines *)
  | _ :: rest -> parse_media rest media

let parse sdp =
  let lines = String.split_on_char '\n' sdp
    |> List.map String.trim
    |> List.filter (fun l -> String.length l > 0)
  in
  let rec parse_session lines session =
    match lines with
    | [] -> Ok session
    | line :: rest when String.length line > 0 ->
      let typ = line.[0] in
      let value = if String.length line > 2 then String.sub line 2 (String.length line - 2) else "" in
      (match typ with
       | 'v' ->
         (try parse_session rest { session with version = int_of_string value }
          with _ -> Error "Invalid version")
       | 'o' ->
         (match parse_origin value with
          | Ok o -> parse_session rest { session with origin = o }
          | Error e -> Error e)
       | 's' -> parse_session rest { session with name = value }
       | 'i' -> parse_session rest { session with info = Some value }
       | 'u' -> parse_session rest { session with uri = Some value }
       | 'e' -> parse_session rest { session with email = value :: session.email }
       | 'p' -> parse_session rest { session with phone = value :: session.phone }
       | 'c' ->
         (match parse_connection value with
          | Ok c -> parse_session rest { session with connection = Some c }
          | Error e -> Error e)
       | 'b' ->
         (match parse_bandwidth value with
          | Ok b -> parse_session rest { session with bandwidth = b :: session.bandwidth }
          | Error e -> Error e)
       | 't' ->
         (match split_spaces value with
          | [start; stop] ->
            (try parse_session rest { session with timing = (int_of_string start, int_of_string stop) }
             with _ -> Error "Invalid timing")
          | _ -> Error "Invalid timing line")
       | 'a' ->
         let (name, value_opt) = parse_attribute value in
         let session' = match name with
           | "ice-lite" -> { session with ice_lite = true }
           | "ice-ufrag" -> { session with ice_ufrag = value_opt }
           | "ice-pwd" -> { session with ice_pwd = value_opt }
           | "ice-options" ->
             { session with ice_options = match value_opt with
                 | Some v -> split_spaces v
                 | None -> [] }
           | "fingerprint" ->
             (match value_opt with
              | Some v ->
                (match parse_fingerprint v with
                 | Ok fp -> { session with fingerprint = Some fp }
                 | Error _ -> session)
              | None -> session)
           | "group" ->
             (match value_opt with
              | Some v ->
                let parts = split_spaces v in
                (match parts with
                 | "BUNDLE" :: mids -> { session with bundle_group = Some mids }
                 | _ -> session)
              | None -> session)
           | _ -> { session with attributes = (name, value_opt) :: session.attributes }
         in
         parse_session rest session'
       | 'm' ->
         (* Start of media section *)
         (match parse_media_line value with
          | Ok (mt, port, proto, fmt) ->
            let media_start = { default_media with media_type = mt; port; proto; fmt } in
            (match parse_media rest media_start with
             | Ok (media, remaining) ->
               parse_session remaining { session with media = media :: session.media }
             | Error e -> Error e)
          | Error e -> Error e)
       | _ -> parse_session rest session)
    | _ :: rest -> parse_session rest session
  in
  match parse_session lines default_session with
  | Ok s -> Ok { s with media = List.rev s.media }
  | Error e -> Error e

(* ============================================
   Generation
   ============================================ *)

let connection_to_string c =
  let addr = match c.ttl, c.num_addresses with
    | Some t, Some n -> Printf.sprintf "%s/%d/%d" c.address t n
    | Some t, None -> Printf.sprintf "%s/%d" c.address t
    | None, _ -> c.address
  in
  Printf.sprintf "%s %s %s"
    (string_of_net_type c.net_type)
    (string_of_addr_type c.addr_type)
    addr

let origin_to_string o =
  Printf.sprintf "%s %s %s %s %s %s"
    o.username o.sess_id o.sess_version
    (string_of_net_type o.net_type)
    (string_of_addr_type o.addr_type)
    o.unicast_address

let media_to_string m =
  let buf = Buffer.create 256 in

  (* m= line *)
  let fmt_str = String.concat " " m.fmt in
  Buffer.add_string buf (Printf.sprintf "m=%s %d %s %s\r\n"
    (string_of_media_type m.media_type)
    m.port
    (string_of_transport m.proto)
    fmt_str);

  (* c= line *)
  (match m.connection with
   | Some c -> Buffer.add_string buf (Printf.sprintf "c=%s\r\n" (connection_to_string c))
   | None -> ());

  (* b= lines *)
  List.iter (fun b ->
    Buffer.add_string buf (Printf.sprintf "b=%s:%d\r\n" b.bw_type b.bandwidth)
  ) m.bandwidth;

  (* ICE attributes *)
  (match m.ice_ufrag with
   | Some u -> Buffer.add_string buf (Printf.sprintf "a=ice-ufrag:%s\r\n" u)
   | None -> ());
  (match m.ice_pwd with
   | Some p -> Buffer.add_string buf (Printf.sprintf "a=ice-pwd:%s\r\n" p)
   | None -> ());
  if m.ice_options <> [] then
    Buffer.add_string buf (Printf.sprintf "a=ice-options:%s\r\n" (String.concat " " m.ice_options));

  (* ICE candidates *)
  List.iter (fun c ->
    Buffer.add_string buf (Printf.sprintf "a=candidate:%s\r\n" (ice_candidate_to_string c))
  ) m.ice_candidates;

  (* DTLS attributes *)
  (match m.fingerprint with
   | Some fp -> Buffer.add_string buf (Printf.sprintf "a=fingerprint:%s\r\n" (fingerprint_to_string fp))
   | None -> ());
  (match m.setup with
   | Some s -> Buffer.add_string buf (Printf.sprintf "a=setup:%s\r\n" (string_of_setup s))
   | None -> ());

  (* Mid *)
  (match m.mid with
   | Some mid -> Buffer.add_string buf (Printf.sprintf "a=mid:%s\r\n" mid)
   | None -> ());

  (* RTCP-mux *)
  if m.rtcp_mux then Buffer.add_string buf "a=rtcp-mux\r\n";

  (* SCTP attributes *)
  (match m.sctp_port with
   | Some p -> Buffer.add_string buf (Printf.sprintf "a=sctp-port:%d\r\n" p)
   | None -> ());
  (match m.max_message_size with
   | Some s -> Buffer.add_string buf (Printf.sprintf "a=max-message-size:%d\r\n" s)
   | None -> ());

  (* Direction *)
  Buffer.add_string buf (Printf.sprintf "a=%s\r\n" (string_of_direction m.direction));

  (* Other attributes *)
  List.iter (fun (name, value) ->
    match value with
    | Some v -> Buffer.add_string buf (Printf.sprintf "a=%s:%s\r\n" name v)
    | None -> Buffer.add_string buf (Printf.sprintf "a=%s\r\n" name)
  ) m.attributes;

  Buffer.contents buf

let to_string s =
  let buf = Buffer.create 1024 in

  (* Session-level lines *)
  Buffer.add_string buf (Printf.sprintf "v=%d\r\n" s.version);
  Buffer.add_string buf (Printf.sprintf "o=%s\r\n" (origin_to_string s.origin));
  Buffer.add_string buf (Printf.sprintf "s=%s\r\n" s.name);

  (match s.info with
   | Some i -> Buffer.add_string buf (Printf.sprintf "i=%s\r\n" i)
   | None -> ());

  (match s.uri with
   | Some u -> Buffer.add_string buf (Printf.sprintf "u=%s\r\n" u)
   | None -> ());

  List.iter (fun e -> Buffer.add_string buf (Printf.sprintf "e=%s\r\n" e)) s.email;
  List.iter (fun p -> Buffer.add_string buf (Printf.sprintf "p=%s\r\n" p)) s.phone;

  (match s.connection with
   | Some c -> Buffer.add_string buf (Printf.sprintf "c=%s\r\n" (connection_to_string c))
   | None -> ());

  List.iter (fun b ->
    Buffer.add_string buf (Printf.sprintf "b=%s:%d\r\n" b.bw_type b.bandwidth)
  ) s.bandwidth;

  let (start, stop) = s.timing in
  Buffer.add_string buf (Printf.sprintf "t=%d %d\r\n" start stop);

  (* Session-level ICE *)
  if s.ice_lite then Buffer.add_string buf "a=ice-lite\r\n";
  (match s.ice_ufrag with
   | Some u -> Buffer.add_string buf (Printf.sprintf "a=ice-ufrag:%s\r\n" u)
   | None -> ());
  (match s.ice_pwd with
   | Some p -> Buffer.add_string buf (Printf.sprintf "a=ice-pwd:%s\r\n" p)
   | None -> ());
  if s.ice_options <> [] then
    Buffer.add_string buf (Printf.sprintf "a=ice-options:%s\r\n" (String.concat " " s.ice_options));

  (* Session-level fingerprint *)
  (match s.fingerprint with
   | Some fp -> Buffer.add_string buf (Printf.sprintf "a=fingerprint:%s\r\n" (fingerprint_to_string fp))
   | None -> ());

  (* Bundle group *)
  (match s.bundle_group with
   | Some mids -> Buffer.add_string buf (Printf.sprintf "a=group:BUNDLE %s\r\n" (String.concat " " mids))
   | None -> ());

  (* Session attributes *)
  List.iter (fun (name, value) ->
    match value with
    | Some v -> Buffer.add_string buf (Printf.sprintf "a=%s:%s\r\n" name v)
    | None -> Buffer.add_string buf (Printf.sprintf "a=%s\r\n" name)
  ) s.attributes;

  (* Media sections *)
  List.iter (fun m ->
    Buffer.add_string buf (media_to_string m)
  ) s.media;

  Buffer.contents buf

(* ============================================
   WebRTC Helpers
   ============================================ *)

let create_datachannel_offer ~ice_ufrag ~ice_pwd ~fingerprint ?sctp_port ?max_message_size () =
  let sctp_port = Option.value ~default:5000 sctp_port in
  let max_message_size = Option.value ~default:262144 max_message_size in
  let sess_id = string_of_int (Random.int 1000000000) in
  {
    default_session with
    origin = {
      username = "-";
      sess_id;
      sess_version = "2";
      net_type = IN;
      addr_type = IP4;
      unicast_address = "127.0.0.1";
    };
    name = "-";
    timing = (0, 0);
    ice_ufrag = Some ice_ufrag;
    ice_pwd = Some ice_pwd;
    fingerprint = Some fingerprint;
    bundle_group = Some ["0"];
    media = [{
      default_media with
      media_type = Application;
      port = 9;  (* Placeholder, actual port from ICE *)
      proto = UDP_DTLS_SCTP;
      fmt = ["webrtc-datachannel"];
      mid = Some "0";
      setup = Some Actpass;
      sctp_port = Some sctp_port;
      max_message_size = Some max_message_size;
      direction = Sendrecv;
    }];
  }

let create_datachannel_answer ~offer ~ice_ufrag ~ice_pwd ~fingerprint =
  let sess_id = string_of_int (Random.int 1000000000) in
  {
    offer with
    origin = {
      offer.origin with
      sess_id;
      sess_version = "2";
    };
    ice_ufrag = Some ice_ufrag;
    ice_pwd = Some ice_pwd;
    fingerprint = Some fingerprint;
    media = List.map (fun m ->
      { m with
        setup = Some Active;  (* Answer is active if offer was actpass *)
        ice_candidates = [];  (* Clear offer's candidates *)
      }
    ) offer.media;
  }

let add_ice_candidate media candidate =
  { media with ice_candidates = candidate :: media.ice_candidates }

let get_ice_candidates session =
  List.fold_left (fun acc media ->
    let mid = Option.value ~default:"0" media.mid in
    List.fold_left (fun acc2 cand ->
      (mid, cand) :: acc2
    ) acc media.ice_candidates
  ) [] session.media

let get_media_by_mid session mid =
  List.find_opt (fun m -> m.mid = Some mid) session.media

let is_datachannel_session session =
  List.exists (fun m ->
    m.media_type = Application &&
    (m.proto = DTLS_SCTP || m.proto = UDP_DTLS_SCTP)
  ) session.media

(* ============================================
   Pretty printing
   ============================================ *)

let pp_session fmt session =
  Format.fprintf fmt "%s" (to_string session)
