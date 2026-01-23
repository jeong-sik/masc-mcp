(** WebRTC DataChannel Self-Test *)

open Masc_mcp.Webrtc_datachannel

let () =
  print_endline "═══════════════════════════════════════════════════";
  print_endline "  MASC WebRTC DataChannel Self-Test";
  print_endline "═══════════════════════════════════════════════════\n";

  (* 1. Native 가용성 체크 *)
  let native_available = is_native_available () in
  Printf.printf "🔍 Native libdatachannel available: %b\n" native_available;

  (* 2. 기본 초기화 (native 선호) *)
  let backend1 = init () in
  Printf.printf "🚀 Default init backend: %s\n" (string_of_backend backend1);
  cleanup ();

  (* 3. Stub 강제 *)
  let backend2 = init ~prefer_native:false () in
  Printf.printf "📦 Forced stub backend: %s\n" (string_of_backend backend2);
  cleanup ();

  (* 4. 다시 native로 *)
  let backend3 = init () in
  Printf.printf "🔄 Re-init with native: %s\n\n" (string_of_backend backend3);

  (* 5. PeerConnection 생성 *)
  let pc = create_peer_connection () in
  Printf.printf "🔗 PeerConnection created\n";
  Printf.printf "   ICE state: %s\n" (string_of_ice_state (get_ice_state pc));

  (* 6. DataChannel 생성 (음성용 설정) *)
  let dc = create_data_channel pc ~label:"voice-audio"
    ~init:{ default_channel_init with reliability = audio_reliability } () in
  Printf.printf "\n📡 DataChannel created\n";
  Printf.printf "   Label: %s\n" (get_label dc);
  Printf.printf "   State: %s\n" (string_of_channel_state (get_channel_state dc));
  Printf.printf "   Reliability: unordered=%b, unreliable=%b\n"
    audio_reliability.unordered audio_reliability.unreliable;

  (* 7. Status JSON *)
  let status = status_json pc in
  print_endline "\n📊 Status JSON:";
  print_endline (Yojson.Safe.pretty_to_string status);

  (* Cleanup *)
  close_peer_connection pc;
  cleanup ();

  print_endline "\n═══════════════════════════════════════════════════";
  Printf.printf "✅ Self-test completed! Backend: %s\n" (string_of_backend backend3);
  print_endline "═══════════════════════════════════════════════════"
