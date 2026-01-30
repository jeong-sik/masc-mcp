(** MCP Protocol Utilities
    HTTP content negotiation for MCP Streamable HTTP transport *)

module Http_negotiation = struct
  (** Standard SSE content type *)
  let sse_content_type = "text/event-stream"

  (** JSON content type *)
  let json_content_type = "application/json"

  (** Parse Accept header value into list of (media_type, q_value) *)
  let parse_accept_header accept_header =
    match accept_header with
    | None -> []
    | Some header ->
      header
      |> String.split_on_char ','
      |> List.map String.trim
      |> List.filter_map (fun part ->
        let part = String.lowercase_ascii part in
        (* Extract media type and parameters *)
        let parts = String.split_on_char ';' part in
        match parts with
        | [] -> None
        | media_type :: params ->
          let media_type = String.trim media_type in
          (* Find q parameter if present *)
          let q_value =
            List.find_map (fun param ->
              let param = String.trim param in
              if String.length param >= 2 && param.[0] = 'q' && param.[1] = '=' then
                let q_str = String.sub param 2 (String.length param - 2) in
                try Some (float_of_string q_str)
                with Failure _ -> None
              else None
            ) params
            |> Option.value ~default:1.0
          in
          Some (media_type, q_value)
      )

  (** Check if a media type is accepted (q > 0) *)
  let is_media_type_accepted media_types target =
    let target = String.lowercase_ascii target in
    List.exists (fun (media_type, q) ->
      q > 0.0 && media_type = target
    ) media_types

  (** Check if Accept header accepts SSE (text/event-stream with q > 0)
      Wildcards (*/*) are NOT sufficient for SSE *)
  let accepts_sse_header accept_header =
    let media_types = parse_accept_header accept_header in
    is_media_type_accepted media_types sse_content_type

  (** Check if Accept header accepts MCP Streamable HTTP
      Requires BOTH application/json AND text/event-stream (with q > 0)
      This distinguishes streamable from legacy SSE-only clients *)
  let accepts_streamable_mcp accept_header =
    let media_types = parse_accept_header accept_header in
    let accepts_json = is_media_type_accepted media_types json_content_type in
    let accepts_sse = is_media_type_accepted media_types sse_content_type in
    accepts_json && accepts_sse
end
