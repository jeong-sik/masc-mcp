# Install Checklist

## Prereqs
- [ ] OCaml 5.x + opam
- [ ] dune 3.x

## Build
- [ ] `opam pin add mcp_protocol https://github.com/jeong-sik/mcp-protocol-sdk.git -y`
- [ ] `opam pin add ocaml-webrtc https://github.com/jeong-sik/ocaml-webrtc.git -y`
- [ ] `opam install . --deps-only`
- [ ] `dune build`

## Run
- [ ] `./start-masc-mcp.sh --port 8935`
- [ ] `curl http://127.0.0.1:8935/health`

## MCP Config
- [ ] `~/.mcp.json`에 서버 등록
```json
{
  "mcpServers": {
    "masc": { "type": "http", "url": "http://127.0.0.1:8935/mcp" }
  }
}
```
