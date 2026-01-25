# Install Checklist

설치 후 동작 확인용 체크리스트입니다.

## Pre-flight

- [ ] OCaml 5.x + opam + dune 설치
- [ ] 의존성 설치 (`opam install . --deps-only`)
- [ ] 빌드 완료 (`dune build`)

## Run

- [ ] 서버 실행 (`./start-masc-mcp.sh --port 8935`)

## Post-install checks

```bash
curl http://127.0.0.1:8935/health

curl -sS http://127.0.0.1:8935/mcp \
  -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","id":1,"method":"tools/list","params":{}}'
```
