# Setup

간단 설치/실행/연동 방법만 정리합니다.

## 요구사항

- OCaml 5.x + opam
- dune 3.x

## 설치

```bash
git clone https://github.com/jeong-sik/masc-mcp.git
cd masc-mcp

# 외부 의존성 pin (opam에 없음)
opam pin add mcp_protocol https://github.com/jeong-sik/mcp-protocol-sdk.git -y
opam pin add ocaml-webrtc https://github.com/jeong-sik/ocaml-webrtc.git -y

# 의존성 설치
opam install . --deps-only

# 빌드
dune build
```

## 실행

```bash
# HTTP 모드 (기본)
./start-masc-mcp.sh --port 8935

# stdio 모드 (레거시)
./start-masc-mcp.sh --stdio

# 상태 확인
curl http://127.0.0.1:8935/health
```

## MCP 설정

README의 예시 구성(Claude Code) 참고:

```json
{
  "mcpServers": {
    "masc": { "type": "http", "url": "http://127.0.0.1:8935/mcp" }
  }
}
```

## 최소 사용 예시

```text
masc_join(agent_name: "claude")
masc_add_task(title: "My first task")
masc_claim(agent_name: "claude", task_id: "task-001")
masc_done(agent_name: "claude", task_id: "task-001")
```
