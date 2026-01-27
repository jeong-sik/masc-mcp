# masc-mcp

OCaml로 만든 개인용 MCP 서버입니다.
같은 코드베이스에서 여러 AI 에이전트가 작업할 때, 최소한의 조율(작업 보드, 메시지, 상태 공유)을 제공하기 위해 만들었습니다.

다음 전제를 두고 있습니다:

- 개인/소규모 환경 기준으로 사용·테스트했습니다.
- API와 동작은 자주 바뀔 수 있습니다.
- 보안·권한 모델은 강하지 않습니다. 로컬 또는 신뢰된 네트워크를 전제로 합니다.

## Quickstart

```bash
./start-masc-mcp.sh --http --port 8935
curl http://127.0.0.1:8935/health
```

## MCP 설정

`~/.mcp.json` 예시:

```json
{
  "mcpServers": {
    "masc": {
      "type": "http",
      "url": "http://127.0.0.1:8935/mcp"
    }
  }
}
```

## 기본 사용 흐름

가장 단순한 흐름은 아래 순서입니다.

```text
join → 작업 추가/클레임 → 진행 공유 → 완료 → leave
```

예시:

```text
masc_join(agent_name: "codex")
masc_add_task(title: "README 정리")
masc_claim(agent_name: "codex", task_id: "task-001")
masc_broadcast(agent_name: "codex", message: "작업 시작")
masc_done(agent_name: "codex", task_id: "task-001")
masc_leave(agent_name: "codex")
```

## 제공 기능 (요약)

- 작업 보드: `masc_tasks`, `masc_add_task`, `masc_claim`, `masc_done`, `masc_release`
- 상태/헬스: `masc_status`, `masc_agents`, `masc_heartbeat`, `masc_cleanup_zombies`
- 커뮤니케이션: `masc_broadcast`, `masc_messages`, `masc_portal_*`, SSE 이벤트(`/sse`)
- 안전장치: `masc_interrupt`, `masc_approve`, `masc_reject`
- 워크스페이스 도우미: `masc_worktree_*`, `masc_lock`/`masc_unlock`, `masc_set_room`
- 표면적 줄이기: `masc_switch_mode`, `masc_get_config`

## 저장소와 백엔드

- 기본 백엔드는 파일 기반이며, 프로젝트 루트의 `.masc/` 아래에 상태를 저장합니다.
- PostgreSQL 백엔드를 사용할 수 있습니다. 환경변수 설정은 `docs/SETUP.md`를 참고하세요.

## 문서

- `docs/QUICKSTART.md`: 빠른 시작
- `docs/SETUP.md`: 설치/실행/백엔드 설정
- `docs/MCP-TEMPLATE.md`: MCP 설정 템플릿
- `docs/MODE-SYSTEM.md`: 모드/카테고리
- `docs/SPEC.md`: 동작 스펙과 데이터 모델
- `docs/INTERRUPT-DESIGN.md`: 승인/중단 패턴
- `docs/MITOSIS.md`: 컨텍스트 한계 대응 메모

## 운영 메모

- 기본 HTTP 엔드포인트는 `/mcp`, `/health`, `/sse`입니다.
- 시작 스크립트는 `start-masc-mcp.sh`이며, Eio 런타임을 기준으로 동작합니다.
- 여러 에이전트를 동시에 사용할 때는 SSE를 별도 터미널에서 모니터링하면 디버깅이 쉽습니다.
