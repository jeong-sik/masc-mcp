# Performance SLO (MASC MCP)

## 목표
- 로컬/단일 머신 기준의 체감 지연 최소화
- 대시보드 통합 시 안정적인 조회/스트림 유지

## 대상 지표

### MCP JSON-RPC (tools/call)
- P50 < 80ms
- P95 < 300ms
- P99 < 800ms

### REST API
- /api/v1/status P95 < 150ms
- /api/v1/tasks P95 < 250ms (limit=50)
- /api/v1/messages P95 < 250ms (limit=20)

### SSE
- 연결 성공 < 1s
- 이벤트 전달 지연 P95 < 500ms

## 측정 방법
- `benchmarks/quick-bench.sh`
- `benchmarks/benchmark.sh`

환경 변수:
```
MASC_URL=http://127.0.0.1:8935/mcp
MASC_AGENT=bench
MASC_TOKEN=<optional>
```

## 경고 기준
- MCP P95가 1s 이상 지속되면 장애로 간주
- REST API P95가 1s 이상이면 대시보드 사용성 붕괴
- SSE drop/reconnect가 5분 내 3회 이상이면 안정성 이슈

## 개선 힌트
- REST 페이지네이션/필터 적극 사용
- 메시지/태스크 조회는 limit을 낮춘다
- 필요 시 PostgreSQL 백엔드로 전환
