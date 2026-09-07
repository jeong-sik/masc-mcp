---
rfc: "0427"
title: "실행 레인은 사실을 말하고, 자기 shim 을 스스로 배포한다 — keeper 가 도구를 마음껏 쓰기 위한 네 갈래"
status: Draft
created: 2026-09-06
updated: 2026-09-06
author: vincent
supersedes: []
superseded_by: null
related: ["0400", "0404", "0405", "0421", "0422"]
---

## 1. 문제

keeper 가 도구를 마음껏 쓰려면 두 가지가 먼저 있어야 한다. 도구가 돌려주는 답이
사실이어야 하고, 레인이 죽었을 때 keeper 와 운영자가 그 이유를 바로 알아야 한다.
2026-09-05 하루의 로그(`system_log_2026-09-05.jsonl`, 175,525줄)는 둘 다 아직
아니라고 말한다.

### 1.1 하루치 숫자

| 항목 | 값 | 출처 |
|---|---|---|
| tool_call 전체 | 11,892 (ok 11,417 · error 475) | `tool_call tool=` |
| Execute 호출 | 1,820 | 같은 줄 |
| tool_execute 승인 경로 | readonly_sandbox 841 · keeper_always_allow 552 · **judge(one_shot_resolution) 319** · observed_in_box 32 | `operation=tool_execute source=` |
| Execute 오류 중 cwd 없음 | 38 (`cwd_missing`/`cwd_not_directory`) | `tool=Execute outcome=error` |
| Grep 오류 중 엔드포인트에 없는 경로 | 16 (`remote_*_read_failed … rg: … No such file`). 메시지의 `/Users/…` 는 게스트 답을 호스트 표기로 되쓴 것이다(§1.2 A) | `tool=Grep outcome=error` |
| Read 오류 | 109 (그중 `path_outside_sandbox` 12) | `tool=Read outcome=error` |
| 실행 레인 전멸 시간 | 13:39Z ~ 15:57Z, keeper 8명 | 보드 p-d5ed6f05, #33425 |

### 1.2 네 가지 증상

**(A) 읽기·실행 도구가 호스트 경로로 게스트 트리를 찾는다.** RFC-0400 이후
microvm keeper 의 트리는 게스트 볼륨(`/masc-work/<keeper>`)에 있고, remote_ssh
keeper 의 트리는 원격 계정 안(`/opt/masc-playground`)에 있다. 그런데 오늘
`Execute` 는 `cwd_not_directory: /Users/dancer/me/.masc/playground/polisher/masc-t1348`
로 38번 거절됐다(`Keeper_tool_execute_path.resolve_missing_cwd` 가 호스트 경로의
존재를 본다; #33461 이 고쳤다). 처음 이 문서는 rondo 의 `Grep` 오류
`remote_ssh_read_failed … rg: /Users/dancer/me/.masc/playground/rondo/repos/masc: No such
file` 를 "원격 rg 에 호스트 경로가 갔다" 로 읽었다. 틀렸다. 읽기 경로의 호스트→원격
변환(`Keeper_sandbox_read_backend.container_path_of_host` → `Keeper_remote_path.host_to_remote`)
은 맞게 동작하고(microvm 매핑을 `test_keeper_sandbox_read_backend` 에 고정했다), 원격 rg 는
`/opt/masc-playground/rondo/repos/masc` 를 받았다. 그 디렉터리가 원격에 없어서 rg 가
exit 2 로 그 경로를 찍었고, 레인이 게스트 출력의 keeper 루트를 호스트 표기로 되쓰는
`Keeper_remote_path.rewrite_output` 이 그것을 `/Users/…` 로 바꿔 keeper 에게 보였다. 즉
그 16건은 keeper 가 없는 경로를 물은 것이고, 레인은 사실을 말했다. polisher 의
"있는 패턴인데 `ok:true, matches:[]`" 는 이 부류가 아니라 재시작 전 서버에서만 보인
현상으로, 재시작 뒤에는 polisher 3건·sangsu 7건·rondo 19건의 Grep 이 모두 성공했다.
A 의 나머지 둘(A-2, A-3)은 그 뒤 실측으로 닫았다. rg 는 없는 경로에 exit 2, 빈
디렉터리에 exit 1 로 끝나고, 읽기 op 는 이미 그 둘을 가른다(2 는 실패에
`error_detail`, 죽은 레인은 `classify_read_outcome` 이 언제나 오류). 원격 argv 를
타입으로 좁히는 쪽도 부를 자리가 트리에 하나뿐이라 지킬 것이 세 줄이다.

**(B) shim 은 손으로 배포되고, 서버는 그 사실을 모른다.** shim 은 운영자가
`build-shim.sh` 로 만들어 `~/me/.masc/microvm/shim/` 과 원격 호스트
`/usr/local/bin` 에 복사한다. 2026-09-05 서버가 프로토콜 v3 으로 올라갔을 때 shim
은 v2 그대로였고, 실행 레인이 2시간 18분 동안 전부 죽었다. #33425 가 한 버전
차이를 견디게 했지만, 두 버전 차이나 새 설정 키는 다시 같은 모양으로 죽는다.
배포 단위가 둘인 채로는 이 클래스가 닫히지 않는다.

**(C) judge 가 여전히 실행 다섯 건 중 한 건을 본다.** 319/1,742 = 18%. RFC-0422 의
상자는 오늘 32건을 증명했다. 상자가 살아있는 레인은 16:20Z 부터 전부다(게스트
shim 15:53Z, remote_ssh 테스트베드 16:20Z 교체). 어느 비율까지 내려가는지는
아직 측정 전이다.

**(D) 레인이 죽으면 keeper 는 세 시간 동안 폴링한다.** 보드 스레드 p-d5ed6f05 에
keeper 여덟 명이 "n번째 데이터포인트" 를 쌓았다. 원문은 같았고(`trailer carries
v=2, this build speaks v=3`) 필요한 건 한 줄이었다: "서버와 shim 버전이 다르다.
운영자가 shim 을 바꿔야 한다." 서버는 그 사실을 `run_probe` 에서 이미 알고 있었다.
keeper 가 읽을 자리가 없었을 뿐이다.

## 2. 목표와 측정

| 갈래 | 목표 | 측정 (주간, 로그) |
|---|---|---|
| A 경로 | 게스트·원격 트리를 가진 keeper 의 Read/Grep/Execute 가 호스트 경로를 보지 않는다 | 호스트 확인에서 난 `cwd_missing`+`cwd_not_directory` 0 (엔드포인트가 답한 것은 셈에 넣지 않는다), 알려진-매치 탐침 100% 적중 |
| B 배포 | shim 은 서버 릴리즈의 일부다. 부팅이 게스트 shim 을 놓고, preflight 가 원격 shim 의 해시를 본다 | `remote_shim_version_skew` 0, `microvm_shim_missing` 0, 운영자 복사 절차 삭제 |
| C 통행료 | judge 경유 비율을 18% 에서 측정값 기준으로 내린다 | `source=one_shot_resolution` / 전체 tool_execute, `observed_in_box` 수, `observe run refused` 수 |
| D 진단 | keeper 가 한 도구 호출로 자기 레인 상태와 운영자 조치를 읽는다 | 레인 장애 시 보드 "데이터포인트" 댓글 수, 장애 인지까지 시간 |

## 3. 설계

### 3-A. 트리를 가진 쪽이 경로의 권위다

`Keeper_types_profile_sandbox.tree_location_of_profile` 이 이미 `Endpoint_owned |
Shared_mount` 를 가른다. 읽기 디스패치(`Keeper_sandbox_read_backend.resolve_read_dispatch`)
는 이 값을 쓰고 rg 인자도 `container_path_of_host` 로 변환하지만, 그 앞의 cwd 해석
(`Keeper_tool_execute_path.resolve_tool_read_cwd`)은 여전히 호스트 경로를 만든 뒤 그 존재를
호스트에서 확인한다. Execute 쪽은 #33461 이 옮겼다.

- `Endpoint_owned` 프로필에서는 호스트 파일시스템을 보지 않는다. cwd 는 논리
  경로(keeper 루트 기준 상대 경로)로만 검증하고, 존재 확인은 엔드포인트가 한다
  (shim 이 `cwd` 를 `chdir` 하며 실패하면 `remote_ssh_path_jail_violation` 또는
  ENOENT 를 트레일러로 돌려준다. 이미 그렇게 동작한다).
- rg·cat 등 읽기 명령의 인자는 `Keeper_remote_path` 의 논리→원격 변환을 거친다.
  변환이 없는 경로는 오류다. 호스트 경로가 원격 argv 에 실리는 일은 타입으로
  막는다: 원격 argv 를 만드는 함수는 `Remote_path.t` 만 받는다.
- Grep 의 0 매치는 rg 의 exit 1 과 "검색한 디렉터리가 있었다" 두 사실이 함께 있을
  때만 `matches: []` 다. rg 가 exit 2 를 내면 오류로 돌려준다(지금도 그렇다).
  디렉터리 존재는 같은 요청 안에서 `test -d` 로 확인하지 않고, rg 의 stderr
  `No such file or directory` 를 exit 2 와 함께 읽는다.

검증: 게스트 안에서만 있는 파일에 대한 Read/Grep/Execute 세 도구 시나리오를
`test_keeper_sandbox_read_backend` 와 `test_keeper_tool_filesystem_remote_write` 옆에
둔다. 호스트에 같은 이름의 빈 디렉터리를 두고도 게스트의 결과가 나와야 한다.

### 3-B. shim 은 릴리즈 산출물이고, 설치기가 놓고, 부팅이 검증한다

- 릴리즈 워크플로(`.github/workflows/release.yml`)가 `build-shim.sh` 로
  linux/arm64 와 linux/amd64 정적 shim 을 만들어 릴리즈 자산으로 올린다.
  매트릭스에 이미 `ubuntu-24.04-arm` 러너가 있어 각 Linux 잡이 자기
  아키텍처의 shim 을 만든다 (B-1, #33581).
- `scripts/install.sh` 가 다른 companion 자산을 받는 길로 게스트 shim 도 받아
  `<base>/.masc/microvm/shim/masc-exec-shim` 에 놓고, 옆에
  `masc-exec-shim.sha256` 을 쓴다. 내용은 릴리즈 `SHA256SUMS` 가 그 자산에 준
  줄 그대로(`<hash>  <asset>`). 게스트 아키텍처는 호스트가 macos-arm64 나
  linux-arm64 면 arm64, linux-x64 면 amd64. microvm keeper 를 돌리지 않는
  호스트는 `--no-guest-shim` 으로 건너뛴다.
- microvm 부팅(`prepare_microvm_shim_dir`): 옆에 sha256 파일이 있으면 바이너리의
  해시와 비교한다. 다르면 `microvm_shim_hash_mismatch` 로 부팅을 거절하고
  메시지에 두 해시를 적는다. 파일이 없으면 손으로 빌드한 shim 으로 보고 그대로
  돌리되 부팅 로그에 검증 안 됨을 남긴다. 릴리즈가 안 준 shim 이 조용히 도는
  것이 2026-09-05 장애의 형태였고, 이 검증이 그 형태를 이름으로 막는다.
- 서버 바이너리가 해시 상수를 품는 원안은 버렸다. 그 상수는 릴리즈마다 손으로
  갱신해야 하고(`masc-releases-hand-edit-four-of-six-version-surfaces` 가 그
  비용이다), 서버가 부팅 중 네트워크에서 바이너리를 받아 놓는 것은 부팅을
  릴리즈 서버 가용성에 묶는다. 설치기는 이미 자산을 받고 해시를 검증하는
  자리라 그 일이 거기 놓인다.
- probe 는 sha256 이 아니라 자기가 나온 릴리즈를 적는다(`release`, v3 안의 추가
  필드라 이 키를 모르는 쪽은 그냥 무시한다). 해시로 잡으려던 원안은 엔드포인트마다
  아키텍처가 달라 성립하지 않는다. arm64 호스트가 가진 shim 과 amd64 엔드포인트의
  shim 은 같은 릴리즈여도 절대 같은 해시가 아니다. 릴리즈 문자열은 아키텍처와
  무관하게 두 쪽을 비교한다.
- 서버는 probe 의 릴리즈를 자기 `Build_version.current` 와 견주고, 다르면
  `remote_shim_outdated` 를 WARN 으로 남긴다. 막지는 않는다. 한 버전 차이는
  #33425 로 견디도록 만든 것이고, 릴리즈 차이는 고칠 일이지 레인을 세울 일이 아니다.
  릴리즈를 안 적는 낡은 shim 도 같은 WARN 이다.
- 새 명령 `masc shim install <endpoint>` 는 만들지 않는다. `masc-exec-ssh-bootstrap
  --endpoint <name> --shim <path>` 가 이미 그 일(핀된 채널로 업로드,
  `/usr/local/bin` 설치, probe 기록)을 한다. WARN 문구가 그 명령을 그대로 적는다.
  그 도구가 `--probe` 출력을 `/usr/local/share/masc/exec-shim.version` 에 남기므로,
  이 필드가 생긴 뒤로는 그 파일도 릴리즈를 담는다.
- 운영자 절차 문서(`MICROVM-REMOTE-RUNBOOK.md` "shim 받기")는 설치기가 놓는다는
  한 줄과, 릴리즈 없이 손으로 빌드할 때의 예외만 남긴다.

검증: 릴리즈 잡의 설치기 스모크(`install-smoke.sh`)가 Linux 에서는 shim 을
스테이징해 설치기가 놓은 바이너리와 sidecar 의 해시가 릴리즈 `SHA256SUMS` 와
같은지 확인하고, shim 이 없는 macOS 잡에서는 `--no-guest-shim` 경로를 확인한다.
단위 테스트(`test_keeper_sandbox_microvm`)는 sidecar 있음·없음·불일치·형식
오류 네 경우의 부팅 판정을 고정한다.

### 3-C. 상자가 증명하는 비율을 측정하고, 표를 그 결과로 넓힌다

RFC-0422 §4-2 그대로다. 2026-09-05T16:20Z 부터 이틀 동안
`source=observed_in_box`, `source=one_shot_resolution`, `observe run refused` 를
센다. 그 다음:

- `observe run refused` 의 stderr 를 모아 "게스트 안 쓰기만 한 명령"(RFC-0422
  §1.1 의 264건 부류)이 얼마인지 본다. 그 부류가 절반을 넘으면 keeper 하나를
  `observation_run = "guest_local"` 카나리로 돌린다.
- judge 가 허용한 명령 중 상자가 같은 답을 냈을 것(exit 0 로 관측)의 비율을
  본다. 표(RFC-0404)에 넣을 후보는 그 교집합에서만 고른다. 텍스트 예측을 늘리는
  대신 상자의 판정을 표의 근거로 쓴다.

검증은 숫자 자체다. `scripts/measure-rfc-0427-judge-share.py --since … --until …
<system_log_*.jsonl>` 이 §5 의 행을 만든다(`--selftest` 로 자기 검사). 주간 표를 §5 에
덧붙인다.

### 3-D. keeper 가 읽는 레인 상태는 서버가 이미 아는 것의 투영이다

`Keeper_sandbox_remote.shared_state` 는 엔드포인트마다 probe 결과(major,
capabilities)와 첫 디스패치 여부를 가진다. 여기에 마지막 디스패치의 결과
분류(성공 / 전송 오류 / 버전 오류 / 도달 불가)와 시각을 더하고, 읽기 전용 도구
`keeper_lane_status` 가 그것을 돌려준다.

- 출력은 레인마다 한 줄: 프로필, 엔드포인트, shim 버전과 capability, 마지막
  성공 시각, 마지막 실패 분류와 원문 한 줄, 그리고 실패 분류에 대응하는 운영자
  조치 문장(`remote_shim_version_skew` → "shim 을 다시 놓아야 한다. 운영자
  작업이다"). 조치 문장은 분류 variant 위의 exhaustive match 로, string 분류기가
  아니다.
- 이 도구는 상태를 바꾸지 않고, 다른 keeper 의 레인은 보여 주지 않는다. 함대
  차원의 뷰는 대시보드의 일이다.
- 상태는 authority 가 아니라 projection 이다. 저장하지 않고, 서버 재시작이면
  비어 있다. "알 수 없음" 은 그대로 알 수 없음이다.

검증: `remote_shim_version_skew` 를 내는 stub 엔드포인트에서 도구가 그 분류와
조치 문장을 돌려주는 테스트. 보드에서 데이터포인트 댓글이 사라지는지는 다음
장애 때 본다.

## 4. 순서와 크기

| 단계 | 내용 | 크기 | 선행 |
|---|---|---|---|
| A-1 | `Endpoint_owned` 프로필의 cwd 해석에서 호스트 존재 확인 제거, 논리 경로 검증만 | 3 파일 (#33461, 머지) | 없음 |
| D-1 | `shared_state` 에 마지막 디스패치 분류, `keeper_lane_status` 도구 | 12 파일 (#33472, 머지) | 없음 |
| B-1 | 릴리즈 워크플로에 shim 두 아키텍처 빌드와 자산 업로드 | 3 파일 (#33581, 머지) | 없음 |
| B-2 | 설치기가 shim 과 sha256 sidecar 를 놓고, 부팅이 그 쌍을 검증 | 10 파일 (#33601, 머지) | B-1 |
| B-3 | probe 가 자기 릴리즈를 적고, 서버가 다르면 `remote_shim_outdated` WARN | 15 파일 (#33659) | B-1 |
| C-1 | 이틀 측정표, 카나리 결정 | 문서 | 16:20Z + 48h |
| C-2 | 카나리와 표 후보 | 설정 + 표 | C-1 |

한 단계가 한 Draft PR 이다. A 와 D 는 서로 독립이라 병렬로 간다.

## 5. 측정 기록

| 창 | tool_execute | judge | observed_in_box | refused | unavailable | cwd 오류 | 비고 |
|---|---|---|---|---|---|---|---|
| 09-05 00:00~16:30Z | 1,839 | 319 (17.3%) | 39 | 2 | 6 | 38 | 상자 전. 손으로 센 초안(1,742/319/32/2/38)을 스크립트가 대체 |
| 09-05 하루 | 7,697 | 414 (5.4%) | 1,382 | 87 | 20 | 64 | 16:20Z 부터 전 레인에 상자 |
| 09-05 16:20Z ~ 09-06 05:40Z | 7,829 | 113 (1.4%) | 1,613 | 95 | 20 | 35 | A-1 은 09-06 04:54Z 재시작부터 라이브(그 뒤 cwd 오류 1건은 엔드포인트가 답한 것) |

### B 갈래: shim 이 릴리즈의 일부가 된 뒤

2026-09-06 15:14Z 에 호스트의 게스트 shim 과 rondo-remote 테스트베드
(`sbx-sshd-probe` 컨테이너의 `/usr/local/bin`) 를 main 에서 빌드한 shim 으로 바꿨다.
probe 응답은 `{"version":"3.0.0+4eca0168","capabilities":["observe"],"release":"0.33.0"}`.
그 전에 설치돼 있던 것은 릴리즈를 적기 전 빌드였다.

| 지표 | 교체 전 (09-06 하루) | 교체·재시작 이후 (15:16Z~15:41Z) |
|---|---|---|
| `remote_shim_outdated` | 107 (엔드포인트마다 약 80초 간격) | 0 |
| `remote_shim_version_skew` | 0 | 0 |
| `microvm_shim_missing` | 0 | 0 |
| `microvm_shim_hash_mismatch` | 0 | 0 |
| sidecar 검증 성공 | 93 (옛 해시) | 8 (새 해시) |
| authorized tool_execute | — | 46, 레인 실패 0 |

§2 가 B 에 건 기준은 `remote_shim_version_skew` 0, `microvm_shim_missing` 0,
운영자 복사 절차 삭제였다. 앞의 둘은 위 표이고, 세 번째는 B-2 가 설치기로 옮기면서
런북에서 없앴다. `remote_shim_outdated` 는 B-3 이 새로 만든 신호이고, 그것이 가리킨
수리를 그대로 했더니 0 이 됐다.

## 6. 하지 않는 것

- 표(RFC-0404)와 셸 IR(RFC-0421)에 텍스트 규칙을 더 얹는 일. 상자의 판정이 근거가
  되기 전에는 넓히지 않는다.
- 레인 상태를 저장하거나, 그것으로 스케줄링을 막는 Gate. 투영만 한다.
- microsandbox 백엔드 살리기. #32837 과 #33431 은 별건이다.
- 죽어 있는 remote_ssh 엔드포인트 다섯 개(127.0.0.1:2222, :22222)를 살리는 일.
  다시 쓸 때 `masc-exec-ssh-bootstrap --endpoint <name> --shim <릴리즈 자산>` 으로 붙인다.
- Grep 의 0 매치 판정을 더 조이는 일. 이미 갈라져 있다. rg 15.1.0 은 없는 경로에
  2, 빈 디렉터리에 1 로 끝나고(실측), 읽기 op 는 2 를 실패로 보고 `error_detail`
  을 붙인다. 죽은 레인은 `classify_read_outcome` 이 언제나 오류로 만든다. exit 1
  을 받아들이는 Grep 레인이 죽은 레인을 빈 성공으로 읽지 않게 하려고 그렇게 쓴
  것이고, 차등 테스트가 그걸 고정한다. 남는 경우는 "있지만 비어 있는 트리에서
  0 매치" 하나인데, 그건 참인 답이다. keeper 가 그걸로 헤맸다는 증거가 나오면
  그때 rg 가 몇 개 파일을 뒤졌는지 같이 돌려주는 쪽으로 연다.
- 원격 argv 를 `Remote_path.t` 로 좁히는 일. `run_readonly_in_sandbox` 를 부르는
  자리는 트리 전체에 하나뿐이고, 그 자리는 이미 변환된 경로를 쓴다. 타입을 더
  아래로 내리려면 keeper 의 타입이 `Masc_exec.Sandbox_target.runner` 까지 내려가야
  하는데, 그건 아래 계층이 위 계층을 알게 되는 일이다. 세 줄을 지키자고 계층
  방향을 뒤집지 않는다.
