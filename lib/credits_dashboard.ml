(** Credits Dashboard - AI Service Usage Monitoring

    HTTP endpoints:
    - /dashboard/credits - Visual HTML dashboard
    - /api/v1/credits - JSON API

    Monitors:
    - Claude Max (4 accounts)
    - ChatGPT Pro (2 accounts)
    - ElevenLabs, RunPod, Railway, Anthropic API, etc.

    @author MASC-MCP
    @since 2026-01
*)

(** Get ME_ROOT path *)
let me_root () =
  match Sys.getenv_opt "ME_ROOT" with
  | Some path -> path
  | None -> Filename.concat (Sys.getenv "HOME") "me"

(** Credits JSON path *)
let credits_json_path () =
  Filename.concat (me_root ()) "data/state/credits.json"

(** Read credits.json *)
let read_credits_json () : Yojson.Safe.t option =
  let path = credits_json_path () in
  if Sys.file_exists path then
    try Some (Yojson.Safe.from_file path)
    with _ -> None
  else None

(** Get credits JSON as string *)
let json_api () : string =
  match read_credits_json () with
  | Some json -> Yojson.Safe.to_string json
  | None -> {|{"error": "credits.json not found"}|}

(** Color for percentage (CSS class) *)
let color_class pct =
  if pct >= 70.0 then "green"
  else if pct >= 30.0 then "yellow"
  else "red"

(** Color for balance *)
let balance_class bal =
  if bal >= 50.0 then "green"
  else if bal >= 20.0 then "yellow"
  else "red"

(** Generate the dashboard HTML page *)
let html () = {|<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Credits Dashboard</title>
  <style>
    * { box-sizing: border-box; margin: 0; padding: 0; }
    body {
      font-family: -apple-system, BlinkMacSystemFont, 'SF Pro', 'Segoe UI', sans-serif;
      background: linear-gradient(135deg, #1a1a2e 0%, #16213e 50%, #0f3460 100%);
      color: #e0e0e0;
      min-height: 100vh;
      padding: 20px;
    }
    .container { max-width: 1200px; margin: 0 auto; }

    /* Header */
    header {
      display: flex;
      justify-content: space-between;
      align-items: center;
      padding: 15px 0;
      border-bottom: 1px solid #333;
      margin-bottom: 20px;
    }
    h1 {
      font-size: 24px;
      background: linear-gradient(90deg, #f472b6, #a78bfa);
      -webkit-background-clip: text;
      -webkit-text-fill-color: transparent;
    }
    .updated { font-size: 12px; color: #888; }

    /* Cards Grid */
    .cards-grid {
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(280px, 1fr));
      gap: 16px;
      margin-bottom: 20px;
    }

    /* Service Card */
    .card {
      background: rgba(255,255,255,0.05);
      border-radius: 12px;
      padding: 16px;
      border: 1px solid rgba(255,255,255,0.1);
    }
    .card-header {
      display: flex;
      justify-content: space-between;
      align-items: center;
      margin-bottom: 12px;
    }
    .card-title {
      font-size: 14px;
      font-weight: 600;
      color: #fff;
      display: flex;
      align-items: center;
      gap: 8px;
    }
    .card-title .icon { font-size: 18px; }
    .card-source {
      font-size: 10px;
      padding: 2px 6px;
      border-radius: 4px;
      background: rgba(255,255,255,0.1);
      color: #888;
    }

    /* Progress Bar */
    .progress-container {
      height: 8px;
      background: rgba(255,255,255,0.1);
      border-radius: 4px;
      overflow: hidden;
      margin: 8px 0;
    }
    .progress-bar {
      height: 100%;
      border-radius: 4px;
      transition: width 0.3s ease;
    }
    .progress-bar.green { background: linear-gradient(90deg, #4ade80, #22c55e); }
    .progress-bar.yellow { background: linear-gradient(90deg, #facc15, #eab308); }
    .progress-bar.red { background: linear-gradient(90deg, #f87171, #ef4444); }

    /* Values */
    .value-row {
      display: flex;
      justify-content: space-between;
      margin: 4px 0;
    }
    .value-label { font-size: 12px; color: #888; }
    .value-amount { font-size: 14px; font-weight: 500; }
    .value-amount.green { color: #4ade80; }
    .value-amount.yellow { color: #facc15; }
    .value-amount.red { color: #f87171; }
    .value-amount.large { font-size: 24px; }

    /* Account Sub-items */
    .account-item {
      padding: 8px 0;
      border-top: 1px solid rgba(255,255,255,0.05);
    }
    .account-item:first-child { border-top: none; }
    .account-email { font-size: 11px; color: #a5b4fc; margin-bottom: 4px; }

    /* Sections */
    .section-title {
      font-size: 16px;
      font-weight: 600;
      margin: 24px 0 12px;
      color: #a5b4fc;
    }

    /* Footer */
    footer {
      text-align: center;
      padding: 20px;
      color: #666;
      font-size: 12px;
    }
    footer a { color: #a5b4fc; text-decoration: none; }

    /* Auto-refresh indicator */
    .refresh-indicator {
      position: fixed;
      bottom: 20px;
      right: 20px;
      background: rgba(0,0,0,0.7);
      padding: 8px 12px;
      border-radius: 8px;
      font-size: 11px;
      color: #888;
    }
    .refresh-indicator.loading { color: #4ade80; }

    /* Mobile */
    @media (max-width: 600px) {
      .cards-grid { grid-template-columns: 1fr; }
      .card { padding: 12px; }
    }
  </style>
</head>
<body>
  <div class="container">
    <header>
      <h1>📊 Credits Dashboard</h1>
      <div class="updated" id="updated">Loading...</div>
    </header>

    <!-- Summary Cards -->
    <div class="cards-grid" id="cards"></div>

    <!-- Claude Max Section -->
    <div class="section-title">🟣 Claude Max Accounts</div>
    <div class="cards-grid" id="claude-cards"></div>

    <!-- Other Services -->
    <div class="section-title">🔧 Other Services</div>
    <div class="cards-grid" id="other-cards"></div>

    <footer>
      <a href="/dashboard">← MASC Dashboard</a> |
      <a href="/api/v1/credits">JSON API</a> |
      Refresh: 30s
    </footer>
  </div>

  <div class="refresh-indicator" id="refresh-indicator">Auto-refresh: 30s</div>

  <script>
    let refreshTimer = 30;

    async function fetchCredits() {
      try {
        document.getElementById('refresh-indicator').classList.add('loading');
        document.getElementById('refresh-indicator').textContent = 'Refreshing...';

        const resp = await fetch('/api/v1/credits');
        const data = await resp.json();
        renderDashboard(data);

        document.getElementById('refresh-indicator').classList.remove('loading');
        refreshTimer = 30;
      } catch (e) {
        console.error('Failed to fetch credits:', e);
        document.getElementById('refresh-indicator').textContent = 'Error: ' + e.message;
      }
    }

    function colorClass(pct) {
      if (pct >= 70) return 'green';
      if (pct >= 30) return 'yellow';
      return 'red';
    }

    function balanceClass(bal) {
      if (bal >= 50) return 'green';
      if (bal >= 20) return 'yellow';
      return 'red';
    }

    function formatNumber(n) {
      return new Intl.NumberFormat().format(n);
    }

    function renderDashboard(data) {
      const updated = data.updated_at ? new Date(data.updated_at).toLocaleString() : 'Unknown';
      document.getElementById('updated').textContent = 'Updated: ' + updated;

      const services = data.services || {};
      const cardsEl = document.getElementById('cards');
      const claudeEl = document.getElementById('claude-cards');
      const otherEl = document.getElementById('other-cards');

      cardsEl.innerHTML = '';
      claudeEl.innerHTML = '';
      otherEl.innerHTML = '';

      // Summary cards
      const summaryCards = [];

      // ElevenLabs
      if (services.elevenlabs) {
        const el = services.elevenlabs;
        const pct = el.percent_remaining || 0;
        summaryCards.push(`
          <div class="card">
            <div class="card-header">
              <div class="card-title"><span class="icon">🔊</span> ElevenLabs</div>
              <span class="card-source">${el.source || 'api'}</span>
            </div>
            <div class="progress-container">
              <div class="progress-bar ${colorClass(pct)}" style="width: ${pct}%"></div>
            </div>
            <div class="value-row">
              <span class="value-label">Characters</span>
              <span class="value-amount">${formatNumber(el.remaining || 0)} / ${formatNumber(el.limit || 0)}</span>
            </div>
          </div>
        `);
      }

      // RunPod
      if (services.runpod) {
        const rp = services.runpod;
        summaryCards.push(`
          <div class="card">
            <div class="card-header">
              <div class="card-title"><span class="icon">🚀</span> RunPod</div>
              <span class="card-source">${rp.source || 'api'}</span>
            </div>
            <div class="value-row">
              <span class="value-label">Balance</span>
              <span class="value-amount large ${balanceClass(rp.balance || 0)}">$${(rp.balance || 0).toFixed(2)}</span>
            </div>
            <div class="value-row">
              <span class="value-label">Hours Remaining</span>
              <span class="value-amount">${rp.hours_remaining || 0}h</span>
            </div>
          </div>
        `);
      }

      // Anthropic API
      if (services.anthropic) {
        const an = services.anthropic;
        summaryCards.push(`
          <div class="card">
            <div class="card-header">
              <div class="card-title"><span class="icon">🤖</span> Anthropic API</div>
              <span class="card-source">${an.source || 'browser'}</span>
            </div>
            <div class="value-row">
              <span class="value-label">Balance</span>
              <span class="value-amount large ${balanceClass(an.balance || 0)}">$${(an.balance || 0).toFixed(2)}</span>
            </div>
          </div>
        `);
      }

      // Railway
      if (services.railway) {
        const rl = services.railway;
        summaryCards.push(`
          <div class="card">
            <div class="card-header">
              <div class="card-title"><span class="icon">🚂</span> Railway</div>
              <span class="card-source">${rl.source || 'browser'}</span>
            </div>
            <div class="value-row">
              <span class="value-label">Estimated Bill</span>
              <span class="value-amount large">$${(rl.estimated_bill || 0).toFixed(2)}</span>
            </div>
            <div class="value-row">
              <span class="value-label">Period</span>
              <span class="value-amount">${rl.period || 'N/A'}</span>
            </div>
          </div>
        `);
      }

      cardsEl.innerHTML = summaryCards.join('');

      // Claude Max accounts
      const claudeMax = services.claude_max || {};
      const claudeCards = [];
      for (const [key, acc] of Object.entries(claudeMax)) {
        if (key === 'source' || key === 'last_updated') continue;
        const weeklyPct = acc.weekly_usage_all_models || 0;
        const extraPct = acc.extra_usage_percent || 0;
        claudeCards.push(`
          <div class="card">
            <div class="card-header">
              <div class="card-title"><span class="icon">🟣</span> Claude Max</div>
              <span class="card-source">${acc.plan || 'Max'}</span>
            </div>
            <div class="account-email">${acc.email || key}</div>
            <div class="value-row">
              <span class="value-label">Weekly (All)</span>
              <span class="value-amount ${colorClass(100 - weeklyPct)}">${weeklyPct}%</span>
            </div>
            <div class="progress-container">
              <div class="progress-bar ${colorClass(100 - weeklyPct)}" style="width: ${weeklyPct}%"></div>
            </div>
            <div class="value-row">
              <span class="value-label">Extra Usage</span>
              <span class="value-amount ${colorClass(100 - extraPct)}">${extraPct}%</span>
            </div>
            <div class="value-row">
              <span class="value-label">Extra Balance</span>
              <span class="value-amount ${balanceClass(acc.extra_balance || 0)}">$${(acc.extra_balance || 0).toFixed(2)}</span>
            </div>
          </div>
        `);
      }
      claudeEl.innerHTML = claudeCards.join('');

      // Other services
      const otherCards = [];

      // OpenAI
      if (services.openai) {
        const oi = services.openai;
        otherCards.push(`
          <div class="card">
            <div class="card-header">
              <div class="card-title"><span class="icon">💚</span> OpenAI API</div>
              <span class="card-source">${oi.source || 'manual'}</span>
            </div>
            <div class="value-row">
              <span class="value-label">Balance</span>
              <span class="value-amount large ${balanceClass(oi.balance || 0)}">$${(oi.balance || 0).toFixed(2)}</span>
            </div>
          </div>
        `);
      }

      // Codex Pro
      if (services.codex_pro) {
        const cx = services.codex_pro;
        otherCards.push(`
          <div class="card">
            <div class="card-header">
              <div class="card-title"><span class="icon">🔷</span> Codex Pro</div>
              <span class="card-source">${cx.source || 'manual'}</span>
            </div>
            <div class="value-row">
              <span class="value-label">Credits</span>
              <span class="value-amount large">${cx.credits || 0}</span>
            </div>
          </div>
        `);
      }

      // ChatGPT Pro
      const chatgpt = services.chatgpt_pro || {};
      if (chatgpt.accounts) {
        for (const [email, acc] of Object.entries(chatgpt.accounts)) {
          otherCards.push(`
            <div class="card">
              <div class="card-header">
                <div class="card-title"><span class="icon">💬</span> ChatGPT Pro</div>
                <span class="card-source">${acc.status || 'active'}</span>
              </div>
              <div class="account-email">${email}</div>
              <div class="value-row">
                <span class="value-label">Plan</span>
                <span class="value-amount">${acc.plan || 'Pro'}</span>
              </div>
            </div>
          `);
        }
      }

      // Z.ai GLM Max
      if (services.zai_glm_max) {
        const zai = services.zai_glm_max;
        const pct = zai.credits_limit > 0 ? (zai.credits_remaining / zai.credits_limit * 100) : 0;
        otherCards.push(`
          <div class="card">
            <div class="card-header">
              <div class="card-title"><span class="icon">🔶</span> Z.ai GLM Max</div>
              <span class="card-source">${zai.source || 'browser'}</span>
            </div>
            <div class="progress-container">
              <div class="progress-bar ${colorClass(pct)}" style="width: ${pct}%"></div>
            </div>
            <div class="value-row">
              <span class="value-label">Credits</span>
              <span class="value-amount">${formatNumber(zai.credits_remaining || 0)} / ${formatNumber(zai.credits_limit || 0)}</span>
            </div>
          </div>
        `);
      }

      // Google Ultra
      if (services.google_ultra) {
        const gu = services.google_ultra;
        otherCards.push(`
          <div class="card">
            <div class="card-header">
              <div class="card-title"><span class="icon">🔴</span> Google Ultra</div>
              <span class="card-source">${gu.status || 'active'}</span>
            </div>
            <div class="value-row">
              <span class="value-label">Plan</span>
              <span class="value-amount">${gu.plan || 'Google One AI Premium'}</span>
            </div>
          </div>
        `);
      }

      // Gemini
      if (services.gemini) {
        const gem = services.gemini;
        otherCards.push(`
          <div class="card">
            <div class="card-header">
              <div class="card-title"><span class="icon">💎</span> Gemini API</div>
              <span class="card-source">${gem.source || 'browser'}</span>
            </div>
            <div class="value-row">
              <span class="value-label">Cost (KRW)</span>
              <span class="value-amount large">₩${formatNumber(gem.cost_krw || 0)}</span>
            </div>
            <div class="value-row">
              <span class="value-label">Period</span>
              <span class="value-amount">${gem.period || 'N/A'}</span>
            </div>
          </div>
        `);
      }

      otherEl.innerHTML = otherCards.join('');
    }

    // Initial fetch
    fetchCredits();

    // Auto-refresh every 30s
    setInterval(() => {
      refreshTimer--;
      if (refreshTimer <= 0) {
        fetchCredits();
      } else {
        document.getElementById('refresh-indicator').textContent = 'Auto-refresh: ' + refreshTimer + 's';
      }
    }, 1000);
  </script>
</body>
</html>|}
