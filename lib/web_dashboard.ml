(** MASC Web Dashboard - Real-time Agent Coordination Visualization

    HTTP endpoint: /dashboard
    SSE events: /sse (existing)

    Shows:
    - Active agents with status indicators
    - Task board (Kanban style)
    - Recent broadcasts
    - File locks
    - Tempo status

    @author MASC-MCP
    @since 2026-01
*)

(** Generate the dashboard HTML page *)
let html () = {|<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>MASC Dashboard</title>
  <style>
    * { box-sizing: border-box; margin: 0; padding: 0; }
    body {
      font-family: -apple-system, BlinkMacSystemFont, 'SF Pro', 'Segoe UI', sans-serif;
      background: linear-gradient(135deg, #0f0c29 0%, #1a1a2e 50%, #16213e 100%);
      color: #e0e0e0;
      min-height: 100vh;
      padding: 20px;
    }
    .container { max-width: 1400px; margin: 0 auto; }

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
      background: linear-gradient(90deg, #4ade80, #22d3ee);
      -webkit-background-clip: text;
      -webkit-text-fill-color: transparent;
      display: flex;
      align-items: center;
      gap: 10px;
    }
    .status-dot {
      width: 12px; height: 12px;
      border-radius: 50%;
      background: #666;
      animation: pulse 2s infinite;
    }
    .status-dot.connected { background: #4ade80; }
    @keyframes pulse { 0%, 100% { opacity: 1; } 50% { opacity: 0.5; } }

    /* Stats Grid */
    .stats-grid {
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
      gap: 15px;
      margin-bottom: 20px;
    }
    .stat-card {
      background: rgba(255,255,255,0.05);
      border-radius: 12px;
      padding: 20px;
      border: 1px solid rgba(255,255,255,0.1);
    }
    .stat-label { font-size: 12px; color: #888; text-transform: uppercase; }
    .stat-value { font-size: 32px; font-weight: bold; color: #4ade80; }

    /* Sections */
    .grid-2col {
      display: grid;
      grid-template-columns: 1fr 1fr;
      gap: 20px;
      margin-bottom: 20px;
    }
    @media (max-width: 900px) { .grid-2col { grid-template-columns: 1fr; } }

    .section {
      background: rgba(255,255,255,0.03);
      border-radius: 12px;
      padding: 20px;
      border: 1px solid rgba(255,255,255,0.08);
    }
    .section h2 {
      font-size: 14px;
      color: #4ade80;
      text-transform: uppercase;
      letter-spacing: 1px;
      margin-bottom: 15px;
      display: flex;
      align-items: center;
      gap: 8px;
    }

    /* Agents */
    .agent-list { display: flex; flex-direction: column; gap: 10px; }
    .agent {
      display: flex;
      align-items: center;
      gap: 12px;
      padding: 12px;
      background: rgba(255,255,255,0.05);
      border-radius: 8px;
    }
    .agent-status {
      width: 10px; height: 10px;
      border-radius: 50%;
    }
    .agent-status.active { background: #4ade80; box-shadow: 0 0 10px #4ade80; }
    .agent-status.busy { background: #fbbf24; box-shadow: 0 0 10px #fbbf24; }
    .agent-status.inactive { background: #666; }
    .agent-name { font-weight: 600; flex: 1; }
    .agent-task { font-size: 12px; color: #888; max-width: 200px; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }

    /* Tasks (Kanban) */
    .task-board {
      display: grid;
      grid-template-columns: repeat(3, 1fr);
      gap: 15px;
    }
    .task-column h3 {
      font-size: 12px;
      color: #888;
      margin-bottom: 10px;
      display: flex;
      align-items: center;
      gap: 6px;
    }
    .task-column h3 .count {
      background: rgba(255,255,255,0.1);
      padding: 2px 8px;
      border-radius: 10px;
      font-size: 10px;
    }
    .task-list { display: flex; flex-direction: column; gap: 8px; min-height: 100px; }
    .task {
      padding: 10px;
      background: rgba(255,255,255,0.05);
      border-radius: 6px;
      border-left: 3px solid #666;
      font-size: 13px;
    }
    .task.todo { border-left-color: #888; }
    .task.in-progress { border-left-color: #fbbf24; }
    .task.done { border-left-color: #4ade80; opacity: 0.7; }
    .task-title { font-weight: 500; margin-bottom: 4px; }
    .task-meta { font-size: 11px; color: #666; }

    /* Messages */
    .message-list { display: flex; flex-direction: column; gap: 8px; max-height: 300px; overflow-y: auto; }
    .message {
      padding: 10px;
      background: rgba(255,255,255,0.03);
      border-radius: 6px;
      font-size: 13px;
    }
    .message-header { display: flex; justify-content: space-between; margin-bottom: 4px; }
    .message-from { color: #22d3ee; font-weight: 500; }
    .message-time { color: #666; font-size: 11px; }
    .message-content { color: #ccc; }

    /* Tempo */
    .tempo-badge {
      display: inline-flex;
      align-items: center;
      gap: 6px;
      padding: 6px 12px;
      border-radius: 20px;
      font-size: 12px;
      font-weight: 600;
    }
    .tempo-badge.normal { background: rgba(74,222,128,0.2); color: #4ade80; }
    .tempo-badge.slow { background: rgba(251,191,36,0.2); color: #fbbf24; }
    .tempo-badge.fast { background: rgba(34,211,238,0.2); color: #22d3ee; }
    .tempo-badge.paused { background: rgba(248,113,113,0.2); color: #f87171; }

    /* Empty state */
    .empty { color: #666; font-style: italic; padding: 20px; text-align: center; }
  </style>
</head>
<body>
  <div class="container">
    <header>
      <h1><span class="status-dot" id="status-dot"></span> MASC Dashboard</h1>
      <div class="tempo-badge normal" id="tempo-badge">Normal</div>
    </header>

    <div class="stats-grid" id="stats-grid">
      <div class="stat-card">
        <div class="stat-label">Agents</div>
        <div class="stat-value" id="stat-agents">-</div>
      </div>
      <div class="stat-card">
        <div class="stat-label">Tasks</div>
        <div class="stat-value" id="stat-tasks">-</div>
      </div>
      <div class="stat-card">
        <div class="stat-label">In Progress</div>
        <div class="stat-value" id="stat-in-progress">-</div>
      </div>
      <div class="stat-card">
        <div class="stat-label">Status</div>
        <div class="stat-value" id="stat-locks">-</div>
      </div>
    </div>

    <div class="grid-2col">
      <div class="section">
        <h2>Agents</h2>
        <div class="agent-list" id="agent-list">
          <div class="empty">No agents connected</div>
        </div>
      </div>

      <div class="section">
        <h2>Recent Broadcasts</h2>
        <div class="message-list" id="message-list">
          <div class="empty">No recent messages</div>
        </div>
      </div>
    </div>

    <div class="section">
      <h2>Task Board</h2>
      <div class="task-board">
        <div class="task-column">
          <h3>Todo <span class="count" id="todo-count">0</span></h3>
          <div class="task-list" id="todo-list"></div>
        </div>
        <div class="task-column">
          <h3>In Progress <span class="count" id="progress-count">0</span></h3>
          <div class="task-list" id="progress-list"></div>
        </div>
        <div class="task-column">
          <h3>Done <span class="count" id="done-count">0</span></h3>
          <div class="task-list" id="done-list"></div>
        </div>
      </div>
    </div>
  </div>

  <script>
    const statusDot = document.getElementById('status-dot');
    const tempoBadge = document.getElementById('tempo-badge');

    // REST API helper
    async function apiCall(endpoint) {
      const res = await fetch('/api/v1/' + endpoint);
      return res.json();
    }

    // Fetch initial data via REST API
    async function fetchData() {
      try {
        const [status, tasks, agents, msgs] = await Promise.all([
          apiCall('status'),
          apiCall('tasks'),
          apiCall('agents'),
          apiCall('messages')
        ]);

        updateStats(agents, tasks, status);
        updateAgents(agents);
        updateTasks(tasks);
        updateMessages(msgs);
        updateTempo(status);
      } catch (e) {
        console.error('Fetch error:', e);
      }
    }

    function updateStats(agents, tasks, status) {
      const agentList = agents.agents || [];
      const taskList = tasks.tasks || [];
      document.getElementById('stat-agents').textContent = agentList.length;
      document.getElementById('stat-tasks').textContent = taskList.length;
      document.getElementById('stat-in-progress').textContent =
        taskList.filter(t => t.status === 'in_progress' || t.status === 'claimed').length;
      document.getElementById('stat-locks').textContent = status.paused ? '⏸' : '✓';
    }

    function updateAgents(data) {
      const list = document.getElementById('agent-list');
      const agents = data.agents || [];
      if (agents.length === 0) {
        list.innerHTML = '<div class="empty">No agents connected</div>';
        return;
      }
      list.innerHTML = agents.map(a => `
        <div class="agent">
          <div class="agent-status ${a.status || 'inactive'}"></div>
          <div class="agent-name">${a.name || a}</div>
          <div class="agent-task">${a.current_task || ''}</div>
        </div>
      `).join('');
    }

    function updateTasks(data) {
      const tasks = data.tasks || [];
      const todo = tasks.filter(t => t.status === 'todo');
      const progress = tasks.filter(t => t.status === 'in_progress' || t.status === 'claimed');
      const done = tasks.filter(t => t.status === 'done').slice(0, 5);

      document.getElementById('todo-count').textContent = todo.length;
      document.getElementById('progress-count').textContent = progress.length;
      document.getElementById('done-count').textContent = done.length;

      document.getElementById('todo-list').innerHTML = todo.slice(0, 5).map(t => `
        <div class="task todo">
          <div class="task-title">${t.title || t.id}</div>
          <div class="task-meta">Priority: ${t.priority || 3}</div>
        </div>
      `).join('') || '<div class="empty">No tasks</div>';

      document.getElementById('progress-list').innerHTML = progress.map(t => `
        <div class="task in-progress">
          <div class="task-title">${t.title || t.id}</div>
          <div class="task-meta">${t.assignee || 'Unassigned'}</div>
        </div>
      `).join('') || '<div class="empty">No tasks</div>';

      document.getElementById('done-list').innerHTML = done.map(t => `
        <div class="task done">
          <div class="task-title">${t.title || t.id}</div>
        </div>
      `).join('') || '<div class="empty">No tasks</div>';
    }

    function updateMessages(data) {
      const list = document.getElementById('message-list');
      const msgs = data.messages || [];
      if (msgs.length === 0) {
        list.innerHTML = '<div class="empty">No recent messages</div>';
        return;
      }
      list.innerHTML = msgs.slice(0, 10).map(m => `
        <div class="message">
          <div class="message-header">
            <span class="message-from">${m.from || m.from_agent || 'Unknown'}</span>
            <span class="message-time">${new Date(m.timestamp || Date.now()).toLocaleTimeString()}</span>
          </div>
          <div class="message-content">${m.content || m.message || ''}</div>
        </div>
      `).join('');
    }

    function updateTempo(status) {
      // Convert tempo_interval_s to mode: <120s=fast, <400s=normal, else=slow
      const interval = status.tempo_interval_s || 300;
      let mode = 'normal';
      if (status.paused) mode = 'paused';
      else if (interval < 120) mode = 'fast';
      else if (interval > 400) mode = 'slow';
      tempoBadge.className = 'tempo-badge ' + mode;
      tempoBadge.textContent = mode.charAt(0).toUpperCase() + mode.slice(1) + ' (' + Math.round(interval) + 's)';
    }

    // SSE for real-time updates
    function connectSSE() {
      const es = new EventSource('/sse');
      es.onopen = () => {
        statusDot.classList.add('connected');
        console.log('SSE connected');
      };
      es.onerror = () => {
        statusDot.classList.remove('connected');
        setTimeout(connectSSE, 3000);
      };
      es.onmessage = (e) => {
        try {
          const event = JSON.parse(e.data);
          handleEvent(event);
        } catch (err) {
          console.log('SSE:', e.data);
        }
      };
    }

    function handleEvent(event) {
      const type = event.type || event.event;
      if (type === 'broadcast' || type === 'agent_joined' || type === 'agent_left' ||
          type === 'task_update' || type === 'tempo_change') {
        fetchData(); // Refresh on relevant events
      }
    }

    // Initial load + polling fallback
    fetchData();
    connectSSE();
    setInterval(fetchData, 5000); // Fallback polling
  </script>
</body>
</html>|}
