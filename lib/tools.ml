(** MCP Tool Definitions for MASC *)

open Types

(** All MASC tool schemas *)
let all_schemas : tool_schema list = [
  {
    name = "masc_set_room";
    description = "Set the working directory for MASC operations. Use this to work with .masc/ in a different project.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("path", `Assoc [
          ("type", `String "string");
          ("description", `String "Absolute or relative path to the project directory");
        ]);
      ]);
      ("required", `List [`String "path"]);
    ];
  };

  {
    name = "masc_init";
    description = "Initialize MASC room for multi-agent collaboration. Creates .masc/ folder in current project. Call this ONCE at the start of a multi-agent session. If .masc/ already exists, you'll auto-join. Workflow: init → join → (claim tasks / broadcast / portal) → leave";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("agent_name", `Assoc [
          ("type", `String "string");
          ("description", `String "Your agent identity: 'claude' (Claude Code), 'gemini' (Gemini CLI), or 'codex' (Codex CLI)");
        ]);
      ]);
    ];
  };

  {
    name = "masc_join";
    description = "Join the MASC room/cluster to collaborate with other AI agents. A 'room' is defined by shared .masc/ folder (FS mode) or same PostgreSQL + MASC_CLUSTER_NAME (distributed mode). Call at session start. Your presence will be visible to other agents (gemini, codex, etc). They can @mention you for help. Check masc_status after joining to see active agents and available tasks.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("agent_name", `Assoc [
          ("type", `String "string");
          ("description", `String "Your identity: 'claude', 'gemini', or 'codex'");
        ]);
        ("capabilities", `Assoc [
          ("type", `String "array");
          ("items", `Assoc [("type", `String "string")]);
          ("description", `String "Your strengths (e.g., ['typescript', 'code-review', 'testing'])");
        ]);
      ]);
      ("required", `List [`String "agent_name"]);
    ];
  };

  {
    name = "masc_leave";
    description = "Leave the MASC room and mark yourself as offline. \
Call when: (1) session ends, (2) switching rooms, (3) work complete. \
Side effects: releases all your locks, sets presence to offline. \
Other agents will see you've left via SSE. \
Example: masc_leave({agent_name: 'claude-xyz'})";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("agent_name", `Assoc [
          ("type", `String "string");
          ("description", `String "Your agent name");
        ]);
      ]);
      ("required", `List [`String "agent_name"]);
    ];
  };

  {
    name = "masc_status";
    description = "Get current room/cluster status: active agents with capabilities, task queue, recent broadcasts, and cluster info. Shows cluster name (from MASC_CLUSTER_NAME or basename of ME_ROOT) and storage backend (fs or postgres).";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc []);
    ];
  };

  {
    name = "masc_pause";
    description = "Pause the MASC room. Stops orchestrator from spawning new agents. Broadcasts notification to all agents. Use when you need to stop automated work temporarily.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("reason", `Assoc [
          ("type", `String "string");
          ("description", `String "Reason for pausing (e.g., 'Need to review', 'Taking a break')");
          ("default", `String "Manual pause");
        ]);
      ]);
    ];
  };

  {
    name = "masc_resume";
    description = "Resume the MASC room after pause. Allows orchestrator to spawn agents again. Broadcasts notification to all agents.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc []);
    ];
  };

  {
    name = "masc_pause_status";
    description = "Check if the room is currently paused and get pause details.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc []);
    ];
  };

  {
    name = "masc_add_task";
    description = "Add a new task to the backlog for agents to claim. \
Tasks have status flow: todo → claimed → done/cancelled. \
Priority 1=urgent, 5=low (default 3). \
Returns task-XXX ID for tracking. \
Example: masc_add_task({title: 'Fix login bug', priority: 1, description: 'Users cannot login with SSO'})";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("title", `Assoc [
          ("type", `String "string");
          ("description", `String "Task title");
        ]);
        ("priority", `Assoc [
          ("type", `String "integer");
          ("description", `String "Priority 1-5 (1=highest)");
          ("default", `Int 3);
        ]);
        ("description", `Assoc [
          ("type", `String "string");
          ("description", `String "Task description");
        ]);
      ]);
      ("required", `List [`String "title"]);
    ];
  };

  {
    name = "masc_batch_add_tasks";
    description = "Add multiple tasks in one call (more efficient than repeated masc_add_task). \
Use when: loading sprint backlog, importing from JIRA, creating related tasks. \
Each task gets unique ID (task-XXX). Atomic: all succeed or all fail. \
Example: masc_batch_add_tasks({tasks: [{title: 'Task A', priority: 2}, {title: 'Task B'}]})";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("tasks", `Assoc [
          ("type", `String "array");
          ("items", `Assoc [
            ("type", `String "object");
            ("properties", `Assoc [
              ("title", `Assoc [
                ("type", `String "string");
                ("description", `String "Task title");
              ]);
              ("priority", `Assoc [
                ("type", `String "integer");
                ("description", `String "Priority 1-5 (1=highest)");
                ("default", `Int 3);
              ]);
              ("description", `Assoc [
                ("type", `String "string");
                ("description", `String "Task description");
              ]);
            ]);
            ("required", `List [`String "title"]);
          ]);
          ("description", `String "List of tasks to add");
        ]);
      ]);
      ("required", `List [`String "tasks"]);
    ];
  };

  {
    name = "masc_claim";
    description = "Claim a task from the backlog BEFORE starting work. This prevents other agents from working on the same task (collision avoidance). Prefer masc_transition(action='claim') for CAS-guarded transitions.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("agent_name", `Assoc [
          ("type", `String "string");
          ("description", `String "Your agent name");
        ]);
        ("task_id", `Assoc [
          ("type", `String "string");
          ("description", `String "Task ID (e.g., 'task-001')");
        ]);
      ]);
      ("required", `List [`String "agent_name"; `String "task_id"]);
    ];
  };

  {
    name = "masc_transition";
    description = "Unified task state transition (single entrypoint). Actions: claim, start, done, cancel, release. Supports CAS via expected_version (backlog.version). Use notes for done, reason for cancel.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("agent_name", `Assoc [
          ("type", `String "string");
          ("description", `String "Your agent name");
        ]);
        ("task_id", `Assoc [
          ("type", `String "string");
          ("description", `String "Task ID (e.g., 'task-001')");
        ]);
        ("action", `Assoc [
          ("type", `String "string");
          ("description", `String "Transition action: claim | start | done | cancel | release");
        ]);
        ("expected_version", `Assoc [
          ("type", `String "integer");
          ("description", `String "Optional CAS guard (current backlog.version). Transition fails if mismatched");
        ]);
        ("notes", `Assoc [
          ("type", `String "string");
          ("description", `String "Completion notes (used with action='done')");
        ]);
        ("reason", `Assoc [
          ("type", `String "string");
          ("description", `String "Cancellation reason (used with action='cancel')");
        ]);
      ]);
      ("required", `List [`String "agent_name"; `String "task_id"; `String "action"]);
    ];
  };

  {
    name = "masc_release";
    description = "Release a claimed or in-progress task back to backlog. Prefer masc_transition(action='release') for a single entrypoint.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("agent_name", `Assoc [
          ("type", `String "string");
          ("description", `String "Your agent name");
        ]);
        ("task_id", `Assoc [
          ("type", `String "string");
          ("description", `String "Task ID to release");
        ]);
        ("expected_version", `Assoc [
          ("type", `String "integer");
          ("description", `String "Optional CAS guard (current backlog.version). Transition fails if mismatched");
        ]);
      ]);
      ("required", `List [`String "agent_name"; `String "task_id"]);
    ];
  };

  {
    name = "masc_done";
    description = "Mark a task as completed. Prefer masc_transition(action='done') for CAS-guarded transitions.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("agent_name", `Assoc [
          ("type", `String "string");
          ("description", `String "Your agent name");
        ]);
        ("task_id", `Assoc [
          ("type", `String "string");
          ("description", `String "Task ID");
        ]);
        ("notes", `Assoc [
          ("type", `String "string");
          ("description", `String "Completion notes");
        ]);
      ]);
      ("required", `List [`String "agent_name"; `String "task_id"]);
    ];
  };

  (* A2A CancelTask API *)
  {
    name = "masc_cancel_task";
    description = "Cancel a running or pending task. A2A compatible. Prefer masc_transition(action='cancel') for CAS-guarded transitions.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("agent_name", `Assoc [
          ("type", `String "string");
          ("description", `String "Your agent name");
        ]);
        ("task_id", `Assoc [
          ("type", `String "string");
          ("description", `String "Task ID to cancel");
        ]);
        ("reason", `Assoc [
          ("type", `String "string");
          ("description", `String "Cancellation reason (optional)");
        ]);
      ]);
      ("required", `List [`String "agent_name"; `String "task_id"]);
    ];
  };

  {
    name = "masc_task_history";
    description = "Fetch recent task transition history from event logs. Useful for audits or debugging transitions.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("task_id", `Assoc [
          ("type", `String "string");
          ("description", `String "Task ID to filter (e.g., 'task-001')");
        ]);
        ("limit", `Assoc [
          ("type", `String "integer");
          ("description", `String "Max events to return (default: 50)");
          ("default", `Int 50);
        ]);
      ]);
      ("required", `List [`String "task_id"]);
    ];
  };
  {
    name = "masc_tasks";
    description = "List all tasks in backlog with their status and assignee. \
Shows: todo (available), claimed (in progress), done, cancelled. \
Use before masc_claim to find available tasks. \
Output includes task ID, title, priority, assignee, timestamps. \
Tip: Look for status='todo' tasks to claim.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc []);
    ];
  };

  {
    name = "masc_archive_view";
    description = "View archived tasks from tasks-archive.json. Shows tasks that were completed and cleaned up by gc.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("limit", `Assoc [
          ("type", `String "integer");
          ("description", `String "Max number of archived tasks to show (default: 20)");
        ]);
      ]);
    ];
  };

  (* Priority Queue Tools *)
  {
    name = "masc_claim_next";
    description = "Automatically claim the highest priority unclaimed task. Use this when you want to pick up the most important available work without manually checking the task board. Returns the claimed task details including priority level.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("agent_name", `Assoc [
          ("type", `String "string");
          ("description", `String "Your agent name");
        ]);
      ]);
      ("required", `List [`String "agent_name"]);
    ];
  };

  {
    name = "masc_update_priority";
    description = "Change the priority of a task. Priority 1 is highest (most urgent), 5 is lowest. Use this to reprioritize work based on new information or urgency changes.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("task_id", `Assoc [
          ("type", `String "string");
          ("description", `String "Task ID to update");
        ]);
        ("priority", `Assoc [
          ("type", `String "integer");
          ("description", `String "New priority (1=highest, 5=lowest)");
          ("minimum", `Int 1);
          ("maximum", `Int 5);
        ]);
      ]);
      ("required", `List [`String "task_id"; `String "priority"]);
    ];
  };

  {
    name = "masc_broadcast";
    description = "Send a message visible to ALL agents via SSE push. Use for: status updates ('Starting task X'), help requests ('@gemini can you review this?'), completions ('✅ Done!'). Use @agent_name to ping specific agent. Default: compact format (64% token savings).";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("agent_name", `Assoc [
          ("type", `String "string");
          ("description", `String "Your agent name");
        ]);
        ("message", `Assoc [
          ("type", `String "string");
          ("description", `String "Message content (use @mention for specific agents)");
        ]);
        ("format", `Assoc [
          ("type", `String "string");
          ("enum", `List [`String "compact"; `String "verbose"]);
          ("description", `String "Output format: 'compact' (default, 64% savings) or 'verbose' (JSON for debugging)");
          ("default", `String "compact");
        ]);
      ]);
      ("required", `List [`String "agent_name"; `String "message"]);
    ];
  };

  {
    name = "masc_messages";
    description = "Get recent broadcast messages from all agents. \
Use to: catch up after joining, check if someone @mentioned you, see room activity. \
Returns chronological list with sender, timestamp, content. \
Default: last 20 messages. Use limit param for more/less. \
Tip: Search for '@your-name' in results to find mentions.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("since_seq", `Assoc [
          ("type", `String "integer");
          ("description", `String "Get messages after this sequence number");
          ("default", `Int 0);
        ]);
        ("limit", `Assoc [
          ("type", `String "integer");
          ("description", `String "Max messages to return");
          ("default", `Int 10);
        ]);
      ]);
    ];
  };

  {
    name = "masc_lock";
    description = "Acquire exclusive lock on a file before editing. Prevents other agents from modifying same file. \
IMPORTANT: Always unlock after editing! Locks auto-release on masc_leave. \
Prefer git worktrees for larger changes (masc_worktree_create). \
Example: masc_lock({agent_name: 'claude', file: 'src/utils.ts'}) → edit → masc_unlock";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("agent_name", `Assoc [
          ("type", `String "string");
          ("description", `String "Your agent name");
        ]);
        ("file", `Assoc [
          ("type", `String "string");
          ("description", `String "Relative file path or resource key to lock");
        ]);
      ]);
      ("required", `List [`String "agent_name"; `String "file"]);
    ];
  };

  {
    name = "masc_unlock";
    description = "Release a file lock after editing is complete. \
ALWAYS call after masc_lock to allow other agents to edit. \
Fails silently if you don't hold the lock. \
Example: masc_unlock({agent_name: 'claude', file: 'src/utils.ts'})";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("agent_name", `Assoc [
          ("type", `String "string");
          ("description", `String "Your agent name");
        ]);
        ("file", `Assoc [
          ("type", `String "string");
          ("description", `String "Relative file path or resource key to unlock");
        ]);
      ]);
      ("required", `List [`String "agent_name"; `String "file"]);
    ];
  };

  {
    name = "masc_listen";
    description = "Listen for incoming messages (blocking). Returns after message arrives or timeout.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("agent_name", `Assoc [
          ("type", `String "string");
          ("description", `String "Your agent name");
        ]);
        ("timeout", `Assoc [
          ("type", `String "integer");
          ("description", `String "Max seconds to wait (default: 300)");
          ("default", `Int 300);
        ]);
      ]);
      ("required", `List [`String "agent_name"]);
    ];
  };

  {
    name = "masc_who";
    description = "List all agents currently in the room with their capabilities. \
Shows: agent name, join time, capabilities (e.g., ['typescript', 'testing']). \
Use to: find who can help, check if specific agent is online, see team composition. \
Agents appear after masc_join, disappear after masc_leave. \
Tip: Use capabilities to find the right agent for @mentions.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc []);
    ];
  };

  {
    name = "masc_reset";
    description = "⚠️ DESTRUCTIVE: Reset MASC room completely. Deletes ALL data in .masc/ folder. \
Removes: tasks, messages, agents, locks, cache, telemetry. Cannot be undone. \
Use only for: fresh start, corrupted state recovery, testing. \
Requires confirm=true to execute. Example: masc_reset({confirm: true})";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("confirm", `Assoc [
          ("type", `String "boolean");
          ("description", `String "Set to true to confirm reset");
          ("default", `Bool false);
        ]);
      ]);
    ];
  };

  (* Portal/A2A Tools - Direct Agent-to-Agent Communication *)
  {
    name = "masc_portal_open";
    description = "Open a direct channel to another agent (A2A protocol). Unlike broadcast, portal messages are PRIVATE between two agents. Use for: delegating tasks to specific agent, getting expert help, parallel work handoff. The target agent will see your tasks in their portal_status.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("agent_name", `Assoc [
          ("type", `String "string");
          ("description", `String "Your agent name (e.g., 'claude')");
        ]);
        ("target_agent", `Assoc [
          ("type", `String "string");
          ("description", `String "Target agent name (e.g., 'gemini')");
        ]);
        ("initial_message", `Assoc [
          ("type", `String "string");
          ("description", `String "Optional initial message to send");
        ]);
      ]);
      ("required", `List [`String "agent_name"; `String "target_agent"]);
    ];
  };

  {
    name = "masc_portal_send";
    description = "Send a task/request through your open portal. The connected agent will receive this as a pending A2A task. Good for: code review requests, parallel subtasks, expert consultations. Check portal_status to see if they've responded. Default: compact format (64% token savings).";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("agent_name", `Assoc [
          ("type", `String "string");
          ("description", `String "Your agent name");
        ]);
        ("message", `Assoc [
          ("type", `String "string");
          ("description", `String "Message/task to send through portal");
        ]);
        ("format", `Assoc [
          ("type", `String "string");
          ("enum", `List [`String "compact"; `String "verbose"]);
          ("description", `String "Output format: 'compact' (default, 64% savings) or 'verbose' (JSON)");
          ("default", `String "compact");
        ]);
      ]);
      ("required", `List [`String "agent_name"; `String "message"]);
    ];
  };

  {
    name = "masc_portal_close";
    description = "Close your portal connection to external services. \
Use when: finished with external API, cleaning up before leave. \
Portals are tunnels to external MCP servers (e.g., GitHub, Slack). \
Auto-closes on masc_leave. Check masc_portal_status for active portals.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("agent_name", `Assoc [
          ("type", `String "string");
          ("description", `String "Your agent name");
        ]);
      ]);
      ("required", `List [`String "agent_name"]);
    ];
  };

  {
    name = "masc_portal_status";
    description = "Get status of your portal connections and pending A2A tasks.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("agent_name", `Assoc [
          ("type", `String "string");
          ("description", `String "Your agent name");
        ]);
      ]);
      ("required", `List [`String "agent_name"]);
    ];
  };

  (* Git Worktree Integration - v2 Agent Isolation *)
  {
    name = "masc_worktree_create";
    description = "Create an isolated Git worktree for your work. Each agent works in its own worktree to avoid conflicts. The worktree is created at .worktrees/{agent}-{task}/ with a new branch. This is BETTER than file locks: you get complete isolation and can work in parallel. After work, create a PR with `gh pr create` and remove the worktree.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("agent_name", `Assoc [
          ("type", `String "string");
          ("description", `String "Your agent name (e.g., 'claude')");
        ]);
        ("task_id", `Assoc [
          ("type", `String "string");
          ("description", `String "Task ID or feature name (e.g., 'PK-12345' or 'fix-login')");
        ]);
        ("base_branch", `Assoc [
          ("type", `String "string");
          ("description", `String "Base branch to create worktree from (default: 'develop' or 'main')");
          ("default", `String "develop");
        ]);
      ]);
      ("required", `List [`String "agent_name"; `String "task_id"]);
    ];
  };

  {
    name = "masc_worktree_remove";
    description = "Remove a worktree after your work is merged. This cleans up both the worktree directory and the local branch. Call this after your PR is merged to keep the repo clean.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("agent_name", `Assoc [
          ("type", `String "string");
          ("description", `String "Your agent name");
        ]);
        ("task_id", `Assoc [
          ("type", `String "string");
          ("description", `String "Task ID used when creating the worktree");
        ]);
      ]);
      ("required", `List [`String "agent_name"; `String "task_id"]);
    ];
  };

  {
    name = "masc_worktree_list";
    description = "List all active worktrees in the project. Shows which agents are working on what tasks.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc []);
    ];
  };

  (* ============================================ *)
  (* Heartbeat & Agent Health                    *)
  (* ============================================ *)

  {
    name = "masc_heartbeat";
    description = "Update your heartbeat timestamp. Call periodically (every few minutes) to indicate you're still active. Agents without heartbeat for 5+ minutes are considered 'zombies' and can be cleaned up.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("agent_name", `Assoc [
          ("type", `String "string");
          ("description", `String "Your agent name");
        ]);
      ]);
      ("required", `List [`String "agent_name"]);
    ];
  };

  {
    name = "masc_cleanup_zombies";
    description = "Clean up zombie agents (no heartbeat for 5+ minutes). Removes stale agents and releases their file locks. Run this periodically or when you suspect agent crashes.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc []);
    ];
  };

  {
    name = "masc_heartbeat_start";
    description = "Start periodic heartbeat broadcasts. Runs in background, sending pings at specified interval. Use for keep-alive or automated status updates.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("interval", `Assoc [
          ("type", `String "integer");
          ("description", `String "Interval in seconds between heartbeats (min: 5, max: 300)");
          ("default", `Int 30);
        ]);
        ("message", `Assoc [
          ("type", `String "string");
          ("description", `String "Heartbeat message content");
          ("default", `String "🏓 heartbeat");
        ]);
      ]);
    ];
  };

  {
    name = "masc_heartbeat_stop";
    description = "Stop a periodic heartbeat started by masc_heartbeat_start. \
Use when: long task complete, no longer need keep-alive, cleaning up. \
Get heartbeat_id from masc_heartbeat_start response or masc_heartbeat_list.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("heartbeat_id", `Assoc [
          ("type", `String "string");
          ("description", `String "ID of heartbeat to stop (from masc_heartbeat_start)");
        ]);
      ]);
      ("required", `List [`String "heartbeat_id"]);
    ];
  };

  {
    name = "masc_heartbeat_list";
    description = "List all active heartbeats in the room. \
Shows: heartbeat_id, agent, interval, last_beat time. \
Use to: find orphaned heartbeats, debug presence issues, cleanup before leave.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc []);
    ];
  };

  {
    name = "masc_gc";
    description = "Garbage collection - cleanup zombies, archive stale tasks, delete old messages. One command to clean everything older than N days.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("days", `Assoc [
          ("type", `String "integer");
          ("default", `Int 7);
          ("description", `String "Age threshold in days (default: 7)");
        ]);
      ]);
    ];
  };

  {
    name = "masc_agents";
    description = "Get detailed status of all agents including zombie detection, current tasks, capabilities, and last seen time.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc []);
    ];
  };

  (* ============================================ *)
  (* Agent Discovery - Capability Broadcasting   *)
  (* ============================================ *)

  {
    name = "masc_register_capabilities";
    description = "Register your capabilities for agent discovery. Other agents can then find you by capability. Examples: ['typescript', 'code-review', 'testing', 'python', 'architecture'].";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("agent_name", `Assoc [
          ("type", `String "string");
          ("description", `String "Your agent name");
        ]);
        ("capabilities", `Assoc [
          ("type", `String "array");
          ("items", `Assoc [("type", `String "string")]);
          ("description", `String "List of your capabilities (e.g., ['typescript', 'testing'])");
        ]);
      ]);
      ("required", `List [`String "agent_name"; `String "capabilities"]);
    ];
  };

  {
    name = "masc_agent_update";
    description = "Update agent metadata (status/capabilities). Use for external agents or manual corrections. Status guards prevent illegal transitions.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("agent_name", `Assoc [
          ("type", `String "string");
          ("description", `String "Agent name or nickname");
        ]);
        ("status", `Assoc [
          ("type", `String "string");
          ("description", `String "Optional status: active | busy | listening | inactive");
        ]);
        ("capabilities", `Assoc [
          ("type", `String "array");
          ("items", `Assoc [("type", `String "string")]);
          ("description", `String "Optional capability list (overwrites existing)");
        ]);
      ]);
      ("required", `List [`String "agent_name"]);
    ];
  };

  {
    name = "masc_find_by_capability";
    description = "Find agents by capability. Use this to discover who can help with specific tasks. Only returns non-zombie agents.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("capability", `Assoc [
          ("type", `String "string");
          ("description", `String "Capability to search for (e.g., 'typescript')");
        ]);
      ]);
      ("required", `List [`String "capability"]);
    ];
  };

  (* A2A Agent Card - Discovery *)
  {
    name = "masc_agent_card";
    description = "Get or update the A2A-compatible Agent Card for this MASC instance. Agent Cards enable standardized agent discovery and capability advertisement. Use 'get' to retrieve current card, 'refresh' to regenerate with current bindings.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("action", `Assoc [
          ("type", `String "string");
          ("enum", `List [`String "get"; `String "refresh"]);
          ("description", `String "Action: 'get' returns current card, 'refresh' regenerates it");
        ]);
      ]);
    ];
  };

  (* ============================================ *)
  (* Planning with Files (Manus AI pattern)      *)
  (* ============================================ *)

  {
    name = "masc_plan_init";
    description = "Initialize a planning context for a task. Creates task_plan.md, notes.md, and deliverable.md structure. Works with file or PostgreSQL backend.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("task_id", `Assoc [
          ("type", `String "string");
          ("description", `String "Task ID to create planning context for");
        ]);
      ]);
      ("required", `List [`String "task_id"]);
    ];
  };

  {
    name = "masc_plan_update";
    description = "Update the task plan (main execution plan). Overwrites the current plan.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("task_id", `Assoc [
          ("type", `String "string");
          ("description", `String "Task ID");
        ]);
        ("content", `Assoc [
          ("type", `String "string");
          ("description", `String "New plan content (markdown)");
        ]);
      ]);
      ("required", `List [`String "task_id"; `String "content"]);
    ];
  };

  {
    name = "masc_note_add";
    description = "Add a note/observation to the planning context. Notes are timestamped and appended.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("task_id", `Assoc [
          ("type", `String "string");
          ("description", `String "Task ID");
        ]);
        ("note", `Assoc [
          ("type", `String "string");
          ("description", `String "Note content");
        ]);
      ]);
      ("required", `List [`String "task_id"; `String "note"]);
    ];
  };

  {
    name = "masc_deliver";
    description = "Attach final output/result to a task for handoff or review. \
Use for: code diffs, PR URLs, analysis reports, generated files. \
Deliverables persist with task and are visible to other agents. \
Call before masc_transition(action='done'). \
Example: masc_deliver({task_id: 'task-001', content: 'PR: github.com/org/repo/pull/123'})";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("task_id", `Assoc [
          ("type", `String "string");
          ("description", `String "Task ID");
        ]);
        ("content", `Assoc [
          ("type", `String "string");
          ("description", `String "Deliverable content");
        ]);
      ]);
      ("required", `List [`String "task_id"; `String "content"]);
    ];
  };

  {
    name = "masc_error_add";
    description = "Add an error/failure to the planning context (PDCA Check phase). Use to track failures, bugs, and issues encountered during task execution.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("task_id", `Assoc [
          ("type", `String "string");
          ("description", `String "Task ID");
        ]);
        ("error_type", `Assoc [
          ("type", `String "string");
          ("description", `String "Type of error: build, test, runtime, logic, api, etc.");
        ]);
        ("message", `Assoc [
          ("type", `String "string");
          ("description", `String "Error message or description");
        ]);
        ("context", `Assoc [
          ("type", `String "string");
          ("description", `String "Optional context (file path, function name, etc.)");
        ]);
      ]);
      ("required", `List [`String "task_id"; `String "error_type"; `String "message"]);
    ];
  };

  {
    name = "masc_error_resolve";
    description = "Mark an error as resolved. Use when you've fixed an issue tracked in the planning context.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("task_id", `Assoc [
          ("type", `String "string");
          ("description", `String "Task ID");
        ]);
        ("error_index", `Assoc [
          ("type", `String "integer");
          ("description", `String "0-based index of the error to mark as resolved");
        ]);
      ]);
      ("required", `List [`String "task_id"; `String "error_index"]);
    ];
  };

  {
    name = "masc_plan_get";
    description = "Get the full planning context for a task as markdown (for LLM context).";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("task_id", `Assoc [
          ("type", `String "string");
          ("description", `String "Task ID (optional if current task is set)");
        ]);
      ]);
      ("required", `List []);
    ];
  };

  (* ============================================ *)
  (* Session-level Context                        *)
  (* ============================================ *)

  {
    name = "masc_plan_set_task";
    description = "Set the current task for the session. After this, you can omit task_id in subsequent planning calls.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("task_id", `Assoc [
          ("type", `String "string");
          ("description", `String "Task ID to set as current");
        ]);
      ]);
      ("required", `List [`String "task_id"]);
    ];
  };

  {
    name = "masc_plan_get_task";
    description = "Get the task_id you're currently working on (session-scoped). \
Returns null if no task is set. Useful for: resuming work after context switch, \
verifying current assignment, debugging session state. \
Set via masc_plan_set_task. Auto-cleared on masc_leave.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc []);
      ("required", `List []);
    ];
  };

  {
    name = "masc_plan_clear_task";
    description = "Clear your current task assignment without completing it. \
Use when: switching to different task, abandoning work, resetting session. \
Does NOT change task status (use masc_transition for that). \
Auto-called on masc_leave.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc []);
      ("required", `List []);
    ];
  };

  (* ============================================ *)
  (* Consensus / Voting System                   *)
  (* ============================================ *)

  {
    name = "masc_vote_create";
    description = "Create a vote for multi-agent consensus. Use for decisions like: which approach to take, PR approval, architecture choices. All active agents can vote.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("proposer", `Assoc [
          ("type", `String "string");
          ("description", `String "Your agent name (vote creator)");
        ]);
        ("topic", `Assoc [
          ("type", `String "string");
          ("description", `String "What are we voting on? (e.g., 'Approach for API refactoring')");
        ]);
        ("options", `Assoc [
          ("type", `String "array");
          ("items", `Assoc [("type", `String "string")]);
          ("description", `String "Vote options (e.g., ['Option A: REST', 'Option B: GraphQL'])");
        ]);
        ("required_votes", `Assoc [
          ("type", `String "integer");
          ("description", `String "Number of votes needed to resolve (usually 2 or 3)");
          ("default", `Int 2);
        ]);
      ]);
      ("required", `List [`String "proposer"; `String "topic"; `String "options"]);
    ];
  };

  {
    name = "masc_vote_cast";
    description = "Cast your vote on an active proposal. Your choice must match one of the options exactly.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("agent_name", `Assoc [
          ("type", `String "string");
          ("description", `String "Your agent name");
        ]);
        ("vote_id", `Assoc [
          ("type", `String "string");
          ("description", `String "Vote ID from masc_vote_create");
        ]);
        ("choice", `Assoc [
          ("type", `String "string");
          ("description", `String "Your choice (must match an option exactly)");
        ]);
      ]);
      ("required", `List [`String "agent_name"; `String "vote_id"; `String "choice"]);
    ];
  };

  {
    name = "masc_vote_status";
    description = "Get status of a specific vote including current votes and result.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("vote_id", `Assoc [
          ("type", `String "string");
          ("description", `String "Vote ID to check");
        ]);
      ]);
      ("required", `List [`String "vote_id"]);
    ];
  };

  {
    name = "masc_votes";
    description = "List all votes in the room (active and resolved). \
Votes are used for: multi-agent decisions, consensus building, approvals. \
Shows: vote_id, question, options, current tally, status. \
Use masc_vote_cast to participate, masc_vote_create to start new vote.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc []);
    ];
  };

  (* ============================================ *)
  (* LangGraph Interrupt Pattern (Human-in-Loop) *)
  (* ============================================ *)

  {
    name = "masc_interrupt";
    description = "Pause workflow and wait for user approval (LangGraph interrupt pattern). Use before dangerous operations like database deletion, production changes, or external API calls. The workflow will be suspended until approved or rejected.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("agent_name", `Assoc [
          ("type", `String "string");
          ("description", `String "Your agent name");
        ]);
        ("task_id", `Assoc [
          ("type", `String "string");
          ("description", `String "Task ID being interrupted");
        ]);
        ("step", `Assoc [
          ("type", `String "integer");
          ("description", `String "Step number (1-based)");
        ]);
        ("action", `Assoc [
          ("type", `String "string");
          ("description", `String "Action description (what you're about to do)");
        ]);
        ("message", `Assoc [
          ("type", `String "string");
          ("description", `String "Approval request message to show user");
        ]);
      ]);
      ("required", `List [`String "agent_name"; `String "task_id"; `String "step"; `String "action"; `String "message"]);
    ];
  };

  {
    name = "masc_approve";
    description = "Approve an interrupted workflow checkpoint. Use when user confirms the dangerous action should proceed.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("task_id", `Assoc [
          ("type", `String "string");
          ("description", `String "Task ID to approve");
        ]);
      ]);
      ("required", `List [`String "task_id"]);
    ];
  };

  {
    name = "masc_reject";
    description = "Reject an interrupted workflow checkpoint. Use when user declines the dangerous action.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("task_id", `Assoc [
          ("type", `String "string");
          ("description", `String "Task ID to reject");
        ]);
        ("reason", `Assoc [
          ("type", `String "string");
          ("description", `String "Rejection reason");
        ]);
      ]);
      ("required", `List [`String "task_id"]);
    ];
  };

  {
    name = "masc_pending_interrupts";
    description = "List all pending interrupted workflows waiting for approval.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc []);
    ];
  };

  {
    name = "masc_branch";
    description = "Create a new execution branch from an existing checkpoint. Use for exploring alternative paths (e.g., 'try approach A here, try approach B there'). The source checkpoint is marked as 'branched' and a new checkpoint is created with the same state but a new branch name.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("agent_name", `Assoc [
          ("type", `String "string");
          ("description", `String "Your agent name");
        ]);
        ("task_id", `Assoc [
          ("type", `String "string");
          ("description", `String "Task ID containing the checkpoint");
        ]);
        ("source_step", `Assoc [
          ("type", `String "integer");
          ("description", `String "Step number of the checkpoint to branch from");
        ]);
        ("branch_name", `Assoc [
          ("type", `String "string");
          ("description", `String "Name for the new branch (e.g., 'approach-a', 'safe-mode')");
        ]);
      ]);
      ("required", `List [`String "agent_name"; `String "task_id"; `String "source_step"; `String "branch_name"]);
    ];
  };

  (* ============================================ *)
  (* Cost Tracking                               *)
  (* ============================================ *)

  {
    name = "masc_cost_log";
    description = "Log token usage and cost for tracking multi-agent expenses. Call after significant API calls to track spending per agent and task.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("agent_name", `Assoc [
          ("type", `String "string");
          ("description", `String "Agent name (claude, gemini, codex)");
        ]);
        ("model", `Assoc [
          ("type", `String "string");
          ("description", `String "Model name (e.g., opus-4, gemini-2)");
        ]);
        ("input_tokens", `Assoc [
          ("type", `String "integer");
          ("description", `String "Number of input tokens");
        ]);
        ("output_tokens", `Assoc [
          ("type", `String "integer");
          ("description", `String "Number of output tokens");
        ]);
        ("cost_usd", `Assoc [
          ("type", `String "number");
          ("description", `String "Estimated cost in USD");
        ]);
        ("task_id", `Assoc [
          ("type", `String "string");
          ("description", `String "Optional task ID for attribution");
        ]);
      ]);
      ("required", `List [`String "agent_name"; `String "cost_usd"]);
    ];
  };

  {
    name = "masc_cost_report";
    description = "Get cost report showing token usage and spending by agent. Use to monitor multi-agent collaboration expenses.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("period", `Assoc [
          ("type", `String "string");
          ("description", `String "Time period: hourly, daily, weekly, monthly, all");
          ("default", `String "daily");
        ]);
        ("agent", `Assoc [
          ("type", `String "string");
          ("description", `String "Filter by agent name (optional)");
        ]);
        ("task_id", `Assoc [
          ("type", `String "string");
          ("description", `String "Filter by task ID (optional)");
        ]);
      ]);
    ];
  };

  (* ============================================ *)
  (* Authentication & Authorization              *)
  (* ============================================ *)

  {
    name = "masc_auth_enable";
    description = "Enable authentication for this room. Returns a room secret that should be shared securely with authorized agents. Once enabled, agents need tokens to perform actions.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("require_token", `Assoc [
          ("type", `String "boolean");
          ("description", `String "If true, all actions require a valid token. If false, tokens are optional but provide elevated permissions.");
          ("default", `Bool false);
        ]);
      ]);
    ];
  };

  {
    name = "masc_auth_disable";
    description = "Disable authentication for this room. All agents can perform any action without tokens.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc []);
    ];
  };

  {
    name = "masc_auth_status";
    description = "Check authentication status for this room.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc []);
    ];
  };

  {
    name = "masc_auth_create_token";
    description = "Create a new authentication token for an agent. The token should be kept secret and passed in subsequent requests.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("agent_name", `Assoc [
          ("type", `String "string");
          ("description", `String "Agent name to create token for");
        ]);
        ("role", `Assoc [
          ("type", `String "string");
          ("description", `String "Agent role: 'reader' (read-only), 'worker' (can claim/lock/broadcast), 'admin' (full access)");
          ("default", `String "worker");
        ]);
      ]);
      ("required", `List [`String "agent_name"]);
    ];
  };

  {
    name = "masc_auth_refresh";
    description = "Refresh an expired or soon-to-expire token. Returns a new token.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("agent_name", `Assoc [
          ("type", `String "string");
          ("description", `String "Your agent name");
        ]);
        ("token", `Assoc [
          ("type", `String "string");
          ("description", `String "Your current token");
        ]);
      ]);
      ("required", `List [`String "agent_name"; `String "token"]);
    ];
  };

  {
    name = "masc_auth_revoke";
    description = "Revoke an agent's token. The agent will need a new token to perform authenticated actions.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("agent_name", `Assoc [
          ("type", `String "string");
          ("description", `String "Agent name whose token to revoke");
        ]);
      ]);
      ("required", `List [`String "agent_name"]);
    ];
  };

  {
    name = "masc_auth_list";
    description = "List all agent credentials (admin only). Shows agent names, roles, and token expiry times.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc []);
    ];
  };

  (* Rate limit tools *)
  {
    name = "masc_rate_limit_status";
    description = "Get your current rate limit status. Shows remaining requests per category (general, broadcast, task ops, file locks) and burst tokens.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("agent_name", `Assoc [
          ("type", `String "string");
          ("description", `String "Your agent name");
        ]);
      ]);
      ("required", `List [`String "agent_name"]);
    ];
  };

  {
    name = "masc_rate_limit_config";
    description = "Get or update rate limit configuration (admin only). Shows limits per category and role multipliers.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("per_minute", `Assoc [
          ("type", `String "integer");
          ("description", `String "Base requests per minute (default: 10)");
        ]);
        ("burst_allowed", `Assoc [
          ("type", `String "integer");
          ("description", `String "Burst tokens available (default: 5)");
        ]);
        ("broadcast_per_minute", `Assoc [
          ("type", `String "integer");
          ("description", `String "Broadcast operations per minute (default: 15)");
        ]);
        ("task_ops_per_minute", `Assoc [
          ("type", `String "integer");
          ("description", `String "Task operations per minute (default: 30)");
        ]);
      ]);
    ];
  };

  (* ============================================ *)
  (* Encryption (Data Protection)                *)
  (* ============================================ *)

  {
    name = "masc_encryption_status";
    description = "Get encryption status for this MASC room. Shows if encryption is enabled, key status, and RNG state.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc []);
    ];
  };

  {
    name = "masc_encryption_enable";
    description = "Enable encryption for sensitive data in this MASC room. Requires setting MASC_ENCRYPTION_KEY environment variable (32-byte key) or providing a key file path.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("key_source", `Assoc [
          ("type", `String "string");
          ("description", `String "Key source: 'env' (from MASC_ENCRYPTION_KEY), 'file:<path>' (from file), or 'generate' (create new key)");
          ("default", `String "env");
        ]);
      ]);
    ];
  };

  {
    name = "masc_encryption_disable";
    description = "Disable encryption for this MASC room. Existing encrypted data will remain encrypted but new data will be stored in plain text.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc []);
    ];
  };

  {
    name = "masc_generate_key";
    description = "Generate a new random 256-bit encryption key. Returns the key in hex format. Store this securely - losing the key means losing access to encrypted data.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("output", `Assoc [
          ("type", `String "string");
          ("description", `String "Output format: 'hex' (64 chars), 'base64' (44 chars)");
          ("default", `String "hex");
        ]);
      ]);
    ];
  };

  (* ============================================ *)
  (* Mode Management (Serena-style)              *)
  (* ============================================ *)

  {
    name = "masc_switch_mode";
    description = "Switch MASC mode to reduce token usage. Available modes: 'minimal' (core+health), 'standard' (core+comm+worktree+health), 'parallel' (multi-agent heavy: comm+portal+discovery+voting+interrupt), 'full' (all features), 'solo' (single-agent: core+worktree). Use 'custom' with categories parameter for fine-grained control.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("mode", `Assoc [
          ("type", `String "string");
          ("enum", `List [`String "minimal"; `String "standard"; `String "parallel"; `String "full"; `String "solo"; `String "custom"]);
          ("description", `String "Mode preset: minimal, standard, parallel, full, solo, or custom");
        ]);
        ("categories", `Assoc [
          ("type", `String "array");
          ("items", `Assoc [("type", `String "string")]);
          ("description", `String "For 'custom' mode: list of categories to enable (core, comm, portal, worktree, health, discovery, voting, interrupt, cost, auth, ratelimit, encryption)");
        ]);
      ]);
      ("required", `List [`String "mode"]);
    ];
  };

  {
    name = "masc_get_config";
    description = "Get current MASC mode configuration. Shows enabled categories, disabled categories, and available mode presets.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc []);
    ];
  };

  (* ============================================ *)
  (* Spawn - Auto-dispatch agents                 *)
  (* ============================================ *)

  {
    name = "masc_spawn";
    description = "Spawn an agent to execute a task. This is the 'magic pillar' - automatically starts an agent (claude, gemini, codex) with a prompt and returns the result. Use this to orchestrate multi-agent collaboration without manual terminal setup.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("agent_name", `Assoc [
          ("type", `String "string");
          ("description", `String "Agent to spawn: 'claude', 'gemini', 'codex', or custom command");
        ]);
        ("prompt", `Assoc [
          ("type", `String "string");
          ("description", `String "The task/prompt to send to the agent");
        ]);
        ("timeout_seconds", `Assoc [
          ("type", `String "integer");
          ("default", `Int 300);
          ("description", `String "Max execution time in seconds (default: 300)");
        ]);
        ("working_dir", `Assoc [
          ("type", `String "string");
          ("description", `String "Working directory for the agent (optional)");
        ]);
      ]);
      ("required", `List [`String "agent_name"; `String "prompt"]);
    ];
  };

  (* ============================================ *)
  (* Relay Tools (Infinite Context via Handoff)  *)
  (* ============================================ *)

  {
    name = "masc_relay_status";
    description = "Check current context usage and relay readiness. Shows estimated token count, usage ratio, and whether relay is recommended. Call periodically to monitor context health.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("messages", `Assoc [
          ("type", `String "integer");
          ("description", `String "Number of messages in conversation");
        ]);
        ("tool_calls", `Assoc [
          ("type", `String "integer");
          ("description", `String "Number of tool calls made");
        ]);
        ("model", `Assoc [
          ("type", `String "string");
          ("description", `String "Model name (claude, gemini, codex) for max context lookup");
          ("default", `String "claude");
        ]);
      ]);
    ];
  };

  {
    name = "masc_relay_checkpoint";
    description = "Save a checkpoint of current state for smooth handoff. Call at key moments (after completing subtasks, before complex operations). Checkpoints enable proactive relay with minimal context loss.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("summary", `Assoc [
          ("type", `String "string");
          ("description", `String "Brief summary of work done so far");
        ]);
        ("current_task", `Assoc [
          ("type", `String "string");
          ("description", `String "Current task being worked on (optional)");
        ]);
        ("todos", `Assoc [
          ("type", `String "array");
          ("items", `Assoc [("type", `String "string")]);
          ("description", `String "List of remaining TODO items");
        ]);
        ("pdca_state", `Assoc [
          ("type", `String "string");
          ("description", `String "Current PDCA cycle state (optional)");
        ]);
        ("relevant_files", `Assoc [
          ("type", `String "array");
          ("items", `Assoc [("type", `String "string")]);
          ("description", `String "List of files being worked on");
        ]);
      ]);
      ("required", `List [`String "summary"]);
    ];
  };

  {
    name = "masc_relay_now";
    description = "Trigger immediate relay to a new agent. Use when context is getting full or before a complex task. The new agent will receive compressed context and continue seamlessly.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("summary", `Assoc [
          ("type", `String "string");
          ("description", `String "Summary of work for handoff");
        ]);
        ("current_task", `Assoc [
          ("type", `String "string");
          ("description", `String "Task to continue (optional)");
        ]);
        ("target_agent", `Assoc [
          ("type", `String "string");
          ("description", `String "Agent to relay to (default: claude)");
          ("default", `String "claude");
        ]);
        ("generation", `Assoc [
          ("type", `String "integer");
          ("description", `String "Current relay generation (default: 0)");
          ("default", `Int 0);
        ]);
      ]);
      ("required", `List [`String "summary"]);
    ];
  };

  {
    name = "masc_relay_smart_check";
    description = "Proactive relay check with task hint. Predicts if upcoming task will overflow context and suggests relay BEFORE starting the task. Key for smooth transitions.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("messages", `Assoc [
          ("type", `String "integer");
          ("description", `String "Current message count");
        ]);
        ("tool_calls", `Assoc [
          ("type", `String "integer");
          ("description", `String "Current tool call count");
        ]);
        ("task_hint", `Assoc [
          ("type", `String "string");
          ("enum", `List [`String "large_file"; `String "multi_file"; `String "long_running"; `String "exploration"; `String "simple"]);
          ("description", `String "Hint about upcoming task complexity");
        ]);
        ("file_count", `Assoc [
          ("type", `String "integer");
          ("description", `String "For multi_file hint: number of files");
        ]);
      ]);
      ("required", `List [`String "task_hint"]);
    ];
  };

  (* ============================================ *)
  (* Mitosis Tools (Cell Division Pattern)       *)
  (* ============================================ *)

  {
    name = "masc_mitosis_status";
    description = "Get current agent cell status and stem pool state. Shows generation, task count, tool calls, and available reserve cells. Use to monitor lifecycle state.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc []);
    ];
  };

  {
    name = "masc_mitosis_all";
    description = "Get mitosis status of ALL agents in the cluster (cross-machine). Shows each agent's context pressure so you can see if another agent needs handoff help. Use for collaboration awareness.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc []);
    ];
  };

  {
    name = "masc_mitosis_pool";
    description = "View the stem cell pool - reserve agents ready for instant handoff. Shows warm cells, their generation, and readiness state.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc []);
    ];
  };

  {
    name = "masc_mitosis_divide";
    description = "Manually trigger cell division (mitosis). Parent cell dies gracefully (apoptosis) while child cell inherits compressed DNA (context). Use for proactive handoff.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("summary", `Assoc [
          ("type", `String "string");
          ("description", `String "Current context summary to compress into DNA");
        ]);
        ("current_task", `Assoc [
          ("type", `String "string");
          ("description", `String "The task to continue after division");
        ]);
      ]);
      ("required", `List [`String "summary"]);
    ];
  };

  {
    name = "masc_mitosis_check";
    description = "2-Phase mitosis check. Phase 1 (50%): should_prepare=true → extract DNA. Phase 2 (80%): should_handoff=true → execute handoff. Returns current phase and thresholds.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("context_ratio", `Assoc [
          ("type", `String "number");
          ("description", `String "Current context usage ratio (0.0-1.0)");
        ]);
      ]);
    ];
  };

  {
    name = "masc_mitosis_record";
    description = "Record activity for mitosis trigger tracking. Call after completing tasks or tool calls to update counters.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("task_done", `Assoc [
          ("type", `String "boolean");
          ("description", `String "Whether a task was completed");
        ]);
        ("tool_called", `Assoc [
          ("type", `String "boolean");
          ("description", `String "Whether a tool was called");
        ]);
      ]);
    ];
  };

  {
    name = "masc_mitosis_prepare";
    description = "Phase 1: Prepare for division - extract DNA and mark cell as ready. Does NOT handoff yet. Call this at 50% context to prepare early, actual handoff happens at 80%.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("full_context", `Assoc [
          ("type", `String "string");
          ("description", `String "Full context to extract DNA from (will be compressed)");
        ]);
      ]);
      ("required", `List [`String "full_context"]);
    ];
  };

  {
    name = "masc_memento_mori";
    description = {|Memento Mori - Agent self-awareness of mortality.
A convenience tool combining mitosis check + prepare + divide in one call.
Call this periodically to check your context health and auto-handle lifecycle:
- <50%: Returns "continue" - keep working
- 50-80%: Auto-prepares DNA, returns "prepared" - ready for handoff when needed
- >80%: Auto-divides and spawns successor, returns handoff result

This embodies the philosophical concept of "memento mori" - agents should be aware
of their context limits and gracefully hand over work to successors.|};
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("context_ratio", `Assoc [
          ("type", `String "number");
          ("description", `String "Current context usage ratio (0.0-1.0). Estimate based on messages/tool calls.");
        ]);
        ("full_context", `Assoc [
          ("type", `String "string");
          ("description", `String "Current conversation context for DNA extraction (required if context_ratio > 0.5)");
        ]);
        ("summary", `Assoc [
          ("type", `String "string");
          ("description", `String "Brief summary of current work for handoff (optional, defaults to auto-generated)");
        ]);
        ("current_task", `Assoc [
          ("type", `String "string");
          ("description", `String "Task ID being worked on (optional)");
        ]);
        ("target_agent", `Assoc [
          ("type", `String "string");
          ("description", `String "Agent to spawn as successor (default: claude)");
        ]);
      ]);
      ("required", `List [`String "context_ratio"]);
    ];
  };

  (* ============================================ *)
  (* A2A MCP Tools (A2A Protocol via MCP)        *)
  (* ============================================ *)

  {
    name = "masc_a2a_discover";
    description = "Discover available A2A agents. Returns agent cards with capabilities, skills, and protocol bindings. Use for local room discovery or remote endpoint fetching.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("endpoint", `Assoc [
          ("type", `String "string");
          ("description", `String "Remote endpoint URL (optional, defaults to local room)");
        ]);
        ("capability", `Assoc [
          ("type", `String "string");
          ("description", `String "Filter by capability (e.g., 'typescript', 'code-review')");
        ]);
      ]);
    ];
  };

  {
    name = "masc_a2a_query_skill";
    description = "Query detailed information about an agent's skill, including input/output modes and examples. Use to understand what a skill can do before delegating.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("agent_name", `Assoc [
          ("type", `String "string");
          ("description", `String "Target agent name");
        ]);
        ("skill_id", `Assoc [
          ("type", `String "string");
          ("description", `String "Skill ID to query (e.g., 'task-management', 'git-worktree')");
        ]);
      ]);
      ("required", `List [`String "agent_name"; `String "skill_id"]);
    ];
  };

  {
    name = "masc_a2a_delegate";
    description = "Delegate a task to another A2A agent. Opens portal, sends task, returns task ID. Use sync for waiting, async for fire-and-forget, stream for real-time updates.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("target_agent", `Assoc [
          ("type", `String "string");
          ("description", `String "Agent to delegate to");
        ]);
        ("task_type", `Assoc [
          ("type", `String "string");
          ("enum", `List [`String "sync"; `String "async"; `String "stream"]);
          ("default", `String "async");
          ("description", `String "Type: 'sync' (wait), 'async' (fire-and-forget), 'stream' (real-time)");
        ]);
        ("message", `Assoc [
          ("type", `String "string");
          ("description", `String "Task description or prompt to send");
        ]);
        ("artifacts", `Assoc [
          ("type", `String "array");
          ("items", `Assoc [
            ("type", `String "object");
            ("properties", `Assoc [
              ("name", `Assoc [("type", `String "string")]);
              ("mime_type", `Assoc [("type", `String "string")]);
              ("data", `Assoc [("type", `String "string")]);
            ]);
          ]);
          ("description", `String "Optional input artifacts (files, data)");
        ]);
        ("timeout", `Assoc [
          ("type", `String "integer");
          ("default", `Int 300);
          ("description", `String "Timeout in seconds (default: 300)");
        ]);
      ]);
      ("required", `List [`String "target_agent"; `String "message"]);
    ];
  };

  {
    name = "masc_a2a_subscribe";
    description = "Subscribe to events from agents (task updates, broadcasts, completions). Connect to SSE endpoint to receive events.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("agent_name", `Assoc [
          ("type", `String "string");
          ("description", `String "Agent to subscribe to (or '*' for all agents)");
        ]);
        ("events", `Assoc [
          ("type", `String "array");
          ("items", `Assoc [
            ("type", `String "string");
            ("enum", `List [`String "task_update"; `String "broadcast"; `String "completion"; `String "error"]);
          ]);
          ("description", `String "Event types to subscribe to");
        ]);
      ]);
      ("required", `List [`String "events"]);
    ];
  };

  {
    name = "masc_a2a_unsubscribe";
    description = "Stop receiving events from a background subscription. \
Call when: (1) done monitoring, (2) switching to different events, (3) cleanup before leave. \
Frees server resources - always unsubscribe when done. \
Get subscription_id from masc_a2a_subscribe response. \
Example: masc_a2a_unsubscribe({subscription_id: 'sub-abc123'})";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("subscription_id", `Assoc [
          ("type", `String "string");
          ("description", `String "Subscription ID to remove");
        ]);
      ]);
      ("required", `List [`String "subscription_id"]);
    ];
  };

  {
    name = "masc_poll_events";
    description = "Poll buffered events for a subscription. Use this for background subscription workflow: subscribe → do work → poll_events periodically. Returns and clears buffered events.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("subscription_id", `Assoc [
          ("type", `String "string");
          ("description", `String "Subscription ID to poll events from");
        ]);
        ("clear", `Assoc [
          ("type", `String "boolean");
          ("description", `String "Clear buffer after reading (default: true)");
          ("default", `Bool true);
        ]);
      ]);
      ("required", `List [`String "subscription_id"]);
    ];
  };

  (* ============================================ *)
  (* Tempo Control (Pace Management)             *)
  (* ============================================ *)

  {
    name = "masc_tempo";
    description = "Get or set cluster tempo (pace control). Use to slow down for careful work or speed up for simple tasks.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("action", `Assoc [
          ("type", `String "string");
          ("enum", `List [`String "get"; `String "set"]);
          ("description", `String "Get current tempo or set new tempo");
        ]);
        ("mode", `Assoc [
          ("type", `String "string");
          ("enum", `List [`String "normal"; `String "slow"; `String "fast"; `String "paused"]);
          ("description", `String "Tempo mode (only for set action)");
        ]);
        ("reason", `Assoc [
          ("type", `String "string");
          ("description", `String "Why changing tempo");
        ]);
      ]);
      ("required", `List [`String "action"]);
    ];
  };

  (* ============================================ *)
  (* MCP 2025-11-25 Spec Compliance Tools        *)
  (* ============================================ *)

  {
    name = "masc_mcp_session";
    description = "Manage MCP sessions (X-MCP-Session-ID). Sessions track client context across requests.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("action", `Assoc [
          ("type", `String "string");
          ("enum", `List [`String "get"; `String "create"; `String "list"; `String "cleanup"; `String "remove"]);
          ("description", `String "Session action");
        ]);
        ("session_id", `Assoc [
          ("type", `String "string");
          ("description", `String "Session ID (for get/remove)");
        ]);
        ("agent_name", `Assoc [
          ("type", `String "string");
          ("description", `String "Agent name (for create)");
        ]);
      ]);
      ("required", `List [`String "action"]);
    ];
  };

  {
    name = "masc_cancellation";
    description = "Manage cancellation tokens for long-running operations. Check tokens to abort work gracefully.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("action", `Assoc [
          ("type", `String "string");
          ("enum", `List [`String "create"; `String "cancel"; `String "check"; `String "list"; `String "cleanup"]);
          ("description", `String "Cancellation action");
        ]);
        ("token_id", `Assoc [
          ("type", `String "string");
          ("description", `String "Token ID (for cancel/check)");
        ]);
        ("reason", `Assoc [
          ("type", `String "string");
          ("description", `String "Cancellation reason");
        ]);
      ]);
      ("required", `List [`String "action"]);
    ];
  };

  {
    name = "masc_subscription";
    description = "Subscribe to resource changes (tasks, agents, messages, votes). Receive notifications via polling or SSE.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("action", `Assoc [
          ("type", `String "string");
          ("enum", `List [`String "subscribe"; `String "unsubscribe"; `String "list"; `String "poll"]);
          ("description", `String "Subscription action");
        ]);
        ("subscriber", `Assoc [
          ("type", `String "string");
          ("description", `String "Subscriber ID (agent_name or session_id)");
        ]);
        ("resource", `Assoc [
          ("type", `String "string");
          ("enum", `List [`String "tasks"; `String "agents"; `String "messages"; `String "votes"]);
          ("description", `String "Resource type");
        ]);
        ("filter", `Assoc [
          ("type", `String "string");
          ("description", `String "Optional filter (specific ID or '*')");
        ]);
        ("subscription_id", `Assoc [
          ("type", `String "string");
          ("description", `String "Subscription ID (for unsubscribe/poll)");
        ]);
      ]);
      ("required", `List [`String "action"]);
    ];
  };

  {
    name = "masc_progress";
    description = "Send progress notifications for long-running tasks. Broadcasts via SSE using MCP notifications/progress format.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("action", `Assoc [
          ("type", `String "string");
          ("enum", `List [`String "start"; `String "update"; `String "step"; `String "complete"; `String "stop"]);
          ("description", `String "Progress action: start tracking, update progress, step forward, complete, or stop tracking");
        ]);
        ("task_id", `Assoc [
          ("type", `String "string");
          ("description", `String "Task identifier for progress tracking");
        ]);
        ("progress", `Assoc [
          ("type", `String "number");
          ("description", `String "Progress value (0.0 to 1.0, for 'update' action)");
        ]);
        ("message", `Assoc [
          ("type", `String "string");
          ("description", `String "Optional progress message");
        ]);
        ("total_steps", `Assoc [
          ("type", `String "integer");
          ("description", `String "Total steps (for 'start' action, default: 100)");
        ]);
      ]);
      ("required", `List [`String "action"; `String "task_id"]);
    ];
  };

  (* Cellular Agent - Handover Tools *)
  {
    name = "masc_handover_create";
    description = "Create a handover record (agent's 'last will') before context limit or session end. Contains goal, progress, decisions, warnings for the next agent. Inspired by Stanford Generative Agents memory stream + Erlang 'let it crash' supervisor pattern.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("agent_name", `Assoc [
          ("type", `String "string");
          ("description", `String "Your agent name (the dying agent)");
        ]);
        ("task_id", `Assoc [
          ("type", `String "string");
          ("description", `String "Task being worked on");
        ]);
        ("session_id", `Assoc [
          ("type", `String "string");
          ("description", `String "Current session identifier");
        ]);
        ("reason", `Assoc [
          ("type", `String "string");
          ("enum", `List [`String "context_limit"; `String "timeout"; `String "explicit"; `String "error"; `String "complete"]);
          ("description", `String "Why handover is triggered");
        ]);
        ("goal", `Assoc [
          ("type", `String "string");
          ("description", `String "Current goal being pursued");
        ]);
        ("progress", `Assoc [
          ("type", `String "string");
          ("description", `String "Summary of progress made");
        ]);
        ("completed_steps", `Assoc [
          ("type", `String "array");
          ("items", `Assoc [("type", `String "string")]);
          ("description", `String "Steps already completed");
        ]);
        ("pending_steps", `Assoc [
          ("type", `String "array");
          ("items", `Assoc [("type", `String "string")]);
          ("description", `String "Steps remaining to do");
        ]);
        ("decisions", `Assoc [
          ("type", `String "array");
          ("items", `Assoc [("type", `String "string")]);
          ("description", `String "Key decisions made and why (implicit knowledge transfer)");
        ]);
        ("assumptions", `Assoc [
          ("type", `String "array");
          ("items", `Assoc [("type", `String "string")]);
          ("description", `String "What we're assuming is true");
        ]);
        ("warnings", `Assoc [
          ("type", `String "array");
          ("items", `Assoc [("type", `String "string")]);
          ("description", `String "Gotchas and things to watch out for");
        ]);
        ("errors", `Assoc [
          ("type", `String "array");
          ("items", `Assoc [("type", `String "string")]);
          ("description", `String "Unresolved errors from PDCA loop");
        ]);
        ("files", `Assoc [
          ("type", `String "array");
          ("items", `Assoc [("type", `String "string")]);
          ("description", `String "Files modified during this session");
        ]);
        ("context_pct", `Assoc [
          ("type", `String "integer");
          ("description", `String "Context usage percentage when handover triggered");
        ]);
      ]);
      ("required", `List [`String "agent_name"; `String "task_id"; `String "reason"; `String "goal"]);
    ];
  };

  {
    name = "masc_handover_list";
    description = "List all handover records, optionally filtering by pending (unclaimed) ones. Use to see what work is waiting to be picked up.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("pending_only", `Assoc [
          ("type", `String "boolean");
          ("description", `String "If true, only show unclaimed handovers");
          ("default", `Bool false);
        ]);
      ]);
    ];
  };

  {
    name = "masc_handover_claim";
    description = "Claim a pending handover to continue the work. You become the successor agent. The handover DNA will be loaded into your context.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("agent_name", `Assoc [
          ("type", `String "string");
          ("description", `String "Your agent name (the successor)");
        ]);
        ("handover_id", `Assoc [
          ("type", `String "string");
          ("description", `String "ID of the handover to claim");
        ]);
      ]);
      ("required", `List [`String "agent_name"; `String "handover_id"]);
    ];
  };

  {
    name = "masc_handover_get";
    description = "Get full details of a handover record as formatted markdown. Use to understand context before claiming.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("handover_id", `Assoc [
          ("type", `String "string");
          ("description", `String "ID of the handover to retrieve");
        ]);
      ]);
      ("required", `List [`String "handover_id"]);
    ];
  };

  (* Auto-spawn on claim *)
  {
    name = "masc_handover_claim_and_spawn";
    description = "Claim a handover AND automatically spawn the successor agent with the DNA. The successor agent will receive the handover context as its initial prompt and begin work immediately.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("handover_id", `Assoc [
          ("type", `String "string");
          ("description", `String "ID of the handover to claim");
        ]);
        ("agent_name", `Assoc [
          ("type", `String "string");
          ("description", `String "Agent to spawn (claude, gemini, codex, ollama)");
        ]);
        ("additional_instructions", `Assoc [
          ("type", `String "string");
          ("description", `String "Optional extra instructions for the successor agent");
        ]);
        ("timeout_seconds", `Assoc [
          ("type", `String "integer");
          ("description", `String "Timeout for the spawned agent (default: 300)");
        ]);
      ]);
      ("required", `List [`String "handover_id"; `String "agent_name"]);
    ];
  };

  (* ===== Execution Memory Tools ===== *)

  {
    name = "masc_run_init";
    description = "Initialize execution memory for a task. Creates .masc/runs/{task_id}/ to track plan, notes, and deliverables.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("task_id", `Assoc [
          ("type", `String "string");
          ("description", `String "Task ID to track");
        ]);
        ("agent_name", `Assoc [
          ("type", `String "string");
          ("description", `String "Agent working on the task");
        ]);
      ]);
      ("required", `List [`String "task_id"; `String "agent_name"]);
    ];
  };

  {
    name = "masc_run_plan";
    description = "Set or update the execution plan for a task run. \
Use at start of work to document your approach. Supports markdown. \
Plan is versioned - updates create new revision. Other agents can view via masc_run_get. \
Example: masc_run_plan({task_id: 'task-001', plan: '## Steps\\n1. Analyze code\\n2. Write tests\\n3. Refactor'})";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("task_id", `Assoc [
          ("type", `String "string");
          ("description", `String "Task ID");
        ]);
        ("plan", `Assoc [
          ("type", `String "string");
          ("description", `String "The plan (markdown supported)");
        ]);
      ]);
      ("required", `List [`String "task_id"; `String "plan"]);
    ];
  };

  {
    name = "masc_run_log";
    description = "Add a timestamped note to task's execution log. \
Use for: progress updates, blockers found, decisions made, key findings. \
Auto-timestamps with ISO8601. Creates audit trail for handoffs. \
Example: masc_run_log({task_id: 'task-001', note: 'Found 3 failing tests in auth module'})";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("task_id", `Assoc [
          ("type", `String "string");
          ("description", `String "Task ID");
        ]);
        ("note", `Assoc [
          ("type", `String "string");
          ("description", `String "Note to add (will be timestamped)");
        ]);
      ]);
      ("required", `List [`String "task_id"; `String "note"]);
    ];
  };

  {
    name = "masc_run_deliverable";
    description = "Record the final deliverable/output of a task run. Marks the run as completed.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("task_id", `Assoc [
          ("type", `String "string");
          ("description", `String "Task ID");
        ]);
        ("deliverable", `Assoc [
          ("type", `String "string");
          ("description", `String "The deliverable (markdown supported)");
        ]);
      ]);
      ("required", `List [`String "task_id"; `String "deliverable"]);
    ];
  };

  {
    name = "masc_run_get";
    description = "Get full execution history for a task as markdown. \
Returns: plan, logs (timestamped), and deliverable if completed. \
Use when: resuming work, reviewing progress, preparing handoff. \
Example: masc_run_get({task_id: 'task-001'}) → '## Plan\\n...\\n## Logs\\n- 10:00 Started...'";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("task_id", `Assoc [
          ("type", `String "string");
          ("description", `String "Task ID to retrieve");
        ]);
      ]);
      ("required", `List [`String "task_id"]);
    ];
  };

  {
    name = "masc_run_list";
    description = "List all task runs with status (active/completed). \
Shows: task_id, has_plan, log_count, has_deliverable. \
Use to find: abandoned work, completed runs for review, active executions. \
Example response: [{task_id: 'task-001', status: 'active', logs: 5}]";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc []);
    ];
  };

  (* ===== Cache Tools (Phase 11) ===== *)
  {
    name = "masc_cache_set";
    description = "Set a cache entry for sharing context between agents. Useful for caching file contents, API responses, or expensive computations.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("key", `Assoc [
          ("type", `String "string");
          ("description", `String "Cache key (e.g., 'file:src/main.ts', 'jira:PK-123')");
        ]);
        ("value", `Assoc [
          ("type", `String "string");
          ("description", `String "Value to cache");
        ]);
        ("ttl_seconds", `Assoc [
          ("type", `String "integer");
          ("description", `String "Time-to-live in seconds. Omit for no expiry.");
        ]);
        ("tags", `Assoc [
          ("type", `String "array");
          ("items", `Assoc [("type", `String "string")]);
          ("description", `String "Tags for filtering (e.g., ['file', 'typescript'])");
        ]);
      ]);
      ("required", `List [`String "key"; `String "value"]);
    ];
  };

  {
    name = "masc_cache_get";
    description = "Get a cached entry by key. Returns null if not found or expired.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("key", `Assoc [
          ("type", `String "string");
          ("description", `String "Cache key to retrieve");
        ]);
      ]);
      ("required", `List [`String "key"]);
    ];
  };

  {
    name = "masc_cache_delete";
    description = "Delete a specific cache entry. \
Use when: invalidating stale data, clearing specific key, freeing memory. \
No error if key doesn't exist. Use masc_cache_list to find keys.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("key", `Assoc [
          ("type", `String "string");
          ("description", `String "Cache key to delete");
        ]);
      ]);
      ("required", `List [`String "key"]);
    ];
  };

  {
    name = "masc_cache_list";
    description = "List cache entries with keys, TTL remaining, and tags. \
Filter by tag to find related entries. Shows creation time and expiry. \
Use before: cache cleanup, debugging stale data, finding specific entries. \
Example: masc_cache_list({tag: 'api'}) → [{key: 'user_123', ttl: 3600, tags: ['api']}]";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("tag", `Assoc [
          ("type", `String "string");
          ("description", `String "Filter by tag (optional)");
        ]);
      ]);
    ];
  };

  {
    name = "masc_cache_clear";
    description = "Delete ALL cache entries. DESTRUCTIVE - cannot be undone. \
Use only when: resetting room state, debugging cache issues, fresh start. \
Consider masc_cache_delete for targeted cleanup instead.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc []);
    ];
  };

  {
    name = "masc_cache_stats";
    description = "Get cache usage statistics. \
Shows: total entries, memory size, oldest/newest entry age, hit/miss ratio. \
Use to: monitor cache health, decide when to clear, debug performance.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc []);
    ];
  };

  (* ===== Tempo Tools (Phase 12) ===== *)

  {
    name = "masc_tempo_get";
    description = "Get current orchestrator tempo (check interval). Shows adaptive tempo status.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc []);
    ];
  };

  {
    name = "masc_tempo_set";
    description = "Set orchestrator tempo manually. Interval is clamped between 60s (fast) and 600s (slow).";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("interval_seconds", `Assoc [
          ("type", `String "number");
          ("description", `String "Check interval in seconds (60-600)");
        ]);
        ("reason", `Assoc [
          ("type", `String "string");
          ("description", `String "Reason for tempo change");
        ]);
      ]);
      ("required", `List [`String "interval_seconds"]);
    ];
  };

  {
    name = "masc_tempo_adjust";
    description = "Automatically adjust tempo based on pending task urgency. Fast for urgent tasks, slow when idle.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc []);
    ];
  };

  {
    name = "masc_tempo_reset";
    description = "Reset room tempo to default 300s (5 minutes). \
Tempo controls SSE heartbeat interval and agent timeout detection. \
Use after: intensive work phase complete, debugging tempo issues. \
Lower tempo = faster detection but more overhead. Default balances both. \
Example: masc_tempo_reset() → {tempo: 300, message: 'Reset to default'}";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc []);
    ];
  };

  (* ===== Dashboard Tools (Phase 13) ===== *)
  {
    name = "masc_dashboard";
    description = "Show full MASC dashboard with agents, tasks, locks, messages, tempo, and worktrees. Use with 'watch -n 1' for real-time updates.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("compact", `Assoc [
          ("type", `String "boolean");
          ("description", `String "If true, show compact single-line summary instead of full dashboard");
        ]);
      ]);
    ];
  };

  (* ===== Level 2: Organization Tools (Phase 13) ===== *)

  (* Fitness Selection *)
  {
    name = "masc_agent_fitness";
    description = "Get fitness scores for agents based on performance metrics. Higher scores indicate better performance (completion rate, reliability, speed). Use for understanding agent capabilities.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("agent_name", `Assoc [
          ("type", `String "string");
          ("description", `String "Optional: Get fitness for specific agent. If omitted, returns all agents.");
        ]);
        ("days", `Assoc [
          ("type", `String "integer");
          ("description", `String "Number of days to analyze (default: 7)");
          ("default", `Int 7);
        ]);
      ]);
    ];
  };

  {
    name = "masc_select_agent";
    description = "Select the best agent for a task based on fitness scores. Uses weighted scoring: completion (35%), reliability (25%), speed (15%), handoff (15%), collaboration (10%).";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("available_agents", `Assoc [
          ("type", `String "array");
          ("items", `Assoc [("type", `String "string")]);
          ("description", `String "List of available agent names to choose from");
        ]);
        ("strategy", `Assoc [
          ("type", `String "string");
          ("enum", `List [`String "capability_first"; `String "elite_1"; `String "roulette_wheel"; `String "random"]);
          ("description", `String "Selection strategy (default: capability_first)");
          ("default", `String "capability_first");
        ]);
        ("days", `Assoc [
          ("type", `String "integer");
          ("description", `String "Days of metrics to consider (default: 7)");
          ("default", `Int 7);
        ]);
      ]);
      ("required", `List [`String "available_agents"]);
    ];
  };

  (* Hebbian Learning *)
  {
    name = "masc_collaboration_graph";
    description = "View the Hebbian collaboration graph showing learned agent relationships. Stronger connections indicate successful collaboration patterns.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("format", `Assoc [
          ("type", `String "string");
          ("enum", `List [`String "text"; `String "json"]);
          ("description", `String "Output format (default: text)");
          ("default", `String "text");
        ]);
      ]);
    ];
  };

  {
    name = "masc_consolidate_learning";
    description = "Trigger Hebbian consolidation - apply decay to old collaboration patterns and prune weak connections. Mimics memory consolidation during sleep.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("decay_after_days", `Assoc [
          ("type", `String "integer");
          ("description", `String "Apply decay to connections older than this (default: 7)");
          ("default", `Int 7);
        ]);
      ]);
    ];
  };

  (* Drift Guard *)
  {
    name = "masc_verify_handoff";
    description = "Verify handoff context integrity. Compares original and received context to detect semantic drift, information loss, or distortion. Threshold: 0.85 similarity.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("original", `Assoc [
          ("type", `String "string");
          ("description", `String "Original context before handoff");
        ]);
        ("received", `Assoc [
          ("type", `String "string");
          ("description", `String "Received context after handoff");
        ]);
        ("threshold", `Assoc [
          ("type", `String "number");
          ("description", `String "Similarity threshold (default: 0.85)");
          ("default", `Float 0.85);
        ]);
      ]);
      ("required", `List [`String "original"; `String "received"]);
    ];
  };

  (* Metrics *)
  {
    name = "masc_get_metrics";
    description = "Get raw performance metrics for an agent. Returns task completion data, timing, error rates, and collaboration history.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("agent_name", `Assoc [
          ("type", `String "string");
          ("description", `String "Agent name to get metrics for");
        ]);
        ("days", `Assoc [
          ("type", `String "integer");
          ("description", `String "Number of days of history (default: 7)");
          ("default", `Int 7);
        ]);
      ]);
      ("required", `List [`String "agent_name"]);
    ];
  };

  (* ===== Level 4: Swarm Intelligence Tools ===== *)

  {
    name = "masc_swarm_init";
    description = "Initialize a new swarm for emergent collective intelligence. Creates swarm with configurable behavior mode: flocking (cluster around success), foraging (distributed search), stigmergy (pheromone trails), or quorum_sensing (collective decisions).";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("behavior", `Assoc [
          ("type", `String "string");
          ("enum", `List [`String "flocking"; `String "foraging"; `String "stigmergy"; `String "quorum_sensing"]);
          ("description", `String "Swarm behavior mode (default: flocking)");
          ("default", `String "flocking");
        ]);
        ("selection_pressure", `Assoc [
          ("type", `String "number");
          ("description", `String "Evolution selection pressure 0.0-1.0 (default: 0.3)");
          ("default", `Float 0.3);
        ]);
        ("mutation_rate", `Assoc [
          ("type", `String "number");
          ("description", `String "Agent mutation rate 0.0-1.0 (default: 0.1)");
          ("default", `Float 0.1);
        ]);
      ]);
    ];
  };

  {
    name = "masc_swarm_join";
    description = "Join an agent to the swarm. Agents in swarm participate in collective behaviors like flocking, foraging, and quorum voting.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("agent_name", `Assoc [
          ("type", `String "string");
          ("description", `String "Agent name to add to swarm");
        ]);
      ]);
      ("required", `List [`String "agent_name"]);
    ];
  };

  {
    name = "masc_swarm_leave";
    description = "Remove yourself from the swarm collective. \
Differs from masc_leave: swarm is a coordination layer on top of room. \
Agent stays in room but exits swarm consensus/voting. \
Use when: switching to solo mode, swarm task complete, reducing overhead. \
Example: masc_swarm_leave({agent_name: 'claude-xyz'})";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("agent_name", `Assoc [
          ("type", `String "string");
          ("description", `String "Agent name to remove from swarm");
        ]);
      ]);
      ("required", `List [`String "agent_name"]);
    ];
  };

  {
    name = "masc_swarm_status";
    description = "Get current swarm status including active agents, behavior mode, pheromone trails, and pending proposals.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc []);
    ];
  };

  {
    name = "masc_swarm_evolve";
    description = "Run one evolution cycle on the swarm. Applies selection pressure, fitness-based ranking, and optional mutation. High-fitness agents are preserved while low-fitness may be replaced.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc []);
    ];
  };

  {
    name = "masc_swarm_propose";
    description = "Create a quorum proposal for collective decision making. Proposals require threshold (default 60%) of agents to vote in favor.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("description", `Assoc [
          ("type", `String "string");
          ("description", `String "Description of the proposal");
        ]);
        ("threshold", `Assoc [
          ("type", `String "number");
          ("description", `String "Vote threshold 0.0-1.0 (default: 0.6)");
          ("default", `Float 0.6);
        ]);
      ]);
      ("required", `List [`String "description"]);
    ];
  };

  {
    name = "masc_swarm_vote";
    description = "Vote on a swarm proposal. When threshold is reached, proposal status updates to approved/rejected.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("proposal_id", `Assoc [
          ("type", `String "string");
          ("description", `String "ID of the proposal to vote on");
        ]);
        ("vote_for", `Assoc [
          ("type", `String "boolean");
          ("description", `String "true to vote for, false to vote against");
        ]);
      ]);
      ("required", `List [`String "proposal_id"; `String "vote_for"]);
    ];
  };

  {
    name = "masc_swarm_deposit";
    description = "Deposit pheromone on a path to signal success. Other agents following stigmergy behavior will be attracted to strong pheromone trails.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("path_id", `Assoc [
          ("type", `String "string");
          ("description", `String "Identifier for the path/solution");
        ]);
        ("strength", `Assoc [
          ("type", `String "number");
          ("description", `String "Pheromone strength to deposit (default: 0.2)");
          ("default", `Float 0.2);
        ]);
      ]);
      ("required", `List [`String "path_id"]);
    ];
  };

  {
    name = "masc_swarm_trails";
    description = "Get the strongest pheromone trails in the swarm. Useful for finding successful paths/solutions discovered by other agents.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("limit", `Assoc [
          ("type", `String "integer");
          ("description", `String "Maximum trails to return (default: 5)");
          ("default", `Int 5);
        ]);
      ]);
    ];
  };

  (* ============================================ *)
  (* Multi-Room Management Tools                  *)
  (* ============================================ *)

  {
    name = "masc_rooms_list";
    description = "List all available MASC rooms. Returns rooms with agent/task counts and current active room. Use to see available coordination spaces before entering one.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc []);
    ];
  };

  {
    name = "masc_room_create";
    description = "Create a new MASC room for coordination. Room ID is auto-generated from name (slugified). Use to create separate spaces for different projects or teams.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("name", `Assoc [
          ("type", `String "string");
          ("description", `String "Room display name (e.g., 'My Project Dev', 'Personal Projects')");
        ]);
        ("description", `Assoc [
          ("type", `String "string");
          ("description", `String "Optional room description");
        ]);
      ]);
      ("required", `List [`String "name"]);
    ];
  };

  {
    name = "masc_room_enter";
    description = "Enter a specific MASC room. Switches context to the selected room and auto-joins with a unique nickname. Use after masc_rooms_list to switch between coordination spaces.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("room_id", `Assoc [
          ("type", `String "string");
          ("description", `String "Room ID to enter (e.g., 'my-project-dev', 'default')");
        ]);
        ("agent_type", `Assoc [
          ("type", `String "string");
          ("description", `String "Your agent type: 'claude', 'gemini', or 'codex'");
          ("default", `String "claude");
        ]);
      ]);
      ("required", `List [`String "room_id"]);
    ];
  };

  (* ============================================ *)
  (* Audit & Governance Tools (Trust Building)   *)
  (* ============================================ *)

  {
    name = "masc_audit_query";
    description = "Query audit logs to inspect agent actions and security events. Returns recent security events: auth success/failure, anomalies, violations. Use for trust verification and debugging collaboration issues.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("agent", `Assoc [
          ("type", `String "string");
          ("description", `String "Filter by agent name (optional)");
        ]);
        ("event_type", `Assoc [
          ("type", `String "string");
          ("enum", `List [
            `String "auth_success";
            `String "auth_failure";
            `String "anomaly_detected";
            `String "security_violation";
            `String "tool_call";
            `String "all"
          ]);
          ("description", `String "Filter by event type (default: all)");
          ("default", `String "all");
        ]);
        ("limit", `Assoc [
          ("type", `String "integer");
          ("description", `String "Maximum events to return (default: 50)");
          ("default", `Int 50);
        ]);
        ("since_hours", `Assoc [
          ("type", `String "number");
          ("description", `String "Only show events from last N hours (default: 24)");
          ("default", `Float 24.0);
        ]);
      ]);
    ];
  };

  {
    name = "masc_audit_stats";
    description = "Get security statistics and trust metrics for agents. Shows auth success rate, anomaly count, task completion rate per agent. Use to evaluate agent reliability before delegation.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("agent", `Assoc [
          ("type", `String "string");
          ("description", `String "Specific agent to analyze (optional, shows all if omitted)");
        ]);
      ]);
    ];
  };

  {
    name = "masc_governance_set";
    description = "Configure governance policies for the room. Enables audit logging, anomaly detection, and agent isolation. Enterprise security for production use.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("level", `Assoc [
          ("type", `String "string");
          ("enum", `List [
            `String "development";
            `String "production";
            `String "enterprise";
            `String "paranoid"
          ]);
          ("description", `String "Security level: development (permissive), production (basic), enterprise (audit+encryption), paranoid (max isolation)");
          ("default", `String "production");
        ]);
        ("audit_enabled", `Assoc [
          ("type", `String "boolean");
          ("description", `String "Enable audit logging (default: true for production+)");
          ("default", `Bool true);
        ]);
        ("anomaly_detection", `Assoc [
          ("type", `String "boolean");
          ("description", `String "Enable anomaly detection (auth spikes, low success rate)");
          ("default", `Bool false);
        ]);
      ]);
    ];
  };

  (* Walph Pattern: Iterative Task Loop
     Integration with llm-mcp chains:
     - preset="coverage" → chain.orchestrate walph-coverage (FeedbackLoop for test coverage)
     - preset="refactor" → chain.orchestrate walph-refactor (FeedbackLoop for lint errors)
     - preset="docs" → chain.orchestrate walph-docs (FeedbackLoop for documentation)
     - preset="drain" → simple task claiming without chain execution *)
  {
    name = "masc_walph_loop";
    description = "Walph pattern: Keep claiming and completing tasks until stop condition. Iterates claim_next → work → done cycle. Control via @walph in broadcast: START <preset>, STOP, PAUSE, RESUME, STATUS. Presets map to llm-mcp FeedbackLoop chains: coverage → walph-coverage, refactor → walph-refactor, docs → walph-docs, review → pr-review-pipeline, drain → simple claim loop.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("agent_name", `Assoc [
          ("type", `String "string");
          ("description", `String "Your agent name for claiming tasks");
        ]);
        ("preset", `Assoc [
          ("type", `String "string");
          ("enum", `List [
            `String "coverage";
            `String "refactor";
            `String "docs";
            `String "review";
            `String "drain"
          ]);
          ("description", `String "Loop preset: coverage (80%+ test coverage), refactor (0 lint errors), docs (90%+ doc coverage), review (PR self-review), drain (empty backlog)");
          ("default", `String "drain");
        ]);
        ("max_iterations", `Assoc [
          ("type", `String "integer");
          ("description", `String "Maximum iterations before forced stop (default: 10)");
          ("default", `Int 10);
          ("minimum", `Int 1);
          ("maximum", `Int 100);
        ]);
        ("target", `Assoc [
          ("type", `String "string");
          ("description", `String "Target file or directory for preset (e.g., src/utils.ts)");
        ]);
      ]);
      ("required", `List [`String "agent_name"]);
    ];
  };

  (* Walph Control: STOP, PAUSE, RESUME, STATUS *)
  {
    name = "masc_walph_control";
    description = "Control a running @walph loop. Commands: STOP (end loop after current iteration), PAUSE (suspend loop), RESUME (continue paused loop), STATUS (get current state). Can also be triggered via broadcast: '@walph STOP', '@walph PAUSE', etc.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("command", `Assoc [
          ("type", `String "string");
          ("description", `String "Control command");
          ("enum", `List [`String "STOP"; `String "PAUSE"; `String "RESUME"; `String "STATUS"]);
        ]);
        ("agent_name", `Assoc [
          ("type", `String "string");
          ("description", `String "Agent sending the command (for audit trail)");
        ]);
      ]);
      ("required", `List [`String "command"; `String "agent_name"]);
    ];
  };

  (* Walph Natural Language: Control walph via natural language *)
  {
    name = "masc_walph_natural";
    description = "Control Walph loop using natural language. Heuristic-based intent classification (Korean/English). Examples: '커버리지 올려줘' → START coverage, '그만' → STOP, '잠깐 멈춰' → PAUSE, '다시 시작' → RESUME, '지금 뭐해?' → STATUS. Falls back to llm-mcp for ambiguous messages (requires llm-mcp server on port 8932).";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("message", `Assoc [
          ("type", `String "string");
          ("description", `String "Natural language message to interpret (e.g., '커버리지 좀 올려줘', 'stop the loop', '지금 진행상황 알려줘')");
        ]);
        ("agent_name", `Assoc [
          ("type", `String "string");
          ("description", `String "Agent sending the command (for audit trail)");
        ]);
      ]);
      ("required", `List [`String "message"; `String "agent_name"]);
    ];
  };

  (* Swarm Walph: Coordinate multiple Walph instances *)
  {
    name = "masc_swarm_walph";
    description = "Swarm-level control for all Walph instances in the room. View aggregate status, stop/pause/resume all running loops at once. Commands: STATUS (show all Walph states), STOP (signal all to stop), PAUSE (pause all running), RESUME (resume all paused). Useful for coordinating multi-agent workloads.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("command", `Assoc [
          ("type", `String "string");
          ("description", `String "Swarm control command");
          ("enum", `List [`String "STATUS"; `String "STOP"; `String "PAUSE"; `String "RESUME"]);
          ("default", `String "STATUS");
        ]);
        ("agent_name", `Assoc [
          ("type", `String "string");
          ("description", `String "Agent issuing the swarm command (for audit trail)");
        ]);
      ]);
      ("required", `List [`String "agent_name"]);
    ];
  };

  (* Hat System: Role-based personas for agents *)
  {
    name = "masc_hat_wear";
    description = "Wear a hat (persona) to specialize agent behavior. Hats: builder (🔨 code), reviewer (🔍 review), researcher (🔬 explore), tester (🧪 tests), architect (📐 design), debugger (🐛 fix), documenter (📝 docs). Broadcast format: @agent:hat (e.g., @claude:builder).";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("hat", `Assoc [
          ("type", `String "string");
          ("description", `String "Hat to wear");
          ("enum", `List [
            `String "builder"; `String "reviewer"; `String "researcher";
            `String "tester"; `String "architect"; `String "debugger"; `String "documenter"
          ]);
          ("default", `String "builder");
        ]);
        ("agent_name", `Assoc [
          ("type", `String "string");
          ("description", `String "Agent wearing the hat");
        ]);
      ]);
      ("required", `List [`String "agent_name"]);
    ];
  };

  {
    name = "masc_hat_status";
    description = "Show current hat status for all agents. Displays which persona each agent is currently using.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("agent_name", `Assoc [
          ("type", `String "string");
          ("description", `String "Agent requesting status");
        ]);
      ]);
      ("required", `List [`String "agent_name"]);
    ];
  };

  (* Bounded Autonomy: Constrained multi-agent execution with formal guarantees *)
  {
    name = "masc_bounded_run";
    description = "Run multi-agent loop with formal constraints. Guarantees: termination (hard_max_iterations), safety (post-check prevents silent violations), predictive limits (token_buffer). Use for autonomous agent collaboration with budget control.";
    input_schema = `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [
        ("agents", `Assoc [
          ("type", `String "array");
          ("items", `Assoc [("type", `String "string")]);
          ("description", `String "List of agents to use in round-robin: ['gemini', 'codex', 'claude']");
        ]);
        ("prompt", `Assoc [
          ("type", `String "string");
          ("description", `String "Initial prompt for agents");
        ]);
        ("constraints", `Assoc [
          ("type", `String "object");
          ("description", `String "Execution limits");
          ("properties", `Assoc [
            ("max_turns", `Assoc [
              ("type", `String "integer");
              ("description", `String "Maximum agent turns (default: 10)");
            ]);
            ("max_tokens", `Assoc [
              ("type", `String "integer");
              ("description", `String "Maximum total tokens (default: 100000)");
            ]);
            ("max_cost_usd", `Assoc [
              ("type", `String "number");
              ("description", `String "Maximum cost in USD (default: 1.0)");
            ]);
            ("max_time_seconds", `Assoc [
              ("type", `String "number");
              ("description", `String "Maximum wall-clock time (default: 300)");
            ]);
            ("token_buffer", `Assoc [
              ("type", `String "integer");
              ("description", `String "Buffer for predictive token limit (default: 5000)");
            ]);
            ("hard_max_iterations", `Assoc [
              ("type", `String "integer");
              ("description", `String "Absolute failsafe iteration limit (default: 100)");
            ]);
          ]);
        ]);
        ("goal", `Assoc [
          ("type", `String "object");
          ("description", `String "Termination condition");
          ("properties", `Assoc [
            ("path", `Assoc [
              ("type", `String "string");
              ("description", `String "JSONPath to check in agent output, e.g., '$.status' or '$.result.done'");
            ]);
            ("condition", `Assoc [
              ("type", `String "object");
              ("description", `String "Comparison: {eq: value}, {gte: 0.95}, {lt: 5}, {in: ['done', 'success']}");
            ]);
          ]);
          ("required", `List [`String "path"; `String "condition"]);
        ]);
      ]);
      ("required", `List [`String "agents"; `String "prompt"; `String "goal"]);
    ];
  };
]

(** Get tool by name *)
let find_tool name =
  List.find_opt (fun s -> s.name = name) all_schemas
