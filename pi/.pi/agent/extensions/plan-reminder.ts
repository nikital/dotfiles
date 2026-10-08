/**
 * Plan Mode Reminder 
 *
 * Lightweight toggle that appends a planning reminder to every user message.
 * Does NOT disable tools — the agent still has full capabilities; it's just
 * instructed to plan, not execute.
 *
 * Usage:
 *   /plan          — toggle on/off
 *   Ctrl+Alt+P     — toggle on/off
 */

import type { ExtensionAPI, ExtensionContext } from "@earendil-works/pi-coding-agent";
import { Key } from "@earendil-works/pi-tui";

const STATUS_KEY = "plan-reminder";

interface PlanReminderState {
  enabled: boolean;
}

export default function (pi: ExtensionAPI) {
  let enabled = false;

  function updateStatus(ctx: ExtensionContext) {
    if (enabled) {
      ctx.ui.setStatus(STATUS_KEY, ctx.ui.theme.fg("warning", "⏸ plan"));
    } else {
      ctx.ui.setStatus(STATUS_KEY, undefined);
    }
  }

  function persist() {
    pi.appendEntry(STATUS_KEY, { enabled } satisfies PlanReminderState);
  }

  function toggle(ctx: ExtensionContext) {
    enabled = !enabled;
    updateStatus(ctx);
    persist();
    const msg = enabled
      ? "Plan mode enabled. Reminder will be appended to messages."
      : "Plan mode disabled.";
    ctx.ui.notify(msg, "info");
  }

  // --- Commands & shortcuts ---

  pi.registerCommand("plan", {
    description: "Toggle planning mode reminder",
    handler: async (_args, ctx) => toggle(ctx),
  });

  pi.registerShortcut(Key.ctrlAlt("p"), {
    description: "Toggle planning mode reminder",
    handler: async (ctx) => toggle(ctx),
  });

  // --- Inject reminder via before_agent_start ---

  pi.on("before_agent_start", async () => {
    if (!enabled) return;
    return {
      message: {
        customType: STATUS_KEY,
        content: "Reminder: you're planning, don't start executing the plan!",
        display: true,
      },
    };
  });

  // --- Persistence across sessions ---

  pi.on("session_start", async (_event, ctx) => {
    const saved = ctx.sessionManager
      .getEntries()
      .filter(
        (e: { type: string; customType?: string }) =>
          e.type === "custom" && e.customType === STATUS_KEY,
      )
      .pop() as { data?: PlanReminderState } | undefined;

    if (saved?.data?.enabled) {
      enabled = true;
    }
    updateStatus(ctx);
  });
}
