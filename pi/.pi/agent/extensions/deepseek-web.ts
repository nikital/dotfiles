/**
 * DeepSeek Web - search and fetch live web content through DeepSeek's
 * in-house Search / Fetch API (https://api-docs.deepseek.com/guides/search).
 */

import {
  defineTool,
  type ExtensionAPI,
  type ExtensionToolContext,
} from "@earendil-works/pi-coding-agent";
import { Type } from "typebox";

async function post<T>(
  path: string,
  body: unknown,
  ctx: ExtensionToolContext,
  signal?: AbortSignal,
): Promise<T> {
  const model = ctx.modelRegistry
    .getAll()
    .find((m) => m.provider === "deepseek");
  if (!model) throw new Error("No deepseek model is configured.");

  const auth = await ctx.modelRegistry.getApiKeyAndHeaders(model);
  if (!auth.ok) throw new Error(auth.error);
  if (!auth.apiKey)
    throw new Error("The deepseek provider has no API key configured.");

  const baseUrl = model.baseUrl.replace(/\/+$/, "");
  const res = await fetch(`${baseUrl}/v1/tools/${path}`, {
    method: "POST",
    headers: {
      ...auth.headers,
      Authorization: `Bearer ${auth.apiKey}`,
      "Content-Type": "application/json",
    },
    body: JSON.stringify(body),
    signal,
  });
  if (!res.ok) {
    const data = await res.json().catch(() => undefined);
    throw new Error(
      data?.error?.message ?? `DeepSeek ${path} failed: HTTP ${res.status}`,
    );
  }
  return (await res.json()) as T;
}

const webSearch = defineTool({
  name: "web_search",
  label: "Web Search",
  description: `Search the live web with DeepSeek's search engine. Returns ranked results with title, URL, date, and summary. Supports operators in the query: site:, -site:, "exact phrase", -term, lang:en, after:2026-01-01, before:2026-06-01.`,
  promptSnippet: "web_search: search the live web (title, URL, date, summary)",
  parameters: Type.Object({
    query: Type.String({
      description:
        "Search query, 1-128 characters. Search operators are supported.",
    }),
    count: Type.Optional(
      Type.Integer({
        minimum: 1,
        maximum: 50,
        description: "Number of results to return (default 10).",
      }),
    ),
    need_content: Type.Optional(
      Type.Boolean({
        description:
          "Include extracted page text in each result (default false).",
      }),
    ),
  }),
  annotations: { readOnlyHint: true, openWorldHint: true },

  async execute(_id, params, signal, _onUpdate, ctx) {
    const data = await post<{ results: unknown[] }>(
      "search",
      params,
      ctx,
      signal,
    );
    return {
      content: [{ type: "text", text: JSON.stringify(data, null, 2) }],
      details: data,
    };
  },
});

const webFetch = defineTool({
  name: "web_fetch",
  label: "Web Fetch",
  description: "Fetch a single web page and return its content as Markdown.",
  promptSnippet: "web_fetch: fetch a web page as Markdown",
  parameters: Type.Object({
    url: Type.String({ description: "HTTP(S) URL of the page to fetch." }),
  }),
  annotations: { readOnlyHint: true, openWorldHint: true },

  async execute(_id, params, signal, _onUpdate, ctx) {
    const data = await post<{ title: string; content: string }>(
      "fetch",
      params,
      ctx,
      signal,
    );
    return {
      content: [{ type: "text", text: data.content }],
      details: data,
    };
  },
});

export default function (pi: ExtensionAPI) {
  pi.registerTool(webSearch);
  pi.registerTool(webFetch);
}
