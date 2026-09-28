#!/usr/bin/env python3

import json
import subprocess
from dataclasses import dataclass
from datetime import datetime

# ---------------------------------------------------------------------------
# Queries
# ---------------------------------------------------------------------------

# `user-review-requested` means requested from me directly, not via a team.
# (section title, org TODO keyword, GitHub search query)
SECTIONS = [
    ("Waiting for my review", "TODO",
     "is:pr is:open archived:false user-review-requested:@me"),
    ("Reviewed by me, waiting on others", "WAITING",
     "is:pr is:open archived:false reviewed-by:@me -user-review-requested:@me -author:@me"),
    ("My open PRs", "WAITING",
     "is:pr is:open archived:false author:@me"),
]

GRAPHQL = """
query($q: String!, $me: String!, $endCursor: String) {
  search(query: $q, type: ISSUE, first: 100, after: $endCursor) {
    pageInfo { hasNextPage endCursor }
    nodes {
      ... on PullRequest {
        number title url headRefName updatedAt reviewDecision
        repository { nameWithOwner }
        author { login }
        reviews(author: $me, last: 1) { nodes { state } }
      }
    }
  }
}
"""

# ---------------------------------------------------------------------------
# Fetching
# ---------------------------------------------------------------------------

@dataclass
class PullRequest:
    ref: str
    title: str
    url: str
    branch: str
    updated_at: datetime
    author: str
    my_review: str | None        # APPROVED / CHANGES_REQUESTED / COMMENTED / PENDING
    review_decision: str | None  # APPROVED / CHANGES_REQUESTED / REVIEW_REQUIRED


def gh(*args: str) -> str:
    return subprocess.run(["gh", *args], stdout=subprocess.PIPE, text=True, check=True).stdout


def search(query: str, me: str) -> list[PullRequest]:
    # --paginate concatenates one JSON document per page.
    raw = gh("api", "graphql", "--paginate",
             "-f", f"q={query}", "-f", f"me={me}", "-f", f"query={GRAPHQL}")
    decoder = json.JSONDecoder()
    prs, pos = [], 0
    while pos < len(raw.rstrip()):
        page, pos = decoder.raw_decode(raw, pos)
        prs += [parse(node) for node in page["data"]["search"]["nodes"]]
        while pos < len(raw) and raw[pos].isspace():
            pos += 1
    return sorted(prs, key=lambda pr: pr.updated_at, reverse=True)


def parse(node: dict) -> PullRequest:
    reviews = node["reviews"]["nodes"]
    return PullRequest(
        ref=f'{node["repository"]["nameWithOwner"]}#{node["number"]}',
        title=node["title"],
        url=node["url"],
        branch=node["headRefName"],
        updated_at=datetime.fromisoformat(node["updatedAt"]),
        author=(node.get("author") or {}).get("login", "ghost"),
        my_review=reviews[0]["state"] if reviews else None,
        review_decision=node.get("reviewDecision"),
    )

# ---------------------------------------------------------------------------
# Org output
# ---------------------------------------------------------------------------

def clean(text: str) -> str:
    # Keep heading and property lines single-line.
    return " ".join(text.split())


def entry(pr: PullRequest, keyword: str) -> str:
    props = [
        ("AUTHOR", pr.author),
        ("UPDATED", pr.updated_at.strftime("%Y-%m-%d")),
        ("MY_REVIEW", pr.my_review or "-"),
        ("DECISION", pr.review_decision or "-"),
        ("URL", pr.url),
    ]
    branch = clean(pr.branch)
    # copy: is a custom link type defined in review-requests.el; following it copies the branch.
    lines = [f"** {keyword} [[{pr.url}][{pr.ref}]] [[copy:{branch}][{branch}]]", clean(pr.title), ":PROPERTIES:"]
    lines += [f":{key}: {value}" for key, value in props]
    lines.append(":END:")
    return "\n".join(lines)


def section(title: str, keyword: str, prs: list[PullRequest]) -> str:
    heading = f"* {title} ({len(prs)})"
    body = "\n".join(entry(pr, keyword) for pr in prs) if prs else "/none/"
    return heading + "\n" + body

# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main() -> None:
    me = gh("api", "user", "--jq", ".login").strip()
    print(f"#+TITLE: GitHub PRs for {me}, {datetime.now():%Y-%m-%d %H:%M}")
    print("#+TODO: TODO | WAITING\n")
    print("\n\n".join(section(title, keyword, search(query, me))
                       for title, keyword, query in SECTIONS))


if __name__ == "__main__":
    main()
