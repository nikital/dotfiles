---
description: How I like to augment my review
---

I'm reviewing this branch (relative to origin/main).
Read commits on this branch for context.

This PR is structued into refactoring vs abstraction/extraction vs new logic commits.

For each refactoring commit and abstraction commit, I need to you kick a
subagent per-commit that will identify ALL behaviour changes in that
commit. Give context to each subagent regarding the bigger picture. The subagent
needs to be precise, which means for suspected code movements it needs to use
scripting to normalize the differences away until we surface all that has
changed. Don't just look, analyze using tools to confirm.

If the commit is about logic then the subagent should analyze the logic and I
want you to augment my review with important architectural points which I could
miss or subtle behavior that needs to be discussed. This is like making me a
"did you consider X?" list of points, not only actual bugs.

After this is mapped, the agent needs to create a report in like
04-code--Drop-the-token-param.md (04 being commit number) which will contain:
- High level summary of code movements
- ALL changes that aren't pure movements or path fixups following the movement.
- Possible issues/bugs/inconsisntencies for futher operator review.
(First prepare a unified example template that you'll give each subagent so all .mds
come back looking similar)

Finish when all the reports are available and convert all this party into html
using pandoc with cross linking (next / prev / index) for easy browsering. (you
may need to sudo dnf install it)

Go over all reports and surface high level points that are not related to a
specific commit but are cross cutting concerns. Arrange it so that the "next"
link on the last commit are those concerns so I see them last.
