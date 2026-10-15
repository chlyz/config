# User environment instructions

The user is already authenticated with GitHub in the host environment. A
sandboxed `gh auth status` can incorrectly report that the token is invalid, so
do not treat that result as authoritative or ask the user to log in again.
When the user has authorized a Git or GitHub write operation, retry the actual
operation with elevated sandbox permissions first. Git index updates, commits,
and network pushes may require elevation because `.git` is read-only and
network credentials are unavailable inside the default sandbox.

## Repository text

- Use an 80-character line-length guideline for Markdown and normal text.
