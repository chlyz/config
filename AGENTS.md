# Working agreements

## Git workflow

- Make, inspect, and verify changes locally by default.
- Accumulate related work in the working tree; do not create a commit for every
  small change.
- Never run `git commit`, `git push`, or any other publishing action unless the
  user directly asks for that specific action in the current conversation.
- Before committing, show the user the accumulated diff and the proposed
  semantic commit message. Wait for their approval.
- A request to commit does not authorize pushing. A request to push does not
  authorize committing unrelated or unreviewed changes.
- Only force-push when the user explicitly requests it for an identified,
  intentional history rewrite. State the branch and rewritten commits first.
- Leave unrelated untracked or modified files untouched.

## Commit messages

- Use Conventional Commit form: `type(scope): imperative summary`.
- Keep the subject at 50 characters or fewer, use a blank line before the body,
  and wrap the body at 72 characters.
- Use lowercase canonical types: `feat`, `fix`, `refactor`, `docs`, `test`,
  `build`, `ci`, and `chore`.

## Repository text

- Use a 100-character line-length guideline for Markdown and normal text.
- Run Rumdl for changed Markdown before a commit when it is available.

## Emacs configuration

- Prefer a minimal, built-in-first setup with a restrained, semantic interface.
- Use [Protesilaos Stavrou's Emacs configuration][prot-dotemacs] as a style and
  inspiration reference; adapt its ideas deliberately rather than copying it wholesale.

[prot-dotemacs]: https://protesilaos.com/emacs/dotemacs
