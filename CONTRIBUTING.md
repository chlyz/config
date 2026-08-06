# Contributing

## Commit messages

This repository follows [Conventional Commits][conventional-commits].
It also uses Git's conventional subject and body formatting.

```text
<type>(<scope>): <imperative summary>

Explain what changed and why. Wrap body lines at 72 characters.

Footer: optional metadata
```

- Keep the subject to 50 characters or fewer, including the type and
  scope.
- Use the imperative mood and omit the trailing period.
- Leave a blank line between the subject and body.
- Wrap body lines at 72 characters.
- The 72-character rule applies only to commit messages. Normal text files,
  including Markdown, use a 100-character guideline.
- Use a lowercase module noun as the scope, such as `config` or
  `bootstrap`.
- Use standard lowercase types: `feat`, `fix`, `refactor`, `docs`, and
  `test`, `build`, `ci`, or `chore`.

Examples:

```text
feat(config): add portable bootstrap
fix(alacritty): map Ctrl+Backspace to word deletion
refactor(sway): use home-relative launcher paths
```

[conventional-commits]: https://www.conventionalcommits.org/en/v1.0.0/
