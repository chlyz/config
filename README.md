# Dotfiles

This directory contains link packages. The `files` package owns personal
configuration files, while `scripts` owns executables installed in
`~/.local/bin`.

Run the repository-local bootstrap script once after cloning:

```sh
./bootstrap
```

It installs the required tools, links the configuration, and makes `config`
available through `~/.local/bin`. After that, run `config` from any directory:

```sh
config link
```

It targets `$HOME` by default. Inspect the current state or preview changes
without writing links with:

```sh
config status
config link --dry-run
```

To remove only links that resolve back to this repository, run:

```sh
config unlink
```

## Project workspaces

`project [DIRECTORY]` creates or attaches to a tmux session for a project (the
current directory by default). On first use, it opens an `editor` window running
Neovim and a `codex` window running Codex with `gpt-5.6-terra` in the same
directory. A later invocation attaches to the existing workspace instead of
starting new editor or Codex processes:

```sh
project ~/source/ferrichess
project
```

In Bash, `p` is an alias for `project`.

## Taskwarrior

Taskwarrior stores tasks and hooks under `~/.task` and follows the active Modus
theme. Weeks start on Monday; confirmations and bulk-change safeguards retain
their upstream defaults. Install it with the rest of the declared system
packages:

```sh
config packages install
```

The session name is based on the directory name. The command refuses to reuse a
same-named session that belongs to a different directory.

## Zsh

The Zsh setup is intentionally small and built-in-first: native completion and
shared history, a restrained directory-and-Git prompt, plus FZF, zoxide, and
`lf` when installed. Zsh Vi Mode owns modal command-line editing, cursor shape,
and Wayland clipboard integration. It is inspired by Wincent's Bash-like Zsh
configuration and by Protesilaos' preference for transparent, deliberate
configuration.

Try it without changing your login shell:

```sh
zsh
```

Once you are comfortable with it, make it your login shell with `chsh -s
"$(command -v zsh)"`.

Path components beginning with `dot-` are mapped to dotfiles: for example,
`files/dot-bashrc` becomes `~/.bashrc` and `files/dot-config/emacs/init.el`
becomes `~/.config/emacs/init.el`. The command refuses to overwrite files or
links it does not own.

## Rust tools and Alacritty

`./bootstrap` is idempotent: it reuses an existing Cargo or Rustup installation,
ensures the stable toolchain, installs the pinned versions of Rumdl and
Alacritty, and links the repository. `config update` runs the same workflow.

Alacritty is installed under Cargo's user bin directory, with matching terminfo,
desktop entry, icon, and Bash completion files installed under the home
directory. The system manifests list the platform build dependencies: use
[`systems/fedora.toml`](systems/fedora.toml) on Fedora or
[`systems/ubuntu.toml`](systems/ubuntu.toml) on Ubuntu 22.04. Existing distro
packages are left untouched and should be removed manually once the Cargo
version has been verified.

## System packages

Platform manifests live in `systems/`. They list the system packages and
user-level `uv` tools intentionally installed for this setup, grouped by
purpose. They do not export Fedora's base image or dependency closure. The
current machine is captured in
[`systems/fedora.toml`](systems/fedora.toml) for Fedora 44.

Install every package declared for the current Fedora or Ubuntu system with:

```sh
config packages install
```

The command detects the distribution from `/etc/os-release`, selects the
matching manifest, runs the system package manager through `sudo`, then installs
the declared `uv` tools. Bootstrap installs `uv` when it is missing. Preview the
commands without making changes with:

```sh
config packages install --dry-run
```

To update all system packages, including those outside this configuration, and
all declared `uv` tools, run:

```sh
config packages upgrade
```

## Network menu

`Super+N` opens a Rofi menu for connecting to, disconnecting from, or toggling
Wi-Fi through NetworkManager. It requires the `NetworkManager` and `rofi`
packages, and the NetworkManager service must be running. The `nm-applet`
indicator additionally needs a PolicyKit agent; on Fedora use
`lxqt-policykit`.

## Waybar

Waybar replaces Swaybar and hosts an interactive tray for `nm-applet`. Its
network display opens the Rofi network menu on left click and
`nm-connection-editor` on right click. Install the Fedora packages with:

```sh
sudo dnf install waybar network-manager-applet lxqt-policykit pavucontrol
```
