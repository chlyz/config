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

## Themes

`config theme` opens a Rofi selector and applies the chosen Modus variant to
Emacs, Neovim, Alacritty, Kitty, Foot, tmux, Taskwarrior, Sway/i3, Hyprland, Waybar,
Rofi, and Codex. A theme can also be selected directly:

```sh
config theme modus-vivendi-tinted
config theme current
config theme list
```

The `config` command installs native completion definitions for Bash, Zsh, and
Fish, including subcommands, options, and every supported Modus theme name.

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
[`systems/fedora.toml`](systems/fedora.toml) for Fedora 45.

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

### Hyprland packages

Hyprland is installed from the community-maintained
[`lionheartp/Hyprland` COPR][hyprland-copr]. Its stable packages are built on
Fedora infrastructure. The repository definition limits it to Hyprland and
its supporting libraries, so it cannot supply unrelated packages such as
Kitty, Waybar, or wlroots.

Install the restricted repository definition once:

```sh
sudo install -Dm644 systems/hyprland.repo /etc/yum.repos.d/hyprland.repo
```

Then run `config packages install`. To install only the Hyprland stack without
optional desktop applications, use:

```sh
sudo dnf --setopt=install_weak_deps=False install \
  hyprland hyprland-guiutils hypridle hyprlock xdg-desktop-portal-hyprland
```

Use the stable `hyprland` package. The repository definition excludes
`hyprland-git` and the Noctalia meta package. Updates use DNF; this repository
no longer builds Hyprland from source.

Installation leaves the current session running. Link the dotfiles with
`config link`, then select Hyprland at login.

[hyprland-copr]: https://copr.fedorainfracloud.org/coprs/lionheartp/Hyprland/

### dwm-style Hyprland

[`hyprland.lua`](files/dot-config/hypr/hyprland.lua) uses a left master area
at 55% width, a right stack, and new windows as master. Native monocle applies
to the focused workspace. Windows have one-pixel borders, compact gaps, square
corners, and no animation, blur, or shadows. A lone tiled window has no gaps
or border. The bottom bar uses the Sway bar's layout and shared stylesheet.

The configuration retains the Sway setup's scale of 2, custom keyboard
layouts, modifier remapping, and these shortcuts:

| Shortcut | Action |
| --- | --- |
| `Alt+,` / `Alt+.` | Focus previous / next window in layout order |
| `Alt+E` | Promote to master; on master, swap with the first stack window |
| `Alt+U` | Toggle master/stack and monocle on this workspace |
| `Alt+H` / `Alt+L` | Shrink / grow the master area |
| `Alt+I` / `Alt+D` | Add / remove a master window |
| `Alt+Shift+Space` | Toggle the focused window's floating state |
| `Alt+1` through `Alt+9` | View a numbered workspace |
| `Alt+Shift+1` through `Alt+Shift+9` | Send window to that workspace |
| `Alt+O` | Return to the previous workspace |
| `Alt+P` | Open Rofi |
| `Alt+B` | Toggle the bar |
| `Alt` + left / right mouse drag | Move / resize a window |
| `Super+Return` | Open Foot |
| `Super+C` | Close window |
| `Super+1` through `Super+9`, `Super+0` | View workspaces 1 through 10 |
| `Super+Shift+1` through `Super+Shift+9`, `Super+Shift+0` | Send window there |
| `Super+T` | Toggle the persistent tmux dropdown |
| `Super+H` | Focus or open Emacs |
| `Super+Space` | Switch keyboard layout |
| `Super+F` | Toggle fullscreen |
| `Super+S` | Copy a screenshot region |
| `Print` | Save a full screenshot with Grim |
| `Super+N` | Open the network menu |
| `Super+Shift+L` | Lock |
| `Super+Shift+G` | Toggle game mode, disabling idle lock, display-off, and suspend |
| `Super+Shift+R` | Reload configuration |
| `Super+Shift+E` | Ask for confirmation before exiting Hyprland |

Hypridle locks after 15 minutes, turns displays off after 20 minutes, and
suspends the computer after 30 minutes idle.
Visible Steam games inhibit idle handling even when unfocused. The game-mode
bar indicator shows automatic inhibition or the manual override; clicking it
toggles the manual override. Manual locking remains available in game mode.
Media playback and brightness keys follow the Sway bindings. The bar includes
battery status and network details; right-click the network module to open
NetworkManager's connection editor.
Hyprland uses its own bar configuration; Sway keeps its existing bar.
The enabled Waybar user service selects the configuration for each session.
`config theme` updates Hyprland's border colors alongside the other apps.

The display uses 10-bit output with automatic HDR switching disabled to avoid
TV mode changes during fullscreen video and workspace switching. Both Hyprland
and Sway use Foot for `Super+Return` and the persistent `Super+T` dropdown. The
dropdown keeps its centered, golden-ratio geometry. Emacs frames open on demand
rather than at login in either session.

Hyprlock shows the time, date, keyboard layout, and authentication feedback.
Its password outline turns amber for Caps Lock, and the pointer stays hidden.

Workspaces provide numbered destinations resembling dwm tags. They do not
provide dwm's multi-tag views or let one window belong to several workspaces.

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
