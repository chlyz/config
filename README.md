# Dotfiles

This directory contains Stow packages. The `files` package owns personal
configuration files, while `scripts` owns executables installed in
`~/.local/bin`.

From this directory, install or refresh the links with:

```sh
stow --dotfiles files scripts
```

The repository lives below the home directory, so Stow's default target is
`$HOME`. To remove the links later, run:

```sh
stow --dotfiles --delete files scripts
```

Files whose names begin with `dot-` are mapped to dotfiles by `--dotfiles`:
for example, `files/dot-bashrc` becomes `~/.bashrc`.

## System packages

Platform manifests live in `systems/`. They list the packages intentionally
installed for this setup, grouped by purpose. They do not export Fedora's base
image or dependency closure. The current machine is captured in
[`systems/fedora.toml`](systems/fedora.toml) for Fedora 44.

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
