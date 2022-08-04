#
# Global
#

# Create a hash table for globally stashing variables without polluting main
# scope with a bunch of identifiers.
typeset -A __WINCENT

__WINCENT[ITALIC_ON]=$'\e[3m'
__WINCENT[ITALIC_OFF]=$'\e[23m'

# --- Completion --- {{{

fpath=($HOME/.zsh/completions $fpath)

autoload -U compinit
compinit -u
# Make completion:
# - Try exact (case-sensitive) match first.
# - Then fall back to case-insensitive.
# - Accept abbreviations after . or _ or - (ie. f.b -> foo.bar).
# - Substring complete (ie. bar -> foobar).
zstyle ':completion:*' matcher-list '' '+m:{[:lower:]}={[:upper:]}' '+m:{[:upper:]}={[:lower:]}' '+m:{_-}={-_}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'

# Colorize completions using default `ls` colors.
zstyle ':completion:*' list-colors ''

# Allow completion of ..<Tab> to ../ and beyond.
zstyle -e ':completion:*' special-dirs '[[ $PREFIX = (../)#(..) ]] && reply=(..)'

# $CDPATH is overpowered (can allow us to jump to 100s of directories) so tends
# to dominate completion; exclude path-directories from the tag-order so that
# they will only be used as a fallback if no completions are found.
zstyle ':completion:*:complete:(cd|pushd):*' tag-order 'local-directories named-directories'

# Categorize completion suggestions with headings:
zstyle ':completion:*' group-name ''
zstyle ':completion:*:descriptions' format %F{default}%B%{$__WINCENT[ITALIC_ON]%}--- %d ---%{$__WINCENT[ITALIC_OFF]%}%b%f

# Enable keyboard navigation of completions in menu
# (not just tab/shift-tab but cursor keys as well):
zstyle ':completion:*' menu select

# }}}

# --- Prompt --- {{{

autoload -U colors
colors

RPROMPT_BASE="%F{blue}%~%f"
setopt PROMPT_SUBST

# Anonymous function to avoid leaking variables.
function () {
  # Check for tmux by looking at $TERM, because $TMUX won't be propagated to any
  # nested sudo shells but $TERM will.
  local TMUXING=$([[ "$TERM" =~ "tmux" ]] && echo tmux)
  if [ -n "$TMUXING" -a -n "$TMUX" ]; then
    # In a a tmux session created in a non-root or root shell.
    local LVL=$(($SHLVL - 1))
  elif [ -n "$XAUTHORITY" ]; then
    # Probably in X on Linux.
    local LVL=$(($SHLVL - 2))
  else
    # Either in a root shell created inside a non-root tmux session,
    # or not in a tmux session.
    local LVL=$SHLVL
  fi
  local SUFFIX='%(!.%F{yellow}%n%f.)%(!.%F{yellow}.%F{red})'$(printf '\u276f%.0s' {1..$LVL})'%f'

  export PS1="%F{green}${SSH_TTY:+%n@%m}%f%B${SSH_TTY:+:}%b%F{blue}%B%1~%b%F{yellow}%B%(1j.*.)%(?..!)%b%f %B${SUFFIX}%b "
  if [[ -n "$TMUXING" ]]; then
    # Outside tmux, ZLE_RPROMPT_INDENT ends up eating the space after PS1, and
    # prompt still gets corrupted even if we add an extra space to compensate.
    export ZLE_RPROMPT_INDENT=0
  fi
}

export RPROMPT=$RPROMPT_BASE
export SPROMPT="zsh: correct %F{red}'%R'%f to %F{red}'%r'%f [%B%Uy%u%bes, %B%Un%u%bo, %B%Ue%u%bdit, %B%Ua%u%bbort]? "

# }}}

# --- History --- {{{

export HISTSIZE=100000
export HISTFILE="$HOME/.local/share/.zsh_history"
export SAVEHIST=$HISTSIZE

# }}}

# --- Options --- {{{

# Description of all available options:
#
#     https://zsh.sourceforge.io/Doc/Release/Options.html

setopt AUTO_CD
setopt AUTO_PARAM_SLASH
setopt AUTO_PUSHD
setopt AUTO_RESUME
setopt CLOBBER
setopt NO_FLOW_CONTROL
setopt HIST_FIND_NO_DUPS
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_VERIFY
setopt INTERACTIVE_COMMENTS
setopt LIST_PACKED
setopt NO_NOMATCH
setopt PRINT_EXIT_VALUE
setopt PUSHD_IGNORE_DUPS
setopt PUSHD_SILENT
setopt SHARE_HISTORY

# }}}

# --- Plugins --- {{{

# NOTE: must come before zsh-history-substring-search & zsh-syntax-highlighting.
autoload -U select-word-style
select-word-style bash # only alphanumeric chars are considered WORDCHARS

# source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=59'
ZSH_AUTOSUGGEST_STRATEGY=(match_prev_cmd completion)

# NOTE: must come after select-word-style.
# source ~/.zsh/zsh-history-substring-search/zsh-history-substring-search.zsh

# Note that this will only ensure unique history if we supply a prefix
# before hitting "up" (ie. we perform a "search"). HIST_FIND_NO_DUPS
# won't prevent dupes from appearing when just hitting "up" without a
# prefix (ie. that's "zle up-line-or-history" and not classified as a
# "search"). So, we have HIST_IGNORE_DUPS to make life bearable for that
# case.
#
# https://superuser.com/a/1494647/322531
HISTORY_SUBSTRING_SEARCH_ENSURE_UNIQUE=1

# Uncomment this to get syntax highlighting:
# NOTE: must come after select-word-style.
# source ~/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# }}}

# --- Bindings --- {{{

# Use vi mode.
bindkey -v
export KEYTIMEOUT=1

# Change cursor shape depending on the mode:
#
#     https://github.com/LukeSmithxyz/voidrice/blob/master/.config/zsh/.zshrc
#

function zle-keymap-select () {
    case $KEYMAP in
        vicmd) echo -ne '\e[1 q';;      # block
        viins|main) echo -ne '\e[5 q';; # beam
    esac
}
zle -N zle-keymap-select

zle-line-init() {
    zle -K viins # initiate `vi insert` as keymap (can be removed if `bindkey -V` has been set elsewhere)
    echo -ne "\e[5 q"
}
zle -N zle-line-init

# Use beam shape cursor on startup.
echo -ne '\e[5 q'

# Use beam shape cursor for each new prompt.
preexec() { echo -ne '\e[5 q' ;}


# Use "cbt" capability ("back_tab", as per `man terminfo`), if we have it:
if tput cbt &> /dev/null; then
    # Make Shift-tab go to previous completion
    bindkey "$(tput cbt)" reverse-menu-complete
fi

bindkey '^P' history-substring-search-up
bindkey '^N' history-substring-search-down
bindkey -M vicmd 'k' history-substring-search-up
bindkey -M vicmd 'j' history-substring-search-down
bindkey -M viins '^a' beginning-of-line
bindkey -M viins '^e' end-of-line
bindkey -M viins '^f' forward-char
bindkey -M viins '^b' backward-char
bindkey -M viins '^[f' forward-word
bindkey -M viins '^[b' backward-word
bindkey -M viins '^d' delete-char
bindkey -M viins '^[d' delete-word
bindkey -M viins '^H' backward-kill-word

bindkey -M viins '^s' autosuggest-execute

autoload -U edit-command-line
zle -N edit-command-line
bindkey -M viins '^x^x' edit-command-line

bindkey ' ' magic-space # do history expansion on space

# Make CTRL-Z background things and unbackground them.
function fg-bg() {
  if [[ $#BUFFER -eq 0 ]]; then
    fg
  else
    zle push-input
  fi
}
zle -N fg-bg
bindkey '^Z' fg-bg

# }}}

# --- Colors --- {{{

color_file="$XDG_DATA_HOME/.background"

function fzf_default() {
    read -r background < "$color_file"
    if [ $background = dark ]; then
        # set_dark_theme
        local fg_main='#ffffff'
        local bg_main='#000000'
        local bg_alt='#191a1b'
        local red='#ff8059'
        local green='#44bc44'
        local magenta='#feacd0'

        export FZF_DEFAULT_OPTS="--color=bg+:$bg_alt,bg:$bg_main,spinner:$red,hl:$blue --color=fg:$fg_main,header:$green,info:$cyan,pointer:$magenta --color=marker:$green,fg+:$fg_main,prompt:$cyan,hl+:$red --layout=reverse --exact"
    else
        # set_light_theme
        local fg_main='#000000'
        local bg_main='#ffffff'
        local bg_alt='#f2eff3'
        local red='#a60000'
        local green='#005e00'
        local magenta='#721045'
        local cyan='#00538b'
        local blue='#0031a9'
        export FZF_DEFAULT_OPTS="--color=bg+:$bg_alt,bg:$bg_main,spinner:$red,hl:$blue --color=fg:$fg_main,header:$green,info:$cyan,pointer:$magenta --color=marker:$green,fg+:$fg_main,prompt:$cyan,hl+:$red --layout=reverse --exact"
    fi
}

function set_dark_theme() {
    echo "dark" > "$color_file"
    cp "$HOME/.config/kitty/modus-vivendi-theme.conf" \
        "$HOME/.config/kitty/current-theme.conf"
    cp "$HOME/.config/task/modus-vivendi.theme" \
        "$HOME/.config/task/current.theme"
    fzf_default
}

function set_light_theme() {
    echo "light" > "$color_file"
    cp "$HOME/.config/kitty/modus-operandi-theme.conf" \
        "$HOME/.config/kitty/current-theme.conf"
    cp "$HOME/.config/task/modus-operandi.theme" \
        "$HOME/.config/task/current.theme"
    fzf_default
}

function toggle_theme() {
    if [ ! -f $color_file ]; then
        set_light_theme
    else
        read -r background < "$color_file"
        if [ $background = light ]; then
            set_dark_theme
        else
            set_light_theme
        fi
        source "$HOME/.bashrc"
    fi
}

if [ ! -f $color_file ]; then
    set_light_theme
else
    read -r background < "$color_file"
    if [ $background = light ]; then
        set_light_theme
    else
        set_dark_theme
    fi
fi

# }}}

#
# Other prerequisites before we set up `$PATH`.
#

# TODO: Check what the following does.
# test -d $HOME/n && export N_PREFIX="$HOME/n"

#
# Other
#

# source $HOME/.zsh/path # Must come first! (Others depend on it.)

source $XDG_CONFIG_HOME/zsh/aliases
# source $HOME/.zsh/common
# source $HOME/.zsh/color
# source $HOME/.zsh/exports
# source $HOME/.zsh/functions
# source $HOME/.zsh/hash
# source $HOME/.zsh/vars

#
# Hooks
#

autoload -U add-zsh-hook

function -set-tab-and-window-title() {
  emulate -L zsh
  local CMD="${1:gs/$/\\$}"
  print -Pn "\e]0;$CMD:q\a"
}

# $HISTCMD (the current history event number) is shared across all shells
# (due to SHARE_HISTORY). Maintain this local variable to count the number of
# commands run in this specific shell.
HISTCMD_LOCAL=0

# Executed before displaying prompt.
function -update-window-title-precmd() {
  emulate -L zsh
  if [[ HISTCMD_LOCAL -eq 0 ]]; then
    # About to display prompt for the first time; nothing interesting to show in
    # the history. Show $PWD.
    -set-tab-and-window-title "$(basename $PWD)"
  else
    local LAST=$(history | tail -1 | awk '{print $2}')
    if [ -n "$TMUX" ]; then
      # Inside tmux, just show the last command: tmux will prefix it with the
      # session name (for context).
      -set-tab-and-window-title "$LAST"
    else
      # Outside tmux, show $PWD (for context) followed by the last command.
      -set-tab-and-window-title "$(basename $PWD) > $LAST"
    fi
  fi
}
add-zsh-hook precmd -update-window-title-precmd

# Executed before executing a command: $2 is one-line (truncated) version of
# the command.
function -update-window-title-preexec() {
  emulate -L zsh
  setopt EXTENDED_GLOB
  HISTCMD_LOCAL=$((++HISTCMD_LOCAL))

  # Skip ENV=settings, sudo, ssh; show first distinctive word of command;
  # mostly stolen from:
  #   https://github.com/robbyrussell/oh-my-zsh/blob/master/lib/termsupport.zsh
  local TRIMMED="${2[(wr)^(*=*|mosh|ssh|sudo)]}"
  if [ -n "$TMUX" ]; then
    # Inside tmux, show the running command: tmux will prefix it with the
    # session name (for context).
    -set-tab-and-window-title "$TRIMMED"
  else
    # Outside tmux, show $PWD (for context) followed by the running command.
    -set-tab-and-window-title "$(basename $PWD) > $TRIMMED"
  fi
}
add-zsh-hook preexec -update-window-title-preexec

typeset -F SECONDS
function -record-start-time() {
  emulate -L zsh
  ZSH_START_TIME=${ZSH_START_TIME:-$SECONDS}
}
add-zsh-hook preexec -record-start-time

function -report-start-time() {
  emulate -L zsh
  if [ $ZSH_START_TIME ]; then
    local DELTA=$(($SECONDS - $ZSH_START_TIME))
    local DAYS=$((~~($DELTA / 86400)))
    local HOURS=$((~~(($DELTA - $DAYS * 86400) / 3600)))
    local MINUTES=$((~~(($DELTA - $DAYS * 86400 - $HOURS * 3600) / 60)))
    local SECS=$(($DELTA - $DAYS * 86400 - $HOURS * 3600 - $MINUTES * 60))
    local ELAPSED=''
    test "$DAYS" != '0' && ELAPSED="${DAYS}d"
    test "$HOURS" != '0' && ELAPSED="${ELAPSED}${HOURS}h"
    test "$MINUTES" != '0' && ELAPSED="${ELAPSED}${MINUTES}m"
    if [ "$ELAPSED" = '' ]; then
      SECS="$(print -f "%.2f" $SECS)s"
    elif [ "$DAYS" != '0' ]; then
      SECS=''
    else
      SECS="$((~~$SECS))s"
    fi
    ELAPSED="${ELAPSED}${SECS}"
    export RPROMPT="%F{cyan}%{$__WINCENT[ITALIC_ON]%}${ELAPSED}%{$__WINCENT[ITALIC_OFF]%}%f $RPROMPT_BASE"
    unset ZSH_START_TIME
  else
    export RPROMPT="$RPROMPT_BASE"
  fi
}
add-zsh-hook precmd -report-start-time

# add-zsh-hook precmd bounce

function -auto-ls-after-cd() {
  emulate -L zsh
  # Only in response to a user-initiated `cd`, not indirectly (eg. via another
  # function).
  if [ "$ZSH_EVAL_CONTEXT" = "toplevel:shfunc" ]; then
    ls -a
    # if [ -f .nirarc ]; then
    #     echo 'hej'
    #     source .nirarc
    # fi
  fi
}
add-zsh-hook chpwd -auto-ls-after-cd

# Remember each command we run.
function -record-command() {
  __WINCENT[LAST_COMMAND]="$2"
}
add-zsh-hook preexec -record-command

# Update vcs_info (slow) after any command that probably changed it.
# function -maybe-show-vcs-info() {
#   local LAST="$__WINCENT[LAST_COMMAND]"

#   # In case user just hit enter, overwrite LAST_COMMAND, because preexec
#   # will not run and it will otherwise linger.
#   __WINCENT[LAST_COMMAND]="<unset>"

#   # Check first word; via:
#   # http://tim.vanwerkhoven.org/post/2012/10/28/ZSH/Bash-string-manipulation
#   case "$LAST[(w)1]" in
#     cd|cp|git|rm|touch|mv)
#       vcs_info
#       ;;
#     *)
#       ;;
#   esac
# }
# add-zsh-hook precmd -maybe-show-vcs-info

# adds `cdr` command for navigating to recent directories
autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
add-zsh-hook chpwd chpwd_recent_dirs

# enable menu-style completion for cdr
zstyle ':completion:*:*:cdr:*:*' menu selection

# fall through to cd if cdr is passed a non-recent dir as an argument
zstyle ':chpwd:*' recent-dirs-default true

# Local and host-specific overrides.

LOCAL_RC=$HOME/.zshrc.local
test -f $LOCAL_RC && source $LOCAL_RC

# If your hostname ever gets unset on macOS, reset with:
#   sudo scutil --set HostName $desired-host-name
HOST_RC=$HOME/.zsh/host/$(hostname -s | tr '[:upper:]' '[:lower:]')
test -f $HOST_RC && source $HOST_RC

#
# /etc/motd
#

if [ -e /etc/motd ]; then
  if ! cmp -s $HOME/.hushlogin /etc/motd; then
    tee $HOME/.hushlogin < /etc/motd
  fi
fi

# Use lf to switch directories and bind it to ctrl-o
lfcd () {
    tmp="$(mktemp -uq)"
    trap 'rm -f $tmp >/dev/null 2>&1' HUP INT QUIT TERM PWR EXIT
    lf -last-dir-path="$tmp" "$@"
    if [ -f "$tmp" ]; then
        dir="$(cat "$tmp")"
        [ -d "$dir" ] && [ "$dir" != "$(pwd)" ] && cd "$dir"
    fi
}
bindkey -s '^o' '^ulfcd\n'

. "$HOME/.cargo/env"
eval "$(zoxide init bash)"

# NOTE: must come before zsh-history-substring-search & zsh-syntax-highlighting.
autoload -U select-word-style
select-word-style bash # only alphanumeric chars are considered WORDCHARS

source ~/.local/git/zsh-autosuggestions/zsh-autosuggestions.zsh
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=59'
ZSH_AUTOSUGGEST_STRATEGY=(match_prev_cmd completion)

# NOTE: must come after select-word-style.
source /home/chlyz/.local/git/zsh-history-substring-search/zsh-history-substring-search.zsh

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
