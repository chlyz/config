if status is-interactive
    # Commands to run in interactive sessions can go here
end

#
# Modus Vivendi (Maybe? or just swap the meaning of white and black?)
#

# Available colors can be found via
#   * `set_color -c`
# and other options via
#   * `set_color -h`.
# see also https://fishshell.com/docs/current/interactive.html#color

set fish_color_normal         normal
set fish_color_command        blue
set fish_color_keyword        normal
set fish_color_quote          brblue
set fish_color_redirection    normal
set fish_color_end            normal
set fish_color_error          red
set fish_color_param          normal
set fish_color_valid_path     normal
set fish_color_option         normal
set fish_color_comment        brwhite
set fish_color_selection      normal
set fish_color_operator       brmagenta
set fish_color_escape         normal
set fish_color_autosuggestion white
set fish_color_cwd            normal
set fish_color_cwd_root       normal
set fish_color_user           normal
set fish_color_host           normal
set fish_color_host_remote    normal
set fish_color_status         normal
set fish_color_cancel         normal --reverse
set fish_color_search_match   normal

# No message when opening a new shell.
set fish_greeting

fish_ssh_agent

zoxide init fish | source
