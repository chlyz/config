set -l config_themes \
    modus-operandi-tinted \
    modus-vivendi-tinted \
    modus-operandi \
    modus-vivendi \
    modus-operandi-deuteranopia \
    modus-vivendi-deuteranopia \
    modus-operandi-tritanopia \
    modus-vivendi-tritanopia \
    current \
    list

complete -c config -f
complete -c config -n '__fish_use_subcommand' -a link -d 'Link this repository into HOME'
complete -c config -n '__fish_use_subcommand' -a unlink -d 'Remove managed links'
complete -c config -n '__fish_use_subcommand' -a status -d 'Show managed link status'
complete -c config -n '__fish_use_subcommand' -a theme -d 'Select and apply a theme'
complete -c config -n '__fish_use_subcommand' -a update -d 'Update Rust and repository tools'
complete -c config -n '__fish_use_subcommand' -a packages -d 'Manage system packages and user tools'
complete -c config -n '__fish_use_subcommand' -a help -d 'Show command help'

complete -c config -n '__fish_seen_subcommand_from theme' -a "$config_themes"
complete -c config -n '__fish_seen_subcommand_from packages' -a 'install upgrade'
complete -c config -n '__fish_seen_subcommand_from link unlink packages' -l dry-run -d 'Print actions without running them'
