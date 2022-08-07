function fish_prompt
    set_color yellow
    echo -n (hostname)
    if [ $PWD != $HOME ]
        set_color white
        echo -n ':'
        set_color blue
        echo -n (basename $PWD)
    end
    set_color cyan
    printf '%s ' (__fish_git_prompt)
    set_color red
    echo -n '> '
    set_color normal
end
