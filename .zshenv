export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8

export EDITOR="/usr/local/bin/nvim"
export TERMINAL="$HOME/.local/bin/kitty"

# export GIT_CEILING_DIRECTORIES=$HOME

# Colors for directory listings
export LS_COLORS="rs=0:di=00;94:ln=04;36:mh=00:pi=00;35:so=00;35:do=01;35:bd=00;35:cd=00;35:or=00;36:mi=101;30:su=37;41:sg=30;43:ca=30;41:tw=00;94:ow=00;94:st=37;44:ex=00;95:*.tar=01;31:*.tgz=00;96:*.arc=01;31:*.arj=01;31:*.taz=01;31:*.lha=01;31:*.lz4=01;31:*.lzh=01;31:*.lzma=01;31:*.tlz=01;31:*.txz=01;31:*.tzo=01;31:*.t7z=00;96:*.zip=00;96:*.z=01;31:*.dz=01;31:*.gz=00;96:*.lrz=01;31:*.lz=01;31:*.lzo=01;31:*.xz=01;31:*.zst=01;31:*.tzst=01;31:*.bz2=01;31:*.bz=01;31:*.tbz=01;31:*.tbz2=01;31:*.tz=01;31:*.deb=00;96:*.rpm=01;31:*.jar=01;31:*.war=01;31:*.ear=01;31:*.sar=01;31:*.rar=01;31"

# Same as LS_COLORS except no underline for symbolic links
export LF_COLORS="rs=0:di=00;94:ln=00;36:mh=00:pi=00;35:so=00;35:do=01;35:bd=00;35:cd=00;35:or=00;36:mi=101;30:su=37;41:sg=30;43:ca=30;41:tw=00;94:ow=00;94:st=37;44:ex=00;95:*.tar=01;31:*.tgz=00;96:*.arc=01;31:*.arj=01;31:*.taz=01;31:*.lha=01;31:*.lz4=01;31:*.lzh=01;31:*.lzma=01;31:*.tlz=01;31:*.txz=01;31:*.tzo=01;31:*.t7z=00;96:*.zip=00;96:*.z=01;31:*.dz=01;31:*.gz=00;96:*.lrz=01;31:*.lz=01;31:*.lzo=01;31:*.xz=01;31:*.zst=01;31:*.tzst=01;31:*.bz2=01;31:*.bz=01;31:*.tbz=01;31:*.tbz2=01;31:*.tz=01;31:*.deb=00;96:*.rpm=01;31:*.jar=01;31:*.war=01;31:*.ear=01;31:*.sar=01;31:*.rar=01;31"

# Improve the `less` pager.
export LESSPROMPT='?f%f .?ltLine %lt:?pt%pt\%:?btByte %bt:-...'
export LESS="-iFMRX -#.25"
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;38;5;208m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[04;38;5;111m'

# Specify default directories
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"

export _JAVA_AWT_WM_NONREPARENTING=1
export JAVA_HOME="/usr/lib/jvm/java-11-openjdk-amd64"
export PATH=~/go/bin:~/.local/bin:$PATH:/usr/local/go/bin


export SCONSFLAGS="DEVENV=gcc COMPILER=gcc HOST=true VERBOSE=false -j8"
export NIRA_NIRATOOLS="$HOME/git/tools/NIRATools"
export NIRA_JENKINSTOOLS="$HOME/git/tools/JenkinsTools"
export BOSCH_COMPILER_X52=" "
export CMOCK_DIR="$NIRA_NIRATOOLS/cmock/cmock_afa2949_patched"
export COAN="/usr/bin"
export CPPUTEST_HOME="$NIRA_NIRATOOLS/cpputest/3.7.1"
export GCC_V850_COMPILER="/opt/cross/"
export GCC_X86_COMPILER=" "
export M2RSM="$NIRA_NIRATOOLS/MSquared/M2RSM"
export MATLAB_INCLUDES_32BIT="$NIRA_NIRATOOLS/Matlab/includes/R2007b"
export MATLAB_INCLUDES_64BIT="$NIRA_NIRATOOLS/Matlab/includes/R2020a"
export MW_MINGW64_LOC=" "
export PCLint="$NIRA_NIRATOOLS/pclint/9.00L"
export PUGIXML="$NIRA_NIRATOOLS/Compilers/Libraries/pugixml/1.7"
export UNITY_DIR="$NIRA_NIRATOOLS/cmock/cmock_afa2949_patched/vendor/unity"
export NIRARC_SOURCED=1
export BOOST_LIB_PATH="/home/chlyz/git/tools/NIRATools/Boost/boost_1_62_0"

