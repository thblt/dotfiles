# * Non-interactive and dumb terminals.

# Don't do anything if not running interactively
[[ $- != *i* ]] && return

# Don't do anything on dumb terms.  This prevents issues with TRAMP.
[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return

# * Variables

export EDITOR="emacsclient -ca ''"
export GREP_COLOR="mt=31"
alias grep='grep --color=auto'

# * Prompt

thblt_prompt_reset() {
    echo -n '%f%k%b'
}

thblt_prompt_git_status() {
    oid=""
    local_branch=""
    upstream_branch=""
    ahead=0
    behind=0

    modified=0 # Are there untracked files?

    git status --porcelain=v2 --branch 2&>/dev/null |
        while read line ; do
            case "$line" in
                \#\ branch.oid\ *)       export oid=$(echo $line | awk '{print $3}');
                                         ;;
                \#\ branch.head\ *)      export local_branch=$(echo $line | awk '{print $3}');
                                         ;;
                \#\ branch.upstream\ *)  export upstream_branch=$(echo $line | awk '{print $3}');
                                         ;;
                \#\ branch.ab\ *)        export ahead=$(echo $line | awk '{print $3}' | cut -b 2-);
                                         export behind=$(echo $line | awk '{print $4}' | cut -b 2-);
                                         ;;

                \?*)                     export modified=1; ;;
                1*)                      export modified=1; ;;
                2*)                      export modified=1;
            esac;
        done;

    [[ -z $oid ]] && return;

    if [[ 0 != $modified ]]; then
        echo -n "%K{11}%F{232}"
    else
        echo -n "%K{118}%F{232}"
    fi

    echo -n " %B"

    if [[ "(detached)" == "$local_branch" ]]; then
        echo -n $(git rev-parse --short $oid);
    else
        echo -n " $local_branch";
    fi
    echo -n "%b"

    if [[ 0 < $(($ahead + $behind)) ]]; then
        echo -n " "
        [[ 0 < $ahead ]] && echo -n "▲ $ahead"
        [[ 0 < $behind ]] && echo -n "▼ $behind"
    fi;
    echo -n ' '
}

thblt_prompt() {
    # Jobs indicator
    # Syntax: <Mikachu> in man zshall, search for conditional substrings in prompts
    echo -n "%(1j.%F{45} ✵ .)"
    # Opening [
     echo -n "%F{255}%B["

     # Path
     echo -n "%F{39}%B%~"
     thblt_prompt_reset

     # Closing ]
     echo -n "%F{255}%B]"
     thblt_prompt_reset

     # User/host
     if [[ -n $SSH_CONNECTION ]]; then
        # Aggressive colors
        echo -n " %F{253}%K{253}%F{232}%B ⇅ $HOST %b%F{0}";
        echo -n '%K{232}%F{253}'
        thblt_prompt_reset
    fi

    if [[ 0 == $UID ]]; then
        echo -n "%F{226}%B" # Default color
        echo -n "%(?..%F{9})" # Override if $?
        echo -n " # " # The prompt itself
    elif [[ -n $IN_NIX_SHELL ]]; then
        # nix-shell
        echo -n " "
        echo -n "%K{39}%F{255}%B $name ";
        thblt_prompt_reset
        echo -n ' ◆ ';
    else
        echo -n "%(?..%F{9} %?)"
        echo -n ' ❱ '
    fi
}

thblt_rprompt() {

    # git
    thblt_prompt_git_status
    thblt_prompt_reset
    return
}

# ** Install the prompt

setopt PROMPT_SUBST
PROMPT='$(thblt_prompt)'
RPROMPT='$(thblt_rprompt)'

# * Misc

# ** Completion
autoload -U compinit && compinit
zstyle ':completion:*' menu select

# ** Implicit CD
setopt AUTOcd

# ** History

export HISTFILE=$HOME/.zsh_history
export SAVEHIST=20000
export HISTSIZE=20000
setopt append_history
setopt hist_ignore_all_dups
setopt hist_expire_dups_first
setopt share_history

# * Aliases

# ** General

alias bc="bc -l"
alias bgd="bg;disown;"
alias beep="notify-send \"Beep beep beep\""
alias e="${EDITOR} --no-wait" # Shorthand
alias ee="${EDITO} --create-frame --no-wait"# Shorthand
alias o=xdg-open
alias nix-zsh="nix-shell --run zsh"

alias proced="emacs -nw -q -e proced"

texclean() {
    for f in $@; do
        f=$(basename -a --suffix .tex $f)
        rm -f \
           $f.aux \
           $f.bbl \
           $f.bcf \
           $f.blg \
           $f.dvi \
           $f.fdb_latexmk \
           $f.fls \
           $f.idx \
           $f.ilg \
           $f.ind \
           $f.log \
           $f.out \
           $f.run.xml \
           $f.synctex.gz \
           $f.toc \
           $f.xdv \
           texput.log;
    done;
}

# cd to the containing directory of a given binary
cdb() {
    cd $(dirname $(realpath $(which $1)))
}

# ** Git aliases

alias ga="git add"
alias gc="git commit"
alias gd="git diff"
alias gp="git push"
alias grm="git rm"

# ** Pipe aliass

alias -g C='| wl-copy'
alias -g H='| head'
alias -g T='| tail'
alias -g G='| grep'
alias -g L="| less"
alias -g M="| most"
alias -g LL="2>&1 | less"
alias -g CA="2>&1 | cat -A"
alias -g NE="2> /dev/null"
alias -g NUL="> /dev/null 2>&1"
alias -g P="2>&1| pygmentize -l pytb"

# ** Misc aliases and functions
alias apt-what-have-i-installed="comm -23 <(comm -23 <(apt-mark showmanual | sort -u) <(cat /usr/local/share/base-packages.list | sort -u)) <($HOME/.dotfiles/postinstall/debian-bootstrap list | sort -u)"
alias bc="bc -l"

# * More aliases

function bump-elisp-version() {
    sed -i "s/\;; Version: [.0-9]\+$/;; Version: $1/" $@[2,-1]
}

alias efivardump="efivar --list | xargs -I vn efivar --print --name=vn"
alias fuck='sudo $(fc -ln -1)' # 'sudo $(history -p \!\!)' is bash-only
alias ls="ls --color"

function mkcd() {
    if mkdir -p "$@"
    then cd "$@"
    fi
}

alias push-priv-dotfiles="pushd;cd ~/.dotfiles.private;git add -Av;git commit -m 'Autocommit by `whoami` on `hostname` at `date -Iseconds`';git pull;git push;popd"
alias wifi-off="sudo rfkill block wifi"
alias wifi-on="sudo rfkill unblock wifi"

# Moving between directories
alias bd=". bd -s"
alias ..="cd .."
alias ..2="cd ../.."
alias ..3="cd ../../.."
alias ..4="cd ../../../.."
alias ..5="cd ../../../../.."
alias ..6="cd ../../../../../.."

sslinfo () {
    echo | openssl s_client -showcerts -servername $1 -connect $1:443 2>/dev/null | openssl x509 -inform pem -noout -text
}

# * Line editor

# Prefix-based history search with up and down arrow
bindkey '^[[A' up-line-or-search
bindkey '^[[B' down-line-or-search

# By default, ZSH considers / to be part of the word.  This makes
# forward-word and backward-word stop at directory delimiters Notice:
# WORDCHARS define the non-alphanumeric characters which are
# considered PART of words, not breaking points.
export WORDCHARS='*?[]~=&;\!#$%^(){}<>'

# ======

# ** Use system clipboard

# @FIXME This will only work in Wayland.  I can live with this, or
# choose the clipboard program to use depending on what we're running
# on (check for $WAYLAND_DISPLAY then $DISPLAY).
x-copy-region-as-kill () {
    zle copy-region-as-kill
    print -rn - $CUTBUFFER | wl-copy
}
zle -N x-copy-region-as-kill

x-kill-region () {
    zle kill-region
    print -rn - $CUTBUFFER | wl-copy
}
zle -N x-kill-region

x-kill-line () {
    zle kill-line
    print -rn - $CUTBUFFER | wl-copy
}
zle -N x-kill-line

x-yank () {
    CUTBUFFER=$(wl-paste)
    zle yank
}
zle -N x-yank

if [[ "$DISPLAY" || "$WAYLAND_DISPLAY" ]]; then
    bindkey -e '\ew' x-copy-region-as-kill
    bindkey -e '^W' x-kill-region
    bindkey -e '^K' x-kill-line
    bindkey -e '^Y' x-yank
fi

# ** zle-line-init

# Allow to recall aborted command
# From <https://www.topbug.net/blog/2016/10/03/restore-the-previously-canceled-command-in-zsh/>
function zle-line-init {
    # Your other zle-line-init configuration ...

    # Store the last non-empty aborted line in MY_LINE_ABORTED
    if [[ -n $ZLE_LINE_ABORTED ]]; then
        MY_LINE_ABORTED="$ZLE_LINE_ABORTED"
    fi

    # Restore aborted line on the first undo.
    if [[ -n $MY_LINE_ABORTED ]]; then
        local savebuf="$BUFFER" savecur="$CURSOR"
        BUFFER="$MY_LINE_ABORTED"
        CURSOR="$#BUFFER"
        zle split-undo
        BUFFER="$savebuf" CURSOR="$savecur"
    fi
}
zle -N zle-line-init

# Case-insensitive completion *only* when there's no case sensitive match.
zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'

# ** fzf

if [ -n "${commands[fzf-share]}" ]; then
    source "$(fzf-share)/key-bindings.zsh"
    source "$(fzf-share)/completion.zsh"
fi

# * Make run-help work with subcommands

unalias run-help
autoload -Uz run-help-git
autoload -Uz run-help


# Source profile
source ~/.profile
