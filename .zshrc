# Don't do anything if not running interactively
[[ $- != *i* ]] && return

ZSH_LIB_DIR=$HOME/.local/lib/zsh

for p in $ZSH_LIB_DIR/*; do
    b=$(basename $p)

    if [ -e $p/$b.zsh ]; then
        source $p/$b.zsh
    elif [ -e $p/$b.zsh-theme ]; then
        source $p/$b.zsh-theme
    fi
done;

thblt_prompt_reset() {
    echo -n '%f%k%b'
}

thblt_prompt_path() {
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
        [[ 0 < $ahead ]] && echo -n " ↑$ahead"
        [[ 0 < $behind ]] && echo -n " ↓$behind"
    fi;
    echo -n ' '
}

thblt_prompt() {
    # Jobs indicator
    [[ 0 != $(jobs | wc -l) ]] && echo -n "%F{45} ✵ " && thblt_prompt_reset

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
        echo -n "%F{39}%(?..%F{9})"
        echo -n ' ◆ ';
    else
        echo -n "%(?..%F{9})"
        echo -n ' ❱ '
    fi
}

thblt_rprompt() {

    # git
   thblt_prompt_git_status
   thblt_prompt_reset

    # nix-shell
   if [[ -n $IN_NIX_SHELL ]]; then
       echo -n " "
       echo -n "%K{39}%F{255}%B $name ";
   fi
   return
}

# Prompt
setopt PROMPT_SUBST
PROMPT='$(thblt_prompt)'
RPROMPT='$(thblt_rprompt)'

# Completion
autoload -U compinit && compinit
zstyle ':completion:*' menu select

# Implicit CD
setopt AUTOcd

# History
export HISTFILE=$HOME/.zsh_history
export SAVEHIST=20000
export HISTSIZE=20000
setopt append_history
setopt hist_ignore_all_dups
setopt hist_expire_dups_first
setopt share_history

# editing

# Prefix-based history search with up and down arrow
bindkey '^[[A' up-line-or-search
bindkey '^[[B' down-line-or-search

# By default, ZSH considers / to be part of the word.  This makes
# forward-word and backward-word stop at directory delimiters
export WORDCHARS="*?.[]~=&;\!#$%^(){}<>"

# ======

# Variables
export EDITOR="emacsclient -ca ''"

# Aliases
alias bc="bc -l"
alias bgd="bg;disown;"
alias e="${EDITOR} --no-wait" # Shorthand
alias ee="${EDITO} --create-frame --no-wait" # Shorthand
alias o=xdg-open

alias nix-zsh="nix-shell --run zsh"

texclean() {
    for f in $@; do
        rm $f.aux $f.bbl $f.bcf $f.blg $f.idx $f.ilg $f.ind $f.log $f.out $f.run.xml $f.toc;
    done;
}

qrpass() {
    pass show $1 | head -n 1 | qrencode -s 3 -o - | feh -Z -
}

export GREP_COLOR=31
alias grep='grep --color=auto'

# Git aliases
alias ga="git add"
alias gc="git commit"
alias gd="git diff"
alias gp="git push"
alias grm="git rm"

#Pipe aliass
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

# Misc aliases and functions
alias apt-what-have-i-installed="comm -23 <(comm -23 <(apt-mark showmanual | sort -u) <(cat /usr/local/share/base-packages.list | sort -u)) <($HOME/.dotfiles/postinstall/debian-bootstrap list | sort -u)"
alias bc="bc -l"

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

# Use system clipboard
x-copy-region-as-kill () {
    zle copy-region-as-kill
    print -rn $CUTBUFFER | xsel -i -b
}
zle -N x-copy-region-as-kill

x-kill-region () {
    zle kill-region
    print -rn $CUTBUFFER | xsel -i -b
}
zle -N x-kill-region

x-kill-line () {
    zle kill-line
    print -rn $CUTBUFFER | xsel -i -b
}
zle -N x-kill-line

x-yank () {
    CUTBUFFER=$(xsel -o -b </dev/null)
    zle yank
}
zle -N x-yank

which xsel 2&>/dev/null > /dev/null
if [[ 0 == $? ]]; then
    bindkey -e '\ew' x-copy-region-as-kill
    bindkey -e '^W' x-kill-region
    bindkey -e '^K' x-kill-line
    bindkey -e '^Y' x-yank
fi

# Source profile
source ~/.profile
