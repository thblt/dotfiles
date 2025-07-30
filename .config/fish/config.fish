if status is-interactive
    alias ..2="cd ../.."
    alias ..3="cd ../../.."
    alias ..4="cd ../../../.."
    alias ..5="cd ../../../../.."
    alias ..6="cd ../../../../../.."
    alias G=" \| grep "
end

set __fish_git_prompt_showdirtystate true

set __fish_git_prompt_showdirtystate yes
set __fish_git_prompt_showstashstate yes
set __fish_git_prompt_showuntrackedfiles yes
set __fish_git_prompt_showupstream yes
set __fish_git_prompt_color_branch yellow
set __fish_git_prompt_color_upstream_ahead green
set __fish_git_prompt_color_upstream_behind red

set __fish_git_prompt_char_dirtystate '⚡'
set __fish_git_prompt_char_stagedstate '→'
set __fish_git_prompt_char_untrackedfiles '☡'
set __fish_git_prompt_char_stashstate '↩'
set __fish_git_prompt_char_upstream_ahead '+'
set __fish_git_prompt_char_upstream_behind -
function fish_prompt
    string join '' -- (set_color green) (prompt_pwd) (set_color normal) ' > '
end

function fish_right_prompt
    string join '' (fish_git_prompt)
end

alias e="emacsclient -n"
alias magit="emacsclient -n -e \"(magit)\""