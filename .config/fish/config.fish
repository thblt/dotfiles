if status is-interactive
    alias ..2="cd ../.."
    alias ..3="cd ../../.."
    alias ..4="cd ../../../.."
    alias ..5="cd ../../../../.."
    alias ..6="cd ../../../../../.."
    alias G=" \| grep "
end
function fish_prompt
    string join '' -- (set_color green) (prompt_pwd) (set_color normal) '>'
end # interactive user name @ host name, date/time in YYYY-mm-dd format and path
function fish_right_prompt
    string join '' (fish_git_prompt)
end # interactive user name @ host name, date/time in YYYY-mm-dd format and path
