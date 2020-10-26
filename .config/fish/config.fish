set fish_greeting

export PATH=$PATH:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/opt/local/bin:$HOME/.config/composer/vendor/bin
export _JAVA_OPTIONS="-Dswing.aatext=true -Dawt.useSystemAAFontSettings=on"
export EDITOR="vim"

export LANG=ru_RU.UTF-8
export LC_CTYPE=ru_RU.UTF-8
export LC_NUMERIC="ru_RU.UTF-8"
export LC_TIME="ru_RU.UTF-8"
export LC_COLLATE="ru_RU.UTF-8"
export LC_MONETARY="ru_RU.UTF-8"
export LC_MESSAGES="ru_RU.UTF-8"
export LC_PAPER="ru_RU.UTF-8"
export LC_NAME="ru_RU.UTF-8"
export LC_ADDRESS="ru_RU.UTF-8"
export LC_TELEPHONE="ru_RU.UTF-8"
export LC_MEASUREMENT="ru_RU.UTF-8"
export LC_IDENTIFICATION="ru_RU.UTF-8"

export PUB_HOSTED_URL=http://pub-dev.wrke.in
export DART_SDK_PATH=/usr/local

alias fr="git forest --all --style=10 --topo-order --color=always --pretty=format:' %s %C(blue)%an %C(red)%h%C(reset)' | less -R"
alias s="git s"
alias reupload="git reupload"
alias f="git f"
alias rebase="git rebase"
alias checkout="git checkout"
alias gpush="git push origin"
alias commit="git commit"
alias branch="git branch"
alias tag="git tag"
alias show="git show"
alias add="git add"
alias grm="git rm"
alias grma="git commit --dry-run --short | grep '^ D '| awk '{print $2}' | xargs git rm"
alias reset="git reset"
alias dff="git diff -w -b -M"
alias crm="git checkout remotes/origin/master"
alias stash="git stash"
alias co="git checkout"
alias gpro="git pull --rebase origin"
alias mc="mc -S modarin256"
alias mcedit="mcedit -S modarin256"

alias filter_shit="grep -v '/assets/' | grep -v 'favicon.ico' | grep -v '/vendor' | grep -v '/cookie' | grep -v '/fonts' | grep -v '/apple' | grep -v '/static'"

export TERM=screen-256color

if which tmux >/dev/null 2>&1
     test -z "$TMUX" && tmux new-session
end

