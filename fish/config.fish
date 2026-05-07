if status is-interactive
    # Commands to run in interactive sessions can go here
end

set -U fish_greeting ""

function !!
    eval sudo $history[1]
end

alias ls='ls --color=auto'
alias grep='grep --color=auto'

# Beautifying commands
alias neofetch='fastfetch'
alias nf='fastfetch'
alias ff='fastfetch --color-keys blue --logo-color-1 "blue" --logo-color-2 "blue" --color-title "blue"'
alias cow='fortune | cowsay'
alias startx='start-de.sh'

# Utilities
alias cpc='xclip -sel c <'
alias c='clear'
alias xampp='sudo /opt/lampp/manager-linux-x64.run'
alias protontricks='protontricks --no-bwrap --gui'

# "Short"cuts
alias nv='nvim'
alias v='vim'
alias rofimoji='rofimoji --action copy'
alias rsync="rsync -avhP"
alias cat='bat'
alias vesktop="discord_dbus"
alias cd_votv="cd /home/anon/.steam/steam/steamapps/compatdata/4108658418/pfx/drive_c/users/steamuser/AppData/Local/VotV/Saved/SaveGames"
alias wolframscript="wolframscript -charset UTF"

function manimplay -a file scene
    while true
	find . -name $file | entr -d sh -c "manim -ql $file $scene && cvlc --one-instance --play-and-pause ./media/videos/main/480p15/$scene.mp4 > /dev/null 2>&1 &"
    end
end

alias e="emacsclient -c -a ''"
alias es='/usr/local/bin/emacs --load /home/anon/.emacs.d/init.el --daemon &'
alias tldr='tldr --pager'

# Exports
export EDITOR="emacsclient -nw -c -a ''"
export INFOPATH="$INFOPATH:/home/anon/.emacs.d/info:/usr/share/info"
set -gx PATH $PATH /home/anon/.miniforge3/bin/

# Sets
set QT_SCALE_FACTOR 3
set TERM screen-256color

# Binds
bind \eo append_and_disown

function r
    set -l tempfile '/tmp/lf_cd_'.(id -u)
    lf -config /home/anon/.config/lf/.lfrc -last-dir-path=$tempfile $argv
    if test -s $tempfile
        set -l lastdir (cat $tempfile)
        cd $lastdir
        command rm -f $tempfile
    end
end

function append_and_disown
    set -l current_command (commandline -b)
    commandline -r "$current_command & disown"
end