# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

####

function huh () {
    echo -e "Real: $(id -run):$(id -rgn)\n$(id -rGn)"; echo -e "\nEffective: $(id -un):$(id -gn)\n$(id -Gn)"
}

function vm? () {
    command dmesg | command grep -Pi '(vmware|vbox|hypervisor)'
}

function ancestry {
    local pid=${1:-$$}
    local origPid=$pid
    local ppid=0
    local COND=
    while (( $ppid != 1 )); do # init
        ppid=$(/bin/ps -o ppid= -p $pid 2> /dev/null)
        if (( $? != 0 )); then
            echo "Process $pid does not exist."
            return 1
        fi
        if (( $ppid != 1 )) ; then
            if [[ "$COND" == '' ]]; then 
                COND="\$1==$ppid"
            else
                COND="${COND} || \$1==$ppid" 
            fi
        fi
        pid=$ppid
    done
    COND="${COND} || \$1==$origPid"   
    /bin/ps axf | /usr/bin/head -1
    /bin/ps axf | eval "/usr/bin/awk '{if ($COND) print \$0;}'"
}

function lddd() {
    LD_TRACE_LOADED_OBJECTS=1 $1
}

########
export TERM=xterm-256color
export HISTCONTROL=erasedups
export HISTSIZE=10000
shopt -s histappend
shopt -s checkwinsize

alias ls='ls --color=auto'
unset LS_COLORS
export LS_COLORS='di=1:ow=1:fi=0:ln=36:pi=5:so=5:bd=5:cd=5:or=31;01:mi=0;05;01:ex=35:'

alias l='ls -lh'
alias ll='ls -lh'
alias la='ls -lrtha'
alias lr='ls -lrth'
alias lt='ls -lrth'
alias tt='tree -Faghpu'

alias e=emacs25
alias en='e -nw'

export EDITOR='emacs25 -nw'
alias E="sudoedit $@"

export VISUAL=emacs25

####

# export PYTHONPATH=/usr/local/alembic-1.5.8/lib:/home/cwachter/dev/zabc/libpython/:$PYTHONPATH





####
export SUDO_PS1="\n-< NOT YOU: \u >-@\h/\t\n[\w] $ "

if [[ ! -n "$SSH_TTY" ]]; then
    export PS1="\n\u@\h \t\n[\w] $ "
else
    export PS1="{ssh-\u@\h \w } > "
fi
