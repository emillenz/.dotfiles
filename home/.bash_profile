[ -f ~/.bashrc ] && source $HOME/.bashrc

export GOPATH=$HOME/.local/share/go
# export CPLUS_INCLUDE_PATH=$CPLUS_INCLUDE_PATH:/usr/include/eigen3

export PATH="\
$PATH:\
$GOPATH:\
$HOME/.config/emacs/bin:\
$HOME/.local/bin"
