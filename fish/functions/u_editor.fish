#!/usr/bin/env fish

function u_editor --description="open $EDITOR with args, if nil open $EDITOR with fileexplorer in current directory"
    if test -z "$argv"
        $EDITOR (pwd)
    else
        $EDITOR $argv
    end
end
