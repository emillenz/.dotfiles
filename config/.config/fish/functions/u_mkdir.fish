#!/usr/bin/env fish

function u_mkdir --description="create a directory (and neccessary parent directories) and set cwd"
    command mkdir --parents $argv
    set -l dest_dir $argv[(count $argv)]
    if test $status = 0
        switch $dest_dir
            case "-*"
            case "*"
                cd $dest_dir
        end
    end
end
