#!/usr/bin/env fish

function u_cp  --description="create subdirectories if neccessary"
    command cp $argv &>/dev/null

    set -l dest_dir $argv[(count $argv)]

    if test $status = 0 && not test -e $dest_dir
        switch $dest_dir
            case "-*"
            case "*"
                set -l parent_dir (string split '/' $argv[(count $argv)] --right --fields=1)
                command mkdir --parents $parent_dir
                echo "created directory: $parent_dir"
                command cp $argv
        end
    end
end
