# ---
# title:  nushell custom commands and aliases
# author: emil lenz
# email:  emillenz@protonmail.com
# date:   2024-03-05
# ---

# mv :: automatically create missing destination dir's.
export def mv [from: path, to: path] {
        if not ($to | path exists) {
                mkdir --verbose ($to | path dirname)
        }
        ^mv --verbose $from $to
}

# cp :: automatically create missing destination dir's.
export def cp [from: path, to: path] {
        if not ($to | path exists) {
                mkdir --verbose ($to | path dirname)
        }
        ^cp --verbose --progress --interactive $from $to
}
