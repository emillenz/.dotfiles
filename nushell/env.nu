use std

# Directories to search for scripts when calling source or use
$env.NU_LIB_DIRS = [
        ($nu.default-config-dir | path join 'scripts')
]

# Directories to search for plugin binaries when calling register
$env.NU_PLUGIN_DIRS = [
        ($nu.default-config-dir | path join 'plugins')
]

# NOTE :: Using somethinng as starship is bloat if you can define your own prompt inside nushell (way more understandeable and extensible).
def create_left_prompt [] {
        let dir = (
                if ($env.PWD | path split | zip ($nu.home-path | path split) | all { $in.0 == $in.1 }) {
                        ($env.PWD | str replace $nu.home-path "~")
                } else {
                        $env.PWD
                }
        )
        let path_color = (if (is-admin) { ansi red_bold } else { ansi magenta_bold })
        let path = ([$path_color, $dir, (ansi reset)] | str join)

        let last_exit_code = match $env.LAST_EXIT_CODE {
                        0 => ""
                        _ => $"(ansi red)[err] (ansi reset)"
        }

        let dur_mod = ([
                (ansi yellow),
                ($"($env.CMD_DURATION_MS)ms" | into duration | format duration sec),
                (ansi reset),
                ] | str join
        )

        let time_mod = ([
                (ansi magenta),
                (date now | format date '%F %a %R'),
                (ansi reset),
                ] | str join
        )

        let mod_sep =        ' │ '
        let modules = ([
                "[",
                ([$dur_mod, $time_mod] | str join $mod_sep),
                "]",
                ] | str join " ")

        let fill_len = ((term size | get columns) - ($modules | ansi strip | str length) - 1)
        let fill = ([
                (ansi reset)
                "\n "
                ('─' | std repeat $fill_len | str join),
                ] | str join
        )

        return (
                [$fill,
                        $modules,
                        "\n",
                        $path,
                        $last_exit_code
                ]
                | str join " "
        )
}

$env.PROMPT_COMMAND = {|| create_left_prompt }
$env.PROMPT_COMMAND_RIGHT = ""

$env.PROMPT_INDICATOR = {|| $"(ansi blue_bold)::(ansi reset) " }
$env.PROMPT_INDICATOR_VI_INSERT = {|| $"(ansi blue_bold)::(ansi reset) " }
$env.PROMPT_INDICATOR_VI_NORMAL = {|| $"(ansi green_bold)::(ansi reset) " }
$env.PROMPT_MULTILINE_INDICATOR = {|| $"(ansi blue_bold)>>(ansi reset) " }

# If you want previously entered commands to have a different prompt from the usual one,
# you can uncomment one or more of the following lines.
# This can be useful if you have a 2-line prompt and it's taking up a lot of space
# because every command entered takes up 2 lines instead of 1. You can then uncomment
# the line below so that previously entered commands show with a single `🚀`.
# $env.TRANSIENT_PROMPT_COMMAND = {|| "... " }
# $env.TRANSIENT_PROMPT_INDICATOR = {|| "" }
# $env.TRANSIENT_PROMPT_INDICATOR_VI_INSERT = {|| "" }
# $env.TRANSIENT_PROMPT_INDICATOR_VI_NORMAL = {|| "" }
# $env.TRANSIENT_PROMPT_MULTILINE_INDICATOR = {|| "" }
# $env.TRANSIENT_PROMPT_COMMAND_RIGHT = {|| "" }

# Specifies how environment variables are:
# - converted from a string to a value on Nushell startup (from_string)
# - converted from a value back to a string when running external commands (to_string)
# NOTE :: The conversions happen *after* config.nu is loaded
$env.ENV_CONVERSIONS = {
        "PATH": {
                from_string: { |s| $s | split row (char esep) | path expand --no-symlink }
                to_string: { |v| $v | path expand --no-symlink | str join (char esep) }
        }
        "Path": {
                from_string: { |s| $s | split row (char esep) | path expand --no-symlink }
                to_string: { |v| $v | path expand --no-symlink | str join (char esep) }
        }
}
