# ---
# title:  nushell utility commands
# author: emil lenz
# email:  emillenz@protonmail.com
# date:   2024-03-05
# ---

# async :: launch async bash command
export def async [cmd: string, --output (-o)] {
        $"($cmd)(if $output { ' &>/dev/null' }) &" | bash
}

# rofi integrated with nushell ::
export def rofi [input: list, menu_name: string, icon?: string] {
        let prompt = (
                if ($icon == null) {
                        $"($menu_name) ::"
                } else {
                        $"($icon)  ($menu_name) ::"
                }
        )

        $input
        | to text
        | ^rofi -dmenu -i -no-custom -p $prompt
        | str trim
        | if $in == "" {
                exit 1
        } else {
                $in
        }
}
