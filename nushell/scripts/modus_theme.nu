# -----
# title:  Modus vivendi colortheme for Nushell
# author: Emil Lenz
# email:  emillenz@protonmail.com
# date:   2024-01-26
# -----

const modus_vivendi = {
        background: "#000000"
        foreground: "#ffffff"
        cursor:     "#ffffff"

        # OUTPUT
        separator:                  "#ffffff"
        header:                     {fg: "#00bcff" attr: "b"}
        row_index:                  {fg: "#00bcff" attr: "b"}

        search_result:              {fg: "#000000" bg: "#f3d000"}

        # NOTE :: differentiate numbers from types; string -> white default
        int:                        "#00bcff"
        float:                      "#00bcff"

        empty:                      "#6ae4b9"
        bool:                       "#6ae4b9"
        filesize:                   "#6ae4b9"
        duration:                   "#6ae4b9"
        nothing:                    "#6ae4b9"
        cellpath:                   "#6ae4b9"
        date:                       "#6ae4b9"
        range:                      "#6ae4b9"

        string:                     "#ffffff"
        binary:                     "#ffffff"
        record:                     "#ffffff"
        list:                       "#ffffff"
        block:                      "#ffffff"

        # SYNTAX
        hints:                      "#989898"
        shape_garbage:              {fg: "#000000" bg: "#ff5f59"}
        shape_external:             {fg: "#feacd0" attr: "b"}
        shape_internalcall:         {fg: "#feacd0" attr: "b"}
        shape_custom:               {fg: "#feacd0" attr: "b"}
        shape_variable:             "#00d3d0"

        shape_externalarg:          "#b6a0ff"
        shape_flag:                 "#b6a0ff"

        shape_directory:            "#6ae4b9"
        shape_filepath:             "#6ae4b9"
        shape_datetime:             "#6ae4b9"
        shape_binary:               "#6ae4b9"
        shape_nothing:              "#6ae4b9"
        shape_bool:                 "#6ae4b9"
        shape_float:                "#00bcff"
        shape_int:                  "#00bcff"
        shape_literal:              "#79a8ff"
        shape_string:               "#79a8ff"
        shape_string_interpolation: "#79a8ff"
        shape_signature:            "#79a8ff"

        shape_block:                "#c6daff"
        shape_matching_brackets:    "#c6daff"
        shape_list:                 "#c6daff"
        shape_range:                "#c6daff"
        shape_record:               "#c6daff"
        shape_table:                "#c6daff"

        shape_operator:             "#ff7f9f"
        shape_globpattern:          "#ff7f9f"
        shape_match_pattern:        "#ff7f9f"
        shape_or:                   "#ff7f9f"
        shape_pipe:                 "#ff7f9f"
        shape_and:                  "#ff7f9f"
        shape_redirection:          "#ff7f9f"
}

const modus_operandi = {
        background: "#ffffff"
        foreground: "#000000"
        cursor:     "#000000"

        # OUTPUT
        separator:                  "#000000"
        header:                     {fg: "#0031a9" attr: "b"}
        row_index:                  {fg: "#0031a9" attr: "b"}

        search_result:              {fg: "#f3d000" bg: "#000000"}

        # NOTE :: differentiate numbers from types; string -> white default
        int:                        "#005f5f"
        float:                      "#005f5f"

        empty:                      "#3548cf"
        bool:                       "#3548cf"
        filesize:                   "#3548cf"
        duration:                   "#3548cf"
        nothing:                    "#3548cf"
        cellpath:                   "#3548cf"
        date:                       "#3548cf"
        range:                      "#3548cf"

        string:                     "#000000"
        binary:                     "#000000"
        record:                     "#000000"
        list:                       "#000000"
        block:                      "#000000"

        # SYNTAX
        hints:                      "#595959"
        shape_garbage:              {fg: "#000000" bg: "#ff5f59"}
        shape_external:             {fg: "#721045" attr: "b"}
        shape_internalcall:         {fg: "#721045" attr: "b"}
        shape_custom:               {fg: "#721045" attr: "b"}
        shape_variable:             "#005e8b"

        shape_externalarg:          "#531ab6"
        shape_flag:                 "#531ab6"

        shape_directory:            "#005f5f"
        shape_filepath:             "#005f5f"
        shape_datetime:             "#005f5f"
        shape_binary:               "#005f5f"
        shape_nothing:              "#005f5f"
        shape_bool:                 "#005f5f"
        shape_float:                "#8f0075"
        shape_int:                  "#8f0075"
        shape_literal:              "#3548cf"
        shape_string:               "#3548cf"
        shape_string_interpolation: "#3548cf"
        shape_signature:            "#3548cf"

        shape_block:                "#0031a9"
        shape_matching_brackets:    "#0031a9"
        shape_list:                 "#0031a9"
        shape_range:                "#0031a9"
        shape_record:               "#0031a9"
        shape_table:                "#0031a9"

        shape_operator:             "#a0132f"
        shape_globpattern:          "#a0132f"
        shape_match_pattern:        "#a0132f"
        shape_or:                   "#a0132f"
        shape_pipe:                 "#a0132f"
        shape_and:                  "#a0132f"
        shape_redirection:          "#a0132f"
}
