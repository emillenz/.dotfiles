;;; Compiled snippets and support files for `csharp-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'csharp-mode
                     '(("using" "using System.$1;" "using System....;" nil nil nil "/home/lenz/.config/doom/snippets/csharp-mode/using.2" nil "using")
                       ("using" "using System;" "using System;" nil nil nil "/home/lenz/.config/doom/snippets/csharp-mode/using.1" nil "using")
                       ("using" "using $1;" "using ...;" nil nil nil "/home/lenz/.config/doom/snippets/csharp-mode/using" nil "using")
                       ("region" "#region $1\n$0\n#endregion" "#region ... #endregion" nil nil nil "/home/lenz/.config/doom/snippets/csharp-mode/region" nil "region")
                       ("prop" "/// <summary>\n/// $5\n/// </summary>\n/// <value>$6</value>\n$1 $2 $3\n{\n    get {\n        return this.$4;\n    }\n    set {\n        this.$4 = value;\n    }\n}" "property ... ... { ... }" nil nil nil "/home/lenz/.config/doom/snippets/csharp-mode/prop" nil "prop")
                       ("namespace" "namespace $1\n{\n$0\n}" "namespace .. { ... }" nil nil nil "/home/lenz/.config/doom/snippets/csharp-mode/namespace" nil "namespace")
                       ("method" "/// <summary>\n/// ${5:Description}\n/// </summary>${2:$(if (string= (upcase yas-text) \"VOID\") \"\" (format \"%s%s%s\" \"\\n/// <returns><c>\" yas-text \"</c></returns>\"))}\n${1:public} ${2:void} ${3:MethodName}($4)\n{\n$0\n}" "public void Method { ... }" nil nil nil "/home/lenz/.config/doom/snippets/csharp-mode/method" nil "method")
                       ("foreach" "foreach (${1:var} ${2:item} in ${3:collection})\n{\n    $0\n}" "foreach" nil nil nil "/home/lenz/.config/doom/snippets/csharp-mode/foreach" nil "foreach")
                       ("comment" "/// <exception cref=\"$1\">$2</exception>" "/// <exception cref=\"...\"> ... </exception>" nil nil nil "/home/lenz/.config/doom/snippets/csharp-mode/comment.3" nil "comment")
                       ("comment" "/// <returns>$1</returns>" "/// <param name=\"...\"> ... </param>" nil nil nil "/home/lenz/.config/doom/snippets/csharp-mode/comment.2" nil "comment")
                       ("comment" "/// <param name=\"$1\">$2</param>" "/// <param name=\"...\"> ... </param>" nil nil nil "/home/lenz/.config/doom/snippets/csharp-mode/comment.1" nil "comment")
                       ("comment" "/// <summary>\n/// $1\n/// </summary>" "/// <summary> ... </summary>" nil nil nil "/home/lenz/.config/doom/snippets/csharp-mode/comment" nil "comment")
                       ("class" "${5:public} class ${1:Name}\n{\n    #region Ctor & Destructor\n    /// <summary>\n    /// ${3:Standard Constructor}\n    /// </summary>\n    public $1($2)\n    {\n    }\n\n    /// <summary>\n    /// ${4:Default Destructor}\n    /// </summary>    \n    public ~$1()\n    {\n    }\n    #endregion\n}" "class ... { ... }" nil nil nil "/home/lenz/.config/doom/snippets/csharp-mode/class" nil "class")
                       ("attrib" "/// <summary>\n/// $3\n/// </summary>\nprivate $1 ${2:$(if (> (length yas-text) 0) (format \"_%s%s\" (downcase (substring yas-text 0 1)) (substring yas-text 1 (length yas-text))) \"\")};\n\n/// <summary>\n/// ${3:Description}\n/// </summary>\n/// <value><c>$1</c></value>\npublic ${1:Type} ${2:Name}\n{\n    get {\n        return this.${2:$(if (> (length yas-text) 0) (format \"_%s%s\" (downcase (substring yas-text 0 1)) (substring yas-text 1 (length yas-text))) \"\")};\n    }\n    set {\n        this.${2:$(if (> (length yas-text) 0) (format \"_%s%s\" (downcase (substring yas-text 0 1)) (substring yas-text 1 (length yas-text))) \"\")} = value;\n    }\n}" "private _attribute ....; public Property ... ... { ... }" nil nil nil "/home/lenz/.config/doom/snippets/csharp-mode/attrib.2" nil "attrib")
                       ("attrib" "/// <summary>\n/// $3\n/// </summary>\nprivate $1 $2;\n\n/// <summary>\n/// $4\n/// </summary>\n/// <value>$5</value>\npublic $1 $2\n{\n    get {\n        return this.$2;\n    }\n    set {\n        this.$2 = value;\n    }\n}" "private attribute ....; public property ... ... { ... }" nil nil nil "/home/lenz/.config/doom/snippets/csharp-mode/attrib.1" nil "attrib")
                       ("attrib" "/// <summary>\n/// $3\n/// </summary>\nprivate $1 $2;" "private attribute ....;" nil nil nil "/home/lenz/.config/doom/snippets/csharp-mode/attrib" nil "attrib")))


;;; Do not edit! File generated at Thu Apr 25 09:20:47 2024
