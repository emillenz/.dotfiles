;;; Compiled snippets and support files for `rust-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'rust-mode
                     '(("whilel" "while let ${1:pattern} = ${2:expression} {\n      $0\n}" "while let PATTERN = EXPR { ... }" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/while-let" nil "whilel")
                       ("while" "while ${1:true} { `(doom-snippets-format \"%n%s%n\")`$0 }" "while ... { ... }" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/while" nil "while")
                       ("warn!" "#![warn(${1:lint})]" "#![warn(lint)]" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/warn!" nil nil)
                       ("warn" "#[warn(${1:lint})]" "#[warn(lint)]" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/warn" nil nil)
                       ("v" "vec![${1:`%`}]" "vec![...]" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/vec" nil "v")
                       ("uhashmap" "use std::collections::HashMap;" "use std::collections::HashMap"
                        (doom-snippets-bolp)
                        nil nil "/home/lenz/.config/doom/snippets/rust-mode/use-HashMap" nil "uhashmap")
                       ("ufile" "use std::fs::File;" "use std::fs::File"
                        (doom-snippets-bolp)
                        nil nil "/home/lenz/.config/doom/snippets/rust-mode/use-File" nil "ufile")
                       ("use" "use ${1:std::${2:io}};$0" "use ..."
                        (doom-snippets-bolp)
                        nil nil "/home/lenz/.config/doom/snippets/rust-mode/use" nil "use")
                       ("un" "unsafe { `(doom-snippets-format \"%n%s%n\")`$0 }" "unsafe { ... }" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/unsafe" nil "un")
                       ("union" "union ${1:Type} {\n     $0\n}" "union Type { ... }" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/union" nil nil)
                       ("type" "type ${1:TypeName} = ${2:i32};" "type Name = ...;" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/type" nil "type")
                       ("tr"
                        (progn
                          (doom-snippets-expand :uuid "trait"))
                        "trait ... { ... }" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/trait_alias" nil "trait_alias")
                       ("trait" "trait ${1:Name} {\n    `%`$0\n}" "trait ... { ... }" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/trait" nil "trait")
                       ("testmod" "#[cfg(test)]\nmod ${1:tests} {\n    use super::*;\n\n    #[test]\n    fn ${2:test_name}() {\n       $0\n    }\n}" "test module" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/testmod" nil nil)
                       ("test" "#[test]\nfn ${1:test_name}() {\n   `%`$0\n}" "test function"
                        (doom-snippets-bolp)
                        nil nil "/home/lenz/.config/doom/snippets/rust-mode/test" nil "test")
                       ("st"
                        (progn
                          (doom-snippets-expand :uuid "struct"))
                        "struct"
                        (doom-snippets-bolp)
                        nil nil "/home/lenz/.config/doom/snippets/rust-mode/struct_alias" nil "struct_alias")
                       ("struct" "struct ${1:Name} {\n    `%`$0\n}" "struct"
                        (doom-snippets-bolp)
                        nil nil "/home/lenz/.config/doom/snippets/rust-mode/struct" nil "struct")
                       ("static" "static ${1:VARNAME}${2:: ${3:i32}} = ${4:value};" "static VAR = ..." nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/static" nil "static")
                       ("ret" "return ${1:`%`};$0" "return" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/ret" nil "ret")
                       ("pmod" "pub mod ${1:name} {\n    `%`$0\n}" "pub mod" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/pub_mod" nil "pmod")
                       ("pfn" "pub fn ${1:function_name}($2) ${3:-> ${4:i32} }{\n   `%`$0\n}" "public function" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/pub_fn" nil "pfn")
                       ("pafn" "pub async fn ${1:function_name}($2) ${3:-> ${4:i32} }{\n   `%`$0\n}\n" "public async function"
                        (doom-snippets-bolp)
                        nil nil "/home/lenz/.config/doom/snippets/rust-mode/pub_async_fn" nil "pafn")
                       ("p" "println!(\"$1\", ${2:`%`});$0" "println!(...)" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/println" nil "p")
                       ("partial" "impl PartialEq for ${1:Type} {\n    fn eq(&self, other: &Self) -> bool {\n        $0\n    }\n}\n" "impl PartialEq for Type" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/partial" nil "partial")
                       ("pa" "panic!(\"$1\", ${2:`%`});$0" "panic!(...)" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/panic" nil "pa")
                       ("new" "${1:Vec}::new(${2:`%`})`(if (eolp) \";\" \"\")`" "Type::new(...)" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/new" nil "new")
                       ("mod" "mod ${1:name} {\n    `%`$0\n}" "mod" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/mod" nil "mod")
                       ("ma"
                        (progn
                          (doom-snippets-expand :uuid "match"))
                        "match" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/match_alias" nil "match_alias")
                       ("match?" "match ${1:x} {\n    Ok(${2:var}) => $3,\n    Err(${4:error}) => $5\n}" "match n { Ok(x), Err(y) }" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/match-ok" nil "match?")
                       ("match" "match ${1:x} {\n    `%`$0\n}" "match" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/match" nil "match")
                       ("main" "fn main() {\n   `%`$0\n}" "main" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/main" nil "main")
                       ("macro" "macro_rules! ${1:name} {\n     ($2) => ($3);\n}\n" "macro_rules! name { (..) => (..); }" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/macro" nil "macro")
                       ("loop" "loop { `(doom-snippets-format \"%n%s%n\")`$0 }" "loop { ... }" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/loop" nil "loop")
                       ("'s" "'static" "'static" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/lifetime-static" nil "'s")
                       ("letm" "let mut ${1:var} = $0`(if (eolp) \";\" \"\")`" "let mut" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/letm" nil "letm")
                       ("let" "let ${1:var} = $0`(if (eolp) \";\" \"\")`" "let" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/let" nil "let")
                       ("fn" "|${1:x}|${2: -> i32} `(if (> (doom-snippets-count-lines %) 1) \"{ \" \"\")``(doom-snippets-format \"%n%s%n\")`$0`(if (> (doom-snippets-count-lines %) 1) \" }\" \"\")`" "anonymous function"
                        (not
                         (doom-snippets-bolp))
                        nil nil "/home/lenz/.config/doom/snippets/rust-mode/lambda" nil "fn")
                       ("iterator" "impl Iterator for ${1:Type} {\n    type Item = ${2:Type};\n    fn next(&mut self) -> Option<Self::Item> {\n        $0\n    }\n}\n" "impl Iterator for Type" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/iterator" nil "iterator")
                       ("ife" "if ${1:x} {${2:`%`}}${3: else {$4}}$0" "inline if-else" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/inline-if-else" nil "ife")
                       ("impl" "impl ${1:name}${2: for ${3:Type}} {\n   `%`$0\n}" "impl" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/impl" nil nil)
                       ("ign" "#[ignore]" "#[ignore]" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/ignore" nil "ign")
                       ("ifl" "if let ${1:Some(${2:x})} = ${3:var} {\n   `%`$0\n}" "if let ..." nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/if-let" nil "ifl")
                       ("if" "if ${1:x} {\n   `%`$0\n}" "if ... { ... }" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/if" nil "if")
                       ("fromstr" "impl FromStr for ${1:Type} {\n    type Err = ${2:Error};\n\n    fn from_str(s: &str) -> Result<Self, Self::Err> {\n        `%`\n        Ok(Self{})\n    }\n}\n" "impl FromStr for Type { fn from_str(...) }" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/fromstr" nil nil)
                       ("from" "impl From<${1:From}> for ${2:Type} {\n    fn from(source: $1) -> Self {\n       `%`$0\n       Self { }\n    }\n}\n" "impl From<From> for Type { fn from(...) }" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/from" nil nil)
                       ("f" "format!(\"$1\", ${2:`%`})" "format!(..., ...)" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/format" nil "f")
                       ("for" "for ${1:x} in ${2:items} {\n    `%`$0\n}" "for in" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/for" nil "for")
                       ("fn" "fn ${1:function_name}($2) ${3:-> ${4:i32} }{\n   `%`$0\n}\n" "function"
                        (doom-snippets-bolp)
                        nil nil "/home/lenz/.config/doom/snippets/rust-mode/fn" nil "fn")
                       ("ec" "extern crate ${1:name};" "extern crate ..."
                        (doom-snippets-bolp)
                        nil nil "/home/lenz/.config/doom/snippets/rust-mode/extern-crate" nil "ec")
                       ("extc" "extern \"C\" {\n    `%`$0\n}" "extern \"C\" { ... }"
                        (doom-snippets-bolp)
                        nil nil "/home/lenz/.config/doom/snippets/rust-mode/extern" nil "extc")
                       ("error" "impl std::error::Error for ${1:Type} {\n    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {\n        $0\n        None\n    }\n}\n" "impl Error for Type { fn source(...) }" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/error" nil "error")
                       ("ep" "eprintln!(\"$1\", ${2:`%`});$0" "eprintln!(...)" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/eprintln" nil "ep")
                       ("envv" "env::var(\"$1\")" "env::var(...)" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/env-var" nil "envv")
                       ("argv" "env::args()" "env::args()" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/env-args" nil "argv")
                       ("enum" "enum ${1:EnumName} {\n    `%`$0\n}" "enum" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/enum" nil "enum")
                       ("elif" "else if ${1:true} {\n   `%`$0\n}" "else if ... { ... }" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/elseif" nil "elif")
                       ("else" "else {\n   `%`$0\n}" "else { ... }" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/else" nil "else")
                       ("display" "impl Display for ${1:Type} {\n    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {\n        write!(f, \"`%`$0\")\n    }\n}\n" "impl Display for Type { fn fmt (...) }" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/display" nil nil)
                       ("disperror" "impl Display for $1 {\n    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {\n        write!(f, \"{}\", $0)\n    }\n}\n\nimpl std::error::Error for ${1:Type} {\n    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {\n        None\n    }\n}\n" "Display and Error Traits" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/disperror" nil "disperror")
                       ("der" "#[derive($1)]" "#[derive(...)]"
                        (doom-snippets-bolp)
                        nil nil "/home/lenz/.config/doom/snippets/rust-mode/derive" nil "der")
                       ("deref_mut" "impl std::ops::DerefMut for ${1:Type} {\n    fn deref_mut(&mut self) -> &mut Self::Target {\n        &mut self.$0\n    }\n}\n" "impl DerefMut for Type" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/deref_mut" nil "deref_mut")
                       ("deref" "impl std::ops::Deref for ${1:Type} {\n    type Target = ${2:Type};\n    fn deref(&self) -> &Self::Target {\n        &self.$0\n    }\n}\n" "impl Deref for Type" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/deref" nil "deref")
                       ("dass" "debug_assert!(`%`$0);" "debug_assert!(...)" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/debug_assert" nil "dass")
                       ("const" "const ${1:VARNAME}${2: ${3:i32}} = ${4:value};" "const VAR = ..." nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/const" nil "const")
                       ("cfg=" "#[cfg(${1:option} = \"${2:value}\")]" "#[cfg(option = \"value\")]" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/cfg=" nil nil)
                       ("cfg" "#[cfg($0)]" "#[cfg(...)]" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/cfg" nil nil)
                       ("case" "${1:pattern} => ${2:expression}," "pattern => expression," nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/case" nil nil)
                       ("b" "${1:Label} { `(doom-snippets-format \"%n%s%n\")`$1 }$0" "block" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/block" nil "b")
                       ("afn" "async fn ${1:function_name}($2) ${3:-> ${4:i32} }{\n   `%`$0\n}\n" "async function"
                        (doom-snippets-bolp)
                        nil nil "/home/lenz/.config/doom/snippets/rust-mode/async_fn" nil "afn")
                       ("=" "${1:x} = ${2:value}`(if (eolp) \";\" \"\")`$0" "assignment" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/assignment" nil "=")
                       ("assn" "assert_ne!(${1:`%`}, $2);" "assert_ne!(..., ...)" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/assert_ne" nil "assn")
                       ("asse" "assert_eq!(${1:`%`}, $2);" "assert_eq!(..., ...)" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/assert_eq" nil "asse")
                       ("ass" "assert!(`%`$0);" "assert!(...)" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/assert" nil "ass")
                       ("asref" "impl std::convert::AsRef<${1:Type}> for ${2:Type} {\n    fn as_ref(&self) -> &$2 {\n        $0\n    }\n}\n" "impl AsRef<Type> for Type" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/asref" nil "asref")
                       ("=>" "${1:_} => ${0:...}" "x => y" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/arrow" nil "=>")
                       ("allow!" "#![allow(${1:lint})]" "#![allow(lint)]" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/allow!" nil nil)
                       ("allow" "#[allow(${1:lint})]" "#[allow(lint)]" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/allow" nil nil)
                       ("ts" "to_string()" ".to_string()"
                        (doom-snippets-without-trigger
                         (eq
                          (char-before)
                          46))
                        nil nil "/home/lenz/.config/doom/snippets/rust-mode/_to_string" nil "ts")
                       ("i" "iter()" ".iter()"
                        (doom-snippets-without-trigger
                         (eq
                          (char-before)
                          46))
                        nil nil "/home/lenz/.config/doom/snippets/rust-mode/_iter" nil "i")
                       ("ii" "into_iter()" ".into_iter()"
                        (doom-snippets-without-trigger
                         (eq
                          (char-before)
                          46))
                        nil nil "/home/lenz/.config/doom/snippets/rust-mode/_into_iter" nil "ii")
                       ("fm" "filter_map(${1:`%`})" ".filter_map(...)"
                        (doom-snippets-without-trigger
                         (eq
                          (char-before)
                          46))
                        nil nil "/home/lenz/.config/doom/snippets/rust-mode/_filter_map" nil "fm")
                       ("vwc" "Vec::with_capacity(${1:n})" "Vec::with_capacity(...)" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/Vec-with_capacity" nil "vwc")
                       ("sf" "String::from(\"$0\")" "String::from(...)" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/String_from" nil "sf")
                       ("so" "Some(${1:`%`})" "Some(...)" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/Some" nil "so")
                       ("res" "Result<${1:T}, ${2:()}>" "Result<T, E>" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/Result" nil "res")
                       ("no" "None" "None" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/None" nil "no")
                       ("fo" "File::open(${1:`%`})`(if (eolp) \";\" \"\")`" "File::open(...)" nil nil nil "/home/lenz/.config/doom/snippets/rust-mode/File-open" nil "fo")))


;;; Do not edit! File generated at Thu Apr 25 09:20:49 2024
