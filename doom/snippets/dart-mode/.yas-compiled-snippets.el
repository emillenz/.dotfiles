;;; Compiled snippets and support files for `dart-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'dart-mode
                     '(("while" "while ($1) {\n  $0\n}" "while" nil nil nil "/home/lenz/.config/doom/snippets/dart-mode/while" nil nil)
                       ("typedef" "typedef ${1:Type} ${2:Name}(${3:params});" "typedef" nil nil nil "/home/lenz/.config/doom/snippets/dart-mode/typedef" nil nil)
                       ("try" "try {\n  $0\n} catch (${1:e}) {\n}" "try" nil nil nil "/home/lenz/.config/doom/snippets/dart-mode/try" nil nil)
                       ("test" "test('$1', () {\n  $0\n});" "test" nil nil nil "/home/lenz/.config/doom/snippets/dart-mode/test" nil nil)
                       ("switch" "switch ($1) {\n  case $2:\n    $0\n    break;\n  default:\n}" "switch case" nil nil nil "/home/lenz/.config/doom/snippets/dart-mode/switch" nil nil)
                       ("stless" "class $1 extends StatelessWidget {\n  @override\n  Widget build(BuildContext context) {\n    return Container(\n      $2\n    );\n  }\n}" "StatelessWidget" nil
                        ("flutter")
                        nil "/home/lenz/.config/doom/snippets/dart-mode/stless" nil nil)
                       ("stful" "class $1 extends StatefulWidget {\n  @override\n  _$1State createState() => _$1State();\n}\n\nclass _$1State extends State<$1> {\n  @override\n  Widget build(BuildContext context) {\n    return Container(\n      $2\n    );\n  }\n}" "StatefulWidget" nil
                        ("flutter")
                        nil "/home/lenz/.config/doom/snippets/dart-mode/stful" nil nil)
                       ("stanim" "class $1 extends StatefulWidget {\n  @override\n  _$1State createState() => _$1State();\n}\n\nclass _$1State extends State<$1>\n    with SingleTickerProviderStateMixin {\n  AnimationController _controller;\n\n  @override\n  void initState() {\n    super.initState();\n    _controller = AnimationController(vsync: this);\n  }\n\n  @override\n  void dispose() {\n    super.dispose();\n    _controller.dispose();\n  }\n\n  @override\n  Widget build(BuildContext context) {\n    return Container(\n      $2\n    );\n  }\n}" "StateufulWidget with AnimationController" nil
                        ("flutter")
                        nil "/home/lenz/.config/doom/snippets/dart-mode/stanim" nil nil)
                       ("set" "${1:Type} _${2:Name};\nset $2($1 $2) => _$2 = $2;" "setter" nil nil nil "/home/lenz/.config/doom/snippets/dart-mode/set" nil nil)
                       ("part" "part of ${1:Part}$0" "part" nil nil nil "/home/lenz/.config/doom/snippets/dart-mode/part" nil nil)
                       ("main" "main(List<String> args) {\n  $0\n}" "main" nil
                        ("dart")
                        nil "/home/lenz/.config/doom/snippets/dart-mode/main" nil nil)
                       ("is" "@override\nvoid initState() {\n  super.initState();\n  $0\n}" "initState" nil
                        ("flutter")
                        nil "/home/lenz/.config/doom/snippets/dart-mode/inis" nil nil)
                       ("imp" "import '${1:Library}';$0" "import" nil nil nil "/home/lenz/.config/doom/snippets/dart-mode/impo" nil nil)
                       ("impl" "implements ${1:Name}$0" "implements" nil nil nil "/home/lenz/.config/doom/snippets/dart-mode/impl" nil nil)
                       ("ife" "if ($1) {\n  $0\n} else {\n}" "if else" nil nil nil "/home/lenz/.config/doom/snippets/dart-mode/ife" nil nil)
                       ("if" "if ($1) {\n  $0\n}" "if" nil nil nil "/home/lenz/.config/doom/snippets/dart-mode/if" nil nil)
                       ("group" "group('$1', () {\n  $0\n});" "group" nil nil nil "/home/lenz/.config/doom/snippets/dart-mode/group" nil nil)
                       ("getset" "${1:Type} _${2:Name};\n$1 get $2 => _$2;\nset $2($1 $2) => _$2 = $2;$0" "getset" nil nil nil "/home/lenz/.config/doom/snippets/dart-mode/getset" nil nil)
                       ("get" "${1:Type} _${2:Name};\n$1 get $2 => _$2;$0" "getter" nil nil nil "/home/lenz/.config/doom/snippets/dart-mode/get" nil nil)
                       ("fun" "${1:Type} ${2:Name}($3) {\n  $0\n}" "fun" nil nil nil "/home/lenz/.config/doom/snippets/dart-mode/fun" nil nil)
                       ("fori" "for (var ${1:item} in ${2:items}) {\n  $0\n}" "for-in" nil nil nil "/home/lenz/.config/doom/snippets/dart-mode/fori" nil nil)
                       ("for" "for (var i = 0; i < ${1:count}; i++) {\n  $0\n}" "for" nil nil nil "/home/lenz/.config/doom/snippets/dart-mode/for" nil nil)
                       ("ext" "extends ${1:Name}$0" "ext" nil nil nil "/home/lenz/.config/doom/snippets/dart-mode/ext" nil nil)
                       ("elif" "else if ($1) {\n  $0\n}" "if else" nil nil nil "/home/lenz/.config/doom/snippets/dart-mode/elif" nil nil)
                       ("do" "do {\n  $0\n} while ($1);" "do while" nil nil nil "/home/lenz/.config/doom/snippets/dart-mode/do" nil nil)
                       ("dis" "@override\nvoid dispose() {\n  super.dispose();\n  $0\n}" "dispose" nil
                        ("flutter")
                        nil "/home/lenz/.config/doom/snippets/dart-mode/dis" nil nil)
                       ("dcd" "@override\nvoid didChangeDependencies() {\n  super.didChangeDependencies();\n  $0\n}" "didChangeDependencies" nil
                        ("flutter")
                        nil "/home/lenz/.config/doom/snippets/dart-mode/didchdep" nil nil)
                       ("cls" "class ${1:Name} {\n  $0\n}" "class" nil nil nil "/home/lenz/.config/doom/snippets/dart-mode/cls" nil nil)
                       ("blt" "abstract class ${1:Name} implements Built<$1, $1Builder> {\n  factory $1([void Function($1Builder) updates]) = _$$1;\n  $1._();\n\n  $0\n}" "built value" nil
                        ("dart")
                        nil "/home/lenz/.config/doom/snippets/dart-mode/bltval" nil nil)
                       ("afun" "Future<${1:Type}> ${2:Name}($3) async {\n  $0\n}" "async fun" nil nil nil "/home/lenz/.config/doom/snippets/dart-mode/afun" nil nil)
                       ("acls" "abstract class ${1:Name} {\n  $0\n}" "abstract class" nil nil nil "/home/lenz/.config/doom/snippets/dart-mode/acls" nil nil)))


;;; Do not edit! File generated at Thu Apr 25 09:20:47 2024
