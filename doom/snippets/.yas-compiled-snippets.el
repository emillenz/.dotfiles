;;; Compiled snippets and support files for `snippets'
;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("doom-snippets.el" ";;; doom-snippets.el --- A collection of yasnippet snippets\n;;\n;; Copyright (C) 2017-2022 Henrik Lissner\n;;\n;; Author: Henrik Lissner <contact@henrik.io>\n;; Created: December 5, 2014\n;; Modified: May 04, 2022\n;; Version: 1.1.0\n;; Keywords: convenience, snippets\n;; Homepage: https://github.com/doomemacs/snippets\n;; Package-Requires: ((emacs \"24.4\") (cl-lib \"0.5\") (yasnippet \"0.12.0\"))\n;;\n;; This file is not part of GNU Emacs.\n;;\n;;; Commentary:\n;;\n;; Official snippet collection for Doom Emacs. See https://github.com/doomemacs.\n;;\n;;; Code:\n\n(defvar doom-snippets-dir\n  (file-name-directory\n   (cond (load-in-progress load-file-name)\n         ((bound-and-true-p byte-compile-current-file)\n          byte-compile-current-file)\n         (buffer-file-name)))\n  \"The base directory of the doom-snippets library.\")\n\n(defvar doom-snippets-enable-short-helpers nil\n  \"If non-nil, defines convenience aliases for doom-snippets' api.\n\n+ `!%!' = (doom-snippets-format \\\"%n%s%n\\\")\n+ `!%' = (doom-snippets-format \\\"%n%s\\\")\n+ `%$' = (doom-snippets-format \\\"%e\\\")\n+ `%expand' = `doom-snippets-expand'\n+ `%format' = `doom-snippets-format'\n+ `%without-trigger' = `doom-snippets-without-trigger'\")\n\n;;;###autoload\n(defun doom-snippets-remove-compiled-snippets ()\n  \"Delete all .yas-compiled-snippets.el files.\"\n  (interactive)\n  (let ((default-directory doom-snippets-dir))\n    (dolist (file (file-expand-wildcards \"*/.yas-compiled-snippets.el\"))\n      (delete-file file)\n      (message \"Deleting %s\" file))))\n\n;;;###autoload\n(defun doom-snippets-initialize ()\n  \"Add `doom-snippets-dir' to `yas-snippet-dirs', replacing the default\nyasnippet directory.\"\n  (setq yas-wrap-around-region nil)\n  (add-to-list 'yas-snippet-dirs 'doom-snippets-dir)\n  (yas-load-directory doom-snippets-dir t))\n\n;;;###autoload\n(eval-after-load 'yasnippet\n  (lambda () (doom-snippets-initialize)))\n\n(provide 'doom-snippets)\n;;; doom-snippets.el ends here\n" "doom-snippets.el" nil nil nil "/home/orion/.config/doom/snippets/snippets/doom-snippets.el" nil nil)
                       ("doom-snippets-lib.el" ";;; doom-snippets-lib.el -*- lexical-binding: t; -*-\n\n(eval-when-compile\n  (require 'cl-lib))\n(eval-and-compile\n  (require 'yasnippet))\n\n;; Simpler `yas-selected-text' alias for templates\n(defvaralias '% 'yas-selected-text)\n(when doom-snippets-enable-short-helpers\n  (defalias '!%! 'doom-snippets-newline-selected-newline)\n  (defalias '!% 'doom-snippets-newline-selected)\n  (defalias '%$ 'doom-snippets-newline-or-eol)\n  (defalias '%without-trigger 'doom-snippets-without-trigger)\n  (defalias '%format 'doom-snippets-format)\n  (defalias '%expand 'doom-snippets-expand))\n\n\n(defmacro doom-snippets-without-trigger (&rest body)\n  \"Preform BODY after moving over the trigger keyword.\nWithout this, tests like `bolp' would meaninglessly fail because the cursor is\nalways in front of the word that triggered this snippet.\"\n  `(progn\n     (unless (memq (char-before) (list ?\\  ?\\n))\n       (backward-word))\n     ,@body))\n\n(defun doom-snippets-text (&optional default trim)\n  \"Return `yas-selected-text' (or `default').\n\nIf TRIM is non-nil, trim leading and trailing whitespace from\n`yas-selected-text'/`default'.\"\n  (let ((text (or yas-selected-text default \"\")))\n    (if trim\n        (string-trim text)\n      text)))\n\n(defun doom-snippets-format (format &optional default trim)\n  \"Returns a formatted string.\n\nLike `format', but with a custom spec:\n\n  %s  The contents of your current selection (`yas-selected-text`)\n  %n  A newline, if your current selection spans more than a single line\n  %e  A newline, unless the point is at EOL\n\nIf `yas-selected-text` is empty, `DEFAULT` is used.\n\nIf `TRIM` is non-nil, whitespace on the ends of `yas-selected-text` is\ntrimmed.\"\n  (let* ((text (or yas-selected-text default \"\"))\n         (text (if trim (string-trim text) text)))\n    (format-spec format\n                 `((?s . ,text)\n                   (?n . ,(if (> (doom-snippets-count-lines text) 1) \"\\n\" \"\"))\n                   (?e . ,(if (eolp) \"\" \"\\n\"))\n                   ))))\n\n(defun doom-snippets-newline-selected-newline ()\n  \"Return `yas-selected-text' surrounded with newlines if it consists of more\nthan one line.\"\n  (doom-snippets-format \"%n%s%n\" nil t))\n\n(defun doom-snippets-newline-selected ()\n  \"Return `yas-selected-text' prefixed with a newline if it consists of more\nthan one line.\"\n  (doom-snippets-format \"%n%s\" nil t))\n\n(defun doom-snippets-newline-or-eol ()\n  \"Return newline, unless at `eolp'.\"\n  (doom-snippets-format \"%e\"))\n\n(defun doom-snippets-count-lines (str)\n  \"Return how many lines are in STR.\"\n  (if (and (stringp str)\n           (not (string-empty-p str)))\n      (length (split-string str \"\\\\(\\r\\n\\\\|[\\n\\r]\\\\)\"))\n    0))\n\n(defun doom-snippets-bolp ()\n  \"Return t if point is preceded only by indentation.\n\nUnlike `bolp', this ignores the trigger word for the current snippet.\"\n  (or (bolp)\n      (save-excursion\n        (if (region-active-p)\n            (goto-char (region-beginning))\n          (unless (memq (char-before) (list ?\\  ?\\n))\n            (backward-word)))\n        (skip-chars-backward \" \\t\")\n        (bolp))))\n\n(defun doom-snippets-expand (property value &optional mode)\n  \"Expand a snippet whose PROPERTY equals VALUE in MODE.\n\nPROPERTY can be one of :uuid, :name, :key, or :file, and VALUE must be the\nuuid/name/key/(absolute) filepath of the template you want to expand.\"\n  (let ((snippet (let ((yas-choose-tables-first nil) ; avoid prompts\n                       (yas-choose-keys-first nil))\n                   (cl-loop for tpl in (yas--all-templates\n                                        (yas--get-snippet-tables mode))\n                            if (string= value\n                                        (pcase property\n                                          (:uuid (yas--template-uuid tpl))\n                                          (:name (yas--template-name tpl))\n                                          (:key  (yas--template-key tpl))\n                                          (:file (yas--template-load-file tpl))))\n                            return tpl))))\n    (if snippet\n        (yas-expand-snippet snippet)\n      (error \"Couldn't find %S snippet\" value))))\n\n(provide 'doom-snippets-lib)\n;;; doom-snippets-lib.el ends here\n" "doom-snippets-lib.el" nil nil nil "/home/orion/.config/doom/snippets/snippets/doom-snippets-lib.el" nil nil)
                       ("README.md" "class ${1:Name} {\n    $0\n}\n```\n\n**In `js-mode/cl`:**\n\n```yasnippet\n# name: class\n# key: cl\n# type: command\n# --\n(doom-snippets-expand :name \"class\")\n```\n\n### `doom-snippets-format FORMAT &optional DEFAULT TRIM`\n\nReturns `FORMAT`, which is a format string with a custom spec:\n\n| spec | description                                                        |\n|------|--------------------------------------------------------------------|\n| %s   | The contents of your current selection (`yas-selected-text`)       |\n| %!   | A newline, if your current selection spans more than a single line |\n| %>   | A newline, unless the point is at EOL                              |\n\n+ If `yas-selected-text` is empty, `DEFAULT` is used.\n+ If `TRIM` is non-nil, whitespace on the ends of `yas-selected-text` is\n  trimmed.\n  \nAn example of its use:\n\n```yasnippet\n# -*- mode: snippet -*-\n# name: while ... { ... }\n# key: while\n# --\nwhile ${1:true} { `(doom-snippets-format \"%n%s%n\")`$0 }\n```\n\nIf the selection consists of a single line, this will expand to:\n\n``` javascript\nwhile true { console.log(\"Hello world\")| }\n```\n\nIf it consists of multiple lines, it will expand to:\n\n``` javascript\nwhile true { \n  console.log(\"Hello\")\n  console.log(\"world\")| \n}\n```\n\n`PROPERTY` can be `:uuid`, `:name`, `:key` or `:file`, and `MODE` restricts the\nsnippet search to a certain snippet table (by major mode). It isn't wise to use\n`MODE` to reference snippets for other major modes, because it will only see\nsnippets that yasnippet have already loaded (and it lazy loads each table).\n\n### `doom-snippets-without-trigger &rest BODY`\n\nPerforms `BODY` after moving the point to the start of the trigger keyword.\n\nWithout this, tests like `bolp` would meaninglessly fail because the cursor is\nalways in front of the word that triggered this snippet.\n\n``` yasnippet\n# -*- mode: snippet -*-\n# name: .to_string()\n# key: ts\n# condition: (doom-snippets-without-trigger (eq (char-before) ?.))\n# --\nto_string()\n```\n\n### `doom-snippets-enable-short-helpers`\n\nIf this variable is non-nil, this package will define the following shortcut\nfunction aliases for your convenience:\n\n+ `!%!` = `(doom-snippets-format \"%n%s%n\")`\n+ `!%` = `(doom-snippets-format \"%n%s\")`\n+ `%$` = `(doom-snippets-format \"%>\")`\n+ `(%expand ...)` = `(doom-snippets-expand ...)`\n+ `(%format ...)` = `(doom-snippets-format ...)`\n+ `(%without-trigger ...)` = `(doom-snippets-without-trigger ...)`\n\n\n[yasnippet]: https://github.com/capitaomorte/yasnippet\n[Doom Emacs]: https://github.com/doomemacs/doomemacs\n" "class" nil nil nil "/home/orion/.config/doom/snippets/snippets/README.md" nil nil)
                       ("LICENSE" "The MIT License (MIT)\n\nCopyright (c) 2016-2022 Henrik Lissner.\n\nPermission is hereby granted, free of charge, to any person obtaining\na copy of this software and associated documentation files (the\n\"Software\"), to deal in the Software without restriction, including\nwithout limitation the rights to use, copy, modify, merge, publish,\ndistribute, sublicense, and/or sell copies of the Software, and to\npermit persons to whom the Software is furnished to do so, subject to\nthe following conditions:\n\nThe above copyright notice and this permission notice shall be\nincluded in all copies or substantial portions of the Software.\n\nTHE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND,\nEXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF\nMERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.\nIN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY\nCLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,\nTORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE\nSOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.\n" "LICENSE" nil nil nil "/home/orion/.config/doom/snippets/snippets/LICENSE" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("deftest" "(ert-deftest $1 ()`(doom-snippets-format \"%n%s\")`$0)" "ert-deftest" nil nil nil "/home/orion/.config/doom/snippets/snippets/+emacs-lisp-ert-mode/ert-deftest" nil "deftest")
                       ("deft"
                        (progn
                          (doom-snippets-expand :uuid "ert-deftest"))
                        "ert-deftest" nil nil nil "/home/orion/.config/doom/snippets/snippets/+emacs-lisp-ert-mode/deft" nil "deft")))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("ts-angular" "const util       = require(\"gulp-util\"),\n      browserify = require(\"browserify\"),\n      source     = require(\"vinyl-source-stream\"),\n      buffer     = require(\"vinyl-buffer\"),\n      uglify     = require(\"gulp-uglify\");\n\ngulp.task(\"ts\", () => {\n    return browserify({ entries: ${1:dir.source} + \"app.ts\", debug: true })\n        .plugin(tsify)\n        .external(\"@angular/platform-browser-dynamic\")\n        .bundle().on(\"error\", err => { console.error(err.toString()); })\n        .pipe(source(\"app.js\"))\n        .pipe(buffer())\n        .pipe(util.env.pack ? uglify() : util.noop())\n        .pipe(gulp.dest(${2:dir.build} + \"/app/\"));\n});" "Typescript (w/ Angular) compilation task" nil nil nil "/home/orion/.config/doom/snippets/snippets/+javascript-gulp-mode/ts-angular" nil nil)
                       ("ts" "const util       = require(\"gulp-util\"),\n      browserify = require(\"browserify\"),\n      source     = require(\"vinyl-source-stream\"),\n      buffer     = require(\"vinyl-buffer\"),\n      uglify     = require(\"gulp-uglify\");\n\ngulp.task(\"ts\", () => {\n    return browserify({ entries: ${1:dir.source} + \"app.ts\", debug: true })\n        .plugin(tsify)\n        .bundle().on(\"error\", err => { console.error(err.toString()); })\n        .pipe(source(\"app.js\"))\n        .pipe(buffer())\n        .pipe(util.env.pack ? uglify() : util.noop())\n        .pipe(gulp.dest(${2:dir.build} + \"/app/\"));\n});" "Typescript compilation task" nil nil nil "/home/orion/.config/doom/snippets/snippets/+javascript-gulp-mode/ts" nil nil)
                       ("task" "gulp.task('${1:task-name}', ($2) => {\n    `%`$0\n});" "Gulp task" nil nil nil "/home/orion/.config/doom/snippets/snippets/+javascript-gulp-mode/task" nil nil)
                       ("tape" "const tape   = require(\"gulp-tape\"),\n      faucet = require(\"faucet\");\n\ngulp.task(\"test\", () => {\n    return gulp.src(\"test/**/test_*.js\")\n        .pipe(tape({reporter: faucet()}))\n});" "Tape test-runner task" nil nil nil "/home/orion/.config/doom/snippets/snippets/+javascript-gulp-mode/tape" nil nil)
                       ("sass" "const sass    = require(\"gulp-sass\"),\n      bourbon = require(\"node-bourbon\").includePaths;\n\ngulp.task(\"sass\", () => {\n    const dest = ${1:dir.build} + \"/css\";\n\n    return gulp.src(\"stylesheets/**.scss\", {cwd: ${2:dir.source}})\n        .pipe(changed(dest, {extension: \".scss\"}))\n        .pipe(sass({\n                includePaths: bourbon,\n                outputStyle: \"compressed\",\n                sourcemap: true\n            }).on(\"error\", sass.logError))\n        .pipe(gulp.dest(dest))\n        .pipe(brsync.stream());\n});" "SASS compilation task" nil nil nil "/home/orion/.config/doom/snippets/snippets/+javascript-gulp-mode/sass" nil nil)
                       ("pug" "const pug     = require(\"gulp-pug\"),\n      changed = require(\"gulp-changed\");\n\ngulp.task(\"pug\", () => {\n    let locals = {locals: {release: util.env.release}};\n\n    return gulp.src(\"views/**/*.pug\", {cwd: ${1:dir.source}, base: $1})\n        .pipe(changed(${2:dir.build}, {extension: \".pug\"}))\n        .pipe(pug(locals))\n        .pipe(gulp.dest(dir.build));\n});" "Pug compilation task" nil nil nil "/home/orion/.config/doom/snippets/snippets/+javascript-gulp-mode/pug" nil nil)
                       ("es6" "const util       = require(\"gulp-util\"),\n      uglify     = require(\"gulp-uglify\"),\n      browserify = require(\"browserify\"),\n      source     = require(\"vinyl-source-stream\"),\n      buffer     = require(\"vinyl-buffer\");\n\ngulp.task(\"js\", () => {\n    return browserify({\n            entries: dir.source + \"main.js\",\n            debug: !util.env.pack,\n            detectGlobals: false,\n            paths: [dir.source]\n        })\n        .transform(\"babelify\", {presets: [\"es2015\"], ignore: util.env.pack ? null : /.*/})\n        .bundle()\n        .pipe(source(\"main.js\"))\n        .pipe(buffer())\n        .pipe(util.env.pack ? uglify({mangle: {keep_fnames: true}}) : util.noop())\n        .pipe(gulp.dest(dir.build));\n});" "ES6+Babel compilation task" nil nil nil "/home/orion/.config/doom/snippets/snippets/+javascript-gulp-mode/es6" nil nil)
                       ("default" "gulp.task('default', $0);" "Default gulp task" nil nil nil "/home/orion/.config/doom/snippets/snippets/+javascript-gulp-mode/default" nil nil)
                       ("browsersync" "const brsync = require(\"browser-sync\").create();\n\ngulp.task(\"serve\", () => {\n    brsync.init({ server: { baseDir: \"./\" } });\n\n    // TODO add browserSync\n    gulp.watch(\"**/*.pug\", [\"pug\"]);\n    gulp.watch(\"sass/**/*.scss\", [\"sass\"]);\n    gulp.watch(\"**/*.html\").on('change', brsync.reload);\n});" "Browsersync task" nil nil nil "/home/orion/.config/doom/snippets/snippets/+javascript-gulp-mode/browsersync" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("preferences" "Action.preferences.${1:name}" "Action.preferences.?" nil nil nil "/home/orion/.config/doom/snippets/snippets/+javascript-lb6-mode/preferences" nil nil)
                       ("lbupdate" "<key>LBChangelog</key>\n<string>$1</string>\n\n<key>LBUpdateURL</key>\n<string>https://raw.githubusercontent.com/hlissner/lb6-${2:actions}/master/${3:Name of Action}.lbaction/Contents/Info.plist</string>\n\n<key>LBDownloadURL</key>\n<string>https://dl.v0.io/launchbar/$3.lbaction</string>\n\n" "lbupdate"
                        (eq major-mode 'nxml-mode)
                        nil nil "/home/orion/.config/doom/snippets/snippets/+javascript-lb6-mode/lbupdate" nil "lbupdate")
                       ("include" "include(\"${1:shared/lib/${2:lib.js}}\");" "include(...)" nil nil nil "/home/orion/.config/doom/snippets/snippets/+javascript-lb6-mode/include" nil nil)
                       ("LBSuggestionsScript" "<key>LBSuggestionsScript</key>\n<dict>\n    <key>LBScriptName</key>\n    <string>${1:suggestions}.js</string>\n\n    <key>LBBackgroundKillEnabled</key>\n    <true />\n</dict>" "LBSuggestionsScript" nil nil nil "/home/orion/.config/doom/snippets/snippets/+javascript-lb6-mode/LBSuggestionsScript" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("love.update" "function love.update(dt)\n    `%`${0:-- update code}\nend\n" "love.update" love-mode nil nil "/home/orion/.config/doom/snippets/snippets/+lua-love-mode/love.update" nil nil)
                       ("love.load" "function love.load()\n    `%`${0:-- load code}\nend\n" "love.load" love-mode nil nil "/home/orion/.config/doom/snippets/snippets/+lua-love-mode/love.load" nil nil)
                       ("love.keyboard.isDown" "love.keyboard.isDown(${1:key})\n" "love.keyboard.isDown(key)" love-mode nil nil "/home/orion/.config/doom/snippets/snippets/+lua-love-mode/love.keyboard.isDown" nil nil)
                       ("love.draw" "function love.draw(dt)\n    `%`${0:-- draw code}\nend\n" "love.draw" love-mode nil nil "/home/orion/.config/doom/snippets/snippets/+lua-love-mode/love.draw" nil nil)
                       ("love.conf" "function love.conf(t)\n    t.identity = nil                   -- The name of the save directory (string)\n    t.version = \"0.9.1\"                -- The LÖVE version this game was made for (string)\n    t.console = false                  -- Attach a console (boolean, Windows only)\n\n    t.window.title = \"Untitled\"        -- The window title (string)\n    t.window.icon = nil                -- Filepath to an image to use as the window's icon (string)\n    t.window.width = 800               -- The window width (number)\n    t.window.height = 600              -- The window height (number)\n    t.window.borderless = false        -- Remove all border visuals from the window (boolean)\n    t.window.resizable = false         -- Let the window be user-resizable (boolean)\n    t.window.minwidth = 1              -- Minimum window width if the window is resizable (number)\n    t.window.minheight = 1             -- Minimum window height if the window is resizable (number)\n    t.window.fullscreen = false        -- Enable fullscreen (boolean)\n    t.window.fullscreentype = \"normal\" -- Standard fullscreen or desktop fullscreen mode (string)\n    t.window.vsync = true              -- Enable vertical sync (boolean)\n    t.window.fsaa = 0                  -- The number of samples to use with multi-sampled antialiasing (number)\n    t.window.display = 1               -- Index of the monitor to show the window in (number)\n    t.window.highdpi = false           -- Enable high-dpi mode for the window on a Retina display (boolean). Added in 0.9.1\n    t.window.srgb = false              -- Enable sRGB gamma correction when drawing to the screen (boolean). Added in 0.9.1\n\n    t.modules.audio = true             -- Enable the audio module (boolean)\n    t.modules.event = true             -- Enable the event module (boolean)\n    t.modules.graphics = true          -- Enable the graphics module (boolean)\n    t.modules.image = true             -- Enable the image module (boolean)\n    t.modules.joystick = true          -- Enable the joystick module (boolean)\n    t.modules.keyboard = true          -- Enable the keyboard module (boolean)\n    t.modules.math = true              -- Enable the math module (boolean)\n    t.modules.mouse = true             -- Enable the mouse module (boolean)\n    t.modules.physics = true           -- Enable the physics module (boolean)\n    t.modules.sound = true             -- Enable the sound module (boolean)\n    t.modules.system = true            -- Enable the system module (boolean)\n    t.modules.timer = true             -- Enable the timer module (boolean)\n    t.modules.window = true            -- Enable the window module (boolean)\n    t.modules.thread = true            -- Enable the thread module (boolean)\nend\n" "love.conf" love-mode nil nil "/home/orion/.config/doom/snippets/snippets/+lua-love-mode/love.conf" nil nil)
                       ("love" "function love.${1:update}($2)\n    `%`${0:-- code}\nend\n" "love.? function" love-mode nil nil "/home/orion/.config/doom/snippets/snippets/+lua-love-mode/love" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("route_inline" "Route::${1:get}('${2:/}', '${3:Controller@Action}')->name('${4:action}');" "Route::<req>(<url>, <route>)->name(<route>)" nil nil nil "/home/orion/.config/doom/snippets/snippets/+php-laravel-mode/route_inline" nil nil)
                       ("route" "Route::${1:get}('${2:/}', function () {\n    `%`$0\n});" "Route::<req>(<url>, <route>)" nil nil nil "/home/orion/.config/doom/snippets/snippets/+php-laravel-mode/route" nil nil)
                       ("nroute" "Route::${1:get}('${2:/}', ['as' => '${3:name}', function () {\n    `%`$0\n});" "Route::<req>(<url>, [..., <route>])" nil nil nil "/home/orion/.config/doom/snippets/snippets/+php-laravel-mode/nroute" nil nil)
                       ("mig" "Schema::table('$1', function (Blueprint $table) {\n    `%`$0\n});\n" "Laravel Migration method" nil nil nil "/home/orion/.config/doom/snippets/snippets/+php-laravel-mode/migration" nil "mig")
                       ("match" "Route::match([${1:'get', 'post'}], '${2:/}', function () {\n    `%`$0\n});" "Route::match([...], <func>)" nil nil nil "/home/orion/.config/doom/snippets/snippets/+php-laravel-mode/match" nil nil)
                       ("group" "Route::group([$1], function() {\n    `%`$0\n});" "Route::group([<attrs>], <func>)" nil nil nil "/home/orion/.config/doom/snippets/snippets/+php-laravel-mode/group" nil nil)
                       ("any" "Route::any('${1:/}', function () {\n    `%`$0\n});" "Route::any(<uri>, <func>)" nil nil nil "/home/orion/.config/doom/snippets/snippets/+php-laravel-mode/any" nil nil)
                       ("__" "<?php\n\nnamespace `(+php-laravel-mode--get-namespace)`;\n\nclass `(+php-laravel-mode--get-class-name)`\n{\n    $0\n}" "PHP template" nil nil nil "/home/orion/.config/doom/snippets/snippets/+php-laravel-mode/__" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("when" "when\n" "when"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/when" nil nil)
                       ("uppercase" "uppercase\n" "uppercase"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/uppercase" nil nil)
                       ("toJson" "toJson\n" "toJson"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/toJson" nil nil)
                       ("service" "service\n" "service"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/service" nil nil)
                       ("run" "run\n" "run"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/run" nil nil)
                       ("reloadWithDebugInfo" "reloadWithDebugInfo\n" "reloadWithDebugInfo"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/reloadWithDebugInfo" nil nil)
                       ("provider" "provider\n" "provider"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/provider" nil nil)
                       ("otherwise" "otherwise\n" "otherwise"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/otherwise" nil nil)
                       ("noop" "noop\n" "noop"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/noop" nil nil)
                       ("ngResource" "ngResource\n" "ngResource"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ngResource" nil nil)
                       ("ng-view" "ng-view\n" "ng-view"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-view" nil nil)
                       ("ng-value" "ng-value\n" "ng-value"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-value" nil nil)
                       ("ng-valid" "ng-valid\n" "ng-valid"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-valid" nil nil)
                       ("ng-true-value" "ng-true-value\n" "ng-true-value"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-true-value" nil nil)
                       ("ng-trim" "ng-trim\n" "ng-trim"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-trim" nil nil)
                       ("ng-transclude" "ng-transclude\n" "ng-transclude"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-transclude" nil nil)
                       ("ng-switch-when" "ng-switch-when\n" "ng-switch-when"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-switch-when" nil nil)
                       ("ng-switch-default" "ng-switch-default\n" "ng-switch-default"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-switch-default" nil nil)
                       ("ng-switch" "ng-switch\n" "ng-switch"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-switch" nil nil)
                       ("ng-swipe-right" "ng-swipe-right\n" "ng-swipe-right"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-swipe-right" nil nil)
                       ("ng-swipe-left" "ng-swipe-left\n" "ng-swipe-left"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-swipe-left" nil nil)
                       ("ng-submit" "ng-submit\n" "ng-submit"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-submit" nil nil)
                       ("ng-style" "ng-style\n" "ng-style"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-style" nil nil)
                       ("ng-srcset" "ng-srcset\n" "ng-srcset"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-srcset" nil nil)
                       ("ng-src" "ng-src\n" "ng-src"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-src" nil nil)
                       ("ng-show" "ng-show\n" "ng-show"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-show" nil nil)
                       ("ng-selected" "ng-selected\n" "ng-selected"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-selected" nil nil)
                       ("ng-required" "ng-required\n" "ng-required"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-required" nil nil)
                       ("ng-repeat-start" "ng-repeat-start\n" "ng-repeat-start"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-repeat-start" nil nil)
                       ("ng-repeat-end" "ng-repeat-end\n" "ng-repeat-end"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-repeat-end" nil nil)
                       ("ng-repeat" "ng-repeat\n" "ng-repeat"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-repeat" nil nil)
                       ("ng-readonly" "ng-readonly\n" "ng-readonly"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-readonly" nil nil)
                       ("ng-pristine" "ng-pristine\n" "ng-pristine"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-pristine" nil nil)
                       ("ng-pluralize" "ng-pluralize\n" "ng-pluralize"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-pluralize" nil nil)
                       ("ng-pattern" "ng-pattern\n" "ng-pattern"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-pattern" nil nil)
                       ("ng-paste" "ng-paste\n" "ng-paste"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-paste" nil nil)
                       ("ng-options" "ng-options\n" "ng-options"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-options" nil nil)
                       ("ng-open" "ng-open\n" "ng-open"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-open" nil nil)
                       ("ng-non-bindable" "ng-non-bindable\n" "ng-non-bindable"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-non-bindable" nil nil)
                       ("ng-mouseup" "ng-mouseup\n" "ng-mouseup"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-mouseup" nil nil)
                       ("ng-mouseover" "ng-mouseover\n" "ng-mouseover"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-mouseover" nil nil)
                       ("ng-mousemove" "ng-mousemove\n" "ng-mousemove"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-mousemove" nil nil)
                       ("ng-mouseleave" "ng-mouseleave\n" "ng-mouseleave"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-mouseleave" nil nil)
                       ("ng-mouseenter" "ng-mouseenter\n" "ng-mouseenter"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-mouseenter" nil nil)
                       ("ng-mousedown" "ng-mousedown\n" "ng-mousedown"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-mousedown" nil nil)
                       ("ng-model-options" "ng-model-options\n" "ng-model-options"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-model-options" nil nil)
                       ("ng-model" "ng-model\n" "ng-model"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-model" nil nil)
                       ("ng-minlength" "ng-minlength\n" "ng-minlength"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-minlength" nil nil)
                       ("ng-messages" "ng-messages\n" "ng-messages"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-messages" nil nil)
                       ("ng-message" "ng-message\n" "ng-message"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-message" nil nil)
                       ("ng-maxlength" "ng-maxlength\n" "ng-maxlength"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-maxlength" nil nil)
                       ("ng-list" "ng-list\n" "ng-list"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-list" nil nil)
                       ("ng-keyup" "ng-keyup\n" "ng-keyup"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-keyup" nil nil)
                       ("ng-keypress" "ng-keypress\n" "ng-keypress"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-keypress" nil nil)
                       ("ng-keydown" "ng-keydown\n" "ng-keydown"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-keydown" nil nil)
                       ("ng-invalid" "ng-invalid\n" "ng-invalid"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-invalid" nil nil)
                       ("ng-init" "ng-init\n" "ng-init"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-init" nil nil)
                       ("ng-include" "ng-include\n" "ng-include"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-include" nil nil)
                       ("ng-if" "ng-if\n" "ng-if"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-if" nil nil)
                       ("ng-href" "ng-href\n" "ng-href"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-href" nil nil)
                       ("ng-hide" "ng-hide\n" "ng-hide"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-hide" nil nil)
                       ("ng-form" "ng-form\n" "ng-form"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-form" nil nil)
                       ("ng-focus" "ng-focus\n" "ng-focus"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-focus" nil nil)
                       ("ng-false-value" "ng-false-value\n" "ng-false-value"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-false-value" nil nil)
                       ("ng-disabled" "ng-disabled\n" "ng-disabled"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-disabled" nil nil)
                       ("ng-dirty" "ng-dirty\n" "ng-dirty"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-dirty" nil nil)
                       ("ng-dblclick" "ng-dblclick\n" "ng-dblclick"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-dblclick" nil nil)
                       ("ng-cut" "ng-cut\n" "ng-cut"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-cut" nil nil)
                       ("ng-csp" "ng-csp\n" "ng-csp"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-csp" nil nil)
                       ("ng-copy" "ng-copy\n" "ng-copy"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-copy" nil nil)
                       ("ng-controller" "ng-controller\n" "ng-controller"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-controller" nil nil)
                       ("ng-cloak" "ng-cloak\n" "ng-cloak"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-cloak" nil nil)
                       ("ng-click" "ng-click\n" "ng-click"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-click" nil nil)
                       ("ng-class-odd" "ng-class-odd\n" "ng-class-odd"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-class-odd" nil nil)
                       ("ng-class-even" "ng-class-even\n" "ng-class-even"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-class-even" nil nil)
                       ("ng-class" "ng-class\n" "ng-class"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-class" nil nil)
                       ("ng-checked" "ng-checked\n" "ng-checked"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-checked" nil nil)
                       ("ng-change" "ng-change\n" "ng-change"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-change" nil nil)
                       ("ng-blur" "ng-blur\n" "ng-blur"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-blur" nil nil)
                       ("ng-bind-template" "ng-bind-template\n" "ng-bind-template"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-bind-template" nil nil)
                       ("ng-bind-html" "ng-bind-html\n" "ng-bind-html"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-bind-html" nil nil)
                       ("ng-bind" "ng-bind\n" "ng-bind"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-bind" nil nil)
                       ("ng-app" "ng-app\n" "ng-app"
                        (memq major-mode
                              '(web-mode html-mode jade-mode jaded-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/ng-app" nil nil)
                       ("module" "module\n" "module"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/module" nil nil)
                       ("mock" "mock\n" "mock"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/mock" nil nil)
                       ("lowercase" "lowercase\n" "lowercase"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/lowercase" nil nil)
                       ("isUndefined" "isUndefined\n" "isUndefined"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/isUndefined" nil nil)
                       ("isString" "isString\n" "isString"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/isString" nil nil)
                       ("isObject" "isObject\n" "isObject"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/isObject" nil nil)
                       ("isNumber" "isNumber\n" "isNumber"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/isNumber" nil nil)
                       ("isFunction" "isFunction\n" "isFunction"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/isFunction" nil nil)
                       ("isElement" "isElement\n" "isElement"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/isElement" nil nil)
                       ("isDefined" "isDefined\n" "isDefined"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/isDefined" nil nil)
                       ("isDate" "isDate\n" "isDate"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/isDate" nil nil)
                       ("isArray" "isArray\n" "isArray"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/isArray" nil nil)
                       ("injector" "injector\n" "injector"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/injector" nil nil)
                       ("inject" "inject\n" "inject"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/inject" nil nil)
                       ("identity" "identity\n" "identity"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/identity" nil nil)
                       ("fromJson" "fromJson\n" "fromJson"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/fromJson" nil nil)
                       ("forEach" "forEach\n" "forEach"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/forEach" nil nil)
                       ("filter" "filter\n" "filter"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/filter" nil nil)
                       ("factory" "factory\n" "factory"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/factory" nil nil)
                       ("extend" "extend\n" "extend"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/extend" nil nil)
                       ("equals" "equals\n" "equals"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/equals" nil nil)
                       ("element" "element\n" "element"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/element" nil nil)
                       ("dump" "dump\n" "dump"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/dump" nil nil)
                       ("directive" "directive\n" "directive"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/directive" nil nil)
                       ("defer" "defer\n" "defer"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/defer" nil nil)
                       ("copy" "copy\n" "copy"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/copy" nil nil)
                       ("controller" "controller\n" "controller"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/controller" nil nil)
                       ("config" "config\n" "config"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/config" nil nil)
                       ("bootstrap" "bootstrap\n" "bootstrap"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/bootstrap" nil nil)
                       ("bind" "bind\n" "bind"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/bind" nil nil)
                       ("angular" "angular\n" "angular"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/angular" nil nil)
                       ("_" "angular\nbind\nbootstrap\ncopy\nelement\nequals\nextend\nforEach\nfromJson\nidentity\ninjector\nisArray\nisDate\nisDefined\nisElement\nisFunction\nisNumber\nisObject\nisString\nisUndefined\nlowercase\nmodule\nnoop\nreloadWithDebugInfo\ntoJson\nuppercase\n$anchorScroll\n$animate\n$cacheFactory\n$compile\n$controller\n$document\n$exceptionHandler\n$filter\n$http\n$httpBackend\n$interpolate\n$interval\n$locale\n$location\n$log\n$parse\n$q\n$rootElement\n$rootScope\n$sce\n$sceDelegate\n$templateCache\n$templateRequest\n$timeout\n$window\n$animateProvider\n$compileProvider\n$controllerProvider\n$filterProvider\n$httpProvider\n$interpolateProvider\n$locationProvider\n$logProvider\n$parseProvider\n$rootScopeProvider\n$sceDelegateProvider\n$sceProvider\n$injector\n$provide\n$ariaProvider\n$aria\n$cookieStore\n$cookies\nmock\n$exceptionHandlerProvider\nTzDate\ndump\ninject\n$resource\n$routeProvider\n$route\n$routeParams\n$sanitize\n$swipe\ncontroller\n$scope\nservice\nfactory\nprovider\nngResource\ndefer\nconfig\nwhen\notherwise\ndirective\nrun\nfilter" "_" nil nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/_" nil nil)
                       ("TzDate" "TzDate\n" "TzDate"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/TzDate" nil nil)
                       ("$window" "$window\n" "$window"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/$window" nil nil)
                       ("$timeout" "$timeout\n" "$timeout"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/$timeout" nil nil)
                       ("$templateRequest" "$templateRequest\n" "$templateRequest"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/$templateRequest" nil nil)
                       ("$templateCache" "$templateCache\n" "$templateCache"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/$templateCache" nil nil)
                       ("$swipe" "$swipe\n" "$swipe"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/$swipe" nil nil)
                       ("$scope" "$scope\n" "$scope"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/$scope" nil nil)
                       ("$sceProvider" "$sceProvider\n" "$sceProvider"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/$sceProvider" nil nil)
                       ("$sceDelegateProvider" "$sceDelegateProvider\n" "$sceDelegateProvider"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/$sceDelegateProvider" nil nil)
                       ("$sceDelegate" "$sceDelegate\n" "$sceDelegate"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/$sceDelegate" nil nil)
                       ("$sce" "$sce\n" "$sce"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/$sce" nil nil)
                       ("$sanitize" "$sanitize\n" "$sanitize"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/$sanitize" nil nil)
                       ("$routeProvider" "$routeProvider\n" "$routeProvider"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/$routeProvider" nil nil)
                       ("$routeParams" "$routeParams\n" "$routeParams"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/$routeParams" nil nil)
                       ("$route" "$route\n" "$route"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/$route" nil nil)
                       ("$rootScopeProvider" "$rootScopeProvider\n" "$rootScopeProvider"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/$rootScopeProvider" nil nil)
                       ("$rootScope" "$rootScope\n" "$rootScope"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/$rootScope" nil nil)
                       ("$rootElement" "$rootElement\n" "$rootElement"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/$rootElement" nil nil)
                       ("$resource" "$resource\n" "$resource"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/$resource" nil nil)
                       ("$q" "$q\n" "$q"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/$q" nil nil)
                       ("$provide" "$provide\n" "$provide"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/$provide" nil nil)
                       ("$parseProvider" "$parseProvider\n" "$parseProvider"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/$parseProvider" nil nil)
                       ("$parse" "$parse\n" "$parse"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/$parse" nil nil)
                       ("$logProvider" "$logProvider\n" "$logProvider"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/$logProvider" nil nil)
                       ("$log" "$log\n" "$log"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/$log" nil nil)
                       ("$locationProvider" "$locationProvider\n" "$locationProvider"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/$locationProvider" nil nil)
                       ("$location" "$location\n" "$location"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/$location" nil nil)
                       ("$locale" "$locale\n" "$locale"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/$locale" nil nil)
                       ("$interval" "$interval\n" "$interval"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/$interval" nil nil)
                       ("$interpolateProvider" "$interpolateProvider\n" "$interpolateProvider"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/$interpolateProvider" nil nil)
                       ("$interpolate" "$interpolate\n" "$interpolate"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/$interpolate" nil nil)
                       ("$injector" "$injector\n" "$injector"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/$injector" nil nil)
                       ("$httpProvider" "$httpProvider\n" "$httpProvider"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/$httpProvider" nil nil)
                       ("$httpBackend" "$httpBackend\n" "$httpBackend"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/$httpBackend" nil nil)
                       ("$http" "$http\n" "$http"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/$http" nil nil)
                       ("$filterProvider" "$filterProvider\n" "$filterProvider"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/$filterProvider" nil nil)
                       ("$filter" "$filter\n" "$filter"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/$filter" nil nil)
                       ("$exceptionHandlerProvider" "$exceptionHandlerProvider\n" "$exceptionHandlerProvider"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/$exceptionHandlerProvider" nil nil)
                       ("$exceptionHandler" "$exceptionHandler\n" "$exceptionHandler"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/$exceptionHandler" nil nil)
                       ("$document" "$document\n" "$document"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/$document" nil nil)
                       ("$cookies" "$cookies\n" "$cookies"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/$cookies" nil nil)
                       ("$cookieStore" "$cookieStore\n" "$cookieStore"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/$cookieStore" nil nil)
                       ("$controllerProvider" "$controllerProvider\n" "$controllerProvider"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/$controllerProvider" nil nil)
                       ("$controller" "$controller\n" "$controller"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/$controller" nil nil)
                       ("$compileProvider" "$compileProvider\n" "$compileProvider"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/$compileProvider" nil nil)
                       ("$compile" "$compile\n" "$compile"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/$compile" nil nil)
                       ("$cacheFactory" "$cacheFactory\n" "$cacheFactory"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/$cacheFactory" nil nil)
                       ("$ariaProvider" "$ariaProvider\n" "$ariaProvider"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/$ariaProvider" nil nil)
                       ("$aria" "$aria\n" "$aria"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/$aria" nil nil)
                       ("$animateProvider" "$animateProvider\n" "$animateProvider"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/$animateProvider" nil nil)
                       ("$animate" "$animate\n" "$animate"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/$animate" nil nil)
                       ("$anchorScroll" "$anchorScroll\n" "$anchorScroll"
                        (memq major-mode
                              '(js-mode js2-mode))
                        nil nil "/home/orion/.config/doom/snippets/snippets/+web-angularjs-mode/$anchorScroll" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("useState" "const [${1:state}, set${1:$(s-upper-camel-case yas-text)}] = useState(${2:initialState});\n$0" "useState" nil nil nil "/home/orion/.config/doom/snippets/snippets/+web-react-mode/useState" nil nil)
                       ("useEffect" "useEffect(() => {\n  $0\n});\n" "useEffect" nil nil nil "/home/orion/.config/doom/snippets/snippets/+web-react-mode/useEffect" nil nil)
                       ("shouldComponentUpdate" "shouldComponentUpdate(nextProps, nextState) {\n  $0\n}\n" "shouldComponentUpdate" nil nil nil "/home/orion/.config/doom/snippets/snippets/+web-react-mode/shouldComponentUpdate" nil nil)
                       ("rren" "ReactDOM.render(<${1:ComponentName} />, document.${2:body});" "ReactDOM.render(component, node)" nil nil nil "/home/orion/.config/doom/snippets/snippets/+web-react-mode/render" nil "rren")
                       ("reducer" "const initialState = {\n  $1\n};\n\nexport default (state = initialState, action) => {\n  switch (action.type) {\n    $0\n\n    default:\n      return state;\n  }\n};\n" "reducer" nil nil nil "/home/orion/.config/doom/snippets/snippets/+web-react-mode/reducer" nil nil)
                       ("mergeProps" "const mergeProps = (stateProps, dispatchProps, ownProps) => ({\n  ...stateProps,\n  ...dispatchProps,\n  ...ownProps,\n  $0\n});\n" "mergeProps" nil nil nil "/home/orion/.config/doom/snippets/snippets/+web-react-mode/mergeProps" nil nil)
                       ("getSnapshotBeforeUpdate" "static getSnapshotBeforeUpdate(prevProps, prevState) {\n  $0\n  return null;\n}\n" "getSnapshotBeforeUpdate" nil nil nil "/home/orion/.config/doom/snippets/snippets/+web-react-mode/getSnapshotBeforeUpdate" nil nil)
                       ("getDerivedStateFromProps" "static getDerivedStateFromProps(nextProps, prevState) {\n  $0\n  return null;\n}\n" "getDerivedStateFromProps" nil nil nil "/home/orion/.config/doom/snippets/snippets/+web-react-mode/getDerivedStateFromProps" nil nil)
                       ("container" "import { connect } from 'react-redux';\n\nconst mapStateToProps = (state, ownProps) => ({\n  $0\n});\n\nconst mapDispatchToProps = (dispatch, ownProps) => ({\n\n});\n\nconst ${1:`(f-base buffer-file-name)`} = connect(\n  mapStateToProps,\n  mapDispatchToProps\n)(${2:Component});\n\nexport default $1;\n" "Redux container" nil nil nil "/home/orion/.config/doom/snippets/snippets/+web-react-mode/container" nil nil)
                       ("componentWillUnmount" "componentWillUnmount() {\n  $0\n}\n" "componentWillUnmount" nil nil nil "/home/orion/.config/doom/snippets/snippets/+web-react-mode/componentWillUnmount" nil nil)
                       ("componentDidUpdate" "componentDidUpdate() {\n  $0\n}" "componentDidUpdate" nil nil nil "/home/orion/.config/doom/snippets/snippets/+web-react-mode/componentDidUpdate" nil nil)
                       ("componentDidMount" "componentDidMount() {\n  $0\n}" "componentDidMount" nil nil nil "/home/orion/.config/doom/snippets/snippets/+web-react-mode/componentDidMount" nil nil)
                       ("component-class" "import { Component } from 'react';\n\nclass ${1:`(f-base buffer-file-name)`} extends Component {\n  render() {\n    return (\n      $0\n    );\n  }\n}\n\nexport default $1;" "React component class" nil nil nil "/home/orion/.config/doom/snippets/snippets/+web-react-mode/component-class" nil nil)
                       ("component" "import { Component } from 'react';\n\nconst ${1:`(f-base buffer-file-name)`} = (props) => (\n  $0\n);\n\nexport default $1;" "React component" nil nil nil "/home/orion/.config/doom/snippets/snippets/+web-react-mode/component" nil nil)
                       ("action" "const ${1:$(upcase (s-snake-case yas-text))} = '${1:$(upcase (s-snake-case yas-text))}';\n\nexport const ${1:actionName} = (${2:args}) => ({\n  type: '${1:$(upcase (s-snake-case yas-text))}',\n  payload: {\n    $0\n  },\n});\n" "action" nil nil nil "/home/orion/.config/doom/snippets/snippets/+web-react-mode/action" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("a10_virtual_server" "- name: ${1:Manage A10 Networks AX/SoftAX/Thunder/vThunder devices}\n  a10_virtual_server: host=$2 username=$3 password=$4 virtual_server=$5 $0\n" "Manage A10 Networks AX/SoftAX/Thunder/vThunder devices" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/a10/a10_virtual_server" nil nil)
                       ("a10_service_group" "- name: ${1:Manage A10 Networks AX/SoftAX/Thunder/vThunder devices}\n  a10_service_group: host=$2 username=$3 password=$4 service_group=$5 $0\n" "Manage A10 Networks AX/SoftAX/Thunder/vThunder devices" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/a10/a10_service_group" nil nil)
                       ("a10_server" "- name: ${1:Manage A10 Networks AX/SoftAX/Thunder/vThunder devices}\n  a10_server: host=$2 username=$3 password=$4 server_name=$5 $0\n" "Manage A10 Networks AX/SoftAX/Thunder/vThunder devices" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/a10/a10_server" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("s3" "- name: ${1:S3 module putting a file into S3.}\n  s3: bucket=$2 mode=$3 $0\n" "S3 module putting a file into S3." ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/amazon/s3" nil nil)
                       ("route53" "- name: ${1:add or delete entries in Amazons Route53 DNS service}\n  route53: command=$2 zone=$3 record=$4 type=$5 $0\n" "add or delete entries in Amazons Route53 DNS service" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/amazon/route53" nil nil)
                       ("rds_subnet_group" "- name: ${1:manage RDS database subnet groups}\n  rds_subnet_group: state=${2:present} name=$3 region=$4 $0\n" "manage RDS database subnet groups" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/amazon/rds_subnet_group" nil nil)
                       ("rds_param_group" "- name: ${1:manage RDS parameter groups}\n  rds_param_group: state=${2:present} name=$3 region=$4 $0\n" "manage RDS parameter groups" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/amazon/rds_param_group" nil nil)
                       ("rds" "- name: ${1:create, delete, or modify an Amazon rds instance}\n  rds: command=$2 instance_name=$3 region=$4 $0\n" "create, delete, or modify an Amazon rds instance" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/amazon/rds" nil nil)
                       ("elasticache" "- name: ${1:Manage cache clusters in Amazon Elasticache.}\n  elasticache: state=$2 name=$3 $0\n" "Manage cache clusters in Amazon Elasticache." ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/amazon/elasticache" nil nil)
                       ("ec2_vpc" "- name: ${1:configure AWS virtual private clouds}\n  ec2_vpc: cidr_block=$2 resource_tags=$3 state=${4:present} $0\n" "configure AWS virtual private clouds" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/amazon/ec2_vpc" nil nil)
                       ("ec2_vol" "- name: ${1:create and attach a volume, return volume id and device map}\n  ec2_vol: $0\n" "create and attach a volume, return volume id and device map" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/amazon/ec2_vol" nil nil)
                       ("ec2_tag" "- name: ${1:create and remove tag(s) to ec2 resources.}\n  ec2_tag: resource=$2 $0\n" "create and remove tag(s) to ec2 resources." ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/amazon/ec2_tag" nil nil)
                       ("ec2_snapshot" "- name: ${1:creates a snapshot from an existing volume}\n  ec2_snapshot: $0\n" "creates a snapshot from an existing volume" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/amazon/ec2_snapshot" nil nil)
                       ("ec2_scaling_policy" "- name: ${1:Create or delete AWS scaling policies for Autoscaling groups}\n  ec2_scaling_policy: state=$2 name=$3 asg_name=$4 $0\n" "Create or delete AWS scaling policies for Autoscaling groups" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/amazon/ec2_scaling_policy" nil nil)
                       ("ec2_metric_alarm" "- name: ${1:Create/update or delete AWS Cloudwatch 'metric alarms'}\n  ec2_metric_alarm: state=$2 name=$3 $0\n" "Create/update or delete AWS Cloudwatch 'metric alarms'" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/amazon/ec2_metric_alarm" nil nil)
                       ("ec2_lc" "- name: ${1:Create or delete AWS Autoscaling Launch Configurations}\n  ec2_lc: state=$2 name=$3 instance_type=$4 $0\n" "Create or delete AWS Autoscaling Launch Configurations" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/amazon/ec2_lc" nil nil)
                       ("ec2_key" "- name: ${1:maintain an ec2 key pair.}\n  ec2_key: name=$2 $0\n" "maintain an ec2 key pair." ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/amazon/ec2_key" nil nil)
                       ("ec2_group" "- name: ${1:maintain an ec2 VPC security group.}\n  ec2_group: name=$2 description=$3 $0\n" "maintain an ec2 VPC security group." ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/amazon/ec2_group" nil nil)
                       ("ec2_facts" "- name: ${1:Gathers facts about remote hosts within ec2 (aws)}\n  ec2_facts: $0\n" "Gathers facts about remote hosts within ec2 (aws)" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/amazon/ec2_facts" nil nil)
                       ("ec2_elb_lb" "- name: ${1:Creates or destroys Amazon ELB.}\n  ec2_elb_lb: state=$2 name=$3 $0\n" "Creates or destroys Amazon ELB." ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/amazon/ec2_elb_lb" nil nil)
                       ("ec2_elb" "- name: ${1:De-registers or registers instances from EC2 ELBs}\n  ec2_elb: state=$2 instance_id=$3 $0\n" "De-registers or registers instances from EC2 ELBs" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/amazon/ec2_elb" nil nil)
                       ("ec2_eip" "- name: ${1:associate an EC2 elastic IP with an instance.}\n  ec2_eip: $0\n" "associate an EC2 elastic IP with an instance." ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/amazon/ec2_eip" nil nil)
                       ("ec2_asg" "- name: ${1:Create or delete AWS Autoscaling Groups}\n  ec2_asg: state=$2 name=$3 $0\n" "Create or delete AWS Autoscaling Groups" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/amazon/ec2_asg" nil nil)
                       ("ec2_ami_search" "- name: ${1:Retrieve AWS AMI for a given operating system.}\n  ec2_ami_search: distro=$2 release=$3 $0\n" "Retrieve AWS AMI for a given operating system." ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/amazon/ec2_ami_search" nil nil)
                       ("ec2_ami" "- name: ${1:create or destroy an image in ec2, return imageid}\n  ec2_ami: $0\n" "create or destroy an image in ec2, return imageid" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/amazon/ec2_ami" nil nil)
                       ("ec2" "- name: ${1:create, terminate, start or stop an instance in ec2, return instanceid}\n  ec2: instance_type=$2 image=$3 $0\n" "create, terminate, start or stop an instance in ec2, return instanceid" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/amazon/ec2" nil nil)
                       ("cloudformation" "- name: ${1:create a AWS CloudFormation stack}\n  cloudformation: stack_name=$2 state=$3 template=$4 $0\n" "create a AWS CloudFormation stack" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/amazon/cloudformation" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("azure" "- name: ${1:create or terminate a virtual machine in azure}\n  azure: name=$2 location=$3 storage_account=$4 image=$5 $0\n" "create or terminate a virtual machine in azure" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/azure/azure" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("uri" "- name: ${1:Interacts with webservices}\n  uri: url=$2 $0\n" "Interacts with webservices" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/basics/uri" nil nil)
                       ("slurp" "- name: ${1:Slurps a file from remote nodes}\n  slurp: src=$2 $0\n" "Slurps a file from remote nodes" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/basics/slurp" nil nil)
                       ("get_url" "- name: ${1:Downloads files from HTTP, HTTPS, or FTP to node}\n  get_url: url=$2 dest=$3 $0\n" "Downloads files from HTTP, HTTPS, or FTP to node" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/basics/get_url" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("netscaler" "- name: ${1:Manages Citrix NetScaler entities}\n  netscaler: nsc_host=$2 user=$3 password=$4 name=${5:hostname} $0\n" "Manages Citrix NetScaler entities" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/citrix/netscaler" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("shell" "- name: ${1:Execute commands in nodes.}\n  shell: free_form=$2 $0\n" "Execute commands in nodes." ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/commands/shell" nil nil)
                       ("script" "- name: ${1:Runs a local script on a remote node after transferring it}\n  script: free_form=$2 $0\n" "Runs a local script on a remote node after transferring it" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/commands/script" nil nil)
                       ("raw" "- name: ${1:Executes a low-down and dirty SSH command}\n  raw: free_form=$2 $0\n" "Executes a low-down and dirty SSH command" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/commands/raw" nil nil)
                       ("command" "- name: ${1:Executes a command on a remote node}\n  command: free_form=$2 $0\n" "Executes a command on a remote node" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/commands/command" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("digital_ocean_sshkey" "- name: ${1:Create/delete an SSH key in DigitalOcean}\n  digital_ocean_sshkey: $0\n" "Create/delete an SSH key in DigitalOcean" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/digital_ocean/digital_ocean_sshkey" nil nil)
                       ("digital_ocean_domain" "- name: ${1:Create/delete a DNS record in DigitalOcean}\n  digital_ocean_domain: $0\n" "Create/delete a DNS record in DigitalOcean" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/digital_ocean/digital_ocean_domain" nil nil)
                       ("digital_ocean" "- name: ${1:Create/delete a droplet/SSH_key in DigitalOcean}\n  digital_ocean: $0\n" "Create/delete a droplet/SSH_key in DigitalOcean" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/digital_ocean/digital_ocean" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("docker" "- name: ${1:manage docker containers}\n  docker: image=$2 $0\n" "manage docker containers" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/docker/docker" nil nil)
                       ("docker_image" "- name: ${1:manage docker images}\n  docker_image: name=$2 $0\n" "manage docker images" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/docker/_docker_image" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("bigip_pool_member" "- name: ${1:Manages F5 BIG-IP LTM pool members}\n  bigip_pool_member: server=$2 user=$3 password=$4 state=${5:present} pool=$6 host=$7 port=$8 $0\n" "Manages F5 BIG-IP LTM pool members" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/f5/bigip_pool_member" nil nil)
                       ("bigip_pool" "- name: ${1:Manages F5 BIG-IP LTM pools}\n  bigip_pool: server=$2 user=$3 password=$4 name=$5 $0\n" "Manages F5 BIG-IP LTM pools" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/f5/bigip_pool" nil nil)
                       ("bigip_node" "- name: ${1:Manages F5 BIG-IP LTM nodes}\n  bigip_node: server=$2 user=$3 password=$4 state=${5:present} host=$6 $0\n" "Manages F5 BIG-IP LTM nodes" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/f5/bigip_node" nil nil)
                       ("bigip_monitor_tcp" "- name: ${1:Manages F5 BIG-IP LTM tcp monitors}\n  bigip_monitor_tcp: server=$2 user=$3 password=$4 name=$5 send=${6:none} receive=${7:none} $0\n" "Manages F5 BIG-IP LTM tcp monitors" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/f5/bigip_monitor_tcp" nil nil)
                       ("bigip_monitor_http" "- name: ${1:Manages F5 BIG-IP LTM http monitors}\n  bigip_monitor_http: server=$2 user=$3 password=$4 name=$5 send=${6:none} receive=${7:none} receive_disable=${8:none} $0\n" "Manages F5 BIG-IP LTM http monitors" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/f5/bigip_monitor_http" nil nil)
                       ("bigip_facts" "- name: ${1:Collect facts from F5 BIG-IP devices}\n  bigip_facts: server=$2 user=$3 password=$4 include=$5 $0\n" "Collect facts from F5 BIG-IP devices" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/f5/bigip_facts" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("xattr" "- name: ${1:set/retrieve extended attributes}\n  xattr: name=${2:None} $0\n" "set/retrieve extended attributes" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/files/xattr" nil nil)
                       ("unarchive" "- name: ${1:Copies an archive to a remote location and unpack it}\n  unarchive: src=$2 dest=$3 $0\n" "Copies an archive to a remote location and unpack it" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/files/unarchive" nil nil)
                       ("template" "- name: ${1:Templates a file out to a remote server.}\n  template: src=$2 dest=$3 $0\n" "Templates a file out to a remote server." ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/files/template" nil nil)
                       ("synchronize" "- name: ${1:Uses rsync to make synchronizing file paths in your playbooks quick and easy.}\n  synchronize: src=$2 dest=$3 $0\n" "Uses rsync to make synchronizing file paths in your playbooks quick and easy." ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/files/synchronize" nil nil)
                       ("stat" "- name: ${1:retrieve file or file system status}\n  stat: path=$2 $0\n" "retrieve file or file system status" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/files/stat" nil nil)
                       ("replace" "- name: ${1:Replace all instances of a particular string in a file using a back-referenced regular expression.}\n  replace: dest=$2 regexp=$3 $0\n" "Replace all instances of a particular string in a file using a back-referenced regular expression." ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/files/replace" nil nil)
                       ("lineinfile" "- name: ${1:Ensure a particular line is in a file, or replace an existing line using a back-referenced regular expression.}\n  lineinfile: dest=$2 $0\n" "Ensure a particular line is in a file, or replace an existing line using a back-referenced regular expression." ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/files/lineinfile" nil nil)
                       ("ini_file" "- name: ${1:Tweak settings in INI files}\n  ini_file: dest=$2 section=$3 $0\n" "Tweak settings in INI files" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/files/ini_file" nil nil)
                       ("file" "- name: ${1:Sets attributes of files}\n  file: path=${2:[]} $0\n" "Sets attributes of files" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/files/file" nil nil)
                       ("fetch" "- name: ${1:Fetches a file from remote nodes}\n  fetch: src=$2 dest=$3 $0\n" "Fetches a file from remote nodes" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/files/fetch" nil nil)
                       ("copy" "- name: ${1:Copies files to remote locations.}\n  copy: dest=$2 $0\n" "Copies files to remote locations." ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/files/copy" nil nil)
                       ("assemble" "- name: ${1:Assembles a configuration file from fragments}\n  assemble: src=$2 dest=$3 $0\n" "Assembles a configuration file from fragments" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/files/assemble" nil nil)
                       ("acl" "- name: ${1:Sets and retrieves file ACL information.}\n  acl: name=$2 $0\n" "Sets and retrieves file ACL information." ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/files/acl" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("gce_pd" "- name: ${1:utilize GCE persistent disk resources}\n  gce_pd: name=$2 $0\n" "utilize GCE persistent disk resources" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/google/gce_pd" nil nil)
                       ("gce_net" "- name: ${1:create/destroy GCE networks and firewall rules}\n  gce_net: $0\n" "create/destroy GCE networks and firewall rules" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/google/gce_net" nil nil)
                       ("gce_lb" "- name: ${1:create/destroy GCE load-balancer resources}\n  gce_lb: $0\n" "create/destroy GCE load-balancer resources" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/google/gce_lb" nil nil)
                       ("gce" "- name: ${1:create or terminate GCE instances}\n  gce: zone=${2:us-central1-a} $0\n" "create or terminate GCE instances" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/google/gce" nil nil)
                       ("gc_storage" "- name: ${1:This module manages objects/buckets in Google Cloud Storage.}\n  gc_storage: bucket=$2 mode=$3 gcs_secret_key=$4 gcs_access_key=$5 $0\n" "This module manages objects/buckets in Google Cloud Storage." ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/google/gc_storage" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("fireball" "- name: ${1:Enable fireball mode on remote node}\n  fireball: $0\n" "Enable fireball mode on remote node" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/helper/fireball" nil nil)
                       ("accelerate" "- name: ${1:Enable accelerated mode on remote node}\n  accelerate: $0\n" "Enable accelerated mode on remote node" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/helper/accelerate" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("group_by" "- name: ${1:Create Ansible groups based on facts}\n  group_by: key=$2 $0\n" "Create Ansible groups based on facts" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/inventory/group_by" nil nil)
                       ("add_host" "- name: ${1:add a host (and alternatively a group) to the ansible-playbook in-memory inventory}\n  add_host: name=$2 $0\n" "add a host (and alternatively a group) to the ansible-playbook in-memory inventory" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/inventory/add_host" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("pip" "- name: ${1:Manages Python library dependencies.}\n  pip: $0\n" "Manages Python library dependencies." ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/language/pip" nil nil)
                       ("npm" "- name: ${1:Manage node.js packages with npm}\n  npm: $0\n" "Manage node.js packages with npm" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/language/npm" nil nil)
                       ("gem" "- name: ${1:Manage Ruby gems}\n  gem: name=$2 $0\n" "Manage Ruby gems" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/language/gem" nil nil)
                       ("easy_install" "- name: ${1:Installs Python libraries}\n  easy_install: name=$2 $0\n" "Installs Python libraries" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/language/easy_install" nil nil)
                       ("cpanm" "- name: ${1:Manages Perl library dependencies.}\n  cpanm: $0\n" "Manages Perl library dependencies." ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/language/cpanm" nil nil)
                       ("composer" "- name: ${1:Dependency Manager for PHP}\n  composer: working_dir=$2 $0\n" "Dependency Manager for PHP" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/language/composer" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("linode" "- name: ${1:create / delete / stop / restart an instance in Linode Public Cloud}\n  linode: $0\n" "create / delete / stop / restart an instance in Linode Public Cloud" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/linode/linode" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("wait_for" "- name: ${1:Waits for a condition before continuing.}\n  wait_for: $0\n" "Waits for a condition before continuing." ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/logic/wait_for" nil nil)
                       ("set_fact" "- name: ${1:Set host facts from a task}\n  set_fact: key_value=$2 $0\n" "Set host facts from a task" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/logic/set_fact" nil nil)
                       ("pause" "- name: ${1:Pause playbook execution}\n  pause: $0\n" "Pause playbook execution" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/logic/pause" nil nil)
                       ("include_vars" "- name: ${1:Load variables from files, dynamically within a task.}\n  include_vars: free-form=$2 $0\n" "Load variables from files, dynamically within a task." ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/logic/include_vars" nil nil)
                       ("fail" "- name: ${1:Fail with custom message}\n  fail: $0\n" "Fail with custom message" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/logic/fail" nil nil)
                       ("debug" "- name: ${1:Print statements during execution}\n  debug: $0\n" "Print statements during execution" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/logic/debug" nil nil)
                       ("async_status" "- name: ${1:Obtain status of asynchronous task}\n  async_status: jid=$2 $0\n" "Obtain status of asynchronous task" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/logic/async_status" nil nil)
                       ("assert" "- name: ${1:Fail with custom message}\n  assert: that=$2 $0\n" "Fail with custom message" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/logic/assert" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("rabbitmq_vhost" "- name: ${1:Manage the state of a virtual host in RabbitMQ}\n  rabbitmq_vhost: name=$2 $0\n" "Manage the state of a virtual host in RabbitMQ" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/messaging/rabbitmq_vhost" nil nil)
                       ("rabbitmq_user" "- name: ${1:Adds or removes users to RabbitMQ}\n  rabbitmq_user: user=$2 $0\n" "Adds or removes users to RabbitMQ" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/messaging/rabbitmq_user" nil nil)
                       ("rabbitmq_policy" "- name: ${1:Manage the state of policies in RabbitMQ.}\n  rabbitmq_policy: name=$2 pattern=$3 tags=$4 $0\n" "Manage the state of policies in RabbitMQ." ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/messaging/rabbitmq_policy" nil nil)
                       ("rabbitmq_plugin" "- name: ${1:Adds or removes plugins to RabbitMQ}\n  rabbitmq_plugin: names=$2 $0\n" "Adds or removes plugins to RabbitMQ" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/messaging/rabbitmq_plugin" nil nil)
                       ("rabbitmq_parameter" "- name: ${1:Adds or removes parameters to RabbitMQ}\n  rabbitmq_parameter: component=$2 name=$3 $0\n" "Adds or removes parameters to RabbitMQ" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/messaging/rabbitmq_parameter" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("virt" "- name: ${1:Manages virtual machines supported by libvirt}\n  virt: name=$2 $0\n" "Manages virtual machines supported by libvirt" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/misc/virt" nil nil)
                       ("riak" "- name: ${1:This module handles some common Riak operations}\n  riak: $0\n" "This module handles some common Riak operations" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/misc/riak" nil nil)
                       ("redis" "- name: ${1:Various redis commands, slave and flush}\n  redis: command=$2 $0\n" "Various redis commands, slave and flush" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/misc/redis" nil nil)
                       ("ovirt" "- name: ${1:oVirt/RHEV platform management}\n  ovirt: user=$2 url=$3 instance_name=$4 password=$5 $0\n" "oVirt/RHEV platform management" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/misc/ovirt" nil nil)
                       ("mongodb_user" "- name: ${1:Adds or removes a user from a MongoDB database.}\n  mongodb_user: database=$2 user=$3 $0\n" "Adds or removes a user from a MongoDB database." ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/misc/mongodb_user" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("zabbix_maintenance" "- name: ${1:Create Zabbix maintenance windows}\n  zabbix_maintenance: server_url=$2 login_user=$3 login_password=$4 name=$5 desc=${6:Created by Ansible} $0\n" "Create Zabbix maintenance windows" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/monitoring/zabbix_maintenance" nil nil)
                       ("stackdriver" "- name: ${1:Send code deploy and annotation events to stackdriver}\n  stackdriver: key=$2 $0\n" "Send code deploy and annotation events to stackdriver" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/monitoring/stackdriver" nil nil)
                       ("rollbar_deployment" "- name: ${1:Notify Rollbar about app deployments}\n  rollbar_deployment: token=$2 environment=$3 revision=$4 $0\n" "Notify Rollbar about app deployments" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/monitoring/rollbar_deployment" nil nil)
                       ("pingdom" "- name: ${1:Pause/unpause Pingdom alerts}\n  pingdom: state=$2 checkid=$3 uid=$4 passwd=$5 key=$6 $0\n" "Pause/unpause Pingdom alerts" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/monitoring/pingdom" nil nil)
                       ("pagerduty" "- name: ${1:Create PagerDuty maintenance windows}\n  pagerduty: state=$2 name=$3 user=$4 passwd=$5 token=$6 requester_id=$7 $0\n" "Create PagerDuty maintenance windows" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/monitoring/pagerduty" nil nil)
                       ("newrelic_deployment" "- name: ${1:Notify newrelic about app deployments}\n  newrelic_deployment: token=$2 $0\n" "Notify newrelic about app deployments" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/monitoring/newrelic_deployment" nil nil)
                       ("nagios" "- name: ${1:Perform common tasks in Nagios related to downtime and notifications.}\n  nagios: action=$2 services=$3 command=$4 $0\n" "Perform common tasks in Nagios related to downtime and notifications." ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/monitoring/nagios" nil nil)
                       ("monit" "- name: ${1:Manage the state of a program monitored via Monit}\n  monit: name=$2 state=$3 $0\n" "Manage the state of a program monitored via Monit" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/monitoring/monit" nil nil)
                       ("logentries" "- name: ${1:Module for tracking logs via logentries.com}\n  logentries: path=$2 $0\n" "Module for tracking logs via logentries.com" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/monitoring/logentries" nil nil)
                       ("librato_annotation" "- name: ${1:create an annotation in librato}\n  librato_annotation: user=$2 api_key=$3 title=$4 links=$5 $0\n" "create an annotation in librato" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/monitoring/librato_annotation" nil nil)
                       ("datadog_event" "- name: ${1:Posts events to DataDog  service}\n  datadog_event: api_key=$2 title=$3 text=$4 $0\n" "Posts events to DataDog  service" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/monitoring/datadog_event" nil nil)
                       ("boundary_meter" "- name: ${1:Manage boundary meters}\n  boundary_meter: name=$2 apiid=$3 apikey=$4 $0\n" "Manage boundary meters" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/monitoring/boundary_meter" nil nil)
                       ("bigpanda" "- name: ${1:Notify BigPanda about deployments}\n  bigpanda: component=$2 version=$3 token=$4 state=$5 $0\n" "Notify BigPanda about deployments" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/monitoring/bigpanda" nil nil)
                       ("airbrake_deployment" "- name: ${1:Notify airbrake about app deployments}\n  airbrake_deployment: token=$2 environment=$3 $0\n" "Notify airbrake about app deployments" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/monitoring/airbrake_deployment" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("mysql_variables" "- name: ${1:Manage MySQL global variables}\n  mysql_variables: variable=$2 $0\n" "Manage MySQL global variables" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/mysql/mysql_variables" nil nil)
                       ("mysql_user" "- name: ${1:Adds or removes a user from a MySQL database.}\n  mysql_user: name=$2 $0\n" "Adds or removes a user from a MySQL database." ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/mysql/mysql_user" nil nil)
                       ("mysql_replication" "- name: ${1:Manage MySQL replication}\n  mysql_replication: $0\n" "Manage MySQL replication" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/mysql/mysql_replication" nil nil)
                       ("mysql_db" "- name: ${1:Add or remove MySQL databases from a remote host.}\n  mysql_db: name=$2 $0\n" "Add or remove MySQL databases from a remote host." ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/mysql/mysql_db" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("openvswitch_port" "- name: ${1:Manage Open vSwitch ports}\n  openvswitch_port: bridge=$2 port=$3 $0\n" "Manage Open vSwitch ports" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/network/openvswitch_port" nil nil)
                       ("openvswitch_bridge" "- name: ${1:Manage Open vSwitch bridges}\n  openvswitch_bridge: bridge=$2 $0\n" "Manage Open vSwitch bridges" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/network/openvswitch_bridge" nil nil)
                       ("lldp" "- name: ${1:get details reported by lldp}\n  lldp: $0\n" "get details reported by lldp" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/network/lldp" nil nil)
                       ("dnsmadeeasy" "- name: ${1:Interface with dnsmadeeasy.com (a DNS hosting service).}\n  dnsmadeeasy: account_key=$2 account_secret=$3 domain=$4 state=$5 $0\n" "Interface with dnsmadeeasy.com (a DNS hosting service)." ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/network/dnsmadeeasy" nil nil)
                       ("dnsimple" "- name: ${1:Interface with dnsimple.com (a DNS hosting service).}\n  dnsimple: $0\n" "Interface with dnsimple.com (a DNS hosting service)." ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/network/dnsimple" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("typetalk" "- name: ${1:Send a message to typetalk}\n  typetalk: client_id=$2 client_secret=$3 topic=$4 msg=$5 $0\n" "Send a message to typetalk" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/notification/typetalk" nil nil)
                       ("twilio" "- name: ${1:Sends a text message to a mobile phone through Twilio.}\n  twilio: account_sid=$2 auth_token=$3 msg=$4 to_number=$5 from_number=$6 $0\n" "Sends a text message to a mobile phone through Twilio." ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/notification/twilio" nil nil)
                       ("sns" "- name: ${1:Send Amazon Simple Notification Service (SNS) messages}\n  sns: msg=$2 topic=$3 $0\n" "Send Amazon Simple Notification Service (SNS) messages" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/notification/sns" nil nil)
                       ("slack" "- name: ${1:Send Slack notifications}\n  slack: domain=$2 token=$3 msg=$4 $0\n" "Send Slack notifications" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/notification/slack" nil nil)
                       ("osx_say" "- name: ${1:Makes an OSX computer to speak.}\n  osx_say: msg=$2 $0\n" "Makes an OSX computer to speak." ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/notification/osx_say" nil nil)
                       ("nexmo" "- name: ${1:Send a SMS via nexmo}\n  nexmo: api_key=$2 api_secret=$3 src=$4 dest=$5 msg=$6 $0\n" "Send a SMS via nexmo" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/notification/nexmo" nil nil)
                       ("mqtt" "- name: ${1:Publish a message on an MQTT topic for the IoT}\n  mqtt: topic=$2 payload=$3 $0\n" "Publish a message on an MQTT topic for the IoT" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/notification/mqtt" nil nil)
                       ("mail" "- name: ${1:Send an email}\n  mail: subject=$2 $0\n" "Send an email" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/notification/mail" nil nil)
                       ("jabber" "- name: ${1:Send a message to jabber user or chat room}\n  jabber: user=$2 password=$3 to=$4 msg=$5 $0\n" "Send a message to jabber user or chat room" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/notification/jabber" nil nil)
                       ("irc" "- name: ${1:Send a message to an IRC channel}\n  irc: msg=$2 channel=$3 $0\n" "Send a message to an IRC channel" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/notification/irc" nil nil)
                       ("hipchat" "- name: ${1:Send a message to hipchat}\n  hipchat: token=$2 room=$3 msg=$4 $0\n" "Send a message to hipchat" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/notification/hipchat" nil nil)
                       ("grove" "- name: ${1:Sends a notification to a grove.io channel}\n  grove: channel_token=$2 message=$3 $0\n" "Sends a notification to a grove.io channel" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/notification/grove" nil nil)
                       ("flowdock" "- name: ${1:Send a message to a flowdock}\n  flowdock: token=$2 type=$3 msg=$4 $0\n" "Send a message to a flowdock" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/notification/flowdock" nil nil)
                       ("campfire" "- name: ${1:Send a message to Campfire}\n  campfire: subscription=$2 token=$3 room=$4 msg=$5 $0\n" "Send a message to Campfire" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/notification/campfire" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("quantum_subnet" "- name: ${1:Add/remove subnet from a network}\n  quantum_subnet: login_username=${2:admin} login_password=${3:true} login_tenant_name=${4:true} network_name=${5:None} name=${6:None} cidr=${7:None} $0\n" "Add/remove subnet from a network" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/openstack/quantum_subnet" nil nil)
                       ("quantum_router_interface" "- name: ${1:Attach/Dettach a subnet's interface to a router}\n  quantum_router_interface: login_username=${2:admin} login_password=${3:yes} login_tenant_name=${4:yes} router_name=${5:None} subnet_name=${6:None} $0\n" "Attach/Dettach a subnet's interface to a router" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/openstack/quantum_router_interface" nil nil)
                       ("quantum_router_gateway" "- name: ${1:set/unset a gateway interface for the router with the specified external network}\n  quantum_router_gateway: login_username=${2:admin} login_password=${3:yes} login_tenant_name=${4:yes} router_name=${5:None} network_name=${6:None} $0\n" "set/unset a gateway interface for the router with the specified external network" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/openstack/quantum_router_gateway" nil nil)
                       ("quantum_router" "- name: ${1:Create or Remove router from openstack}\n  quantum_router: login_username=${2:admin} login_password=${3:yes} login_tenant_name=${4:yes} name=${5:None} $0\n" "Create or Remove router from openstack" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/openstack/quantum_router" nil nil)
                       ("quantum_network" "- name: ${1:Creates/Removes networks from OpenStack}\n  quantum_network: login_username=${2:admin} login_password=${3:yes} login_tenant_name=${4:yes} name=${5:None} $0\n" "Creates/Removes networks from OpenStack" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/openstack/quantum_network" nil nil)
                       ("quantum_floating_ip_associate" "- name: ${1:Associate or disassociate a particular floating IP with an instance}\n  quantum_floating_ip_associate: login_username=${2:admin} login_password=${3:yes} login_tenant_name=${4:true} instance_name=${5:None} ip_address=${6:None} $0\n" "Associate or disassociate a particular floating IP with an instance" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/openstack/quantum_floating_ip_associate" nil nil)
                       ("quantum_floating_ip" "- name: ${1:Add/Remove floating IP from an instance}\n  quantum_floating_ip: login_username=${2:admin} login_password=${3:yes} login_tenant_name=${4:yes} network_name=${5:None} instance_name=${6:None} $0\n" "Add/Remove floating IP from an instance" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/openstack/quantum_floating_ip" nil nil)
                       ("nova_keypair" "- name: ${1:Add/Delete key pair from nova}\n  nova_keypair: login_username=${2:admin} login_password=${3:yes} login_tenant_name=${4:yes} name=${5:None} $0\n" "Add/Delete key pair from nova" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/openstack/nova_keypair" nil nil)
                       ("nova_compute" "- name: ${1:Create/Delete VMs from OpenStack}\n  nova_compute: login_username=${2:admin} login_password=${3:yes} login_tenant_name=${4:yes} name=${5:None} image_id=${6:None} image_name=${7:None} $0\n" "Create/Delete VMs from OpenStack" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/openstack/nova_compute" nil nil)
                       ("keystone_user" "- name: ${1:Manage OpenStack Identity (keystone) users, tenants and roles}\n  keystone_user: $0\n" "Manage OpenStack Identity (keystone) users, tenants and roles" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/openstack/keystone_user" nil nil)
                       ("glance_image" "- name: ${1:Add/Delete images from glance}\n  glance_image: login_username=${2:admin} login_password=${3:yes} login_tenant_name=${4:yes} name=${5:None} $0\n" "Add/Delete images from glance" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/openstack/glance_image" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("zypper_repository" "- name: ${1:Add and remove Zypper repositories}\n  zypper_repository: $0\n" "Add and remove Zypper repositories" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/os/zypper_repository" nil nil)
                       ("zypper" "- name: ${1:Manage packages on SUSE and openSUSE}\n  zypper: name=$2 $0\n" "Manage packages on SUSE and openSUSE" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/os/zypper" nil nil)
                       ("yum" "- name: ${1:Manages packages with the I(yum) package manager}\n  yum: name=$2 $0\n" "Manages packages with the I(yum) package manager" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/os/yum" nil nil)
                       ("urpmi" "- name: ${1:Urpmi manager}\n  urpmi: pkg=$2 $0\n" "Urpmi manager" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/os/urpmi" nil nil)
                       ("swdepot" "- name: ${1:Manage packages with swdepot package manager (HP-UX)}\n  swdepot: name=$2 state=$3 $0\n" "Manage packages with swdepot package manager (HP-UX)" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/os/swdepot" nil nil)
                       ("svr4pkg" "- name: ${1:Manage Solaris SVR4 packages}\n  svr4pkg: name=$2 state=$3 $0\n" "Manage Solaris SVR4 packages" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/os/svr4pkg" nil nil)
                       ("rpm_key" "- name: ${1:Adds or removes a gpg key from the rpm db}\n  rpm_key: key=$2 $0\n" "Adds or removes a gpg key from the rpm db" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/os/rpm_key" nil nil)
                       ("rhn_register" "- name: ${1:Manage Red Hat Network registration using the C(rhnreg_ks) command}\n  rhn_register: $0\n" "Manage Red Hat Network registration using the C(rhnreg_ks) command" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/os/rhn_register" nil nil)
                       ("rhn_channel" "- name: ${1:Adds or removes Red Hat software channels}\n  rhn_channel: name=$2 sysname=$3 url=$4 user=$5 password=$6 $0\n" "Adds or removes Red Hat software channels" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/os/rhn_channel" nil nil)
                       ("redhat_subscription" "- name: ${1:Manage Red Hat Network registration and subscriptions using the C(subscription-manager) command}\n  redhat_subscription: $0\n" "Manage Red Hat Network registration and subscriptions using the C(subscription-manager) command" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/os/redhat_subscription" nil nil)
                       ("portinstall" "- name: ${1:Installing packages from FreeBSD's ports system}\n  portinstall: name=$2 $0\n" "Installing packages from FreeBSD's ports system" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/os/portinstall" nil nil)
                       ("portage" "- name: ${1:Package manager for Gentoo}\n  portage: $0\n" "Package manager for Gentoo" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/os/portage" nil nil)
                       ("pkgutil" "- name: ${1:Manage CSW-Packages on Solaris}\n  pkgutil: name=$2 state=$3 $0\n" "Manage CSW-Packages on Solaris" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/os/pkgutil" nil nil)
                       ("pkgng" "- name: ${1:Package manager for FreeBSD >= 9.0}\n  pkgng: name=$2 $0\n" "Package manager for FreeBSD >= 9.0" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/os/pkgng" nil nil)
                       ("pkgin" "- name: ${1:Package manager for SmartOS}\n  pkgin: name=$2 $0\n" "Package manager for SmartOS" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/os/pkgin" nil nil)
                       ("pacman" "- name: ${1:Manage packages with I(pacman)}\n  pacman: $0\n" "Manage packages with I(pacman)" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/os/pacman" nil nil)
                       ("opkg" "- name: ${1:Package manager for OpenWrt}\n  opkg: name=$2 $0\n" "Package manager for OpenWrt" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/os/opkg" nil nil)
                       ("openbsd_pkg" "- name: ${1:Manage packages on OpenBSD.}\n  openbsd_pkg: name=$2 state=$3 $0\n" "Manage packages on OpenBSD." ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/os/openbsd_pkg" nil nil)
                       ("macports" "- name: ${1:Package manager for MacPorts}\n  macports: name=$2 $0\n" "Package manager for MacPorts" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/os/macports" nil nil)
                       ("layman" "- name: ${1:Manage Gentoo overlays}\n  layman: name=$2 $0\n" "Manage Gentoo overlays" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/os/layman" nil nil)
                       ("homebrew_tap" "- name: ${1:Tap a Homebrew repository.}\n  homebrew_tap: tap=$2 $0\n" "Tap a Homebrew repository." ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/os/homebrew_tap" nil nil)
                       ("homebrew_cask" "- name: ${1:Install/uninstall homebrew casks.}\n  homebrew_cask: name=$2 $0\n" "Install/uninstall homebrew casks." ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/os/homebrew_cask" nil nil)
                       ("homebrew" "- name: ${1:Package manager for Homebrew}\n  homebrew: name=$2 $0\n" "Package manager for Homebrew" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/os/homebrew" nil nil)
                       ("apt_rpm" "- name: ${1:apt_rpm package manager}\n  apt_rpm: pkg=$2 $0\n" "apt_rpm package manager" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/os/apt_rpm" nil nil)
                       ("apt_repository" "- name: ${1:Add and remove APT repositories}\n  apt_repository: repo=${2:none} $0\n" "Add and remove APT repositories" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/os/apt_repository" nil nil)
                       ("apt_key" "- name: ${1:Add or remove an apt key}\n  apt_key: $0\n" "Add or remove an apt key" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/os/apt_key" nil nil)
                       ("apt" "- name: ${1:Manages apt-packages}\n  apt: $0\n" "Manages apt-packages" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/os/apt" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("postgresql_user" "- name: ${1:Adds or removes a users (roles) from a PostgreSQL database.}\n  postgresql_user: name=$2 $0\n" "Adds or removes a users (roles) from a PostgreSQL database." ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/postgresql/postgresql_user" nil nil)
                       ("postgresql_privs" "- name: ${1:Grant or revoke privileges on PostgreSQL database objects.}\n  postgresql_privs: database=$2 roles=$3 $0\n" "Grant or revoke privileges on PostgreSQL database objects." ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/postgresql/postgresql_privs" nil nil)
                       ("postgresql_db" "- name: ${1:Add or remove PostgreSQL databases from a remote host.}\n  postgresql_db: name=$2 $0\n" "Add or remove PostgreSQL databases from a remote host." ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/postgresql/postgresql_db" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("rax_scaling_policy" "- name: ${1:Manipulate Rackspace Cloud Autoscale Scaling Policy}\n  rax_scaling_policy: name=$2 policy_type=$3 scaling_group=$4 $0\n" "Manipulate Rackspace Cloud Autoscale Scaling Policy" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/rackspace/rax_scaling_policy" nil nil)
                       ("rax_scaling_group" "- name: ${1:Manipulate Rackspace Cloud Autoscale Groups}\n  rax_scaling_group: flavor=$2 image=$3 max_entities=$4 min_entities=$5 name=$6 server_name=$7 $0\n" "Manipulate Rackspace Cloud Autoscale Groups" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/rackspace/rax_scaling_group" nil nil)
                       ("rax_queue" "- name: ${1:create / delete a queue in Rackspace Public Cloud}\n  rax_queue: $0\n" "create / delete a queue in Rackspace Public Cloud" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/rackspace/rax_queue" nil nil)
                       ("rax_network" "- name: ${1:create / delete an isolated network in Rackspace Public Cloud}\n  rax_network: $0\n" "create / delete an isolated network in Rackspace Public Cloud" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/rackspace/rax_network" nil nil)
                       ("rax_meta" "- name: ${1:Manipulate metadata for Rackspace Cloud Servers}\n  rax_meta: $0\n" "Manipulate metadata for Rackspace Cloud Servers" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/rackspace/rax_meta" nil nil)
                       ("rax_keypair" "- name: ${1:Create a keypair for use with Rackspace Cloud Servers}\n  rax_keypair: name=$2 $0\n" "Create a keypair for use with Rackspace Cloud Servers" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/rackspace/rax_keypair" nil nil)
                       ("rax_identity" "- name: ${1:Load Rackspace Cloud Identity}\n  rax_identity: $0\n" "Load Rackspace Cloud Identity" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/rackspace/rax_identity" nil nil)
                       ("rax_files_objects" "- name: ${1:Upload, download, and delete objects in Rackspace Cloud Files}\n  rax_files_objects: container=$2 $0\n" "Upload, download, and delete objects in Rackspace Cloud Files" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/rackspace/rax_files_objects" nil nil)
                       ("rax_files" "- name: ${1:Manipulate Rackspace Cloud Files Containers}\n  rax_files: container=$2 $0\n" "Manipulate Rackspace Cloud Files Containers" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/rackspace/rax_files" nil nil)
                       ("rax_facts" "- name: ${1:Gather facts for Rackspace Cloud Servers}\n  rax_facts: $0\n" "Gather facts for Rackspace Cloud Servers" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/rackspace/rax_facts" nil nil)
                       ("rax_dns_record" "- name: ${1:Manage DNS records on Rackspace Cloud DNS}\n  rax_dns_record: data=$2 name=$3 type=$4 $0\n" "Manage DNS records on Rackspace Cloud DNS" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/rackspace/rax_dns_record" nil nil)
                       ("rax_dns" "- name: ${1:Manage domains on Rackspace Cloud DNS}\n  rax_dns: $0\n" "Manage domains on Rackspace Cloud DNS" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/rackspace/rax_dns" nil nil)
                       ("rax_clb_nodes" "- name: ${1:add, modify and remove nodes from a Rackspace Cloud Load Balancer}\n  rax_clb_nodes: load_balancer_id=$2 $0\n" "add, modify and remove nodes from a Rackspace Cloud Load Balancer" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/rackspace/rax_clb_nodes" nil nil)
                       ("rax_clb" "- name: ${1:create / delete a load balancer in Rackspace Public Cloud}\n  rax_clb: $0\n" "create / delete a load balancer in Rackspace Public Cloud" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/rackspace/rax_clb" nil nil)
                       ("rax_cdb_user" "- name: ${1:create / delete a Rackspace Cloud Database}\n  rax_cdb_user: $0\n" "create / delete a Rackspace Cloud Database" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/rackspace/rax_cdb_user" nil nil)
                       ("rax_cdb_database" "- name: ${1:create / delete a database in the Cloud Databases}\n  rax_cdb_database: $0\n" "create / delete a database in the Cloud Databases" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/rackspace/rax_cdb_database" nil nil)
                       ("rax_cdb" "- name: ${1:create/delete or resize a Rackspace Cloud Databases instance}\n  rax_cdb: $0\n" "create/delete or resize a Rackspace Cloud Databases instance" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/rackspace/rax_cdb" nil nil)
                       ("rax_cbs_attachments" "- name: ${1:Manipulate Rackspace Cloud Block Storage Volume Attachments}\n  rax_cbs_attachments: device=$2 volume=$3 server=$4 state=${5:present} $0\n" "Manipulate Rackspace Cloud Block Storage Volume Attachments" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/rackspace/rax_cbs_attachments" nil nil)
                       ("rax_cbs" "- name: ${1:Manipulate Rackspace Cloud Block Storage Volumes}\n  rax_cbs: name=$2 size=${3:100} state=${4:present} volume_type=${5:SATA} $0\n" "Manipulate Rackspace Cloud Block Storage Volumes" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/rackspace/rax_cbs" nil nil)
                       ("rax" "- name: ${1:create / delete an instance in Rackspace Public Cloud}\n  rax: $0\n" "create / delete an instance in Rackspace Public Cloud" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/rackspace/rax" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("subversion" "- name: ${1:Deploys a subversion repository.}\n  subversion: repo=$2 dest=$3 $0\n" "Deploys a subversion repository." ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/source_control/subversion" nil nil)
                       ("hg" "- name: ${1:Manages Mercurial (hg) repositories.}\n  hg: repo=$2 dest=$3 $0\n" "Manages Mercurial (hg) repositories." ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/source_control/hg" nil nil)
                       ("github_hooks" "- name: ${1:Manages github service hooks.}\n  github_hooks: user=$2 oauthkey=$3 repo=$4 action=$5 $0\n" "Manages github service hooks." ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/source_control/github_hooks" nil nil)
                       ("git" "- name: ${1:Deploy software (or files) from git checkouts}\n  git: repo=$2 dest=$3 $0\n" "Deploy software (or files) from git checkouts" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/source_control/git" nil nil)
                       ("bzr" "- name: ${1:Deploy software (or files) from bzr branches}\n  bzr: name=$2 dest=$3 $0\n" "Deploy software (or files) from bzr branches" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/source_control/bzr" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("zfs" "- name: ${1:Manage zfs}\n  zfs: name=$2 state=$3 $0\n" "Manage zfs" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/system/zfs" nil nil)
                       ("user" "- name: ${1:Manage user accounts}\n  user: name=$2 $0\n" "Manage user accounts" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/system/user" nil nil)
                       ("ufw" "- name: ${1:Manage firewall with UFW}\n  ufw: $0\n" "Manage firewall with UFW" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/system/ufw" nil nil)
                       ("sysctl" "- name: ${1:Manage entries in sysctl.conf.}\n  sysctl: name=$2 $0\n" "Manage entries in sysctl.conf." ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/system/sysctl" nil nil)
                       ("setup" "- name: ${1:Gathers facts about remote hosts}\n  setup: $0\n" "Gathers facts about remote hosts" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/system/setup" nil nil)
                       ("service" "- name: ${1:Manage services.}\n  service: name=$2 $0\n" "Manage services." ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/system/service" nil nil)
                       ("selinux" "- name: ${1:Change policy and state of SELinux}\n  selinux: state=$2 $0\n" "Change policy and state of SELinux" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/system/selinux" nil nil)
                       ("seboolean" "- name: ${1:Toggles SELinux booleans.}\n  seboolean: name=$2 state=$3 $0\n" "Toggles SELinux booleans." ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/system/seboolean" nil nil)
                       ("ping" "- name: ${1:Try to connect to host and return C(pong) on success.}\n  ping: $0\n" "Try to connect to host and return C(pong) on success." ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/system/ping" nil nil)
                       ("open_iscsi" "- name: ${1:Manage iscsi targets with open-iscsi}\n  open_iscsi: $0\n" "Manage iscsi targets with open-iscsi" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/system/open_iscsi" nil nil)
                       ("ohai" "- name: ${1:Returns inventory data from I(Ohai)}\n  ohai: $0\n" "Returns inventory data from I(Ohai)" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/system/ohai" nil nil)
                       ("mount" "- name: ${1:Control active and configured mount points}\n  mount: name=$2 src=$3 fstype=$4 state=$5 $0\n" "Control active and configured mount points" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/system/mount" nil nil)
                       ("modprobe" "- name: ${1:Add or remove kernel modules}\n  modprobe: name=$2 $0\n" "Add or remove kernel modules" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/system/modprobe" nil nil)
                       ("lvol" "- name: ${1:Configure LVM logical volumes}\n  lvol: vg=$2 lv=$3 $0\n" "Configure LVM logical volumes" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/system/lvol" nil nil)
                       ("lvg" "- name: ${1:Configure LVM volume groups}\n  lvg: vg=$2 $0\n" "Configure LVM volume groups" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/system/lvg" nil nil)
                       ("locale_gen" "- name: ${1:Creates or removes locales.}\n  locale_gen: name=$2 $0\n" "Creates or removes locales." ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/system/locale_gen" nil nil)
                       ("kernel_blacklist" "- name: ${1:Blacklist kernel modules}\n  kernel_blacklist: name=$2 $0\n" "Blacklist kernel modules" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/system/kernel_blacklist" nil nil)
                       ("hostname" "- name: ${1:Manage hostname}\n  hostname: name=$2 $0\n" "Manage hostname" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/system/hostname" nil nil)
                       ("group" "- name: ${1:Add or remove groups}\n  group: name=$2 $0\n" "Add or remove groups" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/system/group" nil nil)
                       ("getent" "- name: ${1:a wrapper to the unix getent utility}\n  getent: database=$2 $0\n" "a wrapper to the unix getent utility" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/system/getent" nil nil)
                       ("firewalld" "- name: ${1:Manage arbitrary ports/services with firewalld}\n  firewalld: permanent=${2:true} state=${3:enabled} $0\n" "Manage arbitrary ports/services with firewalld" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/system/firewalld" nil nil)
                       ("filesystem" "- name: ${1:Makes file system on block device}\n  filesystem: fstype=$2 dev=$3 $0\n" "Makes file system on block device" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/system/filesystem" nil nil)
                       ("facter" "- name: ${1:Runs the discovery program I(facter) on the remote system}\n  facter: $0\n" "Runs the discovery program I(facter) on the remote system" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/system/facter" nil nil)
                       ("debconf" "- name: ${1:Configure a .deb package}\n  debconf: name=$2 $0\n" "Configure a .deb package" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/system/debconf" nil nil)
                       ("cron" "- name: ${1:Manage cron.d and crontab entries.}\n  cron: name=$2 $0\n" "Manage cron.d and crontab entries." ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/system/cron" nil nil)
                       ("capabilities" "- name: ${1:Manage Linux capabilities}\n  capabilities: path=$2 capability=$3 $0\n" "Manage Linux capabilities" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/system/capabilities" nil nil)
                       ("authorized_key" "- name: ${1:Adds or removes an SSH authorized key}\n  authorized_key: user=$2 key=$3 $0\n" "Adds or removes an SSH authorized key" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/system/authorized_key" nil nil)
                       ("at" "- name: ${1:Schedule the execution of a command or script file via the at command.}\n  at: count=$2 units=$3 $0\n" "Schedule the execution of a command or script file via the at command." ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/system/at" nil nil)
                       ("alternatives" "- name: ${1:Manages alternative programs for common commands}\n  alternatives: name=$2 path=$3 $0\n" "Manages alternative programs for common commands" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/system/alternatives" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("vsphere_guest" "- name: ${1:Create/delete/manage a guest VM through VMware vSphere.}\n  vsphere_guest: vcenter_hostname=$2 guest=$3 username=$4 password=$5 $0\n" "Create/delete/manage a guest VM through VMware vSphere." ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/vmware/vsphere_guest" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("supervisorctl" "- name: ${1:Manage the state of a program or group of programs running via supervisord}\n  supervisorctl: name=$2 state=$3 $0\n" "Manage the state of a program or group of programs running via supervisord" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/web_infrastructure/supervisorctl" nil nil)
                       ("jira" "- name: ${1:create and modify issues in a JIRA instance}\n  jira: uri=$2 operation=$3 username=$4 password=$5 $0\n" "create and modify issues in a JIRA instance" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/web_infrastructure/jira" nil nil)
                       ("jboss" "- name: ${1:deploy applications to JBoss}\n  jboss: deployment=$2 $0\n" "deploy applications to JBoss" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/web_infrastructure/jboss" nil nil)
                       ("htpasswd" "- name: ${1:manage user files for basic authentication}\n  htpasswd: path=$2 name=$3 $0\n" "manage user files for basic authentication" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/web_infrastructure/htpasswd" nil nil)
                       ("ejabberd_user" "- name: ${1:Manages users for ejabberd servers}\n  ejabberd_user: username=$2 host=$3 $0\n" "Manages users for ejabberd servers" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/web_infrastructure/ejabberd_user" nil nil)
                       ("django_manage" "- name: ${1:Manages a Django application.}\n  django_manage: command=$2 app_path=$3 $0\n" "Manages a Django application." ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/web_infrastructure/django_manage" nil nil)
                       ("apache2_module" "- name: ${1:enables/disables a module of the Apache2 webserver}\n  apache2_module: name=$2 $0\n" "enables/disables a module of the Apache2 webserver" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/web_infrastructure/apache2_module" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("win_user" "- name: ${1:Manages local Windows user accounts}\n  win_user: name=$2 password=$3 $0\n" "Manages local Windows user accounts" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/windows/win_user" nil nil)
                       ("win_stat" "- name: ${1:returns information about a Windows file}\n  win_stat: path=$2 $0\n" "returns information about a Windows file" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/windows/win_stat" nil nil)
                       ("win_service" "- name: ${1:Manages Windows services}\n  win_service: name=$2 $0\n" "Manages Windows services" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/windows/win_service" nil nil)
                       ("win_ping" "- name: ${1:A windows version of the classic ping module.}\n  win_ping: $0\n" "A windows version of the classic ping module." ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/windows/win_ping" nil nil)
                       ("win_msi" "- name: ${1:Installs and uninstalls Windows MSI files}\n  win_msi: path=$2 $0\n" "Installs and uninstalls Windows MSI files" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/windows/win_msi" nil nil)
                       ("win_group" "- name: ${1:Add and remove local groups}\n  win_group: name=$2 $0\n" "Add and remove local groups" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/windows/win_group" nil nil)
                       ("win_get_url" "- name: ${1:Fetches a file from a given URL}\n  win_get_url: url=$2 $0\n" "Fetches a file from a given URL" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/windows/win_get_url" nil nil)
                       ("win_feature" "- name: ${1:Installs and uninstalls Windows Features}\n  win_feature: name=$2 $0\n" "Installs and uninstalls Windows Features" ansible nil nil "/home/orion/.config/doom/snippets/snippets/ansible-mode/windows/win_feature" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("scc" "(spy-calls-count ${1:'foo})" "spy-calls-count ..." nil nil nil "/home/orion/.config/doom/snippets/snippets/buttercup-minor-mode/spy-calls-count" nil "scc")
                       ("scaf" "(spy-calls-args-for ${1:'foo} ${2:args...})" "spy-calls-args-for ..." nil nil nil "/home/orion/.config/doom/snippets/snippets/buttercup-minor-mode/spy-calls-args-for" nil "scaf")
                       ("sca" "(spy-calls-any ${1:'foo})" "spy-calls-any ..." nil nil nil "/home/orion/.config/doom/snippets/snippets/buttercup-minor-mode/spy-calls-any" nil "sca")
                       ("scaa" "(spy-calls-all-args ${1:'foo})" "spy-calls-all-args ..." nil nil nil "/home/orion/.config/doom/snippets/snippets/buttercup-minor-mode/spy-calls-all-args" nil "scaa")
                       ("ate" "and-throw-error ${1:'error}" ":and-throw-error ..."
                        (doom-snippets-without-trigger
                         (eq
                          (char-before)
                          58))
                        nil nil "/home/orion/.config/doom/snippets/snippets/buttercup-minor-mode/spy-and-throw-error" nil "ate")
                       ("arv" "and-return-value ${1:value}" ":and-return-value ..."
                        (doom-snippets-without-trigger
                         (eq
                          (char-before)
                          58))
                        nil nil "/home/orion/.config/doom/snippets/snippets/buttercup-minor-mode/spy-and-return-value" nil "arv")
                       ("act" "and-call-through" ":and-call-through ..."
                        (doom-snippets-without-trigger
                         (eq
                          (char-before)
                          58))
                        nil nil "/home/orion/.config/doom/snippets/snippets/buttercup-minor-mode/spy-and-call-through" nil "act")
                       ("acf" "and-call-fake ${1:#'fn}" ":and-call-fake ..."
                        (doom-snippets-without-trigger
                         (eq
                          (char-before)
                          58))
                        nil nil "/home/orion/.config/doom/snippets/snippets/buttercup-minor-mode/spy-and-call-fake" nil "acf")
                       ("it" "(it \"${1:...}\" ${2:`(doom-snippets-format \"%n%s\")`}$0)" "it ..." nil nil nil "/home/orion/.config/doom/snippets/snippets/buttercup-minor-mode/it" nil nil)
                       ("t" "to-throw" ":to-throw"
                        (doom-snippets-without-trigger
                         (eq
                          (char-before)
                          58))
                        nil nil "/home/orion/.config/doom/snippets/snippets/buttercup-minor-mode/expect-to-throw" nil "t")
                       ("m" "to-match $1" ":to-match ..."
                        (doom-snippets-without-trigger
                         (eq
                          (char-before)
                          58))
                        nil nil "/home/orion/.config/doom/snippets/snippets/buttercup-minor-mode/expect-to-match" nil "m")
                       ("s" "to-have-same-items-as $1" ":to-have-same-items-as ..."
                        (doom-snippets-without-trigger
                         (eq
                          (char-before)
                          58))
                        nil nil "/home/orion/.config/doom/snippets/snippets/buttercup-minor-mode/expect-to-have-same-items-as" nil "s")
                       ("bcw" "to-have-been-called-with ${1:args...}" ":to-have-been-called-with ..."
                        (doom-snippets-without-trigger
                         (eq
                          (char-before)
                          58))
                        nil nil "/home/orion/.config/doom/snippets/snippets/buttercup-minor-mode/expect-to-have-been-called-with" nil "bcw")
                       ("bct" "to-have-been-called-times ${1:1}" ":to-have-been-called-time ..."
                        (doom-snippets-without-trigger
                         (eq
                          (char-before)
                          58))
                        nil nil "/home/orion/.config/doom/snippets/snippets/buttercup-minor-mode/expect-to-have-been-called-times" nil "bct")
                       ("bc" "to-have-been-called" ":to-have-been-called ..."
                        (doom-snippets-without-trigger
                         (eq
                          (char-before)
                          58))
                        nil nil "/home/orion/.config/doom/snippets/snippets/buttercup-minor-mode/expect-to-have-been-called" nil "bc")
                       ("e" "to-equal $1" ":to-equal"
                        (doom-snippets-without-trigger
                         (eq
                          (char-before)
                          58))
                        nil nil "/home/orion/.config/doom/snippets/snippets/buttercup-minor-mode/expect-to-equal" nil "e")
                       ("c" "to-contain $1" ":to-contain ..."
                        (doom-snippets-without-trigger
                         (eq
                          (char-before)
                          58))
                        nil nil "/home/orion/.config/doom/snippets/snippets/buttercup-minor-mode/expect-to-contain" nil "c")
                       ("b" "to-be ${1:nil}" ":to-be ..."
                        (doom-snippets-without-trigger
                         (eq
                          (char-before)
                          58))
                        nil nil "/home/orion/.config/doom/snippets/snippets/buttercup-minor-mode/expect-to-be" nil "b")
                       ("expect" "(expect ${1:`(doom-snippets-format \"value\")`}$0)" "expect" nil nil nil "/home/orion/.config/doom/snippets/snippets/buttercup-minor-mode/expect" nil "expect")
                       ("ex"
                        (progn
                          (doom-snippets-expand :uuid "expect"))
                        "expect" nil nil nil "/home/orion/.config/doom/snippets/snippets/buttercup-minor-mode/ex" nil "ex")
                       ("describe" "(describe \"${1:group}\"${2:`(doom-snippets-format \"%n%s\")`}$0)" "describe ... ..." nil nil nil "/home/orion/.config/doom/snippets/snippets/buttercup-minor-mode/describe" nil "describe")
                       ("desc"
                        (progn
                          (doom-snippets-expand :uuid "describe"))
                        "describe ... ..." nil nil nil "/home/orion/.config/doom/snippets/snippets/buttercup-minor-mode/desc" nil "desc")
                       ("bfe" "(before-each ${1:`(doom-snippets-format \"%n%s\")`})" "before-each ..." nil nil nil "/home/orion/.config/doom/snippets/snippets/buttercup-minor-mode/before-each" nil "bfe")
                       ("bfa" "(before-all ${1:`(doom-snippets-format \"%n%s\")`})" "before-all ..." nil nil nil "/home/orion/.config/doom/snippets/snippets/buttercup-minor-mode/before-all" nil "bfa")
                       ("afe" "(after-each ${1:`(doom-snippets-format \"%n%s\")`})" "after-each ..." nil nil nil "/home/orion/.config/doom/snippets/snippets/buttercup-minor-mode/after-each" nil "afe")
                       ("afa" "(after-all ${1:`(doom-snippets-format \"%n%s\")`})" "after-all ..." nil nil nil "/home/orion/.config/doom/snippets/snippets/buttercup-minor-mode/after-all" nil "afa")))


;;; contents of the .yas-setup.el support file:
;;;
(defun doom-snippets-c++-using-std-p ()
  "Return non-nil if 'using namespace std' is found at the top of this file."
  (save-excursion
    (goto-char (point-max))
    (or (search-forward "using namespace std;" 512 t)
        (search-forward "std::" 1024 t))))

(defun doom-snippets-c++-class-name (str)
  "Search for a class name like `DerivedClass' in STR
(which may look like `DerivedClass : ParentClass1, ParentClass2, ...')
If found, the class name is returned, otherwise STR is returned"
  (yas-substr str "[^: ]*"))

(defun doom-snippets-c++-class-method-decl-choice ()
  "Choose and return the end of a C++11 class method declaration"
  (yas-choose-value '(";" " = default;" " = delete;")))
;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("vec" "`(setq --cpp-ns (if (doom-snippets-c++-using-std-p) \"\" \"std::\"))\n--cpp-ns`vector<${1:type}> ${2:var}${3:(${4:10}, $1($5))};" "vector" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/vector" nil "vec")
                       ("using" "using namespace ${std};" "using namespace ..." nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/using" nil "using")
                       ("ucp" "std::unique_copy(std::begin(${1:container}), std::end($1),\n  std::ostream_iterator<string>(std::cout, \"\\n\"));" "unique_copy" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/unique_copy" nil nil)
                       ("uqe" "auto pos = std::unique(std::begin(${1:container}), std::end($1));" "unique" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/unique" nil nil)
                       ("tryw" "try {\n    `(or yas/selected-text (car kill-ring))`\n} catch ${1:Exception} {\n\n}\n" "tryw" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/tryw" nil "tryw")
                       ("try" "try {\n    `%`$0\n} catch (${1:type}) {\n\n}\n" "try" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/try" nil nil)
                       ("ltr" "${1:container}.erase(0, $1.find_first_not_of(\" \\t\\n\\r\"));" "remove whitespace at beginning" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/trim_left" nil nil)
                       ("lwr" "std::transform(std::begin(${1:container}), std::end($1), std::begin($1), [](char c) {\n  `(or (concat % \"\\n\") \"\")`return std::tolower(c);\n});" "string to lower case" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/transform_lowercase" nil nil)
                       ("upr" "std::transform(std::begin(${1:container}), std::end($1), std::begin($1), [](char c) {\n  `(or (concat % \"\\n\") \"\")`return std::toupper(c);\n});" "transform" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/transform-with-closure" nil nil)
                       ("tfm" "std::transform(std::begin(${1:container}), std::end($1),\n  std::begin($1), []($2) {\n$3%\n});\n$0\n" "transform" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/transform" nil nil)
                       ("throw" "throw ${1:MyError}($0);" "throw" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/throw" nil "throw")
                       ("this" "this" "this" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/this" nil nil)
                       ("ts" "BOOST_AUTO_TEST_SUITE( ${1:test_suite1} )\n\n$0\n\nBOOST_AUTO_TEST_SUITE_END()" "test_suite" nil
                        ("testing")
                        nil "/home/orion/.config/doom/snippets/snippets/c++-mode/test_suite" nil "ts")
                       ("test_main" "int main(int argc, char **argv) {\n      ::testing::InitGoogleTest(&argc, argv);\n       return RUN_ALL_TESTS();\n}" "test_main" nil
                        ("testing")
                        nil "/home/orion/.config/doom/snippets/snippets/c++-mode/test_main" nil "test_main")
                       ("tc" "BOOST_AUTO_TEST_CASE( ${1:test_case} )\n{\n        $0\n}" "test case" nil
                        ("testing")
                        nil "/home/orion/.config/doom/snippets/snippets/c++-mode/test case" nil "tc")
                       ("temp" "template<${1:$$(yas/choose-value '(\"typename\" \"class\"))} ${2:T}>\n$0" "template" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/template" nil "temp")
                       ("swr" "std::swap_ranges(std::begin(${1:container}), std::end($1), std::begin($2));" "swap_ranges" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/swap_ranges" nil nil)
                       ("st" "std::$0" "std::" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/std_colon" nil "st")
                       ("std" "using namespace std;" "std" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/std" nil "std")
                       ("sts" "std::stable_sort(std::begin(${1:container}), std::end($1));" "stable_sort" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/stable_sort" nil nil)
                       ("spt" "auto pos = std::stable_partition(std::begin(${1:container}), std::end($1), []($2) {\n  `%`$3\n});\nif (pos != std::end($1)) {\n  $4\n}" "stable_partition" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/stable_partition" nil nil)
                       ("sth" "std::sort_heap(std::begin(${1:container}), std::end($1));" "sort_heap" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/sort_heap" nil nil)
                       ("srt" "std::sort(std::begin(${1:container}), std::end($1));" "sort" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/sort" nil nil)
                       ("srn" "auto pos = std::search_n(std::begin(${1:container}), std::end($1),$2,$3);\nif (pos != std::end($1)) {\n  `%`$4\n}" "search_n" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/search_n" nil nil)
                       ("srh" "auto pos = std::search(std::begin(${1:container}), std::end($1),\n  std::begin($2), std::end($3));\nif (pos != std::end($1)) {\n  `%`$4\n}" "search" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/search" nil nil)
                       ("rtc" "std::rotate_copy(std::begin(${1:container}), std::begin($2), std::end($1), std::begin($3));" "rotate_copy" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/rotate_copy" nil nil)
                       ("rte" "std::rotate(std::begin(${1:container}), std::begin($2), std::end($1));" "rotate" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/rotate" nil nil)
                       ("rvc" "std::reverse_copy(std::begin(${1:container}), std::end($1), std::begin($2));" "reverse_copy" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/reverse_copy" nil nil)
                       ("rvr" "std::reverse(std::begin(${1:container}), std::end($1));" "reverse" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/reverse" nil nil)
                       ("rpi" "std::replace_if(\n  std::begin(${1:container}),\n  std::end($1), []($2) {\n    `%`$3\n  },\n  $4\n);" "replace_if" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/replace_if" nil nil)
                       ("rci" "std::replace_copy_if(\n  std::begin(${1:container}),\n  std::end($1),\n  std::begin($1), []($2) {\n    `%`$3\n  },\n  $4\n);" "replace_copy_if" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/replace_copy_if" nil nil)
                       ("rpc" "std::replace_copy(std::begin(${1:container}), std::end($1), std::begin($1), $2, $3);" "replace_copy" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/replace_copy" nil nil)
                       ("rpl" "std::replace(std::begin(${1:container}), std::end($1), $2, $3);" "replace" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/replace" nil nil)
                       ("rmi" "auto pos = std::remove_if(std::begin(${1:container}), std::end($1), []($2) {\n  `%`$3\n});\nif (pos != std::end($1)) {\n  $4\n}" "remove_if" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/remove_if" nil nil)
                       ("rmf" "std::remove_copy_if(std::begin(${1:container}), std::end($1),\n  std::begin($1), []($2) {\n    `%`$3\n  }\n);" "remove_copy_if" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/remove_copy_if" nil nil)
                       ("rmc" "std::remove_copy(std::begin(${1:container}), std::end($1),\n  std::begin($1), $2);" "remove_copy" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/remove_copy" nil nil)
                       ("rmv" "auto pos = std::remove(std::begin(${1:container}), std::end($1), $2);\nif (pos != std::end($1)) {\n  `%`$3\n}" "remove" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/remove" nil nil)
                       ("shf" "std::random_shuffle(std::begin(${1:container}), std::end($1));" "random_shuffle" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/random_shuffle" nil nil)
                       ("phh" "std::push_heap(std::begin(${1:container}), std::end($1));" "push_heap" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/push_heap" nil nil)
                       ("public" "public:\n    $0" "public" nil nil
                        ((yas-also-auto-indent-first-line t))
                        "/home/orion/.config/doom/snippets/snippets/c++-mode/public" nil nil)
                       ("protected" "protected:\n    $0" "protected" nil nil
                        ((yas-also-auto-indent-first-line t))
                        "/home/orion/.config/doom/snippets/snippets/c++-mode/protected" nil nil)
                       ("private" "private:\n    $0" "private" nil nil
                        ((yas-also-auto-indent-first-line t))
                        "/home/orion/.config/doom/snippets/snippets/c++-mode/private" nil nil)
                       ("prp" "if (std::prev_permutation(std::begin(${1:container}), std::end($1))) {\n  `%`$2\n}" "prev_permutation" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/prev_permutation" nil nil)
                       ("ppt" "auto pos = std::partition_point(std::begin(${1:container}), std::end($1), []($2) {\n  `%`$3\n});\nif (pos != std::end($1)) {\n  $4\n}" "partition_point" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/partition_point" nil nil)
                       ("ptc" "std::partition_copy(std::begin(${1:container}), std::end($1),\n                    std::begin($2), std::end($3));" "partition_copy" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/partition_copy" nil nil)
                       ("ptn" "auto pos = std::partition(std::begin(${1:container}), std::end($1), []($2) {\n  `%`$3\n});\nif (pos != std::end($1)) {\n  $4\n}" "partition" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/partition" nil nil)
                       ("psc" "std::partial_sort_copy(std::begin(${1:container}), std::end($1),\n                       std::begin($2), std::end($3));" "partial_sort_copy" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/partial_sort_copy" nil nil)
                       ("pst" "std::partial_sort(std::begin(${1:container}), std::end($1), std::end($1));" "partial_sort" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/partial_sort" nil nil)
                       ("pack" "void cNetCommBuffer::pack(${1:type}) {\n\n}\n\n$0" "pack" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/pack" nil "pack")
                       ("os" "#include <ostream>" "ostream" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/ostream" nil "os")
                       ("<<" "std::ostream& operator<<(std::ostream& s, const ${1:type}& ${2:c})\n{\n         $0\n         return s;\n}" "operator<<" nil
                        ("operator overloading")
                        nil "/home/orion/.config/doom/snippets/snippets/c++-mode/operator_ostream" nil "<<")
                       (">>" "istream& operator>>(istream& s, const ${1:type}& ${2:c})\n{\n         $0\n}\n" "operator>>" nil
                        ("operator overloading")
                        nil "/home/orion/.config/doom/snippets/snippets/c++-mode/operator_istream" nil ">>")
                       ("[]" "${1:Type}& operator[](${2:int index})\n{\n        $0\n}" "operator[]" nil
                        ("operator overloading")
                        nil "/home/orion/.config/doom/snippets/snippets/c++-mode/operator[]" nil "[]")
                       ("==" "bool ${1:MyClass}::operator==(const $1 &other) const {\n     $0\n}" "operator==" nil
                        ("operator overloading")
                        nil "/home/orion/.config/doom/snippets/snippets/c++-mode/operator==" nil "==")
                       ("=" "${1:MyClass}& $1::operator=(const $1 &rhs) {\n    // Check for self-assignment!\n    if (this == &rhs)\n      return *this;\n    $0\n    return *this;\n}" "operator=" nil
                        ("operator overloading")
                        nil "/home/orion/.config/doom/snippets/snippets/c++-mode/operator=" nil "=")
                       ("+=" "${1:MyClass}& $1::operator+=(${2:const $1 &rhs})\n{\n  $0\n  return *this;\n}" "operator+=" nil
                        ("operator overloading")
                        nil "/home/orion/.config/doom/snippets/snippets/c++-mode/operator+=" nil "+=")
                       ("+" "${1:MyClass} $1::operator+(const $1 &other)\n{\n    $1 result = *this;\n    result += other;\n    return result;\n}" "operator+" nil
                        ("operator overloading")
                        nil "/home/orion/.config/doom/snippets/snippets/c++-mode/operator+" nil "+")
                       ("!=" "bool ${1:MyClass}::operator!=(const $1 &other) const {\n    return !(*this == other);\n}" "operator!=" nil
                        ("operator overloading")
                        nil "/home/orion/.config/doom/snippets/snippets/c++-mode/operator!=" nil "!=")
                       ("nth" "std::nth_element(std::begin(${1:container}), std::end($1), std::end($1));" "nth_element" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/nth_element" nil nil)
                       ("nno" "if (std::none_of(std::begin(${1:container}), std::end($1), []($2) {\n  `%`$3\n})) {\n  $4\n}" "none_of" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/none_of" nil nil)
                       ("nxp" "if (std::next_permutation(std::begin(${1:container}), std::end($1))) {\n  `%`$2\n}" "next_permutation" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/next_permutation" nil nil)
                       ("nss" "namespace ${1:name} {\n  `%`$0\n}" "namespace" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/namespace_block" nil nil)
                       ("ns" "namespace ${1:name}" "namespace ..." nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/namespace" nil "ns")
                       ("mpb" "std::move_backward(std::begin(${1:container}), std::end($1), std::end($1));" "move_backward" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/move_backward" nil nil)
                       ("mod" "class ${1:Class} : public cSimpleModule\n{\n   $0\n}" "module" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/module" nil "mod")
                       ("msm" "auto values = std::mismatch(std::begin(${1:container}), std::end($1), std::begin($1));\nif (values.first == std::end($1)) {\n  `%`$2\n} else {\n  $3\n}" "mismatch" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/mismatch" nil nil)
                       ("mme" "auto minmax = std::minmax_element(std::begin(${1:container}), std::end($1));" "minmax_element" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/minmax_element" nil nil)
                       ("mne" "auto pos = std::min_element(std::begin(${1:container}), std::end($1));" "min_element" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/min_element" nil nil)
                       ("mrg" "std::merge(std::begin(${1:container}), std::end($1),\nstd::begin($2), std::end($3), std::begin($4));" "merge" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/merge" nil nil)
                       ("mf" "${1:type} ${2:Name}::${3:name}(${4:args})${5: const}\n{\n        $0\n}\n" "member_function" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/member_function" nil nil)
                       ("mxe" "auto pos = std::max_element(std::begin(${1:container}), std::end($1));" "max_element" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/max_element" nil nil)
                       ("map" "std::map<${1:type1}$0> ${2:var};" "map" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/map" nil "map")
                       ("mkh" "std::make_heap(std::begin(${1:container}), std::end($1));" "make_heap" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/make_heap" nil nil)
                       ("lxc" "if (std::lexigraphical_compare(std::begin(${1:container}), std::end($1), std::begin($2), std::end($3))) {\n  `%`$4\n}" "lexigraphical_compare" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/lexigraphical_compare" nil nil)
                       ("lam" "[$1]($2) { `(!%!)`$3 }" "lambda" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/lambda" nil nil)
                       ("iter" "${1:std::}${2:vector<int>}::iterator ${3:iter};\n" "iterator" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/iterator" nil "iter")
                       ("isu" "auto pos = std::is_sorted_until(std::begin(${1:container}), std::end($1));\nif (pos != std::end($1)) {\n  `%`$2\n}" "is_sorted_until" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/is_sorted_until" nil nil)
                       ("iss" "if (std::is_sorted(std::begin(${1:container}), std::end($1))) {\n  `%`$2\n}" "is_sorted" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/is_sorted" nil nil)
                       ("ipr" "if (std::is_permutation(std::begin(${1:container}), std::end($1), std::begin($2))) {\n  `%`$3\n}" "is_permutation" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/is_permutation" nil nil)
                       ("ipt" "if (std::is_partitioned(std::begin(${1:container}), std::end($1), []($2) {\n  `%`$3\n})) {\n  $4\n}" "is_partitioned" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/is_partitioned" nil nil)
                       ("ihu" "auto pos = std::is_heap_until(std::begin(${1:container}), std::end($1));\nif (pos != std::end($1)) {\n  `%`$2\n}" "is_heap_until" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/is_heap_until" nil nil)
                       ("ihp" "if (std::is_heap(std::begin(${1:container}), std::end($1))) {\n  `%`$2\n}" "is_heap" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/is_heap" nil nil)
                       ("ita" "std::iota(std::begin(${1:container}), std::end($1), $2);" "iota" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/iota" nil nil)
                       ("il" "inline $0" "inline" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/inline" nil "il")
                       ("istr" "#include <string>" "#include <string>" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/include_string" nil "istr")
                       ("iss" "#include <sstream>" "#include <sstream>" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/include_sstream" nil "iss")
                       ("iio" "#include <iostream>" "#include <iostream>" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/include_iostream" nil "iio")
                       ("inch" "#include \"`(file-name-nondirectory (file-name-sans-extension (or (doom-snippets-text) (buffer-file-name) )))`.h\"" "#include \"{self}.h\"" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/include_header" nil nil)
                       ("inc" "#include <`%`${1:lib}>" "#include <lib>" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/include" nil nil)
                       ("ignore" "${1:std::}cin.ignore(std::numeric_limits<std::streamsize>::max(), '\\n');" "ignore" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/ignore" nil "ignore")
                       ("gtest" "#include <gtest/gtest.h>" "gtest" nil
                        ("testing")
                        nil "/home/orion/.config/doom/snippets/snippets/c++-mode/gtest" nil "gtest")
                       ("gnn" "std::generate_n(std::begin(${1:container}), $2, []($3) {\n  `%`$4\n});" "generate_n" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/generate_n" nil nil)
                       ("gnr" "std::generate(std::begin(${1:container}), std::end($1), []($2) {\n  `%`$3\n});" "generate" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/generate" nil nil)
                       ("function" "${1:void} ${2:Class}::${3:name}($4)${5: const} {\n    $0\n}" "function" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/function" nil nil)
                       ("f" "${1:void} ${2:name}($3)$0" "fun_declaration" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/fun_declaration" nil "f")
                       ("fr" "friend $0;" "friend" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/friend" nil "fr")
                       ("forit" "for (${1:iter}=${2:var}.begin(); $1!=$2.end(); ++$1) {\n    `%`$0\n}" "for iterator loop" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/for_iterator" nil "forit")
                       ("fore" "std::for_each(std::begin(${1:container}), std::end($1), []($2) {\n  `%`$3\n});" "for each loop" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/for_each" nil "fore")
                       ("fixt" "BOOST_FIXTURE_TEST_SUITE( ${1:name}, ${2:Fixture} )\n\n$0\n\nBOOST_AUTO_TEST_SUITE_END()" "fixture" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/fixture" nil "fixt")
                       ("fni" "auto pos = std::find_if(std::begin(${1:container}), std::end($1), []($2) {\n  `%`$3\n});\nif (pos != std::end($1)) {\n  $4\n}" "find_if" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/find_if" nil nil)
                       ("ffo" "auto pos = std::find_first_of(\n  std::begin(${1:container}), std::end($1),\n  std::begin($2), std::end($3)\n);\nif (pos != std::end($1)) {\n  `%`$4\n}" "find_first_of" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/find_first_of" nil nil)
                       ("fne" "auto pos = std::find_std::end(\n  std::begin(${1:container}), std::end($1),\n  std::begin($2), std::end($3)\n);\nif (pos != std::end($1)) {\n  `%`$4\n}" "find_end" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/find_end" nil nil)
                       ("fnd" "auto pos = std::find(std::begin(${1:container}), std::end($1), $2);\nif (pos != std::end($1)) {\n  `%`$3\n}" "find" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/find" nil nil)
                       ("fin" "auto pos = std::find_if_not(std::begin(${1:container}), std::end($1),[]($2) {\n  $3\n});\nif (pos != std::end($1)) {\n  $4\n}\n$0\n" "find_if_not" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/fin" nil nil)
                       ("fln" "std::fill_n(std::begin(${1:container}), $2, $3);" "fill_n" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/fill_n" nil nil)
                       ("fil" "std::fill(std::begin(${1:container}), std::end($1), $2);" "fill" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/fill" nil nil)
                       ("erf" "${1:container}.erase($1.find_last_not_of(\" \\t\\n\\r\") + 1);" "generate_n" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/erase_find_last_not_of" nil nil)
                       ("erm" "${1:container}.erase(std::remove(std::begin($1), std::end($1), $2), std::end($1));" "remove" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/erase" nil nil)
                       ("eql" "if (std::equal(std::begin(${1:container}), std::end($1), std::begin($2))) {\n  `%`$3\n}" "equal" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/equal" nil nil)
                       ("enum" "enum ${1:NAME} {$0};" "enum" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/enum" nil nil)
                       ("cast" "check_and_cast<${1:Type} *>(${2:msg});" "dynamic_casting" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/dynamic_casting" nil "cast")
                       ("/**" "/**\n * $0\n */" "doc" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/doc" nil "/**")
                       ("dla" "delete[] ${1:arr};" "delete[]" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/delete[]" nil "dla")
                       ("dl" "delete ${1:pointer};" "delete" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/delete" nil "dl")
                       ("<<" "friend std::ostream& operator<<(std::ostream&, const ${1:Name}&);" "d_operator<<" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/d_operator_ostream" nil nil)
                       (">>" "friend std::istream& operator>>(std::istream&, const ${1:Name}&);" "d_operator>>" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/d_operator_istream" nil nil)
                       ("c[" "const ${1:Type}& operator[](${2:int index}) const;" "d_operator[]_const" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/d_operator[]_const" nil "c[")
                       ("[" "${1:Type}& operator[](${2:int index});" "d_operator[]" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/d_operator[]" nil "[")
                       ("<<" "friend std::ostream& operator<<(std::ostream&, const ${1:Class}&);" "d_operator<<" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/d_operator" nil "<<")
                       ("d+=" "${1:MyClass}& operator+=(${2:const $1 &});" "d+=" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/d+=" nil "d+=")
                       ("cstd" "#include <cstdlib>" "cstd" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/cstd" nil "cstd")
                       ("out" "`(setq --cpp-ns (if (doom-snippets-c++-using-std-p) \"\" \"std::\"))\n--cpp-ns`cout << `%`$1 << `--cpp-ns`endl;`(progn (makunbound '--cpp-ns) \"\")`" "cout" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/cout" nil "out")
                       ("count_if" "auto n = std::count_if(std::begin(${1:container}), std::end($1), []($2) {\n  $3\n});" "count_if" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/count_if" nil nil)
                       ("cnt" "auto n = std::count(std::begin(${1:container}), std::end($1), $2);" "count" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/count" nil nil)
                       ("oit" "std::copy(std::begin(${1:container}), std::end($1), std::ostream_iterator<$2>{\n%\\istd::cout, \"$3\"\n});" "copy" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/copy_ostream_iter" nil nil)
                       ("cpn" "std::copy_n(std::begin(${1:container}), $2, std::end($1));" "copy_n" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/copy_n" nil nil)
                       ("cpi" "std::copy_if(std::begin(${1:container}), std::end($1), std::begin($2),\n[]($3) {\n  $4\n});" "copy_if" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/copy_if" nil nil)
                       ("cpb" "std::copy_backward(std::begin(${1:container}), std::end($1), std::end($1));" "copy_backward" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/copy_backward" nil nil)
                       ("cpy" "std::copy(std::begin(${1:container}), std::end($1), std::begin($2));" "copy" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/copy" nil nil)
                       ("ct" "${1:Class}::$1(${2:args}) ${3: : ${4:init}} {\n    $0\n}" "constructor" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/constructor" nil "ct")
                       ("c[" "const ${1:Type}& operator[](${2:int index}) const\n{\n        $0\n}" "const_[]" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/const_[]" nil "c[")
                       ("cls11" "class ${1:Name}\n{\npublic:\n${2:  ${3://! Default constructor\n  }${1:$(doom-snippets-c++-class-name yas-text)}()${4:;$(doom-snippets-c++-class-method-decl-choice)}\n\n}${5:  ${6://! Copy constructor\n  }${1:$(doom-snippets-c++-class-name yas-text)}(const ${1:$(doom-snippets-c++-class-name yas-text)} &other)${7:;$(doom-snippets-c++-class-method-decl-choice)}\n\n}${8:  ${9://! Move constructor\n  }${1:$(doom-snippets-c++-class-name yas-text)}(${1:$(doom-snippets-c++-class-name yas-text)} &&other)${10: noexcept}${11:;$(doom-snippets-c++-class-method-decl-choice)}\n\n}${12:  ${13://! Destructor\n  }${14:virtual }~${1:$(doom-snippets-c++-class-name yas-text)}()${15: noexcept}${16:;$(doom-snippets-c++-class-method-decl-choice)}\n\n}${17:  ${18://! Copy assignment operator\n  }${1:$(doom-snippets-c++-class-name yas-text)}& operator=(const ${1:$(doom-snippets-c++-class-name yas-text)} &other)${19:;$(doom-snippets-c++-class-method-decl-choice)}\n\n}${20:  ${21://! Move assignment operator\n  }${1:$(doom-snippets-c++-class-name yas-text)}& operator=(${1:$(doom-snippets-c++-class-name yas-text)} &&other)${22: noexcept}${23:;$(doom-snippets-c++-class-method-decl-choice)}\n\n}$0\n\nprotected:\nprivate:\n};" "class11" nil
                        ("c++11")
                        nil "/home/orion/.config/doom/snippets/snippets/c++-mode/class11" nil "d7c41f87-9b8a-479d-bb12-89f4cbdd46a7")
                       ("class" "class ${1:Name} {\n    public:\n        ${1:$(yas/substr yas-text \"[^: ]*\")}();\n        ${2:virtual ~${1:$(yas/substr yas-text \"[^: ]*\")}();}\n    $0\n};" "class" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/class" nil nil)
                       ("cin" "`(setq --cpp-ns (if (doom-snippets-c++-using-std-p) \"\" \"std::\"))\n--cpp-ns`cin >> ${1:string};" "cin" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/cin" nil nil)
                       ("err" "`(setq --cpp-ns (if (doom-snippets-c++-using-std-p) \"\" \"std::\"))\ncerr` << `%`$1 << `--cpp-ns`endl;`(progn (makunbound '--cpp-ns) \"\")`" "cerr" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/cerr" nil "err")
                       ("req" "BOOST_REQUIRE( ${1:condition} );\n$0" "boost_require" nil
                        ("boost")
                        nil "/home/orion/.config/doom/snippets/snippets/c++-mode/boost_require" nil "req")
                       ("beginend" "${1:v}.begin(), $1.end" "v.begin(), v.end()" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/beginend" nil "beginend")
                       ("assert" "assert($0);" "assert" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/assert" nil nil)
                       ("ano" "if (std::any_of(std::begin(${1:container}), std::end($1), []($2) {\n  `%`$3\n})) {\n  $4\n}" "any_of" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/any_of" nil nil)
                       ("alo" "if (std::all_of(std::begin(${1:container}), std::end($1), []($2) {\n  `%`$3\n})) {\n  $4\n}" "all_of" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/all_of" nil nil)
                       ("ajf" "auto pos = std::adjacent_find(std::begin(${1:container}), std::end($1));\nif (pos != std::end($1)) {\n  $2\n}" "adjacent_find" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/adjacent_find" nil nil)
                       ("acl" "auto sum = std::accumulate(std::begin(${1:container}), std::end($1), 0, [](int total, $2) {\n  `%`$3\n});" "accumulate w/ closure" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/accumulate-with-closure" nil nil)
                       ("acm" "auto sum = std::accumulate(std::begin(${1:container}), std::end($1), 0);" "accumulate" nil nil nil "/home/orion/.config/doom/snippets/snippets/c++-mode/accumulate" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("uni" "#include <unistd.h>" "unistd" nil nil nil "/home/orion/.config/doom/snippets/snippets/c-mode/unistd" nil "uni")
                       ("union" "typedef union {\n        $0\n} ${1:name};" "union" nil nil nil "/home/orion/.config/doom/snippets/snippets/c-mode/union" nil "union")
                       ("switch" "switch (${1:ch}) {\n       case ${2:const}:\n       ${3:a = b};\n       break;\n       ${4:default:\n       ${5:action}}\n}\n" "switch" nil nil nil "/home/orion/.config/doom/snippets/snippets/c-mode/switch" nil nil)
                       ("string" "#include <string.h>" "string" nil nil nil "/home/orion/.config/doom/snippets/snippets/c-mode/string" nil nil)
                       ("stdlib" "#include <stdlib.h>" "stdlib" nil nil nil "/home/orion/.config/doom/snippets/snippets/c-mode/stdlib" nil nil)
                       ("stdio" "#include <stdio.h>" "stdio" nil nil nil "/home/orion/.config/doom/snippets/snippets/c-mode/stdio" nil nil)
                       ("p" "printf(\"${1:format}\"${2:, $3});" "printf" nil nil nil "/home/orion/.config/doom/snippets/snippets/c-mode/printf" nil "p")
                       ("packed" "__attribute__((__packed__))$0" "packed" nil nil nil "/home/orion/.config/doom/snippets/snippets/c-mode/packed" nil "packed")
                       ("malloc" "malloc(sizeof($1)${2: * ${3:3}});\n$0" "malloc" nil nil nil "/home/orion/.config/doom/snippets/snippets/c-mode/malloc" nil "malloc")
                       ("fopen" "FILE *${f} = fopen(${\"file\"}, \"${r}\");" "FILE *fp = fopen(..., ...);" nil nil nil "/home/orion/.config/doom/snippets/snippets/c-mode/fopen" nil nil)
                       ("def" "#define $0" "define" nil nil nil "/home/orion/.config/doom/snippets/snippets/c-mode/define" nil "def")
                       ("compile" "// -*- compile-command: \"${1:gcc -Wall -o ${2:dest} ${3:file}}\" -*-" "compile" nil nil nil "/home/orion/.config/doom/snippets/snippets/c-mode/compile" nil "compile")
                       ("ass" "#include <assert.h>\n$0" "assert" nil nil nil "/home/orion/.config/doom/snippets/snippets/c-mode/assert" nil "ass")))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("while" "while (${1:condition}) {\n    $0\n}" "while" nil nil nil "/home/orion/.config/doom/snippets/snippets/cc-mode/while" nil nil)
                       ("typedef" "typedef ${1:type} ${2:alias};" "typedef" nil nil nil "/home/orion/.config/doom/snippets/snippets/cc-mode/typedef" nil nil)
                       ("?" "(${1:cond}) ? ${2:then} : ${3:else};" "ternary" nil nil nil "/home/orion/.config/doom/snippets/snippets/cc-mode/ternary" nil "?")
                       ("switch" "switch (${1:variable}) {\n    case ${2:value}: $0break;\n}\n" "switch" nil nil nil "/home/orion/.config/doom/snippets/snippets/cc-mode/switch" nil nil)
                       ("struct" "struct ${1:name} {\n    $0\n};" "struct ... { ... }" nil nil nil "/home/orion/.config/doom/snippets/snippets/cc-mode/struct" nil "struct")
                       ("once" "#ifndef ${1:`(upcase (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))`_H}\n#define $1\n\n$0\n\n#endif /* $1 */" "#ifndef XXX; #define XXX; #endif" nil nil nil "/home/orion/.config/doom/snippets/snippets/cc-mode/once" nil "once")
                       ("math" "#include <math.h>$0" "math" nil nil nil "/home/orion/.config/doom/snippets/snippets/cc-mode/math" nil nil)
                       ("main" "int main(${1:int argc, char *argv[]}) {\n    `%`$0\n    return 0;\n}" "main"
                        (doom-snippets-bolp)
                        nil nil "/home/orion/.config/doom/snippets/snippets/cc-mode/main" nil nil)
                       ("incc" "#include <$0>" "#include <...>"
                        (doom-snippets-bolp)
                        nil nil "/home/orion/.config/doom/snippets/snippets/cc-mode/incc" nil nil)
                       ("inc" "#include \"$0\"" "#include \"...\""
                        (doom-snippets-bolp)
                        nil nil "/home/orion/.config/doom/snippets/snippets/cc-mode/inc" nil nil)
                       ("ifdef" "#ifdef ${1:MACRO}\n\n$0\n\n#endif // $1" "ifdef"
                        (doom-snippets-bolp)
                        nil nil "/home/orion/.config/doom/snippets/snippets/cc-mode/ifdef" nil "ifdef")
                       ("if" "if (${1:condition}) {\n    `%`$0\n}" "if (...) { ... }" nil nil nil "/home/orion/.config/doom/snippets/snippets/cc-mode/if" nil "if")
                       ("fori" "for (${1:int} ${2:i} = 0; $2 < ${3:length}; $2++}) {\n    `%`$0\n}" "fori" nil nil nil "/home/orion/.config/doom/snippets/snippets/cc-mode/fori" nil "fori")
                       ("for" "for ($1; $2; $3) {\n    `%`$0\n}" "for" nil nil nil "/home/orion/.config/doom/snippets/snippets/cc-mode/for" nil "for")
                       ("elseif" "else if (${1:condition}) {\n    `%`$0\n}" "elseif" nil nil nil "/home/orion/.config/doom/snippets/snippets/cc-mode/elseif" nil nil)
                       ("else" "else {\n    `%`$0\n}" "else" nil nil nil "/home/orion/.config/doom/snippets/snippets/cc-mode/else" nil nil)
                       ("do" "do {\n    $0\n} while (${1:condition});" "do { ... } while (...)" nil nil nil "/home/orion/.config/doom/snippets/snippets/cc-mode/do" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("whenl" "(when-let [$1 $2]\n  $3)$>\n  $0$>" "whenl" nil nil nil "/home/orion/.config/doom/snippets/snippets/clojure-mode/whenl" nil nil)
                       ("when" "(when $1\n      $2)$>\n$0$>" "when" nil nil nil "/home/orion/.config/doom/snippets/snippets/clojure-mode/when" nil nil)
                       ("use" "(:use [$1 :refer [$2]])$>" "use" nil nil
                        ((yas-triggers-in-field nil))
                        "/home/orion/.config/doom/snippets/snippets/clojure-mode/use" nil nil)
                       ("try" "(try\n$1$>\n(catch ${2:Exception} e$>\n$3$>))$>" "try" nil nil nil "/home/orion/.config/doom/snippets/snippets/clojure-mode/try" nil nil)
                       ("test" "(deftest $1\n  (is (= $2))$>\n  $0)$>" "test" nil nil nil "/home/orion/.config/doom/snippets/snippets/clojure-mode/test" nil nil)
                       ("require" "(:require [$1 :as $2])$>" "require" nil nil
                        ((yas-triggers-in-field nil))
                        "/home/orion/.config/doom/snippets/snippets/clojure-mode/require" nil nil)
                       ("reduce" "(reduce ${1:(fn [p n] $0)} $2)" "reduce" nil nil nil "/home/orion/.config/doom/snippets/snippets/clojure-mode/reduce" nil nil)
                       ("print" "(println $1)\n$0" "print" nil nil nil "/home/orion/.config/doom/snippets/snippets/clojure-mode/print" nil nil)
                       ("pr" "(prn $1)\n$0" "pr" nil nil nil "/home/orion/.config/doom/snippets/snippets/clojure-mode/pr" nil nil)
                       ("opts" "{:keys [$1]$>\n :or {$2}$>\n :as $3}$>" "opts" nil nil nil "/home/orion/.config/doom/snippets/snippets/clojure-mode/opts" nil nil)
                       ("ns" "(ns `(cl-flet ((try-src-prefix\n		(path src-pfx)\n		(let ((parts (split-string path src-pfx)))\n		  (if (= 2 (length parts))\n		      (cl-second parts)\n		    nil))))\n       (let* ((p (buffer-file-name))\n	      (p2 (cl-first\n		   (cl-remove-if-not '(lambda (x) x)\n				     (mapcar\n				      '(lambda (pfx)\n					 (try-src-prefix p pfx))\n				      '(\"/src/cljs/\" \"/src/clj/\" \"/src/\" \"/test/\")))))\n	      (p3 (file-name-sans-extension p2))\n	      (p4 (mapconcat '(lambda (x) x)\n			     (split-string p3 \"/\")\n			     \".\")))\n	 (replace-regexp-in-string \"_\" \"-\" p4)))`)" "ns" nil nil nil "/home/orion/.config/doom/snippets/snippets/clojure-mode/ns" nil nil)
                       ("mdoc" "^{:doc \"$1\"}" "mdoc" nil nil nil "/home/orion/.config/doom/snippets/snippets/clojure-mode/mdoc" nil nil)
                       ("map" "(map #($1) $2)$>" "map lambda" nil nil nil "/home/orion/.config/doom/snippets/snippets/clojure-mode/map.lambda" nil nil)
                       ("map" "(map $1 $2)" "map" nil nil nil "/home/orion/.config/doom/snippets/snippets/clojure-mode/map" nil nil)
                       ("let" "(let [$1 $2]$>\n  $3)$>\n$0" "let" nil nil nil "/home/orion/.config/doom/snippets/snippets/clojure-mode/let" nil nil)
                       ("is" "(is (= $1 $2))" "is" nil nil nil "/home/orion/.config/doom/snippets/snippets/clojure-mode/is" nil nil)
                       ("import" "(:import ($1))$>" "import" nil nil
                        ((yas-triggers-in-field nil))
                        "/home/orion/.config/doom/snippets/snippets/clojure-mode/import" nil nil)
                       ("ifl" "(if-let [$1 $2]\n  $3)$>\n$0" "ifl" nil nil nil "/home/orion/.config/doom/snippets/snippets/clojure-mode/ifl" nil nil)
                       ("if" "(if $1\n  $2$>\n  $3)$>\n$0" "if" nil nil nil "/home/orion/.config/doom/snippets/snippets/clojure-mode/if" nil nil)
                       ("for" "(for [$1 $2]\n  $3)$>" "for" nil nil nil "/home/orion/.config/doom/snippets/snippets/clojure-mode/for" nil nil)
                       ("fn" "(fn [$1]\n  $0)$>" "fn" nil nil nil "/home/orion/.config/doom/snippets/snippets/clojure-mode/fn" nil nil)
                       ("doseq" "(doseq [$1 $2]\n  $3)$>\n$0" "doseq" nil nil nil "/home/orion/.config/doom/snippets/snippets/clojure-mode/doseq" nil nil)
                       ("deft" "(deftype\n  ^{\"$1\"}$>\n  $2$>\n  [$3]$>\n  $0)$>" "deftype" nil nil nil "/home/orion/.config/doom/snippets/snippets/clojure-mode/deft" nil nil)
                       ("defr" "(defrecord\n  ^{\"$1\"}$>\n  $2$>\n  [$3]$>\n  $0)$>" "defrecord" nil nil nil "/home/orion/.config/doom/snippets/snippets/clojure-mode/defr" nil nil)
                       ("defn" "(defn $1\n  \"$2\"$>\n  [$3]$>\n  $0)$>" "defn" nil nil nil "/home/orion/.config/doom/snippets/snippets/clojure-mode/defn" nil nil)
                       ("defm" "(defmacro $1\n  \"$2\"$>\n  [$3]$>\n  $0)$>" "defmacro" nil nil nil "/home/orion/.config/doom/snippets/snippets/clojure-mode/defm" nil nil)
                       ("def" "(def $0)" "def" nil nil nil "/home/orion/.config/doom/snippets/snippets/clojure-mode/def" nil nil)
                       ("bp" "(swank.core/break)" "bp" nil nil nil "/home/orion/.config/doom/snippets/snippets/clojure-mode/bp" nil nil)
                       ("bench" "(dotimes [_ 5 ]$>\n  (time (dotimes [i 1000000]$>\n  $0$>\n  )))$>" "bench" nil nil nil "/home/orion/.config/doom/snippets/snippets/clojure-mode/bench" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("set" "set(${1:var} ${2:value})" "set" nil nil nil "/home/orion/.config/doom/snippets/snippets/cmake-mode/set" nil "set")
                       ("proj" "project ($0)" "project" nil nil nil "/home/orion/.config/doom/snippets/snippets/cmake-mode/project" nil "proj")
                       ("opt" "option (${1:OPT} \"${2:docstring}\" ${3:value})" "option" nil nil nil "/home/orion/.config/doom/snippets/snippets/cmake-mode/option" nil "opt")
                       ("msg" "message(${1:STATUS }\"$0\")" "message" nil nil nil "/home/orion/.config/doom/snippets/snippets/cmake-mode/message" nil "msg")
                       ("macro" "macro(${1:name}${2: args})\n\nendmacro()" "macro" nil nil nil "/home/orion/.config/doom/snippets/snippets/cmake-mode/macro" nil "macro")
                       ("inc"
                        (progninclude
                         ($0))
                        "include" nil nil nil "/home/orion/.config/doom/snippets/snippets/cmake-mode/include" nil "inc")
                       ("if" "if(${1:cond})\n        $2\nelse(${3:cond})\n        $0\nendif()" "ifelse" nil nil nil "/home/orion/.config/doom/snippets/snippets/cmake-mode/ifelse" nil "if")
                       ("if" "if(${1:cond})\n   $0\nendif()" "if" nil nil nil "/home/orion/.config/doom/snippets/snippets/cmake-mode/if" nil "if")
                       ("fun" "function (${1:name})\n         $0\nendfunction()" "function" nil nil nil "/home/orion/.config/doom/snippets/snippets/cmake-mode/function" nil "fun")
                       ("for" "foreach(${1:item} \\${${2:array}})\n        $0\nendforeach()" "foreach" nil nil nil "/home/orion/.config/doom/snippets/snippets/cmake-mode/foreach" nil "for")
                       ("min" "cmake_minimum_required(VERSION ${1:2.6})" "cmake_minimum_required" nil nil nil "/home/orion/.config/doom/snippets/snippets/cmake-mode/cmake_minimum_required" nil "min")))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("sec" "Section \"${1:Device}\"\n        $0\nEndSection" "section" nil nil nil "/home/orion/.config/doom/snippets/snippets/conf-unix-mode/section" nil "sec")))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("wlog" "wlog : / $1\n" "wlog" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/wlog" nil "wlog")
                       ("wilog" "without loss $1\n" "without loss" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/without_loss" nil "wilog")
                       ("vmc" "vm_compute.\n" "vm_compute" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/vm_compute" nil "vmc")
                       ("varv" "Variant $1 : $2 := $3 : $4.\n" "Variant" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/variant" nil "varv")
                       ("vs" "Variables $1 , $2: $3.\n" "Variables" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/variables" nil "vs")
                       ("v" "Variable $1: $2.\n" "Variable" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/variable" nil "v")
                       ("unset_verbose_compat_notations" "Unset Verbose Compat Notations\n" "Unset Verbose Compat Notations" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_verbose_compat_notations" nil nil)
                       ("unset_universe_polymorphism" "Unset Universe Polymorphism\n" "Unset Universe Polymorphism" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_universe_polymorphism" nil nil)
                       ("unset_universe_minimization_tounset" "Unset Universe Minimization ToUnset\n" "Unset Universe Minimization ToUnset" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_universe_minimization_tounset" nil nil)
                       ("unset_universal_lemma_under_conjunction" "Unset Universal Lemma Under Conjunction\n" "Unset Universal Lemma Under Conjunction" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_universal_lemma_under_conjunction" nil nil)
                       ("unset_undo" "Unset Undo\n" "Unset Undo" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_undo" nil nil)
                       ("unset_typeclasses_unique_solutions" "Unset Typeclasses Unique Solutions\n" "Unset Typeclasses Unique Solutions" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_typeclasses_unique_solutions" nil nil)
                       ("unset_typeclasses_unique_instances" "Unset Typeclasses Unique Instances\n" "Unset Typeclasses Unique Instances" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_typeclasses_unique_instances" nil nil)
                       ("unset_typeclasses_strict_resolution" "Unset Typeclasses Strict Resolution\n" "Unset Typeclasses Strict Resolution" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_typeclasses_strict_resolution" nil nil)
                       ("unset_typeclasses_modulo_eta" "Unset Typeclasses Modulo Eta\n" "Unset Typeclasses Modulo Eta" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_typeclasses_modulo_eta" nil nil)
                       ("unset_typeclasses_depth" "Unset Typeclasses Depth\n" "Unset Typeclasses Depth" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_typeclasses_depth" nil nil)
                       ("unset_typeclasses_dependency_order" "Unset Typeclasses Dependency Order\n" "Unset Typeclasses Dependency Order" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_typeclasses_dependency_order" nil nil)
                       ("unset_typeclasses_debug" "Unset Typeclasses Debug\n" "Unset Typeclasses Debug" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_typeclasses_debug" nil nil)
                       ("unset_typeclass_resolution_for_conversion" "Unset Typeclass Resolution For Conversion\n" "Unset Typeclass Resolution For Conversion" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_typeclass_resolution_for_conversion" nil nil)
                       ("unset_typeclass_resolution_after_apply" "Unset Typeclass Resolution After Apply\n" "Unset Typeclass Resolution After Apply" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_typeclass_resolution_after_apply" nil nil)
                       ("unset_transparent_obligations" "Unset Transparent Obligations\n" "Unset Transparent Obligations" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_transparent_obligations" nil nil)
                       ("unset_tactic_pattern_unification" "Unset Tactic Pattern Unification\n" "Unset Tactic Pattern Unification" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_tactic_pattern_unification" nil nil)
                       ("unset_tactic_evars_pattern_unification" "Unset Tactic Evars Pattern Unification\n" "Unset Tactic Evars Pattern Unification" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_tactic_evars_pattern_unification" nil nil)
                       ("unset_tactic_compat_context" "Unset Tactic Compat Context\n" "Unset Tactic Compat Context" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_tactic_compat_context" nil nil)
                       ("unset_suggest_proof_using" "Unset Suggest Proof Using\n" "Unset Suggest Proof Using" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_suggest_proof_using" nil nil)
                       ("unset_strongly_strict_implicit" "Unset Strongly Strict Implicit\n" "Unset Strongly Strict Implicit" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_strongly_strict_implicit" nil nil)
                       ("unset_strict_universe_declaration" "Unset Strict Universe Declaration\n" "Unset Strict Universe Declaration" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_strict_universe_declaration" nil nil)
                       ("unset_strict_proofs" "Unset Strict Proofs\n" "Unset Strict Proofs" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_strict_proofs" nil nil)
                       ("unset_standard_proposition_elimination_names" "Unset Standard Proposition Elimination Names\n" "Unset Standard Proposition Elimination Names" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_standard_proposition_elimination_names" nil nil)
                       ("unset_simpliscbn" "Unset SimplIsCbn\n" "Unset SimplIsCbn" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_simpliscbn" nil nil)
                       ("unset_silent" "Unset Silent\n" "Unset Silent" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_silent" nil nil)
                       ("unset_shrink_obligations" "Unset Shrink Obligations\n" "Unset Shrink Obligations" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_shrink_obligations" nil nil)
                       ("unset_short_module_printing" "Unset Short Module Printing\n" "Unset Short Module Printing" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_short_module_printing" nil nil)
                       ("unset_search_blacklist" "Unset Search Blacklist\n" "Unset Search Blacklist" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_search_blacklist" nil nil)
                       ("unset_rewriting_schemes" "Unset Rewriting Schemes\n" "Unset Rewriting Schemes" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_rewriting_schemes" nil nil)
                       ("unset_reversible_pattern_implicit" "Unset Reversible Pattern Implicit\n" "Unset Reversible Pattern Implicit" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_reversible_pattern_implicit" nil nil)
                       ("unset_regular_subst_tactic" "Unset Regular Subst Tactic\n" "Unset Regular Subst Tactic" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_regular_subst_tactic" nil nil)
                       ("unset_refine_instance_mode" "Unset Refine Instance Mode\n" "Unset Refine Instance Mode" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_refine_instance_mode" nil nil)
                       ("unset_record_elimination_schemes" "Unset Record Elimination Schemes\n" "Unset Record Elimination Schemes" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_record_elimination_schemes" nil nil)
                       ("unset_proof_using_clear_unused" "Unset Proof Using Clear Unused\n" "Unset Proof Using Clear Unused" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_proof_using_clear_unused" nil nil)
                       ("unset_program_mode" "Unset Program Mode\n" "Unset Program Mode" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_program_mode" nil nil)
                       ("unset_printing_wildcard" "Unset Printing Wildcard\n" "Unset Printing Wildcard" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_printing_wildcard" nil nil)
                       ("unset_printing_width" "Unset Printing Width\n" "Unset Printing Width" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_printing_width" nil nil)
                       ("unset_printing_universes" "Unset Printing Universes\n" "Unset Printing Universes" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_printing_universes" nil nil)
                       ("unset_printing_synth" "Unset Printing Synth\n" "Unset Printing Synth" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_printing_synth" nil nil)
                       ("unset_printing_records" "Unset Printing Records\n" "Unset Printing Records" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_printing_records" nil nil)
                       ("unset_printing_record" "Unset Printing Record\n" "Unset Printing Record" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_printing_record" nil nil)
                       ("unset_printing_projections" "Unset Printing Projections\n" "Unset Printing Projections" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_printing_projections" nil nil)
                       ("unset_printing_primitive_projection_parameters" "Unset Printing Primitive Projection Parameters\n" "Unset Printing Primitive Projection Parameters" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_printing_primitive_projection_parameters" nil nil)
                       ("unset_printing_primitive_projection_compatibility" "Unset Printing Primitive Projection Compatibility\n" "Unset Printing Primitive Projection Compatibility" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_printing_primitive_projection_compatibility" nil nil)
                       ("unset_printing_notations" "Unset Printing Notations\n" "Unset Printing Notations" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_printing_notations" nil nil)
                       ("unset_printing_matching" "Unset Printing Matching\n" "Unset Printing Matching" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_printing_matching" nil nil)
                       ("unset_printing_let" "Unset Printing Let\n" "Unset Printing Let" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_printing_let" nil nil)
                       ("unset_printing_implicit_defensive" "Unset Printing Implicit Defensive\n" "Unset Printing Implicit Defensive" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_printing_implicit_defensive" nil nil)
                       ("unset_printing_implicit" "Unset Printing Implicit\n" "Unset Printing Implicit" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_printing_implicit" nil nil)
                       ("unset_printing_if" "Unset Printing If\n" "Unset Printing If" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_printing_if" nil nil)
                       ("unset_printing_existential_instances" "Unset Printing Existential Instances\n" "Unset Printing Existential Instances" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_printing_existential_instances" nil nil)
                       ("unset_printing_depth" "Unset Printing Depth\n" "Unset Printing Depth" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_printing_depth" nil nil)
                       ("unset_printing_constructor" "Unset Printing Constructor\n" "Unset Printing Constructor" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_printing_constructor" nil nil)
                       ("unset_printing_coercions" "Unset Printing Coercions\n" "Unset Printing Coercions" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_printing_coercions" nil nil)
                       ("unset_printing_coercion" "Unset Printing Coercion\n" "Unset Printing Coercion" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_printing_coercion" nil nil)
                       ("unset_printing_all" "Unset Printing All\n" "Unset Printing All" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_printing_all" nil nil)
                       ("unset_primitive_projections" "Unset Primitive Projections\n" "Unset Primitive Projections" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_primitive_projections" nil nil)
                       ("unset_parsing_explicit" "Unset Parsing Explicit\n" "Unset Parsing Explicit" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_parsing_explicit" nil nil)
                       ("unset_nonrecursive_elimination_schemes" "Unset Nonrecursive Elimination Schemes\n" "Unset Nonrecursive Elimination Schemes" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_nonrecursive_elimination_schemes" nil nil)
                       ("unset_maximal_implicit_insertion" "Unset Maximal Implicit Insertion\n" "Unset Maximal Implicit Insertion" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_maximal_implicit_insertion" nil nil)
                       ("unset_ltac_debug" "Unset Ltac Debug\n" "Unset Ltac Debug" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_ltac_debug" nil nil)
                       ("unset_loose_hint_behavior" "Unset Loose Hint Behavior\n" "Unset Loose Hint Behavior" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_loose_hint_behavior" nil nil)
                       ("unset_keyed_unification" "Unset Keyed Unification\n" "Unset Keyed Unification" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_keyed_unification" nil nil)
                       ("unset_kernel_term_sharing" "Unset Kernel Term Sharing\n" "Unset Kernel Term Sharing" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_kernel_term_sharing" nil nil)
                       ("unset_intuition_negation_unfolding" "Unset Intuition Negation Unfolding\n" "Unset Intuition Negation Unfolding" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_intuition_negation_unfolding" nil nil)
                       ("unset_intuition_iff_unfolding" "Unset Intuition Iff Unfolding\n" "Unset Intuition Iff Unfolding" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_intuition_iff_unfolding" nil nil)
                       ("unset_inline_level" "Unset Inline Level\n" "Unset Inline Level" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_inline_level" nil nil)
                       ("unset_injection_on_proofs" "Unset Injection On Proofs\n" "Unset Injection On Proofs" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_injection_on_proofs" nil nil)
                       ("unset_injection_l2r_pattern_order" "Unset Injection L2R Pattern Order\n" "Unset Injection L2R Pattern Order" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_injection_l2r_pattern_order" nil nil)
                       ("unset_info_trivial" "Unset Info Trivial\n" "Unset Info Trivial" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_info_trivial" nil nil)
                       ("unset_info_level" "Unset Info Level\n" "Unset Info Level" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_info_level" nil nil)
                       ("unset_info_eauto" "Unset Info Eauto\n" "Unset Info Eauto" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_info_eauto" nil nil)
                       ("unset_info_auto" "Unset Info Auto\n" "Unset Info Auto" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_info_auto" nil nil)
                       ("unset_implicit_arguments" "Unset Implicit Arguments\n" "Unset Implicit Arguments" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_implicit_arguments" nil nil)
                       ("unset_hyps_limit" "Unset Hyps Limit\n" "Unset Hyps Limit" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_hyps_limit" nil nil)
                       ("unset_hide_obligations" "Unset Hide Obligations\n" "Unset Hide Obligations" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_hide_obligations" nil nil)
                       ("unset_functional_induction_rewrite_dependent" "Unset Functional Induction Rewrite Dependent\n" "Unset Functional Induction Rewrite Dependent" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_functional_induction_rewrite_dependent" nil nil)
                       ("unset_function_raw_tcc" "Unset Function_raw_tcc\n" "Unset Function_raw_tcc" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_function_raw_tcc" nil nil)
                       ("unset_function_debug" "Unset Function_debug\n" "Unset Function_debug" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_function_debug" nil nil)
                       ("unset_firstorder_depth" "Unset Firstorder Depth\n" "Unset Firstorder Depth" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_firstorder_depth" nil nil)
                       ("unset_extraction_typeexpand" "Unset Extraction TypeExpand\n" "Unset Extraction TypeExpand" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_extraction_typeexpand" nil nil)
                       ("unset_extraction_safeimplicits" "Unset Extraction SafeImplicits\n" "Unset Extraction SafeImplicits" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_extraction_safeimplicits" nil nil)
                       ("unset_extraction_optimize" "Unset Extraction Optimize\n" "Unset Extraction Optimize" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_extraction_optimize" nil nil)
                       ("unset_extraction_keepsingleton" "Unset Extraction KeepSingleton\n" "Unset Extraction KeepSingleton" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_extraction_keepsingleton" nil nil)
                       ("unset_extraction_flag" "Unset Extraction Flag\n" "Unset Extraction Flag" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_extraction_flag" nil nil)
                       ("unset_extraction_file_comment" "Unset Extraction File Comment\n" "Unset Extraction File Comment" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_extraction_file_comment" nil nil)
                       ("unset_extraction_conservative_types" "Unset Extraction Conservative Types\n" "Unset Extraction Conservative Types" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_extraction_conservative_types" nil nil)
                       ("unset_extraction_autoinline" "Unset Extraction AutoInline\n" "Unset Extraction AutoInline" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_extraction_autoinline" nil nil)
                       ("unset_extraction_accessopaque" "Unset Extraction AccessOpaque\n" "Unset Extraction AccessOpaque" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_extraction_accessopaque" nil nil)
                       ("unset_equality_scheme" "Unset Equality Scheme\n" "Unset Equality Scheme" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_equality_scheme" nil nil)
                       ("unset_elimination_schemes" "Unset Elimination Schemes\n" "Unset Elimination Schemes" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_elimination_schemes" nil nil)
                       ("unset_dump_bytecode" "Unset Dump Bytecode\n" "Unset Dump Bytecode" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_dump_bytecode" nil nil)
                       ("unset_discriminate_introduction" "Unset Discriminate Introduction\n" "Unset Discriminate Introduction" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_discriminate_introduction" nil nil)
                       ("unset_dependent_propositions_elimination" "Unset Dependent Propositions Elimination\n" "Unset Dependent Propositions Elimination" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_dependent_propositions_elimination" nil nil)
                       ("unset_default_timeout" "Unset Default Timeout\n" "Unset Default Timeout" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_default_timeout" nil nil)
                       ("unset_default_proof_using" "Unset Default Proof Using\n" "Unset Default Proof Using" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_default_proof_using" nil nil)
                       ("unset_default_proof_mode" "Unset Default Proof Mode\n" "Unset Default Proof Mode" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_default_proof_mode" nil nil)
                       ("unset_default_goal_selector" "Unset Default Goal Selector\n" "Unset Default Goal Selector" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_default_goal_selector" nil nil)
                       ("unset_default_clearing_used_hypotheses" "Unset Default Clearing Used Hypotheses\n" "Unset Default Clearing Used Hypotheses" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_default_clearing_used_hypotheses" nil nil)
                       ("unset_decidable_equality_schemes" "Unset Decidable Equality Schemes\n" "Unset Decidable Equality Schemes" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_decidable_equality_schemes" nil nil)
                       ("unset_debug_unification" "Unset Debug Unification\n" "Unset Debug Unification" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_debug_unification" nil nil)
                       ("unset_debug_trivial" "Unset Debug Trivial\n" "Unset Debug Trivial" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_debug_trivial" nil nil)
                       ("unset_debug_tactic_unification" "Unset Debug Tactic Unification\n" "Unset Debug Tactic Unification" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_debug_tactic_unification" nil nil)
                       ("unset_debug_rakam" "Unset Debug RAKAM\n" "Unset Debug RAKAM" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_debug_rakam" nil nil)
                       ("unset_debug_eauto" "Unset Debug Eauto\n" "Unset Debug Eauto" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_debug_eauto" nil nil)
                       ("unset_debug_auto" "Unset Debug Auto\n" "Unset Debug Auto" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_debug_auto" nil nil)
                       ("unset_contextual_implicit" "Unset Contextual Implicit\n" "Unset Contextual Implicit" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_contextual_implicit" nil nil)
                       ("unset_congruence_verbose" "Unset Congruence Verbose\n" "Unset Congruence Verbose" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_congruence_verbose" nil nil)
                       ("unset_congruence_depth" "Unset Congruence Depth\n" "Unset Congruence Depth" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_congruence_depth" nil nil)
                       ("unset_compat_notations" "Unset Compat Notations\n" "Unset Compat Notations" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_compat_notations" nil nil)
                       ("unset_case_analysis_schemes" "Unset Case Analysis Schemes\n" "Unset Case Analysis Schemes" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_case_analysis_schemes" nil nil)
                       ("unset_bullet_behavior" "Unset Bullet Behavior\n" "Unset Bullet Behavior" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_bullet_behavior" nil nil)
                       ("unset_bracketing_last_introduction_pattern" "Unset Bracketing Last Introduction Pattern\n" "Unset Bracketing Last Introduction Pattern" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_bracketing_last_introduction_pattern" nil nil)
                       ("unset_boolean_equality_schemes" "Unset Boolean Equality Schemes\n" "Unset Boolean Equality Schemes" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_boolean_equality_schemes" nil nil)
                       ("unset_automatic_introduction" "Unset Automatic Introduction\n" "Unset Automatic Introduction" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_automatic_introduction" nil nil)
                       ("unset_automatic_coercions_import" "Unset Automatic Coercions Import\n" "Unset Automatic Coercions Import" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_automatic_coercions_import" nil nil)
                       ("unset_atomic_load" "Unset Atomic Load\n" "Unset Atomic Load" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_atomic_load" nil nil)
                       ("unset_asymmetric_patterns" "Unset Asymmetric Patterns\n" "Unset Asymmetric Patterns" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unset_asymmetric_patterns" nil nil)
                       ("unlock" "unlock $1\n" "unlock" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unlock" nil "unlock")
                       ("u" "unfold $1\n" "unfold" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unfold" nil "u")
                       ("unfocus" "Unfocus.\n" "Unfocus" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/unfocus" nil nil)
                       ("typeclasses_opaque" "Typeclasses Opaque $1.\n" "Typeclasses Opaque" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/typeclasses_opaque" nil nil)
                       ("tryif" "tryif $1 then $2 else $3\n" "tryif" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/tryif" nil "tryif")
                       ("try" "try $1\n" "try" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/try" nil nil)
                       ("t" "trivial\n" "trivial" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/trivial" nil "t")
                       ("transparent" "Transparent $1.\n" "Transparent" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/transparent" nil nil)
                       ("trans" "transitivity $1\n" "transitivity" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/transitivity" nil "trans")
                       ("timeout" "Timeout\n" "Timeout" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/timeout" nil nil)
                       ("th" "Theorem $1 : $2.\n$3\nQed.\n" "Theorem" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/theorem" nil "th")
                       ("ta" "tauto\n" "tauto" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/tauto" nil "ta")
                       ("tactic_notation" "Tactic Notation $1 := $2.\n" "Tactic Notation" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/tactic_notation" nil nil)
                       ("sy" "symmetry\n" "symmetry" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/symmetry" nil "sy")
                       ("suffices" "suffices $1 : $2\n" "suffices" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/suffices" nil "suffices")
                       ("suff" "suff $1 : $2\n" "suff" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/suff" nil "suff")
                       ("su" "subst $1\n" "subst" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/subst" nil "su")
                       ("str" "Structure $1 : $2 := {\n$3 : $4;\n$5 : $6 }\n" "Structure" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/structure" nil "str")
                       ("strategy" "Strategy $1 [$2].\n" "Strategy" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/strategy" nil nil)
                       ("str" "stepr $1\n" "stepr" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/stepr" nil "str")
                       ("stl" "stepl $1\n" "stepl" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/stepl" nil "stl")
                       ("sprm" "splitRmult\n" "split_Rmult" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/split_rmult" nil "sprm")
                       ("spra" "splitRabs\n" "split_Rabs" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/split_rabs" nil "spra")
                       ("sp" "split\n" "split" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/split" nil "sp")
                       ("spec" "specialize\n" "specialize" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/specialize" nil "spec")
                       ("oblssolve" "Solve Obligations using $1.\n" "Solve Obligations" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/solve_obligations" nil "oblssolve")
                       ("solve" "solve [ $1 | $2 ]\n" "solve" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/solve" nil nil)
                       ("simeq" "simplify_eq ${1:hyp}\n" "simplify_eq" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/simplify_eq" nil "simeq")
                       ("sinv" "simple inversion\n" "simple inversion" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/simple_inversion" nil "sinv")
                       ("sind" "simple induction\n" "simple induction" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/simple_induction" nil "sind")
                       ("sdes" "simple destruct\n" "simple destruct" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/simple_destruct" nil "sdes")
                       ("s" "simpl\n" "simpl" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/simpl" nil "s")
                       ("show" "Show $1.\n" "Show" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/show" nil nil)
                       ("strew" "setoid rewrite $1\n" "setoid rewrite" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/setoid_rewrite" nil "strew")
                       ("strep" "setoid replace $1 with $2\n" "setoid replace with" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/setoid_replace_with" nil "strep")
                       ("set_verbose_compat_notations" "Set Verbose Compat Notations\n" "Set Verbose Compat Notations" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_verbose_compat_notations" nil nil)
                       ("set_universe_polymorphism" "Set Universe Polymorphism\n" "Set Universe Polymorphism" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_universe_polymorphism" nil nil)
                       ("set_universe_minimization_toset" "Set Universe Minimization ToSet\n" "Set Universe Minimization ToSet" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_universe_minimization_toset" nil nil)
                       ("set_universal_lemma_under_conjunction" "Set Universal Lemma Under Conjunction\n" "Set Universal Lemma Under Conjunction" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_universal_lemma_under_conjunction" nil nil)
                       ("set_undo" "Set Undo\n" "Set Undo" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_undo" nil nil)
                       ("set_typeclasses_unique_solutions" "Set Typeclasses Unique Solutions\n" "Set Typeclasses Unique Solutions" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_typeclasses_unique_solutions" nil nil)
                       ("set_typeclasses_unique_instances" "Set Typeclasses Unique Instances\n" "Set Typeclasses Unique Instances" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_typeclasses_unique_instances" nil nil)
                       ("set_typeclasses_strict_resolution" "Set Typeclasses Strict Resolution\n" "Set Typeclasses Strict Resolution" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_typeclasses_strict_resolution" nil nil)
                       ("set_typeclasses_modulo_eta" "Set Typeclasses Modulo Eta\n" "Set Typeclasses Modulo Eta" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_typeclasses_modulo_eta" nil nil)
                       ("set_typeclasses_depth" "Set Typeclasses Depth\n" "Set Typeclasses Depth" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_typeclasses_depth" nil nil)
                       ("set_typeclasses_dependency_order" "Set Typeclasses Dependency Order\n" "Set Typeclasses Dependency Order" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_typeclasses_dependency_order" nil nil)
                       ("set_typeclasses_debug" "Set Typeclasses Debug\n" "Set Typeclasses Debug" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_typeclasses_debug" nil nil)
                       ("set_typeclass_resolution_for_conversion" "Set Typeclass Resolution For Conversion\n" "Set Typeclass Resolution For Conversion" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_typeclass_resolution_for_conversion" nil nil)
                       ("set_typeclass_resolution_after_apply" "Set Typeclass Resolution After Apply\n" "Set Typeclass Resolution After Apply" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_typeclass_resolution_after_apply" nil nil)
                       ("set_transparent_obligations" "Set Transparent Obligations\n" "Set Transparent Obligations" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_transparent_obligations" nil nil)
                       ("set_tactic_pattern_unification" "Set Tactic Pattern Unification\n" "Set Tactic Pattern Unification" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_tactic_pattern_unification" nil nil)
                       ("set_tactic_evars_pattern_unification" "Set Tactic Evars Pattern Unification\n" "Set Tactic Evars Pattern Unification" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_tactic_evars_pattern_unification" nil nil)
                       ("set_tactic_compat_context" "Set Tactic Compat Context\n" "Set Tactic Compat Context" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_tactic_compat_context" nil nil)
                       ("set_suggest_proof_using" "Set Suggest Proof Using\n" "Set Suggest Proof Using" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_suggest_proof_using" nil nil)
                       ("set_strongly_strict_implicit" "Set Strongly Strict Implicit\n" "Set Strongly Strict Implicit" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_strongly_strict_implicit" nil nil)
                       ("set_strict_universe_declaration" "Set Strict Universe Declaration\n" "Set Strict Universe Declaration" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_strict_universe_declaration" nil nil)
                       ("set_strict_proofs" "Set Strict Proofs\n" "Set Strict Proofs" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_strict_proofs" nil nil)
                       ("set_standard_proposition_elimination_names" "Set Standard Proposition Elimination Names\n" "Set Standard Proposition Elimination Names" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_standard_proposition_elimination_names" nil nil)
                       ("set_simpliscbn" "Set SimplIsCbn\n" "Set SimplIsCbn" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_simpliscbn" nil nil)
                       ("set_silent" "Set Silent\n" "Set Silent" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_silent" nil nil)
                       ("set_shrink_obligations" "Set Shrink Obligations\n" "Set Shrink Obligations" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_shrink_obligations" nil nil)
                       ("set_short_module_printing" "Set Short Module Printing\n" "Set Short Module Printing" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_short_module_printing" nil nil)
                       ("set_search_blacklist" "Set Search Blacklist\n" "Set Search Blacklist" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_search_blacklist" nil nil)
                       ("set_rewriting_schemes" "Set Rewriting Schemes\n" "Set Rewriting Schemes" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_rewriting_schemes" nil nil)
                       ("set_reversible_pattern_implicit" "Set Reversible Pattern Implicit\n" "Set Reversible Pattern Implicit" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_reversible_pattern_implicit" nil nil)
                       ("set_regular_subst_tactic" "Set Regular Subst Tactic\n" "Set Regular Subst Tactic" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_regular_subst_tactic" nil nil)
                       ("set_refine_instance_mode" "Set Refine Instance Mode\n" "Set Refine Instance Mode" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_refine_instance_mode" nil nil)
                       ("set_record_elimination_schemes" "Set Record Elimination Schemes\n" "Set Record Elimination Schemes" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_record_elimination_schemes" nil nil)
                       ("set_proof_using_clear_unused" "Set Proof Using Clear Unused\n" "Set Proof Using Clear Unused" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_proof_using_clear_unused" nil nil)
                       ("set_program_mode" "Set Program Mode\n" "Set Program Mode" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_program_mode" nil nil)
                       ("set_printing_wildcard" "Set Printing Wildcard\n" "Set Printing Wildcard" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_printing_wildcard" nil nil)
                       ("set_printing_width" "Set Printing Width\n" "Set Printing Width" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_printing_width" nil nil)
                       ("set_printing_universes" "Set Printing Universes\n" "Set Printing Universes" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_printing_universes" nil nil)
                       ("set_printing_unfocused" "Set Printing Unfocused\n" "Set Printing Unfocused" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_printing_unfocused" nil nil)
                       ("set_printing_synth" "Set Printing Synth\n" "Set Printing Synth" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_printing_synth" nil nil)
                       ("set_printing_records" "Set Printing Records\n" "Set Printing Records" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_printing_records" nil nil)
                       ("set_printing_record" "Set Printing Record\n" "Set Printing Record" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_printing_record" nil nil)
                       ("set_printing_projections" "Set Printing Projections\n" "Set Printing Projections" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_printing_projections" nil nil)
                       ("set_printing_primitive_projection_parameters" "Set Printing Primitive Projection Parameters\n" "Set Printing Primitive Projection Parameters" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_printing_primitive_projection_parameters" nil nil)
                       ("set_printing_primitive_projection_compatibility" "Set Printing Primitive Projection Compatibility\n" "Set Printing Primitive Projection Compatibility" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_printing_primitive_projection_compatibility" nil nil)
                       ("set_printing_notations" "Set Printing Notations\n" "Set Printing Notations" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_printing_notations" nil nil)
                       ("set_printing_matching" "Set Printing Matching\n" "Set Printing Matching" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_printing_matching" nil nil)
                       ("set_printing_let" "Set Printing Let\n" "Set Printing Let" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_printing_let" nil nil)
                       ("set_printing_implicit_defensive" "Set Printing Implicit Defensive\n" "Set Printing Implicit Defensive" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_printing_implicit_defensive" nil nil)
                       ("set_printing_implicit" "Set Printing Implicit\n" "Set Printing Implicit" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_printing_implicit" nil nil)
                       ("set_printing_if" "Set Printing If\n" "Set Printing If" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_printing_if" nil nil)
                       ("set_printing_goal_tags" "Set Printing Goal Tags\n" "Set Printing Goal Tags" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_printing_goal_tags" nil nil)
                       ("set_printing_goal_names" "Set Printing Goal Names\n" "Set Printing Goal Names" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_printing_goal_names" nil nil)
                       ("set_printing_existential_instances" "Set Printing Existential Instances\n" "Set Printing Existential Instances" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_printing_existential_instances" nil nil)
                       ("set_printing_depth" "Set Printing Depth\n" "Set Printing Depth" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_printing_depth" nil nil)
                       ("set_printing_constructor" "Set Printing Constructor\n" "Set Printing Constructor" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_printing_constructor" nil nil)
                       ("set_printing_compact_contexts" "Set Printing Compact Contexts\n" "Set Printing Compact Contexts" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_printing_compact_contexts" nil nil)
                       ("set_printing_coercions" "Set Printing Coercions\n" "Set Printing Coercions" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_printing_coercions" nil nil)
                       ("set_printing_coercion" "Set Printing Coercion\n" "Set Printing Coercion" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_printing_coercion" nil nil)
                       ("set_printing_all" "Set Printing All\n" "Set Printing All" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_printing_all" nil nil)
                       ("set_primitive_projections" "Set Primitive Projections\n" "Set Primitive Projections" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_primitive_projections" nil nil)
                       ("set_parsing_explicit" "Set Parsing Explicit\n" "Set Parsing Explicit" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_parsing_explicit" nil nil)
                       ("set_nonrecursive_elimination_schemes" "Set Nonrecursive Elimination Schemes\n" "Set Nonrecursive Elimination Schemes" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_nonrecursive_elimination_schemes" nil nil)
                       ("set_maximal_implicit_insertion" "Set Maximal Implicit Insertion\n" "Set Maximal Implicit Insertion" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_maximal_implicit_insertion" nil nil)
                       ("set_ltac_debug" "Set Ltac Debug\n" "Set Ltac Debug" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_ltac_debug" nil nil)
                       ("set_loose_hint_behavior" "Set Loose Hint Behavior\n" "Set Loose Hint Behavior" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_loose_hint_behavior" nil nil)
                       ("set_keyed_unification" "Set Keyed Unification\n" "Set Keyed Unification" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_keyed_unification" nil nil)
                       ("set_kernel_term_sharing" "Set Kernel Term Sharing\n" "Set Kernel Term Sharing" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_kernel_term_sharing" nil nil)
                       ("set_intuition_negation_unfolding" "Set Intuition Negation Unfolding\n" "Set Intuition Negation Unfolding" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_intuition_negation_unfolding" nil nil)
                       ("set_intuition_iff_unfolding" "Set Intuition Iff Unfolding\n" "Set Intuition Iff Unfolding" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_intuition_iff_unfolding" nil nil)
                       ("set_inline_level" "Set Inline Level\n" "Set Inline Level" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_inline_level" nil nil)
                       ("set_injection_on_proofs" "Set Injection On Proofs\n" "Set Injection On Proofs" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_injection_on_proofs" nil nil)
                       ("set_injection_l2r_pattern_order" "Set Injection L2R Pattern Order\n" "Set Injection L2R Pattern Order" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_injection_l2r_pattern_order" nil nil)
                       ("set_info_trivial" "Set Info Trivial\n" "Set Info Trivial" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_info_trivial" nil nil)
                       ("set_info_level" "Set Info Level\n" "Set Info Level" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_info_level" nil nil)
                       ("set_info_eauto" "Set Info Eauto\n" "Set Info Eauto" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_info_eauto" nil nil)
                       ("set_info_auto" "Set Info Auto\n" "Set Info Auto" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_info_auto" nil nil)
                       ("set_implicit_arguments" "Set Implicit Arguments\n" "Set Implicit Arguments" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_implicit_arguments" nil nil)
                       ("set_hyps_limit" "Set Hyps Limit\n" "Set Hyps Limit" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_hyps_limit" nil nil)
                       ("set_hide_obligations" "Set Hide Obligations\n" "Set Hide Obligations" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_hide_obligations" nil nil)
                       ("set_functional_induction_rewrite_dependent" "Set Functional Induction Rewrite Dependent\n" "Set Functional Induction Rewrite Dependent" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_functional_induction_rewrite_dependent" nil nil)
                       ("set_function_raw_tcc" "Set Function_raw_tcc\n" "Set Function_raw_tcc" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_function_raw_tcc" nil nil)
                       ("set_function_debug" "Set Function_debug\n" "Set Function_debug" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_function_debug" nil nil)
                       ("set_firstorder_depth" "Set Firstorder Depth\n" "Set Firstorder Depth" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_firstorder_depth" nil nil)
                       ("set_extraction_typeexpand" "Set Extraction TypeExpand\n" "Set Extraction TypeExpand" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_extraction_typeexpand" nil nil)
                       ("set_extraction_safeimplicits" "Set Extraction SafeImplicits\n" "Set Extraction SafeImplicits" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_extraction_safeimplicits" nil nil)
                       ("set_extraction_optimize" "Set Extraction Optimize\n" "Set Extraction Optimize" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_extraction_optimize" nil nil)
                       ("set_extraction_keepsingleton" "Set Extraction KeepSingleton\n" "Set Extraction KeepSingleton" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_extraction_keepsingleton" nil nil)
                       ("set_extraction_flag" "Set Extraction Flag\n" "Set Extraction Flag" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_extraction_flag" nil nil)
                       ("set_extraction_file_comment" "Set Extraction File Comment\n" "Set Extraction File Comment" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_extraction_file_comment" nil nil)
                       ("set_extraction_conservative_types" "Set Extraction Conservative Types\n" "Set Extraction Conservative Types" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_extraction_conservative_types" nil nil)
                       ("set_extraction_autoinline" "Set Extraction AutoInline\n" "Set Extraction AutoInline" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_extraction_autoinline" nil nil)
                       ("set_extraction_accessopaque" "Set Extraction AccessOpaque\n" "Set Extraction AccessOpaque" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_extraction_accessopaque" nil nil)
                       ("set_equality_scheme" "Set Equality Scheme\n" "Set Equality Scheme" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_equality_scheme" nil nil)
                       ("set_elimination_schemes" "Set Elimination Schemes\n" "Set Elimination Schemes" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_elimination_schemes" nil nil)
                       ("set_dump_bytecode" "Set Dump Bytecode\n" "Set Dump Bytecode" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_dump_bytecode" nil nil)
                       ("set_discriminate_introduction" "Set Discriminate Introduction\n" "Set Discriminate Introduction" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_discriminate_introduction" nil nil)
                       ("set_dependent_propositions_elimination" "Set Dependent Propositions Elimination\n" "Set Dependent Propositions Elimination" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_dependent_propositions_elimination" nil nil)
                       ("set_default_timeout" "Set Default Timeout\n" "Set Default Timeout" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_default_timeout" nil nil)
                       ("set_default_proof_using" "Set Default Proof Using\n" "Set Default Proof Using" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_default_proof_using" nil nil)
                       ("set_default_proof_mode" "Set Default Proof Mode\n" "Set Default Proof Mode" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_default_proof_mode" nil nil)
                       ("set_default_goal_selector" "Set Default Goal Selector\n" "Set Default Goal Selector" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_default_goal_selector" nil nil)
                       ("set_default_clearing_used_hypotheses" "Set Default Clearing Used Hypotheses\n" "Set Default Clearing Used Hypotheses" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_default_clearing_used_hypotheses" nil nil)
                       ("set_decidable_equality_schemes" "Set Decidable Equality Schemes\n" "Set Decidable Equality Schemes" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_decidable_equality_schemes" nil nil)
                       ("set_debug_unification" "Set Debug Unification\n" "Set Debug Unification" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_debug_unification" nil nil)
                       ("set_debug_trivial" "Set Debug Trivial\n" "Set Debug Trivial" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_debug_trivial" nil nil)
                       ("set_debug_tactic_unification" "Set Debug Tactic Unification\n" "Set Debug Tactic Unification" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_debug_tactic_unification" nil nil)
                       ("set_debug_rakam" "Set Debug RAKAM\n" "Set Debug RAKAM" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_debug_rakam" nil nil)
                       ("set_debug_eauto" "Set Debug Eauto\n" "Set Debug Eauto" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_debug_eauto" nil nil)
                       ("set_debug_auto" "Set Debug Auto\n" "Set Debug Auto" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_debug_auto" nil nil)
                       ("set_contextual_implicit" "Set Contextual Implicit\n" "Set Contextual Implicit" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_contextual_implicit" nil nil)
                       ("set_congruence_verbose" "Set Congruence Verbose\n" "Set Congruence Verbose" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_congruence_verbose" nil nil)
                       ("set_congruence_depth" "Set Congruence Depth\n" "Set Congruence Depth" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_congruence_depth" nil nil)
                       ("set_compat_notations" "Set Compat Notations\n" "Set Compat Notations" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_compat_notations" nil nil)
                       ("set_case_analysis_schemes" "Set Case Analysis Schemes\n" "Set Case Analysis Schemes" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_case_analysis_schemes" nil nil)
                       ("set_bullet_behavior" "Set Bullet Behavior\n" "Set Bullet Behavior" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_bullet_behavior" nil nil)
                       ("set_bracketing_last_introduction_pattern" "Set Bracketing Last Introduction Pattern\n" "Set Bracketing Last Introduction Pattern" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_bracketing_last_introduction_pattern" nil nil)
                       ("set_boolean_equality_schemes" "Set Boolean Equality Schemes\n" "Set Boolean Equality Schemes" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_boolean_equality_schemes" nil nil)
                       ("set_automatic_introduction" "Set Automatic Introduction\n" "Set Automatic Introduction" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_automatic_introduction" nil nil)
                       ("set_automatic_coercions_import" "Set Automatic Coercions Import\n" "Set Automatic Coercions Import" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_automatic_coercions_import" nil nil)
                       ("set_atomic_load" "Set Atomic Load\n" "Set Atomic Load" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_atomic_load" nil nil)
                       ("set_asymmetric_patterns" "Set Asymmetric Patterns\n" "Set Asymmetric Patterns" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set_asymmetric_patterns" nil nil)
                       ("set" "set $1 := $2\n" "set" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/set" nil "set")
                       ("sec" "Section $1.\n" "Section" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/section" nil "sec")
                       ("searchrewrite" "SearchRewrite $1\n" "SearchRewrite" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/searchrewrite" nil nil)
                       ("searchpattern" "SearchPattern ($1)\n" "SearchPattern" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/searchpattern" nil nil)
                       ("searchabout" "SearchAbout $1\n" "SearchAbout" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/searchabout" nil nil)
                       ("search" "Search $1\n" "Search" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/search" nil nil)
                       ("sc" "Scheme ${1:name} := $1.\n" "Scheme" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/scheme" nil "sc")
                       ("save" "Save.\n" "Save" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/save" nil nil)
                       ("romega" "romega\n" "romega" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/romega" nil nil)
                       ("ring" "ring $1\n" "ring" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/ring" nil "ring")
                       ("rig" "right\n" "right" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/right" nil "rig")
                       ("rewrite_all" "rewrite_all\n" "rewrite_all <-" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/rewrite_all" nil nil)
                       ("r" "rewrite $1\n" "rewrite" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/rewrite" nil "r")
                       ("r" "revert dependent\n" "revert dependent" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/revert_dependent" nil "r")
                       ("r" "revert\n" "revert" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/revert" nil "r")
                       ("reset_extraction_inline" "Reset Extraction Inline.\n" "Reset Extraction Inline" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/reset_extraction_inline" nil nil)
                       ("reserved_notation" "Reserved Notation\n" "Reserved Notation" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/reserved_notation" nil nil)
                       ("reserved_infix" "Reserved Infix\n" "Reserved Infix" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/reserved_infix" nil nil)
                       ("require_import" "Require Import $1.\n" "Require Import" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/require_import" nil nil)
                       ("require_export" "Require Export $1.\n" "Require Export" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/require_export" nil nil)
                       ("require" "Require $1.\n" "Require" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/require" nil nil)
                       ("rep" "replace $1 with $2\n" "replace with" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/replace_with" nil "rep")
                       ("repeat" "repeat $1\n" "repeat" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/repeat" nil nil)
                       ("ren" "rename $1 into $2\n" "rename into" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/rename_into" nil "ren")
                       ("remove_printing_let" "Remove Printing Let $1.\n" "Remove Printing Let" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/remove_printing_let" nil nil)
                       ("remove_printing_if" "Remove Printing If $1.\n" "Remove Printing If" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/remove_printing_if" nil nil)
                       ("remove_loadpath" "Remove LoadPath\n" "Remove LoadPath" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/remove_loadpath" nil nil)
                       ("rk" "Remark $1 : $2.\n$3\nQed.\n" "Remark" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/remark" nil "rk")
                       ("refl" "reflexivity $1\n" "reflexivity" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/reflexivity" nil "refl")
                       ("ref" "refine\n" "refine" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/refine" nil "ref")
                       ("red" "red\n" "red" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/red" nil "red")
                       ("recextrm" "Recursive Extraction Module ${1:id}.\n" "Recursive Extraction Module" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/recursive_extraction_module" nil "recextrm")
                       ("recextrl" "Recursive Extraction Library ${1:id}.\n" "Recursive Extraction Library" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/recursive_extraction_library" nil "recextrl")
                       ("recextr" "Recursive Extraction ${1:id}.\n" "Recursive Extraction" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/recursive_extraction" nil "recextr")
                       ("rec" "Record $1 : $2 := {\n$3 : $4;\n$5 : $6 }\n" "Record" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/record" nil "rec")
                       ("quote" "quote\n" "quote" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/quote" nil "quote")
                       ("qed" "Qed.\n" "Qed" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/qed" nil nil)
                       ("pwd" "Pwd.\n" "Pwd" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/pwd" nil nil)
                       ("psatz" "psatz\n" "psatz" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/psatz" nil nil)
                       ("pr" "Proposition $1 : $2.\nProof.\n$3\nQed.\n" "Proposition" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/proposition" nil "pr")
                       ("prcongr" "prop_congr\n" "prop_congr" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/prop_congr" nil "prcongr")
                       ("prol" "prolog\n" "prolog" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/prolog" nil "prol")
                       ("progress" "progress $1\n" "progress" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/progress" nil nil)
                       ("pth" "Program Theorem $1 : $2.\nProof.\n$3\nQed.\n" "Program Theorem" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/program_theorem" nil "pth")
                       ("pl" "Program Lemma $1 : $2.\nProof.\n$3\nQed.\n" "Program Lemma" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/program_lemma" nil "pl")
                       ("pinstance" "Program Instance [ $1 ] => $2 where \n$3 := $4;\n$5 := $6.\n" "Program Instance" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/program_instance" nil "pinstance")
                       ("pfix" "Program Fixpoint $1 ($2:$3) {struct ${1:arg}} : $4 :=\n$5.\n" "Program Fixpoint" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/program_fixpoint" nil "pfix")
                       ("pdef" "Program Definition $1:$2 := $3.\n" "Program Definition" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/program_definition" nil "pdef")
                       ("print_coercions" "Print Coercions.\n" "Print Coercions" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/print_coercions" nil nil)
                       ("p" "Print $1.\n" "Print" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/print" nil "p")
                       ("preterm" "Preterm.\n" "Preterm" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/preterm" nil nil)
                       ("pi" "Prenex Implicits $1\n" "Prenex Implicits" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/prenex_implicits" nil "pi")
                       ("po" "pose $1 := $2\n" "pose" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/pose" nil "po")
                       ("pat" "pattern\n" "pattern" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/pattern" nil "pat")
                       ("par" "Parameters $1: $2\n" "Parameters" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/parameters" nil "par")
                       ("par" "Parameter $1: $2\n" "Parameter" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/parameter" nil "par")
                       ("opsc" "Open Scope $1\n" "Open Scope" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/open_scope" nil "opsc")
                       ("open_local_scope" "Open Local Scope $1\n" "Open Local Scope" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/open_local_scope" nil nil)
                       ("opaque" "Opaque $1.\n" "Opaque" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/opaque" nil nil)
                       ("o" "omega\n" "omega" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/omega" nil "o")
                       ("obls" "Obligations $1.\n$2\nQed.\n" "Obligations" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/obligations" nil "obls")
                       ("obligation_tactic" "Obligation Tactic := $1.\n" "Obligation Tactic" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/obligation_tactic" nil nil)
                       ("obl" "Obligation $1.\n$2\nQed.\n" "Obligation" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/obligation" nil "obl")
                       ("nsatz" "nsatz\n" "nsatz" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/nsatz" nil nil)
                       ("nra" "nra\n" "nra" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/nra" nil nil)
                       ("now_show" "now_show\n" "now_show" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/now_show" nil nil)
                       ("now" "now\n" "now" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/now" nil nil)
                       ("nots" "Notation $1 := $2.\n" "Notation (simple)" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/notation_simple" nil "nots")
                       ("nia" "nia\n" "nia" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/nia" nil nil)
                       ("nobl" "Next Obligation.\n$1\nQed.\n" "Next Obligation" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/next_obligation" nil "nobl")
                       ("nnorm" "nat_norm\n" "nat_norm" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/nat_norm" nil "nnorm")
                       ("ncongr" "nat_congr\n" "nat_congr" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/nat_congr" nil "ncongr")
                       ("m" "multimatch $1 with\n| $2 => $3\nend\n" "multimatch with" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/multimatch_with" nil "m")
                       ("mov" "move $1 after $2\n" "move after" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/move_after" nil "mov")
                       ("m" "move\n" "move" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/move" nil "m")
                       ("mti" "Module Type $1.\n$2\nEnd $3.\n" "Module Type" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/module_type" nil "mti")
                       ("moi" "Module $1 : $2.\n$3\nEnd $4.\n" "Module :" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/module" nil "moi")
                       ("m5" "match $1 with\n| $2 => $3\n| $4 => $5\n| $6 => $7\n| $8 => $9\n| $10 => $11\nend\n" "match with 5" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/match_with_5" nil "m5")
                       ("m4" "match $1 with\n| $2 => $3\n| $4 => $5\n| $6 => $7\n| $8 => $9\nend\n" "match with 4" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/match_with_4" nil "m4")
                       ("m3" "match $1 with\n| $2 => $3\n| $4 => $5\n| $6 => $7\nend\n" "match with 3" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/match_with_3" nil "m3")
                       ("m2" "match $1 with\n| $2 => $3\n| $4 => $5\nend\n" "match with 2" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/match_with_2" nil "m2")
                       ("m" "match $1 with\n| $2 => $3\nend\n" "match with" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/match_with" nil "m")
                       ("ltac" "Ltac $1 := $2\n" "Ltac" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/ltac" nil "ltac")
                       ("lra" "lra\n" "lra" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/lra" nil nil)
                       ("locate_notation" "Locate Notation ($1) $2\n" "Locate Notation" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/locate_notation" nil nil)
                       ("locate_library" "Locate Library $1.\n" "Locate Library" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/locate_library" nil nil)
                       ("locate" "Locate\n" "Locate" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/locate" nil nil)
                       ("local_strategy" "Local Strategy $1 [$2].\n" "Local Strategy" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/local_strategy" nil nil)
                       ("local_open_scope" "Local Open Scope $1\n" "Local Open Scope" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/local_open_scope" nil nil)
                       ("lnots" "Local Notation $1 := $2.\n" "Local Notation" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/local_notation" nil "lnots")
                       ("local_ltac" "Local Ltac $1 := $2\n" "Local Ltac" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/local_ltac" nil nil)
                       ("local_definition" "Local Definition $1:$2 := $3.\n" "Local Definition" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/local_definition" nil nil)
                       ("local_coercion" "Local Coercion ${1:id} : ${2:typ1} >-> ${3:typ2}.\n" "Local Coercion" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/local_coercion" nil nil)
                       ("lclsc" "Local Close Scope $1\n" "Local Close Scope" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/local_close_scope" nil "lclsc")
                       ("local_axioms" "Local Axioms $1 , $2: $3\n" "Local Axioms" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/local_axioms" nil nil)
                       ("local_axiom" "Local Axiom $1 : $2\n" "Local Axiom" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/local_axiom" nil nil)
                       ("local_arguments" "Local Arguments ${1:id} : ${2:rule}\n" "Local Arguments" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/local_arguments" nil nil)
                       ("load" "load\n" "load" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/load" nil "load")
                       ("lin" "linear\n" "linear" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/linear" nil "lin")
                       ("lia" "lia\n" "lia" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/lia" nil nil)
                       ("li" "let $1 := $2 in $3\n" "let in" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/let_in" nil "li")
                       ("Let" "Let $1 : $2 := $3.\n" "Let" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/let" nil "Let")
                       ("l" "Lemma $1 : $2.\nProof.\n$3\nQed.\n" "Lemma" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/lemma" nil "l")
                       ("left" "left\n" "left" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/left" nil "left")
                       ("m" "lazymatch $1 with\n| $2 => $3\nend\n" "lazymatch with" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/lazymatch_with" nil "m")
                       ("lazy" "lazy beta delta [$1] iota zeta\n" "lazy" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/lazy" nil "lazy")
                       ("lst" "nil\n" "last" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/last" nil "lst")
                       ("lap" "lapply\n" "lapply" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/lapply" nil "lap")
                       ("invcl" "inversion_clear\n" "inversion_clear" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/inversion_clear" nil "invcl")
                       ("inv" "inversion $1\n" "inversion" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/inversion" nil "inv")
                       ("intu" "intuition $1\n" "intuition" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/intuition" nil "intu")
                       ("is" "intros $1\n" "intros" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/intros" nil "is")
                       ("i" "intro\n" "intro" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/intro" nil "i")
                       ("inst" "instantiate\n" "instantiate" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/instantiate" nil "inst")
                       ("instance" "Instance $1:$2.\nProof.\n$3Defined.\n" "Instance" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/instance" nil nil)
                       ("inspect" "Inspect $1.\n" "Inspect" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/inspect" nil nil)
                       ("inj" "injection $1\n" "injection" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/injection" nil "inj")
                       ("info" "info $1\n" "info" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/info" nil nil)
                       ("indv" "Inductive $1 : $2 := $3 : $4.\n" "Inductive" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/inductive" nil "indv")
                       ("ind" "induction $1\n" "induction" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/induction" nil "ind")
                       ("include" "Include $1.\n" "Include" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/include" nil nil)
                       ("import" "Import $1.\n" "Import" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/import" nil nil)
                       ("implicit_types" "Implicit Types $1 : $2.\n" "Implicit Types" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/implicit_types" nil nil)
                       ("implicit_arguments_on" "Implicit Arguments On.\n" "Implicit Arguments On" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/implicit_arguments_on" nil nil)
                       ("implicit_arguments_off" "Implicit Arguments Off.\n" "Implicit Arguments Off" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/implicit_arguments_off" nil nil)
                       ("implicit_arguments" "Implicit Arguments $1 [$2].\n" "Implicit Arguments" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/implicit_arguments" nil nil)
                       ("if" "if $1 then $2 else $3\n" "if" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/if" nil "if")
                       ("idtac" "idtac\n" "idtac" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/idtac" nil nil)
                       ("identity_coercion" "Identity Coercion $1.\n" "Identity Coercion" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/identity_coercion" nil nil)
                       ("hyp" "Hypothesis $1: $2\n" "Hypothesis" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/hypothesis" nil "hyp")
                       ("hyp" "Hypotheses $1: $2\n" "Hypotheses" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/hypotheses" nil "hyp")
                       ("hnf" "hnf\n" "hnf" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/hnf" nil "hnf")
                       ("hv" "Hint View for $1\n" "Hint View for" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/hint_view_for" nil "hv")
                       ("hu" "Hint Unfold $1 : $2.\n" "Hint Unfold" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/hint_unfold" nil "hu")
                       ("hrw" "Hint Rewrite -> ${1:t1,t2...} using ${2:tac} : ${3:db}.\n" "Hint Rewrite ->" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/hint_rewrite" nil "hrw")
                       ("hr" "Hint Resolve $1 : ${1:db}.\n" "Hint Resolve" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/hint_resolve" nil "hr")
                       ("hi" "Hint Immediate $1 : ${1:db}.\n" "Hint Immediate" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/hint_immediate" nil "hi")
                       ("he" "Hint Extern ${1:cost} ${2:pat} => ${3:tac} : ${4:db}.\n" "Hint Extern" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/hint_extern" nil "he")
                       ("hc" "Hint Constructors $1 : $2.\n" "Hint Constructors" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/hint_constructors" nil "hc")
                       ("hv" "have $1 : $2\n" "have" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/have" nil "hv")
                       ("goal" "Goal $1.\n" "Goal" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/goal" nil nil)
                       ("gvs" "Global Variables $1 , $2: $3.\n" "Global Variables" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/global_variables" nil "gvs")
                       ("gv" "Global Variable $1: $2.\n" "Global Variable" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/global_variable" nil "gv")
                       ("gfa" "gfail\n" "gfail" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/gfail" nil "gfa")
                       ("gd" "generalize dependent $1\n" "generalize dependent" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/generalize_dependent" nil "gd")
                       ("g" "generalize $1\n" "generalize" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/generalize" nil "g")
                       ("generalizable_variables" "Generalizable Variables $1.\n" "Generalizable Variables" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/generalizable_variables" nil nil)
                       ("generalizable_all_variables" "Generalizable All Variables.\n" "Generalizable All Variables" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/generalizable_all_variables" nil nil)
                       ("fs" "Functional Scheme ${1:name} := Induction for ${2:fun}.\n" "Functional Scheme" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/functional_scheme" nil "fs")
                       ("finv" "functional inversion ${1:H}\n" "functional inversion" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/functional_inversion" nil "finv")
                       ("fi" "functional induction ${1:f} ${2:args}\n" "functional induction" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/functional_induction" nil "fi")
                       ("func" "Function $1 ($2:$3) {struct ${1:arg}} : $4 :=\n$5.\n" "Function" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/function" nil "func")
                       ("f4" "fun ($1:$2) ($3:$4) ($5:$6) ($7:$8) => $9\n" "fun (4 args)" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/fun_4_args" nil "f4")
                       ("f3" "fun ($1:$2) ($3:$4) ($5:$6) => $7\n" "fun (3 args)" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/fun_3_args" nil "f3")
                       ("f2" "fun ($1:$2) ($3:$4) => $5\n" "fun (2 args)" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/fun_2_args" nil "f2")
                       ("f" "fun $1:$2 => $3\n" "fun (1 args)" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/fun_1_args" nil "f")
                       ("four" "fourier\n" "fourier" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/fourier" nil "four")
                       ("fo4" "forall ($1:$2) ($3:$4) ($5:$6) ($7:$8), $9\n" "forall (4 args)" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/forall_4_args" nil "fo4")
                       ("fo3" "forall ($1:$2) ($3:$4) ($5:$6), $7\n" "forall (3 args)" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/forall_3_args" nil "fo3")
                       ("fo2" "forall ($1:$2) ($3:$4), $5\n" "forall (2 args)" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/forall_2_args" nil "fo2")
                       ("fo" "forall $1:$2,$3\n" "forall" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/forall" nil "fo")
                       ("fold" "fold $1\n" "fold" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/fold" nil "fold")
                       ("focus" "Focus $1.\n" "Focus" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/focus" nil nil)
                       ("fix" "Fixpoint $1 ($2:$3) {struct ${1:arg}} : $4 :=\n$5.\n" "Fixpoint" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/fixpoint" nil "fix")
                       ("fsto" "firstorder\n" "firstorder" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/firstorder" nil "fsto")
                       ("first" "first [ $1 | $2 ]\n" "first" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/first" nil nil)
                       ("field" "field\n" "field" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/field" nil "field")
                       ("false_hyp" "false_hyp ${1:H} ${2:G}\n" "false_hyp" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/false_hyp" nil nil)
                       ("fail" "Fail\n" "Fail" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/fail" nil nil)
                       ("fct" "Fact $1 : $2.\n" "Fact" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/fact" nil "fct")
                       ("extraction_noinline" "Extraction NoInline $1.\n" "Extraction NoInline" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/extraction_noinline" nil nil)
                       ("extrl" "Extraction Library ${1:id}.\n" "Extraction Library" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/extraction_library" nil "extrl")
                       ("extrlang" "Extraction Language $1.\n" "Extraction Language" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/extraction_language" nil "extrlang")
                       ("extraction_inline" "Extraction Inline $1.\n" "Extraction Inline" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/extraction_inline" nil nil)
                       ("extr" "Extraction ${1:id}.\n" "Extraction" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/extraction" nil "extr")
                       ("export" "Export $1.\n" "Export" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/export" nil nil)
                       ("ex" "exists $1\n" "exists" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/exists" nil "ex")
                       ("existing_instances" "Existing Instances \n" "Existing Instances" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/existing_instances" nil nil)
                       ("existing_instance" "Existing Instance \n" "Existing Instance" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/existing_instance" nil nil)
                       ("existing_class" "Existing Class \n" "Existing Class" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/existing_class" nil nil)
                       ("exf" "exfalso\n" "exfalso" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/exfalso" nil "exf")
                       ("ex" "Example $1:$2 := $3.\n" "Example" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/example" nil "ex")
                       ("exa" "exact\n" "exact" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/exact" nil "exa")
                       ("eval" "Eval $1.\n" "Eval" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/eval" nil nil)
                       ("esp" "esplit\n" "esplit" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/esplit" nil "esp")
                       ("er" "erewrite $1\n" "erewrite" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/erewrite" nil "er")
                       ("eqs" "Equations $1:$2 := $3.\n" "Equations" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/equations" nil "eqs")
                       ("eng" "enough ($1: $2).\n{ $3\n}\n" "enough" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/enough" nil "eng")
                       ("elt" "elimtype\n" "elimtype" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/elimtype" nil "elt")
                       ("e" "elim $1\n" "elim" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/elim" nil "e")
                       ("eleft" "eleft\n" "eleft" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/eleft" nil "eleft")
                       ("eex" "eexists\n" "eexists" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/eexists" nil "eex")
                       ("edes" "edestruct \n" "edestruct" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/edestruct" nil "edes")
                       ("econs" "econstructor\n" "econstructor" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/econstructor" nil "econs")
                       ("ea" "eauto\n" "eauto" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/eauto" nil "ea")
                       ("easy" "easy\n" "easy" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/easy" nil nil)
                       ("eas" "eassumption\n" "eassumption" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/eassumption" nil "eas")
                       ("eap" "eapply $1\n" "eapply" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/eapply" nil "eap")
                       ("dind" "double induction $1 $2\n" "double induction" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/double_induction" nil "dind")
                       ("done" "done\n" "done" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/done" nil nil)
                       ("do" "do ${1:num} ${2:tac}\n" "do" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/do" nil nil)
                       ("discrR" "discrR\n" "discrR" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/discrr" nil "discrR")
                       ("dis" "discriminate\n" "discriminate" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/discriminate" nil "dis")
                       ("deswe" "destruct_with_eqn \n" "destruct_with_eqn" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/destruct_with_eqn" nil "deswe")
                       ("desall" "destruct_all \n" "destruct_all" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/destruct_all" nil "desall")
                       ("des" "destruct \n" "destruct" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/destruct" nil "des")
                       ("deseq" "destr_eq \n" "destr_eq" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/destr_eq" nil "deseq")
                       ("derive_inversion" "Derive Inversion ${1:id} with $1 Sort $2.\n" "Derive Inversion" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/derive_inversion" nil nil)
                       ("derive_dependent_inversion" "Derive Dependent Inversion ${1:id} with $1 Sort $2.\n" "Derive Dependent Inversion" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/derive_dependent_inversion" nil nil)
                       ("depr" "dependent rewrite -> ${1:id}\n" "dependent rewrite ->" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/dependent_rewrite" nil "depr")
                       ("depinvc" "dependent inversion_clear\n" "dependent inversion_clear" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/dependent_inversion_clear" nil "depinvc")
                       ("depinv" "dependent inversion\n" "dependent inversion" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/dependent_inversion" nil "depinv")
                       ("def" "Definition $1:$2 := $3.\n" "Definition" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/definition" nil "def")
                       ("decs" "decompose sum $1\n" "decompose sum" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/decompose_sum" nil "decs")
                       ("decr" "decompose record $1\n" "decompose record" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/decompose_record" nil "decr")
                       ("dec" "decompose [$1] $2\n" "decompose" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/decompose" nil "dec")
                       ("dm" "Declare Module $1 : $2 := $3.\n" "Declare Module : :=" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/declare_module" nil "dm")
                       ("declare" "Declare $1.\n" "Declare" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/declare" nil nil)
                       ("decr" "decide_right\n" "decide_right" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/decide_right" nil "decr")
                       ("decl" "decide_left\n" "decide_left" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/decide_left" nil "decl")
                       ("deg" "decide equality\n" "decide equality" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/decide_equality" nil "deg")
                       ("decide" "decide\n" "decide" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/decide" nil nil)
                       ("cutr" "cutrewrite -> $1 = $2\n" "cutrewrite" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/cutrewrite" nil "cutr")
                       ("cut" "cut\n" "cut" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/cut" nil "cut")
                       ("cor" "Corollary $1 : $2.\nProof.\n$3\nQed.\n" "Corollary" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/corollary" nil "cor")
                       ("contr" "contradiction\n" "contradiction" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/contradiction" nil "contr")
                       ("contr" "contradict\n" "contradict" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/contradict" nil "contr")
                       ("context" "Context $1, ($2 : $3).\n" "Context" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/context" nil nil)
                       ("cons" "constructor\n" "constructor" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/constructor" nil "cons")
                       ("conj" "Conjecture $1: $2.\n" "Conjecture" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/conjecture" nil "conj")
                       ("cong" "congruence\n" "congruence" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/congruence" nil "cong")
                       ("con" "congr $1\n" "congr" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/congr" nil "con")
                       ("cmpu" "compute\n" "compute" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/compute" nil "cmpu")
                       ("cmpa" "compare $1 $2\n" "compare" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/compare" nil "cmpa")
                       ("comments" "Comments $1.\n" "Comments" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/comments" nil nil)
                       ("coindv" "CoInductive $1 : $2 :=\n|$3 : $4.\n" "CoInductive" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/coinductive" nil "coindv")
                       ("coind" "coinduction\n" "coinduction" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/coinduction" nil "coind")
                       ("cfix" "CoFixpoint $1 ($2:$3) : $4 :=\n$5.\n" "CoFixpoint" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/cofixpoint" nil "cfix")
                       ("cof" "cofix\n" "cofix" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/cofix" nil "cof")
                       ("coerc" "Coercion ${1:id} : ${2:typ1} >-> ${3:typ2}.\n" "Coercion" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/coercion" nil "coerc")
                       ("clsc" "Close Scope $1\n" "Close Scope" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/close_scope" nil "clsc")
                       ("cl" "clearbody\n" "clearbody" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/clearbody" nil "cl")
                       ("cldep" "clear dependent\n" "clear dependent" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/clear_dependent" nil "cldep")
                       ("cl" "clear\n" "clear" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/clear" nil "cl")
                       ("class" "Class [ $1 ] := \n$2 : $3;\n$4 : $5.\n" "Class" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/class" nil "class")
                       ("check" "Check\n" "Check" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/check" nil nil)
                       ("chp" "Chapter $1 : $2.\n" "Chapter" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/chapter" nil "chp")
                       ("ch" "change \n" "change" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/change" nil "ch")
                       ("cd" "Cd $1.\n" "Cd" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/cd" nil nil)
                       ("cbv" "cbv beta delta [$1] iota zeta\n" "cbv" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/cbv" nil "cbv")
                       ("cbn" "cbn beta delta [$1] iota zeta\n" "cbn (with flags)" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/cbn_with_flags" nil "cbn")
                       ("c" "cbn\n" "cbn" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/cbn" nil "c")
                       ("cty" "case_type \n" "case_type" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/case_type" nil "cty")
                       ("ceq" "case_eq \n" "case_eq" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/case_eq" nil "ceq")
                       ("case" "case \n" "case" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/case" nil nil)
                       ("canonical_structure" "Canonical Structure $1.\n" "Canonical Structure" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/canonical_structure" nil nil)
                       ("by" "by $1\n" "by" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/by" nil "by")
                       ("bcongr" "bool_congr\n" "bool_congr" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/bool_congr" nil "bcongr")
                       ("bndsc" "Bind Scope ${1:scope} with ${2:type}\n" "Bind Scope" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/bind_scope" nil "bndsc")
                       ("axs" "Axioms $1 , $2: $3\n" "Axioms" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/axioms" nil "axs")
                       ("ax" "Axiom $1 : $2\n" "Axiom" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/axiom" nil "ax")
                       ("ar" "autorewrite with ${1:db,db...}\n" "autorewrite with" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/autorewrite_with" nil "ar")
                       ("a" "auto\n" "auto" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/auto" nil "a")
                       ("as" "assumption\n" "assumption" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/assumption" nil "as")
                       ("assb" "assert ( $1 : $2 ) by $3\n" "assert by" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/assert_by" nil "assb")
                       ("argsc" "Arguments Scope ${1:id} [ ${2:_} ]\n" "Arguments Scope" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/arguments_scope" nil "argsc")
                       ("args" "Arguments ${1:id} : ${2:rule}\n" "Arguments" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/arguments" nil "args")
                       ("ap" "apply \n" "apply" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/apply" nil "ap")
                       ("admitted" "Admitted\n" "Admitted" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/admitted" nil nil)
                       ("oblsadmit" "Admit Obligations.\n" "Admit Obligations" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/admit_obligations" nil "oblsadmit")
                       ("add_setoid" "Add Setoid $1.\n" "Add Setoid" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/add_setoid" nil nil)
                       ("add_semi_ring" "Add Semi Ring $1.\n" "Add Semi Ring" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/add_semi_ring" nil nil)
                       ("add_ring" "Add Ring $1.\n" "Add Ring" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/add_ring" nil nil)
                       ("add_rec_ml_path" "Add Rec ML Path $1.\n" "Add Rec ML Path" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/add_rec_ml_path" nil nil)
                       ("add_rec_loadpath" "Add Rec LoadPath $1.\n" "Add Rec LoadPath" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/add_rec_loadpath" nil nil)
                       ("add_printing_let" "Add Printing Let $1.\n" "Add Printing Let" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/add_printing_let" nil nil)
                       ("add_printing_if" "Add Printing If $1.\n" "Add Printing If" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/add_printing_if" nil nil)
                       ("add_printing" "Add Printing $1.\n" "Add Printing" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/add_printing" nil nil)
                       ("add_parametric_relation" "Add Parametric Relation : \n" "Add Parametric Relation" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/add_parametric_relation" nil nil)
                       ("add_parametric_morphism" "Add Parametric Morphism : \n" "Add Parametric Morphism" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/add_parametric_morphism" nil nil)
                       ("addmor" "Add Morphism ${1:f} : ${2:id}\n" "Add Morphism" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/add_morphism" nil "addmor")
                       ("add_ml_path" "Add ML Path $1.\n" "Add ML Path" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/add_ml_path" nil nil)
                       ("add_loadpath" "Add LoadPath $1.\n" "Add LoadPath" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/add_loadpath" nil nil)
                       ("add_field" "Add Field $1.\n" "Add Field" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/add_field" nil nil)
                       ("add_abstract_semi_ring" "Add Abstract Semi Ring $1.\n" "Add Abstract Semi Ring" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/add_abstract_semi_ring" nil nil)
                       ("add_abstract_ring" "Add Abstract Ring $1.\n" "Add Abstract Ring" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/add_abstract_ring" nil nil)
                       ("abs" "absurd \n" "absurd" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/absurd" nil "abs")
                       ("abstract" "abstract ${1:tac} using ${2:name}.\n" "abstract" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/abstract" nil nil)
                       ("about" "About $1.\n" "About" nil nil nil "/home/orion/.config/doom/snippets/snippets/coq-mode/about" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("uni" "uniform(${1:0}, ${2:1})" "uniform" nil nil nil "/home/orion/.config/doom/snippets/snippets/cpp-omnet-mode/uniform" nil "uni")
                       ("sched" "scheduleAt(simTime()+${1:1.0}, ${2:event});" "scheduleAt" nil nil nil "/home/orion/.config/doom/snippets/snippets/cpp-omnet-mode/scheduleAt" nil "sched")
                       ("par" "${1:var} = par(\"${2:par}\");" "parameter_omnetpp" nil nil nil "/home/orion/.config/doom/snippets/snippets/cpp-omnet-mode/parameter_omnetpp" nil "par")
                       ("omnet" "#include <omnetpp.h>" "omnet" nil nil nil "/home/orion/.config/doom/snippets/snippets/cpp-omnet-mode/omnet" nil "omnet")
                       ("nan" "isnan(${1:x})" "nan" nil nil nil "/home/orion/.config/doom/snippets/snippets/cpp-omnet-mode/nan" nil "nan")
                       ("math" "#include <cmath>" "math" nil nil nil "/home/orion/.config/doom/snippets/snippets/cpp-omnet-mode/math" nil "math")
                       ("intuni" "intuniform(${1:0}, ${2:1})" "intuniform" nil nil nil "/home/orion/.config/doom/snippets/snippets/cpp-omnet-mode/intuniform" nil "intuni")
                       ("emit" "emit(${1:signal_id}, ${2:long});" "emit_signal" nil nil nil "/home/orion/.config/doom/snippets/snippets/cpp-omnet-mode/emit_signal" nil "emit")
                       ("ev" "EV << \"${1:string}\"$0;" "EV" nil nil nil "/home/orion/.config/doom/snippets/snippets/cpp-omnet-mode/EV" nil "ev")))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("using" "using System.$1;" "using System....;" nil nil nil "/home/orion/.config/doom/snippets/snippets/csharp-mode/using.2" nil "using")
                       ("using" "using System;" "using System;" nil nil nil "/home/orion/.config/doom/snippets/snippets/csharp-mode/using.1" nil "using")
                       ("using" "using $1;" "using ...;" nil nil nil "/home/orion/.config/doom/snippets/snippets/csharp-mode/using" nil "using")
                       ("region" "#region $1\n$0\n#endregion" "#region ... #endregion" nil nil nil "/home/orion/.config/doom/snippets/snippets/csharp-mode/region" nil "region")
                       ("prop" "/// <summary>\n/// $5\n/// </summary>\n/// <value>$6</value>\n$1 $2 $3\n{\n    get {\n        return this.$4;\n    }\n    set {\n        this.$4 = value;\n    }\n}" "property ... ... { ... }" nil nil nil "/home/orion/.config/doom/snippets/snippets/csharp-mode/prop" nil "prop")
                       ("namespace" "namespace $1\n{\n$0\n}" "namespace .. { ... }" nil nil nil "/home/orion/.config/doom/snippets/snippets/csharp-mode/namespace" nil "namespace")
                       ("method" "/// <summary>\n/// ${5:Description}\n/// </summary>${2:$(if (string= (upcase yas-text) \"VOID\") \"\" (format \"%s%s%s\" \"\\n/// <returns><c>\" yas-text \"</c></returns>\"))}\n${1:public} ${2:void} ${3:MethodName}($4)\n{\n$0\n}" "public void Method { ... }" nil nil nil "/home/orion/.config/doom/snippets/snippets/csharp-mode/method" nil "method")
                       ("foreach" "foreach (${1:var} ${2:item} in ${3:collection})\n{\n    $0\n}" "foreach" nil nil nil "/home/orion/.config/doom/snippets/snippets/csharp-mode/foreach" nil "foreach")
                       ("comment" "/// <exception cref=\"$1\">$2</exception>" "/// <exception cref=\"...\"> ... </exception>" nil nil nil "/home/orion/.config/doom/snippets/snippets/csharp-mode/comment.3" nil "comment")
                       ("comment" "/// <returns>$1</returns>" "/// <param name=\"...\"> ... </param>" nil nil nil "/home/orion/.config/doom/snippets/snippets/csharp-mode/comment.2" nil "comment")
                       ("comment" "/// <param name=\"$1\">$2</param>" "/// <param name=\"...\"> ... </param>" nil nil nil "/home/orion/.config/doom/snippets/snippets/csharp-mode/comment.1" nil "comment")
                       ("comment" "/// <summary>\n/// $1\n/// </summary>" "/// <summary> ... </summary>" nil nil nil "/home/orion/.config/doom/snippets/snippets/csharp-mode/comment" nil "comment")
                       ("class" "${5:public} class ${1:Name}\n{\n    #region Ctor & Destructor\n    /// <summary>\n    /// ${3:Standard Constructor}\n    /// </summary>\n    public $1($2)\n    {\n    }\n\n    /// <summary>\n    /// ${4:Default Destructor}\n    /// </summary>    \n    public ~$1()\n    {\n    }\n    #endregion\n}" "class ... { ... }" nil nil nil "/home/orion/.config/doom/snippets/snippets/csharp-mode/class" nil "class")
                       ("attrib" "/// <summary>\n/// $3\n/// </summary>\nprivate $1 ${2:$(if (> (length yas-text) 0) (format \"_%s%s\" (downcase (substring yas-text 0 1)) (substring yas-text 1 (length yas-text))) \"\")};\n\n/// <summary>\n/// ${3:Description}\n/// </summary>\n/// <value><c>$1</c></value>\npublic ${1:Type} ${2:Name}\n{\n    get {\n        return this.${2:$(if (> (length yas-text) 0) (format \"_%s%s\" (downcase (substring yas-text 0 1)) (substring yas-text 1 (length yas-text))) \"\")};\n    }\n    set {\n        this.${2:$(if (> (length yas-text) 0) (format \"_%s%s\" (downcase (substring yas-text 0 1)) (substring yas-text 1 (length yas-text))) \"\")} = value;\n    }\n}" "private _attribute ....; public Property ... ... { ... }" nil nil nil "/home/orion/.config/doom/snippets/snippets/csharp-mode/attrib.2" nil "attrib")
                       ("attrib" "/// <summary>\n/// $3\n/// </summary>\nprivate $1 $2;\n\n/// <summary>\n/// $4\n/// </summary>\n/// <value>$5</value>\npublic $1 $2\n{\n    get {\n        return this.$2;\n    }\n    set {\n        this.$2 = value;\n    }\n}" "private attribute ....; public property ... ... { ... }" nil nil nil "/home/orion/.config/doom/snippets/snippets/csharp-mode/attrib.1" nil "attrib")
                       ("attrib" "/// <summary>\n/// $3\n/// </summary>\nprivate $1 $2;" "private attribute ....;" nil nil nil "/home/orion/.config/doom/snippets/snippets/csharp-mode/attrib" nil "attrib")))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '((":" "${1:prop}: ${2:`%`};" "...: ...;" nil nil nil "/home/orion/.config/doom/snippets/snippets/css-mode/property" nil ":")
                       ("pad" "padding: ${1:10px};" "padding: ...;" nil nil nil "/home/orion/.config/doom/snippets/snippets/css-mode/padding" nil "pad")
                       ("media_print" "@media print {\n    `%`$0\n}" "@media print { ... }" nil nil nil "/home/orion/.config/doom/snippets/snippets/css-mode/media_print" nil nil)
                       ("media_orientation" "(orientation: ${1:landscape})" "@media (orientation: ?)"
                        (looking-back "@media "
                                      (line-beginning-position))
                        nil nil "/home/orion/.config/doom/snippets/snippets/css-mode/media_orientation" nil nil)
                       ("med" "@media ${1:screen} {\n    `%`$0\n}" "@media" nil nil nil "/home/orion/.config/doom/snippets/snippets/css-mode/media" nil "med")
                       ("mar" "margin: ${1:0 auto};" "margin: ...;" nil nil nil "/home/orion/.config/doom/snippets/snippets/css-mode/margin" nil "mar")
                       ("impurl" "@import url(\"`(doom-snippets-text nil t)`$0\");" "@import url(...)" nil nil nil "/home/orion/.config/doom/snippets/snippets/css-mode/importurl" nil "impurl")
                       ("impfont" "@import url(\"http://fonts.googleapis.com/css?family=${1:Open+Sans}\");" "@import url(\"//fonts.googleapis...\")" nil nil nil "/home/orion/.config/doom/snippets/snippets/css-mode/importfont" nil "impfont")
                       ("imp" "@import \"`(doom-snippets-text nil t)`$0\";" "@import" nil nil nil "/home/orion/.config/doom/snippets/snippets/css-mode/import" nil "imp")))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("while" "while ($1) {\n  $0\n}" "while" nil nil nil "/home/orion/.config/doom/snippets/snippets/dart-mode/while" nil nil)
                       ("typedef" "typedef ${1:Type} ${2:Name}(${3:params});" "typedef" nil nil nil "/home/orion/.config/doom/snippets/snippets/dart-mode/typedef" nil nil)
                       ("try" "try {\n  $0\n} catch (${1:e}) {\n}" "try" nil nil nil "/home/orion/.config/doom/snippets/snippets/dart-mode/try" nil nil)
                       ("test" "test('$1', () {\n  $0\n});" "test" nil nil nil "/home/orion/.config/doom/snippets/snippets/dart-mode/test" nil nil)
                       ("switch" "switch ($1) {\n  case $2:\n    $0\n    break;\n  default:\n}" "switch case" nil nil nil "/home/orion/.config/doom/snippets/snippets/dart-mode/switch" nil nil)
                       ("stless" "class $1 extends StatelessWidget {\n  @override\n  Widget build(BuildContext context) {\n    return Container(\n      $2\n    );\n  }\n}" "StatelessWidget" nil
                        ("flutter")
                        nil "/home/orion/.config/doom/snippets/snippets/dart-mode/stless" nil nil)
                       ("stful" "class $1 extends StatefulWidget {\n  @override\n  _$1State createState() => _$1State();\n}\n\nclass _$1State extends State<$1> {\n  @override\n  Widget build(BuildContext context) {\n    return Container(\n      $2\n    );\n  }\n}" "StatefulWidget" nil
                        ("flutter")
                        nil "/home/orion/.config/doom/snippets/snippets/dart-mode/stful" nil nil)
                       ("stanim" "class $1 extends StatefulWidget {\n  @override\n  _$1State createState() => _$1State();\n}\n\nclass _$1State extends State<$1>\n    with SingleTickerProviderStateMixin {\n  AnimationController _controller;\n\n  @override\n  void initState() {\n    super.initState();\n    _controller = AnimationController(vsync: this);\n  }\n\n  @override\n  void dispose() {\n    super.dispose();\n    _controller.dispose();\n  }\n\n  @override\n  Widget build(BuildContext context) {\n    return Container(\n      $2\n    );\n  }\n}" "StateufulWidget with AnimationController" nil
                        ("flutter")
                        nil "/home/orion/.config/doom/snippets/snippets/dart-mode/stanim" nil nil)
                       ("set" "${1:Type} _${2:Name};\nset $2($1 $2) => _$2 = $2;" "setter" nil nil nil "/home/orion/.config/doom/snippets/snippets/dart-mode/set" nil nil)
                       ("part" "part of ${1:Part}$0" "part" nil nil nil "/home/orion/.config/doom/snippets/snippets/dart-mode/part" nil nil)
                       ("main" "main(List<String> args) {\n  $0\n}" "main" nil
                        ("dart")
                        nil "/home/orion/.config/doom/snippets/snippets/dart-mode/main" nil nil)
                       ("is" "@override\nvoid initState() {\n  super.initState();\n  $0\n}" "initState" nil
                        ("flutter")
                        nil "/home/orion/.config/doom/snippets/snippets/dart-mode/inis" nil nil)
                       ("imp" "import '${1:Library}';$0" "import" nil nil nil "/home/orion/.config/doom/snippets/snippets/dart-mode/impo" nil nil)
                       ("impl" "implements ${1:Name}$0" "implements" nil nil nil "/home/orion/.config/doom/snippets/snippets/dart-mode/impl" nil nil)
                       ("ife" "if ($1) {\n  $0\n} else {\n}" "if else" nil nil nil "/home/orion/.config/doom/snippets/snippets/dart-mode/ife" nil nil)
                       ("if" "if ($1) {\n  $0\n}" "if" nil nil nil "/home/orion/.config/doom/snippets/snippets/dart-mode/if" nil nil)
                       ("group" "group('$1', () {\n  $0\n});" "group" nil nil nil "/home/orion/.config/doom/snippets/snippets/dart-mode/group" nil nil)
                       ("getset" "${1:Type} _${2:Name};\n$1 get $2 => _$2;\nset $2($1 $2) => _$2 = $2;$0" "getset" nil nil nil "/home/orion/.config/doom/snippets/snippets/dart-mode/getset" nil nil)
                       ("get" "${1:Type} _${2:Name};\n$1 get $2 => _$2;$0" "getter" nil nil nil "/home/orion/.config/doom/snippets/snippets/dart-mode/get" nil nil)
                       ("fun" "${1:Type} ${2:Name}($3) {\n  $0\n}" "fun" nil nil nil "/home/orion/.config/doom/snippets/snippets/dart-mode/fun" nil nil)
                       ("fori" "for (var ${1:item} in ${2:items}) {\n  $0\n}" "for-in" nil nil nil "/home/orion/.config/doom/snippets/snippets/dart-mode/fori" nil nil)
                       ("for" "for (var i = 0; i < ${1:count}; i++) {\n  $0\n}" "for" nil nil nil "/home/orion/.config/doom/snippets/snippets/dart-mode/for" nil nil)
                       ("ext" "extends ${1:Name}$0" "ext" nil nil nil "/home/orion/.config/doom/snippets/snippets/dart-mode/ext" nil nil)
                       ("elif" "else if ($1) {\n  $0\n}" "if else" nil nil nil "/home/orion/.config/doom/snippets/snippets/dart-mode/elif" nil nil)
                       ("do" "do {\n  $0\n} while ($1);" "do while" nil nil nil "/home/orion/.config/doom/snippets/snippets/dart-mode/do" nil nil)
                       ("dis" "@override\nvoid dispose() {\n  super.dispose();\n  $0\n}" "dispose" nil
                        ("flutter")
                        nil "/home/orion/.config/doom/snippets/snippets/dart-mode/dis" nil nil)
                       ("dcd" "@override\nvoid didChangeDependencies() {\n  super.didChangeDependencies();\n  $0\n}" "didChangeDependencies" nil
                        ("flutter")
                        nil "/home/orion/.config/doom/snippets/snippets/dart-mode/didchdep" nil nil)
                       ("cls" "class ${1:Name} {\n  $0\n}" "class" nil nil nil "/home/orion/.config/doom/snippets/snippets/dart-mode/cls" nil nil)
                       ("blt" "abstract class ${1:Name} implements Built<$1, $1Builder> {\n  factory $1([void Function($1Builder) updates]) = _$$1;\n  $1._();\n\n  $0\n}" "built value" nil
                        ("dart")
                        nil "/home/orion/.config/doom/snippets/snippets/dart-mode/bltval" nil nil)
                       ("afun" "Future<${1:Type}> ${2:Name}($3) async {\n  $0\n}" "async fun" nil nil nil "/home/orion/.config/doom/snippets/snippets/dart-mode/afun" nil nil)
                       ("acls" "abstract class ${1:Name} {\n  $0\n}" "abstract class" nil nil nil "/home/orion/.config/doom/snippets/snippets/dart-mode/acls" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("le" "log_error ${1:\"message\"}" "log_error MESSAGE ..." nil nil nil "/home/orion/.config/doom/snippets/snippets/direnv-envrc-mode/log_error" nil "le")
                       ("dld" "direnv_layout_dir" "direnv_layout_dir" nil nil nil "/home/orion/.config/doom/snippets/snippets/direnv-envrc-mode/direnv_layout_dir" nil "dld")
                       ("pa" "PATH_add ${1:path}" "PATH_add" nil nil nil "/home/orion/.config/doom/snippets/snippets/direnv-envrc-mode/PATH_add" nil "pa")))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("workdir" "WORKDIR ${1:/path/to/workdir}" "WORKDIR ..." nil nil nil "/home/orion/.config/doom/snippets/snippets/dockerfile-mode/workdir" nil nil)
                       ("volume" "VOLUME ${1:/path}" "VOLUME ..." nil nil nil "/home/orion/.config/doom/snippets/snippets/dockerfile-mode/volume" nil nil)
                       ("user" "USER ${1:daemon}" "USER ..." nil nil nil "/home/orion/.config/doom/snippets/snippets/dockerfile-mode/user" nil nil)
                       ("stopsignal" "STOPSIGNAL ${1:9}" "STOPSIGNAL <signal>" nil nil nil "/home/orion/.config/doom/snippets/snippets/dockerfile-mode/stopsignal" nil nil)
                       ("onbuild" "ONBUILD $0" "ONBUILD <instruction>" nil nil nil "/home/orion/.config/doom/snippets/snippets/dockerfile-mode/onbuild" nil nil)
                       ("label" "LABEL ${1:key}=${2:value}" "LABEL <key>=<value> ..." nil nil nil "/home/orion/.config/doom/snippets/snippets/dockerfile-mode/label" nil nil)
                       ("from" "FROM ${1:phusion/baseimage:${2:latest}}" "FROM <image>[:<tag|digest>]" nil nil nil "/home/orion/.config/doom/snippets/snippets/dockerfile-mode/from" nil nil)
                       ("expose" "EXPOSE ${1:80}" "EXPOSE <port> [<port> ...]" nil nil nil "/home/orion/.config/doom/snippets/snippets/dockerfile-mode/expose" nil nil)
                       ("env" "ENV ${1:var}=${2:value}" "ENV <key>=<value> ..." nil nil nil "/home/orion/.config/doom/snippets/snippets/dockerfile-mode/env" nil nil)
                       ("entrypoint" "ENTRYPOINT ${1:command}" "ENTRYPOINT <command>" nil nil nil "/home/orion/.config/doom/snippets/snippets/dockerfile-mode/entrypoint" nil nil)
                       ("dockerize-ubuntu" "RUN apt-get update && apt-get install -y wget\n\nENV DOCKERIZE_VERSION ${1:v0.6.1}\nRUN wget https://github.com/jwilder/dockerize/releases/download/${DOCKERIZE_VERSION}/dockerize-linux-amd64-${DOCKERIZE_VERSION}.tar.gz \\\n    && tar -C /usr/local/bin -xzvf dockerize-linux-amd64-${DOCKERIZE_VERSION}.tar.gz \\\n    && rm dockerize-linux-amd64-${DOCKERIZE_VERSION}.tar.gz\n$0" "dockerize for Ubuntu Images" nil nil
                        ((yas-indent-line 'fixed))
                        "/home/orion/.config/doom/snippets/snippets/dockerfile-mode/dockerize-ubuntu" nil nil)
                       ("dockerize-alpine" "RUN apk add --no-cache openssl\n\nENV DOCKERIZE_VERSION ${1:v0.6.1}\nRUN wget https://github.com/jwilder/dockerize/releases/download/${DOCKERIZE_VERSION}/dockerize-alpine-linux-amd64-${DOCKERIZE_VERSION}.tar.gz \\\n    && tar -C /usr/local/bin -xzvf dockerize-alpine-linux-amd64-${DOCKERIZE_VERSION}.tar.gz \\\n    && rm dockerize-alpine-linux-amd64-${DOCKERIZE_VERSION}.tar.gz\n$0\n" "dockerize for Alpine Images" nil nil
                        ((yas-indent-line 'fixed))
                        "/home/orion/.config/doom/snippets/snippets/dockerfile-mode/dockerize-alpine" nil nil)
                       ("copy" "COPY ${1:src} ${2:dest}" "COPY <src> <dest>" nil nil nil "/home/orion/.config/doom/snippets/snippets/dockerfile-mode/copy" nil nil)
                       ("arg" "ARG ${1:name}${2:=${3:value}}" "ARG <name>[=<default value>]" nil nil nil "/home/orion/.config/doom/snippets/snippets/dockerfile-mode/arg" nil nil)
                       ("add" "ADD ${1:src} ${2:dest}" "ADD <src> <dest>" nil nil nil "/home/orion/.config/doom/snippets/snippets/dockerfile-mode/add" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("<wip" "#+begin_quote\n 🔨 `%`$0\n#+end_quote" "Notice: work-in-progress" nil nil nil "/home/orion/.config/doom/snippets/snippets/doom-docs-org-mode/notice-wip" nil nil)
                       ("<warn" "#+begin_quote\n 🚧 `%`$0\n#+end_quote" "Notice: warning" nil nil nil "/home/orion/.config/doom/snippets/snippets/doom-docs-org-mode/notice-warn" nil nil)
                       ("<tip" "#+begin_quote\n 📌 `%`$0\n#+end_quote" "Notice: user tip" nil nil nil "/home/orion/.config/doom/snippets/snippets/doom-docs-org-mode/notice-tip" nil nil)
                       ("<quote" "#+begin_quote\n 🕞 ${1:`(or % (format-time-string \"%B %d, %Y\"))`}\n#+end_quote" "Notice: timestamp" nil nil nil "/home/orion/.config/doom/snippets/snippets/doom-docs-org-mode/notice-time" nil nil)
                       ("<quote" "#+begin_quote\n 💬 `%`$0\n#+end_quote" "Notice: opinion/tangent" nil nil nil "/home/orion/.config/doom/snippets/snippets/doom-docs-org-mode/notice-quote" nil nil)
                       ("<credit" "#+begin_quote\n 💕 `%`$0\n#+end_quote" "Notice: credit" nil nil nil "/home/orion/.config/doom/snippets/snippets/doom-docs-org-mode/notice-credit" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("unless" "unless ${1:condition} do\n  $0\nend" "unless" nil nil nil "/home/orion/.config/doom/snippets/snippets/elixir-mode/unless" nil nil)
                       ("test" "test \"$1\" do\n  $0\nend" "test" nil nil nil "/home/orion/.config/doom/snippets/snippets/elixir-mode/test" nil nil)
                       ("rcv" "receive do\n  $0\nend" "receive" nil nil nil "/home/orion/.config/doom/snippets/snippets/elixir-mode/receive" nil nil)
                       ("pry" "require IEx; IEx.pry" "pry" nil
                        ("debug")
                        nil "/home/orion/.config/doom/snippets/snippets/elixir-mode/pry" nil nil)
                       ("mdoc" "@moduledoc \"\"\"\n$0\n\"\"\"" "moduledoc" nil nil nil "/home/orion/.config/doom/snippets/snippets/elixir-mode/mdoc" nil nil)
                       ("iop" "IO.puts(\"$1 #{inspect $1}\")$0" "iop" nil nil nil "/home/orion/.config/doom/snippets/snippets/elixir-mode/iop" nil nil)
                       ("io" "IO.puts(\"$1\")$0" "io" nil nil nil "/home/orion/.config/doom/snippets/snippets/elixir-mode/io" nil nil)
                       ("ife" "if ${1:condition} do\n  $2\nelse\n  $3\nend" "if-else" nil nil nil "/home/orion/.config/doom/snippets/snippets/elixir-mode/ife" nil nil)
                       ("if" "if ${1:condition} do\n  $0\nend" "if" nil nil nil "/home/orion/.config/doom/snippets/snippets/elixir-mode/if" nil nil)
                       ("hinfo" "def handle_info($1, ${2:state}) do\n  $0\nend" "hinfo" nil nil nil "/home/orion/.config/doom/snippets/snippets/elixir-mode/hinfo" nil nil)
                       ("hcast" "def handle_cast($1, ${2:state}) do\n  $0\nend" "hcast" nil nil nil "/home/orion/.config/doom/snippets/snippets/elixir-mode/hcast" nil nil)
                       ("hcall" "def handle_call($1, _from, ${2:state}) do\n  $0\nend" "hcall" nil nil nil "/home/orion/.config/doom/snippets/snippets/elixir-mode/hcall" nil nil)
                       ("df" "def $1($2)${3:$$(when (and yas-moving-away-p yas-modified-p) (concat \" when \" yas-text))}, do: $0" "function-one-line" nil nil nil "/home/orion/.config/doom/snippets/snippets/elixir-mode/function-one-line" nil nil)
                       ("dfun" "def $1($2)${3:$$(when (and yas-moving-away-p yas-modified-p) (concat \" when \" yas-text))} do\n    $0\nend" "function" nil nil nil "/home/orion/.config/doom/snippets/snippets/elixir-mode/function" nil nil)
                       ("for" "for ${2:x} <- ${1:enum} do\n  $2$0\nend" "for" nil nil nil "/home/orion/.config/doom/snippets/snippets/elixir-mode/for" nil nil)
                       ("fn" "fn ${1:x} -> $1$0 end" "fn" nil nil nil "/home/orion/.config/doom/snippets/snippets/elixir-mode/fn" nil nil)
                       ("doc" "@doc \"\"\"\n$0\n\"\"\"" "doc" nil nil nil "/home/orion/.config/doom/snippets/snippets/elixir-mode/doc" nil nil)
                       ("do" "do\n  $0\nend" "do" nil nil nil "/home/orion/.config/doom/snippets/snippets/elixir-mode/do" nil nil)
                       ("defp" "defp $1 do\n  $0\nend" "defp" nil nil nil "/home/orion/.config/doom/snippets/snippets/elixir-mode/defp" nil nil)
                       ("dm" "defmodule ${1:`(concat (capitalize (file-name-nondirectory (directory-file-name (file-name-directory buffer-file-name)))) \".\")`}${2:`(mapconcat 'capitalize (split-string (file-name-base) \"_\") \"\")`} do\n  $0\nend" "defmodule XXX end" nil nil nil "/home/orion/.config/doom/snippets/snippets/elixir-mode/defmodule_filename" nil nil)
                       ("defmodule" "defmodule $1 do\n  $0\nend" "defmodule" nil nil nil "/home/orion/.config/doom/snippets/snippets/elixir-mode/defmodule" nil nil)
                       ("defmacrop" "defmacrop $1 do\n  $0\nend" "defmacrop" nil nil nil "/home/orion/.config/doom/snippets/snippets/elixir-mode/defmacrop" nil nil)
                       ("defmacro" "defmacro $1 do\n  $0\nend" "defmacro" nil nil nil "/home/orion/.config/doom/snippets/snippets/elixir-mode/defmacro" nil nil)
                       ("def" "def ${1:function}${2:(${3:args})} do\n  $0\nend" "def" nil nil nil "/home/orion/.config/doom/snippets/snippets/elixir-mode/def" nil nil)
                       ("cond" "cond do\n  $0\nend" "cond" nil nil nil "/home/orion/.config/doom/snippets/snippets/elixir-mode/cond" nil nil)
                       ("cast" "GenServer.cast(${1:__MODULE__}, $0)" "cast" nil nil nil "/home/orion/.config/doom/snippets/snippets/elixir-mode/cast" nil nil)
                       ("case" "case $1 do\n  $0\nend" "case" nil nil nil "/home/orion/.config/doom/snippets/snippets/elixir-mode/case" nil nil)
                       ("call" "GenServer.call(${1:__MODULE__}, $0)" "call" nil nil nil "/home/orion/.config/doom/snippets/snippets/elixir-mode/call" nil nil)
                       ("after" "after ${1:500} ->\n  $0" "after" nil nil nil "/home/orion/.config/doom/snippets/snippets/elixir-mode/after" nil nil)))


;;; contents of the .yas-setup.el support file:
;;;
;;; emacs-lisp-mode/.yas-setup.el

(defvar doom-modules-dir)

(defvar emacs-snippets-autoload-function-alist
  '((evil-define-command)
    (evil-define-motion)
    (evil-define-operator)
    (evil-define-text-object)
    ;; doom macros
    (def-menu!)
    ;; other plugins
    (defhydra . "%s/body"))
  "An alist that maps special forms to function name format strings.")

(defun %emacs-lisp-evil-autoload ()
  "Generate an autoload function for special Doom and evil macros."
  (let ((form (save-excursion (beginning-of-line 2)
                              (sexp-at-point))))
    (when (and form
               (listp form)
               (assq (car form) emacs-snippets-autoload-function-alist)
               (require 'autoload)
               (not (make-autoload form buffer-file-name)))
      (format " (autoload '%s \"%s\" nil %s)"
              (let ((format (cdr (assq (car form) emacs-snippets-autoload-function-alist))))
                (if format
                    (intern (format format (cadr form)))
                  (cadr form)))
              (file-relative-name
               (file-name-sans-extension buffer-file-name)
               (cond ((file-in-directory-p buffer-file-name doom-modules-dir)
                      (file-truename doom-modules-dir))
                     ((file-in-directory-p buffer-file-name doom-private-dir)
                      (file-truename doom-private-dir))
                     ((doom-project-root))))
              (if (cl-some (lambda (x) (eq (if (listp x) (car x) x) 'interactive)) form)
                  "t"
                "nil")))))
;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("yesno" "(yes-or-no-p \"$1\")" "y-or-n-p" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/yes-or-no-p" nil nil)
                       ("yn" "(y-or-n-p \"$1\")" "y-or-n-p" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/y-or-n-p" nil nil)
                       ("x-word-or-region" ";; example of a command that works on current word or text selection\n(defun down-case-word-or-region ()\n  \"Lower case the current word or text selection.\"\n(interactive)\n(let (pos1 pos2 meat)\n  (if (and transient-mark-mode mark-active)\n      (setq pos1 (region-beginning)\n            pos2 (region-end))\n    (setq pos1 (car (bounds-of-thing-at-point 'symbol))\n          pos2 (cdr (bounds-of-thing-at-point 'symbol))))\n\n  ; now, pos1 and pos2 are the starting and ending positions\n  ; of the current word, or current text selection if exists\n\n  ;; put your code here.\n  $0\n  ;; Some example of things you might want to do\n  (downcase-region pos1 pos2) ; example of a func that takes region as args\n  (setq meat (buffer-substring-no-properties pos1 pos2)) ; grab the text.\n  (delete-region pos1 pos2) ; get rid of it\n  (insert \"newText\") ; insert your new text\n\n  )\n)" "Command that works on region or word" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/x-word-or-region" nil "x-word-or-region")
                       ("x-traverse_dir" ";; apply a function to all files in a dir\n(require 'find-lisp)\n(mapc 'my-process-file (find-lisp-find-files \"~/myweb/\" \"\\\\.html$\"))" "traversing a directory" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/x-traverse_dir" nil "x-traverse_dir")
                       ("x-grabthing" "(setq $0 (thing-at-point 'symbol))" "grab word under cursor" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/x-grabthing" nil "x-grabthing")
                       ("x-grabstring" "(setq $0 (buffer-substring-no-properties myStartPos myEndPos))" "grab buffer substring" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/x-grabstring" nil "x-grabstring")
                       ("x-find-replace" "(defun replace-html-chars-region (start end)\n  \"Replace “<” to “&lt;” and other chars in HTML.\nThis works on the current region.\"\n  (interactive \"r\")\n  (save-restriction \n    (narrow-to-region start end)\n    (goto-char (point-min))\n    (while (search-forward \"&\" nil t) (replace-match \"&amp;\" nil t))\n    (goto-char (point-min))\n    (while (search-forward \"<\" nil t) (replace-match \"&lt;\" nil t))\n    (goto-char (point-min))\n    (while (search-forward \">\" nil t) (replace-match \"&gt;\" nil t))\n    )\n  )" "find and replace on region" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/x-find-replace" nil "x-find-replace")
                       ("x-file" "(defun read-lines (filePath)\n  \"Return a list of lines in FILEPATH.\"\n  (with-temp-buffer\n    (insert-file-contents filePath)\n    (split-string\n     (buffer-string) \"\\n\" t)) )\n\n;; process all lines\n(mapc \n (lambda (aLine) \n   (message aLine) ; do your stuff here\n   )\n (read-lines \"inputFilePath\")\n)" "read lines of a file" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/x-file.read-lines" nil "x-file")
                       ("x-file" "(defun doThisFile (fpath)\n  \"Process the file at path FPATH ...\"\n  (let ()\n    ;; create temp buffer without undo record or font lock. (more efficient)\n    ;; first space in temp buff name is necessary\n    (set-buffer (get-buffer-create \" myTemp\"))\n    (insert-file-contents fpath nil nil nil t)\n\n    ;; process it ...\n    ;; (goto-char 0) ; move to begining of file's content (in case it was open)\n    ;; ... do something here\n    ;; (write-file fpath) ;; write back to the file\n\n    (kill-buffer \" myTemp\")))" "a function that process a file" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/x-file.process" nil "x-file")
                       ("x-dired" ";; idiom for processing a list of files in dired's marked files\n \n;; suppose myProcessFile is your function that takes a file path\n;; and do some processing on the file\n\n(defun dired-myProcessFile ()\n  \"apply myProcessFile function to marked files in dired.\"\n  (interactive)\n  (require 'dired)\n  (mapc 'myProcessFile (dired-get-marked-files))\n)\n\n;; to use it, type M-x dired-myProcessFile" "process marked files in dired" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/x-dired.process_marked" nil "x-dired")
                       ("<" "\"\\\\_<${1:word}\\\\_>\"" "word_regexp" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/word_regexp" nil "<")
                       ("word-or-region" ";; example of a command that works on current word or text selection\n(defun down-case-word-or-region ()\n  \"Lower case the current word or text selection.\"\n(interactive)\n(let (pos1 pos2 meat)\n  (if (and transient-mark-mode mark-active)\n      (setq pos1 (region-beginning)\n            pos2 (region-end))\n    (setq pos1 (car (bounds-of-thing-at-point 'symbol))\n          pos2 (cdr (bounds-of-thing-at-point 'symbol))))\n\n  ; now, pos1 and pos2 are the starting and ending positions\n  ; of the current word, or current text selection if exists\n\n  ;; put your code here.\n  $0\n  ;; Some example of things you might want to do\n  (downcase-region pos1 pos2) ; example of a func that takes region as args\n  (setq meat (buffer-substring-no-properties pos1 pos2)) ; grab the text.\n  (delete-region pos1 pos2) ; get rid of it\n  (insert \"newText\") ; insert your new text\n\n  )\n)\n" "Command that works on region or word" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/word-or-region" nil nil)
                       ("wcb" "(with-current-buffer ${1:buffer} ${0:`(doom-snippets-format \"%n%s\")`})`(doom-snippets-newline-or-eol)`" "with-current-buffer" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/with-current-buffer" nil "wcb")
                       ("wg" "(widget-get $0 )" "widget-get" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/widget-get" nil "widget-get")
                       ("while" "(while ${1:condition} ${0:`(doom-snippets-format \"%n%s\")`})`(doom-snippets-newline-or-eol)`" "while" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/while" nil "while")
                       ("wl" "(when-let* (${1:var-list}) `(doom-snippets-format \"%n%s\")`$0)`(doom-snippets-newline-or-eol)`" "when-let" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/when-let" nil "wl")
                       ("when" "(when ${1:t} `(doom-snippets-format \"%n%s\")`$0)`(doom-snippets-newline-or-eol)`" "when" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/when" nil nil)
                       ("v" "(vector $0)" "vector" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/vector" nil "vector")
                       ("up" "(use-package ${1:package}\n  :${2:config}\n  `%`$0)" "use-package" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/use-package" nil "up")
                       ("unless" "(unless ${1:condition} ${0:`(doom-snippets-format \"%n%s\")`})`(doom-snippets-newline-or-eol)`" "unless" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/unless" nil nil)
                       ("traverse_dir" ";; apply a function to all files in a dir\n(require 'find-lisp)\n(mapc 'my-process-file (find-lisp-find-files \"~/myweb/\" \"\\\\.html$\"))\n" "traversing a directory" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/traverse_dir" nil nil)
                       ("tap" "(thing-at-point '$0) ; symbol, list, sexp, defun, filename, url, email, word, sentence, whitespace, line, page ..." "thing-at-point" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/thing-at-point" nil "thing-at-point")
                       ("substring" "(substring STRING$0 FROM &optional TO)" "substring" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/substring" nil "substring")
                       ("stringp" "(stringp $0)" "stringp" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/stringp" nil "stringp")
                       ("string=" "(string= ${1:`%`} ${2:str})" "string=" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/string=" nil "string=")
                       ("stn" "(string-to-number \"`%`$0\")" "string-to-number" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/string-to-number" nil "stn")
                       ("string-match-p" "(string-match-p \"${0:regexp}\" ${1:string}${2: ${3:START}})" "string-match-p" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/string-match-p" nil nil)
                       ("string-match" "(string-match \"${0:regexp}\" ${1:string}${2: ${3:START}})" "string-match" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/string-match" nil nil)
                       ("string" "(string $0 )" "string" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/string" nil "string")
                       ("ss" "(split-string $0 &optional SEPARATORS OMIT-NULLS)" "split-string" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/split-string" nil "split-string")
                       ("scf" "(skip-chars-forward \"$0\" &optional LIM)" "skip-chars-forward" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/skip-chars-forward" nil "skip-chars-forward")
                       ("scb" "(skip-chars-backward \"$0\" &optional LIM)" "skip-chars-backward" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/skip-chars-backward" nil "skip-chars-backward")
                       ("setqd" "(setq-default ${1:`%`} ${0:value})\n" "setq-default" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/setq-default" nil "setqd")
                       ("setq" "(setq ${1:var} ${0:`%`})" "setq" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/setq" nil nil)
                       ("sm" "(set-mark $0)" "set-mark" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/set-mark" nil "set-mark")
                       ("sfm" "(set-file-modes $0 MODE)" "set-file-modes" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/set-file-modes" nil "set-file-modes")
                       ("sb" "(set-buffer $0 )" "set-buffer" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/set-buffer" nil "set-buffer")
                       ("set" "(set $0 )" "set" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/set" nil "set")
                       ("sfr" "(search-forward-regexp \"$0\" &optional BOUND NOERROR COUNT)" "search-forward-regexp" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/search-forward-regexp" nil "search-forward-regexp")
                       ("sf" "(search-forward \"$0\" &optional BOUND NOERROR COUNT)" "search-forward" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/search-forward" nil "search-forward")
                       ("sbr" "(search-backward-regexp \"$0\" &optional BOUND NOERROR COUNT)" "search-backward-regexp" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/search-backward-regexp" nil "search-backward-regexp")
                       ("sb" "(search-backward \"$0\" &optional BOUND NOERROR COUNT)" "search-backward" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/search-backward" nil "search-backward")
                       ("se" "(save-excursion `(doom-snippets-format \"%n%s\")`$0)" "save-excursion" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/save-excursion" nil "save-excursion")
                       ("sb" "(save-buffer $0)" "save-buffer" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/save-buffer" nil "save-buffer")
                       ("rest" "&rest `%`$0" "&rest ..." nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/rest" nil nil)
                       ("require" "(require '${1:package})" "require" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/require" nil nil)
                       ("req"
                        (progn
                          (doom-snippets-expand :uuid "require"))
                        "require" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/req" nil "req")
                       ("rris" "(replace-regexp-in-string REGEXP$0 REP STRING &optional FIXEDCASE LITERAL SUBEXP START)" "replace-regexp-in-string" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/replace-regexp-in-string" nil "replace-regexp-in-string")
                       ("rr" "(replace-regexp REGEXP$0 TO-STRING &optional DELIMITED START END)" "replace-regexp" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/replace-regexp" nil "replace-regexp")
                       ("repeat" "(repeat $0 )" "repeat" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/repeat" nil "repeat")
                       ("rf" "(rename-file FILE$0 NEWNAME &optional OK-IF-ALREADY-EXISTS)" "rename-file" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/rename-file" nil "rename-file")
                       ("re" "(region-end)" "region-end" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/region-end" nil "region-end")
                       ("rb" "(region-beginning)" "region-beginning" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/region-beginning" nil "region-beginning")
                       ("rap" "(region-active-p)" "region-active-p" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/region-active-p" nil "region-active-p")
                       ("rsf" "(re-search-forward REGEXP$0 &optional BOUND NOERROR COUNT)" "re-search-forward" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/re-search-forward" nil "re-search-forward")
                       ("rsb" "(re-search-backward REGEXP$0 &optional BOUND NOERROR COUNT)" "re-search-backward" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/re-search-backward" nil "re-search-backward")
                       ("put" "(put $0 PROPNAME VALUE)" "put" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/put" nil "put")
                       ("push" "(push ${0:`(doom-snippets-format \"newelt\")`} ${1:place})\n" "push" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/push" nil "push")
                       ("provide" "(provide '`(file-name-base buffer-file-name)`)" "provide" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/provide" nil nil)
                       ("progn" "(progn `(doom-snippets-format \"%n%s\")`$0)`(doom-snippets-newline-or-eol)`" "progn" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/progn" nil nil)
                       ("print" "(print $0)" "print" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/print" nil "print")
                       ("princ" "(princ $0)" "princ" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/princ" nil "princ")
                       ("pmi" "(point-min)" "point-min" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/point-min" nil "point-min")
                       ("pma" "(point-max)" "point-max" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/point-max" nil "point-max")
                       ("pt" "(point)" "point" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/point" nil nil)
                       ("pcase" "(pcase ${1:var}\n  $>(${2:_} ${3:`(doom-snippets-format \"%n%s\")`})$0)`(doom-snippets-newline-or-eol)`" "pcase" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/pcase" nil nil)
                       ("or" "(or `%`$0)" "or" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/or" nil nil)
                       ("optional" "&optional `%`$0" "&optional ..." nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/optional" nil "optional")
                       ("opt"
                        (progn
                          (doom-snippets-expand :uuid "optional"))
                        "&optional ..." nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/opt" nil "opt")
                       ("number-to-string" "(number-to-string `(doom-snippets-format \"%n%s\")`$0)" "number-to-string" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/number-to-string" nil nil)
                       ("null" "(null `(doom-snippets-format \"%n%s\")`$0)" "null" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/null" nil nil)
                       ("nth" "(nth ${0:n} ${1:`%`list})" "nth" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/nth" nil nil)
                       ("not" "(not $0)" "not" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/not" nil nil)
                       ("minor" "(defvar ${1:mode}-modeline-indicator \" ${2:INDICATOR}\"\n  \"call ($1-install-mode) again if this is changed\")\n\n(defvar $1-mode nil)\n(make-variable-buffer-local '$1-mode)\n(put '$1-mode 'permanent-local t)\n\n(defun $1-mode (&optional arg)\n  \"$0\"\n  (interactive \"P\")\n  (setq $1-mode\n        (if (null arg) (not $1-mode)\n          (> (prefix-numeric-value arg) 0)))\n  (force-mode-line-update))\n\n(provide '$1-mode)" "minor_mode" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/minor_mode" nil "minor")
                       ("m" "(message \"${1:`%`}\"$0)" "message" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/message" nil "m")
                       ("memq" "(memq ${0:sym} ${1:`%`list})" "memq" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/memq" nil nil)
                       ("ms" "(match-string $0 )" "match-string" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/match-string" nil "match-string")
                       ("me" "(match-end N$0)" "match-end" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/match-end" nil "match-end")
                       ("mb" "(match-beginning N$0)" "match-beginning" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/match-beginning" nil "match-beginning")
                       ("mapcl" "(mapc (lambda (${2:x}) `(doom-snippets-format \"%n%s\")`$0)\n      ${1:sequence})" "mapc lambda" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/mapcl" nil nil)
                       ("mapcar" "(mapcar ${1:fn} ${0:list})" "mapcar" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/mapcar" nil nil)
                       ("mapc" "(mapc ${1:fn} ${0:list})" "mapc" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/mapc" nil nil)
                       ("mlv" "(make-local-variable $0)" "make-local-variable" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/make-local-variable" nil "make-local-variable")
                       ("mht" "(make-hash-table${1: :test '${2:equal}})" "hash" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/make-hash-table" nil "mht")
                       ("md" "(make-directory $0 &optional PARENTS)" "make-directory" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/make-directory" nil "make-directory")
                       ("looking-at" "(looking-at $0)" "looking-at" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/looking-at" nil nil)
                       ("list" "(list `%`$0)" "list" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/list" nil nil)
                       ("lep" "(line-end-position)" "line-end-position" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/line-end-position" nil "line-end-position")
                       ("lbp" "(line-beginning-position)" "line-beginning-position" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/line-beginning-position" nil "line-beginning-position")
                       ("let" "(let ($1) `(doom-snippets-format \"%n%s\")`)`(doom-snippets-newline-or-eol)`" "let" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/let" nil nil)
                       ("length" "(length `%`$0)" "length" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/length" nil nil)
                       ("lambda" "(lambda ($1) (interactive) `(doom-snippets-format \"%n%s\")`$0)" "lambda" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/lambda" nil "lambda")
                       ("lam" "(λ! $0)" "lambda shortcut" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/lam" nil "lam")
                       ("kb" "(kill-buffer $0)" "kill-buffer" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/kill-buffer" nil "kill-buffer")
                       ("kbd" "(kbd \"${0:`%`}\")" "kbd" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/kbd" nil nil)
                       ("interactive" "(interactive$1)$0" "interactive" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/interactive" nil nil)
                       ("int" "(interactive)$0" "interactive" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/int" nil "int")
                       ("ifc" "(insert-file-contents $0 &optional VISIT BEG END REPLACE)" "insert-file-contents" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/insert-file-contents" nil "insert-file-contents")
                       ("i" "(insert $0)" "insert" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/insert" nil "insert")
                       ("if-let" "(if-let (($1)) $0 `(doom-snippets-format \"%n%s\")`)`(doom-snippets-newline-or-eol)`" "if-let"
                        (>
                         (doom-snippets-count-lines %)
                         1)
                        nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/if-let-2" nil "if-let-2")
                       ("if-let" "(if-let* (($1)) `%`$0)`(doom-snippets-newline-or-eol)`" "if-let"
                        (<=
                         (doom-snippets-count-lines %)
                         1)
                        nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/if-let" nil nil)
                       ("if" "(if ${1:t}`(if % (concat \" \" (doom-snippets-format \"%n%s\")))`$0)" "if" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/if" nil nil)
                       ("grabthing" "(setq $0 (thing-at-point 'symbol))\n" "grab word under cursor" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/grabthing" nil nil)
                       ("grabstring" "(setq $0 (buffer-substring-no-properties myStartPos myEndPos))\n" "grab buffer substring" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/grabstring" nil nil)
                       ("goto-char" "(goto-char $0)" "goto-char" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/goto-char" nil nil)
                       ("gsk" "(global-set-key (kbd \"C-$0\") 'COMMAND)" "global-set-key" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/global-set-key" nil "global-set-key")
                       ("get" "(get SYMBOL$0 PROPNAME)" "get" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/get" nil "get")
                       ("function" "(function $0)" "function" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/function" nil nil)
                       ("funcall" "(funcall $0)" "funcall" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/funcall" nil "funcall")
                       ("fl" "(forward-line $0 )" "forward-line" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/forward-line" nil "forward-line")
                       ("fc" "(forward-char $0)" "forward-char" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/forward-char" nil "forward-char")
                       ("format" "(format \"$0\" $1)" "format" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/format" nil nil)
                       ("find-replace" "(defun replace-html-chars-region (start end)\n  \"Replace “<” to “&lt;” and other chars in HTML.\nThis works on the current region.\"\n  (interactive \"r\")\n  (save-restriction \n    (narrow-to-region start end)\n    (goto-char (point-min))\n    (while (search-forward \"&\" nil t) (replace-match \"&amp;\" nil t))\n    (goto-char (point-min))\n    (while (search-forward \"<\" nil t) (replace-match \"&lt;\" nil t))\n    (goto-char (point-min))\n    (while (search-forward \">\" nil t) (replace-match \"&gt;\" nil t))\n    )\n  )\n" "find and replace on region" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/find-replace" nil nil)
                       ("ff" "(find-file $0 )" "find-file" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/find-file" nil "find-file")
                       ("file.read-lines" "(defun read-lines (filePath)\n  \"Return a list of lines in FILEPATH.\"\n  (with-temp-buffer\n    (insert-file-contents filePath)\n    (split-string\n     (buffer-string) \"\\n\" t)) )\n\n;; process all lines\n(mapc \n (lambda (aLine) \n   (message aLine) ; do your stuff here\n   )\n (read-lines \"inputFilePath\")\n)" "read lines of a file" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/file.read-lines" nil nil)
                       ("file.process" "(defun doThisFile (fpath)\n  \"Process the file at path FPATH ...\"\n  (let ()\n    ;; create temp buffer without undo record or font lock. (more efficient)\n    ;; first space in temp buff name is necessary\n    (set-buffer (get-buffer-create \" myTemp\"))\n    (insert-file-contents fpath nil nil nil t)\n\n    ;; process it ...\n    ;; (goto-char 0) ; move to begining of file's content (in case it was open)\n    ;; ... do something here\n    ;; (write-file fpath) ;; write back to the file\n\n    (kill-buffer \" myTemp\")))\n" "a function that process a file" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/file.process" nil nil)
                       ("frn" "(file-relative-name $0 )" "file-relative-name" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/file-relative-name" nil "file-relative-name")
                       ("fnse" "(file-name-sans-extension $0)" "file-name-sans-extension" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/file-name-sans-extension" nil "file-name-sans-extension")
                       ("fnn" "(file-name-nondirectory $0 )" "file-name-nondirectory" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/file-name-nondirectory" nil "file-name-nondirectory")
                       ("file-name-extension" "(file-name-extension $0)" "file-name-extension" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/file-name-extension" nil nil)
                       ("fnd" "(file-name-directory $0)" "file-name-directory" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/file-name-directory" nil "file-name-directory")
                       ("fboundp" "(fboundp '$0)" "fboundp" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/fboundp" nil nil)
                       ("expand-file-name" "(expand-file-name `%`$0${1: ${2:default-directory}})" "expand-file-name" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/expand-file-name" nil nil)
                       ("error" "(error \"$0\" &optional ARGS)" "error" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/error" nil "error")
                       ("equal" "(equal $0)" "equal" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/equal" nil "equal")
                       ("eq" "(eq $0)" "eq" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/eq" nil "eq")
                       ("eol" "(end-of-line)" "end-of-line" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/end-of-line" nil "end-of-line")
                       ("AH"
                        (progn
                          (doom-snippets-expand :uuid "add-hook!"))
                        "add-hook!" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/doom-add-hook-short" nil "AH")
                       ("doom-add-hook" "(add-hook! ${0:hook} ${1:`(doom-snippets-format \"function\")`})`(doom-snippets-newline-or-eol)`\n" "add-hook!" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/doom-add-hook" nil nil)
                       ("dolist" "(dolist (${1:i} ${2:list}) `(doom-snippets-format \"%n%s\")`$0)`(doom-snippets-newline-or-eol)`" "dolist" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/dolist" nil nil)
                       ("dired.process_marked" ";; idiom for processing a list of files in dired's marked files\n \n;; suppose myProcessFile is your function that takes a file path\n;; and do some processing on the file\n\n(defun dired-myProcessFile ()\n  \"apply myProcessFile function to marked files in dired.\"\n  (interactive)\n  (require 'dired)\n  (mapc 'myProcessFile (dired-get-marked-files))\n)\n\n;; to use it, type M-x dired-myProcessFile\n" "process marked files in dired" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/dired.process_marked" nil nil)
                       ("df" "(directory-files $0 &optional FULL MATCH NOSORT)" "directory-files" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/directory-files" nil "directory-files")
                       ("dr" "(delete-region $0 )" "delete-region" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/delete-region" nil "delete-region")
                       ("df" "(delete-file $0)" "delete-file" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/delete-file" nil "delete-file")
                       ("dd" "(delete-directory $0 &optional RECURSIVE)" "delete-directory" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/delete-directory" nil "delete-directory")
                       ("dc" "(delete-char $0)" "delete-char" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/delete-char" nil "delete-char")
                       ("defvar" "(defvar ${1:var} ${2:`(or % \"value\")`}\n  $>\"${3:TODO}\")" "defvar" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/defvar" nil nil)
                       ("defun" "(defun ${1:name} ($2)`(and % (concat \" \" (doom-snippets-format \"%n%s\")))`$0)`(doom-snippets-newline-or-eol)`" "defun" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/defun" nil nil)
                       ("defsubst" "(defsubst $0 )" "defsubst" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/defsubst" nil "defsubst")
                       ("defmacro" "(defmacro ${1:name} ($2) `(doom-snippets-format \"%n%s\")`$0)`(doom-snippets-newline-or-eol)`" "defmacro" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/defmacro" nil nil)
                       ("defm"
                        (progn
                          (doom-snippets-expand :uuid "defmacro"))
                        "defmacro" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/defm" nil "defm")
                       ("dk" "(define-key ${1:keymap} ${2:key} ${0:fn})" "define-key" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/define-key" nil "dk")
                       ("defcustom" "(defcustom $1 ${2:VALUE} \"${3:doc}\" $4)" "defcustom" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/defcustom" nil nil)
                       ("defconst" "(defconst ${1:var} ${2:`(or % \"value\")`}\n  $>\"${3:TODO}\")" "defconst" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/defconst" nil nil)
                       ("defalias" "(defalias 'SYMBOL$0 'DEFINITION &optional DOCSTRING)" "defalias" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/defalias" nil "defalias")
                       ("dp" "(def-package! ${1:package}\n  :${2:config}\n  `%`$0)" "def-package! ..." nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/def-package!" nil "dp")
                       ("def"
                        (progn
                          (doom-snippets-expand :uuid "defun"))
                        "defun" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/def" nil "def")
                       ("ca" "(custom-autoload$0 SYMBOL LOAD &optional NOSET)" "custom-autoload" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/custom-autoload" nil "custom-autoload")
                       ("cb" "(current-buffer)" "current-buffer" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/current-buffer" nil "cb")
                       ("cf" "(copy-file FILE$0 NEWNAME &optional OK-IF-ALREADY-EXISTS KEEP-TIME PRESERVE-UID-GID)" "copy-file" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/copy-file" nil "copy-file")
                       ("cd" "(copy-directory $0 NEWNAME &optional KEEP-TIME PARENTS)" "copy-directory" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/copy-directory" nil "copy-directory")
                       ("consp" "(consp $0 )" "consp" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/consp" nil "consp")
                       ("cons" "(cons ${1:`%`} $0)" "cons" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/cons" nil nil)
                       ("cc" "(condition-case ex\n    $>${0:`%`}\n  (error $0))" "condition-case" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/condition-case" nil "cc")
                       ("cond" "(cond (${1:t} ${2:`(doom-snippets-format \"%n%s\")`})$0)" "cond" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/cond" nil nil)
                       ("concat" "(concat `%`$0)" "concat" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/concat" nil nil)
                       ("cdb" "(cl-destructuring-bind (${1:args})\n    ${2:expr}\n  `%`$0)" "cl-destructuring-bind" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/cl-destructuring-bind" nil "cdb")
                       ("cl-defun" "(cl-defun ${1:name} ($2) `(doom-snippets-format \"%n%s\")`$0)`(doom-snippets-newline-or-eol)`" "cl-defun" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/cl-defun" nil "cl-defun")
                       ("cl-defmacro" "(cl-defmacro ${1:name} ($2) `(doom-snippets-format \"%n%s\")`$0)`(doom-snippets-newline-or-eol)`" "cl-defmacro" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/cl-defmacro" nil "cl-defmacro")
                       ("cdr" "(cdr ${0:`%`})" "cdr" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/cdr" nil nil)
                       ("cdefm"
                        (progn
                          (doom-snippets-expand :uuid "cl-defmacro"))
                        "cl-defmacro" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/cdefm" nil "cdefm")
                       ("cdef"
                        (progn
                          (doom-snippets-expand :uuid "cl-defun"))
                        "cl-defun" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/cdef" nil "cdef")
                       ("car" "(car ${0:`%`})" "car" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/car" nil nil)
                       ("bsnp" "(buffer-substring-no-properties ${1:start} ${2:end})" "buffer-substring-no-properties" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/buffer-substring-no-properties" nil "bsnp")
                       ("bs" "(buffer-substring ${1:start} ${2:end})" "buffer-substring" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/buffer-substring" nil "bs")
                       ("bmp" "(buffer-modified-p $0)" "buffer-modified-p" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/buffer-modified-p" nil "buffer-modified-p")
                       ("bfn" "buffer-file-name" "buffer-file-name" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/buffer-file-name" nil "bfn")
                       ("botap" "(bounds-of-thing-at-point '$0) ; symbol, list, sexp, defun, filename, url, email, word, sentence, whitespace, line, page ..." "bounds-of-thing-at-point" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/bounds-of-thing-at-point" nil "bounds-of-thing-at-point")
                       ("bol" "(beginning-of-line)" "beginning-of-line" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/beginning-of-line" nil "beginning-of-line")
                       ("bc" "(backward-char $0)" "backward-char" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/backward-char" nil "backward-char")
                       ("autoload" "(autoload '${1:fn} \"${2:file}\"${3:\"`%`${4:doc}\" ${5:t} ${6:type}})" "autoload" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/autoload" nil nil)
                       ("ad" ";;;###autodef" "doom autodef tag"
                        (doom-snippets-bolp)
                        nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/autodef" nil "ad")
                       ("au" ";;;###autoload`(%emacs-lisp-evil-autoload)`$0" "autoload tag"
                        (doom-snippets-without-trigger
                         (bolp))
                        nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/au" nil nil)
                       ("assq" "(assq ${0:`%`sym} ${1:list})" "assq" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/assq" nil nil)
                       ("aset" "(aset ${1:array} ${2:index} ${3:newelt})" "aset" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/aset" nil nil)
                       ("aref" "(aref ${1:array} ${0:index})" "aref" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/aref" nil nil)
                       ("apply" "(apply $0)" "apply" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/apply" nil "apply")
                       ("append" "(append `%`$0)" "append" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/append" nil nil)
                       ("and" "(and `%`$0)" "and" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/and" nil nil)
                       ("ah"
                        (progn
                          (doom-snippets-expand :uuid "add-hook"))
                        "add-hook" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/ah" nil "ah")
                       ("after" "(after! ${1:feature-name} `(doom-snippets-format \"%n%s\")`$0)`(doom-snippets-newline-or-eol)`" "after" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/after" nil nil)
                       ("adv" "(defun ${3:adviser-name} (orig-fn &rest args)\n  ${4:`%`}\n  (apply orig-fn args))\n(advice-add #'${1:function-name} ${2::around} #'${3:adviser-name})" "advise function" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/advise" nil "adv")
                       ("add-hook" "(add-hook '${0:hook} ${1:function})`(doom-snippets-newline-or-eol)`" "add-hook" nil nil nil "/home/orion/.config/doom/snippets/snippets/emacs-lisp-mode/add-hook" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("h" "help $0" "help" nil nil nil "/home/orion/.config/doom/snippets/snippets/erc-mode/help" nil "h")
                       ("b" "blist\n" "blist" nil nil nil "/home/orion/.config/doom/snippets/snippets/erc-mode/blist" nil "b")))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("wi" "with {\n  ${1:expression}\n};\n$0" "with" nil nil nil "/home/orion/.config/doom/snippets/snippets/faust-mode/with" nil nil)
                       ("vs" "vslider(\"${1:name}\", ${2:default}, ${3:min}, ${4:max}, ${5:step})$0" "vslider" nil nil nil "/home/orion/.config/doom/snippets/snippets/faust-mode/vslider" nil nil)
                       ("vg" "vgroup(\"${1:name}\", ${2:expression})$0" "vgroup" nil nil nil "/home/orion/.config/doom/snippets/snippets/faust-mode/vgroup" nil nil)
                       ("vb" "vbargraph(\"${1:name}\", ${2:min}, ${3:max})$0" "vbargraph" nil nil nil "/home/orion/.config/doom/snippets/snippets/faust-mode/vbargraph" nil nil)
                       ("tg" "tgroup(\"${1:name}\", ${2:expression})$0" "tgroup" nil nil nil "/home/orion/.config/doom/snippets/snippets/faust-mode/tgroup" nil nil)
                       ("su" "sum(${1:i}, ${2:Nr}, ${3:expression})$0" "sum" nil nil nil "/home/orion/.config/doom/snippets/snippets/faust-mode/sum" nil nil)
                       ("se" "seq(${1:i}, ${2:Nr}, ${3:expression})$0" "seq" nil nil nil "/home/orion/.config/doom/snippets/snippets/faust-mode/seq" nil nil)
                       ("ru" "  (${1:pattern}) => ${2:expression};\n$0" "rule" nil nil nil "/home/orion/.config/doom/snippets/snippets/faust-mode/rule" nil nil)
                       ("mu" "prod(${1:i}, ${2:Nr}, ${3:expression})$0" "prod" nil nil nil "/home/orion/.config/doom/snippets/snippets/faust-mode/prod" nil nil)
                       ("px" "process(${1:x}) = ${2:expression}($1);" "processx" nil nil nil "/home/orion/.config/doom/snippets/snippets/faust-mode/processx" nil nil)
                       ("pr" "process = $1;\n$0" "process" nil nil nil "/home/orion/.config/doom/snippets/snippets/faust-mode/process" nil nil)
                       ("pa" "par(${1:i}, ${2:Nr}, ${3:expression})$0" "par" nil nil nil "/home/orion/.config/doom/snippets/snippets/faust-mode/par" nil nil)
                       ("ne" "nentry(\"${1:name}\", ${2:default}, ${3:min}, ${4:max}, ${5:step})$0" "nentry" nil nil nil "/home/orion/.config/doom/snippets/snippets/faust-mode/nentry" nil nil)
                       ("im" "import(\"$1.lib\");$0" "import" nil nil nil "/home/orion/.config/doom/snippets/snippets/faust-mode/import" nil nil)
                       ("hs" "hslider(\"${1:name}\", ${2:default}, ${3:min}, ${4:max}, ${5:step})$0" "hslider" nil nil nil "/home/orion/.config/doom/snippets/snippets/faust-mode/hslider" nil nil)
                       ("hg" "hgroup(\"${1:name}\", ${2:expression})$0" "hgroup" nil nil nil "/home/orion/.config/doom/snippets/snippets/faust-mode/hgroup" nil nil)
                       ("he" "declare name \"$1\";\ndeclare version \"${2:0.1}\";\ndeclare author \"$3\";\ndeclare license \"${4:$$\n  (yas-choose-value '(\n    \"AGPLv3\"\n    \"Apache\"\n    \"BSD 2-clause\"\n    \"BSD 3-clause\"\n    \"GPLv2\"\n    \"GPLv3\"\n    \"LGPLv3\"\n    \"MIT\"\n  ))}\";\n$0\n" "header" nil nil nil "/home/orion/.config/doom/snippets/snippets/faust-mode/header" nil nil)
                       ("hb" "hbargraph(\"${1:name}\", ${2:min}, ${3:max})$0" "hbargraph" nil nil nil "/home/orion/.config/doom/snippets/snippets/faust-mode/hbargraph" nil nil)
                       ("dv" "declare version \"${1:0.1}\";\n$0" "declare version" nil nil nil "/home/orion/.config/doom/snippets/snippets/faust-mode/declareversion" nil nil)
                       ("dn" "declare name \"$1\";\n$0" "declare name" nil nil nil "/home/orion/.config/doom/snippets/snippets/faust-mode/declarename" nil nil)
                       ("dl" "declare license \"${1:$$\n  (yas-choose-value '(\n    \"AGPLv3\"\n    \"Apache\"\n    \"BSD 2-clause\"\n    \"BSD 3-clause\"\n    \"GPLv2\"\n    \"GPLv3\"\n    \"LGPLv3\"\n    \"MIT\"\n  ))}\";\n$0" "declare license" nil nil nil "/home/orion/.config/doom/snippets/snippets/faust-mode/declarelicense" nil nil)
                       ("da" "declare author \"$1\";\n$0" "declare author" nil nil nil "/home/orion/.config/doom/snippets/snippets/faust-mode/declareauthor" nil nil)
                       ("de" "declare ${1:key} \"${2:value}\";\n$0" "declare" nil nil nil "/home/orion/.config/doom/snippets/snippets/faust-mode/declare" nil nil)
                       ("co" "component(\"$1.dsp\")$0" "component" nil nil nil "/home/orion/.config/doom/snippets/snippets/faust-mode/component" nil nil)
                       ("ch" "checkbox(\"$1\")$0" "checkbox" nil nil nil "/home/orion/.config/doom/snippets/snippets/faust-mode/checkbox" nil nil)
                       ("ca" "case {\n  $1\n  };\n$0" "case" nil nil nil "/home/orion/.config/doom/snippets/snippets/faust-mode/case" nil nil)
                       ("bu" "button(\"$1\")$0" "button" nil nil nil "/home/orion/.config/doom/snippets/snippets/faust-mode/button" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("while" "(while ${1:t}`(if % (concat \" \" (doom-snippets-format \"%n%s\")))`$0)" "while" nil nil nil "/home/orion/.config/doom/snippets/snippets/fennel-mode/while" nil nil)
                       ("when" "(when ${1:t}`(if % (concat \" \" (doom-snippets-format \"%n%s\")))`$0)" "when" nil nil nil "/home/orion/.config/doom/snippets/snippets/fennel-mode/when" nil nil)
                       ("var" "(var ${1:varname} ${2:value})" "var" nil nil nil "/home/orion/.config/doom/snippets/snippets/fennel-mode/var" nil nil)
                       ("tset" "(tset ${1:table} ${2:field} ${3:value})" "tset" nil nil nil "/home/orion/.config/doom/snippets/snippets/fennel-mode/tset" nil nil)
                       ("tc" "(table.concat ${1:table} ${2:sep}${3: ${4:start} ${5:end}})" "table.concat" nil nil nil "/home/orion/.config/doom/snippets/snippets/fennel-mode/table.concat" nil nil)
                       ("set" "(set ${1:varname} ${2:value})" "set" nil nil nil "/home/orion/.config/doom/snippets/snippets/fennel-mode/set" nil nil)
                       ("req" "(require ${1::module})" "require" nil nil nil "/home/orion/.config/doom/snippets/snippets/fennel-mode/require" nil nil)
                       ("p" "(print ${1:message})" "print" nil nil nil "/home/orion/.config/doom/snippets/snippets/fennel-mode/print" nil nil)
                       ("local" "(local ${1:varname} ${2:value})" "local" nil nil nil "/home/orion/.config/doom/snippets/snippets/fennel-mode/local" nil nil)
                       ("let" "(let [$1] `(doom-snippets-format \"%n%s\")`)`(doom-snippets-newline-or-eol)`" "let" nil nil nil "/home/orion/.config/doom/snippets/snippets/fennel-mode/let" nil nil)
                       ("len" "(length ${1:var})" "length" nil nil nil "/home/orion/.config/doom/snippets/snippets/fennel-mode/length" nil nil)
                       ("lam" "(lambda [$1]`(and % (concat \" \" (doom-snippets-format \"%n%s\")))`$0)`(doom-snippets-newline-or-eol)`" "lambda" nil nil nil "/home/orion/.config/doom/snippets/snippets/fennel-mode/lambda" nil nil)
                       ("if" "(if ${1:t}`(if % (concat \" \" (doom-snippets-format \"%n%s\")))`$0)" "if" nil nil nil "/home/orion/.config/doom/snippets/snippets/fennel-mode/if" nil nil)
                       ("global" "(global ${1:varname} ${2:value})" "global" nil nil nil "/home/orion/.config/doom/snippets/snippets/fennel-mode/global" nil nil)
                       ("for" "(for [${1:i} ${2:1} ${3:10}]`(if % (concat \" \" (doom-snippets-format \"%n%s\")))`$0)" "for" nil nil nil "/home/orion/.config/doom/snippets/snippets/fennel-mode/for" nil nil)
                       ("fn" "(fn ${1:name} [$2]`(and % (concat \" \" (doom-snippets-format \"%n%s\")))`$0)`(doom-snippets-newline-or-eol)`" "fn" nil nil nil "/home/orion/.config/doom/snippets/snippets/fennel-mode/fn" nil nil)
                       ("each" "(each [${1:key} ${2:value} ${3:list}]`(if % (concat \" \" (doom-snippets-format \"%n%s\")))`$0)" "each" nil nil nil "/home/orion/.config/doom/snippets/snippets/fennel-mode/each" nil nil)
                       ("do" "(do`(if % (concat \" \" (doom-snippets-format \"%n%s\")))`$0)" "do" nil nil nil "/home/orion/.config/doom/snippets/snippets/fennel-mode/do" nil nil)))


;;; contents of the .yas-setup.el support file:
;;;
;; -*- no-byte-compile: t; -*-
(require 'doom-snippets-lib);;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("time" "`(current-time-string)`" "(current time)" nil nil nil "/home/orion/.config/doom/snippets/snippets/fundamental-mode/time" nil "time")
                       ("mode" "`comment-start`-*- mode: ${1:mode} -*-`comment-end`" "mode"
                        (=
                         (line-number-at-pos)
                         1)
                        nil nil "/home/orion/.config/doom/snippets/snippets/fundamental-mode/mode" nil "mode")
                       ("elvar" "`comment-start`-*- ${1:var}: ${2:value} -*-`comment-end`" "var" nil nil nil "/home/orion/.config/doom/snippets/snippets/fundamental-mode/localvar" nil "elvar")
                       ("email" "`user-mail-address`\n" "(user's email)" nil nil nil "/home/orion/.config/doom/snippets/snippets/fundamental-mode/email" nil "email")
                       ("#!" "#!/usr/bin/env ${1:bash}\n\n$0\n" "bang"
                        (bolp)
                        nil nil "/home/orion/.config/doom/snippets/snippets/fundamental-mode/bang" nil "#!")))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("ref" "Ref: ${1:URL|12-CHAR HASH|#GITHUBREF}" "Ref: ..." nil nil nil "/home/orion/.config/doom/snippets/snippets/git-commit-mode/references" nil nil)
                       ("fix" "Ref: ${1:URL|12-CHAR HASH|#GITHUBREF}" "Fix: ..." nil nil nil "/home/orion/.config/doom/snippets/snippets/git-commit-mode/fixes" nil nil)
                       ("cab" "Co-authored-by: ${1:username} <${2:$1@users.noreply.github.com}>" "Co-authored-by: ..." nil nil nil "/home/orion/.config/doom/snippets/snippets/git-commit-mode/co-authored-by" nil nil)
                       ("cl" "Close: ${1:URL|12-CHAR HASH|#GITHUBREF}" "Close: ..." nil nil nil "/home/orion/.config/doom/snippets/snippets/git-commit-mode/close" nil nil)
                       ("am" "Amend: ${1:URL|12-CHAR HASH|#GITHUBREF}" "Amend: ..." nil nil nil "/home/orion/.config/doom/snippets/snippets/git-commit-mode/amend" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("wr" "w http.ResponseWriter, r *http.Request" "http request writer" nil nil nil "/home/orion/.config/doom/snippets/snippets/go-mode/wr" nil nil)
                       ("while" "for $1 {\n    `%`$0\n}" "for ... { ... }" nil nil nil "/home/orion/.config/doom/snippets/snippets/go-mode/while" nil nil)
                       ("var" "var ${1:name} ${2:type} = ${3:value}$0" "var" nil nil nil "/home/orion/.config/doom/snippets/snippets/go-mode/var" nil nil)
                       ("test" "func Test${1:Name}(${2:t *testing.T}) {\n    `%`$0\n}" "func Test...() { ... }" nil nil nil "/home/orion/.config/doom/snippets/snippets/go-mode/test" nil nil)
                       ("switch" "switch {\n    case ${1:cond}:\n         $0\n}" "switch" nil nil nil "/home/orion/.config/doom/snippets/snippets/go-mode/switch" nil "switch")
                       ("struct" "type $1 struct {\n    `%`$0\n}" "type ... struct { ... }" nil nil nil "/home/orion/.config/doom/snippets/snippets/go-mode/struct" nil nil)
                       ("select" "select {\n      case ${1:cond}:\n      $0\n}\n" "select" nil nil nil "/home/orion/.config/doom/snippets/snippets/go-mode/select" nil nil)
                       ("prln" "fmt.Println(\"${1:msg}\")$0\n" "println (short)" nil nil nil "/home/orion/.config/doom/snippets/snippets/go-mode/prln" nil nil)
                       ("println" "fmt.Println(\"${1:msg}\")$0\n" "println" nil nil nil "/home/orion/.config/doom/snippets/snippets/go-mode/println" nil nil)
                       ("printf" "fmt.Printf(\"$1\\n\"${2:, ${3:str}})" "printf" nil nil nil "/home/orion/.config/doom/snippets/snippets/go-mode/printf" nil nil)
                       ("pr" "fmt.Printf(\"$1\\n\"${2:, ${3:str}})" "printf" nil nil nil "/home/orion/.config/doom/snippets/snippets/go-mode/pr" nil nil)
                       ("pkg" "package ${1:`(car (last (split-string (file-name-directory buffer-file-name) \"/\") 2))`}" "package (short)" nil nil nil "/home/orion/.config/doom/snippets/snippets/go-mode/pkg" nil nil)
                       ("package" "package ${1:`(car (last (split-string (file-name-directory buffer-file-name) \"/\") 2))`}" "package" nil nil nil "/home/orion/.config/doom/snippets/snippets/go-mode/package" nil nil)
                       ("method" "func (${1:target}) ${2:name}(${3:args})${4: return type} {\n    $0\n}" "func (target) name(args) (results) { ... }" nil nil nil "/home/orion/.config/doom/snippets/snippets/go-mode/method" nil nil)
                       ("map" "map[${1:KeyType}]${2:ValueType}" "map" nil nil nil "/home/orion/.config/doom/snippets/snippets/go-mode/map" nil nil)
                       ("main" "func main() {\n   $0\n}" "func main() { ... }" nil nil nil "/home/orion/.config/doom/snippets/snippets/go-mode/main" nil nil)
                       ("interface" "type $1 interface {\n    `%`$0\n}\n" "type ... interface { ... }" nil nil nil "/home/orion/.config/doom/snippets/snippets/go-mode/interface" nil nil)
                       ("import" "import ${1:package}$0" "import" nil nil nil "/home/orion/.config/doom/snippets/snippets/go-mode/import" nil nil)
                       ("imp" "import ${1:package}$0" "import" nil nil nil "/home/orion/.config/doom/snippets/snippets/go-mode/imp" nil nil)
                       ("iferr" "if err != nil {\n    `%`$0\n}" "if err != nil { ... }" nil nil nil "/home/orion/.config/doom/snippets/snippets/go-mode/iferr" nil nil)
                       ("ife" "if ${1:condition} {\n	`%`$2\n} else {\n	$0\n}" "if ... { ... } else { ... }" nil nil nil "/home/orion/.config/doom/snippets/snippets/go-mode/ife" nil nil)
                       ("if" "if ${1:condition} {\n	`%`$0\n}" "if ... { ... }" nil nil nil "/home/orion/.config/doom/snippets/snippets/go-mode/if" nil nil)
                       ("gofunc" "go func (${1:args}) {\n    $0\n}(${2:values})\n" "go func (short)" nil nil nil "/home/orion/.config/doom/snippets/snippets/go-mode/gofunc" nil nil)
                       ("gof" "go func (${1:args}) {\n    $0\n}(${2:values})\n" "go func" nil nil nil "/home/orion/.config/doom/snippets/snippets/go-mode/gof" nil nil)
                       ("go" "go ${1:func}(${2:args})$0\n" "go" nil nil nil "/home/orion/.config/doom/snippets/snippets/go-mode/go" nil nil)
                       ("func" "func ${1:name}(${2:args})${3: return type} {\n    `%`$0\n}" "func ...(...) ... { ... }" nil nil nil "/home/orion/.config/doom/snippets/snippets/go-mode/func" nil nil)
                       ("forw" "for $1 {\n    `%`$0\n}" "for ... { ... }" nil nil nil "/home/orion/.config/doom/snippets/snippets/go-mode/forw" nil nil)
                       ("fori" "for ${1:i} := ${2:0}; $1 < ${3:10}; $1++ {\n    `%`$0\n}" "for i := 0; i < n; i++ { ... }" nil nil nil "/home/orion/.config/doom/snippets/snippets/go-mode/fori" nil nil)
                       ("foreach" "for ${1:key}, ${2:value} := range ${3:target} {\n    `%`$0\n}" "for key, value := range ... { ... }" nil nil nil "/home/orion/.config/doom/snippets/snippets/go-mode/foreach" nil nil)
                       ("fore" "for ${1:key}, ${2:value} := range ${3:target} {\n    `%`$0\n}" "for key, value := range ... { ... }" nil nil nil "/home/orion/.config/doom/snippets/snippets/go-mode/fore" nil nil)
                       ("for" "for $1 {\n    `%`$0\n}" "for ... { ... }" nil nil nil "/home/orion/.config/doom/snippets/snippets/go-mode/for" nil nil)
                       ("fm" "func (${1:target}) ${2:name}(${3:args})${4: return type} {\n    $0\n}" "func (target) name(args) (results) { ... }" nil nil nil "/home/orion/.config/doom/snippets/snippets/go-mode/fm" nil nil)
                       ("f" "func ${1:name}(${2:args})${3: return type} {\n    `%`$0\n}" "func ...(...) ... { ... }" nil nil nil "/home/orion/.config/doom/snippets/snippets/go-mode/f" nil nil)
                       ("ctxc" "ctx context.Context" "ctx context.Context" nil nil nil "/home/orion/.config/doom/snippets/snippets/go-mode/ctxc" nil nil)
                       ("const" "const ${1:name}${2: type} = ${3:value}$0" "const ... = ..." nil nil nil "/home/orion/.config/doom/snippets/snippets/go-mode/const" nil nil)
                       (":=" "${1:x} := ${2:`%`}" "... := ..." nil nil nil "/home/orion/.config/doom/snippets/snippets/go-mode/coloneq" nil ":=")
                       ("append" "${1:type} = append($1, ${2:elems})\n" "append" nil nil nil "/home/orion/.config/doom/snippets/snippets/go-mode/append" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("pr" "print $0" "print" nil nil nil "/home/orion/.config/doom/snippets/snippets/haskell-mode/print" nil "pr")
                       ("{" "{-# ${1:PRAGMA} #-}" "pragma" nil nil nil "/home/orion/.config/doom/snippets/snippets/haskell-mode/pragma" nil "{")
                       ("class" "class ${1:Class Name} where\n      $0" "new class" nil nil nil "/home/orion/.config/doom/snippets/snippets/haskell-mode/new class" nil "class")
                       ("mod" "module ${1:Module} where\n$0" "module" nil nil nil "/home/orion/.config/doom/snippets/snippets/haskell-mode/module" nil "mod")
                       ("main" "main = do $0" "main" nil nil nil "/home/orion/.config/doom/snippets/snippets/haskell-mode/main" nil "main")
                       ("ins" "instance ${1:${2:(Show a)} => }${3:Ord} ${4:DataType} where\n$0\n" "instance" nil nil nil "/home/orion/.config/doom/snippets/snippets/haskell-mode/instance" nil "ins")
                       ("import" "import${1: qualified} ${2:Module${3:(symbols)}}${4: as ${5:alias}}" "import" nil nil nil "/home/orion/.config/doom/snippets/snippets/haskell-mode/import" nil "import")
                       ("::" "${1:fn-name} :: ${2:type}\n$1" "Function" nil nil nil "/home/orion/.config/doom/snippets/snippets/haskell-mode/function" nil "::")
                       ("d" "{-\n  $0\n-}" "doc" nil nil nil "/home/orion/.config/doom/snippets/snippets/haskell-mode/doc" nil "d")
                       ("da" "data ${1:Type} = $2" "data" nil nil nil "/home/orion/.config/doom/snippets/snippets/haskell-mode/data" nil "da")
                       ("case" "case ${1:var} of\n     ${2:cond} -> ${3:value}\n     $0\n     otherwise -> ${4:other}" "case" nil nil nil "/home/orion/.config/doom/snippets/snippets/haskell-mode/case" nil "case")))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("th" "<th$1>$2</th>" "<th>...</th>" nil
                        ("table")
                        nil "/home/orion/.config/doom/snippets/snippets/html-mode/th" nil nil)
                       ("textarea" "<textarea name=\"$1\" id=\"$2\" rows=\"$3\" cols=\"$4\" tabindex=\"$5\"></textarea>" "<textarea ...></textarea>" nil nil nil "/home/orion/.config/doom/snippets/snippets/html-mode/textarea" nil nil)
                       ("scriptsrc" "<script type=\"text/javascript\" src=\"$1\"></script>" "<script type=\"text/javascript\" src=\"...\"></script>" nil nil nil "/home/orion/.config/doom/snippets/snippets/html-mode/scriptsrc" nil nil)
                       ("script" "<script type=\"text/javascript\">\n  $0\n</script>" "<script type=\"text/javascript\">...</script>" nil nil nil "/home/orion/.config/doom/snippets/snippets/html-mode/script" nil nil)
                       ("meta.http-equiv" "<meta name=\"${1:Content-Type}\" content=\"${2:text/html; charset=UTF-8}\" />" "<meta http-equiv=\"...\" content=\"...\" />" nil
                        ("meta")
                        nil "/home/orion/.config/doom/snippets/snippets/html-mode/meta.http-equiv" nil nil)
                       ("meta" "<meta name=\"${1:generator}\" content=\"${2:content}\" />" "<meta name=\"...\" content=\"...\" />" nil
                        ("meta")
                        nil "/home/orion/.config/doom/snippets/snippets/html-mode/meta" nil nil)
                       ("mailto" "<a href=\"mailto:${1:john@doe.com}\">`(doom-snippets-format \"%n%s%n\")`$0</a>" "<a href=\"mailto:...@...\">...</a>" nil nil nil "/home/orion/.config/doom/snippets/snippets/html-mode/mailto" nil nil)
                       ("linkie" "<!--[if IE${1: version}]>\n<link rel=\"${2:stylesheet}\" href=\"${3:url}\" type=\"${4:text/css}\" media=\"${5:screen}\" />\n<![endif]-->\n" "<!--[if IE]><link stylesheet=\"...\" /><![endif]-->" nil nil nil "/home/orion/.config/doom/snippets/snippets/html-mode/linkie" nil nil)
                       ("link" "<link rel=\"${1:stylesheet}\" href=\"${2:url}\" type=\"${3:text/css}\" media=\"${4:screen}\" />" "<link rel=\"stylesheet\" ... />" nil nil nil "/home/orion/.config/doom/snippets/snippets/html-mode/link" nil "link")
                       ("html.xmlns" "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"${1:en}\" lang=\"${2:en}\">\n  $0\n</html>\n" "<html xmlns=\"...\">...</html>" nil nil nil "/home/orion/.config/doom/snippets/snippets/html-mode/html.xmlns" nil nil)
                       ("html" "<html>\n  $0\n</html>\n" "<html>...</html>" nil nil nil "/home/orion/.config/doom/snippets/snippets/html-mode/html" nil nil)
                       ("form" "<form method=\"$1\" id=\"$2\" action=\"$3\">\n  $0\n</form>" "<form method=\"...\" id=\"...\" action=\"...\"></form>" nil nil nil "/home/orion/.config/doom/snippets/snippets/html-mode/form" nil nil)
                       ("dt" "<dt>$1</dt>" "<dt> ... </dt>" nil
                        ("list")
                        nil "/home/orion/.config/doom/snippets/snippets/html-mode/dt" nil nil)
                       ("doctype.xhtml1_transitional" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">" "DocType XHTML 1.0 Transitional" nil
                        ("meta")
                        nil "/home/orion/.config/doom/snippets/snippets/html-mode/doctype.xhtml1_transitional" nil nil)
                       ("doctype.xhtml1_strict" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">" "DocType XHTML 1.0 Strict" nil
                        ("meta")
                        nil "/home/orion/.config/doom/snippets/snippets/html-mode/doctype.xhtml1_strict" nil nil)
                       ("doctype.xhtml1_1" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">" "DocType XHTML 1.1" nil
                        ("meta")
                        nil "/home/orion/.config/doom/snippets/snippets/html-mode/doctype.xhtml1_1" nil nil)
                       ("doctype.xhml1" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\">" "DocType XHTML 1.0 frameset" nil
                        ("meta")
                        nil "/home/orion/.config/doom/snippets/snippets/html-mode/doctype.xhml1" nil nil)
                       ("doctype" "<!DOCTYPE html>\n" "Doctype HTML 5" nil
                        ("meta")
                        nil "/home/orion/.config/doom/snippets/snippets/html-mode/doctype" nil nil)
                       ("dl" "<dl>\n    $0\n</dl>\n" "<dl> ... </dl>" nil
                        ("list")
                        nil "/home/orion/.config/doom/snippets/snippets/html-mode/dl" nil nil)
                       ("dd" "<dd>$1</dd>" "<dd> ... </dd>" nil
                        ("list")
                        nil "/home/orion/.config/doom/snippets/snippets/html-mode/dd" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("while" "while (${1:true}) {\n      $0\n}" "while loop"
                        (not
                         (sp-point-in-string-or-comment))
                        nil nil "/home/orion/.config/doom/snippets/snippets/java-mode/while" nil "while")
                       ("v" "void $0" "void" nil nil nil "/home/orion/.config/doom/snippets/snippets/java-mode/void" nil "v")
                       ("var=" "${1:int} ${2:variable} = `%`$0;" "variable declaration & assignment"
                        (not
                         (sp-point-in-string-or-comment))
                        nil nil "/home/orion/.config/doom/snippets/snippets/java-mode/var=" nil "var=")
                       ("var" "${1:int} ${2:variable}" "variable declaration"
                        (not
                         (sp-point-in-string-or-comment))
                        nil nil "/home/orion/.config/doom/snippets/snippets/java-mode/var" nil "var")
                       ("try" "try {\n    `%`$0\n} catch (${1:Throwable} e) {\n      ${2:System.out.println(\"Error \" + e.getMessage());\n      e.printStackTrace();}\n}" "try"
                        (not
                         (sp-point-in-string-or-comment))
                        nil nil "/home/orion/.config/doom/snippets/snippets/java-mode/try" nil "try")
                       ("toStr" "public String toString() {\n       $0\n}" "toString" nil nil nil "/home/orion/.config/doom/snippets/snippets/java-mode/toString" nil "toStr")
                       ("." "this.$0" "this" nil nil nil "/home/orion/.config/doom/snippets/snippets/java-mode/this" nil ".")
                       ("tc" "import junit.framework.*;\nimport junit.textui.*;\n\npublic class Test${1:Class} extends TestCase {\n       protected void setUp() {\n                 $0\n       }\n}" "testClass" nil nil nil "/home/orion/.config/doom/snippets/snippets/java-mode/testClass" nil "tc")
                       ("test" "@Test\npublic void test_${1:Case}() {\n       $0\n}" "test" nil nil nil "/home/orion/.config/doom/snippets/snippets/java-mode/test" nil "test")
                       ("ret" "return `%`$0;" "return"
                        (not
                         (sp-point-in-string-or-comment))
                        nil nil "/home/orion/.config/doom/snippets/snippets/java-mode/return" nil "ret")
                       ("p" "public $0" "public" nil nil nil "/home/orion/.config/doom/snippets/snippets/java-mode/public" nil "p")
                       ("pr" "protected $0" "protected" nil nil nil "/home/orion/.config/doom/snippets/snippets/java-mode/protected" nil "pr")
                       ("pri" "private $0" "private" nil nil nil "/home/orion/.config/doom/snippets/snippets/java-mode/private" nil "pri")
                       ("println" "System.out.println(\"`%`$0\");" "println" nil nil nil "/home/orion/.config/doom/snippets/snippets/java-mode/println" nil "println")
                       ("printf" "System.out.printf(\"`%`$0%n\");" "printf" nil nil nil "/home/orion/.config/doom/snippets/snippets/java-mode/printf" nil "printf")
                       ("paintComponent" "@Override public void paintComponent(Graphics g) {\n          `%`$0\n}" "paintComponent (Swing)"
                        (not
                         (sp-point-in-string-or-comment))
                        nil nil "/home/orion/.config/doom/snippets/snippets/java-mode/paintComponent" nil "paintComponent")
                       ("new" "${1:Type} ${2:obj} = new ${3:$1}($4);$0" "new" nil nil nil "/home/orion/.config/doom/snippets/snippets/java-mode/new" nil "new")
                       ("method@" "@Override ${1:public} ${2:void} ${3:methodName}($4) {\n          $0\n}" "@Override method"
                        (not
                         (sp-point-in-string-or-comment))
                        nil nil "/home/orion/.config/doom/snippets/snippets/java-mode/method@" nil "method@")
                       ("method" "${1:void} ${2:name}($3) {\n    $0\n}" "method"
                        (not
                         (sp-point-in-string-or-comment))
                        nil nil "/home/orion/.config/doom/snippets/snippets/java-mode/method" nil "method")
                       ("main" "public static void main(String[] args) {\n       $0\n}" "main" nil nil nil "/home/orion/.config/doom/snippets/snippets/java-mode/main" nil "main")
                       ("doc" "/**\n * $0\n *\n */" "javadoc" nil nil nil "/home/orion/.config/doom/snippets/snippets/java-mode/javadoc" nil "doc")
                       ("iterator" "public Iterator<${1:type}> iterator() {\n       $0\n}\n" "iterator" nil nil nil "/home/orion/.config/doom/snippets/snippets/java-mode/iterator" nil "iterator")
                       ("interface" "interface ${1:`(f-base buffer-file-name)`} {\n          $0\n}" "interface" nil nil nil "/home/orion/.config/doom/snippets/snippets/java-mode/interface" nil "interface")
                       ("import" "import ${1:System.};\n$0" "import" nil nil nil "/home/orion/.config/doom/snippets/snippets/java-mode/import" nil nil)
                       ("ife" "if (${1:true}) {\n    `%`$2\n} else {\n    $0\n}" "ife" nil nil nil "/home/orion/.config/doom/snippets/snippets/java-mode/ife" nil "ife")
                       ("if" "if (${1:true}) {\n   $0\n}" "if"
                        (not
                         (sp-point-in-string-or-comment))
                        nil nil "/home/orion/.config/doom/snippets/snippets/java-mode/if" nil "if")
                       ("fore" "for (${1:Object} ${2:var} : ${3:iterator}) {\n    $0\n}\n" "foreach" nil nil nil "/home/orion/.config/doom/snippets/snippets/java-mode/foreach" nil "fore")
                       ("for" "for (${1:int i = 0}; ${2:i < N}; ${3:i++}) {\n    `%`$0\n}" "for" nil nil nil "/home/orion/.config/doom/snippets/snippets/java-mode/for" nil "for")
                       ("file" "public class ${1:`(file-name-base\n                    (or (buffer-file-name)\n                        (buffer-name)))`} {\n  $0\n}\n" "file_class" nil nil nil "/home/orion/.config/doom/snippets/snippets/java-mode/file_class" nil "file")
                       ("eq" "public boolean equals(${1:Class} other) {\n       $0\n}" "equals" nil nil nil "/home/orion/.config/doom/snippets/snippets/java-mode/equals" nil "eq")
                       ("/*" "/**\n * $0\n */" "doc"
                        (not
                         (use-region-p))
                        nil nil "/home/orion/.config/doom/snippets/snippets/java-mode/doc" nil "/*")
                       ("__init__" "public ${1:`(f-base buffer-file-name)`}($2) {\n       $0\n}" "constructor" nil nil nil "/home/orion/.config/doom/snippets/snippets/java-mode/constructor" nil "__init__")
                       ("class" "${1:public }class ${2:`(f-base buffer-file-name)`} {\n           $0\n}" "class" nil nil nil "/home/orion/.config/doom/snippets/snippets/java-mode/class" nil "class")
                       ("apr_assert" "if (Globals.useAssertions) {\n   ${1:assert ..};\n}\n" "apr_assert" nil nil nil "/home/orion/.config/doom/snippets/snippets/java-mode/apr_assert" nil "apr_assert")
                       ("@return" "@return ${1:description}" "return"
                        (sp-point-in-comment)
                        nil nil "/home/orion/.config/doom/snippets/snippets/java-mode/@return" nil "@return")
                       ("@param" "@param ${1:paramater} $0" "param"
                        (sp-point-in-comment)
                        nil nil "/home/orion/.config/doom/snippets/snippets/java-mode/@param" nil "@param")))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("while" "while (${1:true}) { ${0:`(doom-snippets-format \"%n%s%n\")`} }" "while" nil nil nil "/home/orion/.config/doom/snippets/snippets/js-mode/while" nil nil)
                       ("var" "var ${1:name} = ${0:`%`};" "var ... = ...;" nil nil nil "/home/orion/.config/doom/snippets/snippets/js-mode/var" nil "var")
                       ("try" "try {\n    `%`$0\n} catch (${1:err}) {\n    ${2:// Do something}\n}" "try-catch block" nil nil nil "/home/orion/.config/doom/snippets/snippets/js-mode/try" nil nil)
                       ("r" "return $0;" "return ..." nil nil nil "/home/orion/.config/doom/snippets/snippets/js-mode/return" nil "r")
                       ("require" "require(`%`$0)`(if (eolp) \";\")`" "require(\"...\")" nil nil nil "/home/orion/.config/doom/snippets/snippets/js-mode/require" nil "require")
                       ("req"
                        (progn
                          (doom-snippets-expand :uuid "require"))
                        "require(\"...\")" nil nil nil "/home/orion/.config/doom/snippets/snippets/js-mode/req" nil "req")
                       ("pu" "`(unless (eq (char-before) ?.) \".\")`push(`(doom-snippets-text nil t)`$0)`(if (eolp) \";\")`" "arr.push(elt)" nil nil nil "/home/orion/.config/doom/snippets/snippets/js-mode/push" nil "pu")
                       (":" "${1:key}: ${0:value}" "...: ..." nil nil nil "/home/orion/.config/doom/snippets/snippets/js-mode/property" nil ":")
                       ("m" "${1:method}($2) {\n    `%`$0\n}" "method() { ... }" nil nil nil "/home/orion/.config/doom/snippets/snippets/js-mode/method" nil "m")
                       ("mapfu" "`(unless (eq (char-before) ?.) \".\")`map(function(${1:item}, ${2:i}, ${3:arr}) {\n    `(doom-snippets-format \"%n%s%n\")`$0\n})`(if (eolp) \";\")`" "arr.map(function(item, i, arr) {...})" nil nil nil "/home/orion/.config/doom/snippets/snippets/js-mode/map-function" nil "mapfu")
                       ("map=>" "`(unless (eq (char-before) ?.) \".\")`map((${1:item}, ${2:i}, ${3:arr}) => `(if (> (doom-snippets-count-lines %) 1) (concat \"{ \" (doom-snippets-format \"%n%s%n\") \" }\") %)`$0)`(if (eolp) \";\")`" "arr.map((item, i, arr) => {...})" nil nil nil "/home/orion/.config/doom/snippets/snippets/js-mode/map-arrow" nil "map=>")
                       ("map" "`(unless (eq (char-before) ?.) \".\")`map($0)`(if (eolp) \";\")`" "arr.map(...)" nil nil nil "/home/orion/.config/doom/snippets/snippets/js-mode/map" nil "map")
                       ("log" "console.log(`(doom-snippets-text nil t)`$0)`(if (eolp) \";\")`" "console.log(\"...\");" nil nil nil "/home/orion/.config/doom/snippets/snippets/js-mode/log" nil nil)
                       ("let" "let ${1:name} = ${0:`%`};" "let ... = ...;" nil nil nil "/home/orion/.config/doom/snippets/snippets/js-mode/let" nil nil)
                       ("/**" "/**\n * ${0:`(if % (s-replace \"\\n\" \"\\n * \" %))`}\n */" "/** ... */" nil nil nil "/home/orion/.config/doom/snippets/snippets/js-mode/jsdoc" nil "/**")
                       ("iof" "`(unless (eq (char-before) ?.) \".\")`indexOf(${1:`(or (doom-snippets-text nil t) \"elt\")`}${2: ${3:fromIndex}})`(if (eolp) \";\")`" "arr.indexOf(elt, fromIndex)" nil nil nil "/home/orion/.config/doom/snippets/snippets/js-mode/indexOf" nil "iof")
                       ("import" "import ${1:Object} from '${2:./$3}';" "import ... from ...;" nil nil nil "/home/orion/.config/doom/snippets/snippets/js-mode/import" nil "import")
                       ("imp"
                        (progn
                          (doom-snippets-expand :uuid "import"))
                        "import ... from ...;" nil nil nil "/home/orion/.config/doom/snippets/snippets/js-mode/imp" nil "imp")
                       ("if" "if (${1:true}) {\n    `%`$0\n}" "if" nil nil nil "/home/orion/.config/doom/snippets/snippets/js-mode/if" nil nil)
                       ("gebi" "`(unless (eq (char-before) ?.) \".\")`getElementById(${1:id})`(if (eolp) \";\")`" "getElementById" nil nil nil "/home/orion/.config/doom/snippets/snippets/js-mode/getElementById" nil "gebi")
                       ("function" "function ($1) { `(doom-snippets-format \"%n%s%n\")`$0 }`(if (eolp) \";\")`" "anonymous function"
                        (or
                         (not
                          (doom-snippets-bolp))
                         (region-active-p))
                        nil nil "/home/orion/.config/doom/snippets/snippets/js-mode/function_inline" nil "function_inline")
                       ("function" "function ${1:name}($2) {\n    `(doom-snippets-format \"%n%s%n\")`$0\n}" "named function"
                        (or
                         (doom-snippets-bolp)
                         (region-active-p))
                        nil nil "/home/orion/.config/doom/snippets/snippets/js-mode/function" nil "function_block")
                       ("fu"
                        (progn
                          (doom-snippets-expand :uuid
                                                (if
                                                    (doom-snippets-bolp)
                                                    "function_block" "function_inline")))
                        "anonymous/named function" nil nil nil "/home/orion/.config/doom/snippets/snippets/js-mode/fu" nil nil)
                       ("forin" "for (${1:key} in ${2:list}) {\n    `%`$0\n}" "for (key in list) { ... }" nil nil nil "/home/orion/.config/doom/snippets/snippets/js-mode/forin" nil "forin")
                       ("fori" "for (var ${1:i} = ${2:0}; $1 < ${3:${4:arr}.length}; ++$1) {\n    `%`$0\n}" "for (var i = 0; i < arr.length; ++i) { ... }" nil nil nil "/home/orion/.config/doom/snippets/snippets/js-mode/fori" nil "fori")
                       ("forefu" "`(unless (eq (char-before) ?.) \".\")`forEach(function(${1:item}) {\n    `(doom-snippets-format \"%n%s%n\")`$0\n})`(if (eolp) \";\")`" "arr.forEach(function(item) {...})" nil nil nil "/home/orion/.config/doom/snippets/snippets/js-mode/forEach-function" nil "forefu")
                       ("fore=>" "`(unless (eq (char-before) ?.) \".\")`forEach(${1:item} => `(if (> (doom-snippets-count-lines %) 1) (concat \"{ \" (doom-snippets-format \"%n%s%n\") \" }\") %)`$0)`(if (eolp) \";\")`" "arr.forEach((item) => {...})" nil nil nil "/home/orion/.config/doom/snippets/snippets/js-mode/forEach-arrow" nil "fore=>")
                       ("fore" "`(unless (eq (char-before) ?.) \".\")`forEach(`%`$0)`(if (eolp) \";\")`" "arr.forEach(...)" nil nil nil "/home/orion/.config/doom/snippets/snippets/js-mode/forEach" nil "fore")
                       ("for" "for ($1;$2;$3) {\n    `%`$0\n}" "for (;;)" nil nil nil "/home/orion/.config/doom/snippets/snippets/js-mode/for" nil "for")
                       ("fireEvent" "fireEvent('$0')" "fireEvent" nil nil nil "/home/orion/.config/doom/snippets/snippets/js-mode/fireEvent" nil nil)
                       ("filfu" "`(unless (eq (char-before) ?.) \".\")`filter(function(${1:item}) {\n    `(doom-snippets-format \"%n%s%n\")`$0\n})`(if (eolp) \";\")`" "arr.filter(function(item) {...})" nil nil nil "/home/orion/.config/doom/snippets/snippets/js-mode/filter-function" nil "filfu")
                       ("fil=>" "`(unless (eq (char-before) ?.) \".\")`filter(${1:item} => `(if (> (doom-snippets-count-lines %) 1) (concat \"{ \" (doom-snippets-format \"%n%s%n\") \" }\") %)`$0)`(if (eolp) \";\")`" "arr.filter(item => {...})" nil nil nil "/home/orion/.config/doom/snippets/snippets/js-mode/filter-arrow" nil "fil=>")
                       ("fil" "`(unless (eq (char-before) ?.) \".\")`filter($0)`(if (eolp) \";\")`" "arr.filter(...)" nil nil nil "/home/orion/.config/doom/snippets/snippets/js-mode/filter" nil "fil")
                       ("exp" "module.exports = `%`$0`(if (eolp) \";\")`" "module.exports = ..." nil nil nil "/home/orion/.config/doom/snippets/snippets/js-mode/exports" nil "exports")
                       ("euc" "encodeURIComponent(${1:`%`})`(if (eolp) \";\")`" "encodeURIComponent" nil nil nil "/home/orion/.config/doom/snippets/snippets/js-mode/encodeURIComponent" nil "euc")
                       ("eu" "encodeURI(${1:`%`})`(if (eolp) \";\")`" "encodeURI" nil nil nil "/home/orion/.config/doom/snippets/snippets/js-mode/encodeURI" nil "eu")
                       ("else" "else {\n    `%`$0\n}" "else" nil nil nil "/home/orion/.config/doom/snippets/snippets/js-mode/else" nil nil)
                       ("doc" "document." "document" nil nil nil "/home/orion/.config/doom/snippets/snippets/js-mode/document" nil "doc")
                       ("duc" "decodeURIComponent(${1:`%`})`(if (eolp) \";\")`" "decodeURIComponent" nil nil nil "/home/orion/.config/doom/snippets/snippets/js-mode/decodeURIComponent" nil "duc")
                       ("du" "decodeURI(${1:`%`})`(if (eolp) \";\")`" "decodeURI" nil nil nil "/home/orion/.config/doom/snippets/snippets/js-mode/decodeURI" nil "du")
                       ("constructor" "constructor($1) {\n    `%`$0\n}" "constructor() { ... }" nil nil nil "/home/orion/.config/doom/snippets/snippets/js-mode/constructor" nil nil)
                       ("con" "const ${1:name} = ${0:`%`};" "const ... = ...;" nil nil nil "/home/orion/.config/doom/snippets/snippets/js-mode/const" nil "con")
                       ("class" "class ${1:Name} {\n    $0\n}" "class" nil nil nil "/home/orion/.config/doom/snippets/snippets/js-mode/class" nil nil)
                       ("cl"
                        (progn
                          (doom-snippets-expand :uuid "class"))
                        "class" nil nil nil "/home/orion/.config/doom/snippets/snippets/js-mode/cl" nil "cl")
                       ("=>" "$1 => `(if (> (doom-snippets-count-lines %) 1) (concat \"{ \" (doom-snippets-format \"%n%s%n\") \" }\") %)`$0" "arrow function" nil nil nil "/home/orion/.config/doom/snippets/snippets/js-mode/arrow" nil "=>")
                       ("al" "alert(${0:`%`});" "alert" nil nil nil "/home/orion/.config/doom/snippets/snippets/js-mode/alert" nil "al")
                       ("ael" "`(unless (eq (char-before) ?.) \".\")`addEventListener('${1:DOMContentLoaded}', () => {\n  `%`$0\n})`(if (eolp) \";\")`" "addEventListener" nil nil nil "/home/orion/.config/doom/snippets/snippets/js-mode/addEventListener" nil "ael")
                       ("M" "Math.$0" "Math." nil nil nil "/home/orion/.config/doom/snippets/snippets/js-mode/Math" nil "M")))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("eslint" "\"eslintConfig\": {\n    \"env\": {\n        \"es6\": true,\n        \"browser\": true,\n        \"commonjs\": true,\n        \"node\": true\n    },\n    \"parserOptions\": {\n        \"ecmaFeatures\": {\n            \"jsx\": true\n        }\n    }\n}" "eslintConfig"
                        (equal
                         (file-name-nondirectory buffer-file-name)
                         "package.json")
                        nil nil "/home/orion/.config/doom/snippets/snippets/json-mode/eslintConfig" nil "eslint")))


;;; contents of the .yas-setup.el support file:
;;;
;;; julia-mode/.yas-setup.el -*- lexical-binding: t; -*-

(defun yas-julia-doc-args ()
  "Format arguments of a function slightly nicer for the doc string"
  (replace-regexp-in-string "\\([:blank:]*[,;]?*[^,;=]+=[[:ascii:][:nonascii:]]+\\)" "[\\1]" yas-text))
;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("while" "while ${1:cond}\n    `%`${2:body}\nend\n$0" "while ... ... end" nil nil nil "/home/orion/.config/doom/snippets/snippets/julia-mode/while" nil nil)
                       ("try" "try\n    `%`${1:expr}\ncatch ${2:error}\n    ${3:e_expr}\nfinally\n    ${4:f_expr}\nend\n$0\n" "try ... catch ... finally ... end" nil nil nil "/home/orion/.config/doom/snippets/snippets/julia-mode/tryf" nil nil)
                       ("try" "try\n    `%`${1:expr}\ncatch ${2:error}\n    ${3:e_expr}\nend\n$0\n" "try ... catch ... end" nil nil nil "/home/orion/.config/doom/snippets/snippets/julia-mode/try" nil nil)
                       ("struct" "struct ${1:name}\n    `%`$0\nend" "struct ... end" nil nil nil "/home/orion/.config/doom/snippets/snippets/julia-mode/struct" nil nil)
                       ("quote" "quote\n    `%`$0\nend" "quote ... end" nil nil nil "/home/orion/.config/doom/snippets/snippets/julia-mode/quote" nil nil)
                       ("ptype" "primitive type ${1:${2:type} <: ${3:supertype}} ${4:bits} end\n$0\n" "primitive type ... end" nil nil nil "/home/orion/.config/doom/snippets/snippets/julia-mode/ptype" nil nil)
                       ("mutstr" "mutable struct ${1:name}\n    `%`$0\nend" "mutable struct ... end" nil nil nil "/home/orion/.config/doom/snippets/snippets/julia-mode/mutstr" nil nil)
                       ("module" "module ${1:name}\n`%`$0\nend\n" "module ... ... end" nil nil nil "/home/orion/.config/doom/snippets/snippets/julia-mode/module" nil nil)
                       ("macro" "macro ${1:macro}(${2:args})\n    `%`$0\nend\n" "macro(...) ... end" nil nil nil "/home/orion/.config/doom/snippets/snippets/julia-mode/macro" nil nil)
                       ("let" "let ${1:binding}\n    `%`$0\nend" "let ... ... end" nil nil nil "/home/orion/.config/doom/snippets/snippets/julia-mode/let" nil nil)
                       ("ife" "if ${1:cond}\n    `%`${2:true}\nelse\n    ${3:false}\nend\n$0" "if ... ... else ... end" nil nil nil "/home/orion/.config/doom/snippets/snippets/julia-mode/ife" nil nil)
                       ("if" "if ${1:cond}\n    `%`$0\nend\n" "if ... ... end" nil nil nil "/home/orion/.config/doom/snippets/snippets/julia-mode/if" nil nil)
                       ("fun" "function ${1:name}(${2:args})\n    `%`$0\nend" "function(...) ... end" nil nil nil "/home/orion/.config/doom/snippets/snippets/julia-mode/fun" nil nil)
                       ("for" "for ${1:i} in ${2:1:n}\n    `%`$0\nend\n" "for ... ... end" nil nil nil "/home/orion/.config/doom/snippets/snippets/julia-mode/for" nil nil)
                       ("do" "do ${1:x}\n    `%`$0\nend" "do ... ... end" nil nil nil "/home/orion/.config/doom/snippets/snippets/julia-mode/do" nil nil)
                       ("dfun" "@doc raw\"\"\"\n    $1(${2:$(yas-julia-doc-args)})\n\n${3:Documentation of function.}\n\n# Examples\n\\`\\`\\`jldoctest\njulia> $1($4)\ninsert result of $1($4)\n\\`\\`\\`\n\"\"\"\nfunction ${1:name}(${2:args})\n    `%`$0\nend\n" "@doc ... function ... end" nil nil nil "/home/orion/.config/doom/snippets/snippets/julia-mode/dfun" nil "dfun")
                       ("begin" "begin\n    `%`$0\nend\n" "begin ... end" nil nil nil "/home/orion/.config/doom/snippets/snippets/julia-mode/begin" nil nil)
                       ("beg"
                        (progn
                          (doom-snippets-expand :name "begin"))
                        "begin" nil nil nil "/home/orion/.config/doom/snippets/snippets/julia-mode/beg" nil "beg")
                       ("atype" "abstract type ${1:${2:type} <: ${3:supertype}} end\n$0" "abstract type ... end" nil nil nil "/home/orion/.config/doom/snippets/snippets/julia-mode/atype" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("while" "while ($true) {\n    $0\n}\n" "while" nil nil nil "/home/orion/.config/doom/snippets/snippets/kotlin-mode/while" nil nil)
                       ("var=" "var ${1:variable}${2:: ${3:Int}} = `%`$0\n" "var=" nil nil nil "/home/orion/.config/doom/snippets/snippets/kotlin-mode/var=" nil nil)
                       ("var" "var ${1:variable}: ${2:Int}\n" "var" nil nil nil "/home/orion/.config/doom/snippets/snippets/kotlin-mode/var" nil nil)
                       ("val=" "val ${1:name}${2:: ${3:Int}} = `%`$0" "val=" nil nil nil "/home/orion/.config/doom/snippets/snippets/kotlin-mode/val=" nil nil)
                       ("val" "val ${1:name}: ${2:Int}" "val" nil nil nil "/home/orion/.config/doom/snippets/snippets/kotlin-mode/val" nil nil)
                       ("todo" "TODO('Not yet implemented')\n" "todo" nil nil nil "/home/orion/.config/doom/snippets/snippets/kotlin-mode/todo" nil nil)
                       ("main" "fun main(args: Array<String>) {\n    $0\n}\n" "main" nil nil nil "/home/orion/.config/doom/snippets/snippets/kotlin-mode/main" nil nil)
                       ("interface" "interface ${1:name} {\n    $0\n}\n" "interface" nil nil nil "/home/orion/.config/doom/snippets/snippets/kotlin-mode/interface" nil nil)
                       ("ife" "if (${1:true}) {\n    `%`$2\n} else {\n    $0\n}\n" "ife" nil nil nil "/home/orion/.config/doom/snippets/snippets/kotlin-mode/ife" nil nil)
                       ("if" "if (${1:true}) {\n    $0\n}\n" "if" nil nil nil "/home/orion/.config/doom/snippets/snippets/kotlin-mode/if" nil nil)
                       ("fun" "fun ${1:name}($2)${3:: ${4:Unit}} {\n    ${0:TODO('Not yet implemented')}\n}\n" "fun" nil nil nil "/home/orion/.config/doom/snippets/snippets/kotlin-mode/fun" nil nil)
                       ("forin" "for (${1:key} in ${2:iterable}) {\n    `%`$0\n}\n" "for (key in iterable) { ... }" nil nil nil "/home/orion/.config/doom/snippets/snippets/kotlin-mode/forin" nil nil)
                       ("file_class" "class `(f-base buffer-file-name)`${1:($2)}${3: {\n    $0\n}}\n" "file_class" nil nil nil "/home/orion/.config/doom/snippets/snippets/kotlin-mode/file_class" nil nil)
                       ("class" "class ${1:name}${2:($3)}${4: : $5}${6: {\n    $0\n}}\n" "class" nil nil nil "/home/orion/.config/doom/snippets/snippets/kotlin-mode/class" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("pkg" "\\usepackage{$0}" "usepackage" nil nil nil "/home/orion/.config/doom/snippets/snippets/latex-mode/usepackage" nil "pkg")
                       ("thm" "\\begin{theorem}\n`%`$0\n\\end{theorem}" "theorem" nil
                        ("theorems")
                        nil "/home/orion/.config/doom/snippets/snippets/latex-mode/theorem" nil nil)
                       ("b" "\\textbf{`%`$1}$0" "textbf"
                        (not
                         (save-restriction
                           (widen)
                           (texmathp)))
                        nil nil "/home/orion/.config/doom/snippets/snippets/latex-mode/textbf" nil "b")
                       ("sum" "\\sum${1:$(when (> (length yas-text) 0) \"_\")\n}${1:$(when (> (length yas-text) 1) \"{\")\n}${1:i=0}${1:$(when (> (length yas-text) 1) \"}\")\n}${2:$(when (> (length yas-text) 0) \"^\")\n}${2:$(when (> (length yas-text) 1) \"{\")\n}${2:n}${2:$(when (> (length yas-text) 1) \"}\")} $0" "sum_^" nil nil nil "/home/orion/.config/doom/snippets/snippets/latex-mode/sum_^" nil nil)
                       ("sub" "\\subsection{${1:name}}%\n\\label{subsec:${2:label}}\n\n$0" "subsec" nil nil nil "/home/orion/.config/doom/snippets/snippets/latex-mode/subsec" nil "sub")
                       ("subfig" "\\begin{figure}[ht]\n  \\centering\n  \\subfigure[$1]\n  {\\label{fig:${2:label}} \n    \\includegraphics[width=.${3:5}\\textwidth]{${4:path}}}\n\n  \\caption{${5:caption}}%\n\\label{fig:${6:label}}\n\\end{figure}\n" "subfigure" nil nil nil "/home/orion/.config/doom/snippets/snippets/latex-mode/subfigure" nil "subfig")
                       ("sf" "\\subfigure[${1:caption}]{\n  \\label{fig:${2:label}} \n  \\includegraphics[width=.${3:3}\\textwidth]{${4:path}}}\n$0" "subf" nil nil nil "/home/orion/.config/doom/snippets/snippets/latex-mode/subf" nil "sf")
                       ("sq" "\\sqrt{`%`$1}$0" "sqrt" nil nil nil "/home/orion/.config/doom/snippets/snippets/latex-mode/sqrt" nil nil)
                       ("sec" "\\section{${1:name}}%\n\\label{sec:${2:label}}\n\n$0" "section" nil nil nil "/home/orion/.config/doom/snippets/snippets/latex-mode/section" nil "sec")
                       ("root" "\\sqrt[$1]{`%`$2}" "sqrt[]{}" nil nil nil "/home/orion/.config/doom/snippets/snippets/latex-mode/root" nil nil)
                       ("rmk" "\\begin{remark}\n`%`$0\n\\end{remark}" "remark" nil
                        ("theorems")
                        nil "/home/orion/.config/doom/snippets/snippets/latex-mode/remark" nil nil)
                       ("q" "\\question{${0:`%`}}" "question"
                        (not
                         (save-restriction
                           (widen)
                           (texmathp)))
                        nil nil "/home/orion/.config/doom/snippets/snippets/latex-mode/question" nil "q")
                       ("py" "\\lstset{language=python}\n\\begin[language=python]{lstlisting}\n${0:`%`}\n\\end{lstlisting}" "python" nil nil nil "/home/orion/.config/doom/snippets/snippets/latex-mode/python" nil "py")
                       ("prf" "\\begin{proof}\n`%`$0\n\\end{proof}" "proof" nil
                        ("theorems")
                        nil "/home/orion/.config/doom/snippets/snippets/latex-mode/proof" nil nil)
                       ("prod" "\\prod${1:$(when (> (length yas-text) 0) \"_\")\n}${1:$(when (> (length yas-text) 1) \"{\")\n}${1:i=0}${1:$(when (> (length yas-text) 1) \"}\")\n}${2:$(when (> (length yas-text) 0) \"^\")\n}${2:$(when (> (length yas-text) 1) \"{\")\n}${2:n}${2:$(when (> (length yas-text) 1) \"}\")} $0" "prod_^" nil nil nil "/home/orion/.config/doom/snippets/snippets/latex-mode/prod_^" nil nil)
                       ("no" "\\note{${0:`%`}}" "note" nil nil nil "/home/orion/.config/doom/snippets/snippets/latex-mode/note" nil "no")
                       ("gl" "\\newglossaryentry{${1:AC}}{name=${2:Andrea Crotti}${3:, description=${4:description}}}" "newglossaryentry" nil nil nil "/home/orion/.config/doom/snippets/snippets/latex-mode/newglossaryentry" nil "gl")
                       ("cmd" "\\newcommand{\\\\${1:name}}${2:[${3:0}]}{$0}" "newcommand" nil nil nil "/home/orion/.config/doom/snippets/snippets/latex-mode/newcommand" nil "cmd")
                       ("movie" "\\begin{center}\n\\includemovie[\n  label=test,\n  controls=false,\n  text={\\includegraphics[width=4in]{${1:image.pdf}}}\n]{4in}{4in}{${2:video file}}\n\n\\movieref[rate=3]{test}{Play Fast}\n\\movieref[rate=1]{test}{Play Normal Speed} \n\\movieref[rate=0.2]{test}{Play Slow}\n\\movieref[resume]{test}{Pause/Resume}\n" "movie" nil nil nil "/home/orion/.config/doom/snippets/snippets/latex-mode/movie" nil "movie")
                       ("mc" "\\mathclap{`%`$0}" "mathclap" nil nil nil "/home/orion/.config/doom/snippets/snippets/latex-mode/mathclap" nil nil)
                       ("lst" "\\begin{lstlisting}[float,label=lst:${1:label},caption=nextHopInfo: ${2:caption}]\n${0:`%`}\n\\end{lstlisting}" "listing" nil nil nil "/home/orion/.config/doom/snippets/snippets/latex-mode/listing" nil "lst")
                       ("limsup" "\\limsup_{${1:n} \\to ${2:\\infty}} $0" "limsup" nil nil nil "/home/orion/.config/doom/snippets/snippets/latex-mode/limsup" nil nil)
                       ("liminf" "\\liminf_{${1:n} \\to ${2:\\infty}} $0" "liminf" nil nil nil "/home/orion/.config/doom/snippets/snippets/latex-mode/liminf" nil nil)
                       ("lim" "\\lim_{${1:n} \\to ${2:\\infty}} $0" "lim" nil nil nil "/home/orion/.config/doom/snippets/snippets/latex-mode/lim" nil nil)
                       ("lmm" "\\begin{lemma}\n`%`$0\n\\end{lemma}" "lemma" nil
                        ("theorems")
                        nil "/home/orion/.config/doom/snippets/snippets/latex-mode/lemma" nil nil)
                       ("lab" "\\label{$0}" "label" nil nil nil "/home/orion/.config/doom/snippets/snippets/latex-mode/label" nil "lab")
                       ("it" "\\begin{itemize}\n${0:`%`}\n\\end{itemize}" "itemize" nil nil nil "/home/orion/.config/doom/snippets/snippets/latex-mode/itemize" nil "it")
                       ("-" "\\item ${0:`%`}" "item"
                        (not
                         (save-restriction
                           (widen)
                           (texmathp)))
                        nil nil "/home/orion/.config/doom/snippets/snippets/latex-mode/item" nil "-")
                       ("int" "\\int${1:$(when (> (length yas-text) 0) \"_\")\n}${1:$(when (> (length yas-text) 1) \"{\")\n}${1:left}${1:$(when (> (length yas-text) 1) \"}\")\n}${2:$(when (> (length yas-text) 0) \"^\")\n}${2:$(when (> (length yas-text) 1) \"{\")\n}${2:right}${2:$(when (> (length yas-text) 1) \"}\")} $0" "int_^" nil nil nil "/home/orion/.config/doom/snippets/snippets/latex-mode/int_^" nil nil)
                       ("ig" "\\includegraphics${1:[$2]}{$0}" "includegraphics" nil nil nil "/home/orion/.config/doom/snippets/snippets/latex-mode/includegraphics" nil "ig")
                       ("if" "\\IF {$${1:cond}$}\n    `%`$0\n\\ELSE\n\\ENDIF \n" "if" nil nil nil "/home/orion/.config/doom/snippets/snippets/latex-mode/if" nil "if")
                       ("gp" "\\glspl{${1:label}}" "glspl" nil nil nil "/home/orion/.config/doom/snippets/snippets/latex-mode/glspl" nil "gp")
                       ("g" "\\gls{${1:label}}" "gls"
                        (not
                         (save-restriction
                           (widen)
                           (texmathp)))
                        nil nil "/home/orion/.config/doom/snippets/snippets/latex-mode/gls" nil "g")
                       ("fr" "\\begin{frame}${1:[$2]}\n        ${3:\\frametitle{$4}}\n        ${0:`%`}\n\\end{frame}" "frame" nil nil nil "/home/orion/.config/doom/snippets/snippets/latex-mode/frame" nil "fr")
                       ("frac" "\\frac{${1:`(or % \"numerator\")`}}{${2:denominator}}$0" "frac" nil nil nil "/home/orion/.config/doom/snippets/snippets/latex-mode/frac" nil "frac")
                       ("fig" "\\begin{figure}[ht]\n  \\centering\n  \\includegraphics[${1:options}]{figures/${2:path.pdf}}\n  \\caption{\\label{fig:${3:label}} $0}\n\\end{figure}\n" "figure" nil nil nil "/home/orion/.config/doom/snippets/snippets/latex-mode/figure" nil "fig")
                       ("exc" "\\begin{exercise}\n`%`$0\n\\end{exercise}" "exercise" nil
                        ("theorems")
                        nil "/home/orion/.config/doom/snippets/snippets/latex-mode/exercise" nil nil)
                       ("en" "\\begin{enumerate}\n${0:`%`}\n\\end{enumerate}\n" "enumerate" nil nil nil "/home/orion/.config/doom/snippets/snippets/latex-mode/enumerate" nil "en")
                       ("e" "\\emph{${1:`%`}}$0" "emph"
                        (not
                         (save-restriction
                           (widen)
                           (texmathp)))
                        nil nil "/home/orion/.config/doom/snippets/snippets/latex-mode/emph" nil "e")
                       ("def" "\\begin{definition}\n`%`$0\n\\end{definition}" "definition" nil
                        ("theorems")
                        nil "/home/orion/.config/doom/snippets/snippets/latex-mode/definition" nil nil)
                       ("clr" "\\begin{corollary}\n`%`$0\n\\end{corollary}" "corollary" nil
                        ("theorems")
                        nil "/home/orion/.config/doom/snippets/snippets/latex-mode/corollary" nil nil)
                       ("cols" "\\begin{columns}\n  \\begin{column}{.${1:5}\\textwidth}\n  $0\n  \\end{column}\n\n  \\begin{column}{.${2:5}\\textwidth}\n\n  \\end{column}\n\\end{columns}" "columns" nil nil nil "/home/orion/.config/doom/snippets/snippets/latex-mode/columns" nil "cols")
                       ("code" "\\begin{lstlisting}\n${0:`%`}\n\\end{lstlisting}" "code" nil nil nil "/home/orion/.config/doom/snippets/snippets/latex-mode/code" nil "code")
                       ("c" "\\cite{$1} $0" "cite"
                        (not
                         (save-restriction
                           (widen)
                           (texmathp)))
                        nil nil "/home/orion/.config/doom/snippets/snippets/latex-mode/cite" nil "c")
                       ("ca" "\\caption{${0:`%`}}" "caption" nil nil nil "/home/orion/.config/doom/snippets/snippets/latex-mode/caption" nil "ca")
                       ("G" "\\Gls{${1:label}}" "Gls"
                        (not
                         (save-restriction
                           (widen)
                           (texmathp)))
                        nil nil "/home/orion/.config/doom/snippets/snippets/latex-mode/capgls" nil "G")
                       ("bl" "\\begin{block}{$1}\n        ${0:`%`}\n\\end{block}" "block" nil nil nil "/home/orion/.config/doom/snippets/snippets/latex-mode/block" nil "bl")
                       ("cup" "\\bigcup${1:$(when (> (length yas-text) 0) \"_\")\n}${1:$(when (> (length yas-text) 1) \"{\")\n}${1:i=0}${1:$(when (> (length yas-text) 1) \"}\")\n}${2:$(when (> (length yas-text) 0) \"^\")\n}${2:$(when (> (length yas-text) 1) \"{\")\n}${2:n}${2:$(when (> (length yas-text) 1) \"}\")} $0" "bigcup_^" nil nil nil "/home/orion/.config/doom/snippets/snippets/latex-mode/bigcup_^" nil nil)
                       ("cap" "\\bigcap${1:$(when (> (length yas-text) 0) \"_\")\n}${1:$(when (> (length yas-text) 1) \"{\")\n}${1:i=0}${1:$(when (> (length yas-text) 1) \"}\")\n}${2:$(when (> (length yas-text) 0) \"^\")\n}${2:$(when (> (length yas-text) 1) \"{\")\n}${2:n}${2:$(when (> (length yas-text) 1) \"}\")} $0" "bigcap_^" nil nil nil "/home/orion/.config/doom/snippets/snippets/latex-mode/bigcap_^" nil nil)
                       ("begin" "\\begin{${1:environment}}\n`%`$0\n\\end{$1}" "begin" nil nil nil "/home/orion/.config/doom/snippets/snippets/latex-mode/begin" nil "begin")
                       ("axm" "\\begin{axiom}\n`%`$0\n\\end{axiom}" "axiom" nil
                        ("theorems")
                        nil "/home/orion/.config/doom/snippets/snippets/latex-mode/axiom" nil nil)
                       ("alg" "\\begin{algorithmic}\n${0:`%`}\n\\end{algorithmic}\n" "alg" nil nil nil "/home/orion/.config/doom/snippets/snippets/latex-mode/alg" nil "alg")
                       ("al" "\\begin{alertblock}{$2}\n        ${0:`%`}\n\\end{alertblock}" "alertblock" nil nil nil "/home/orion/.config/doom/snippets/snippets/latex-mode/alertblock" nil "al")
                       ("ac" "\\newacronym{${1:label}}{${1:$(upcase yas-text)}}{${2:Name}}" "acronym" nil nil nil "/home/orion/.config/doom/snippets/snippets/latex-mode/acronym" nil "ac")))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("defun" "(defun ${1:fun} (${2:args})\n       $0\n)" "defun" nil nil nil "/home/orion/.config/doom/snippets/snippets/lisp-interaction-mode/defun" nil "defun")))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("when" "(when (${1:condition}) ${2:`(doom-snippets-format \"%n%s\")`})" "when ... (...)" nil nil nil "/home/orion/.config/doom/snippets/snippets/lisp-mode/when" nil "when")
                       ("unless" "(unless (${1:condition}) ${2:`(doom-snippets-format \"%n%s\")`})" "unless ... (...)" nil nil nil "/home/orion/.config/doom/snippets/snippets/lisp-mode/unless" nil "unless")
                       ("typecast" "(coerce ${1:object} ${2:type})" "cast" nil nil nil "/home/orion/.config/doom/snippets/snippets/lisp-mode/typecast" nil nil)
                       ("slot" "(${1:name} :initarg :${1:$(yas/substr yas-text \"[^: ]*\")}\n           :initform (error \":${1:$(yas/substr yas-text \"[^: ]*\")} must be specified\")\n           ;; :accessor ${1:$(yas/substr yas-text \"[^: ]*\")}\n           :reader ${1:$(yas/substr yas-text \"[^: ]*\")}-changed\n           :writer set-${1:$(yas/substr yas-text \"[^: ]*\")}\n           :type\n           :allocation ${3::class :instance}\n           :documentation \"${2:about-slot}\")\n$0\n" "slot" nil nil nil "/home/orion/.config/doom/snippets/snippets/lisp-mode/slot" nil nil)
                       ("if" "(if ${1:condition} ${2:`(doom-snippets-format \"%n%s\")`})" "if" nil nil nil "/home/orion/.config/doom/snippets/snippets/lisp-mode/if" nil nil)
                       ("format" "(format t \"~& $0 ~%\")" "format" nil nil nil "/home/orion/.config/doom/snippets/snippets/lisp-mode/format" nil nil)
                       ("dotimes" "(dotimes (${1:var} ${2:count}) ${3:`(doom-snippets-format \"%n%s\")`})" "dotimes" nil nil nil "/home/orion/.config/doom/snippets/snippets/lisp-mode/dotimes" nil nil)
                       ("dot"
                        (progn
                          (doom-snippets-expand :uuid "dotimes"))
                        "dotimes" nil nil nil "/home/orion/.config/doom/snippets/snippets/lisp-mode/dot" nil "dot")
                       ("dolist" "(dolist (${1:var} ${2:list}) ${3:`(doom-snippets-format \"%n%s\")`})" "dolist" nil nil nil "/home/orion/.config/doom/snippets/snippets/lisp-mode/dolist" nil nil)
                       ("dol"
                        (progn
                          (doom-snippets-expand :uuid "dolist"))
                        "dolist" nil nil nil "/home/orion/.config/doom/snippets/snippets/lisp-mode/dol" nil "dol")
                       ("do" "(do ((${1:var1} ${2:init-form} ${3:step-form})\n     (${4:var2} ${5:init-form} ${6:step-form}))  \n    (${7:condition} ${8:return-value})\n    (${9:body}))\n$0\n" "do" nil nil nil "/home/orion/.config/doom/snippets/snippets/lisp-mode/do" nil "do")
                       ("defp" "(defpackage #:${1:name}\n   (:nicknames #:${2:nick})\n   (:use #:cl #:closer-mop #:${3:package})\n   (:shadow :${4.symbol})\n   (:shadowing-import-from #:${5:package} #:${6:symbol})\n   (:export :$0))\n" "defpackage" nil nil nil "/home/orion/.config/doom/snippets/snippets/lisp-mode/defpackage" nil "defp")
                       ("cond" "(cond ($1)$2)" "cond" nil nil nil "/home/orion/.config/doom/snippets/snippets/lisp-mode/cond" nil nil)
                       ("defc" "(defclass ${1:name} (${2:inherits})\n  (${4:slots})\n  (:documentation \"${3:...}\"))" "defclass" nil nil nil "/home/orion/.config/doom/snippets/snippets/lisp-mode/class" nil "defc")))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("while" "while ${1:true} do\n    $0\nend" "while ... do ... end" nil nil nil "/home/orion/.config/doom/snippets/snippets/lua-mode/while" nil nil)
                       ("ton" "tonumber($1)\n" "tonumber(...)" nil nil nil "/home/orion/.config/doom/snippets/snippets/lua-mode/ton" nil nil)
                       ("=-" "${1:varName} = $1 - ${2:1}" "self-subtraction" nil nil nil "/home/orion/.config/doom/snippets/snippets/lua-mode/sub" nil "=-")
                       ("sqrt" "# -*- mode: snippet -*-\nmath.sqrt($0)\n" "sqrt" nil nil nil "/home/orion/.config/doom/snippets/snippets/lua-mode/sqrt" nil nil)
                       ("self:" "function self:${1:methodName}($2)\n    $0\nend\n" "psuedo method declaration" nil nil nil "/home/orion/.config/doom/snippets/snippets/lua-mode/self" nil "self:")
                       ("req" "require(\"${1:filename}\")\n" "require(...)" nil nil nil "/home/orion/.config/doom/snippets/snippets/lua-mode/require" nil "req")
                       ("p" "print(`%`$1)\n" "print" nil nil nil "/home/orion/.config/doom/snippets/snippets/lua-mode/print" nil "p")
                       ("pi" "# -*- mode: snippet -*-\nmath.pi\n" "pi" nil nil nil "/home/orion/.config/doom/snippets/snippets/lua-mode/pi" nil nil)
                       ("open" "io.open(${1:\"${2:filename}\"}, \"${3:r}\")\n" "io.open" nil nil nil "/home/orion/.config/doom/snippets/snippets/lua-mode/open" nil nil)
                       ("math.tanh" "# -*- mode: snippet -*-\nmath.tanh($0)\n" "math.tanh" nil nil nil "/home/orion/.config/doom/snippets/snippets/lua-mode/math.tanh" nil nil)
                       ("math.tan" "# -*- mode: snippet -*-\nmath.tan($0)\n" "math.tan" nil nil nil "/home/orion/.config/doom/snippets/snippets/lua-mode/math.tan" nil nil)
                       ("math.sqrt" "# -*- mode: snippet -*-\nmath.sqrt($0)\n" "math.sqrt" nil nil nil "/home/orion/.config/doom/snippets/snippets/lua-mode/math.sqrt" nil nil)
                       ("math.sinh" "# -*- mode: snippet -*-\nmath.sinh($0)\n" "math.sinh" nil nil nil "/home/orion/.config/doom/snippets/snippets/lua-mode/math.sinh" nil nil)
                       ("math.sin" "# -*- mode: snippet -*-\nmath.sin($0)\n" "math.sin" nil nil nil "/home/orion/.config/doom/snippets/snippets/lua-mode/math.sin" nil nil)
                       ("math.randomseed" "# -*- mode: snippet -*-\nmath.randomseed($0)\n" "math.randomseed" nil nil nil "/home/orion/.config/doom/snippets/snippets/lua-mode/math.randomseed" nil nil)
                       ("math.random" "# -*- mode: snippet -*-\nmath.random($0)\n" "math.random" nil nil nil "/home/orion/.config/doom/snippets/snippets/lua-mode/math.random" nil nil)
                       ("math.rad" "# -*- mode: snippet -*-\nmath.rad($0)\n" "math.rad" nil nil nil "/home/orion/.config/doom/snippets/snippets/lua-mode/math.rad" nil nil)
                       ("math.pow" "# -*- mode: snippet -*-\nmath.pow($0)\n" "math.pow" nil nil nil "/home/orion/.config/doom/snippets/snippets/lua-mode/math.pow" nil nil)
                       ("math.pi" "# -*- mode: snippet -*-\nmath.pi\n" "math.pi" nil nil nil "/home/orion/.config/doom/snippets/snippets/lua-mode/math.pi" nil nil)
                       ("math.modf" "# -*- mode: snippet -*-\nmath.modf($0)\n" "math.modf" nil nil nil "/home/orion/.config/doom/snippets/snippets/lua-mode/math.modf" nil nil)
                       ("math.min" "# -*- mode: snippet -*-\nmath.min(${0:x, y, ...})\n" "math.min" nil nil nil "/home/orion/.config/doom/snippets/snippets/lua-mode/math.min" nil nil)
                       ("math.max" "# -*- mode: snippet -*-\nmath.max(${0:x, y, ...})\n" "math.max" nil nil nil "/home/orion/.config/doom/snippets/snippets/lua-mode/math.max" nil nil)
                       ("math.log10" "# -*- mode: snippet -*-\nmath.log10($0)\n" "math.log10" nil nil nil "/home/orion/.config/doom/snippets/snippets/lua-mode/math.log10" nil nil)
                       ("math.ldexp" "# -*- mode: snippet -*-\nmath.ldexp($0)\n" "math.ldexp" nil nil nil "/home/orion/.config/doom/snippets/snippets/lua-mode/math.ldexp" nil nil)
                       ("math.huge" "# -*- mode: snippet -*-\nmath.huge($0)\n" "math.huge" nil nil nil "/home/orion/.config/doom/snippets/snippets/lua-mode/math.huge" nil nil)
                       ("math.frexp" "# -*- mode: snippet -*-\nmath.frexp($0)\n" "math.frexp" nil nil nil "/home/orion/.config/doom/snippets/snippets/lua-mode/math.frexp" nil nil)
                       ("math.fmod" "# -*- mode: snippet -*-\nmath.fmod($0)\n" "math.fmod" nil nil nil "/home/orion/.config/doom/snippets/snippets/lua-mode/math.fmod" nil nil)
                       ("math.floor" "# -*- mode: snippet -*-\nmath.floor($0)\n" "math.floor" nil nil nil "/home/orion/.config/doom/snippets/snippets/lua-mode/math.floor" nil nil)
                       ("math.exp" "# -*- mode: snippet -*-\nmath.exp($0)\n" "math.exp" nil nil nil "/home/orion/.config/doom/snippets/snippets/lua-mode/math.exp" nil nil)
                       ("math.deg" "# -*- mode: snippet -*-\nmath.deg($0)\n" "math.deg" nil nil nil "/home/orion/.config/doom/snippets/snippets/lua-mode/math.deg" nil nil)
                       ("math.cosh" "# -*- mode: snippet -*-\nmath.cosh($0)\n" "math.cosh" nil nil nil "/home/orion/.config/doom/snippets/snippets/lua-mode/math.cosh" nil nil)
                       ("math.cos" "# -*- mode: snippet -*-\nmath.cos($0)\n" "math.cos" nil nil nil "/home/orion/.config/doom/snippets/snippets/lua-mode/math.cos" nil nil)
                       ("math.ceil" "# -*- mode: snippet -*-\nmath.ceil($0)\n" "math.ceil" nil nil nil "/home/orion/.config/doom/snippets/snippets/lua-mode/math.ceil" nil nil)
                       ("math.atan2" "# -*- mode: snippet -*-\nmath.atan2(${1:y}, ${2:x})$0\n" "math.atan2" nil nil nil "/home/orion/.config/doom/snippets/snippets/lua-mode/math.atan2" nil nil)
                       ("math.atan" "# -*- mode: snippet -*-\nmath.atan($0)\n" "math.atan" nil nil nil "/home/orion/.config/doom/snippets/snippets/lua-mode/math.atan" nil nil)
                       ("math.asin" "# -*- mode: snippet -*-\nmath.asin($0)\n" "math.asin" nil nil nil "/home/orion/.config/doom/snippets/snippets/lua-mode/math.asin" nil nil)
                       ("math.acos" "# -*- mode: snippet -*-\nmath.acos($0)\n" "math.acos" nil nil nil "/home/orion/.config/doom/snippets/snippets/lua-mode/math.acos" nil nil)
                       ("math.abs" "# -*- mode: snippet -*-\nmath.abs($0)\n" "math.abs" nil nil nil "/home/orion/.config/doom/snippets/snippets/lua-mode/math.abs" nil nil)
                       ("l" "local $0" "local ..." nil nil nil "/home/orion/.config/doom/snippets/snippets/lua-mode/local" nil "l")
                       ("?" "if ${1:true} then `%`$0 end" "inline if ... then ... end" nil nil nil "/home/orion/.config/doom/snippets/snippets/lua-mode/inline-if" nil "?")
                       ("f=" "${1:name} = function($2)\n    ${3:-- code}\nend\n" "? = function(...) ... end" nil nil nil "/home/orion/.config/doom/snippets/snippets/lua-mode/inline-function" nil "f=")
                       ("if" "if ${1:true} then\n   `%`$0\nend" "if ... then ... end" nil nil nil "/home/orion/.config/doom/snippets/snippets/lua-mode/if" nil nil)
                       ("functioni" "function ${1:name}($2) $3 end" "function" nil nil nil "/home/orion/.config/doom/snippets/snippets/lua-mode/functioni" nil nil)
                       ("fu" "function ${1:name}($2)\n    ${3:-- code}\nend" "function" nil nil nil "/home/orion/.config/doom/snippets/snippets/lua-mode/function" nil "fu")
                       ("forline" "f = io.open(${1:\"${2:filename}\"}, \"${3:r}\")\n\nwhile true do\n    line = f:read()\n    if line == nil then break end\n\n    ${0:-- code}\nend\n" "read file line-by-line" nil nil nil "/home/orion/.config/doom/snippets/snippets/lua-mode/forline" nil nil)
                       ("fori" "for ${1:i}=${2:1},${3:10} do\n    $0\nend\n" "for loop range" nil nil nil "/home/orion/.config/doom/snippets/snippets/lua-mode/fori" nil nil)
                       ("foreach" "for i, ${1:x} in pairs(${2:table}) do\n    $0\nend\n" "for loop range" nil nil nil "/home/orion/.config/doom/snippets/snippets/lua-mode/foreach" nil nil)
                       ("for" "for ${1:i}=${2:1},${3:10} do\n    $0\nend\n" "for loop range" nil nil nil "/home/orion/.config/doom/snippets/snippets/lua-mode/for" nil nil)
                       ("fi" "function ${1:name}($2) $3 end" "function" nil nil nil "/home/orion/.config/doom/snippets/snippets/lua-mode/fi" nil nil)
                       ("elseif" "elseif ${1:true} then\n   `%`$0" "elseif ... then ..." nil nil nil "/home/orion/.config/doom/snippets/snippets/lua-mode/elseif" nil nil)
                       ("cl" "${1:ClassName} = {}\n$1.new = function($2)\n    local self = {}\n\n    ${3:-- code}\n\n    return self\nend" "psuedo class declaration" nil nil nil "/home/orion/.config/doom/snippets/snippets/lua-mode/class" nil "cl")
                       ("=" "${1:varName} = ${0:value}" "... = ..." nil nil nil "/home/orion/.config/doom/snippets/snippets/lua-mode/assignment" nil "=")
                       ("=+" "${1:varName} = $1 + ${2:1}" "self-addition" nil nil nil "/home/orion/.config/doom/snippets/snippets/lua-mode/add" nil "=+")))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("def" "define(\\`${1:macro}',\\`${2:subst}').\n$0" "def" nil nil nil "/home/orion/.config/doom/snippets/snippets/m4-mode/def" nil "def")))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("noinst" "noinst_HEADERS = $0" "noinst_HEADERS" nil nil nil "/home/orion/.config/doom/snippets/snippets/makefile-automake-mode/noinst_HEADERS" nil "noinst")))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("$" "$(${1:VAR})$0" "var" nil nil nil "/home/orion/.config/doom/snippets/snippets/makefile-bsdmake-mode/var" nil "$")
                       ("if" "@if [ ${1:cond} ]\n    then $0\nfi\n" "if" nil nil nil "/home/orion/.config/doom/snippets/snippets/makefile-bsdmake-mode/if" nil "if")
                       ("gen" "all: ${1:targets}\n\n$0\n\nclean:\n        ${2:clean actions}\n" "gen" nil nil nil "/home/orion/.config/doom/snippets/snippets/makefile-bsdmake-mode/gen" nil "gen")
                       ("echo" "@echo ${1:\"message to output\"}\n" "echo" nil nil nil "/home/orion/.config/doom/snippets/snippets/makefile-bsdmake-mode/echo" nil "echo")
                       ("phony" ".PHONY: $0" "PHONY" nil nil nil "/home/orion/.config/doom/snippets/snippets/makefile-bsdmake-mode/PHONY" nil "phony")))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("wildcard" "$(wildcard $0)\n" "wildcard" nil nil nil "/home/orion/.config/doom/snippets/snippets/makefile-gmake-mode/wildcard" nil nil)
                       ("phony" ".PHONY = $0\n" "phony" nil nil nil "/home/orion/.config/doom/snippets/snippets/makefile-gmake-mode/phony" nil nil)
                       ("patsubst" "$(patsubst ${1:from},${2:to},${3:src})\n" "patsubst" nil nil nil "/home/orion/.config/doom/snippets/snippets/makefile-gmake-mode/patsubst" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("all" "all:\n        $0" "all" nil nil nil "/home/orion/.config/doom/snippets/snippets/makefile-mode/all" nil "all")))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("utf8" "<meta http-equiv='Content-Type' content='text/html; charset=utf-8' />\n$0" "utf-8 encoding" nil nil nil "/home/orion/.config/doom/snippets/snippets/markdown-mode/utf8" nil nil)
                       ("rlink" "[${1:`%`Link Text}][$2]$0" "Reference Link" nil nil nil "/home/orion/.config/doom/snippets/snippets/markdown-mode/rlink" nil nil)
                       ("rlb" "[${1:Reference}]: ${2:URL} $3\n$0\n" "Reference Label" nil nil nil "/home/orion/.config/doom/snippets/snippets/markdown-mode/rlb" nil nil)
                       ("rimg" "![${1:Alt Text}][$2]$0" "Referenced Image" nil nil nil "/home/orion/.config/doom/snippets/snippets/markdown-mode/rimg" nil nil)
                       ("ol" "${1:1}. ${2:Text}\n${1:$(number-to-string (1+ (string-to-number %)))}. $0" "Ordered List" nil nil nil "/home/orion/.config/doom/snippets/snippets/markdown-mode/ol" nil nil)
                       ("link" "[${1:`(or % \"text\")`}](${2:http://$3})$0" "Link" nil nil nil "/home/orion/.config/doom/snippets/snippets/markdown-mode/link" nil nil)
                       ("kbd" "<kbd>$0</kbd>" "<kbd>...</kbd>" nil nil nil "/home/orion/.config/doom/snippets/snippets/markdown-mode/kbd" nil nil)
                       ("img" "![${1:Alt Text}](${2:`%`URL})$0" "Image" nil nil nil "/home/orion/.config/doom/snippets/snippets/markdown-mode/img" nil nil)
                       ("---" "-------------------------------------------------------------------------------" "hr" nil nil nil "/home/orion/.config/doom/snippets/snippets/markdown-mode/hr" "direct-keybinding" "---")
                       ("h6" "###### ${1:Header 6}`(unless markdown-asymmetric-header \" ######\")`" "Header 6" nil nil nil "/home/orion/.config/doom/snippets/snippets/markdown-mode/h6" nil "h6")
                       ("h5" "##### ${1:Header 5}`(unless markdown-asymmetric-header \" #####\")`" "Header 5" nil nil nil "/home/orion/.config/doom/snippets/snippets/markdown-mode/h5" nil "h5")
                       ("h4" "#### ${1:Header 4}`(unless markdown-asymmetric-header \" ####\")`" "Header 4" nil nil nil "/home/orion/.config/doom/snippets/snippets/markdown-mode/h4" nil "h4")
                       ("h3" "### ${1:Header 3}`(unless markdown-asymmetric-header \" ###\")`" "Header 3" nil nil nil "/home/orion/.config/doom/snippets/snippets/markdown-mode/h3" nil "h3")
                       ("h2" "## ${1:Header 2}`(unless markdown-asymmetric-header \" ##\")`" "Header 2 (##)" nil nil nil "/home/orion/.config/doom/snippets/snippets/markdown-mode/h2" nil "h2")
                       ("h1" "# ${1:Header 1}`(unless markdown-asymmetric-header \" #\")`" "Header 1 (#)" nil nil nil "/home/orion/.config/doom/snippets/snippets/markdown-mode/h1" nil "h1")
                       ("code" "\\`\\`\\`${1:lang}\n`%`$0\n\\`\\`\\`" "Code block" nil nil nil "/home/orion/.config/doom/snippets/snippets/markdown-mode/code" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("uses" "uses interface ${1:Interface}${2: as ${3:alias}};\n$0" "uses" nil nil nil "/home/orion/.config/doom/snippets/snippets/nesc-mode/uses" nil "uses")
                       ("u8" "uint8_t ${1:var};\n$0" "uint8_t" nil nil nil "/home/orion/.config/doom/snippets/snippets/nesc-mode/uint8_t" nil "u8")
                       ("sim" "#ifdef TOSSIM\n       $0\n#endif" "sim" nil nil nil "/home/orion/.config/doom/snippets/snippets/nesc-mode/sim" nil "sim")
                       ("provides" "provides interface ${1:Interface};" "provides" nil nil nil "/home/orion/.config/doom/snippets/snippets/nesc-mode/provides" nil "provides")
                       ("nx" "nx_uint${1:8}_t ${2:var};\n$0" "nx" nil nil nil "/home/orion/.config/doom/snippets/snippets/nesc-mode/nx" nil "nx")
                       ("mod" "module ${1:Module} {\n       ${2:uses interface ${3:Packet}};\n       $0\n}" "module" nil nil nil "/home/orion/.config/doom/snippets/snippets/nesc-mode/module" nil "mod")
                       ("int" "interface ${1:Interface} {\n          $0\n}" "interface" nil nil nil "/home/orion/.config/doom/snippets/snippets/nesc-mode/interface" nil "int")
                       ("ifdef" "#ifdef ${1:Macro}\n $2\n${3:#else}\n $4\n#endif" "ifdef" nil nil nil "/home/orion/.config/doom/snippets/snippets/nesc-mode/ifdef" nil "ifdef")
                       ("event" "event ${1:void} ${2:On.Event}($3) {\n      $0\n}" "event" nil nil nil "/home/orion/.config/doom/snippets/snippets/nesc-mode/event" nil "event")
                       ("dbg" "dbg(\"${1:Module}\", \"${2:message}\"${3:, ${4:var list}});" "dbg" nil nil nil "/home/orion/.config/doom/snippets/snippets/nesc-mode/dbg" nil "dbg")
                       ("command" "command ${1:void} ${2:naMe}($3) {\n\n}" "command" nil nil nil "/home/orion/.config/doom/snippets/snippets/nesc-mode/command" nil "command")
                       ("tossim" "#ifndef TOSSIM\n        $0\n#endif" "TOSSIM" nil nil nil "/home/orion/.config/doom/snippets/snippets/nesc-mode/TOSSIM" nil "tossim")))


;;; contents of the .yas-setup.el support file:
;;;
;;; ~/work/conf/doom-emacs-private/snippets/nix-mode/.yas-setup.el -*- lexical-binding: t; -*-

(defun %nix-mode-if-parent (name &optional else)
  (if (%nix-mode-in-block-p name)
      (or else "")
    (concat name ".")))

(defun %nix-mode-in-block-p (name)
  (if (not (fboundp 'evil-previous-open-brace))
      nil ; TODO Add non-evil support
    (save-excursion
      (evil-previous-open-brace)
      (re-search-backward (format "^\\s-*%s\\s-*=" name) (line-beginning-position) t))))
;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("wsb" "pkgs.writeScriptBin \"${1:name}\" ''\n  #!${pkgs.stdenv.shell}\n  ${0:`%`}\n''" "pkgs.writeScriptBin ..." nil nil nil "/home/orion/.config/doom/snippets/snippets/nix-mode/writeScriptBin" nil "wsb")
                       ("with" "with ${1:pkgs};" "with ...;" nil nil nil "/home/orion/.config/doom/snippets/snippets/nix-mode/with" nil nil)
                       ("pkgu" "{ stdenv, lib, fetchurl$1 }:\n\nstdenv.mkDerivation rec {\n  version = \"$2\";\n  name = \"$3-\\$\\{version\\}\";\n\n  src = fetchurl {\n    url = \"$4\";\n    sha256 = \"$5\";\n  };\n\n  buildInputs = [ $1 ];\n\n  meta = {\n    description = \"$6\";\n    homepage = \"https://$7\";\n    license = lib.licenses.${8:$$\n    (yas-choose-value '(\n      \"agpl3\"\n      \"asl20\"\n      \"bsd2\"\n      \"bsd3\"\n      \"gpl2\"\n      \"gpl3\"\n      \"lgpl3\"\n      \"mit\"\n    ))};\n    maintainers = [ lib.maintainers.$9 ];\n    platforms = platforms.${10:$$\n    (yas-choose-value '(\n      \"gnu\"\n      \"linux\"\n      \"darwin\"\n      \"freebsd\"\n      \"openbsd\"\n      \"netbsd\"\n      \"cygwin\"\n      \"illumos\"\n      \"unix\"\n      \"all\"\n      \"none\"\n      \"allBut\"\n      \"mesaPlatforms\"\n      \"x86\"\n      \"i686\"\n      \"arm\"\n      \"mips\"\n    ))};\n  };\n}" "package url" nil nil nil "/home/orion/.config/doom/snippets/snippets/nix-mode/package.url" nil "pkgu")
                       ("pkgg" "{ stdenv, lib, fetchFromGitHub$1 }:\n\nstdenv.mkDerivation rec {\n  name = \"$2-\\$\\{version\\}\";\n  version = \"$3\";\n\n  src = fetchFromGitHub {\n    owner = \"$4\";\n    repo = \"$2\";\n    rev = \"${5:v\\$\\{version\\}}\";\n    sha256 = \"$6\";\n  };\n\n  buildInputs = [ $1 ];\n\n  meta = {\n    description = \"$7\";\n    homepage = \"https://${8:github.com/$4/$2}\";\n\n    license = lib.licenses.${9:$$\n    (yas-choose-value '(\n      \"agpl3\"\n      \"asl20\"\n      \"bsd2\"\n      \"bsd3\"\n      \"gpl2\"\n      \"gpl3\"\n      \"lgpl3\"\n      \"mit\"\n    ))};\n    maintainers = [ lib.maintainers.$10 ];\n    platforms = lib.platforms.${11:$$\n    (yas-choose-value '(\n      \"gnu\"\n      \"linux\"\n      \"darwin\"\n      \"freebsd\"\n      \"openbsd\"\n      \"netbsd\"\n      \"cygwin\"\n      \"illumos\"\n      \"unix\"\n      \"all\"\n      \"none\"\n      \"allBut\"\n      \"mesaPlatforms\"\n      \"x86\"\n      \"i686\"\n      \"arm\"\n      \"mips\"\n    ))};\n  };\n}" "package github" nil nil nil "/home/orion/.config/doom/snippets/snippets/nix-mode/package.github" nil "pkgg")
                       ("imp" "imports = [\n  ${0:`%`}\n];" "imports = [ ... ];"
                        (doom-snippets-bolp)
                        nil nil "/home/orion/.config/doom/snippets/snippets/nix-mode/imports" nil "imp")
                       ("hmu" "home-manager.users.`user-login-name`" "home-manager.users.$USER" nil nil nil "/home/orion/.config/doom/snippets/snippets/nix-mode/home-manager.users.USER" nil "hmu")
                       ("hm" "home-manager" "home-manager" nil nil nil "/home/orion/.config/doom/snippets/snippets/nix-mode/home-manager" nil "hm")
                       ("ftb" "builtins.fetchTarball ${1:url}" "builtins.fetchTarball ..." nil nil nil "/home/orion/.config/doom/snippets/snippets/nix-mode/fetchTarball" nil "ftb")
                       ("eva" "`(%nix-mode-if-parent \"environment\")`variables = {\n  ${0:`%`}\n};" "environment.variables" nil nil nil "/home/orion/.config/doom/snippets/snippets/nix-mode/environment.variables" nil "eva")
                       ("esp" "`(%nix-mode-if-parent \"environment\")`systemPackages = with pkgs; [\n  ${0:`%`}\n];" "environment.systemPackages" nil nil nil "/home/orion/.config/doom/snippets/snippets/nix-mode/environment.systemPackages" nil "esp")
                       ("esv" "`(%nix-mode-if-parent \"environment\")`sessionVariables = {\n  ${0:`%`}\n};" "environment.sessionVariables" nil nil nil "/home/orion/.config/doom/snippets/snippets/nix-mode/environment.sessionVariables" nil "esv")
                       ("env" "environment = {\n  ${0:`%`}\n};" "environment" nil nil nil "/home/orion/.config/doom/snippets/snippets/nix-mode/environment" nil "env")))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("sec" "Section \"${1:Program}\"\n        $0\nSectionEnd" "section" nil nil nil "/home/orion/.config/doom/snippets/snippets/nsis-mode/section" nil "sec")
                       ("out" "outFile \"${1:setup}.exe\"" "outfile" nil nil nil "/home/orion/.config/doom/snippets/snippets/nsis-mode/outfile" nil "out")
                       ("$" "$OUTDIR" "outdir" nil nil nil "/home/orion/.config/doom/snippets/snippets/nsis-mode/outdir" nil "$")
                       ("msg" "MessageBox MB_OK \"${1:hello}\"" "message" nil nil nil "/home/orion/.config/doom/snippets/snippets/nsis-mode/message" nil "msg")
                       ("macro" "!macro ${1:Name} UN\n$0\n\n!macroend" "macro" nil nil nil "/home/orion/.config/doom/snippets/snippets/nsis-mode/macro" nil "macro")
                       ("$" "$INSTDIR" "instdir" nil nil nil "/home/orion/.config/doom/snippets/snippets/nsis-mode/instdir" nil "$")
                       ("im" "!insermacro ${1:Name} ${2:\"args\"}" "insert_macro" nil nil nil "/home/orion/.config/doom/snippets/snippets/nsis-mode/insert_macro" nil "im")
                       ("inc" "!include \"${Library.nsh}\"" "include" nil nil nil "/home/orion/.config/doom/snippets/snippets/nsis-mode/include" nil "inc")
                       ("if" "${IF} ${1:cond}\n      $0\n${ElseIf} ${2:else_cond}\n\n${EndIf}" "if" nil nil nil "/home/orion/.config/doom/snippets/snippets/nsis-mode/if" nil "if")
                       ("fun" "Function ${1:Name}\n         $0\nFunctionEnd" "function" nil nil nil "/home/orion/.config/doom/snippets/snippets/nsis-mode/function" nil "fun")
                       ("def" "!define ${1:CONSTANT} ${2:value}" "define" nil nil nil "/home/orion/.config/doom/snippets/snippets/nsis-mode/define" nil "def")))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("ul.id" "<ul id=\"$1\">\n  $0\n</ul>" "<ul id=\"...\">...</ul>" nil
                        ("list")
                        nil "/home/orion/.config/doom/snippets/snippets/nxml-mode/ul.id" nil nil)
                       ("ul" "<ul>\n  $0\n</ul>" "<ul>...</ul>" nil nil nil "/home/orion/.config/doom/snippets/snippets/nxml-mode/ul" nil nil)
                       ("tr" "<tr>\n  $0\n</tr>" "<tr>...</tr>" nil nil nil "/home/orion/.config/doom/snippets/snippets/nxml-mode/tr" nil nil)
                       ("title" "<title>$1</title>" "<title>...</title>" nil nil nil "/home/orion/.config/doom/snippets/snippets/nxml-mode/title" nil nil)
                       ("th" "<th$1>$2</th>" "<th>...</th>" nil nil nil "/home/orion/.config/doom/snippets/snippets/nxml-mode/th" nil nil)
                       ("td" "<td$1>$2</td>" "<td>...</td>" nil nil nil "/home/orion/.config/doom/snippets/snippets/nxml-mode/td" nil nil)
                       ("tag.2l" "<${1:tag}>\n  $2\n</$1>$0" "<tag> \\n...\\n</tag>" nil nil nil "/home/orion/.config/doom/snippets/snippets/nxml-mode/tag.2l" nil nil)
                       ("tag.1l" "<${1:tag}>$2</$1>$0" "<tag>...</tag>" nil nil nil "/home/orion/.config/doom/snippets/snippets/nxml-mode/tag.1l" nil nil)
                       ("table" "<table>\n  $0\n</table>" "<table>...</table>" nil nil nil "/home/orion/.config/doom/snippets/snippets/nxml-mode/table" nil nil)
                       ("style" "<style type=\"text/css\" media=\"${1:screen}\">\n  `%`$0\n</style>\n" "<style type=\"text/css\" media=\"...\">...</style>" nil nil nil "/home/orion/.config/doom/snippets/snippets/nxml-mode/style" nil nil)
                       ("span.id" "<span id=\"$1\">$2</span>" "<span id=\"...\">...</span>" nil nil nil "/home/orion/.config/doom/snippets/snippets/nxml-mode/span.id" nil nil)
                       ("span" "<span>$1</span>" "<span>...</span>" nil nil nil "/home/orion/.config/doom/snippets/snippets/nxml-mode/span" nil nil)
                       ("quote" "<blockquote>\n  $1\n</blockquote>" "<blockquote>...</blockquote>" nil nil nil "/home/orion/.config/doom/snippets/snippets/nxml-mode/quote" nil nil)
                       ("pre" "<pre>\n  $0\n</pre>" "<pre>...</pre>" nil nil nil "/home/orion/.config/doom/snippets/snippets/nxml-mode/pre" nil nil)
                       ("p" "<p>$1</p>" "<p>...</p>" nil nil nil "/home/orion/.config/doom/snippets/snippets/nxml-mode/p" nil nil)
                       ("ol.id" "<ol id=\"$1\">\n  $0\n</ol>" "<ol id=\"...\">...</ol>" nil
                        ("list")
                        nil "/home/orion/.config/doom/snippets/snippets/nxml-mode/ol.id" nil nil)
                       ("ol" "<ol>\n  $0\n</ol>" "<ol>...</ol>" nil nil nil "/home/orion/.config/doom/snippets/snippets/nxml-mode/ol" nil nil)
                       ("name" "<a name=\"$1\"></a>" "<a name=\"...\"></a>" nil nil nil "/home/orion/.config/doom/snippets/snippets/nxml-mode/name" nil nil)
                       ("meta" "<meta name=\"${1:generator}\" content=\"${2:content}\" />" "<meta name=\"...\" content=\"...\" />" nil
                        ("meta")
                        nil "/home/orion/.config/doom/snippets/snippets/nxml-mode/meta" nil nil)
                       ("li" "<li>$1</li>" "<li>...</li>" nil nil nil "/home/orion/.config/doom/snippets/snippets/nxml-mode/li" nil nil)
                       ("input" "<input type=\"$1\" name=\"$2\" value=\"$3\" />" "<input ... />" nil nil nil "/home/orion/.config/doom/snippets/snippets/nxml-mode/input" nil nil)
                       ("img" "<img src=\"$1\" alt=\"$2\" />" "<img src=\"...\" alt=\"...\" />" nil nil nil "/home/orion/.config/doom/snippets/snippets/nxml-mode/img" nil nil)
                       ("html" "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"${1:en}\" lang=\"${2:en}\">\n  $0\n</html>\n" "<html xmlns=\"...\">...</html>" nil nil nil "/home/orion/.config/doom/snippets/snippets/nxml-mode/html" nil nil)
                       ("a" "<a href=\"$1\">$2</a>" "<a href=\"...\">...</a>" nil nil nil "/home/orion/.config/doom/snippets/snippets/nxml-mode/href" nil "a")
                       ("hr" "<hr />\n" "<hr />" nil nil nil "/home/orion/.config/doom/snippets/snippets/nxml-mode/hr" nil nil)
                       ("head" "<head>\n  $0\n</head>" "<head>...</head>" nil nil nil "/home/orion/.config/doom/snippets/snippets/nxml-mode/head" nil nil)
                       ("h6" "<h6>$1</h6>" "<h6>...</h6>" nil
                        ("header")
                        nil "/home/orion/.config/doom/snippets/snippets/nxml-mode/h6" nil nil)
                       ("h5" "<h5>$1</h5>" "<h5>...</h5>" nil
                        ("header")
                        nil "/home/orion/.config/doom/snippets/snippets/nxml-mode/h5" nil nil)
                       ("h4" "<h4>$1</h4>" "<h4>...</h4>" nil
                        ("header")
                        nil "/home/orion/.config/doom/snippets/snippets/nxml-mode/h4" nil nil)
                       ("h3" "<h3>$1</h3>" "<h3>...</h3>" nil
                        ("header")
                        nil "/home/orion/.config/doom/snippets/snippets/nxml-mode/h3" nil nil)
                       ("h2" "<h2>$1</h2>" "<h2>...</h2>" nil
                        ("header")
                        nil "/home/orion/.config/doom/snippets/snippets/nxml-mode/h2" nil nil)
                       ("h1" "<h1>$1</h1>" "<h1>...</h1>" nil
                        ("header")
                        nil "/home/orion/.config/doom/snippets/snippets/nxml-mode/h1" nil nil)
                       ("form" "<form method=\"$1\" action=\"$2\">\n  $0\n</form>" "<form method=\"...\" action=\"...\"></form>" nil nil nil "/home/orion/.config/doom/snippets/snippets/nxml-mode/form" nil nil)
                       ("doctype.xhtml1_transitional" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">" "DocType XHTML 1.0 Transitional" nil
                        ("meta")
                        nil "/home/orion/.config/doom/snippets/snippets/nxml-mode/doctype.xhtml1_transitional" nil nil)
                       ("doctype.xhtml1_strict" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">" "DocType XHTML 1.0 Strict" nil
                        ("meta")
                        nil "/home/orion/.config/doom/snippets/snippets/nxml-mode/doctype.xhtml1_strict" nil nil)
                       ("code" "<code>\n  $0\n</code>" "<code>...</code>" nil nil nil "/home/orion/.config/doom/snippets/snippets/nxml-mode/code" nil nil)
                       ("br" "<br />" "<br />" nil nil nil "/home/orion/.config/doom/snippets/snippets/nxml-mode/br" nil nil)
                       ("body" "<body$1>\n  $0\n</body>" "<body>...</body>" nil nil nil "/home/orion/.config/doom/snippets/snippets/nxml-mode/body" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("width" "#+attr_html: :width ${1:500px}" "#+attr_html: :width ..." nil nil nil "/home/orion/.config/doom/snippets/snippets/org-mode/width" nil nil)
                       ("verse" "#+begin_verse\n`%`$0\n#+end_verse" "verse" nil nil nil "/home/orion/.config/doom/snippets/snippets/org-mode/verse" nil "verse")
                       ("todo" "TODO ${1:task description}" "TODO item" nil nil nil "/home/orion/.config/doom/snippets/snippets/org-mode/todo" nil "todo")
                       ("src" "#+begin_src $1\n`%`$0\n#+end_src" "#+begin_src" nil nil nil "/home/orion/.config/doom/snippets/snippets/org-mode/src" nil "src")
                       ("quote" "#+begin_quote\n`%`$0\n#+end_quote" "quote block" nil nil nil "/home/orion/.config/doom/snippets/snippets/org-mode/quote" nil "quote")
                       ("name" "#+name: $0\n" "name" nil nil nil "/home/orion/.config/doom/snippets/snippets/org-mode/name" nil "name")
                       ("matrix" "\\left \\(\n\\begin{array}{${1:ccc}}\n${2:v1 & v2} \\\\\n`%`$0\n\\end{array}\n\\right \\)" "matrix" nil nil nil "/home/orion/.config/doom/snippets/snippets/org-mode/matrix" nil "matrix")
                       ("jupyter" "#+begin_src jupyter-${1:$$(yas-choose-value '(\"python\" \"julia\" \"R\"))}${2: :session $3}${4: :async yes}\n`%`$0\n#+end_src\n" "#+begin_src jupyter-..." nil nil nil "/home/orion/.config/doom/snippets/snippets/org-mode/jupyter" nil "jupyter")
                       ("srci" "src_${1:language}[${2:header}]{${0:body}}\n" "Inline Source" t nil nil "/home/orion/.config/doom/snippets/snippets/org-mode/inline_source" nil "srci")
                       ("inl" "src_${1:language}${2:[${3::exports code}]}{${4:code}}" "inline code" nil nil nil "/home/orion/.config/doom/snippets/snippets/org-mode/inline" nil "inl")
                       ("img" "#+attr_html: :alt $2 :align ${3:left} :class img\n[[${1:src}]${4:[${5:title}]}]\n`%`$0" "img" nil nil nil "/home/orion/.config/doom/snippets/snippets/org-mode/img" nil "img")
                       ("head" "#+title:    ${1:`(file-name-sans-extension (file-name-nondirectory (buffer-file-name (current-buffer))))`$0}\n#+author:   ${2:`user-full-name`}\n#+email:    ${4:`user-mail-address`}\n#+date:     ${3:`(format-time-string \"%A, %B%e, %Y\")`$0}" "org-file header" nil nil nil "/home/orion/.config/doom/snippets/snippets/org-mode/header" nil "head")
                       ("fig" "#+caption: ${1:caption}\n#+attr_latex: ${2:scale=0.75}\n#+name: fig-${3:label}\n" "figure" nil nil nil "/home/orion/.config/doom/snippets/snippets/org-mode/figure" nil "fig")
                       ("export" "#+begin_export ${1:type}\n`%`$0\n#+end_export" "export" nil nil nil "/home/orion/.config/doom/snippets/snippets/org-mode/export" nil "export")
                       ("ex" "#+begin_example\n`%`$0\n#+end_example\n" "example" nil nil nil "/home/orion/.config/doom/snippets/snippets/org-mode/example" nil "ex")
                       ("entry" "#+begin_html\n---\nlayout: ${1:default}\ntitle: ${2:title}\n---\n#+end_html\n`%`$0" "entry" nil nil nil "/home/orion/.config/doom/snippets/snippets/org-mode/entry" nil "entry")
                       ("elisp" "#+begin_src emacs-lisp :tangle yes\n`%`$0\n#+end_src" "elisp" nil nil nil "/home/orion/.config/doom/snippets/snippets/org-mode/elisp" nil "elisp")
                       ("dot" "#+begin_src dot :file ${1:file}.${2:svg} :results file graphics\n`%`$0\n#+end_src" "dot" nil nil nil "/home/orion/.config/doom/snippets/snippets/org-mode/dot" nil "dot")
                       ("【【" "[[$0]]\n" "chinese【" nil nil
                        ((yas-wrap-around-region
                          (ignore-errors
                            (fcitx--deactivate)))
                         (yas-after-exit-snippet-hook
                          (lambda nil
                            (ignore-errors
                              (fcitx--activate)))))
                        "/home/orion/.config/doom/snippets/snippets/org-mode/chinese_link" nil "chinese_link")
                       ("￥￥" "\\$$1\\$ $0\n" "Chinese$" nil nil
                        ((yas-wrap-around-region
                          (ignore-errors
                            (fcitx--deactivate)))
                         (yas-after-exit-snippet-hook
                          (lambda nil
                            (ignore-errors
                              (fcitx--activate)))))
                        "/home/orion/.config/doom/snippets/snippets/org-mode/chinese_dollar" nil "chinese_dollar")
                       ("blog" "#+startup: showall indent\n#+startup: hidestars\n#+begin_html\n---\nlayout: default\ntitle: ${1:title}\nexcerpt: ${2:excerpt}\n---\n$0" "blog" nil nil nil "/home/orion/.config/doom/snippets/snippets/org-mode/blog" nil "blog")
                       ("<v"
                        (progn
                          (doom-snippets-expand :uuid "verse"))
                        "#+begin_verse block" nil nil nil "/home/orion/.config/doom/snippets/snippets/org-mode/begin_verse" nil nil)
                       ("<s"
                        (progn
                          (doom-snippets-expand :uuid "src"))
                        "#+begin_src ... block" nil nil nil "/home/orion/.config/doom/snippets/snippets/org-mode/begin_src" nil nil)
                       ("<q"
                        (progn
                          (doom-snippets-expand :uuid "quote"))
                        "#+begin_quote block" nil nil nil "/home/orion/.config/doom/snippets/snippets/org-mode/begin_quote" nil "<q")
                       ("<l" "#+begin_export latex\n`%`$0\n#+end_export\n" "#+begin_export latex block" nil nil nil "/home/orion/.config/doom/snippets/snippets/org-mode/begin_export_latex" nil "<l")
                       ("<h" "#+begin_export html\n`%`$0\n#+end_export\n" "#+begin_export html block" nil nil nil "/home/orion/.config/doom/snippets/snippets/org-mode/begin_export_html" nil "<h")
                       ("<a" "#+begin_export ascii\n`%`$0\n#+end_export\n" "#+begin_export ascii" nil nil nil "/home/orion/.config/doom/snippets/snippets/org-mode/begin_export_ascii" nil "<a")
                       ("<E"
                        (progn
                          (doom-snippets-expand :uuid "export"))
                        "#+begin_export ... block" nil nil nil "/home/orion/.config/doom/snippets/snippets/org-mode/begin_export" nil nil)
                       ("<e" "#+begin_example\n`%`$0\n#+end_example\n" "#+begin_example block" nil nil nil "/home/orion/.config/doom/snippets/snippets/org-mode/begin_example" nil "<e")
                       ("<c" "#+begin_comment\n`%`$0\n#+end_comment\n" "#+begin_comment block" nil nil nil "/home/orion/.config/doom/snippets/snippets/org-mode/begin_comment" nil "<c")
                       ("<C" "#+begin_center\n`%`$0\n#+end_center\n" "#+begin_center block" nil nil nil "/home/orion/.config/doom/snippets/snippets/org-mode/begin_center" nil "<C")
                       ("begin" "#+begin_${1:type} ${2:options}\n`%`$0\n#+end_$1" "begin" nil nil nil "/home/orion/.config/doom/snippets/snippets/org-mode/begin" nil "begin")))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("while" "while (${1:condition}) {\n    `%`$0\n}\n" "while loop" nil nil nil "/home/orion/.config/doom/snippets/snippets/php-mode/while" nil nil)
                       ("dump" "var_dump(`%`$1);" "var_dump(...)" nil nil nil "/home/orion/.config/doom/snippets/snippets/php-mode/var_dump" nil "dump")
                       ("urle" "urlencode(${1:`%`})$0" "urlencode(...)" nil nil nil "/home/orion/.config/doom/snippets/snippets/php-mode/urlencode" nil "urle")
                       ("urld" "urldecode(${1:`%`})$0" "urldecode(...)" nil nil nil "/home/orion/.config/doom/snippets/snippets/php-mode/urldecode" nil "urld")
                       ("thi" "\\$this->$0" "$this->..." nil nil nil "/home/orion/.config/doom/snippets/snippets/php-mode/this" nil "thi")
                       ("switch" "switch (${on}) {\n    $0\n}" "switch (...) {...}" nil nil nil "/home/orion/.config/doom/snippets/snippets/php-mode/switch" nil nil)
                       ("p=" "<?=`%`$0 ?>" "<?= ... ?>" nil nil nil "/home/orion/.config/doom/snippets/snippets/php-mode/shorttag-print" nil "p=")
                       ("#!" "#!/usr/bin/env php\n<?php\n\n$0" "#!/usr/bin/env php" nil nil nil "/home/orion/.config/doom/snippets/snippets/php-mode/shebang" nil "#!")
                       ("sel" "self::$0" "self::..." nil nil nil "/home/orion/.config/doom/snippets/snippets/php-mode/self" nil "sel")
                       ("reqo" "require_once(\"${1:...}\");" "require_once(\"...\");" nil nil nil "/home/orion/.config/doom/snippets/snippets/php-mode/require_once" nil "reqo")
                       ("req" "require(\"${1:...}\");" "require(\"...\");" nil nil nil "/home/orion/.config/doom/snippets/snippets/php-mode/require" nil "req")
                       ("/**" "/**\n * ${0:`(if % (s-replace \"\\n\" \"\\n * \" %))`}\n */" "/** ... */" nil nil nil "/home/orion/.config/doom/snippets/snippets/php-mode/phpdoc" nil "/**")
                       ("php" "<?php $0 ?>" "<?php ... ?>" nil nil nil "/home/orion/.config/doom/snippets/snippets/php-mode/php" nil "php")
                       ("par" "parent::$0" "parent::..." nil nil nil "/home/orion/.config/doom/snippets/snippets/php-mode/parent" nil "par")
                       ("->" "\\$${1:obj_name}->${2:var}" "$o->..." nil nil nil "/home/orion/.config/doom/snippets/snippets/php-mode/object-accessor" nil "->")
                       ("met" "${1:public} function ${2:name}($3)\n{\n    `%`$0\n}" "class method" nil nil nil "/home/orion/.config/doom/snippets/snippets/php-mode/method" nil "met")
                       ("inco" "include_once(\"${1:...}\");" "include_once(\"...\");" nil nil nil "/home/orion/.config/doom/snippets/snippets/php-mode/include_once" nil "inco")
                       ("inc" "include(\"${1:...}\");" "include(\"...\");" nil nil nil "/home/orion/.config/doom/snippets/snippets/php-mode/include" nil "inc")
                       ("if" "if ($1) {\n    `%`$0\n}" "if (...) { ... }" nil nil nil "/home/orion/.config/doom/snippets/snippets/php-mode/if" nil "if")
                       ("fu" "function ${1:name}($2) {\n    `%`$0\n}" "function" nil nil nil "/home/orion/.config/doom/snippets/snippets/php-mode/function" nil "fu")
                       ("fori" "for (\\$i = ${1:0}; \\$i < ${2:10}; ++\\$i) {\n    `%`$0\n}\n" "for loop w/ $i" nil nil nil "/home/orion/.config/doom/snippets/snippets/php-mode/fori" nil "fori")
                       ("fore" "foreach (${1:collection} as ${2:var}) {\n    `%`$0\n}" "foreach (...) {...}" nil nil nil "/home/orion/.config/doom/snippets/snippets/php-mode/foreach" nil "fore")
                       ("for" "for ($1;$2;$3) {\n    `%`$0\n}" "for loop" nil nil nil "/home/orion/.config/doom/snippets/snippets/php-mode/for" nil "for")
                       ("eli" "elseif ($1) {\n    `%`$0\n}" "elseif (...) { ... }" nil nil nil "/home/orion/.config/doom/snippets/snippets/php-mode/elseif" nil "eli")
                       ("el" "else {\n    `%`$0\n}" "else { ... }" nil nil nil "/home/orion/.config/doom/snippets/snippets/php-mode/else" nil "el")
                       ("e" "echo `%`$0;" "echo ...;" nil nil nil "/home/orion/.config/doom/snippets/snippets/php-mode/echo" nil "e")
                       ("de" "define(\"${1:CONSTANT}\", ${2:`%`value});" "define(..., ...)" nil nil nil "/home/orion/.config/doom/snippets/snippets/php-mode/define" nil "de")
                       ("co" "continue;" "continue;" nil nil nil "/home/orion/.config/doom/snippets/snippets/php-mode/continue" nil "co")
                       ("cl" "class ${1:Name}${2: extends ${3:Parent}}\n{\n    `%`$0\n}" "PHP class" nil nil nil "/home/orion/.config/doom/snippets/snippets/php-mode/class" nil "cl")
                       ("br" "break;" "break;" nil nil nil "/home/orion/.config/doom/snippets/snippets/php-mode/break" nil "br")
                       ("=" "\\$${1:var_name} = `%`$0;" "$var = value;" nil nil nil "/home/orion/.config/doom/snippets/snippets/php-mode/assignment" nil "=")
                       ("arr" "array(`%`$0)" "array(...)" nil nil nil "/home/orion/.config/doom/snippets/snippets/php-mode/array" nil "arr")
                       ("acl" "abstract class ${1:Name}${2: extends ${3:Parent}}\n{\n    `%`$0\n}\n" "abstract class" nil nil nil "/home/orion/.config/doom/snippets/snippets/php-mode/abstract-class" nil "acl")))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("while" "while (${1:condition}) is (${2:Yes})\n    $0\nendwhile" "while (...)" nil nil nil "/home/orion/.config/doom/snippets/snippets/plantuml-mode/while" nil nil)
                       ("vertif" "!pragma useVerticalIf on\n" "!pragma useVerticalIf on" nil nil nil "/home/orion/.config/doom/snippets/snippets/plantuml-mode/vertif" nil nil)
                       ("uml" "@startuml\n$0\n@enduml" "start and end uml" nil nil nil "/home/orion/.config/doom/snippets/snippets/plantuml-mode/uml" nil nil)
                       ("u" "${1:Down} -up${3:-}> ${2:Up}\n" "Down -up-> Up" nil nil nil "/home/orion/.config/doom/snippets/snippets/plantuml-mode/u" nil nil)
                       ("to" "${1:From} ${2:--} ${3:To}" "From - To" nil nil nil "/home/orion/.config/doom/snippets/snippets/plantuml-mode/to" nil nil)
                       ("trigger-key" "${1:From} -[${2:hidden}]${4:-}> ${3:To}\n\n" "From -[attr]-> To" t nil nil "/home/orion/.config/doom/snippets/snippets/plantuml-mode/ta" nil nil)
                       ("trigger-key" "<style>\n    $0\n</style\n" "style tag" t nil nil "/home/orion/.config/doom/snippets/snippets/plantuml-mode/sty" nil nil)
                       ("storage" "storage ${1:Name} ${2:}{\n    $0\n}" "storage ... {...}" nil nil nil "/home/orion/.config/doom/snippets/snippets/plantuml-mode/storage" nil nil)
                       ("stack" "stack ${1:Name} ${2:}{\n    $0\n}\n" "stack ... {...}" nil nil nil "/home/orion/.config/doom/snippets/snippets/plantuml-mode/stack" nil nil)
                       ("st" "state ${1:Name} ${2:}{\n    $0\n}\n\n" "state ... {...}" nil nil nil "/home/orion/.config/doom/snippets/snippets/plantuml-mode/st" nil nil)
                       ("spla" "split again\n    $0\n" "split again" nil nil nil "/home/orion/.config/doom/snippets/snippets/plantuml-mode/spla" nil nil)
                       ("spl" "split\n    $0\nend split\n" "split" nil nil nil "/home/orion/.config/doom/snippets/snippets/plantuml-mode/spl" nil nil)
                       ("sp" "skinparam ${1:param} {\n    $0\n}\n" "skinparam" nil nil nil "/home/orion/.config/doom/snippets/snippets/plantuml-mode/sp" nil nil)
                       ("sns" "set namespaceSeparator ${0:::}\n" "set namespaceSeparator ..." nil nil nil "/home/orion/.config/doom/snippets/snippets/plantuml-mode/sns" nil nil)
                       ("size" "<size:${1:}>$0</size>\n" "size tag" nil nil nil "/home/orion/.config/doom/snippets/snippets/plantuml-mode/size" nil nil)
                       ("s16" "sprite $${1:Name} [16x16/16] {\nFFFFFFFFFFFFFFFF\nFFFFFFFFFFFFFFFF\nFFFFFFFFFFFFFFFF\nFFFFFFFFFFFFFFFF\nFFFFFFFFFFFFFFFF\nFFFFFFFFFFFFFFFF\nFFFFFFFFFFFFFFFF\nFFFFFFFFFFFFFFFF\nFFFFFFFFFFFFFFFF\nFFFFFFFFFFFFFFFF\nFFFFFFFFFFFFFFFF\nFFFFFFFFFFFFFFFF\nFFFFFFFFFFFFFFFF\nFFFFFFFFFFFFFFFF\nFFFFFFFFFFFFFFFF\nFFFFFFFFFFFFFFFF\n}\n" "16x16 sprite" nil nil nil "/home/orion/.config/doom/snippets/snippets/plantuml-mode/s16" nil nil)
                       ("s" "{static} ${0:Name}\n" "static" nil nil nil "/home/orion/.config/doom/snippets/snippets/plantuml-mode/s" nil nil)
                       ("repeat" "repeat\n    $0\nrepeat while (${1:condition}) is (${2:Yes})\n" "repeat while (...)" nil nil nil "/home/orion/.config/doom/snippets/snippets/plantuml-mode/repeat" nil nil)
                       ("rect" "rectangle ${1:Name} ${2:}{\n    $0\n}\n" "rectangle ... {...}" nil nil nil "/home/orion/.config/doom/snippets/snippets/plantuml-mode/rect" nil nil)
                       ("r" "${1:Left} -right${3:-}> ${2:Right}\n" "Left -right-> Right" nil nil nil "/home/orion/.config/doom/snippets/snippets/plantuml-mode/r" nil nil)
                       ("q" "queue ${1:Name} ${2:}{\n    $0\n}\n" "queue ... {...}" nil nil nil "/home/orion/.config/doom/snippets/snippets/plantuml-mode/q" nil nil)
                       ("public" "+ $0\n" "public bullet" nil nil nil "/home/orion/.config/doom/snippets/snippets/plantuml-mode/public" nil nil)
                       ("protected" "# $0\n" "protected" nil nil nil "/home/orion/.config/doom/snippets/snippets/plantuml-mode/protected" nil nil)
                       ("private" "- $0\n" "private" nil nil nil "/home/orion/.config/doom/snippets/snippets/plantuml-mode/private" nil nil)
                       ("pprivate" "~ $0\n" "package private" nil nil nil "/home/orion/.config/doom/snippets/snippets/plantuml-mode/pprivate" nil nil)
                       ("pkg" "package ${1:Name} <<${2:Rectangle}>> ${3:}{\n    $0\n}" "package ... {...}" nil nil nil "/home/orion/.config/doom/snippets/snippets/plantuml-mode/pkg" nil nil)
                       ("ns" "namespace ${1:Name} ${2:}{\n   $0\n}\n" "namespace ... {...}" nil nil nil "/home/orion/.config/doom/snippets/snippets/plantuml-mode/ns" nil nil)
                       ("note" "note ${1:}\n    $0\nend note" "note" nil nil nil "/home/orion/.config/doom/snippets/snippets/plantuml-mode/note" nil nil)
                       ("node" "node ${1:Name} ${2:}{\n    $0\n}\n" "node ... {...}" nil nil nil "/home/orion/.config/doom/snippets/snippets/plantuml-mode/node" nil nil)
                       ("mm" "@startmindmap\n$0\n@endmindmap\n" "start and end a mindmap" nil nil nil "/home/orion/.config/doom/snippets/snippets/plantuml-mode/mm" nil nil)
                       ("math" "<math>$0</math>\n" "math tag" nil nil nil "/home/orion/.config/doom/snippets/snippets/plantuml-mode/math" nil nil)
                       ("map" "map ${1:Name} ${2:}{\n    $0\n}\n" "map ... {...}" nil nil nil "/home/orion/.config/doom/snippets/snippets/plantuml-mode/map" nil nil)
                       ("m" "{method} ${0:Name}\n" "method" nil nil nil "/home/orion/.config/doom/snippets/snippets/plantuml-mode/m" nil nil)
                       ("latex" "<latex>$0</latex>\n" "latex tag" nil nil nil "/home/orion/.config/doom/snippets/snippets/plantuml-mode/latex" nil nil)
                       ("l" "${1:Right} -left${3:-}> ${2:Left}\n" "Right -left-> Left" nil nil nil "/home/orion/.config/doom/snippets/snippets/plantuml-mode/l" nil nil)
                       ("if" "if (${1:condition}) is (${2:Yes})\n   $0\nendif\n" "if (...) is (...) ... end" nil nil nil "/home/orion/.config/doom/snippets/snippets/plantuml-mode/if" nil nil)
                       ("frame" "frame ${1:Name} ${2:}{\n    $0\n}\n\n" "frame" nil nil nil "/home/orion/.config/doom/snippets/snippets/plantuml-mode/frame" nil nil)
                       ("fork" "fork\n    $0\nend fork\n" "fork" nil nil nil "/home/orion/.config/doom/snippets/snippets/plantuml-mode/fork" nil nil)
                       ("folder" "folder ${1:Name} ${2:}{\n    $0\n}\n" "folder ... {...}" nil nil nil "/home/orion/.config/doom/snippets/snippets/plantuml-mode/folder" nil nil)
                       ("file" "file ${1:Name} ${2:}{\n    $0\n}\n" "file ... {...}" nil nil nil "/home/orion/.config/doom/snippets/snippets/plantuml-mode/file" nil nil)
                       ("fark" "fork again\n    $0\n" "fork again" nil nil nil "/home/orion/.config/doom/snippets/snippets/plantuml-mode/fark" nil nil)
                       ("f" "{field} ${0:Name}" "{field}" nil nil nil "/home/orion/.config/doom/snippets/snippets/plantuml-mode/f" nil nil)
                       ("else" "else ${1:(No)}\n    $0\n" "else (...)" nil nil nil "/home/orion/.config/doom/snippets/snippets/plantuml-mode/else" nil nil)
                       ("elif" "elseif (${1:condition}) is (${2:Yes})\n    $0\n" "elseif (...) is (...) ..." nil nil nil "/home/orion/.config/doom/snippets/snippets/plantuml-mode/elif" nil nil)
                       ("e" "entity ${1:Name} {\n    id : integer <<generated>>\n    --\n    $0\n}" "entity" nil nil nil "/home/orion/.config/doom/snippets/snippets/plantuml-mode/e" nil nil)
                       ("db" "database ${1:Name} ${2:}{\n    $0\n}\n" "database ... {...}" nil nil nil "/home/orion/.config/doom/snippets/snippets/plantuml-mode/db" nil nil)
                       ("d" "${1:Up} -down${3:-}> ${2:Down}\n" "Up -down-> Down" nil nil nil "/home/orion/.config/doom/snippets/snippets/plantuml-mode/d" nil nil)
                       ("comp" "component ${1:Name} ${2:}{\n    $0\n}\n" "component ... {...}" nil nil nil "/home/orion/.config/doom/snippets/snippets/plantuml-mode/comp" nil nil)
                       ("cls" "class ${1:Name} {\n    $0\n}\n" "class ... {...}" nil nil nil "/home/orion/.config/doom/snippets/snippets/plantuml-mode/cls" nil nil)
                       ("clr" "<color:$1>$0\n" "color tag" nil nil nil "/home/orion/.config/doom/snippets/snippets/plantuml-mode/clr" nil nil)
                       ("cloud" "cloud ${1:Name} ${2:}{\n    $0\n}\n" "cloud ... {...}" nil nil nil "/home/orion/.config/doom/snippets/snippets/plantuml-mode/cloud" nil nil)
                       ("card" "card ${1:Name} ${2:}{\n    $0\n}\n" "card ... {...}" nil nil nil "/home/orion/.config/doom/snippets/snippets/plantuml-mode/card" nil nil)
                       ("art" "artifact ${1:Name} ${2:}{\n    $0\n}\n" "artifact ... {...}" nil nil nil "/home/orion/.config/doom/snippets/snippets/plantuml-mode/art" nil nil)
                       ("a" "{abstract} $0\n" "{abstract}" nil nil nil "/home/orion/.config/doom/snippets/snippets/plantuml-mode/a" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("time" "year()$0\n" "year" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/year" nil "time")
                       ("while" "while ($1){\n  $0\n}\n" "while" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/while" nil "while")
                       ("vertex" "vertex(${1:x}, ${2:y}, ${3:z}${6:, ${4:u}, ${5:v}});\n" "vertex 3D" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/vertex_3D" nil "vertex")
                       ("vertex" "vertex(${1:x}, ${2:y}${5:, ${3:u}, ${4:v}});\n" "vertex" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/vertex" nil "vertex")
                       ("var" "${1:Object} ${2:o}${4: = new ${1}($3)};\n" "var object" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/var_object" nil "var")
                       ("var" "${1:String} ${2:str}${4: = ${3:value};\n" "var" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/var" nil "var")
                       ("updatePixels" "updatePixels();\n" "updatePixels" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/updatePixels" nil "updatePixels")
                       ("unhex" "unhex(${3:c});\n" "unhex" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/unhex" nil "unhex")
                       ("unbinary" "unbinary(${3:\"${1:str}\"});\n" "unbinary" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/unbinary" nil "unbinary")
                       ("try" "try {\n  $1\n} catch (${2:Exception} e) {\n  $3\n} finally {\n  $4\n}\n" "try..catch..finally" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/try__catch__finally" nil "try")
                       ("try" "try {\n  $1\n} catch (${2:Exception} e) {\n  $3\n}\n" "try..catch" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/try__catch" nil "try")
                       ("try" "try {\n  $1\n}\n" "try" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/try" nil "try")
                       ("trim" "trim(${3:str});\n" "trim" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/trim" nil "trim")
                       ("triangle" "triangle(${1:x1}, ${2:y1}, ${3:x2}, ${4:y2}, ${5:x3}, ${6:y3});\n" "triangle" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/triangle" nil "triangle")
                       ("translate" "translate(${1:x}, ${2:y}${4:, ${3:z}});\n" "translate" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/translate" nil "translate")
                       ("tint" "tint(${8:${3:value1}, ${4:value2}, ${5:value3}${7:, ${6:alpha}}});\n" "tint" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/tint" nil "tint")
                       ("throw" "throw new Exception(\"${1:Name}\");\n" "throw" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/throw" nil "throw")
                       ("text" "text(${1:stringdata}, ${2:x}, ${3:y}, ${4:width}, ${5:height}${7:, ${6:z}});\n" "text stringdata" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/text_stringdata" nil "text")
                       ("text" "text(${1:data}, ${2:x}, ${3:y}${5:, ${4:z}});\n" "text data" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/text_data" nil "text")
                       ("text" "textWidth(${1:data});\n" "textWidth" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/textWidth" nil "text")
                       ("text" "textSize(${1:size});\n" "textSize" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/textSize" nil "text")
                       ("text" "textLeading(${1:size});\n" "textLeading" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/textLeading" nil "text")
                       ("text" "textFont(${1:font}${7:, ${6:size}});\n" "textFont" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/textFont" nil "text")
                       ("text" "textDescent();\n" "textDescent" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/textDescent" nil "text")
                       ("text" "textAscent();\n" "textAscent" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/textAscent" nil "text")
                       ("tan" "tan(${1:rad});\n" "tan" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/tan" nil "tan")
                       ("switch" "switch ($1){\n  $0\n}\n" "switch" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/switch" nil "switch")
                       ("subset" "subset(${1:array}, ${2:offset});\n" "subset" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/subset" nil "subset")
                       ("stroke" "strokeWeight(${1:1});\n" "strokeWeight" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/strokeWeight" nil "stroke")
                       ("stroke" "stroke(${8:${3:value1}, ${4:value2}, ${5:value3}${7:, ${6:alpha}}});\n" "stroke" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/stroke" nil "stroke")
                       ("str" "str(${3:\"${1:str}\"});\n" "str" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/str" nil "str")
                       ("status" "status(${1:text});\n" "status" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/status" nil "status")
                       ("sqrt" "sqrt(${1:value});\n" "sqrt" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/sqrt" nil "sqrt")
                       ("sq" "sq(${1:value});\n" "sq" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/sq" nil "sq")
                       ("light" "spotLight(${1:v1}, ${2:v2}, ${3:v3}, ${4:x}, ${5:y}, ${6:z}, ${7:nx}, ${8:ny}, ${9:nz}, ${10:angle}, ${11:concentration});\n" "spotLight" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/spotLight" nil "light")
                       ("split" "splitTokens(${3:str}${5:, ${4:tokens}});\n" "splitTokens" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/splitTokens" nil "split")
                       ("split" "split(${3:str}, ${4:delimiter});\n" "split" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/split" nil "split")
                       ("splice" "splice(${1:array}, ${2:value/array2}, ${3:index});\n" "splice" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/splice" nil "splice")
                       ("sphere" "sphereDetail(${1:n});\n" "sphereDetail" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/sphereDetail" nil "sphere")
                       ("sphere" "sphere(${1:radius});\n" "sphere" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/sphere" nil "sphere")
                       ("material" "specular(${8:${3:value1}, ${4:value2}, ${5:value3}${7:, ${6:alpha}}});\n" "specular" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/specular" nil "material")
                       ("sort" "sort(${1:dataArray}${3:, ${2:count}});\n" "sort" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/sort" nil "sort")
                       ("smooth" "smooth();\n" "smooth" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/smooth" nil "smooth")
                       ("size" "size(${1:200}, ${2:200}${3:, OPENGL});\n" "size OPENGL" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/size_OPENGL" nil "size")
                       ("size" "size(${1:512}, ${2:512});\n" "size" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/size" nil "size")
                       ("sin" "sin(${1:rad});\n" "sin" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/sin" nil "sin")
                       ("shorten" "shorten(${1:array});\n" "shorten" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/shorten" nil "shorten")
                       ("material" "shininess(${1:shine});\n" "shininess" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/shininess" nil "material")
                       ("setup" "import processing.opengl.*;\nimport javax.media.opengl.*;\n\nPGraphicsOpenGL pgl;\nGL gl;\n\nvoid setup(){\n    size( ${1:300}, ${2:300}, OPENGL );\n    colorMode( RGB, 1.0 );\n    hint( ENABLE_OPENGL_4X_SMOOTH );\n    pgl = (PGraphicsOpenGL) g;\n    gl = pgl.gl;\n    gl.setSwapInterval(1);\n    initGL();\n    $3\n}\n\nvoid draw(){\n    pgl.beginGL();\n    $4\n    pgl.endGL();\n    getOpenGLErrors();\n}\n\nvoid initGL(){\n    $5\n}\n\nvoid getOpenGLErrors(){\n  int error = gl.glGetError();\n  switch (error){\n    case 1280 :\n      println(\"GL_INVALID_ENUM - An invalid enumerant was passed to an OpenGL command.\");\n    break;\n    case 1282 :\n      println(\"GL_INVALID_OPERATION - An OpenGL command was issued that was invalid or inappropriate for the current state.\");\n    break;\n    case 1281 :\n      println(\"GL_INVALID_VALUE - A value was passed to OpenGL that was outside the allowed range.\");\n    break;\n    case 1285 :\n      println(\"GL_OUT_OF_MEMORY - OpenGL was unable to allocate enough memory to process a command.\");\n    break;\n    case 1283 :\n      println(\"GL_STACK_OVERFLOW - A command caused an OpenGL stack to overflow.\");\n    break;\n    case 1284 :\n      println(\"GL_STACK_UNDERFLOW - A command caused an OpenGL stack to underflow.\");\n    break;\n    case 32817 :\n      println(\"GL_TABLE_TOO_LARGE\");\n    break;\n  }\n}\n" "setup OpenGL" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/setup_OpenGL" nil "setup")
                       ("setup" "void setup(){\n  $1\n}\n\nvoid draw(){\n  $0\n}\n" "setup" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/setup" nil "setup")
                       ("set" "set(${1:x}, ${2:y}, ${3:color/image});\n" "set pixel" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/set_pixel" nil "set")
                       ("glSwapInterval" "// specify the minimum swap interval for buffer swaps.\ngl.setSwapInterval(${1:interval});\n" "setSwapInterval" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/setSwapInterval" nil "glSwapInterval")
                       ("set" "public void set${1:varname}(${2:String} new${3:}) {\n    ${1:fieldName} = new${1:};\n}\n" "set" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/set" nil "set")
                       ("time" "second()$0\n" "second" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/second" nil "time")
                       ("screen" "screen.width$0\n" "screen.width" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/screen_width" nil "screen")
                       ("screen" "screen.height$0\n" "screen.height" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/screen_height" nil "screen")
                       ("coordinates" "screenZ(${1:x}, ${2:y}, ${3:z});\n" "screenZ" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/screenZ" nil "coordinates")
                       ("coordinates" "screenY(${1:x}, ${2:y}, ${3:z});\n" "screenY" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/screenY" nil "coordinates")
                       ("coordinates" "screenX(${1:x}, ${2:y}, ${3:z});\n" "screenX" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/screenX" nil "coordinates")
                       ("scale" "scale(${1:size});\n" "scale SIZE" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/scale_SIZE" nil "scale")
                       ("scale" "scale(${1:x}, ${2:y}${4:, ${3:z}});\n" "scale" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/scale" nil "scale")
                       ("file" "saveStrings(${1:filename}, ${2:strings});\n" "saveStrings" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/saveStrings" nil "file")
                       ("save" "saveFrame(${2:\"${1:filename-####.ext}\"});\n" "saveFrame" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/saveFrame" nil "save")
                       ("file" "saveBytes(${1:filename}, ${2:bytes});\n" "saveBytes" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/saveBytes" nil "file")
                       ("save" "saveFrame(${2:\"${1:filename}\"});\n" "saveFrame" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/save" nil "save")
                       ("saturation" "saturation(${1:color});\n" "saturation" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/saturation" nil "saturation")
                       ("round" "round(${1:value});\n" "round" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/round" nil "round")
                       ("rotateZ" "rotateZ(${1:angle});\n" "rotateZ" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/rotateZ" nil "rotateZ")
                       ("rotateY" "rotateY(${1:angle});\n" "rotateY" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/rotateY" nil "rotateY")
                       ("rotateX" "rotateX(${1:angle});\n" "rotateX" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/rotateX" nil "rotateX")
                       ("rotate" "rotate(${1:angle});\n" "rotate" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/rotate" nil "rotate")
                       ("reverse" "reverse(${1:array});\n" "reverse" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/reverse" nil "reverse")
                       ("matrix" "translate(${1:x}, ${2:y}, ${3:z});\n" "resetMatrix" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/resetMatrix" nil "matrix")
                       ("red" "red(${1:color});\n" "red" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/red" nil "red")
                       ("rect" "rect(${1:x}, ${2:y}, ${3:width}, ${4:height});\n" "rect" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/rect" nil "rect")
                       ("random" "randomSeed(${1:value});\n" "randomSeed" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/randomSeed" nil "random")
                       ("random" "random(${1:value1}${3:, ${2:value2}});\n" "random" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/random" nil "random")
                       ("radians" "radians(${1:deg});\n" "radians" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/radians" nil "radians")
                       ("quad" "quad(${1:x1}, ${2:y1}, ${3:x2}, ${4:y2}, ${5:x3}, ${6:y3}, ${7:x4}, ${8:y4});\n" "quad" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/quad" nil "quad")
                       ("matrix" "pushMatrix();\n${1:};\npopMatrix();\n" "pushMatrix" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/pushMatrix" nil "matrix")
                       ("public" "public ${1:Object} ${2:o}${4: = new ${1}($3)};\n" "public var object" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/public_var_object" nil "public")
                       ("public" "public ${1:String} ${2:str}${4: = ${3:value}};\n" "public var" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/public_var" nil "public")
                       ("public" "public static ${1:String} ${2:str}${4: = ${3:value}};\n" "public static var" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/public_static_var" nil "public")
                       ("public" "public static ${1:void} ${2:name}($3) {\n    $0\n}\n" "public static function" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/public_static_function" nil "public")
                       ("public" "public ${1:void} ${2:name}($3) {\n    $0\n}\n" "public function" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/public_function" nil "public")
                       ("protected" "protected ${1:Object} ${2:o}${4: = new ${1}($3)};\n" "protected var object" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/protected_var_object" nil "protected")
                       ("protected" "protected ${1:String} ${2:str}${4: = ${3:value}};\n" "protected var" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/protected_var" nil "protected")
                       ("protected" "protected ${1:void} ${2:name}($3) {\n    $0\n}\n" "protected function" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/protected_function" nil "protected")
                       ("private" "private ${1:Object} ${2:o}${4: = new ${1}($3)};\n" "private var object" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/private_var_object" nil "private")
                       ("private" "private ${1:String} ${2:str}${4: = ${3:value}};\n" "private var" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/private_var" nil "private")
                       ("private" "private static ${1:String} ${2:str}${4: = ${3:value}};\n" "private static var" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/private_static_var" nil "private")
                       ("private" "private static ${1:void} ${2:name}($3) {\n  $0\n}\n" "private static function" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/private_static_function" nil "private")
                       ("private" "private ${1:void} ${2:name}($3) {\n  $0\n}\n" "private function" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/private_function" nil "private")
                       ("println" "println(\"${1:var}: \"+${1:var});$0\n" "println var" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/println_var" nil "println")
                       ("println" "println(\"$1\");$0\n" "println" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/println" nil "println")
                       ("camera" "printProjection();\n" "printProjection" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/printProjection" nil "camera")
                       ("matrix" "printMatrix();\n" "printMatrix" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/printMatrix" nil "matrix")
                       ("camera" "printCamera();\n" "printCamera" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/printCamera" nil "camera")
                       ("pow" "pow(${1:num}, ${2:exponent});\n" "pow" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/pow" nil "pow")
                       ("light" "pointLight(${1:v1}, ${2:v2}, ${3:v3}, ${4:nx}, ${5:ny}, ${6:nz});\n" "pointLight" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/pointLight" nil "light")
                       ("point" "point(${1:x}, ${2:y}${4:, ${3:z}});\n" "point" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/point" nil "point")
                       ("mouse" "pmouseY$0\n" "pmouseY" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/pmouseY" nil "mouse")
                       ("mouse" "pmouseX$0\n" "pmouseX" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/pmouseX" nil "mouse")
                       ("pixels" "pixels[${1:index}]$0\n" "pixels" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/pixels" nil "pixels")
                       ("perspective" "perspective(${5:${1:fov}, ${2:aspect}, ${3:zNear}, ${4:zFar}});\n" "perspective" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/perspective" nil "perspective")
                       ("param" "param(${1:s});\n" "param" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/param" nil "param")
                       ("package" "/**\n *  ${1:Description}\n *\n *  @author ${2:`(user-full-name)`}\n *  @since  ${3:`(format-time-string \"%d.%m.%Y\" (current-time))`}\n */\n\npackage ${4:package};\n" "package" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/package" nil "package")
                       ("ortho" "ortho(${7:${1:left}, ${2:right}, ${3:bottom}, ${4:top}, ${5:near}, ${6:far}});\n" "ortho" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/ortho" nil "ortho")
                       ("online" "online$0\n" "online" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/online" nil "online")
                       ("normal" "normal(${1:nx}, ${2:ny}, ${3:nz});\n" "normal" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/normal" nil "normal")
                       ("norm" "norm(${1:value}, ${2:low}, ${3:high});\n" "norm" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/norm" nil "norm")
                       ("noiseSeed" "noiseSeed(${1:x});\n" "noiseSeed" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/noiseSeed" nil "noiseSeed")
                       ("noiseDetail" "noiseDetail(${1:octaves}${4:, ${3:falloff}});\n" "noiseDetail" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/noiseDetail" nil "noiseDetail")
                       ("noise" "noise(${1:x}${5:, ${2:y}${4:, ${3:z}}});\n" "noise" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/noise" nil "noise")
                       ("noTint" "noTint();\n" "noTint" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/noTint" nil "noTint")
                       ("noStroke" "noStroke();\n" "noStroke" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/noStroke" nil "noStroke")
                       ("smooth" "noSmooth();\n" "noSmooth" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/noSmooth" nil "smooth")
                       ("light" "noLights();\n" "noLights" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/noLights" nil "light")
                       ("noFill" "noFill();\n" "noFill" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/noFill" nil "noFill")
                       ("cursor" "noCursor();\n" "noCursor" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/noCursor" nil "cursor")
                       ("nfs" "nfs(${3:value}, ${4:left}${6:, ${5:right}});\n" "nfs" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/nfs" nil "nfs")
                       ("nfp" "nfp(${3:value}, ${4:left}${6:, ${5:right}});\n" "nfp" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/nfp" nil "nfp")
                       ("nfc" "nfc(${3:value}${5:, ${4:right}});\n" "nfc" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/nfc" nil "nfc")
                       ("nf" "nf(${3:value}, ${4:left}${6:, ${5:right}});\n" "nf" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/nf" nil "nf")
                       ("mouse" "mouseY$0\n" "mouseY" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/mouseY" nil "mouse")
                       ("mouse" "mouseX$0\n" "mouseX" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/mouseX" nil "mouse")
                       ("mouse" "void mouseReleased(){\n  ${1}\n}\n" "mouseReleased" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/mouseReleased" nil "mouse")
                       ("mouse" "mousePressed$0\n" "mousePressed 2" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/mousePressed2" nil "mouse")
                       ("mouse" "void mousePressed(){\n  ${1}\n}\n" "mousePressed" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/mousePressed" nil "mouse")
                       ("mouse" "void mouseMoved(){\n  ${1}\n}\n" "mouseMoved" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/mouseMoved" nil "mouse")
                       ("mouse" "void mouseDragged(){\n  ${1}\n}\n" "mouseDragged" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/mouseDragged" nil "mouse")
                       ("mouse" "mouseButton$0\n" "mouseButton" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/mosueButton" nil "mouse")
                       ("time" "month()$0\n" "month" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/month" nil "time")
                       ("coordinates" "modelZ(${1:x}, ${2:y}, ${3:z});\n" "modelZ" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/modelZ" nil "coordinates")
                       ("coordinates" "modelY(${1:x}, ${2:y}, ${3:z});\n" "modelY" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/modelY" nil "coordinates")
                       ("coordinates" "modelX(${1:x}, ${2:y}, ${3:z});\n" "modelX" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/modelX" nil "coordinates")
                       ("time" "minute()$0\n" "minute" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/minute" nil "time")
                       ("min" "min(${1:array}};\n" "min array" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/min_array" nil "min")
                       ("min" "min(${1:value1}, ${2:value2}${4:, ${3:value3}});\n" "min" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/min" nil "min")
                       ("time" "millis()$0\n" "millis" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/millis" nil "time")
                       ("max" "max(${1:array}};\n" "max array" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/max_array" nil "max")
                       ("max" "max(${1:value1}, ${2:value2}${4:, ${3:value3}});\n" "max" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/max" nil "max")
                       ("match" "match(${3:str}, ${4:regexp});\n" "match" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/match" nil "match")
                       ("map" "map(${1:value}, ${2:low1}, ${4:high1}, ${5:low2}, ${6:high2});\n" "map" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/map" nil "map")
                       ("mag" "mag(${1:a}, ${2:b}${4:, ${3:c}});\n" "mag" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/mag" nil "mag")
                       ("log" "log(${1:value});\n" "log" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/log" nil "log")
                       ("load" "loadStrings(${2:\"${1:filename}\"});\n" "loadStrings" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/loadStrings" nil "load")
                       ("loadPixels" "loadPixels();\n" "loadPixels" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/loadPixels" nil "loadPixels")
                       ("loadImage" "loadImage(${1:filename});\n" "loadImage" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/loadImage" nil "loadImage")
                       ("font" "${1:font} = loadFont(${3:\"${2:FFScala-32.vlw}\"});\n" "loadFont" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/loadFont" nil "font")
                       ("load" "loadBytes(${2:\"${1:filename}\"});\n" "loadBytes" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/loadBytes" nil "load")
                       ("link" "link(${1:url}${4:, ${3:target}});\n" "link" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/link" nil "link")
                       ("line" "line(${1:x1}, ${2:y1}, ${3:z1}, ${4:x2}, ${5:y2}, ${6:z2});\n" "line 3d" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/line_3d" nil "line")
                       ("line" "line(${1:x1}, ${2:y1}, ${3:x2}, ${4:y2});\n" "line" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/line" nil "line")
                       ("light" "lights();\n" "lights" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/lights" nil "light")
                       ("light" "lightFalloff(${1:v1}, ${2:v2}, ${3:v3});\n" "lightSpecular" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/lightSpecular" nil "light")
                       ("light" "lightFalloff(${1:constant}, ${2:linear}, ${3:quadratic});\n" "lightFalloff" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/lightFalloff" nil "light")
                       ("lerpColor" "lerpColor(${1:c1}, ${2:c2}, ${3:amt});\n" "lerpColor" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/lerpColor" nil "lerpColor")
                       ("lerp" "lerp(${1:value1}, ${2:value2}, ${3:amt});\n" "lerp" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/lerp" nil "lerp")
                       ("key" "void keyTyped(){\n  ${1}\n}\n" "keytyped" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/keyTyped" nil "key")
                       ("keyReleased" "void keyReleased(){\n  ${1}\n}\n" "keyReleased" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/keyReleased" nil "keyReleased")
                       ("key" "keyPressed$0\n" "keyPressed 2" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/keyPressed2" nil "key")
                       ("key" "void keyPressed(){\n  ${1}\n}\n" "keyPressed" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/keyPressed" nil "key")
                       ("key" "keyCode$0\n" "keyCode" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/keyCode" nil "key")
                       ("key" "key$0\n" "key" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/key" nil "key")
                       ("join" "join(${3:strgArray}, ${4:seperator});\n" "join" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/join" nil "join")
                       ("int" "int ${1:f} ${6:= ${3:0}};\n" "int" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/int" nil "int")
                       ("image" "image(${1:img}, ${2:x}, ${3:y}${6:, ${4:width}, ${5:height}});\n" "image" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/image" nil "image")
                       ("?" "? ${1:trueExpression} : ${2:falseExpression}$0\n" "if short" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/if_short" nil "?")
                       ("if" "if ($1){\n  $0\n}\n" "if" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/if" nil "if")
                       ("hue" "hue(${1:color});\n" "hue" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/hue" nil "hue")
                       ("time" "hour()$0\n" "hour" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/hour" nil "time")
                       ("hex" "hex(${3:c});\n" "hex" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/hex" nil "hex")
                       ("green" "green(${1:color});\n" "green" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/green" nil "green")
                       ("glVertex3f" "gl.glVertex3f(${1:0.0f}, ${2:0.0f}, ${3:0.0f});\n" "glVertex3f" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/glVertex3f" nil "glVertex3f")
                       ("glVertex2f" "gl.glVertex2f(${1:0.0f}, ${2:0.0f});\n" "glVertex2f" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/glVertex2f" nil "glVertex2f")
                       ("glTranslatef" "// multiply the current matrix by a translation matrix\ngl.glTranslatef(${1:x}, ${2:y}, ${3:z});\n" "glTranslatef" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/glTranslatef" nil "glTranslatef")
                       ("glTexCoord2f" "// set the current texture coordinates - 2 floats\ngl.glTexCoord2f(${1:0.0f}, ${2:0.0f});\n" "glTexCoord2f" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/glTexCoord2f" nil "glTexCoord2f")
                       ("glScalef" "// multiply the current matrix by a general scaling matrix\ngl.glScalef(${1:x}, ${2:y}, ${3:z});\n" "glScalef" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/glScalef" nil "glScalef")
                       ("glRotatef" "// rotate, x-axis, y-axis, z-axiz\ngl.glRotatef(${1:angle}, ${2:x}, ${3:y}, ${4:z});\n" "glRotatef" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/glRotatef" nil "glRotatef")
                       ("glPushMatrix" "// spush and pop the current matrix stack\ngl.glPushMatrix();\n$1\ngl.glPopMatrix();\n" "glPushMatrix" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/glPushMatrix" nil "glPushMatrix")
                       ("glLoadIdentity" "// replaces the top of the active matrix stack with the identity matrix\ngl.glLoadIdentity();\n" "glLoadIdentity" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/glLoadIdentity" nil "glLoadIdentity")
                       ("glGetError" "println(gl.glGetError());\n" "glGetError" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/glGetError" nil "glGetError")
                       ("glGenLists" "gl.glGenLists(${1:1});\n" "glGenLists" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/glGenLists" nil "glGenLists")
                       ("glGenBuffers" "// import java.nio.IntBuffer;\n// import java.nio.FloatBuffer;\n// import com.sun.opengl.util.BufferUtil;\n\n// You might need to create four buffers to store vertext data, normal data, texture coordinate data, and indices in vertex arrays\nIntBuffer bufferObjects = IntBuffer.allocate(${1:4});\ngl.glGenBuffers($1, bufferObjects);\n\nint vertexCount = ${2:3};\nint numCoordinates = ${3:3};\n// vertexCount * numCoordinates\nFloatBuffer vertices = BufferUtil.newFloatBuffer(vertexCount * numCoordinates);\nfloat[] v = {0.0f, 0.0f, 0.0f,\n             1.0f, 0.0f, 0.0f,\n             0.0f, 1.0f, 1.0f};\nvertices.put(v);\n\n// Bind the first buffer object ID for use with vertext array data\ngl.glBindBuffer(GL.GL_ARRAY_BUFFER, bufferObjects.get(0));\ngl.glBufferData(GL.GL_ARRAY_BUFFER, vertexCount * numCoordinates * BufferUtil.SIZEOF_FLOAT, vertices, GL.GL_STATIC_DRAW);\n" "glGenBuffers" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/glGenBuffers" nil "glGenBuffers")
                       ("glFlush" "// Empties buffers. Call this when all previous issues commands completed\ngl.glFlush();\n" "glFlush" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/glFlush" nil "glFlush")
                       ("glDepthMask" "// enable or disable writing into the depth buffer\ngl.glDepthMask(${1:flag});\n" "glDepthMask" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/glDepthMask" nil "glDepthMask")
                       ("glDeleteBuffers" "${3:// Parameters are the same for glGenBuffers}\ngl.glDeleteBuffers(${1:4}, ${2:bufferObjects});\n" "glDeleteBuffers" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/glDeleteBuffer" nil "glDeleteBuffers")
                       ("glColor4f" "gl.glColor4f(${1:red}, ${2:green}, ${3:blue}, ${4:alpha});\n" "glColor4f" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/glColor4f" nil "glColor4f")
                       ("glColor3f" "gl.glColor3f(${1:red}, ${2:green}, ${3:blue});\n" "glColor3f" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/glColor3f" nil "glColor3f")
                       ("glClearColor" "gl.glClearColor(${1:red}, ${2:green}, ${3:blue}, ${4:alpha});\n" "glClearColor" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/glClearColor" nil "glClearColor")
                       ("glClear" "gl.glClear(${1:GL.GL_COLOR_BUFFER_BIT}${3: | ${2:GL.GL_DEPTH_BUFFER_BIT}});\n" "glClear" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/glClear" nil "glClear")
                       ("glCallList" "// execute a display list\ngl.glCallList(${1:list});\n" "glCallList" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/glCallList" nil "glCallList")
                       ("glBindBuffer" "${2:// A buffer ID of zero unbinds a buffer object}\ngl.glBindBuffer(GL.GL_ARRAY_BUFFER, ${1:0});\n" "glBindBuffer" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/glBindBuffer" nil "glBindBuffer")
                       ("get" "get(${6:${1:x}, ${2:y}${5:, ${3:width}, ${4:height}}});\n" "get pixel" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/get_pixel" nil "get")
                       ("get" "public ${1:String} get${2:var}() {\n    return ${3:fieldName};\n}\n" "get" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/get" nil "get")
                       ("function" "${1:void} ${2:name}($3) {\n  $0\n}\n" "function" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/function" nil "function")
                       ("camera" "frustrum(${7:${1:left}, ${2:right}, ${3:bottom}, ${4:top}, ${5:near}, ${6:far}});\n" "frustum" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/frustum" nil "camera")
                       ("framerate" "frameRate\n" "frameRate 2" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/frameRate2" nil "framerate")
                       ("framerate" "frameRate($0);\n" "frameRate" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/frameRate" nil "framerate")
                       ("framerate" "frameCount\n" "frameCount" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/frameCount" nil "framerate")
                       ("for" "for (${1:Object} ${2:o} : ${3:array}){\n  $0\n}\n" "for in" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/for_in" nil "for")
                       ("for" "for (int ${1:i} = ${2:0}; ${1:i}<${3:len}; ${1:i}++){\n  $0\n}\n" "for" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/for" nil "for")
                       ("focused" "focused\n" "focused" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/focused" nil "focused")
                       ("floor" "floor(${1:value});\n" "floor" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/floor" nil "floor")
                       ("float" "float ${1:f} ${6:= ${3:0.0f}};\n" "float" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/float" nil "float")
                       ("fill" "fill(${8:${3:value1}, ${4:value2}, ${5:value3}${7:, ${6:alpha}}});\n" "fill" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/fill" nil "fill")
                       ("expand" "expand(${1:array}${3:, ${2:newSize}});\n" "expand" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/expand" nil "expand")
                       ("exp" "exp(${1:value});\n" "exp" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/exp" nil "exp")
                       ("file" "endRecord();\n" "endRecord" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/endRecord" nil "file")
                       ("camera" "endCamera();\n" "endCamera" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/endCamera" nil "camera")
                       ("material" "emissive(${8:${3:value1}, ${4:value2}, ${5:value3}});\n" "emissive" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/emissive" nil "material")
                       ("else" "else if ($1) {\n  $0\n}\n" "else if" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/else_if" nil "else")
                       ("else" "else {\n  $0\n}\n" "else" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/else" nil "else")
                       ("ellipseMode" "ellipseMode(${1:CENTER});\n" "ellipseMode" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/ellipseMode" nil "ellipseMode")
                       ("ellipse" "ellipse(${1:x}, ${2:y}, ${3:width}, ${4:height});\n" "ellipse" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/ellipse" nil "ellipse")
                       ("doc" "*\n" "doc - newline" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/doc_newline" nil "doc")
                       ("doc" "/**\n *  ${1:@private}$0\n */\n" "doc - coment" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/doc_comment" nil "doc")
                       ("doc" "/**\n *  ${1:Description}\n *\n *  @author ${2:`(user-full-name)`}\n *  @since  ${3:`(format-time-string \"%d.%m.%Y\" (current-time))`}\n */\n" "doc - class" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/doc_class" nil "doc")
                       ("dist" "dist(${1:x1}, ${2:y1}, ${3:z1}, ${4:x2}, ${5:y2}, ${6:z2});\n" "dist3D" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/dist_3d" nil "dist")
                       ("dist" "dist(${1:x1}, ${2:y1}, ${4:x2}, ${5:y2});\n" "dist" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/dist" nil "dist")
                       ("light" "directionalLight(${1:v1}, ${2:v2}, ${3:v3}, ${4:nx}, ${5:ny}, ${6:nz});\n" "directionalLight" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/directionalLight" nil "light")
                       ("degrees" "degrees(${1:rad});\n" "degrees" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/degrees" nil "degrees")
                       ("default" "default:\n  $0\nbreak;\n" "default" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/default" nil "default")
                       ("curve" "curve(${1:x1}, ${2:y1}, ${3:z1}, ${4:x2}, ${5:y2}, ${6:z2}, ${7:x3}, ${8:y3}, ${9:z3}, ${10:x4}, ${11:y4}, ${12:z4});\n" "curve3D" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/curve_3d" nil "curve")
                       ("curvevertex" "curveVertex(${1:x}, ${2:y}, ${3:z});\n" "curveVertex3D" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/curveVertex_3d" nil "curvevertex")
                       ("curvevertex" "curveVertex(${1:x}, ${2:y});\n" "curveVertex" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/curveVertex" nil "curvevertex")
                       ("curve" "curveTightness(${1:squishy});\n" "curveTightness" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/curveTightness" nil "curve")
                       ("curve" "curvePoint(${1:a}, ${1:b}, ${1:c}, ${1:d}, ${1:t});\n" "curvePoint" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/curvePoint" nil "curve")
                       ("curve" "curveDetail(${1:detail});\n" "curveDetail" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/curveDetail" nil "curve")
                       ("curve" "curve(${1:x1}, ${2:y1}, ${3:x2}, ${4:y2}, ${5:x3}, ${6:y3}, ${7:x4}, ${8:y4});\n" "curve" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/curve" nil "curve")
                       ("file" "createWriter(${1:filename});\n" "createWriter" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/createWriter" nil "file")
                       ("file" "createReader(${1:filename});\n" "createReader" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/createReader" nil "file")
                       ("cos" "cos(${1:rad});\n" "cos" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/cos" nil "cos")
                       ("copy" "copy(${10:${9:srcImg}, }${1:x}, ${2:y}, ${3:width}, ${4:height}, ${5:dx}, ${6:dy}, ${7:dwidth}, ${8:dheight});\n" "copy" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/copy" nil "copy")
                       ("constrain" "constrain(${1:value}, ${2:min}, ${3:max});\n" "constrain" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/constrain" nil "constrain")
                       ("const" "static final ${1:Object} ${2:VAR_NAM} = $0;\n" "const" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/const" nil "const")
                       ("concat" "concat(${1:array1}, ${2:array2});\n" "concat" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/concat" nil "concat")
                       ("color" "color ${1:c} ${6:= color(${3:value1}, ${4:value2}, ${5:value3})};\n" "color" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/color" nil "color")
                       ("class" "${1:public }class ${2:${`(file-name-nondirectory (file-name-sans-extension (buffer-file-name)))`}} ${3:extends}\n{\n\n  //--------------------------------------\n  //  CONSTRUCTOR\n  //--------------------------------------\n\n  public $2 (${4:arguments}) {\n    ${0:// expression}\n  }\n}\n" "class" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/class" nil "class")
                       ("char" "char ${1:m} ${6:= \"${3:char}\"};\n" "char" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/char" nil "char")
                       ("ceil" "ceil(${1:value});\n" "ceil" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/ceil" nil "ceil")
                       ("catch" "catch (${1:Exception} e) {\n  $0\n}\n" "catch" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/catch" nil "catch")
                       ("case" "case ${1:expression}:\n  $0\nbreak;\n" "case" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/case" nil "case")
                       ("camera" "camera(${10:${1:eyeX}, ${2:eyeY}, ${3:eyeZ}, ${4:centerX}, ${5:centerY}, ${6:centerZ}, ${7:upX}, ${8:upY}, ${9:upZ}});\n" "camera" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/camera" nil "camera")
                       ("byte" "byte ${1:b} ${6:= ${3:127}};\n" "byte" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/byte" nil "byte")
                       ("brightness" "brightness(${1:color});\n" "brightness" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/brightness" nil "brightness")
                       ("break" "break ${1:label};\n" "break" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/break" nil "break")
                       ("box" "box(${4:${1:width}, ${2:height}, ${3:depth}});\n" "box" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/box" nil "box")
                       ("boolean" "boolean ${1:b} ${6:= ${3:true}};\n" "boolean" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/boolean" nil "boolean")
                       ("blue" "blue(${1:color});\n" "blue" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/blue" nil "blue")
                       ("binary" "binary(${3:value}${5:, ${4:digits}});\n" "binary" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/binary" nil "binary")
                       ("bezier" "bezier(${1:x1}, ${2:y1}, ${3:z1}, ${4:cx1}, ${5:cy1}, ${6:cz1}, ${7:cx2}, ${8:cy2}, ${9:cz2}, ${10:x2}, ${11:y2}, ${12:z2});\n" "bezier3D" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/bezier_3d" nil "bezier")
                       ("beziervertex" "bezierVertex(${1:cx1}, ${2:cy1}, ${3:cz1}, ${4:cx2}, ${5:cy2}, ${6:cz2}, ${7:x}, ${8:y}, ${9:z});\n" "bezierVertex3D" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/bezierVertex_3d" nil "beziervertex")
                       ("beziervertex" "bezierVertex(${1:cx1}, ${2:cy1}, ${3:cx2}, ${4:cy2}, ${5:x}, ${6:y});\n" "bezierVertex" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/bezierVertex" nil "beziervertex")
                       ("bezier" "bezierTangent(${1:a}, ${1:b}, ${1:c}, ${1:d}, ${1:t});\n" "bezierTangent" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/bezierTangent" nil "bezier")
                       ("bezier" "bezierPoint(${1:a}, ${1:b}, ${1:c}, ${1:d}, ${1:t});\n" "bezierPoint" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/bezierPoint" nil "bezier")
                       ("bezier" "bezierDetail(${1:detail});\n" "bezierDetail" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/bezierDetail" nil "bezier")
                       ("bezier" "bezier(${1:x1}, ${2:y1}, ${3:cx1}, ${4:cy1}, ${5:cx2}, ${6:cy2}, ${7:x2}, ${8:y2});\n" "bezier" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/bezier" nil "bezier")
                       ("file" "beginRecord(${1:renderer}, ${2:filename});\n" "beginRecord" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/beginRecord" nil "file")
                       ("begingl" "pgl.beginGL();\n$1\npgl.endGL();\n" "beginGL" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/beginGL" nil "begingl")
                       ("camera" "beginCamera();\n" "beginCamera" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/beginCamera" nil "camera")
                       ("background" "background(${8:${3:value1}, ${4:value2}, ${5:value3}${7:, ${6:alpha}}});\n" "background" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/background" nil "background")
                       ("atan2" "atan2(${1:y},${2:x});\n" "atan2" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/atan2" nil "atan2")
                       ("atan" "atan(${1:rad});\n" "atan" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/atan" nil "atan")
                       ("asin" "asin(${1:rad});\n" "asin" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/asin" nil "asin")
                       ("arrayCopy" "arrayCopy(${1:src}, ${2:dest}, ${4:, ${3:length}});\n" "arrayCopy" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/arrayCopy" nil "arrayCopy")
                       ("arc" "arc(${1:x}, ${2:y}, ${3:width}, ${4:height}, ${5:start}, ${6:stop});\n" "arc" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/arc" nil "arc")
                       ("for" "append(${1:array}, ${2:element});\n" "foreach" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/append" nil "for")
                       ("light" "ambientLight(${1:v1}, ${2:v2}, ${3:v3}${7:, ${4:x}, ${5:y}, ${6:z}});\n" "ambientLight" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/ambientLight" nil "light")
                       ("material" "ambient(${8:${3:value1}, ${4:value2}, ${5:value3}});\n" "ambient" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/ambient" nil "material")
                       ("alpha" "alpha(${1:color});\n" "alpha" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/alpha" nil "alpha")
                       ("acos" "acos(${1:rad});\n" "acos" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/acos" nil "acos")
                       ("abs" "abs(${1:value});\n" "abs" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/abs" nil "abs")
                       ("pi" "TWO_PI$0\n" "TWO PI" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/TWO_PI" nil "pi")
                       ("string" "String ${1:str} ${6:= \"${3:CCCP}\"};\n" "String" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/String" nil "string")
                       ("PImage" "PImage(${1:width}, ${2:height});\n" "PImage" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/PImage" nil "PImage")
                       ("pi" "PI$0\n" "PI" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/PI" nil "pi")
                       ("PGraphics" "PGraphics pg;\npg = createGraphics(${5:${1:width}, ${2:height}${4:, ${3:applet}}});\n" "PGraphics" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/PGraphics" nil "PGraphics")
                       ("font" "PFont ${1:font};\n$1 = loadFont(${3:\"${2:FFScala-32.vlw}\"});\n" "PFont" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/PFont" nil "font")
                       ("object" "${1:Object} ${2:o}${4: = new ${1}($3)};\n" "Object" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/Object" nil "object")
                       ("pi" "HALF_PI$0\n" "HALF PI" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/HALF_PI" nil "pi")
                       ("ArrayList" "ArrayList<${1:String}> ${2:arraylist} = new ArrayList<$1>();\n" "ArrayList" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/ArrayList" nil "ArrayList")
                       ("array" "${1:int}[] ${2:numbers} ${6:= new $1[${3:length}]};\n" "Array" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/Array" nil "array")
                       ("@" "@return  ${1:parameter}  ${2:description}\n" "@return" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/@return" nil "@")
                       ("@" "@public$0\n" "@public" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/@public" nil "@")
                       ("@" "@private$0\n" "@private" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/@private" nil "@")
                       ("@" "@param  ${1:parameter}  ${2:description}\n" "@param" nil nil nil "/home/orion/.config/doom/snippets/snippets/processing-mode/@param" nil "@")))


;;; contents of the .yas-setup.el support file:
;;;
;; -*- no-byte-compile: t; -*-

(require 'yasnippet)

;; whitespace removing functions from Magnar Sveen ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun yas-s-trim-left (s)
  "Remove whitespace at the beginning of S."
  (if (string-match "\\`[ \t\n\r]+" s)
      (replace-match "" t t s)
    s))

(defun yas-s-trim-right (s)
  "Remove whitespace at the end of S."
  (if (string-match "[ \t\n\r]+\\'" s)
      (replace-match "" t t s)
    s))

(defun yas-s-trim (s)
  "Remove whitespace at the beginning and end of S."
  (yas-s-trim-left (yas-s-trim-right s)))

(defun yas-string-reverse (str)
  "Reverse a string STR manually to be compatible with emacs versions < 25."
  (apply #'string
         (reverse
          (string-to-list str))))

(defun yas-trimmed-comment-start ()
  "This function returns `comment-start' trimmed by whitespaces."
  (yas-s-trim comment-start))

(defun yas-trimmed-comment-end ()
  "This function returns `comment-end' trimmed by whitespaces if `comment-end' is not empty.
Otherwise the reversed output of function `yas-trimmed-comment-start' is returned."
  (if (eq (length comment-end) 0)
      (yas-string-reverse (yas-trimmed-comment-start))
    (yas-s-trim comment-end)))
;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("---" "`(yas-trimmed-comment-start)`${1: comment }${1:$(let* ((start (yas-trimmed-comment-start))\n                                                       (lastcom (aref start (1- (length start))))\n                                                       (end (yas-trimmed-comment-end))\n                                                       (endpadlen (- 79 (+ (current-column) (length end)))))\n                                              (concat (make-string (max endpadlen 0) lastcom)\n                                                      end))}$0" "comment line" nil nil nil "/home/orion/.config/doom/snippets/snippets/prog-mode/commentline" nil "---")
                       ("cob" "${1:$(let* ((col (current-column))\n           (str \"\")\n           (lastcom (substring (yas-trimmed-comment-start) -1))\n           (start (yas-trimmed-comment-start))\n           (end (yas-trimmed-comment-end))\n           (over (- (+ (string-width yas-text) (length start) (length end) col) 77)))\n         (while (< (length str) (+ (- 79 (length start) (length end) col) (if (> over 0) over 0)))\n                (setq str (concat str lastcom)))\n       (concat start str end))}\n${1:$(let* ((col (current-column))\n           (str \"\")\n           (start (yas-trimmed-comment-start))\n           (end (yas-trimmed-comment-end)))\n         (while (< (length str) (ffloor (/ (- 78.0 (+ col (length start) (string-width yas-text) (length end))) 2.0)))\n                (setq str (concat str \" \")))\n        (concat start str))} ${1:comment} ${1:$(let* ((col (current-column))\n                                                     (str \"\")\n                                                     (start (yas-trimmed-comment-start))\n                                                     (end (yas-trimmed-comment-end)))\n                                                   (while (< (length str) (- 79.0 (if (eq (mod (string-width yas-text) 2) 1) (- col 1) col) (length end)))\n                                                          (setq str (concat str \" \")))\n                                                 (concat str end))}\n${1:$(let* ((col (current-column))\n           (str \"\")\n           (lastcom (substring (yas-trimmed-comment-start) -1))\n           (start (yas-trimmed-comment-start))\n           (end (yas-trimmed-comment-end))\n           (over (- (+ (string-width yas-text) (length start) (length end) col) 77)))\n         (while (< (length str) (+ (- 79 (length start) (length end) col) (if (> over 0) over 0)))\n                (setq str (concat str lastcom)))\n       (concat start str end))}$0\n" "commentblock" nil nil nil "/home/orion/.config/doom/snippets/snippets/prog-mode/commentblock" nil "cob")))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("link" "link(rel=\"stylesheet\", href=\"${1:stylesheet.css}\", type=\"type/css\")" "link type=\"stylesheet\"" nil nil nil "/home/orion/.config/doom/snippets/snippets/pug-mode/link" nil nil)))


;;; contents of the .yas-setup.el support file:
;;;
;; -*- no-byte-compile: t; -*-

(defun python-split-args (arg-string)
  "Split a python argument string into ((name, default)..) tuples"
  (mapcar (lambda (x)
             (split-string x "[[:blank:]]*=[[:blank:]]*" t))
          (split-string arg-string "[[:blank:]]*,[[:blank:]]*" t)))

(defun python-args-to-docstring ()
  "return docstring format for the python arguments in yas-text"
  (let* ((indent (concat "\n" (make-string (current-column) 32)))
         (args (python-split-args yas-text))
         (max-len (if args (apply 'max (mapcar (lambda (x) (length (nth 0 x))) args)) 0))
         (formatted-args (mapconcat
                (lambda (x)
                   (concat (nth 0 x) (make-string (- max-len (length (nth 0 x))) ? ) " -- "
                           (if (nth 1 x) (concat "\(default " (nth 1 x) "\)"))))
                args
                indent)))
    (unless (string= formatted-args "")
      (mapconcat 'identity (list "Keyword Arguments:" formatted-args) indent))))
;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("fw" "from __future__ import with_statement" "with_statement" nil
                        ("future")
                        nil "/home/orion/.config/doom/snippets/snippets/python-mode/with_statement" nil "fw")
                       ("with" "with ${1:expr}${2: as ${3:alias}}:\n     $0" "with" nil
                        ("control structure")
                        nil "/home/orion/.config/doom/snippets/snippets/python-mode/with" nil "with")
                       ("wh" "while ${1:True}:\n      $0" "while" nil
                        ("control structure")
                        nil "/home/orion/.config/doom/snippets/snippets/python-mode/while" nil "wh")
                       ("utf8" "# -*- coding: utf-8 -*-\n" "utf-8 encoding" nil nil nil "/home/orion/.config/doom/snippets/snippets/python-mode/utf8" nil "utf8")
                       ("un" "def __unicode__(self):\n    $0" "__unicode__" nil
                        ("dunder methods")
                        nil "/home/orion/.config/doom/snippets/snippets/python-mode/unicode" nil "un")
                       ("try" "try:\n    $1\nexcept $2:\n    $3\nelse:\n    $0" "tryelse" nil nil nil "/home/orion/.config/doom/snippets/snippets/python-mode/tryelse" nil "try")
                       ("try" "try:\n    ${1:`(or % \"pass\")`}\nexcept ${2:Exception}:\n    $0" "try" nil nil nil "/home/orion/.config/doom/snippets/snippets/python-mode/try" nil "try")
                       ("tr" "import pdb; pdb.set_trace()" "trace" nil
                        ("debug")
                        nil "/home/orion/.config/doom/snippets/snippets/python-mode/trace" nil "tr")
                       ("tf" "import unittest\n${1:from ${2:test_file} import *}\n\n$0\n\nif __name__ == '__main__':\n    unittest.main()" "test_file" nil
                        ("testing")
                        nil "/home/orion/.config/doom/snippets/snippets/python-mode/test_file" nil "tf")
                       ("tcs" "class Test${1:toTest}(${2:unittest.TestCase}):\n      $0\n" "test_class" nil
                        ("testing")
                        nil "/home/orion/.config/doom/snippets/snippets/python-mode/test_class" nil "tcs")
                       ("super" "super(${1:Class}, self).${2:function}(${3:args})" "super" nil
                        ("object oriented")
                        nil "/home/orion/.config/doom/snippets/snippets/python-mode/super" nil "super")
                       ("str" "def __str__(self):\n    $0" "__str__" nil
                        ("dunder methods")
                        nil "/home/orion/.config/doom/snippets/snippets/python-mode/str" nil "str")
                       ("defs" "@staticmethod\ndef ${1:method_name}($1):\n    $0" "static" nil nil nil "/home/orion/.config/doom/snippets/snippets/python-mode/static" nil "defs")
                       ("size" "sys.getsizeof($0)" "size" nil nil nil "/home/orion/.config/doom/snippets/snippets/python-mode/size" nil "size")
                       ("#!" "#!/usr/bin/env python\n$0" "shebang line"
                        (eq 1
                            (line-number-at-pos))
                        nil nil "/home/orion/.config/doom/snippets/snippets/python-mode/shebang" nil "#!")
                       ("setup" "from setuptools import setup\n\npackage = '${1:name}'\nversion = '${2:0.1}'\n\nsetup(name=package,\n      version=version,\n      description=\"${3:description}\",\n      url='${4:url}'$0)\n" "setup" nil
                        ("distribute")
                        nil "/home/orion/.config/doom/snippets/snippets/python-mode/setup" nil "setup")
                       ("setdef" "${1:var}.setdefault(${2:key}, []).append(${3:value})" "setdef" nil nil nil "/home/orion/.config/doom/snippets/snippets/python-mode/setdef" nil "setdef")
                       ("sn" "self.$1 = $1" "selfassign" nil
                        ("object oriented")
                        nil "/home/orion/.config/doom/snippets/snippets/python-mode/selfassign" nil "sn")
                       ("s" "self" "self_without_dot" nil
                        ("object oriented")
                        nil "/home/orion/.config/doom/snippets/snippets/python-mode/self_without_dot" nil "s")
                       ("." "self.$0" "self" nil
                        ("object oriented")
                        nil "/home/orion/.config/doom/snippets/snippets/python-mode/self" nil ".")
                       ("script" "#!/usr/bin/env python\n\ndef main():\n   pass\n\nif __name__ == '__main__':\n   main()\n" "script" nil nil nil "/home/orion/.config/doom/snippets/snippets/python-mode/script" nil "script")
                       ("r" "return $0" "return" nil nil nil "/home/orion/.config/doom/snippets/snippets/python-mode/return" nil "r")
                       ("repr" "def __repr__(self):\n    $0" "__repr__" nil
                        ("dunder methods")
                        nil "/home/orion/.config/doom/snippets/snippets/python-mode/repr" nil "repr")
                       ("reg" "${1:regexp} = re.compile(r\"${2:expr}\")\n$0" "reg" nil
                        ("general")
                        nil "/home/orion/.config/doom/snippets/snippets/python-mode/reg" nil "reg")
                       ("prop" "def ${1:foo}():\n   doc = \"\"\"${2:Doc string}\"\"\"\n   def fget(self):\n       return self._$1\n\n   def fset(self, value):\n       self._$1 = value\n\n   def fdel(self):\n       del self._$1\n   return locals()\n$1 = property(**$1())\n\n$0\n" "prop" nil nil nil "/home/orion/.config/doom/snippets/snippets/python-mode/prop" nil nil)
                       ("p" "print($0)" "print" nil nil nil "/home/orion/.config/doom/snippets/snippets/python-mode/print" nil "p")
                       ("ps" "pass" "pass" nil nil nil "/home/orion/.config/doom/snippets/snippets/python-mode/pass" nil "ps")
                       ("pars" "parser = argparse.ArgumentParser(description='$1')\n$0" "parser" nil
                        ("argparser")
                        nil "/home/orion/.config/doom/snippets/snippets/python-mode/parser" nil "pars")
                       ("pargs" "def parse_arguments():\n    parser = argparse.ArgumentParser(description='$1')\n    $0\n    return parser.parse_args()" "parse_args" nil
                        ("argparser")
                        nil "/home/orion/.config/doom/snippets/snippets/python-mode/parse_args" nil "pargs")
                       ("np" "import numpy as np\n$0" "np" nil
                        ("general")
                        nil "/home/orion/.config/doom/snippets/snippets/python-mode/np" nil "np")
                       ("not_impl" "raise NotImplementedError" "not_impl" nil nil nil "/home/orion/.config/doom/snippets/snippets/python-mode/not_impl" nil "not_impl")
                       ("defd" "def ${1:name}(self$2):\n    \\\"\\\"\\\"$3\n    ${2:$(python-args-to-docstring)}\n    \\\"\\\"\\\"\n    `%`$0" "method_docstring" nil
                        ("object oriented")
                        nil "/home/orion/.config/doom/snippets/snippets/python-mode/method_docstring" nil "defd")
                       ("method" "def ${1:method_name}(self${2:, $3}):\n    $0\n" "method" nil
                        ("object oriented")
                        nil "/home/orion/.config/doom/snippets/snippets/python-mode/method" nil nil)
                       ("mt" "__metaclass__ = type" "metaclass" nil
                        ("object oriented")
                        nil "/home/orion/.config/doom/snippets/snippets/python-mode/metaclass" nil "mt")
                       ("main" "def main():\n    $0" "main" nil nil nil "/home/orion/.config/doom/snippets/snippets/python-mode/main" nil "main")
                       ("log" "logger = logging.getLogger(\"${1:name}\")\nlogger.setLevel(logging.${2:level})\n" "logging" nil nil nil "/home/orion/.config/doom/snippets/snippets/python-mode/logging" nil "log")
                       ("ln" "logger = logging.getLogger(__name__)" "logger_name" nil nil nil "/home/orion/.config/doom/snippets/snippets/python-mode/logger_name" nil "ln")
                       ("li" "[${1:x} for $1 in ${2:list}]" "list" nil
                        ("definitions")
                        nil "/home/orion/.config/doom/snippets/snippets/python-mode/list" nil "li")
                       ("lam" "lambda ${1:x}: $0" "lambda" nil nil nil "/home/orion/.config/doom/snippets/snippets/python-mode/lambda" nil "lam")
                       ("iter" "def __iter__(self):\n    return ${1:iter($2)}" "__iter__" nil
                        ("dunder methods")
                        nil "/home/orion/.config/doom/snippets/snippets/python-mode/iter" nil "iter")
                       ("itr" "import ipdb; ipdb.set_trace()" "ipdb trace" nil
                        ("debug")
                        nil "/home/orion/.config/doom/snippets/snippets/python-mode/ipdbdebug" nil "itr")
                       ("int" "import code; code.interact(local=locals())" "interact" nil nil nil "/home/orion/.config/doom/snippets/snippets/python-mode/interact" nil "int")
                       ("initd" "def __init__(self$1):\n    \\\"\\\"\\\"$2\n    ${1:$(python-args-to-docstring)}\n    \\\"\\\"\\\"\n    $0" "init_docstring" nil
                        ("definitions")
                        nil "/home/orion/.config/doom/snippets/snippets/python-mode/init_docstring" nil "initd")
                       ("init" "def __init__(self${1:, args}):\n    $0" "init" nil
                        ("definitions")
                        nil "/home/orion/.config/doom/snippets/snippets/python-mode/init" nil "init")
                       ("imp" "import ${1:lib}${2: as ${3:alias}}\n$0" "import" nil
                        ("general")
                        nil "/home/orion/.config/doom/snippets/snippets/python-mode/import" nil "imp")
                       ("ifmain" "if __name__ == '__main__':\n    ${1:`(or % \"pass\")`}" "ifmain" nil nil nil "/home/orion/.config/doom/snippets/snippets/python-mode/ifmain" nil nil)
                       ("ife" "if $1:\n    ${2:`(or % \"pass\")`}\nelse:\n    $0" "ife" nil
                        ("control structure")
                        nil "/home/orion/.config/doom/snippets/snippets/python-mode/ife" nil "ife")
                       ("if" "if ${1:cond}:\n    ${2:`(or % \"pass\")`}" "if" nil
                        ("control structure")
                        nil "/home/orion/.config/doom/snippets/snippets/python-mode/if" nil "if")
                       ("fd" "def ${1:name}($2):\n    \\\"\\\"\\\"$3\n    ${2:$(python-args-to-docstring)}\n    \\\"\\\"\\\"\n    $0" "function_docstring" nil
                        ("definitions")
                        nil "/home/orion/.config/doom/snippets/snippets/python-mode/function_docstring" nil "fd")
                       ("from" "from ${1:lib} import ${2:funs}" "from" nil
                        ("general")
                        nil "/home/orion/.config/doom/snippets/snippets/python-mode/from" nil "from")
                       ("for" "for ${1:var} in ${2:collection}:\n    ${3:`(or % \"pass\")`}" "for ... in ... : ..." nil
                        ("control structure")
                        nil "/home/orion/.config/doom/snippets/snippets/python-mode/for" nil "for")
                       ("eq" "def __eq__(self, other):\n    return self.$1 == other.$1" "__eq__" nil
                        ("dunder methods")
                        nil "/home/orion/.config/doom/snippets/snippets/python-mode/eq" nil "eq")
                       ("doc" ">>> ${1:function calls}\n${2:desired output}\n$0" "doctest" nil
                        ("testing")
                        nil "/home/orion/.config/doom/snippets/snippets/python-mode/doctest" nil "doc")
                       ("d" "\"\"\"$0\n\"\"\"" "doc" nil nil nil "/home/orion/.config/doom/snippets/snippets/python-mode/doc" nil "d")
                       ("defm" "def ${1:method_name}(self${2:, $3}):\n    ${4:`(or % \"pass\")`}" "method" nil
                        ("object oriented")
                        nil "/home/orion/.config/doom/snippets/snippets/python-mode/defm" nil "defm")
                       ("def" "def ${1:func_name}($2):\n    ${3:`(or % \"pass\")`}" "def function" nil
                        ("definitions")
                        nil "/home/orion/.config/doom/snippets/snippets/python-mode/def" nil "def")
                       ("dec" "def ${1:decorator}(func):\n    $2\n    def _$1(*args, **kwargs):\n        $3\n        ret = func(*args, **kwargs)\n        $4\n        return ret\n\n    return _$1" "def decorator" nil
                        ("definitions")
                        nil "/home/orion/.config/doom/snippets/snippets/python-mode/dec" nil "dec")
                       ("defc" "@classmethod\ndef ${1:method_name}(cls, $1):\n    $0" "classmethod" nil
                        ("object oriented")
                        nil "/home/orion/.config/doom/snippets/snippets/python-mode/classmethod" nil "defc")
                       ("cl" "class ${1:Name}($2):\n    $0" "class" nil
                        ("object oriented")
                        nil "/home/orion/.config/doom/snippets/snippets/python-mode/class" nil "cl")
                       ("cdb" "from celery.contrib import rdb; rdb.set_trace()" "celery pdb" nil
                        ("debug")
                        nil "/home/orion/.config/doom/snippets/snippets/python-mode/celery_pdb" nil "cdb")
                       ("at" "self.assertTrue($0)" "assertTrue" nil
                        ("testing")
                        nil "/home/orion/.config/doom/snippets/snippets/python-mode/assertTrue" nil "at")
                       ("ar" "with self.assertRaises(${1:Exception}):\n     $0\n" "assertRaises" nil nil nil "/home/orion/.config/doom/snippets/snippets/python-mode/assertRaises.with" nil "ar")
                       ("assertRaises" "assertRaises(${1:Exception}, ${2:fun})" "assertRaises" nil
                        ("testing")
                        nil "/home/orion/.config/doom/snippets/snippets/python-mode/assertRaises" nil nil)
                       ("ane" "self.assertNotEqual($1, $2)" "assertNotEqual" nil
                        ("testing")
                        nil "/home/orion/.config/doom/snippets/snippets/python-mode/assertNotEqual" nil "ane")
                       ("ai" "self.assertIn(${1:member}, ${2:container})" "assertIn" nil
                        ("testing")
                        nil "/home/orion/.config/doom/snippets/snippets/python-mode/assertIn" nil "ai")
                       ("af" "self.assertFalse($0)" "assertFalse" nil
                        ("testing")
                        nil "/home/orion/.config/doom/snippets/snippets/python-mode/assertFalse" nil "af")
                       ("ae" "self.assertEqual($1, $2)" "assertEqual" nil
                        ("testing")
                        nil "/home/orion/.config/doom/snippets/snippets/python-mode/assertEqual" nil "ae")
                       ("assert" "assert $0" "assert" nil
                        ("testing")
                        nil "/home/orion/.config/doom/snippets/snippets/python-mode/assert" nil nil)
                       ("argp" "parser.add_argument('${1:varname}', $0)" "arg_positional" nil
                        ("argparser")
                        nil "/home/orion/.config/doom/snippets/snippets/python-mode/arg_positional" nil "argp")
                       ("arg" "parser.add_argument('-$1', '--$2',\n                    $0)\n" "arg" nil
                        ("argparser")
                        nil "/home/orion/.config/doom/snippets/snippets/python-mode/arg" nil "arg")
                       ("all" "__all__ = [\n        $0\n]" "all" nil nil nil "/home/orion/.config/doom/snippets/snippets/python-mode/all" nil nil)
                       ("setit" "def __setitem__(self, ${1:key}, ${2:val}):\n    $0" "__setitem__" nil
                        ("dunder methods")
                        nil "/home/orion/.config/doom/snippets/snippets/python-mode/__setitem__" nil "setit")
                       ("new" "def __new__(mcs, name, bases, dict):\n    $0\n    return type.__new__(mcs, name, bases, dict)\n" "__new__" nil
                        ("dunder methods")
                        nil "/home/orion/.config/doom/snippets/snippets/python-mode/__new__" nil "new")
                       ("len" "def __len__(self):\n    $0" "__len__" nil
                        ("dunder methods")
                        nil "/home/orion/.config/doom/snippets/snippets/python-mode/__len__" nil "len")
                       ("getit" "def __getitem__(self, ${1:key}):\n    $0" "__getitem__" nil
                        ("dunder methods")
                        nil "/home/orion/.config/doom/snippets/snippets/python-mode/__getitem__" nil "getit")
                       ("ex" "def __exit__(self, type, value, traceback):\n    $0" "__exit__" nil
                        ("dunder methods")
                        nil "/home/orion/.config/doom/snippets/snippets/python-mode/__exit__" nil "ex")
                       ("ent" "def __enter__(self):\n    $0\n\n    return self" "__enter__" nil
                        ("dunder methods")
                        nil "/home/orion/.config/doom/snippets/snippets/python-mode/__enter__" nil "ent")
                       ("cont" "def __contains__(self, el):\n    $0" "__contains__" nil
                        ("dunder methods")
                        nil "/home/orion/.config/doom/snippets/snippets/python-mode/__contains__" nil "cont")))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("when" "(when ${1:(${2:predicate})} $0)" "(when ...)" nil nil nil "/home/orion/.config/doom/snippets/snippets/racket-mode/when" nil nil)
                       ("unless" "(unless ${1:(${2:predicate})} $0)" "(unless ...)" nil nil nil "/home/orion/.config/doom/snippets/snippets/racket-mode/unless" nil nil)
                       ("match" "(match ${1:expression} [${2:clause} $0])" "(match ... [... ...]...)" nil nil nil "/home/orion/.config/doom/snippets/snippets/racket-mode/match" nil nil)
                       ("let" "(let$1 ([${2:name} ${3:expression}]$4) $0)" "(let... ([... ...]...) ...)" nil nil nil "/home/orion/.config/doom/snippets/snippets/racket-mode/let" nil nil)
                       ("lambda" "(lambda ${1:(${2:arguments})} $0)" "(lambda (...) ...)" nil nil nil "/home/orion/.config/doom/snippets/snippets/racket-mode/lambda" nil nil)
                       ("if" "(if ${1:(${2:predicate})}\n    $0)" "(if ... ... ...)" nil nil nil "/home/orion/.config/doom/snippets/snippets/racket-mode/if" nil nil)
                       ("for" "(for$1 (${2:for-clause}) $0)" "(for... (...) ...)" nil nil nil "/home/orion/.config/doom/snippets/snippets/racket-mode/for" nil nil)
                       ("do" "(do ([${1:name} ${2:init} ${3:step}]$4)\n    (${5:stop-predicate} ${6:finish})\n  $0)" "(do ([... ... ...]...) (... ...) ...)" nil nil nil "/home/orion/.config/doom/snippets/snippets/racket-mode/do" nil nil)
                       ("define-syntax" "(define-syntax ${1:(${2:name} ${3:arguments})} $0)" "(define-syntax ... ...)" nil nil nil "/home/orion/.config/doom/snippets/snippets/racket-mode/define-syntax" nil nil)
                       ("define" "(define ${1:(${2:name} ${3:arguments})} $0)" "(define ... ...)" nil nil nil "/home/orion/.config/doom/snippets/snippets/racket-mode/define" nil nil)
                       ("cond" "(cond [${1:predicate} $0])" "(cond [... ...]...)" nil nil nil "/home/orion/.config/doom/snippets/snippets/racket-mode/cond" nil nil)
                       ("case-lambda" "(case-lambda [${1:arguments} $0])" "(case-lambda [... ...]...)" nil nil nil "/home/orion/.config/doom/snippets/snippets/racket-mode/caselambda" nil nil)
                       ("case" "(case ${1:expression} [${2:datum} $0])" "(case ... [... ...]...)" nil nil nil "/home/orion/.config/doom/snippets/snippets/racket-mode/case" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("ul" "<ul>\n    <li>`(doom-snippets-format \"%n%s%n\")`$0</li>\n</ul>" "ul > li" nil nil nil "/home/orion/.config/doom/snippets/snippets/rjsx-mode/ul" nil "ul")
                       ("<" "<${1:div}>${0:`(doom-snippets-format \"%n%s%n\")`}</$1>" "HTML/JSX tag" nil nil nil "/home/orion/.config/doom/snippets/snippets/rjsx-mode/tag" nil "<")
                       ("div" "<div>${0:`%`}</div>" "<div></div>" nil nil nil "/home/orion/.config/doom/snippets/snippets/rjsx-mode/div" nil "div")
                       ("</" "<${1:div} `%`$0/>" "HTML/JSX self-closed tag" nil nil nil "/home/orion/.config/doom/snippets/snippets/rjsx-mode/closed-tag" nil "</")))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("subject" "subject(:${1:name}) { $0 }" "subject(:${1:name} { ... })" nil nil nil "/home/orion/.config/doom/snippets/snippets/rspec-mode/subject" nil nil)
                       ("sfgs" "subject(:${1:name}) { `(rspec-snippets-fg-method-call \"build_stubbed\")`(:$1) }\n" "subject(:${1:name} { build_stubbed(:$1) })" nil nil nil "/home/orion/.config/doom/snippets/snippets/rspec-mode/sfgs" nil nil)
                       ("sfgc" "subject(:${1:name}) { `(rspec-snippets-fg-method-call \"create\")`(:$1) }\n" "subject(:${1:name} { create(:$1) })" nil nil nil "/home/orion/.config/doom/snippets/snippets/rspec-mode/sfgc" nil nil)
                       ("sfgb" "subject(:${1:name}) { `(rspec-snippets-fg-method-call \"build\")`(:$1) }\n" "subject(:${1:name} { build(:$1) })" nil nil nil "/home/orion/.config/doom/snippets/snippets/rspec-mode/sfgb" nil nil)
                       ("scn" "scenario \\'${1:does something}\\' do\n  $0\nend\n" "scenario 'does something' do ... end" nil nil nil "/home/orion/.config/doom/snippets/snippets/rspec-mode/scn" nil nil)
                       ("lfgs" "let(:${1:name}) { `(rspec-snippets-fg-method-call \"build_stubbed\")`(:$1) }\n" "let(:${1:name} { build_stubbed(:$1) })" nil nil nil "/home/orion/.config/doom/snippets/snippets/rspec-mode/lfgs" nil nil)
                       ("lfgc" "let(:${1:name}) { `(rspec-snippets-fg-method-call \"create\")`(:$1) }\n" "let(:${1:name} { create(:$1) })" nil nil nil "/home/orion/.config/doom/snippets/snippets/rspec-mode/lfgc" nil nil)
                       ("lfgb" "let(:${1:name}) { `(rspec-snippets-fg-method-call \"build\")`(:$1) }\n" "let(:${1:name} { build(:$1) })" nil nil nil "/home/orion/.config/doom/snippets/snippets/rspec-mode/lfgb" nil nil)
                       ("lett" "let!(:${1:var}) { $0 }" "let!(:var) { ... }" nil nil nil "/home/orion/.config/doom/snippets/snippets/rspec-mode/lett" nil nil)
                       ("let" "let(:${1:var}) { $0 }" "let(:var) { ... }" nil nil nil "/home/orion/.config/doom/snippets/snippets/rspec-mode/let" nil nil)
                       ("its" "its(:${1:property}) { $0 }\n" "it \"does something\" do ... end" nil nil nil "/home/orion/.config/doom/snippets/snippets/rspec-mode/its" nil nil)
                       ("itiexp" "it { is_expected.to $0 }" "it { is_expected.to ... }" nil nil nil "/home/orion/.config/doom/snippets/snippets/rspec-mode/itiexp" nil nil)
                       ("it" "it \\'${1:does something}\\' do\n  $0\nend" "it 'does something' do ... end" nil nil nil "/home/orion/.config/doom/snippets/snippets/rspec-mode/it" nil nil)
                       ("iexp" "is_expected.to $0" "is_expected.to ..." nil nil nil "/home/orion/.config/doom/snippets/snippets/rspec-mode/iexp" nil nil)
                       ("helper" "require File.dirname(__FILE__) + '../spec_helper'\n\n$0" "require File.dirname(__FILE__) + '../spec_helper'" nil nil nil "/home/orion/.config/doom/snippets/snippets/rspec-mode/helper" nil nil)
                       ("featurem" "feature \"${1:description}\", \"${2:modifier}\" do\n  $0\nend\n" "feature \"description\", \"modifier\" do ... end" nil nil nil "/home/orion/.config/doom/snippets/snippets/rspec-mode/featurem" nil nil)
                       ("feature" "feature \\'${1:description}\\' do\n  $0\nend\n" "feature 'description' do ... end" nil nil nil "/home/orion/.config/doom/snippets/snippets/rspec-mode/feature" nil nil)
                       ("expect2" "expect { $1 }.to $0" "expect { ... }.to ..." nil nil nil "/home/orion/.config/doom/snippets/snippets/rspec-mode/expect2" nil nil)
                       ("expect" "expect($1).to $0" "expect(...).to ..." nil nil nil "/home/orion/.config/doom/snippets/snippets/rspec-mode/expect" nil nil)
                       ("exp2" "expect { $1 }.to $0" "expect { ... }.to ..." nil nil nil "/home/orion/.config/doom/snippets/snippets/rspec-mode/exp2" nil nil)
                       ("exp" "expect($1).to $0" "expect(...).to ..." nil nil nil "/home/orion/.config/doom/snippets/snippets/rspec-mode/exp" nil nil)
                       ("describem" "describe ${1:`(rspec-class-from-file-name)`}, \\'${2:modifier}\\' do\n  $0\nend" "describe Class, 'modifier' do ... end" nil nil nil "/home/orion/.config/doom/snippets/snippets/rspec-mode/describem" nil nil)
                       ("describe" "describe `maybe-quote`${1:`(and top-level (rspec-class-from-file-name))`}`maybe-quote` do\n  $0\nend" "describe Class do ... end" nil nil
                        ((top-level
                          (rspec-top-level-desc-p))
                         (maybe-quote
                          (unless top-level "'")))
                        "/home/orion/.config/doom/snippets/snippets/rspec-mode/describe" nil nil)
                       ("desc" "describe \\'${1:modifier}\\' do\n  $0\nend" "describe Class do ... end" nil nil nil "/home/orion/.config/doom/snippets/snippets/rspec-mode/desc" nil nil)
                       ("defm" "RSpec::Matchers.define :${have_something} do\n  match do |${1:subject}|\n    $0\n  end\n\n  failure_message do\n    ${2:'expected something to happen, but it did not'}\n  end\n\n  description do\n    ${3:'have something going for it'}\n  end\nend\n" "RSpec::Matchers.define" nil nil nil "/home/orion/.config/doom/snippets/snippets/rspec-mode/defm" nil nil)
                       ("context" "context \\'${1:modifier}\\' do\n  $0\nend" "context 'modifier' do ... end" nil nil nil "/home/orion/.config/doom/snippets/snippets/rspec-mode/context" nil nil)
                       ("cont" "context \\'${1:modifier}\\' do\n  $0\nend" "context 'modifier' do ... end" nil nil nil "/home/orion/.config/doom/snippets/snippets/rspec-mode/cont" nil nil)
                       ("before" "before$1 do\n  $0\nend" "before do ... end" nil nil nil "/home/orion/.config/doom/snippets/snippets/rspec-mode/before" nil nil)
                       ("after" "after$1 do\n  $0\nend" "after do ... end" nil nil nil "/home/orion/.config/doom/snippets/snippets/rspec-mode/after" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("warn" ".. warning:\n   $0" "warning" nil nil nil "/home/orion/.config/doom/snippets/snippets/rst-mode/warning" nil "warn")
                       ("|" "| $0\n|" "verbatim" nil nil nil "/home/orion/.config/doom/snippets/snippets/rst-mode/verbatim" nil "|")
                       ("url" ".. _${1:description}: $0" "url" nil nil nil "/home/orion/.config/doom/snippets/snippets/rst-mode/url" nil "url")
                       ("term" ":term:\\`$0\\`" "term" nil nil nil "/home/orion/.config/doom/snippets/snippets/rst-mode/term" nil "term")
                       ("pause" ".. rst-class:: build" "pause" nil
                        ("hieroglyph")
                        nil "/home/orion/.config/doom/snippets/snippets/rst-mode/pause" nil "pause")
                       ("src" ".. parsed-literal::\n   $0" "parsed_literal" nil nil nil "/home/orion/.config/doom/snippets/snippets/rst-mode/parsed_literal" nil "src")
                       ("mod" ":mod: \\`$0\\`" "module" nil nil nil "/home/orion/.config/doom/snippets/snippets/rst-mode/module" nil "mod")
                       (":" ":${1:var}: $0" "meta" nil nil nil "/home/orion/.config/doom/snippets/snippets/rst-mode/meta" nil ":")
                       ("inc" ".. literalinclude:: ${1:path}" "literatal include" nil nil nil "/home/orion/.config/doom/snippets/snippets/rst-mode/literal_include" nil "inc")
                       ("inh" ".. inheritance-diagram:: $0" "inheritance" nil nil nil "/home/orion/.config/doom/snippets/snippets/rst-mode/inheritance" nil "inh")
                       ("img" ".. image:: ${1:path}\n   :height: ${2:100}\n   :width: ${3:200}\n   :alt: ${4:description}\n\n$0" "image" nil nil nil "/home/orion/.config/doom/snippets/snippets/rst-mode/image" nil "img")
                       ("graph" ".. graphviz::\n\n   $0" "graphviz" nil nil nil "/home/orion/.config/doom/snippets/snippets/rst-mode/graphviz" nil "graph")
                       ("graph" ".. graph:: $1\n\n   $0" "graph" nil nil nil "/home/orion/.config/doom/snippets/snippets/rst-mode/graph" nil "graph")
                       ("fun" ":function:\\`$0\\`" "function" nil nil nil "/home/orion/.config/doom/snippets/snippets/rst-mode/function" nil "fun")
                       ("graph" ".. digraph:: $1\n\n   $0" "digraph" nil nil nil "/home/orion/.config/doom/snippets/snippets/rst-mode/digraph" nil "graph")
                       ("code" ".. code:: ${1:python}" "code" nil nil nil "/home/orion/.config/doom/snippets/snippets/rst-mode/code" nil "code")
                       ("cls" ":class:\\`$0\\`" "class" nil nil nil "/home/orion/.config/doom/snippets/snippets/rst-mode/class" nil "cls")
                       ("auto" ".. automodule:: ${1:module_name}" "automodule" nil nil nil "/home/orion/.config/doom/snippets/snippets/rst-mode/automodule" nil "auto")
                       ("auto" ".. autofunction:: $0" "autofunction" nil nil nil "/home/orion/.config/doom/snippets/snippets/rst-mode/autofunction" nil "auto")
                       ("auto" ".. autoclass:: $0\n   ${1::members: ${2:members}}" "autoclass" nil nil nil "/home/orion/.config/doom/snippets/snippets/rst-mode/autoclass" nil "auto")))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("zip" "zip(${enums}) { |${row}| $0 }" "zip(...) { |...| ... }" nil
                        ("collections")
                        nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/zip" nil nil)
                       ("y" ":yields: $0" ":yields: arguments (rdoc)" nil
                        ("general")
                        nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/y" nil nil)
                       ("while" "while ${condition}\n  $0\nend" "while ... end" nil
                        ("control structure")
                        nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/while" nil nil)
                       ("when" "when ${condition}\n  $0\nend" "when ... end" nil
                        ("control structure")
                        nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/when" nil nil)
                       ("wh" "where(${field}: ${value})" "where(field: value)" nil nil nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/wh" nil nil)
                       ("w" "attr_writer :" "attr_writer ..." nil
                        ("definitions")
                        nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/w" nil nil)
                       ("vali" "validates :${field}${, ${presence}: ${true}}" "validates :field, presence: true" nil nil nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/vali" nil nil)
                       ("upt" "upto(${n}) { |${i}|\n  $0\n}" "upto(...) { |n| ... }" nil
                        ("control structure")
                        nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/upt" nil nil)
                       ("until" "until ${condition}\n  $0\nend" "until ... end" nil
                        ("control structure")
                        nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/until" nil nil)
                       ("tu" "require 'test/unit'" "tu" nil nil nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/tu" nil "tu")
                       ("to_" "def to_s\n    \"${1:string}\"\nend\n$0" "to_" nil nil nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/to_" nil "to_")
                       ("tim" "times { |${n}| $0 }" "times { |n| ... }" nil
                        ("control structure")
                        nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/tim" nil nil)
                       ("tc" "class TC_${1:Class} < Test::Unit::TestCase\n      $0\nend" "test class" nil nil nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/test class" nil "tc")
                       (":tags" "task :tags do\n    sh \"gem ripper_tags --exclude=vendor\"\n    sh \"ripper-tags -R -f TAGS --exclude=vendor\"\nend\n" "task :tags ... end" nil nil nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/task-tags" nil ":tags")
                       (":spec" "require 'rspec/core/rake_task'\nRSpec::Core::RakeTask.new(:spec) { |t| t.verbose = false  }\n" "Rspec::Core::RakeTask.new(:spec) ..." nil nil nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/task-spec" nil ":spec")
                       (":console" "desc 'Open pry session preloaded with this library'\ntask :console do\n  require 'pry'\n  require '${1:gem_name}'\n  ARGV.clear\n  Pry.start\nend\n" "task :console do ... end" nil nil nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/task-console" nil ":console")
                       ("task" "task :${1:task} do\n    $0\nend" "task ... end" nil nil nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/task" nil "task")
                       ("s" "#{$0}" "str" nil nil nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/str" nil "s")
                       ("select" "select { |${1:element}| $0 }" "select { |...| ... }" nil
                        ("collections")
                        nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/select" nil nil)
                       ("rw" "attr_accessor :" "attr_accessor ..." nil
                        ("definitions")
                        nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/rw" nil nil)
                       ("rpry" "require 'pry-remote'; binding.remote_pry" "binding.pry_remote" nil nil nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/rpry" nil nil)
                       ("retun" "return ${item }unless ${condition}" "return item unless condition" nil nil nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/retun" nil nil)
                       ("retif" "return ${item }if ${condition}" "return item if condition" nil nil nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/retif" nil nil)
                       ("resc" "rescue ${StandardError} => ${e}\n  ${}" "rescue StandardError => e" nil nil nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/resc" nil nil)
                       ("req" "require '$0'" "require \"...\"" nil
                        ("general")
                        nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/req" nil nil)
                       ("rel" "require_relative '$0'" "require_relative" nil
                        ("general")
                        nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/rel" nil nil)
                       ("reject" "reject { |${1:element}| $0 }" "reject { |...| ... }" nil
                        ("collections")
                        nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/reject" nil nil)
                       ("red" "reduce(${1:0}) { |${2:accumulator}, ${3:element}| $0 }" "reduce(...) { |...| ... }" nil nil nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/red" nil nil)
                       ("rb" "#!/usr/bin/ruby -wU" "/usr/bin/ruby -wU" nil nil nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/rb" nil nil)
                       ("r" "attr_reader :" "attr_reader ..." nil
                        ("definitions")
                        nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/r" nil nil)
                       ("puts" "puts $0\n" "puts ..." nil nil nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/puts" nil nil)
                       ("pry" "require 'pry'; binding.pry\n" "binding.pry" nil nil nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/pry" nil nil)
                       ("p" "puts $0\n" "puts ..." nil nil nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/p" nil nil)
                       ("now" "Time.zone.now" "Time.zone.now" nil nil nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/now" nil nil)
                       ("ns" "namespace :${1:name} do \n    $0\nend\n" "namespace ... end" nil nil nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/namespace" nil "ns")
                       ("mod" "module ${1:`(let ((fn (capitalize (file-name-nondirectory\n                                 (file-name-sans-extension\n         (or (buffer-file-name)\n             (buffer-name (current-buffer))))))))\n           (while (string-match \"_\" fn)\n             (setq fn (replace-match \"\" nil nil fn)))\n           fn)`}\n  $0\nend" "module ... end" nil nil nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/mod" nil nil)
                       ("mm" "def method_missing(method, *args)\n  $0\nend" "def method_missing ... end" nil
                        ("definitions")
                        nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/mm" nil nil)
                       ("map" "map { |${e}| $0 }" "map { |...| ... }" nil nil nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/map" nil nil)
                       ("loadpath" "\\$:.unshift File.expand_path('../lib', __FILE__)$0" "Loadpath Setup" nil nil nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/loadpath" nil "loadpath")
                       ("inject" "inject(${1:0}) { |${2:injection}, ${3:element}| $0 }" "inject(...) { |...| ... }" nil
                        ("collections")
                        nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/inject" nil nil)
                       ("init" "def initialize(${1:args})\n    $0\nend" "init" nil nil nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/init" nil "init")
                       ("inc" "include ${1:Module}\n$0" "include Module" nil nil nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/inc" nil nil)
                       ("ife" "if ${1:condition}\n  $2\nelse\n  $3\nend" "if ... else ... end" nil
                        ("control structure")
                        nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/ife" nil nil)
                       ("if" "if ${1:condition}\n  $0\nend" "if ... end" nil
                        ("control structure")
                        nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/if" nil nil)
                       ("hasm" "has_many :${models}${, class_name: '${class}'}" "has_many :models, class_name: 'class'" nil nil nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/hasm" nil nil)
                       ("form" "require 'formula'\n\nclass ${1:Name} <Formula\n  url '${2:url}'\n  homepage '${3:home}'\n  md5 '${4:md5}'\n\n  def install\n    ${5:system \"./configure\"}\n    $0\n  end\nend\n" "formula" nil nil nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/formula" nil "form")
                       ("forin" "for ${1:element} in ${2:collection}\n  $0\nend" "for ... in ...; ... end" nil
                        ("control structure")
                        nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/forin" nil nil)
                       ("for" "for ${1:el} in ${2:collection}\n    $0\nend" "for" nil nil nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/for" nil "for")
                       ("fb" "find_by(${column}: ${value})" "find_by(column: value)" nil nil nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/fb" nil nil)
                       ("eawi" "each_with_index { |${e}, ${i}| $0 }" "each_with_index { |e, i| ... }" nil
                        ("collections")
                        nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/eawi" nil nil)
                       ("eav" "each_value { |${val}| $0 }" "each_value { |val| ... }" nil
                        ("collections")
                        nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/eav" nil nil)
                       ("eai" "each_index { |${i}| $0 }" "each_index { |i| ... }" nil
                        ("collections")
                        nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/eai" nil nil)
                       ("eac" "each_cons(${1:2}) { |${group}| $0 }" "each_cons(...) { |...| ... }" nil
                        ("collections")
                        nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/eac" nil nil)
                       ("ea" "each { |${e}| $0 }" "each { |...| ... }" nil
                        ("collections")
                        nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/ea" nil nil)
                       ("dow" "downto(${0}) { |${n}|\n  $0\n}" "downto(...) { |n| ... }" nil
                        ("control structure")
                        nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/dow" nil nil)
                       ("do" "do\n    $0\nend" "do-end" nil nil nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/do-end" "direct-keybinding" "do")
                       ("det" "detect { |${e}| $0 }" "detect { |...| ... }" nil
                        ("collections")
                        nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/det" nil nil)
                       ("deli" "delete_if { |${e}| $0 }" "delete_if { |...| ... }" nil
                        ("collections")
                        nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/deli" nil nil)
                       ("def" "def ${1:method}${2:(${3:args})}\n    $0\nend" "def ... end" nil nil nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/def" nil "def")
                       ("dee" "Marshal.load(Marshal.dump($0))" "deep_copy(...)" nil
                        ("general")
                        nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/dee" nil nil)
                       ("collect" "collect { |${e}| $0 }" "collect { |...| ... }" nil
                        ("collections")
                        nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/collect" nil nil)
                       ("cls" "class ${1:`(let ((fn (capitalize (file-name-nondirectory\n                                 (file-name-sans-extension\n                 (or (buffer-file-name)\n                     (buffer-name (current-buffer))))))))\n             (replace-regexp-in-string \"_\" \"\" fn t t))`}\n  $0\nend\n" "class ... end" nil
                        ("definitions")
                        nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/cls" nil nil)
                       ("cla" "class << ${self}\n  $0\nend" "class << self ... end" nil
                        ("definitions")
                        nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/cla" nil nil)
                       ("case" "case ${1:object}\nwhen ${2:condition}\n  $0\nend" "case ... end" nil
                        ("general")
                        nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/case" nil nil)
                       ("bm" "Benchmark.bmbm(${1:10}) do |x|\n  $0\nend" "Benchmark.bmbm(...) do ... end" nil
                        ("general")
                        nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/bm" nil nil)
                       ("bench" "require \"benchmark\"\n\nTESTS = ${1:1_000}\nBenchmark.bmbm do |x|\n  x.report(\"${2:var}\") {}\nend\n" "bench" nil nil nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/bench" nil "bench")
                       ("bel" "belongs_to :${model}${, optional: ${true}}" "belongs_to :model, optional: true" nil nil nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/bel" nil nil)
                       ("begin" "begin\n    `%`$0\nrescue ${1:Error}\n       # handle error\nend" "begin ... rescue" nil nil nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/begin" nil "begin")
                       ("bb" "byebug" "byebug" nil nil nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/bb" nil nil)
                       ("#!" "#!${1:/usr/bin/env ruby}\n" "/usr/bin/ruby" nil
                        ("general")
                        nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/bang" nil nil)
                       ("@" "@${1:attr} = $0" "attribute" nil nil nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/attribute" nil "@")
                       ("app" "if __FILE__ == $PROGRAM_NAME\n  $0\nend" "if __FILE__ == $PROGRAM_NAME ... end" nil
                        ("general")
                        nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/app" nil nil)
                       ("any" "any? { |${e}| $0 }" "any? { |...| ... }" nil
                        ("collections")
                        nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/any" nil nil)
                       ("am" "alias_method :${new_name}, :${old_name}" "alias_method new, old" nil
                        ("definitions")
                        nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/am" nil nil)
                       ("all" "all? { |${e}| $0 }" "all? { |...| ... }" nil
                        ("collections")
                        nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/all" nil nil)
                       ("$" "$${1:GLOBAL} = $0" "GLOB" nil nil nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/GLOB" nil "$")
                       ("Enum" "include Enumerable\n\ndef each${1:(&block)}\n  $0\nend\n" "include Enumerable" nil nil nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/Enum" nil nil)
                       ("Comp" "include Comparable\n\ndef <=> other\n  $0\nend" "include Comparable; def <=> ... end" nil
                        ("definitions")
                        nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/Comp" nil nil)
                       ("=b" "=begin rdoc\n  $0\n=end" "=begin rdoc ... =end" nil
                        ("general")
                        nil "/home/orion/.config/doom/snippets/snippets/ruby-mode/=b" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("whilel" "while let ${1:pattern} = ${2:expression} {\n      $0\n}" "while let PATTERN = EXPR { ... }" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/while-let" nil "whilel")
                       ("while" "while ${1:true} { `(doom-snippets-format \"%n%s%n\")`$0 }" "while ... { ... }" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/while" nil "while")
                       ("warn!" "#![warn(${1:lint})]" "#![warn(lint)]" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/warn!" nil nil)
                       ("warn" "#[warn(${1:lint})]" "#[warn(lint)]" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/warn" nil nil)
                       ("v" "vec![${1:`%`}]" "vec![...]" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/vec" nil "v")
                       ("uhashmap" "use std::collections::HashMap;" "use std::collections::HashMap"
                        (doom-snippets-bolp)
                        nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/use-HashMap" nil "uhashmap")
                       ("ufile" "use std::fs::File;" "use std::fs::File"
                        (doom-snippets-bolp)
                        nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/use-File" nil "ufile")
                       ("use" "use ${1:std::${2:io}};$0" "use ..."
                        (doom-snippets-bolp)
                        nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/use" nil "use")
                       ("un" "unsafe { `(doom-snippets-format \"%n%s%n\")`$0 }" "unsafe { ... }" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/unsafe" nil "un")
                       ("union" "union ${1:Type} {\n     $0\n}" "union Type { ... }" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/union" nil nil)
                       ("type" "type ${1:TypeName} = ${2:i32};" "type Name = ...;" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/type" nil "type")
                       ("tr"
                        (progn
                          (doom-snippets-expand :uuid "trait"))
                        "trait ... { ... }" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/trait_alias" nil "trait_alias")
                       ("trait" "trait ${1:Name} {\n    `%`$0\n}" "trait ... { ... }" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/trait" nil "trait")
                       ("testmod" "#[cfg(test)]\nmod ${1:tests} {\n    use super::*;\n\n    #[test]\n    fn ${2:test_name}() {\n       $0\n    }\n}" "test module" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/testmod" nil nil)
                       ("test" "#[test]\nfn ${1:test_name}() {\n   `%`$0\n}" "test function"
                        (doom-snippets-bolp)
                        nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/test" nil "test")
                       ("st"
                        (progn
                          (doom-snippets-expand :uuid "struct"))
                        "struct"
                        (doom-snippets-bolp)
                        nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/struct_alias" nil "struct_alias")
                       ("struct" "struct ${1:Name} {\n    `%`$0\n}" "struct"
                        (doom-snippets-bolp)
                        nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/struct" nil "struct")
                       ("static" "static ${1:VARNAME}${2:: ${3:i32}} = ${4:value};" "static VAR = ..." nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/static" nil "static")
                       ("ret" "return ${1:`%`};$0" "return" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/ret" nil "ret")
                       ("pmod" "pub mod ${1:name} {\n    `%`$0\n}" "pub mod" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/pub_mod" nil "pmod")
                       ("pfn" "pub fn ${1:function_name}($2) ${3:-> ${4:i32} }{\n   `%`$0\n}" "public function" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/pub_fn" nil "pfn")
                       ("pafn" "pub async fn ${1:function_name}($2) ${3:-> ${4:i32} }{\n   `%`$0\n}\n" "public async function"
                        (doom-snippets-bolp)
                        nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/pub_async_fn" nil "pafn")
                       ("p" "println!(\"$1\", ${2:`%`});$0" "println!(...)" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/println" nil "p")
                       ("partial" "impl PartialEq for ${1:Type} {\n    fn eq(&self, other: &Self) -> bool {\n        $0\n    }\n}\n" "impl PartialEq for Type" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/partial" nil "partial")
                       ("pa" "panic!(\"$1\", ${2:`%`});$0" "panic!(...)" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/panic" nil "pa")
                       ("new" "${1:Vec}::new(${2:`%`})`(if (eolp) \";\" \"\")`" "Type::new(...)" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/new" nil "new")
                       ("mod" "mod ${1:name} {\n    `%`$0\n}" "mod" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/mod" nil "mod")
                       ("ma"
                        (progn
                          (doom-snippets-expand :uuid "match"))
                        "match" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/match_alias" nil "match_alias")
                       ("match?" "match ${1:x} {\n    Ok(${2:var}) => $3,\n    Err(${4:error}) => $5\n}" "match n { Ok(x), Err(y) }" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/match-ok" nil "match?")
                       ("match" "match ${1:x} {\n    `%`$0\n}" "match" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/match" nil "match")
                       ("main" "fn main() {\n   `%`$0\n}" "main" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/main" nil "main")
                       ("macro" "macro_rules! ${1:name} {\n     ($2) => ($3);\n}\n" "macro_rules! name { (..) => (..); }" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/macro" nil "macro")
                       ("loop" "loop { `(doom-snippets-format \"%n%s%n\")`$0 }" "loop { ... }" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/loop" nil "loop")
                       ("'s" "'static" "'static" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/lifetime-static" nil "'s")
                       ("letm" "let mut ${1:var} = $0`(if (eolp) \";\" \"\")`" "let mut" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/letm" nil "letm")
                       ("let" "let ${1:var} = $0`(if (eolp) \";\" \"\")`" "let" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/let" nil "let")
                       ("fn" "|${1:x}|${2: -> i32} `(if (> (doom-snippets-count-lines %) 1) \"{ \" \"\")``(doom-snippets-format \"%n%s%n\")`$0`(if (> (doom-snippets-count-lines %) 1) \" }\" \"\")`" "anonymous function"
                        (not
                         (doom-snippets-bolp))
                        nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/lambda" nil "fn")
                       ("iterator" "impl Iterator for ${1:Type} {\n    type Item = ${2:Type};\n    fn next(&mut self) -> Option<Self::Item> {\n        $0\n    }\n}\n" "impl Iterator for Type" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/iterator" nil "iterator")
                       ("ife" "if ${1:x} {${2:`%`}}${3: else {$4}}$0" "inline if-else" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/inline-if-else" nil "ife")
                       ("impl" "impl ${1:name}${2: for ${3:Type}} {\n   `%`$0\n}" "impl" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/impl" nil nil)
                       ("ign" "#[ignore]" "#[ignore]" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/ignore" nil "ign")
                       ("ifl" "if let ${1:Some(${2:x})} = ${3:var} {\n   `%`$0\n}" "if let ..." nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/if-let" nil "ifl")
                       ("if" "if ${1:x} {\n   `%`$0\n}" "if ... { ... }" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/if" nil "if")
                       ("fromstr" "impl FromStr for ${1:Type} {\n    type Err = ${2:Error};\n\n    fn from_str(s: &str) -> Result<Self, Self::Err> {\n        `%`\n        Ok(Self{})\n    }\n}\n" "impl FromStr for Type { fn from_str(...) }" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/fromstr" nil nil)
                       ("from" "impl From<${1:From}> for ${2:Type} {\n    fn from(source: $1) -> Self {\n       `%`$0\n       Self { }\n    }\n}\n" "impl From<From> for Type { fn from(...) }" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/from" nil nil)
                       ("f" "format!(\"$1\", ${2:`%`})" "format!(..., ...)" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/format" nil "f")
                       ("for" "for ${1:x} in ${2:items} {\n    `%`$0\n}" "for in" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/for" nil "for")
                       ("fn" "fn ${1:function_name}($2) ${3:-> ${4:i32} }{\n   `%`$0\n}\n" "function"
                        (doom-snippets-bolp)
                        nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/fn" nil "fn")
                       ("ec" "extern crate ${1:name};" "extern crate ..."
                        (doom-snippets-bolp)
                        nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/extern-crate" nil "ec")
                       ("extc" "extern \"C\" {\n    `%`$0\n}" "extern \"C\" { ... }"
                        (doom-snippets-bolp)
                        nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/extern" nil "extc")
                       ("error" "impl std::error::Error for ${1:Type} {\n    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {\n        $0\n        None\n    }\n}\n" "impl Error for Type { fn source(...) }" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/error" nil "error")
                       ("ep" "eprintln!(\"$1\", ${2:`%`});$0" "eprintln!(...)" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/eprintln" nil "ep")
                       ("envv" "env::var(\"$1\")" "env::var(...)" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/env-var" nil "envv")
                       ("argv" "env::args()" "env::args()" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/env-args" nil "argv")
                       ("enum" "enum ${1:EnumName} {\n    `%`$0\n}" "enum" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/enum" nil "enum")
                       ("elif" "else if ${1:true} {\n   `%`$0\n}" "else if ... { ... }" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/elseif" nil "elif")
                       ("else" "else {\n   `%`$0\n}" "else { ... }" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/else" nil "else")
                       ("display" "impl Display for ${1:Type} {\n    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {\n        write!(f, \"`%`$0\")\n    }\n}\n" "impl Display for Type { fn fmt (...) }" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/display" nil nil)
                       ("disperror" "impl Display for $1 {\n    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {\n        write!(f, \"{}\", $0)\n    }\n}\n\nimpl std::error::Error for ${1:Type} {\n    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {\n        None\n    }\n}\n" "Display and Error Traits" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/disperror" nil "disperror")
                       ("der" "#[derive($1)]" "#[derive(...)]"
                        (doom-snippets-bolp)
                        nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/derive" nil "der")
                       ("deref_mut" "impl std::ops::DerefMut for ${1:Type} {\n    fn deref_mut(&mut self) -> &mut Self::Target {\n        &mut self.$0\n    }\n}\n" "impl DerefMut for Type" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/deref_mut" nil "deref_mut")
                       ("deref" "impl std::ops::Deref for ${1:Type} {\n    type Target = ${2:Type};\n    fn deref(&self) -> &Self::Target {\n        &self.$0\n    }\n}\n" "impl Deref for Type" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/deref" nil "deref")
                       ("dass" "debug_assert!(`%`$0);" "debug_assert!(...)" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/debug_assert" nil "dass")
                       ("const" "const ${1:VARNAME}${2: ${3:i32}} = ${4:value};" "const VAR = ..." nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/const" nil "const")
                       ("cfg=" "#[cfg(${1:option} = \"${2:value}\")]" "#[cfg(option = \"value\")]" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/cfg=" nil nil)
                       ("cfg" "#[cfg($0)]" "#[cfg(...)]" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/cfg" nil nil)
                       ("case" "${1:pattern} => ${2:expression}," "pattern => expression," nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/case" nil nil)
                       ("b" "${1:Label} { `(doom-snippets-format \"%n%s%n\")`$1 }$0" "block" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/block" nil "b")
                       ("afn" "async fn ${1:function_name}($2) ${3:-> ${4:i32} }{\n   `%`$0\n}\n" "async function"
                        (doom-snippets-bolp)
                        nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/async_fn" nil "afn")
                       ("=" "${1:x} = ${2:value}`(if (eolp) \";\" \"\")`$0" "assignment" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/assignment" nil "=")
                       ("assn" "assert_ne!(${1:`%`}, $2);" "assert_ne!(..., ...)" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/assert_ne" nil "assn")
                       ("asse" "assert_eq!(${1:`%`}, $2);" "assert_eq!(..., ...)" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/assert_eq" nil "asse")
                       ("ass" "assert!(`%`$0);" "assert!(...)" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/assert" nil "ass")
                       ("asref" "impl std::convert::AsRef<${1:Type}> for ${2:Type} {\n    fn as_ref(&self) -> &$2 {\n        $0\n    }\n}\n" "impl AsRef<Type> for Type" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/asref" nil "asref")
                       ("=>" "${1:_} => ${0:...}" "x => y" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/arrow" nil "=>")
                       ("allow!" "#![allow(${1:lint})]" "#![allow(lint)]" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/allow!" nil nil)
                       ("allow" "#[allow(${1:lint})]" "#[allow(lint)]" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/allow" nil nil)
                       ("ts" "to_string()" ".to_string()"
                        (doom-snippets-without-trigger
                         (eq
                          (char-before)
                          46))
                        nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/_to_string" nil "ts")
                       ("i" "iter()" ".iter()"
                        (doom-snippets-without-trigger
                         (eq
                          (char-before)
                          46))
                        nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/_iter" nil "i")
                       ("ii" "into_iter()" ".into_iter()"
                        (doom-snippets-without-trigger
                         (eq
                          (char-before)
                          46))
                        nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/_into_iter" nil "ii")
                       ("fm" "filter_map(${1:`%`})" ".filter_map(...)"
                        (doom-snippets-without-trigger
                         (eq
                          (char-before)
                          46))
                        nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/_filter_map" nil "fm")
                       ("vwc" "Vec::with_capacity(${1:n})" "Vec::with_capacity(...)" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/Vec-with_capacity" nil "vwc")
                       ("sf" "String::from(\"$0\")" "String::from(...)" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/String_from" nil "sf")
                       ("so" "Some(${1:`%`})" "Some(...)" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/Some" nil "so")
                       ("res" "Result<${1:T}, ${2:()}>" "Result<T, E>" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/Result" nil "res")
                       ("no" "None" "None" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/None" nil "no")
                       ("fo" "File::open(${1:`%`})`(if (eolp) \";\" \"\")`" "File::open(...)" nil nil nil "/home/orion/.config/doom/snippets/snippets/rust-mode/File-open" nil "fo")))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("try" "try {\n    `%`$0\n} catch {\n    case e: ${1:Throwable} => \n        ${2:// TODO: handle exception}\n}" "try { ... } catch { case e => ... }" nil nil nil "/home/orion/.config/doom/snippets/snippets/scala-mode/try" nil nil)
                       ("throw" "throw new ${1:Exception}($2) $0" "throw new Exception" nil nil nil "/home/orion/.config/doom/snippets/snippets/scala-mode/throw" nil nil)
                       ("main" "def main(args: Array[String]) = {\n    `%`$0\n}" "def main(args: Array[String]) = { ... }" nil nil nil "/home/orion/.config/doom/snippets/snippets/scala-mode/main" nil nil)
                       ("if" "if (${1:condition}) {\n    `%`$0\n}" "if (...) { ... }" nil nil nil "/home/orion/.config/doom/snippets/snippets/scala-mode/if" nil nil)
                       ("def" "def ${1:name}(${2:args}): ${3:Unit} = {\n    `%`$0\n}" "def fn(args): R = { ... }" nil nil nil "/home/orion/.config/doom/snippets/snippets/scala-mode/def" nil nil)
                       ("cons" "${1:element1} :: ${2:element2} $0" "element1 :: element2" nil nil nil "/home/orion/.config/doom/snippets/snippets/scala-mode/cons" nil nil)
                       ("case" "case ${1:_} => $0" "case ... => ..." nil nil nil "/home/orion/.config/doom/snippets/snippets/scala-mode/case" nil nil)
                       ("app" "object ${1:name} extends App {\n    `%`$0\n}" "object ... extends App { ... }" nil nil nil "/home/orion/.config/doom/snippets/snippets/scala-mode/app" nil nil)
                       ("List" "List(${1:args}) $0" "List(...)" nil nil nil "/home/orion/.config/doom/snippets/snippets/scala-mode/List" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("zip" "zip(${1:$list1}, ${2:$list2})" "zip($list1, $list2, ...)" nil
                        ("List functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/zip" nil nil)
                       ("variable-exists" "variable-exists(${1:$name})" "variable-exists($name)" nil
                        ("Introspection functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/variable-exists" nil nil)
                       ("unquote" "unquote(${1:$string})" "unquote($string)" nil
                        ("String functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/unquote" nil nil)
                       ("unitless" "unitless(${1:$number})" "unitless($number)" nil
                        ("Introspection functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/unitless" nil nil)
                       ("unit" "unit(${1:$number})" "unit($number)" nil
                        ("Introspection functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/unit" nil nil)
                       ("unique-id" "unique-id()" "unique-id()" nil
                        ("Misc functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/unique-id" nil nil)
                       ("type-of" "type-of(${1:$value})" "type-of($value)" nil
                        ("Introspection functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/type-of" nil nil)
                       ("transparentize" "transparentize(${1:$color}, ${2:$amount})" "transparentize($color, $amount)" nil
                        ("Opacity functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/transparentize" nil nil)
                       ("to-upper-case" "to-upper-case(${1:$string})" "to-upper-case($string)" nil
                        ("String functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/to-upper-case" nil nil)
                       ("to-lower-case" "to-lower-case(${1:$string})" "to-lower-case($string)" nil
                        ("String functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/to-lower-case" nil nil)
                       ("str-slice" "str-slice(${1:$string}, ${2:$start-at}${3:, ${4:$end-at}})" "str-slice($string, $start-at, [$end-at])" nil
                        ("String functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/str-slice" nil nil)
                       ("str-length" "str-length(${1:$string})" "str-length($string)" nil
                        ("String functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/str-length" nil nil)
                       ("str-insert" "str-insert(${1:$string}, ${2:$insert}, ${3:$index})" "str-insert($string, $insert, $index)" nil
                        ("String functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/str-insert" nil nil)
                       ("str-index" "str-index(${1:$string}, ${2:$substring})" "str-index($string, $substring)" nil
                        ("String functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/str-index" nil nil)
                       ("set-nth" "set-nth(${1:$list}, ${2:$n}, ${3:$value})" "set-nth($list, $n, $value)" nil
                        ("List functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/set-nth" nil nil)
                       ("scale-color" "scale-color(${1:$color}, ${2:[$r]}, ${3:[$g]}, ${4:[$b]}, ${5:[$sat]}, ${6:[$light]}, ${7:[$alpha]})" "scale-color($color, [$r], [$g], [$b], [$sat], [$light], [$alpha])" nil
                        ("Other color functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/scale-color" nil nil)
                       ("saturation" "saturation(${1:$color})" "saturation($color)" nil
                        ("HSL functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/saturation" nil nil)
                       ("saturate" "saturate(${1:$color}, ${2:$amount})" "saturate($color, $amount)" nil
                        ("HSL functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/saturate" nil nil)
                       ("round" "round(${1:$number})" "round($number)" nil
                        ("Number functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/round" nil nil)
                       ("rgba" "rgba(${1:$color}, ${2:$alpha})" "rgba($color, $alpha)" nil
                        ("Opacity functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/rgbao" nil "rgba")
                       ("rgba" "rgba(${1:$red}, ${2:$green}, ${3:$blue}, ${4:alpha})" "rgb($red, $green, $blue, $alpha)" nil
                        ("RGB functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/rgba" nil nil)
                       ("rgb" "rgb(${1:$red}, ${2:$green}, ${3:$blue})" "rgb($red, $green, $blue)" nil
                        ("RGB functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/rgb" nil nil)
                       ("red" "red(${1:color})" "red($color)" nil
                        ("RGB functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/red" nil nil)
                       ("random" "random(${1:[$limit]})" "random([$limit])" nil
                        ("Number functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/random" nil nil)
                       ("quote" "quote(${1:$string})" "quote($string)" nil
                        ("String functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/quote" nil nil)
                       ("percentage" "percentage(${1:$number})" "percentage($number)" nil
                        ("Number functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/percentage" nil nil)
                       ("opacity" "opacity(${1:$color})" "opacity($color)" nil
                        ("Opacity functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/opacity" nil nil)
                       ("opacify" "opacify(${1:$color}, ${2:$amount})" "opacify($color, $amount)" nil
                        ("Opacity functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/opacify" nil nil)
                       ("nth" "nth(${1:$list}, ${2:$n})" "nth($list, $n)" nil
                        ("List functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/nth" nil nil)
                       ("mixin-exists" "mixin-exists(${1:$name})" "mixin-exists($name)" nil
                        ("Introspection functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/mixin-exists" nil nil)
                       ("mix" "mix(${1:$color1}, ${2:$color2}${3:, ${4:$weight}})" "mix($color1, $color2, [$weight])" nil
                        ("RGB functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/mix" nil nil)
                       ("min" "min(${1:$n1}, ${2:$n2})" "min($n1, $n2, ...)" nil
                        ("Number functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/min" nil nil)
                       ("max" "max(${1:$n1}, ${2:$n2})" "max($n1, $n2, ...)" nil
                        ("Number functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/max" nil nil)
                       ("map-values" "map-values(${1:$map})" "map-values($map)" nil
                        ("Map functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/map-values" nil nil)
                       ("map-remove" "map-remove(${1:$map}, ${2:$key})" "map-remove($map, $key, ...)" nil
                        ("Map functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/map-remove" nil nil)
                       ("map-merge" "map-merge(${1:$map1}, ${2:$map2})" "map-merge($map1, $map2)" nil
                        ("Map functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/map-merge" nil nil)
                       ("map-keys" "map-keys(${1:$map})" "map-keys($map)" nil
                        ("Map functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/map-keys" nil nil)
                       ("map-has-key" "map-has-key(${1:$map}, ${2:$key})" "map-has-key($map, $key)" nil
                        ("Map functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/map-has-key" nil nil)
                       ("map-get" "map-get(${1:$map}, ${2:$key})" "map-get($map, $key)" nil
                        ("Map functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/map-get" nil nil)
                       ("list-separator" "list-separator(${1:$list})" "list-separator($list)" nil
                        ("List functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/list-separator" nil nil)
                       ("lightness" "lightness(${1:$color})" "lightness($color)" nil
                        ("HSL functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/lightness" nil nil)
                       ("lighten" "lighten(${1:$color}, ${2:$amount})" "lighten($color, $amount)" nil
                        ("HSL functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/lighten" nil nil)
                       ("length" "length(${1:$list})" "length($list)" nil
                        ("List functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/length" nil nil)
                       ("keywords" "keywords(${1:$args})" "keywords($args)" nil
                        ("Map functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/keywords" nil nil)
                       ("join" "join(${1:$list1}, ${2:$list2}, ${3:[$sep]})" "join($list1, $list2, [$separator])" nil
                        ("List functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/join" nil nil)
                       ("invert" "invert(${1:$color})" "invert($color)" nil
                        ("HSL functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/invert" nil nil)
                       ("inspect" "inspect(${1:$value})" "inspect($value)" nil
                        ("Introspection functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/inspect" nil nil)
                       ("index" "index(${1:$list}, ${2:$value})" "index($list, $value)" nil
                        ("List functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/index" nil nil)
                       ("ie-hex-str" "ie-hex-str(${1:$color})" "ie-hex-str($color)" nil
                        ("Other color functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/ie-hex-str" nil nil)
                       ("hue" "hue(${1:$color})" "hue($color)" nil
                        ("HSL functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/hue" nil nil)
                       ("hsla" "hsla(${1:$hue}, ${2:$saturation}, ${3:$lightness}, ${4:alpha})" "hsl($hue, $saturation, $lightness, $alpha)" nil
                        ("HSL functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/hsla" nil nil)
                       ("hsl" "hsl(${1:$hue}, ${2:$saturation}, ${3:$lightness})" "hsl($hue, $saturation, $lightness)" nil
                        ("HSL functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/hsl" nil nil)
                       ("green" "green(${1:color})" "green($color)" nil
                        ("RGB functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/green" nil nil)
                       ("grayscale" "grayscale(${1:$color})" "grayscale($color)" nil
                        ("HSL functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/grayscale" nil nil)
                       ("global-variable-exists" "global-variable-exists(${1:$name})" "global-variable-exists($name)" nil
                        ("Introspection functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/global-variable-exists" nil nil)
                       ("function-exists" "function-exists(${1:$name})" "function-exists($name)" nil
                        ("Introspection functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/function-exists" nil nil)
                       ("for" "@for \\$i from ${1:1} through ${2:10} {\n    `%`$0\n}\n" "@for loop" nil nil nil "/home/orion/.config/doom/snippets/snippets/scss-mode/for" nil nil)
                       ("floor" "floor(${1:$number})" "floor($number)" nil
                        ("Number functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/floor" nil nil)
                       ("feature-exists" "feature-exists(${1:$feature})" "feature-exists($feature)" nil
                        ("Introspection functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/feature-exists" nil nil)
                       ("fade-out" "fade-out(${1:$color}, ${2:$amount})" "fade-out($color, $amount)" nil
                        ("Opacity functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/fade-out" nil nil)
                       ("fade-in" "fade-in(${1:$color}, ${2:$amount})" "fade-in($color, $amount)" nil
                        ("Opacity functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/fade-in" nil nil)
                       ("extend" "@extend `(doom-snippets-text nil t)`$0;\n" "@extend" nil nil nil "/home/orion/.config/doom/snippets/snippets/scss-mode/extend" nil nil)
                       ("elseif" "@else if ${1:condition} {\n    `%`$0\n}\n" "@elseif" nil nil nil "/home/orion/.config/doom/snippets/snippets/scss-mode/elseif" nil nil)
                       ("else" "@else {\n    `%`$0\n}\n" "@else" nil nil nil "/home/orion/.config/doom/snippets/snippets/scss-mode/else" nil nil)
                       ("each" "@each \\$${1:var} in ${2:list, of, things} {\n    `%`$0\n}\n" "@each" nil nil nil "/home/orion/.config/doom/snippets/snippets/scss-mode/each" nil nil)
                       ("desaturate" "desaturate(${1:$color}, ${2:$amount})" "desaturate($color, $amount)" nil
                        ("HSL functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/desaturate" nil nil)
                       ("darken" "darken(${1:$color}, ${2:$amount})" "darken($color, $amount)" nil
                        ("HSL functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/darken" nil nil)
                       ("complement" "complement(${1:$color})" "complement($color)" nil
                        ("HSL functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/complement" nil nil)
                       ("comparable" "comparable(${1:$number1}, ${2:$number2})" "comparable($number1, $number2)" nil
                        ("Introspection functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/comparable" nil nil)
                       ("change-color" "change-color(${1:$color}, ${2:[$r]}, ${3:[$g]}, ${4:[$b]}, ${5:[$hue]}, ${6:[$sat]}, ${7:[$light]}, ${8:[$alpha]})" "change-color($color, [$r], [$g], [$b], [$hue], [$sat], [$light], [$alpha])" nil
                        ("Other color functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/change-color" nil nil)
                       ("ceil" "ceil(${1:$number})" "ceil($number)" nil
                        ("Number functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/ceil" nil nil)
                       ("call" "call(${1:$name}, ${2:$args...})" "call($name, ...)" nil
                        ("Introspection functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/call" nil nil)
                       ("blue" "blue(${1:color})" "blue($color)" nil
                        ("RGB functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/blue" nil nil)
                       ("append" "append(${1:$list}, ${2:$value}, ${3:[$sep]})" "append($list, $value, [$separator])" nil
                        ("List functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/append" nil nil)
                       ("alpha" "alpha(${1:$color})" "alpha($color)" nil
                        ("Opacity functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/alpha" nil nil)
                       ("adjust-hue" "adjust-hue(${1:$color}, ${2:$degree})" "adjust-hue($color, $degree)" nil
                        ("HSL functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/adjust-hue" nil nil)
                       ("adjust-color" "adjust-color(${1:$color}, ${2:[$r]}, ${3:[$g]}, ${4:[$b]}, ${5:[$hue]}, ${6:[$sat]}, ${7:[$light]}, ${8:[$alpha]})" "adjust-color($color, [$r], [$g], [$b], [$hue], [$sat], [$light], [$alpha])" nil
                        ("Other color functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/adjust-color" nil nil)
                       ("abs" "abs(${1:$number})" "abs($number)" nil
                        ("Number functions")
                        nil "/home/orion/.config/doom/snippets/snippets/scss-mode/abs" nil nil)
                       ("while" "@while ${1:condition} {\n    `%`$0\n}\n" "@while" nil nil nil "/home/orion/.config/doom/snippets/snippets/scss-mode/@while" nil "while")
                       ("mix" "@mixin ${1:mixin-name}($2) {\n    `%`$0\n}" "@mixin" nil nil nil "/home/orion/.config/doom/snippets/snippets/scss-mode/@mixin" nil "mix")
                       ("inc" "@include `(doom-snippets-text nil t)`${1:mixin-name};" "@include mixin" nil nil nil "/home/orion/.config/doom/snippets/snippets/scss-mode/@include" nil "inc")
                       ("if" "@if ${1:condition} {\n    `%`$0\n}" "@if" nil nil nil "/home/orion/.config/doom/snippets/snippets/scss-mode/@if" nil "if")))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("=" "${1:VAR}=${0:VALUE}" "VAR=value" nil nil nil "/home/orion/.config/doom/snippets/snippets/sh-mode/var" nil "=")
                       ("<" "\"$1\" -lt \"$2\"\n" "Less than" nil nil nil "/home/orion/.config/doom/snippets/snippets/sh-mode/lt" nil "<")
                       ("local" "local ${1:var}=${0:value}\n" "local var=value" nil nil nil "/home/orion/.config/doom/snippets/snippets/sh-mode/local" nil nil)
                       ("ife" "if ${1:condition}; then\n    `%`${2:# code}\nelse\n     $0\nfi" "ife" nil nil nil "/home/orion/.config/doom/snippets/snippets/sh-mode/ife" nil "ife")
                       ("if;" "${1:condition} && ${0:echo 1}\n" "if ...; then ...; fi" nil nil nil "/home/orion/.config/doom/snippets/snippets/sh-mode/if-one-line" nil "if;")
                       ("if" "if $1; then\n   `%`$0\nfi\n" "if" nil nil nil "/home/orion/.config/doom/snippets/snippets/sh-mode/if" nil nil)
                       (">" "\"$1\" -gt \"$2\"\n" "Greater than" nil nil nil "/home/orion/.config/doom/snippets/snippets/sh-mode/gt" nil ">")
                       ("getopts" "while getopts \"${1:s:h:}\" o; do\n      case \"$o\" in\n           ${2:X}) `(doom-snippets-format \"%n%s\")`$0\n              ;;\n           *) usage\n              ;;\n      esac\ndone" "getopts" nil nil nil "/home/orion/.config/doom/snippets/snippets/sh-mode/getopts" nil "getopts")
                       ("f;" "${1:name}() { `(doom-snippets-format \"%n%s%n\")`$0; }" "one-line function" nil nil nil "/home/orion/.config/doom/snippets/snippets/sh-mode/function-one-line" nil "f;")
                       ("function" "function ${1:name} {\n    `%`$0\n}" "function" nil nil nil "/home/orion/.config/doom/snippets/snippets/sh-mode/function" nil nil)
                       ("for;" "for ${1:var} in ${2:stuff}; do `(doom-snippets-format \"%n%s%n\")`$0; done\n" "for loop (one line)" nil nil nil "/home/orion/.config/doom/snippets/snippets/sh-mode/for-one-line" nil "for;")
                       ("for" "for ${1:var} in ${2:stuff}; do\n    `%`$0\ndone" "for loop" nil nil nil "/home/orion/.config/doom/snippets/snippets/sh-mode/for" nil "for")
                       ("f" "${1:name}() {\n    `%`$0\n}\n" "short function" nil nil nil "/home/orion/.config/doom/snippets/snippets/sh-mode/f" nil "f")
                       ("else" "else\n    `%`$0" "else" nil nil nil "/home/orion/.config/doom/snippets/snippets/sh-mode/else" nil "else")
                       ("elif" "elif ${1:condition}; then\n    `%`$0" "elif" nil nil nil "/home/orion/.config/doom/snippets/snippets/sh-mode/elif" nil "elif")
                       ("case" "case ${1:var} in\n    ${2:match}) $0 ;;\nesac" "case" nil nil nil "/home/orion/.config/doom/snippets/snippets/sh-mode/case" nil "case")
                       ("#!" "#!${1:/usr/bin/env `(symbol-name sh-shell)`}\n$0" "bang" nil nil nil "/home/orion/.config/doom/snippets/snippets/sh-mode/bang" nil "#!")
                       ("args" "[ $# -lt ${1:2} ]" "args" nil nil nil "/home/orion/.config/doom/snippets/snippets/sh-mode/args" nil "args")
                       ("alias" "alias ${1:cmd}=$0" "alias" nil nil nil "/home/orion/.config/doom/snippets/snippets/sh-mode/alias" nil "alias")))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("ul." "ul\n  `%`$0\n" "ul ..." nil nil nil "/home/orion/.config/doom/snippets/snippets/slim-mode/uldot" nil "ul.")
                       ("scriptsrc" "script src=\"${1:lib.js}\"" "script src=\"...\"" nil nil nil "/home/orion/.config/doom/snippets/snippets/slim-mode/scriptsrc" nil nil)
                       ("ol." "ol\n  `%`$0\n" "ol ..." nil nil nil "/home/orion/.config/doom/snippets/snippets/slim-mode/oldot" nil "ol.")
                       ("mkd" "markdown:\n  `%`$0\n" "markdown" nil nil nil "/home/orion/.config/doom/snippets/snippets/slim-mode/mkd" nil nil)
                       ("link" "link rel=\"${1:stylesheet}\" type=\"${2:text/css}\" href=\"${3:/css/master.css}\"" "link ..." nil nil nil "/home/orion/.config/doom/snippets/snippets/slim-mode/link" nil nil)
                       ("li." "li\n  `%`$0\n" "li ..." nil nil nil "/home/orion/.config/doom/snippets/snippets/slim-mode/lidot" nil "li.")
                       ("js" "javascript:\n  $0\n" "javascript: ..." nil nil nil "/home/orion/.config/doom/snippets/snippets/slim-mode/js" nil nil)
                       ("html" "doctype html\nhtml(lang=\"en\")\n  head\n    meta charset=\"utf-8\"\n    title ${1:Page Title}\n  body\n    $0\n    " "HTML page skeleton"
                        (bobp)
                        nil nil "/home/orion/.config/doom/snippets/snippets/slim-mode/html" nil nil)
                       ("desc" "meta name=\"description\" content=\"$0\"" "meta name=\"description\"" nil nil nil "/home/orion/.config/doom/snippets/snippets/slim-mode/desc" nil nil)
                       ("cdn-modernizr.min.js" "script src=\"https://cdnjs.cloudflare.com/ajax/libs/modernizr/${1:2.8.3}/modernizr.min.js\"" "script src=\"https://cdn.js.cloudflare.com/.../modernizr.min.js\"" nil nil nil "/home/orion/.config/doom/snippets/snippets/slim-mode/cdn-modernizr.min.js" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("#" "# -*- mode: snippet -*-\n${1:# contributor: `(user-full-name)`\n}# name: $2\n# key: ${3:trigger-key}${4:\n# condition: t}\n# --\n$0\n" "Snippet header" nil nil nil "/home/orion/.config/doom/snippets/snippets/snippet-mode/vars" nil "#")
                       ("mirror" "\\${${2:n}:${4:\\$(${5:reflection-fn})}\\}$0" "${n:$(...)} mirror" nil nil nil "/home/orion/.config/doom/snippets/snippets/snippet-mode/mirror" nil "mirror")
                       ("group" "# group : ${1:group}" "group" nil nil nil "/home/orion/.config/doom/snippets/snippets/snippet-mode/group" nil "group")
                       ("field" "\\${${1:${2:n}:}$3${4:\\$(${5:lisp-fn})}\\}$0" "${ ...  } field" nil nil nil "/home/orion/.config/doom/snippets/snippets/snippet-mode/field" nil "field")
                       ("`" "\\`$0\\`" "elisp" nil nil nil "/home/orion/.config/doom/snippets/snippets/snippet-mode/elisp" nil "`")
                       ("cont" "# contributor: `user-full-name`" "cont" nil nil nil "/home/orion/.config/doom/snippets/snippets/snippet-mode/cont" nil "cont")))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("while" "while (${1:condition}) {\n    `%`$0\n}\n" "while loop" nil nil nil "/home/orion/.config/doom/snippets/snippets/solidity-mode/while" nil nil)
                       ("struct" "struct ${1:StructName} {\n    $0\n}\n" "struct ... { ... }" nil nil nil "/home/orion/.config/doom/snippets/snippets/solidity-mode/struct" nil nil)
                       ("require" "require($0);\n" "require(...);" nil nil nil "/home/orion/.config/doom/snippets/snippets/solidity-mode/require" nil nil)
                       ("modifier" "modifier ${1:modifierName}(${2:args}) {\n    $0\n    ${3:_;}\n}\n" "modifier ...(...) { ... }" nil nil nil "/home/orion/.config/doom/snippets/snippets/solidity-mode/modifier" nil nil)
                       ("library" "library ${1:LibraryName} {\n    $0\n}\n" "library ... { ... }" nil nil nil "/home/orion/.config/doom/snippets/snippets/solidity-mode/library" nil nil)
                       ("interface" "interface ${1:InterfaceName} {\n    $0\n}\n" "interface ... { ... }" nil nil nil "/home/orion/.config/doom/snippets/snippets/solidity-mode/interface" nil nil)
                       ("ife" "if (${1:condition}) {\n    `%`$2\n} else {\n    $0\n}" "if (...) { ... } else { ... }" nil nil nil "/home/orion/.config/doom/snippets/snippets/solidity-mode/ife" nil nil)
                       ("if" "if (${1:condition}) {\n    `%`$0\n}\n" "if (...) { ... }" nil nil nil "/home/orion/.config/doom/snippets/snippets/solidity-mode/if" nil nil)
                       ("function" "function ${1:functionName}(${2:args})${3: internal}${4: returns (${5:return types})} {\n    $0\n}\n" "function ... { ... }" nil nil nil "/home/orion/.config/doom/snippets/snippets/solidity-mode/function" nil nil)
                       ("for" "for (${1:uint} ${2:i} = 0; $2 < ${3:${4:array}.length}; $2++) {\n    `%`$0\n}\n" "for loop" nil nil nil "/home/orion/.config/doom/snippets/snippets/solidity-mode/for" nil nil)
                       ("event" "event ${1:EventName}(${2:args});\n" "event ...(...)" nil nil nil "/home/orion/.config/doom/snippets/snippets/solidity-mode/event" nil nil)
                       ("enum" "enum ${1:EnumName} {\n    $0\n}\n" "enum ... { ... }" nil nil nil "/home/orion/.config/doom/snippets/snippets/solidity-mode/enum" nil nil)
                       ("contract" "contract ${1:ContractName} {\n    $2\n\n    constructor(${3:args}) public {\n        $0\n    }\n}\n" "contract ... { ... }" nil nil nil "/home/orion/.config/doom/snippets/snippets/solidity-mode/contract" nil nil)
                       ("assert" "assert($0);" "assert(...);" nil nil nil "/home/orion/.config/doom/snippets/snippets/solidity-mode/assert" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("references" "REFERENCES ${1:TableName}([${2:ColumnName}])\n" "REFERENCES ..." nil nil nil "/home/orion/.config/doom/snippets/snippets/sql-mode/references" nil nil)
                       ("create.1" "CREATE PROCEDURE [${1:dbo}].[${2:Name}] \n(\n        $3      $4      = ${5:NULL}     ${6:OUTPUT}\n)\nAS\nBEGIN\n$0\nEND\nGO\n" "create procedure ..." nil nil nil "/home/orion/.config/doom/snippets/snippets/sql-mode/create.1" nil nil)
                       ("create" "CREATE TABLE [${1:dbo}].[${2:TableName}] \n(\n        ${3:Id}     ${4:INT IDENTITY(1,1)}      ${5:NOT NULL}\n$0\n    CONSTRAINT [${6:PK_}] PRIMARY KEY ${7:CLUSTERED} ([$3]) \n)\nGO\n" "create table ..." nil nil nil "/home/orion/.config/doom/snippets/snippets/sql-mode/create" nil nil)
                       ("constraint.1" "CONSTRAINT [${1:FK_Name}] FOREIGN KEY ${2:CLUSTERED} ([${3:ColumnName}]) \n" "CONSTRAINT [..] FOREIGN KEY ..." nil nil nil "/home/orion/.config/doom/snippets/snippets/sql-mode/constraint.1" nil nil)
                       ("constraint" "CONSTRAINT [${1:PK_Name}] PRIMARY KEY ${2:CLUSTERED} ([${3:ColumnName}]) \n" "CONSTRAINT [..] PRIMARY KEY ..." nil nil nil "/home/orion/.config/doom/snippets/snippets/sql-mode/constraint" nil nil)
                       ("column" "    ,   ${1:Name}       ${2:Type}           ${3:NOT NULL}\n" ", ColumnName ColumnType NOT NULL..." nil nil nil "/home/orion/.config/doom/snippets/snippets/sql-mode/column" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("let" "let ${1:name} = ${0:`%`};" "let" nil nil nil "/home/orion/.config/doom/snippets/snippets/typescript-mode/let" nil nil)
                       ("importas" "import * as ${1:`%`} from '$2';" "import * as ..." nil nil nil "/home/orion/.config/doom/snippets/snippets/typescript-mode/importas" nil nil)
                       ("import" "import ${1:`%`} from '$2';" "import ..." nil nil nil "/home/orion/.config/doom/snippets/snippets/typescript-mode/import" nil nil)
                       ("const" "const ${1:name} = ${0:`%`};" "const" nil nil nil "/home/orion/.config/doom/snippets/snippets/typescript-mode/const" nil nil)
                       ("class" "class ${1:Name} {\n    $2\n\n    constructor($3) {\n        `%`$0\n    }\n}" "class" nil nil nil "/home/orion/.config/doom/snippets/snippets/typescript-mode/class" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("fct" "import React from 'react';\n\ninterface ${1:`(file-name-base buffer-file-name)`}PropTypes {\n\n}\n\nconst $1 = (props: $1PropTypes) => (\n  $0\n);\n\nexport { $1 };\n" "functionalComponentTSX" nil nil nil "/home/orion/.config/doom/snippets/snippets/typescript-tsx-mode/componentTSX" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("vue" "<script>\n  export default {\n    name: '${0:`(file-name-base buffer-file-name)`}'$3\n  }\n</script>\n\n<template>\n  ${1:`%`}\n</template>\n\n<style scoped>\n  $2\n</style>\n" "Vue Single-File Component"
                        (equal web-mode-engine "vue")
                        nil nil "/home/orion/.config/doom/snippets/snippets/web-mode/vue" nil "vue")
                       ("video" "<video width=\"${1:560}\" height=\"${2:340}\" controls>\n  <source src=\"${3:path/to/myvideo}.mp4\" type='video/mp4; codecs=\"avc1.42E01E, mp4a.40.2\"'>\n  <source src=\"$3.ogv\" type='video/ogg; codecs=\"theora, vorbis\"'>\n  ${0:Your browser does not support my HTML5 video player}\n</video>" "HTML5 video player" nil nil nil "/home/orion/.config/doom/snippets/snippets/web-mode/video" nil nil)
                       ("php" "<?php `(doom-snippets-format \"%n%s%n\")`$0 ?>" "<?php ... ?>" nil nil nil "/home/orion/.config/doom/snippets/snippets/web-mode/php" nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'snippets
                     '(("--" "--- # ${1:section}\n$0" "section" nil nil nil "/home/orion/.config/doom/snippets/snippets/yaml-mode/section" nil "--")
                       ("list" "[$1]\n$0" "list" nil nil nil "/home/orion/.config/doom/snippets/snippets/yaml-mode/list" nil "list")
                       ("entry" "${1:entry}: ${2:value}\n$0" "entry" nil nil nil "/home/orion/.config/doom/snippets/snippets/yaml-mode/entry" nil "entry")))


;;; Do not edit! File generated at Fri Jul 28 10:03:34 2023
