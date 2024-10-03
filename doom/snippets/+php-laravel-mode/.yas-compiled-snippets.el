;;; Compiled snippets and support files for `+php-laravel-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets '+php-laravel-mode
                     '(("route_inline" "Route::${1:get}('${2:/}', '${3:Controller@Action}')->name('${4:action}');" "Route::<req>(<url>, <route>)->name(<route>)" nil nil nil "/home/lenz/.config/doom/snippets/+php-laravel-mode/route_inline" nil nil)
                       ("route" "Route::${1:get}('${2:/}', function () {\n    `%`$0\n});" "Route::<req>(<url>, <route>)" nil nil nil "/home/lenz/.config/doom/snippets/+php-laravel-mode/route" nil nil)
                       ("nroute" "Route::${1:get}('${2:/}', ['as' => '${3:name}', function () {\n    `%`$0\n});" "Route::<req>(<url>, [..., <route>])" nil nil nil "/home/lenz/.config/doom/snippets/+php-laravel-mode/nroute" nil nil)
                       ("mig" "Schema::table('$1', function (Blueprint $table) {\n    `%`$0\n});\n" "Laravel Migration method" nil nil nil "/home/lenz/.config/doom/snippets/+php-laravel-mode/migration" nil "mig")
                       ("match" "Route::match([${1:'get', 'post'}], '${2:/}', function () {\n    `%`$0\n});" "Route::match([...], <func>)" nil nil nil "/home/lenz/.config/doom/snippets/+php-laravel-mode/match" nil nil)
                       ("group" "Route::group([$1], function() {\n    `%`$0\n});" "Route::group([<attrs>], <func>)" nil nil nil "/home/lenz/.config/doom/snippets/+php-laravel-mode/group" nil nil)
                       ("any" "Route::any('${1:/}', function () {\n    `%`$0\n});" "Route::any(<uri>, <func>)" nil nil nil "/home/lenz/.config/doom/snippets/+php-laravel-mode/any" nil nil)
                       ("__" "<?php\n\nnamespace `(+php-laravel-mode--get-namespace)`;\n\nclass `(+php-laravel-mode--get-class-name)`\n{\n    $0\n}" "PHP template" nil nil nil "/home/lenz/.config/doom/snippets/+php-laravel-mode/__" nil nil)))


;;; Do not edit! File generated at Thu Apr 25 09:20:46 2024
