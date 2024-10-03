;;; Compiled snippets and support files for `web-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'web-mode
                     '(("vue" "<script>\n  export default {\n    name: '${0:`(file-name-base buffer-file-name)`}'$3\n  }\n</script>\n\n<template>\n  ${1:`%`}\n</template>\n\n<style scoped>\n  $2\n</style>\n" "Vue Single-File Component"
                        (equal web-mode-engine "vue")
                        nil nil "/home/lenz/.config/doom/snippets/web-mode/vue" nil "vue")
                       ("video" "<video width=\"${1:560}\" height=\"${2:340}\" controls>\n  <source src=\"${3:path/to/myvideo}.mp4\" type='video/mp4; codecs=\"avc1.42E01E, mp4a.40.2\"'>\n  <source src=\"$3.ogv\" type='video/ogg; codecs=\"theora, vorbis\"'>\n  ${0:Your browser does not support my HTML5 video player}\n</video>" "HTML5 video player" nil nil nil "/home/lenz/.config/doom/snippets/web-mode/video" nil nil)
                       ("php" "<?php `(doom-snippets-format \"%n%s%n\")`$0 ?>" "<?php ... ?>" nil nil nil "/home/lenz/.config/doom/snippets/web-mode/php" nil nil)))


;;; Do not edit! File generated at Thu Apr 25 09:20:49 2024
