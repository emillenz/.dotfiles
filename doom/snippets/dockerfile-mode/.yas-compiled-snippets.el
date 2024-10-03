;;; Compiled snippets and support files for `dockerfile-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'dockerfile-mode
                     '(("workdir" "WORKDIR ${1:/path/to/workdir}" "WORKDIR ..." nil nil nil "/home/lenz/.config/doom/snippets/dockerfile-mode/workdir" nil nil)
                       ("volume" "VOLUME ${1:/path}" "VOLUME ..." nil nil nil "/home/lenz/.config/doom/snippets/dockerfile-mode/volume" nil nil)
                       ("user" "USER ${1:daemon}" "USER ..." nil nil nil "/home/lenz/.config/doom/snippets/dockerfile-mode/user" nil nil)
                       ("stopsignal" "STOPSIGNAL ${1:9}" "STOPSIGNAL <signal>" nil nil nil "/home/lenz/.config/doom/snippets/dockerfile-mode/stopsignal" nil nil)
                       ("onbuild" "ONBUILD $0" "ONBUILD <instruction>" nil nil nil "/home/lenz/.config/doom/snippets/dockerfile-mode/onbuild" nil nil)
                       ("label" "LABEL ${1:key}=${2:value}" "LABEL <key>=<value> ..." nil nil nil "/home/lenz/.config/doom/snippets/dockerfile-mode/label" nil nil)
                       ("from" "FROM ${1:phusion/baseimage:${2:latest}}" "FROM <image>[:<tag|digest>]" nil nil nil "/home/lenz/.config/doom/snippets/dockerfile-mode/from" nil nil)
                       ("expose" "EXPOSE ${1:80}" "EXPOSE <port> [<port> ...]" nil nil nil "/home/lenz/.config/doom/snippets/dockerfile-mode/expose" nil nil)
                       ("env" "ENV ${1:var}=${2:value}" "ENV <key>=<value> ..." nil nil nil "/home/lenz/.config/doom/snippets/dockerfile-mode/env" nil nil)
                       ("entrypoint" "ENTRYPOINT ${1:command}" "ENTRYPOINT <command>" nil nil nil "/home/lenz/.config/doom/snippets/dockerfile-mode/entrypoint" nil nil)
                       ("dockerize-ubuntu" "RUN apt-get update && apt-get install -y wget\n\nENV DOCKERIZE_VERSION ${1:v0.6.1}\nRUN wget https://github.com/jwilder/dockerize/releases/download/${DOCKERIZE_VERSION}/dockerize-linux-amd64-${DOCKERIZE_VERSION}.tar.gz \\\n    && tar -C /usr/local/bin -xzvf dockerize-linux-amd64-${DOCKERIZE_VERSION}.tar.gz \\\n    && rm dockerize-linux-amd64-${DOCKERIZE_VERSION}.tar.gz\n$0" "dockerize for Ubuntu Images" nil nil
                        ((yas-indent-line 'fixed))
                        "/home/lenz/.config/doom/snippets/dockerfile-mode/dockerize-ubuntu" nil nil)
                       ("dockerize-alpine" "RUN apk add --no-cache openssl\n\nENV DOCKERIZE_VERSION ${1:v0.6.1}\nRUN wget https://github.com/jwilder/dockerize/releases/download/${DOCKERIZE_VERSION}/dockerize-alpine-linux-amd64-${DOCKERIZE_VERSION}.tar.gz \\\n    && tar -C /usr/local/bin -xzvf dockerize-alpine-linux-amd64-${DOCKERIZE_VERSION}.tar.gz \\\n    && rm dockerize-alpine-linux-amd64-${DOCKERIZE_VERSION}.tar.gz\n$0\n" "dockerize for Alpine Images" nil nil
                        ((yas-indent-line 'fixed))
                        "/home/lenz/.config/doom/snippets/dockerfile-mode/dockerize-alpine" nil nil)
                       ("copy" "COPY ${1:src} ${2:dest}" "COPY <src> <dest>" nil nil nil "/home/lenz/.config/doom/snippets/dockerfile-mode/copy" nil nil)
                       ("arg" "ARG ${1:name}${2:=${3:value}}" "ARG <name>[=<default value>]" nil nil nil "/home/lenz/.config/doom/snippets/dockerfile-mode/arg" nil nil)
                       ("add" "ADD ${1:src} ${2:dest}" "ADD <src> <dest>" nil nil nil "/home/lenz/.config/doom/snippets/dockerfile-mode/add" nil nil)))


;;; Do not edit! File generated at Thu Apr 25 09:20:47 2024
