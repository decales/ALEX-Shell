(defpackage alex-shell (:use cl))
(in-package alex-shell)

(load "src/input.lsp")
(load "src/parse.lsp")
(load "src/engine.lsp")
(load "src/shell.lsp")

(startShell)

