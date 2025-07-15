;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(load! "+ui")
(load! "+platform")
(load! "+lang-python")
(load! "+cmake-cpp")

;; ======================
;; Tree-sitter Highlighter
;; ======================
(dolist (hook '(python-mode-hook
                c-mode-hook
                c++-mode-hook))
  (add-hook hook #'tree-sitter-mode)
  (add-hook hook #'tree-sitter-hl-mode))


