;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(load! "+ui")
(load! "+platform")
(load! "+lang-python")
(load! "+cmake-cpp")


;; 快速 ESC 配置（jk/kj/jj）
(setq evil-escape-key-sequence "jk"
      evil-escape-delay 0.3   ; 按键间最大间隔秒数
      evil-escape-excluded-major-modes '(vterm-mode eshell-mode term-mode))
(evil-escape-mode 1)
;; ======================
;; Tree-sitter Highlighter
;; ======================
(dolist (hook '(python-mode-hook
                c-mode-hook
                c++-mode-hook))
  (add-hook hook #'tree-sitter-mode)
  (add-hook hook #'tree-sitter-hl-mode))


