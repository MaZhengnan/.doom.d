;;; +ui.el -*- lexical-binding: t; -*-

(setq doom-theme 'doom-one)
(setq display-line-numbers-type t)
(setq org-directory "~/org/")
(setq doom-font (font-spec :family "Iosevka" :size 26 :weight 'light)
      doom-variable-pitch-font (font-spec :family "Iosevka Aile" :size 26)
      doom-unicode-font (font-spec :family "Iosevka Fixed" :size 26)
      doom-big-font (font-spec :family "Iosevka Term" :size 40))

(defun my-cjk-font()
  (dolist (charset '(kana han cjk-misc symbol bopomofo))
    (set-fontset-font t charset (font-spec :family "Noto Sans Mono CJK SC"))))

  (add-hook 'after-setting-font-hook #'my-cjk-font)
(prefer-coding-system 'utf-8)
