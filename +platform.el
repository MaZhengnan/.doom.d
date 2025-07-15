;;; +platform.el -*- lexical-binding: t; -*-

(defvar user-platform nil)
(defvar my-conda-path nil)
(defvar my-clangd-path nil)

(cond
 ((eq system-type 'gnu/linux)
  (setq user-platform "linux")
  (setq my-conda-path "~/miniconda3")
  (setq my-clangd-path "/usr/bin/clangd"))

 ((eq system-type 'darwin)
  (setq user-platform "macos")
  (setq my-conda-path "~/miniconda3")
  (setq my-clangd-path "/opt/homebrew/opt/llvm/bin/clangd"))

 ((eq system-type 'windows-nt)
  (setq user-platform "windows")
  (setq my-conda-path (concat (getenv "USERPROFILE") "/scoop/apps/miniconda3/current"))
  (setq my-clangd-path (concat (getenv "USERPROFILE") "/scoop/apps/llvm/current/bin/clangd.exe"))))
