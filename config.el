;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;
;;; Add to $DOOMDIR/config.el
;;;
;;;


;;; ~/.doom.d/config.el

;; ======================
;; Doom UI Settings
;; ======================
;;; ~/.doom.d/config.el

;; ======================
;; UI & Theme
;; ======================

(setq doom-theme 'doom-one)
(setq display-line-numbers-type t)
(setq org-directory "~/org/")

;; Font setup using Iosevka family
(setq doom-font (font-spec :family "Iosevka" :size 26 :weight 'light)
      doom-variable-pitch-font (font-spec :family "Iosevka Aile" :size 26)
      doom-unicode-font (font-spec :family "Iosevka Fixed" :size 26)
      doom-big-font (font-spec :family "Iosevka Term" :size 40))

(prefer-coding-system 'utf-8)

;; ======================
;; Platform-specific path detection
;; ======================

(cond
 ((eq system-type 'gnu/linux)
  (setq user-platform "linux")
  (setq my-clangd-path "/usr/bin/clangd")
  (setq my-conda-path (expand-file-name "~/miniconda3")))

 ((eq system-type 'darwin)
  (setq user-platform "macos")
  (setq my-clangd-path "/opt/homebrew/opt/llvm/bin/clangd")
  (setq my-conda-path (expand-file-name "~/miniconda3")))

 ((eq system-type 'windows-nt)
  (setq user-platform "windows")
  (setq my-clangd-path (concat (getenv "USERPROFILE") "/scoop/apps/llvm/current/bin/clangd.exe"))
  (setq my-conda-path (concat (getenv "USERPROFILE") "/scoop/apps/miniconda3/current"))))

;; ======================
;; LSP Configuration
;; ======================

(setq lsp-clients-clangd-executable my-clangd-path)
(setq lsp-semantic-tokens-enable t) ; enable rich semantic highlight for LSP
(setq lsp-pyright-use-library-code-for-types t)

;; ~/.doom.d/config.el
(after! lsp-mode
  (require 'lsp-pyright)
  (add-to-list 'lsp-disabled-clients 'pyls)
  (setq lsp-pyright-use-library-code-for-types t)
  (setq lsp-enable-semantic-highlighting t)

  ;; 自动为 python 启用 pyright
  (add-hook 'python-mode-hook #'(lambda ()
                                   (require 'lsp-pyright)
                                   (lsp))))
(add-hook 'python-mode-hook
  (lambda ()
    (when (bound-and-true-p conda-env-current-name)
      (setq-local python-shell-interpreter (executable-find "python")))))

;; ======================
;; Conda Integration
;; ======================

(setq conda-anaconda-home my-conda-path)
(setq conda-env-home-directory conda-anaconda-home)
(setq conda-env-subdirectory "envs")
(conda-env-initialize-interactive-shells)
(conda-env-autoactivate-mode t)

;; Optional: default conda env at startup
;; (conda-env-activate "dev")

;; ======================
;; Org + Jupyter
;; ======================

(use-package! jupyter
  :after org
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((jupyter . t)
     (python . t))))

;; ======================
;; Tree-sitter Highlighter
;; ======================

(dolist (hook '(python-mode-hook
                c-mode-hook
                c++-mode-hook))
  (add-hook hook #'tree-sitter-mode)
  (add-hook hook #'tree-sitter-hl-mode))



(defun my/cmake-configure ()
  "Run cmake -S . -B build and link compile_commands.json."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (compile "cmake -S . -B build -DCMAKE_EXPORT_COMPILE_COMMANDS=ON")
    (run-at-time
     "1 sec" nil
     (lambda ()
       (let ((cmd (cond
                   ((eq system-type 'windows-nt)
                    "cmd /c copy build\\compile_commands.json .")
                   ((memq system-type '(gnu/linux darwin))
                    "ln -sf build/compile_commands.json ."))))
         (when cmd (compile cmd)))))))

(defun my/cmake-build ()
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (compile "cmake --build build")))

(defun my/cmake-run ()
  (interactive)
  (let* ((default-directory (projectile-project-root))
         (exe (cond
               ((eq system-type 'windows-nt) "build\\main.exe")
               (t "./build/main"))))
    (compile exe)))

(defun my/cmake-test ()
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (compile "ctest --test-dir build")))

(defun my/cmake-debug ()
  (interactive)
  (let* ((default-directory (projectile-project-root))
         (cmd (cond
               ((eq system-type 'windows-nt) "gdb build\\main.exe")
               (t "gdb ./build/main"))))
    (compile cmd)))


(map! :after cc-mode
      :map (c-mode-map c++-mode-map)
      :localleader
      :prefix ("c" . "CMake")
      :desc "Configure" "c" #'my/cmake-configure
      :desc "Build"     "b" #'my/cmake-build
      :desc "Run"       "r" #'my/cmake-run
      :desc "CTest"     "t" #'my/cmake-test
      :desc "Debug"     "d" #'my/cmake-debug)

;; 在 CMake mode 下也添加相同快捷键
(use-package! cmake-mode
  :mode "CMakeLists\\.txt\\'"
  :init
  (map! :map cmake-mode-map
        :localleader
        :prefix ("c" . "CMake")
        :desc "Configure" "c" #'my/cmake-configure
        :desc "Build"     "b" #'my/cmake-build
        :desc "Run"       "r" #'my/cmake-run
        :desc "CTest"     "t" #'my/cmake-test
        :desc "Debug"     "d" #'my/cmake-debug))


;; Optional: Install language parsers
;; M-x tree-sitter-install-language-grammar
