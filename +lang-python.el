;;; +lang-python.el -*- lexical-binding: t; -*-

(after! lsp-mode
  (require 'lsp-pyright)
  (add-to-list 'lsp-disabled-clients 'pyls)
  (setq lsp-pyright-use-library-code-for-types t)
  (setq lsp-enable-semantic-tokens-enables t)

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
