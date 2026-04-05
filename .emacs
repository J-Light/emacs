;;; MyInitFile --- Summary
;;; Commentary:
;;;
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(if (version< "27.0" emacs-version)
    (setq package-enable-at-startup nil))

(straight-use-package 'use-package)

(setq use-package-always-defer t)
(setq use-package-expand-minimally t)

;; Startup performance
(setq gc-cons-threshold (* 50 1000 1000))
(setq gc-cons-percentage 0.6)
(setq read-process-output-max (* 1024 1024))
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 4 1000 1000))
            (setq gc-cons-percentage 0.1)))

;; Bootstrap custom file settings
(setq custom-file "~/.emacs.d/my-custom.el")
(unless (file-exists-p custom-file)
  (with-temp-buffer
    (write-file custom-file)))
(load custom-file)

;; Basic Emacs Settings
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq backup-inhibited t)

(setq auto-save-default nil)
(setq ring-bell-function #'ignore)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(global-auto-revert-mode 1)

(defalias 'yes-or-no-p 'y-or-n-p)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(global-display-line-numbers-mode 1)
(menu-bar-mode 0)

(when (display-graphic-p)
  (tool-bar-mode 0))
(setq warning-suppress-log-types '((comp)))
(setq warning-suppress-types '((comp)))
(setq native-comp-async-report-warnings-errors nil)

;; Global Settings
(use-package gcmh
  :straight t
  :ensure t
  :demand t
  :init
  (gcmh-mode 1))

(use-package exec-path-from-shell
  :straight t
  :ensure t
  :if (memq window-system '(mac ns x pgtk))
  :demand t
  :init
  (exec-path-from-shell-initialize))

(use-package which-key
  :straight t
  :ensure t
  :demand t
  :config
  (which-key-mode))

(use-package direnv
  :straight t
  :ensure t
  :demand t
  :config
  (direnv-mode))

(use-package savehist
  :demand t
  :init
  (savehist-mode 1))

(use-package vertico
  :straight t
  :ensure t
  :demand t
  :init
  (vertico-mode 1))

(use-package orderless
  :straight t
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :straight t
  :ensure t
  :demand t
  :init
  (marginalia-mode 1))

(use-package consult
  :straight t
  :ensure t
  :bind (("C-s" . consult-line)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ("C-x b" . consult-buffer)
         ("M-y" . consult-yank-pop)))

(use-package embark
  :straight t
  :ensure t
  :bind (("C-." . embark-act)
         ("C-," . embark-dwim)
         ("C-h B" . embark-bindings)))

(use-package embark-consult
  :straight t
  :ensure t
  :after (embark consult))

(use-package rainbow-delimiters
  :straight t
  :ensure t
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function rainbow-delimiters-mode "rainbow-delimiters.el"))
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package iedit
  :straight t
  :bind ("C-;" . iedit-mode)
  :ensure t)

(use-package vlf
  :straight t
  :ensure t
  :commands vlf)

(use-package uuidgen
  :straight t
  :ensure t)

(use-package flycheck
  :straight t
  :ensure t)

(use-package elmacro
  :straight t
  :ensure t)


;; LSP
(use-package lsp-mode
  :straight t
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-idle-delay 0.5)
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred))

(use-package dap-mode
  :straight t
  :ensure t
  :after lsp-mode
  :commands dap-debug
  :config
  (dap-auto-configure-mode 1)
  (require 'dap-python))

(use-package lsp-ui
  :straight t
  :ensure t
  :commands lsp-ui-mode)

(use-package consult-lsp
  :straight t
  :ensure t
  :commands consult-lsp-symbols)

(use-package lsp-pyright
  :straight t
  :ensure t
  :after lsp-mode
  :init
  (setq lsp-pyright-langserver-command "basedpyright")
  (setq lsp-pyright-langserver-command-args '("--stdio"))
  (setq lsp-pyright-auto-import-completions t)
  (setq lsp-pyright-use-library-code-for-types t)
  (setq lsp-pyright-type-checking-mode "basic"))

(with-eval-after-load 'lsp-mode
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection
                     (lambda ()
                       (list "ruff" "server" "--stdio")))
    :activation-fn (lsp-activate-on "python")
    :add-on? t
    :server-id 'ruff)))

(use-package tree-sitter
  :straight t
  :ensure t
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :straight t
  :ensure t
  :after tree-sitter)

(use-package ts-fold
  :straight (ts-fold :type git :host github :repo "emacs-tree-sitter/ts-fold"))

(use-package company
  :straight t
  :ensure t
  :demand t
  :init
  (global-company-mode)
  :config
  (setq company-dabbrev-downcase nil))

(use-package company-box
  :straight t
  :ensure t
  :if (display-graphic-p)
  :hook (company-mode . company-box-mode))


;; Terminal
(use-package vterm
  :straight t
  :ensure t
  :commands (vterm vterm-other-window)
  :bind ("C-c t" . vterm)
  :init
  (setq vterm-max-scrollback 10000)
  (setq vterm-kill-buffer-on-exit t)
  (setq vterm-always-compile-module t)
  (defun my-open-vterm-number (number)
    "Open or switch to a numbered vterm buffer."
    (interactive "nVterm number: ")
    (vterm (format "*vterm-%d*" number)))
  (defun my-vterm-1 ()
    "Open vterm buffer 1."
    (interactive)
    (my-open-vterm-number 1))
  (defun my-vterm-2 ()
    "Open vterm buffer 2."
    (interactive)
    (my-open-vterm-number 2))
  (defun my-vterm-3 ()
    "Open vterm buffer 3."
    (interactive)
    (my-open-vterm-number 3))
  (defun my-vterm-4 ()
    "Open vterm buffer 4."
    (interactive)
    (my-open-vterm-number 4))
  (defun my-vterm-5 ()
    "Open vterm buffer 5."
    (interactive)
    (my-open-vterm-number 5))
  (defun my-vterm-disable-line-numbers ()
    "Disable line numbers in vterm buffers."
    (display-line-numbers-mode 0))
  :hook (vterm-mode . my-vterm-disable-line-numbers))

(define-key global-map (kbd "C-c v 1") #'my-vterm-1)
(define-key global-map (kbd "C-c v 2") #'my-vterm-2)
(define-key global-map (kbd "C-c v 3") #'my-vterm-3)
(define-key global-map (kbd "C-c v 4") #'my-vterm-4)
(define-key global-map (kbd "C-c v 5") #'my-vterm-5)

(use-package vterm-toggle
  :straight t
  :ensure t
  :after vterm
  :bind ("C-`" . vterm-toggle)
  :init
  (setq vterm-toggle-scope 'project))


;; Project + UI
(use-package project
  :straight nil
  :ensure nil)

(use-package treemacs
  :straight t
  :ensure t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (treemacs-project-follow-mode t)
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))
    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))


(use-package treemacs-icons-dired
  :straight t
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :straight t
  :after (treemacs magit)
  :ensure t)

(use-package lsp-treemacs
  :straight t
  :ensure t
  :init
  (setq lsp-treemacs-sync-mode 1)
  :commands lsp-treemacs-errors-list)
;; (treemacs-start-on-boot)

;; Major Modes
(use-package markdown-mode
  :straight t
  :ensure t
  :hook (markdown-mode . lsp-deferred)
  :config
  (require 'lsp-marksman))


(use-package toml-ts-mode
  :hook (toml-ts-mode . lsp-deferred))

(use-package dockerfile-ts-mode
  :hook (dockerfile-ts-mode . lsp-deferred)
  :config
  (require 'lsp-dockerfile-ls))

(use-package typescript-ts-mode
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :init
  (defun my-typescript-tabonly ()
    "Set TypeScript buffers to tab mode."
    (setq-local indent-tabs-mode t)
    (setq-local tab-width 4)
    (setq-local typescript-ts-mode-indent-offset 4))
  :hook ((typescript-ts-mode . lsp-deferred)
         (tsx-ts-mode . lsp-deferred)
         (typescript-ts-mode . my-typescript-tabonly)))

(use-package go-ts-mode
  :hook (go-ts-mode . lsp-deferred))

(use-package yaml-ts-mode
  :mode (
         ("\\.yml\\'" . yaml-ts-mode)
         ("\\.yaml\\'" . yaml-ts-mode)
         ("\\.fcc\\'" . yaml-ts-mode)
         ("\\.bu\\'" . yaml-ts-mode))
  :hook (yaml-ts-mode . lsp-deferred))

(use-package json-ts-mode
  :mode (("\\.json\\'" . json-ts-mode))
  :hook (json-ts-mode . lsp-deferred))

(use-package python-ts-mode
  :mode (("\\.py\\'" . python-ts-mode))
  :hook (python-ts-mode . lsp-deferred))

;; ;; ;;; .emacs ends here
