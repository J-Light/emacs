;;; MyInitFile --- Summary
;;; Commentary:
;;;
(require 'package)
(add-to-list 'package-archives (cons "org"   "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives (cons "melpa" "https://melpa.org/packages/") t)

(package-initialize)

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

(defalias 'yes-or-no-p 'y-or-n-p)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(global-display-line-numbers-mode 1)
(menu-bar-mode 0)

(when (display-graphic-p)
  (tool-bar-mode 0))
(setq warning-suppress-log-types '((comp)))
(setq warning-suppress-types '((comp)))


;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;;                                         ; Global Settings
(when (eq system-type 'windows-nt)
  (setq exec-path (append exec-path '("C:/unix/bin"))))

(use-package exec-path-from-shell
  :ensure t
  :init
  ;; Check if Emacs is running in a graphical environment
  (when (not (eq window-system nil))
    (exec-path-from-shell-initialize)))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package tree-sitter
  :ensure t
  :config
  ;; activate tree-sitter on any buffer containing code for which it has a parser available
  (global-tree-sitter-mode)
  ;; you can easily see the difference tree-sitter-hl-mode makes for python, ts or tsx
  ;; by switching on and off
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  (add-to-list 'tree-sitter-major-mode-language-alist '(templ-ts-mode . templ))
  )

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

(use-package templ-ts-mode
  :ensure t)

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (defvar lsp-templ-server-command '("templ" "lsp"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection
									 (lambda () lsp-templ-server-command))
					:activation-fn (lsp-activate-on "templ")
					:major-modes '(templ-ts-mode)
					:server-id 'templ-ls))
  (add-to-list 'lsp-language-id-configuration '(templ-ts-mode . "templ"))
  :hook (
         (sh-mode . lsp)
         (typescript-ts-mode . lsp)
		 (go-ts-mode . lsp)
		 (templ-ts-mode . lsp)
		 (php-ts-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

										; optionally
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package lsp-treemacs
  :ensure t
  :init
  (setq lsp-treemacs-sync-mode 1)
  :commands lsp-treemacs-errors-list)

(use-package markdown-mode
  :ensure t
  :hook (markdown-mode . lsp)
  :config
  (require 'lsp-marksman))

(use-package origami
  :ensure t
  :commands (origami-mode)
  :bind (("C-c TAB" . origami-recursively-toggle-node)
         ("C-c o a" . origami-toggle-all-nodes)
         ("C-c o TAB" . origami-toggle-node)
         ("C-c o o" . origami-show-only-node)
         ("C-c o u" . origami-undo)
         ("C-c o U" . origami-redo)
         ("C-c o C-r" . origami-reset))
  :config
  (setq origami-show-fold-header t)
  (add-to-list 'origami-parser-alist '(python-mode . origami-indent-parser))
  :init
  (global-origami-mode))

(use-package rainbow-delimiters
  :ensure t
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function rainbow-delimiters-mode "rainbow-delimiters.el"))
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package toml-mode
  :ensure t)

(use-package elmacro
  :ensure t)

(use-package iedit
  :bind ("C-;" . iedit-mode)
  :ensure t)

(use-package vlf
  :ensure t
  :config
  (require 'vlf-setup))

(use-package uuidgen
  :ensure t)

(use-package flycheck
  :ensure t)

(use-package company
  :ensure t
  :init
  (global-company-mode)
  :config
  (setq company-dabbrev-downcase nil))

(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))

                                        ;Configurations
(use-package nginx-mode
  :ensure t)

(use-package flymake-shellcheck
  :ensure t
  :commands flymake-shellcheck-load
  :init
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load))

										;Languages
(use-package dockerfile-mode
  :ensure t)

;; Define a setup function for TypeScript mode
(defun my-typescript-setup ()
  "Custom configurations for TypeScript mode."
  (setq-local tab-width 2)
  (setq-local indent-tabs-mode nil)
  (setq-local typescript-ts-mode-indent-offset 2))

(use-package typescript-ts-mode
  :mode "\\.ts\\'"
  :hook (typescript-ts-mode . my-typescript-setup))

(use-package go-ts-mode
  :mode
  "\\.go\\'"
  ("go\\.mod\\'" . go-mod-ts-mode)
  :custom
  (indent-tabs-mode t)
  :config
  (setq-default tab-width 4)
  (setq go-ts-mode-indent-offset 4))

(use-package nxml-mode
  :config
  (setq nxml-child-indent 4)
  (setq nxml-outline-child-indent 4))

(use-package yaml-mode
  :ensure t
  :mode
  "\\.fcc\\'"
  "\\.bu\\'"
  )

;; (use-package flycheck-yamllint
;;   :ensure t
;;   :defer t
;;   :init
;;   (progn
;;     (eval-after-load 'flycheck
;;       '(add-hook 'flycheck-mode-hook 'flycheck-yamllint-setup))))

(use-package flymake-shellcheck
  :commands flymake-shellcheck-load
  :init
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load))

(use-package org-plus-contrib
  :ensure t
  :bind ("C-c i" . org-fill-paragraph)
  :hook (org-mode . turn-on-flyspell)
  :init
  (require 'ox-latex)
  (require 'ox-extra)  
  (setq org-tags-column -72)
  (setf org-highlight-latex-and-related '(latex))
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))

  (org-babel-do-load-languages
   'org-babel-load-languages '((python . t))))

(use-package auctex
  :defer t)

(use-package cdlatex
  :ensure t
  :hook (latex-mode . turn-on-cdlatex))

(use-package scheme
  :mode (("\\.jou\\'" . scheme-mode)))

                                        ;Languages
(if (> emacs-major-version 29)
    (use-package csharp-mode
      :ensure t
      :init
      (if (> emacs-major-version 24)
          (electric-pair-local-mode 1)
        (electric-pair-mode 1))))

;; (use-package php-mode
;;   :ensure t)

(use-package php-ts-mode
  :init
  :config
  (use-package geben
	:ensure t)
  )


(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :config
  (setq rust-format-on-save t))

;; (use-package racer
;;   :ensure t
;;   :hook ((rust-mode . racer-mode)
;;          (racer-mode . eldoc-mode)
;;          (racer-mode . company-mode))
;;   :init
;;   (add-hook 'racer-mode-hook #'company-mode))

;; (use-package flycheck-rust
;;   :ensure t
;;   :hook (flycheck-mode . flycheck-rust-setup))

;; PowerShell

(use-package powershell
  :ensure t)

;; JSON Model
(use-package json-mode
  :ensure t)

;; C++
(use-package cmake-mode
  :ensure t)

(use-package cmake-ide
  :ensure t
  :init
  (cmake-ide-setup))

;; (use-package irony
;;   :ensure t
;;   :hook ((irony-mode . irony-cdb-autosetup-compile-options))
;;   :config
;;   (when (eq system-type 'windows-nt)
;;     (when (boundp 'w32-pipe-read-delay)
;;       (setq w32-pipe-read-delay 0))
;;     (when (boundp 'w32-pipe-buffer-size)
;;       (setq irony-server-w32-pipe-buffer-size (* 64 1024))))
;;   :init
;;   (defun my-irony-mode-on ()
;;     ;; avoid enabling irony-mode in modes that inherits c-mode, e.g: php-mode
;;     (when (member major-mode irony-supported-major-modes)
;;       (irony-mode 1)))
;;   (add-hook 'c++-mode-hook 'my-irony-mode-on)
;;   (add-hook 'c-mode-hook 'my-irony-mode-on)
;;   (add-hook 'objc-mode-hook 'my-irony-mode-on)
;;   (use-package company-irony
;;     :ensure t
;;     :hook (irony-mode . company-irony-setup-begin-commands)
;;     :config
;;     (add-to-list 'company-backends 'company-irony))
;;   (use-package flycheck-irony
;;     :ensure t
;;     :hook (flycheck-mode . flycheck-irony-setup)))

(use-package python-mode
  :hook ((python-mode . flycheck-mode))
  :commands company-complete)

;; (use-package company-jedi
;;   :ensure t
;;   :init
;;   (add-to-list 'company-backends 'company-jedi))

(when (memq system-type '(windows-nt ms-dos))
  (setq-default python-shell-completion-native-enable nil))

(use-package virtualenvwrapper
  :ensure t
  :config
  (when (memq system-type '(windows-nt ms-dos))
    (setq venv-location (expand-file-name "~/Envs")))
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell))

;; Matlab
(use-package matlab-mode
  :ensure t
  :mode (("\\.m\\'" . matlab-mode))
  :hook turn-off-auto-fill
  :init
  (setq matlab-shell-command "matlab")
  (setq matlab-indent-function 0)
  :config
  (setq matlab-auto-fill nil))

;; Markdown
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package julia-mode
  :ensure t
  :mode (("\\.jl\\'" . julia-mode))
  :config
  (use-package julia-shell
    :ensure t
    :config
    (require 'julia-shell)
    (defun my-julia-mode-hooks ()
      (require 'julia-shell-mode))
    (add-hook 'julia-mode-hook 'my-julia-mode-hooks)
    (define-key julia-mode-map (kbd "C-c C-c") 'julia-shell-run-region-or-line)
    (define-key julia-mode-map (kbd "C-c C-s") 'julia-shell-save-and-go)))

;;; .emacs ends here
