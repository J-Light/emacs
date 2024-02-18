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

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook (
         (sh-mode . lsp)
         (typescript-ts-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; optionally
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)
(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)

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

(use-package typescript-ts-mode
  :mode "\\.ts\\'")

(use-package nxml-mode
  :config
  (setq nxml-child-indent 4)
  (setq nxml-outline-child-indent 4))

(use-package yaml-mode
  :ensure t
  :mode "\\.fcc\\'")

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

(use-package php-mode
  :ensure t)

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
