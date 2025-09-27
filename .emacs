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


                                        ; Global Settings
(use-package exec-path-from-shell
  :straight t
  :ensure t
  :init
  ;; Check if Emacs is running in a graphical environment
  (when (not (eq window-system nil))
    (exec-path-from-shell-initialize)))

(use-package which-key
  :straight t
  :ensure t
  :config
  (which-key-mode))

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
  :config
  (require 'vlf-setup))

(use-package uuidgen
  :straight t
  :ensure t)

(use-package flycheck
  :straight t
  :ensure t)

(use-package elmacro
  :straight t
  :ensure t)


                                        ; LSP
(use-package lsp-mode
  :straight t
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui
  :straight t
  :ensure t
  :commands lsp-ui-mode)

(use-package helm-lsp
  :straight t
  :commands helm-lsp-workspace-symbol)

(use-package projectile
  :straight t
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

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
  :init
  (global-company-mode)
  :config
  (setq company-dabbrev-downcase nil))

(use-package company-box
  :straight t  
  :ensure t
  :hook (company-mode . company-box-mode))


(use-package treemacs
  :straight t
  :ensure t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
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

(use-package treemacs-projectile
  :straight t  
  :after (treemacs projectile)
  :ensure t)

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

; Major Modes
(use-package markdown-mode
  :straight t
  :ensure t
  :hook (markdown-mode . lsp)
  :config
  (require 'lsp-marksman))


(use-package toml-ts-mode
  :hook (toml-ts-mode . lsp))

(use-package dockerfile-ts-mode
  :hook (dockerfile-ts-mode . lsp)
  :config
  (require 'lsp-dockerfile-ls))

(use-package typescript-ts-mode
  :hook (typescript-ts-mode . lsp)
  :config
  (progn
    (setq-local indent-tabs-mode t)                          ;; Use tabs for indentation
    (setq-local tab-width 4)                                 ;; Tabs appear as 4 spaces
    (setq-local typescript-ts-mode-indent-offset 4)))        ;; Indentation level

    
(use-package go-ts-mode
  :hook (go-ts-mode . lsp)
  :config
  (progn
    (setq-local indent-tabs-mode t)                          ;; Use tabs for indentation
    (setq-local tab-width 4)                                 ;; Tabs appear as 4 spaces
    (setq-local typescript-ts-mode-indent-offset 4)))        ;; Indentation level

(use-package yaml-ts-mode
  :mode (
         ("\\.fcc\\'" . yaml-ts-mode)
         ("\\.bu\\'" . yaml-ts-mode))
  :hook (yaml-ts-mode . lsp))

(use-package json-ts-mode
  :mode (("\\.json\\'" .  json-ts-mode))
  :hook (json-ts-mode . lsp))


(use-package python-ts-mode
  :mode (("\\.py\\'" .  json-ts-mode))
  :hook (python-ts-mode . lsp))


;; ;;                                         ;configurations
;; (use-package nginx-mode
;;   :ensure t)

;; (use-package flymake-shellcheck
;;   :ensure t
;;   :commands flymake-shellcheck-load
;;   :init
;;   (add-hook 'sh-mode-hook 'flymake-shellcheck-load))

;; ;;                                                                           ;Languages



;; (use-package nxml-mode
;;   :config
;;   (setq nxml-child-indent 4)
;;   (setq nxml-outline-child-indent 4))


;; ;; (use-package flycheck-yamllint
;; ;;   :ensure t
;; ;;   :defer t
;; ;;   :init
;; ;;   (progn
;; ;;     (eval-after-load 'flycheck
;; ;;       '(add-hook 'flycheck-mode-hook 'flycheck-yamllint-setup))))

;; ;; (use-package flymake-shellcheck
;; ;;   :commands flymake-shellcheck-load
;; ;;   :init
;; ;;   (add-hook 'sh-mode-hook 'flymake-shellcheck-load))

;; (use-package org
;;   :ensure t
;;   :bind ("C-c i" . org-fill-paragraph)
;;   :hook (org-mode . turn-on-flyspell)
;;   :init
;;   (setq org-tags-column -72)
;;   (setf org-highlight-latex-and-related '(latex))
;;   ;; (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))

;;   (org-babel-do-load-languages
;;    'org-babel-load-languages '((python . t))))

;; (use-package scheme
;;   :mode (("\\.jou\\'" . scheme-mode)))

;; ;;                                         ;Languages
;; ;; (if (> emacs-major-version 29)
;; ;;     (use-package csharp-mode
;; ;;       :ensure t
;; ;;       :init
;; ;;       (if (> emacs-major-version 24)
;; ;;           (electric-pair-local-mode 1)
;; ;;         (electric-pair-mode 1))))

;; ;; ;; (use-package php-mode
;; ;; ;;   :ensure t)

;; ;; (use-package php-ts-mode
;; ;;   :init
;; ;;   :config
;; ;;   (use-package geben
;; ;;   :ensure t)
;; ;;   )

;; (use-package rust-mode
;;   :ensure t
;;   :mode "\\.rs\\'"
;;   :config
;;   (setq rust-format-on-save t))

;; ;; ;; (use-package racer
;; ;; ;;   :ensure t
;; ;; ;;   :hook ((rust-mode . racer-mode)
;; ;; ;;          (racer-mode . eldoc-mode)
;; ;; ;;          (racer-mode . company-mode))
;; ;; ;;   :init
;; ;; ;;   (add-hook 'racer-mode-hook #'company-mode))

;; ;; ;; (use-package flycheck-rust
;; ;; ;;   :ensure t
;; ;; ;;   :hook (flycheck-mode . flycheck-rust-setup))

;; ;; PowerShell

;; (use-package powershell
;;   :ensure t)


;; ;; C++
;; (use-package cmake-mode
;;   :ensure t)

;; (use-package cmake-ide
;;   :ensure t
;;   :init
;;   (cmake-ide-setup))

;; ;; ;; (use-package irony
;; ;; ;;   :ensure t
;; ;; ;;   :hook ((irony-mode . irony-cdb-autosetup-compile-options))
;; ;; ;;   :config
;; ;; ;;   (when (eq system-type 'windows-nt)
;; ;; ;;     (when (boundp 'w32-pipe-read-delay)
;; ;; ;;       (setq w32-pipe-read-delay 0))
;; ;; ;;     (when (boundp 'w32-pipe-buffer-size)
;; ;; ;;       (setq irony-server-w32-pipe-buffer-size (* 64 1024))))
;; ;; ;;   :init
;; ;; ;;   (defun my-irony-mode-on ()
;; ;; ;;     ;; avoid enabling irony-mode in modes that inherits c-mode, e.g: php-mode
;; ;; ;;     (when (member major-mode irony-supported-major-modes)
;; ;; ;;       (irony-mode 1)))
;; ;; ;;   (add-hook 'c++-mode-hook 'my-irony-mode-on)
;; ;; ;;   (add-hook 'c-mode-hook 'my-irony-mode-on)
;; ;; ;;   (add-hook 'objc-mode-hook 'my-irony-mode-on)
;; ;; ;;   (use-package company-irony
;; ;; ;;     :ensure t
;; ;; ;;     :hook (irony-mode . company-irony-setup-begin-commands)
;; ;; ;;     :config
;; ;; ;;     (add-to-list 'company-backends 'company-irony))
;; ;; ;;   (use-package flycheck-irony
;; ;; ;;     :ensure t
;; ;; ;;     :hook (flycheck-mode . flycheck-irony-setup)))

;; ;; (use-package python-mode
;; ;;   :hook ((python-mode . flycheck-mode))
;; ;;   :commands company-complete)

;; ;; ;; (use-package company-jedi
;; ;; ;;   :ensure t
;; ;; ;;   :init
;; ;; ;;   (add-to-list 'company-backends 'company-jedi))

;; ;; (when (memq system-type '(windows-nt ms-dos))
;; ;;   (setq-default python-shell-completion-native-enable nil))

;; ;; ;; (use-package virtualenvwrapper
;; ;; ;;   :ensure t
;; ;; ;;   :config
;; ;; ;;   (when (memq system-type '(windows-nt ms-dos))
;; ;; ;;     (setq venv-location (expand-file-name "~/Envs")))
;; ;; ;;   (venv-initialize-interactive-shells)
;; ;; ;;   (venv-initialize-eshell))

;; ;; Matlab
;; (use-package matlab-mode
;;   :ensure t
;;   :mode (("\\.m\\'" . matlab-mode))
;;   :hook turn-off-auto-fill
;;   :init
;;   (setq matlab-shell-command "matlab")
;;   (setq matlab-indent-function 0)
;;   :config
;;   (setq matlab-auto-fill nil))

;; ;; Markdown
;; (use-package markdown-mode
;;   :ensure t
;;   :commands (markdown-mode gfm-mode)
;;   :mode (("README\\.md\\'" . gfm-mode)
;;          ("\\.md\\'" . markdown-mode)
;;          ("\\.markdown\\'" . markdown-mode))
;;   :init (setq markdown-command "multimarkdown"))

;; ;; julia-ts-mode
;; ;; (use-package julia-mode
;; ;;   :ensure t
;; ;;   :mode (("\\.jl\\'" . julia-mode))
;; ;;   :config
;; ;;   (use-package julia-shell
;; ;;     :ensure t
;; ;;     :config
;; ;;     (require 'julia-shell)
;; ;;     (defun my-julia-mode-hooks ()
;; ;;       (require 'julia-shell-mode))
;; ;;     (add-hook 'julia-mode-hook 'my-julia-mode-hooks)
;; ;;     (define-key julia-mode-map (kbd "C-c C-c") 'julia-shell-run-region-or-line)
;; ;;     (define-key julia-mode-map (kbd "C-c C-s") 'julia-shell-save-and-go)))

;; ;; ;;; .emacs ends here
