(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives (cons "org"   (concat proto "://orgmode.org/elpa/")) t)
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))

(package-initialize)

(setq custom-file "~/.emacs.d/my-custom.el")
(load custom-file)

(eval-when-compile
  (require 'use-package))


;; Basic Emacs Settings
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq backup-inhibited t)
(setq auto-save-default nil)
(setq ring-bell-function #'ignore)
(setq-default indent-tabs-mode nil)
(defalias 'yes-or-no-p 'y-or-n-p)
(menu-bar-mode 0)
(tool-bar-mode 0)

;; NXML Settings
(use-package nxml-mode
  :config
  (setq nxml-child-indent 4)
  (setq nxml-outline-child-indent 4))

;; ISpell Settings
(setq-default ispell-dictionary "british")
(setq-default ispell-highlight-face (quote flyspell-incorrect))
(if (eq system-type 'windows-nt)
    (setq-default ispell-program-name "C:/Program Files (x86)/Aspell/bin/aspell.exe"))

;; (quote
;;  (virtualenvwrapper iedit flycheck vlf uuidgen cmake-mode powershell systemd free-keys matlab-mode org-ref column-marker fill-column-indicator button-lock racer dracula-theme markdown-mode php-mode google-this company-jedi flycheck-irony company-irony irony toml-mode itail json-mode elmacro rust-mode ))

                                        ; Utilities

(use-package elmacro
  :ensure t)

(use-package iedit
  :ensure t)

(use-package vlf
  :ensure t
  :config
  (require 'vlf-setup))

(use-package uuidgen
  :ensure t)


(use-package google-this
  :ensure t
  :config
  (google-this-mode 1))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :hook (python-mode . flycheck-mode)
  :config
  (use-package flyckeck-rust
    :ensure t
    :hook (flycheck-mode . flycheck-rust-setup))
  (use-package flycheck-irony
    :ensure t
    :hook (flycheck-mode . flycheck-irony-setup)))


;; Org Mode
(use-package org-plus-contrib
  :ensure t
  :bind ("C-c i" . org-fill-paragraph)
  :hook (org-mode . turn-on-flyspell)
  :config
  (require 'ox-latex)
  (require 'ox-extra)
  (ox-extras-activate '(latex-header-blocks ignore-headlines))
  (use-package org-ref
    :ensure t
    :bind (("C-c M-r" . org-ref-helm-insert-label-link)
	   ("C-c r" . org-ref-helm-insert-ref-link))))

;; ANSYS Mode
(use-package ansys-mode
  :load-path "~/.emacs.d/ansys-mode"
  :mode (("\\.mac\\'" . ansys-mode)
	 ("\\.inp\\'" . ansys-mode)
	 ("\\.anf$" . ansys-mode)))

;; Nastran Mode
(use-package nastran-mode
  :load-path "~/.emacs.d/nastran-mode"
  :mode "\\.bdf\\'")

;; company
(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (use-package company-jedi
    :ensure t )
  (use-package company-irony
    :ensure t )
  
  (defun my/python-mode-hook ()
    (add-to-list 'company-backends 'company-jedi))
  (add-hook 'python-mode-hook 'my/python-mode-hook)
  (add-to-list 'company-backends 'company-irony)
  (add-to-list 'company-backends 'company-go))


                                        ;Languages
(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :config
  (use-package eldoc-mode
    :ensure t)
  (use-package racer-mode
    :ensure t
    :hook ((rust-mode . racer-mode)
           (racer-mode . eldoc-mode))))

(when (eq system-type 'windows-nt)
    (use-package powershell
      :ensure t))

(use-package json-mode
  :ensure t)

(use-package cmake-mode
  :ensure t)

(use-package irony
  :ensure t
  :hook ((c++-mode . irony-mode)
         (c-mode . irony-mode)
         (objc-mode . irony-mode)
         (irony-mode . irony-cdb-autosetup-compile-options))
  :config
  (when (boundp 'w32-pipe-read-delay)
  (setq w32-pipe-read-delay 0))
  (when (boundp 'w32-pipe-buffer-size)
    (setq irony-server-w32-pipe-buffer-size (* 64 1024))))

;; Python Settings
(use-package python-mode
  :commands company-complete)

(use-package virtualenvwrapper
  :ensure t
  :config
  (setq venv-location "~/Envs")
  (venv-initialize-interactive-shells))
;;;
