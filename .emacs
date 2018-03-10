(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives (cons "org"   (concat proto "://orgmode.org/elpa/")) t)
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))

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
(menu-bar-mode 0)
(when (display-graphic-p)
  (tool-bar-mode 0))

;; Essential Lisp macros
(defun reb-query-replace (to-string)
  "Replace current RE from point with `query-replace-regexp'."
  (interactive
   (progn (barf-if-buffer-read-only)
          (list (query-replace-read-to (reb-target-binding reb-regexp)
                                       "Query replace"  t))))
  (with-current-buffer reb-target-buffer
    (query-replace-regexp (reb-target-binding reb-regexp) to-string)))

(defun pdf-paste-title ()
  "Paste a multiline title from a pdf."
  (interactive)
  (generate-new-buffer "*PDFpastetemp*")
  (switch-to-buffer "*PDFpastetemp*")
  (yank nil)
  (goto-char 0)
  (perform-replace "\n" " " nil nil nil)
  (goto-char 0)
  (perform-replace ":" " -" nil nil nil)
  (goto-char 0)
  (kill-region nil nil 1)
  (kill-buffer)
  (yank nil))







;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))


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
  (use-package flycheck-rust
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
  :init
  (require 'ox-latex)
  (require 'ox-extra)
  (add-to-list 'org-latex-classes
               '("aiaa"
                 "\\documentclass[]{new-aiaa}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (ox-extras-activate '(latex-header-blocks ignore-headlines))
  (setf org-highlight-latex-and-related '(latex))
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5)))

(use-package org-ref
  :requires org-plus-contrib
  :ensure t
  :bind (("C-c M-r" . org-ref-helm-insert-label-link)
         ("C-c r" . org-ref-helm-insert-ref-link)))

;; Nastran Mode
(use-package nastran-mode
  :load-path "~/.emacs.d/nastran-mode"
  :mode "\\.bdf\\'")

;; company
(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-dabbrev-downcase nil)
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

                                        ; ANSYS
;; ANSYS Mode
(use-package ansys-mode
  :load-path "~/.emacs.d/ansys-mode"
  :mode (("\\.mac\\'" . ansys-mode)
	 ("\\.inp\\'" . ansys-mode)
	 ("\\.anf$" . ansys-mode)))

(use-package scheme
  :mode (("\\.jou\\'" . scheme-mode)))


                                        ;Languages
(use-package csharp-mode
  :ensure t
  :init
  (defun my-csharp-mode-hook ()
    ;; enable the stuff you want for C# here
    (if (> emacs-major-version 24)
        (electric-pair-local-mode 1)
      (electric-pair-mode 1)))
  (add-hook 'csharp-mode-hook 'my-csharp-mode-hook))

(use-package php-mode
  :ensure t)


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

;; Issue with windows
(when (memq system-type '(windows-nt ms-dos))
  (setq-default python-shell-completion-native-enable nil))

(use-package virtualenvwrapper
  :ensure t
  :init
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell)
  (when (memq system-type '(windows-nt ms-dos))
    (setq venv-location (expand-file-name "~/Envs")))
  (setq python-environment-directory venv-location))

;; Matlab
(use-package matlab-mode
  :ensure t
  :mode (("\\.m\\'" . matlab-mode))
  :init
  (setq matlab-shell-command "matlab")
  (setq matlab-indent-function t))

;;;

