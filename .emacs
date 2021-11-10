;;; MyInitFile --- Summary
;;; Commentary:
;;;
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
(put 'downcase-region 'disabled nil)
(menu-bar-mode 0)
(when (display-graphic-p)
  (tool-bar-mode 0))

;;; Code:
;; Essential Lisp macros
(defun reb-query-replace (to-string)
  "Replace TO-STRING re from point with `query-replace-regexp'."
  (interactive
   (progn (barf-if-buffer-read-only)
          (list (query-replace-read-to (reb-target-binding reb-regexp)
                                       "Query replace"  t))))
  (with-current-buffer reb-target-buffer
    (query-replace-regexp (reb-target-binding reb-regexp) to-string)))


(defun clean-org-refs ()
  "Clean UTF-8 strings."
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "–" nil t)
    (replace-match "-")))


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

;;                                         ; Global Settings
(when (eq system-type 'windows-nt)
  (setq exec-path (append exec-path '("C:/unix/bin"))))

;; (when (display-graphic-p)
;;   (use-package smart-mode-line
;;     :ensure t
;;     :init
;;     (sml/setup)))

;; (use-package zenburn
;;   :ensure t
;;   :init
;;   (load-theme 'zenburn t))

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
  ;; The python parser currently doesn't fold if/for/etc. blocks, which is
  ;; something we want. However, the basic indentation parser does support
  ;; this with one caveat: you must toggle the node when your cursor is on
  ;; the line of the if/for/etc. statement you want to collapse. You cannot
  ;; fold the statement by toggling in the body of the if/for/etc.
  (add-to-list 'origami-parser-alist '(python-mode . origami-indent-parser))
  :init
  (global-origami-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rainbow Delimiters -  have delimiters be colored by their depth
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package rainbow-delimiters
  :ensure t
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function rainbow-delimiters-mode "rainbow-delimiters.el"))
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))


(use-package elmacro
  :ensure t)

(use-package eldoc
  :ensure t)

(use-package iedit
  :ensure t)

(use-package projectile
  :ensure t
  :config (projectile-mode 1)
  :bind-keymap
  ("C-c p" . projectile-command-map))

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
  :ensure t)

;; company
(use-package company
  :ensure t
  :init
  (global-company-mode)
  :config
  (setq company-dabbrev-downcase nil))

(use-package ispell
  :config
  (setq ispell-local-dictionary "en_US")
  (setq ispell-local-dictionary-alist
        ;; Please note the list `("-d" "en_US")` contains ACTUAL parameters passed to hunspell
        ;; You could use `("-d" "en_US,en_US-med")` to check with multiple dictionaries
        '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))
  (setq-default ispell-highlight-face (quote flyspell-incorrect))
  (ispell-change-dictionary "en_US" t)
  (if (eq system-type 'windows-nt)
      (setq-default ispell-program-name "c:/hunspell/hunspell.exe")))

                                        ;Configurations
(use-package nginx-mode
  :ensure t)


(use-package flymake-shellcheck
  :ensure t
  :commands flymake-shellcheck-load
  :init
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load))


;;                                         ;Languages
;; Dockerfile
(use-package dockerfile-mode
  :ensure t)

;; NXML Settings
(use-package nxml-mode
  :config
  (setq nxml-child-indent 4)
  (setq nxml-outline-child-indent 4))

(use-package yaml-mode
  :ensure t)

(use-package flycheck-yamllint
  :ensure t
  :defer t
  :init
  (progn
    (eval-after-load 'flycheck
      '(add-hook 'flycheck-mode-hook 'flycheck-yamllint-setup))))


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
  (add-to-list 'org-latex-classes
               '("elsevier"
                 "\\documentclass[]{elsarticle}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (ox-extras-activate '(latex-header-blocks ignore-headlines))
  (setq org-tags-column -72)
  (setf org-highlight-latex-and-related '(latex))
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
  (setq org-latex-hyperref-template
        (concat
         "\\hypersetup{colorlinks,\n"
         "allcolors=black,\n"
         "bookmarksopen=true,\n"
         "pdfauthor={%a},\n"
         "pdftitle={%t},\n"
         "pdfkeywords={%k},\n"
         "pdfsubject={%d},\n"
         "pdfcreator={%c}}\n\n"))
  ;; (setq org-latex-default-packages-alist
  ;;     (append
  ;;      (delq(rassoc '("hyperref" nil) org-latex-default-packages-alist)
  ;;           org-latex-default-packages-alist)
  ;;      '(("colorlinks,citecolor=blue,linktocpage=true" "hyperref" nil))))

  (use-package org-ref
    :ensure t
    :bind (("C-c C-]" . org-ref-helm-insert-label-link)
           ("C-c M-]" . org-ref-helm-insert-ref-link))
    :init
    (let ((refpath (getenv "MY_ORG_REF")))
      (unless (eq refpath nil)
        (setq org-ref-bibliography-notes
              (concat (file-name-as-directory (getenv "MY_ORG_REF")) "notes.org")
              org-ref-default-bibliography
              '((concat (file-name-as-directory (getenv "MY_ORG_REF")) "references.bib"))
              org-ref-pdf-directory
              (concat (file-name-as-directory (getenv "MY_ORG_REF")) "bibtex-pdfs"))
        (unless (file-exists-p org-ref-pdf-directory)
          (make-directory org-ref-pdf-directory t)))))
    (setq org-src-fontify-natively t
        org-confirm-babel-evaluate nil
        org-src-preserve-indentation t)

  (org-babel-do-load-languages
   'org-babel-load-languages '((python . t)))

  (setq org-latex-pdf-process
        '("pdflatex -interaction nonstopmode -output-directory %o %f"
          "bibtex %b"
          "pdflatex -interaction nonstopmode -output-directory %o %f"
          "pdflatex -interaction nonstopmode -output-directory %o %f"))

  (setq bibtex-autokey-year-length 4
        bibtex-autokey-name-year-separator "-"
        bibtex-autokey-year-title-separator "-"
        bibtex-autokey-titleword-separator "-"
        bibtex-autokey-titlewords 2
        bibtex-autokey-titlewords-stretch 1
        bibtex-autokey-titleword-length 5)

  ;;(add-to-list 'org-latex-default-packages-alist '("sort&compress,numbers" "natbib" "") t)
  (add-to-list 'org-latex-default-packages-alist '("" "babel" "") nil)
  
  (require 'org-ref)
  (require 'org-ref-url-utils))

;; LaTeX
(use-package auctex
  :defer t)

(use-package cdlatex
  :ensure t
  :hook (latex-mode . turn-on-cdlatex))

;; Nastran Mode

(use-package nastran-mode
  :load-path "~/.emacs.d/nastran-mode"
  :mode "\\.bdf\\'")

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
  (if (> emacs-major-version 24)
      (electric-pair-local-mode 1)
    (electric-pair-mode 1)))

(use-package php-mode
  :ensure t
  :init
  (irony-mode 0))

;; Rust Languages
(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :config
  (setq rust-format-on-save t))

(use-package racer
  :ensure t
  :hook ((rust-mode . racer-mode)
         (racer-mode . eldoc-mode)
         (racer-mode . company-mode))
  :init
  (add-hook 'racer-mode-hook #'company-mode))

(use-package flycheck-rust
  :ensure t
  :hook (flycheck-mode . flycheck-rust-setup))

;; PowerShell
(when (eq system-type 'windows-nt)
  (use-package powershell
    :ensure t))

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


(use-package irony
  :ensure t
  :hook ((irony-mode . irony-cdb-autosetup-compile-options))
  :config
  (when (eq system-type 'windows-nt)
    (when (boundp 'w32-pipe-read-delay)
      (setq w32-pipe-read-delay 0))
    (when (boundp 'w32-pipe-buffer-size)
      (setq irony-server-w32-pipe-buffer-size (* 64 1024))))
  :init
  (defun my-irony-mode-on ()
    ;; avoid enabling irony-mode in modes that inherits c-mode, e.g: php-mode
    (when (member major-mode irony-supported-major-modes)
      (irony-mode 1)))
  (add-hook 'c++-mode-hook 'my-irony-mode-on)
  (add-hook 'c-mode-hook 'my-irony-mode-on)
  (add-hook 'objc-mode-hook 'my-irony-mode-on)
  (use-package company-irony
    :ensure t
    :hook (irony-mode . company-irony-setup-begin-commands)
    :config
    (add-to-list 'company-backends 'company-irony))
  (use-package flycheck-irony
    :ensure t
    :hook (flycheck-mode . flycheck-irony-setup)))


;; Python Settings
(use-package python-mode
  :hook ((python-mode . flycheck-mode))
  :commands company-complete)

(use-package company-jedi
  :ensure t
  :init
  (add-to-list 'company-backends 'company-jedi))

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
