;;; init.el --- My emacs config -*- lexical-binding: t -*-

;;; Commentary:
;; Go placidly among the noise and the haste, and remember what peace there may be in silence. 

;;; Code:

;;; Package Management

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq package-archive-priorities '(("gnu"   .  8)
				   ("nongnu" . 7)
				   ("melpa"  . 4)))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))


(setq debug-on-error t)

(add-to-list 'load-path "~/.emacs.d/lisp/")

(use-package emacs
  ;; Some free keys!
  :bind (("M-i")
	 ("C-z")
	 ("C-x C-z")
	 ("M-\\")
	 ("C-x C-o")
	 ("C-x k" . (lambda () (interactive) (kill-buffer (current-buffer))))
	 ("C-d" . delete-forward-char)
	 ("M-n" . next-sexp)
	 ("M-p" . previous-sexp)
	 ("M-l" . downcase-dwim)
	 ("M-c" . capitalize-dwim)
	 ("M-u" . upcase-dwim))
  :init
  (load-file "~/.emacs.d/lisp/private.el")
  :config
  (setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
  (setq auto-save-file-name-transforms `((".*" "~/.emacs.d/auto-save-list/" t)))
  (setq create-lockfiles nil)
  (setq custom-file "~/.emacs.d/custom.el")
  (load custom-file 'noerror 'nomessage)
  (setq find-function-C-source-directory "~/src/emacs-29.3/src/")
  (setq inhibit-startup-screen t
	kill-whole-line t
	enable-recursive-minibuffers t
	save-interprogram-paste-before-kill t
	initial-scratch-message ""
	y-or-n-p-use-read-key t
	use-short-answers t
	sentence-end-double-space nil)

  (set-face-attribute 'default nil :family "Iosevka")

  (put 'narrow-to-region 'disabled nil)
  (put 'help-fns-edit-variable 'disabled nil)
  (put 'narrow-to-page 'disabled nil)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
	'(read-only t cursor-intangible t face minibuffer-prompt))
  (setq read-extended-command-predicate #'command-completion-default-include-p))

;;; Builtin

(use-package frame
  :config
  (blink-cursor-mode 0))

(use-package dired
  :hook (dired-mode . hl-line-mode)
  :custom
  (dired-dwim-target t)
  (dired-listing-switches "-lah"))

(use-package savehist
  :init (savehist-mode))

(use-package saveplace
  :config
  (setq save-place-limit 1000)
  (save-place-mode 1))

(use-package recentf
  :config (recentf-mode 1))

(use-package elec-pair
  :config (electric-pair-mode 1))

(use-package crm
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator))

(use-package eldoc
  :defer t)

(use-package autorevert
  :delight auto-revert-mode
  :hook ((text-mode prog-mode) . auto-revert-mode)
  :config
  (setq auto-revert-avoid-polling t))

(use-package text-mode
  :hook (text-mode . visual-line-mode))

(use-package eshell
  :bind (("C-c $" . eshell)))

;;; Editing

(use-package paredit
  :hook ((emacs-lisp-mode . paredit-mode)
         (lisp-mode . paredit-mode)
         (scheme-mode . paredit-mode))
  :bind (:map paredit-mode-map
	      ("M-s" . nil)))


;;; Completion

(use-package vertico
  :ensure t
  :bind (:map vertico-map
	      ("M-;" . exit-minibuffer)
	      ("M-q" . vertico-quick-exit)
	      ("C-M-n"   . vertico-next-group)
              ("C-M-p"   . vertico-previous-group))
  :init 
  (vertico-mode)
  (vertico-reverse-mode 1)
  :config
  (setq vertico-cycle t
	vertico-resize t
	vertico-count 12))

(use-package vertico-repeat
  :hook (minibuffer-setup . vertico-repeat-save)
  :bind (("M-R" . vertico-repeat)
	 :map vertico-map
	 ("M-P" . vertico-repeat-previous)
	 ("M-N" . vertico-repeat-next)))

(use-package vertico-multiform
  :after vertico
  :init (vertico-multiform-mode 1)
  :config
  (setq vertico-multiform-categories
	'((embark-keybinding grid)
	  (file grid)
	  (imenu buffer)
	  (history reverse))))

(use-package vertico-directory
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :after vertico
  :bind (:map vertico-map
         ("DEL"   . vertico-directory-delete-char)
         ("M-DEL" . vertico-directory-delete-word)
         ("RET"   . vertico-directory-enter)))


(use-package vertico-posframe
  :disabled t
  :custom
  (vertico-posframe-vertico-multiform-key "M-C")
  :config
  (vertico-posframe-mode 1))


(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless flex basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))
				   (command (styles initials substring orderless)))))


(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init (marginalia-mode))

(use-package embark
  :ensure t
  :hook (embark-collect-mode . hl-line-mode)
  :bind (("C-."   . embark-act)
	 ("C-:"   . embark-act-all)
	 ("M-."   . embark-dwim)
	 ("C-h b" . embark-bindings)
	 ("C-h y" . embark-bindings-in-keymap)
	 ("C-h E" . embark-on-last-message)
	 (:map vertico-map
	       ("M-c" . embark-collect)
	       ("M-e" . embark-export)
	       ("C-SPC" . (lambda () (interactive) (embark-select) (vertico-next)))
	       ("C-M-." . embark-act-all))
	 (:map embark-collect-mode-map
               ("a")
               ("." . embark-act)
	       ("SPC" . (lambda () (interactive) (embark-select) (next-line)))
               ("F" . consult-focus-lines))
	 (:map embark-identifier-map
               ("(" . insert-parentheses)
               ("[" . insert-pair-map)
	       ("D" . dictionary-lookup-definition))
	 :map embark-general-map
	 ("G" . pj/embark-google-search)
	 :map embark-expression-map
	 ("a" . embark-act-with-eval)
	 :map embark-variable-map
	 ("a" . embark-act-with-eval))
  :config
  (setq prefix-help-command #'embark-prefix-help-command)
  (setq embark-confirm-act-all nil)
  (setq embark-indicators
        '(embark-minimal-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator))
  (setq embark-keymap-prompter-key "'")
  (setf (alist-get 'kill-buffer embark-pre-action-hooks) nil)

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
		 q                 (window-parameters (mode-line-format . none))))

  (defun pj/embark-google-search (term)
    (interactive "sSearch Term: ")
    (browse-url
     (format "http://google.com/search?q=%s" term)))

  (defun embark-act-with-eval (expression)
    "Evaluate EXPRESSION and call `embark-act' on the result."
    (interactive "sExpression: ")
    (with-temp-buffer
      (insert (eval (read expression)))
      (embark-act)))
  (defun embark-on-last-message (arg)
    "Act on the last message displayed in the echo area."
    (interactive "P")
    (with-current-buffer "*Messages*"
      (goto-char (1- (point-max)))
      (embark-act arg)))
  )

(use-package embark-consult
  :ensure t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-h j" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)
         ("C-x C-b" . consult-buffer)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x t b" . consult-buffer-other-tab)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ("M-X" . consult-mode-command)
	 ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g M-i" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history) ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history) ;; orig. isearch-edit-string
         ("M-s l" . consult-line) ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)	;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-h" . consult-history)
	 )
  :custom
  (consult-narrow-key "<")
  (completion-in-region-function #'consult-completion-in-region)
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  :init
  ;; Registers
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)

  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))
  )


(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
	 ("C-M-d" . consult-dir)
         ("C-M-k" . consult-dir-jump-file))
  :config
  (setq consult-dir-shadow-filenames nil))


(use-package corfu
  :bind (:map corfu-map
	      ([remap next-line] . nil)
	      ([remap previous-line] . nil)
	      ("C-<return>" . (lambda () (interactive) (corfu-quit) (newline)))
	      ("M-RET" . (lambda () (interactive) (corfu-insert) (newline)))
	      ("M-;" . corfu-insert)
	      ("M-." . corfu-show-location)
	      ([tab] . corfu-next))
  :custom 
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-separator ?\s)
  (corfu-quit-no-match 'separator)
  (corfu-on-exact-match 'insert) ;; Configure handling of exact matches
  (corfu-echo-delay (cons 0.5 0.5))
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-echo-mode))

(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))



;;; Navigation

(use-package ace-window
  :disabled t
  :bind (("M-o" . pj/switch-window))
  :custom (ace-window-posframe-mode 1)
  :config
  (defun pj/switch-window ()
    "If there is only on window open, switch to the most recent buffer."
    (interactive)
    (if (= 1 (count-windows))
	(switch-to-buffer (other-buffer))
      (call-interactively 'ace-window))))


(use-package avy
  :bind (("M-j"   . avy-goto-char-timer)
	 ("M-g g" . avy-goto-end-of-line)
	 ("M-s m" . avy-move-line)
	 ("M-s M-m" . avy-move-region))
  :config
  (defun avy-action-embark (pt)
    "Use embark to act on the item at PT"
    (unwind-protect
	(save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)
  (defun avy-action-embark-dwim (pt)
    "Use embark to act on the item at PT"
    (unwind-protect
	(save-excursion
          (goto-char pt)
          (embark-dwim))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)
  (setf (alist-get ?  avy-dispatch-alist) 'avy-action-embark
	(alist-get ?. avy-dispatch-alist) 'avy-action-embark-dwim)
  )

;;; Windows

(use-package window
  :bind (("M-o" . other-window-alternating)
	 ("M-O" . (lambda () (interactive) (switch-to-buffer (other-buffer)))))
  :config
  (defalias 'other-window-alternating
    (let ((direction 1))
      (lambda (&optional arg)
        "Call `other-window', switching directions each time."
        (interactive)
	(if (one-window-p)
	    (switch-to-buffer (other-buffer)))
        (if (equal last-command 'other-window-alternating)
            (other-window (* direction (or arg 1)))
          (setq direction (- direction))
          (other-window (* direction (or arg 1)))))))
    )

(use-package popper
  :disabled nil
  :bind (("C-`"   . popper-toggle)
         ("C-~'"  . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

(use-package winner-mode
  :hook after-init)

;;; Appearance

(use-package mixed-pitch
  :ensure t
  :custom (mixed-pitch-variable-pitch-cursor nil))

(use-package olivetti
  :bind ("C-c x o" . olivetti-mode)
  :custom-face (olivetti-fringe ((t :background unspecified)))
  :custom (olivetti-body-width 88))

(use-package modus-themes
  :config (load-theme 'modus-operandi t))

(use-package ef-themes
  :ensure t)

(use-package spacious-padding
  :ensure t
  :config			       
  (spacious-padding-mode) ; doesn't load correctly first time. Is fixed in master
  (spacious-padding-mode)
  (spacious-padding-mode))


;;; Org

(use-package org
  :defer t
  :bind (("C-c c" . org-capture)
	 ("C-c a" . org-agenda)
	 ("C-'" . nil)
	 :map org-mode-map
	 ("C-c l d" . org-toggle-link-display)
	 ("C-'")
	 ("C-c RET"))
  :hook
  ((org-mode . org-modern-mode)
   (org-mode . hl-line-mode))
  :config
  (setq org-default-notes-file "~/sync/notes.org"
	org-confirm-babel-evaluate nil
	org-use-speed-commands t
	org-confirm-elisp-link-function nil
	org-startup-indented t
	org-startup-folded 'show2levels)
  (setq org-blank-before-new-entry '((heading . t)
                                     (plain-list-item . auto)))
  (setq org-capture-templates
	'(
	  ("j" "Journal Entry" entry
	   (file+datetree "~/sync/journal.org")
           "* %?"
           :empty-lines 1)
	  ("t" "Task" entry
	   (file+headline org-default-notes-file "Tasks")
           "* TODO %?\n"
           )
	  ("e" "Emacs yakshaving" entry
	   (file+headline org-default-notes-file "Emacs")
	   "* %^{Title}\n%u\n%i\n")
	  ("n" "Note" entry
	   (file org-default-notes-file)
           "* %?"
           :empty-lines 1)))

  (org-babel-do-load-languages 'org-babel-load-languages
			       '((emacs-lisp . t)
				 (shell . t)
				 (scheme . t)
				 (python . t))))

(use-package org-modern
  :ensure t
  :after org
  :hook (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda)
  :config
  (setq org-modern-block-fringe nil))


;;; Features

(use-package wgrep
  :ensure t)

(use-package jinx
  :hook (text-mode . jinx-mode)
  :delight
  :custom (jinx-languages "en de")
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))

(use-package magit :ensure t)

(use-package pdf-tools
  :defer 3
  :bind (:map pdf-view-mode-map
	      ("d" . pdf-view-midnight-minor-mode)
	      ("i" . consult-imenu))
  :config
  (pdf-loader-install)
  (setq-default pdf-view-display-size 'fit-page)
  :hook (pdf-view-mode . auto-revert-mode))

(use-package reftex
  :defer t
  :hook (LaTeX-mode . reftex-mode)
  :config
  (setq reftex-plug-into-AUCTeX t))

(use-package guix)

(with-eval-after-load 'guix-repl
    (setq guix-guile-program  '("guix" "repl")
          guix-config-scheme-compiled-directory  nil
          guix-repl-use-latest  nil
          guix-repl-use-server  nil))

(use-package gptel
  :load-path "~/.emacs.d/gptel/"
  :bind (("C-c RET" . gptel-send)
	 ("C-c C-<return>" . gptel-menu)
	 ("C-c M-a" . gptel-abort))
  :config
  ;; (load-file "~/.emacs.d/elpa/gptel-0.8.6/")
  (setq gptel-api-key (getenv "OPENAI_API_KEY"))
  (setq gptel-expert-commands t)
  (setq gptel-log-level 'info)
  (setq gptel-model "gpt-4o")
  (setq gptel-orgq-branching-context t)
  (setq gptel-prompt-prefix-alist '((markdown-mode . "## ") (org-mode . "** ") (text-mode . "### ")))
  (setq gptel-directives
	`((default . "You are a large language model living in Emacs and a helpful assistant. Respond concisely.")
	  (programming . "You are a large language model and a careful programmer. Provide code and only code as output without any additional text, prompt or note.")
	  (synonym . "List 10 synonyms to the given word.")
	  (elisp . "Write the requested elisp")
	  (define . "Define the word in one sentence. Use the language of the word")
	  (connect . "Write about how the input connects to the topic of ")
	  (chat . "You are a large language model and a conversation partner. Respond concisely.")
	  (1word . "Distill the user input into one word"))))

(use-package eglot
  :hook
  (python-base-mode-hook . eglot-ensure)
  :custom
  (eglot-send-changes-idle-time 0.1)
  (eglot-extend-to-xref t)
  :config
  (fset #'jsonrpc--log-event #'ignore))

(use-package nov :ensure t :mode ("\\.epub\\'" . nov-mode))

(use-package comint
  :ensure nil
  :custom
  (comint-prompt-read-only t)
  :bind
  (:map comint-mode-map
        ("M-h" . consult-history)
        ("M-r") ("M-s")))


;;; My utils

(defun insert-date ()
  "Insert the current date and time in ISO xxx."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M")))

(defun convert-markdown-to-org ()
  "Convert Markdown to Org using Pandoc. If the region is active, convert the region; otherwise, convert the whole buffer."
  (interactive)
  (let* ((start (if (use-region-p) (region-beginning) (point-min)))
         (end (if (use-region-p) (region-end) (point-max)))
         (input (buffer-substring-no-properties start end))
         (output (with-temp-buffer
                   (insert input)
                   (call-process-region (point-min) (point-max) "pandoc" t t nil "-f" "markdown" "-t" "org")
                   (buffer-string))))
    (delete-region start end)
    (insert output)))

;;; Cool elisp file strolling

(defun narrow-to-sexp ()
  (mark-sexp)
  (narrow-to-region (region-beginning) (region-end))
  (deactivate-mark))

(defun next-sexp ()
  "Narrow to next sexp"
  (interactive)
  (widen)
  (forward-sexp 2)
  (backward-sexp)
  (narrow-to-sexp))

(defun previous-sexp ()
  "Narrow to previous sexp"
  (interactive)
  (widen)
  (backward-sexp)
  (narrow-to-sexp))


(provide 'init)

;;; init.el ends here

