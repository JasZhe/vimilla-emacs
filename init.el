(defun my/vc-git-editor-command (command)
  "command is a git subcommand that requires an editor.
example usage: (my/vc-git-editor-command \"rebase -i HEAD~3\")"
  (interactive "P")
  (unless server-mode (server-force-delete) (server-mode))
  (let ((command (if command command (read-string "command: git "))))
    (async-shell-command
     (concat "GIT_EDITOR=\"emacsclient\" bash -c \"git " command "\""))))

(defun my/vc-git-rebase-i (&optional branch)
  "if branch isn't supplied from arg, prompt for it"
  (interactive)
  (let ((revision (if branch branch (read-string "revision: "))))
    (my/vc-git-editor-command (concat "rebase -i " revision))))

(defun my/vc-git-rebase-abort ()
  (interactive)
  (async-shell-command "rebase --abort"))

(defun my/vc-git-rebase-continue ()                  
  (interactive)                                 
  (async-shell-command "rebase --continue"))

(defun my/vc-git-fetch ()                  
  (interactive)                                  
  (async-shell-command "git fetch"))

(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)
(define-key global-map (kbd "s-/") #'comment-line)

(winner-mode)

(defun my/set-transparency-in-terminal ()
  (interactive)
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))

(defun my/set-frame-alpha (&optional arg)
  (interactive "sFrame Alpha? ")
  (if
      (and arg (not (string-empty-p arg)))
      (set-frame-parameter nil 'alpha  (string-to-number arg))
    (set-frame-parameter nil 'alpha 90)))

(defun my/set-frame-alpha-background (&optional arg)
  (interactive "sFrame Alpha Background? ")
  (if
      (and arg (not (string-empty-p arg)))
      (set-frame-parameter nil 'alpha-background  (string-to-number arg))
    (set-frame-parameter nil 'alpha-background 90)))

(setq my-window-map (make-sparse-keymap))

(define-key my-window-map "u" #'winner-undo)
(define-key my-window-map "r" #'winner-redo)

(define-key my-window-map "<"
            (lambda (arg) (interactive "P") (shrink-window-horizontally (if arg arg 1))))
(define-key my-window-map ">"
            (lambda (arg) (interactive "P") (enlarge-window-horizontally (if arg arg 1))))

(define-key my-window-map "-"
            (lambda (arg) (interactive "P") (shrink-window (if arg arg 1))))
(define-key my-window-map "+"
            (lambda (arg) (interactive "P") (enlarge-window (if arg arg 1))))

(define-key my-window-map "v" #'split-window-horizontally)
(define-key my-window-map "s" #'split-window-vertically)

(define-key my-window-map "q" #'delete-window)
(define-key my-window-map "\C-w" #'other-window)

(define-key my-window-map "l" #'windmove-right)
(define-key my-window-map "\C-l" #'windmove-right)

(define-key my-window-map "h" #'windmove-left)
(define-key my-window-map "\C-h" #'windmove-left)

(define-key my-window-map "k" #'windmove-up)
(define-key my-window-map "\C-k" #'windmove-up)

(define-key my-window-map "j" #'windmove-down)
(define-key my-window-map "\C-j" #'windmove-down)

(define-key my-window-map "=" #'balance-windows)

(define-key my-window-map "o" #'maximize-window)
(define-key my-window-map "\C-o" #'delete-other-windows)

(define-key global-map (kbd "\C-w") nil)
(define-key global-map (kbd "\C-w") my-window-map)

(tool-bar-mode 0)
(setq viper-mode t)
(require 'viper)
(require 'rect)
(fido-vertical-mode)
(viper-mode)
(global-hl-line-mode)
(global-visual-line-mode)
(setq column-number-mode t)

(keymap-set minibuffer-local-completion-map "TAB" #'icomplete-force-complete)
(keymap-set global-map "C-z" #'viper-mode) ;; C-z to suspend frame is annoying with viper

(setq scroll-margin 8)
(setq visual-bell t)
(setq ring-bell-function 'ignore)
(setq scroll-preserve-screen-position t)

(advice-add #'viper-search :after
            (lambda (string &rest args)
              (hi-lock-face-buffer string)))

;; keep highlighting after isearch
(setq lazy-highlight-cleanup nil)

;; be explicit about using this advice
(setq my/ioccur-p nil)
(defun my/ioccur ()
  (interactive)
  (setq my/ioccur-p t)
  (call-interactively 'isearch-forward))

(add-hook 'isearch-mode-hook
         (lambda ()
            (if my/ioccur-p
                (advice-add #'isearch-printing-char :after
                            (lambda (&rest args)
                              (if isearch-regexp (isearch-occur isearch-regexp)
                                (isearch-occur isearch-string))))
              (advice-mapc `(lambda (fun props) (advice-remove 'isearch-printing-char fun)) 'isearch-printing-char))))

(add-hook 'isearch-mode-end-hook (lambda () (setq my/ioccur-p nil)))

(use-package xref
  :config
  (progn
    (setq xref-search-program 'ripgrep)
    (setq xref-show-xrefs-function #'xref-show-definitions-completing-read)      
    (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
    )
  )

(add-hook 'prog-mode-hook #'flymake-mode)
(setq treesit-font-lock-level 4)
(which-function-mode)
(electric-pair-mode)

(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
(add-hook 'go-ts-mode-hook (lambda () (call-interactively #'eglot)))

(defface font-lock-func-face 
    '((nil (:foreground "#7F0055" :weight bold))
      (t (:bold t :italic t)))
  "Font Lock mode face used for function calls."
  :group 'font-lock-highlighting-faces)

(font-lock-add-keywords
 'emacs-lisp-mode
 '(("(\\s-*\\(\\_<\\(?:\\sw\\|\\s_\\)+\\)\\_>"
    1 'font-lock-constant-face)) 'append)

(defun my-fl (_limit)
  (let ((opoint  (point))
        (found   nil))
    (with-syntax-table emacs-lisp-mode-syntax-table
      (while (not found)
        (cond ((condition-case ()
                   (save-excursion
                     (skip-chars-forward "'")
                     (setq opoint  (point))
                     (let ((obj  (read (current-buffer))))
                       (and (symbolp obj)  (fboundp obj)
                            (progn (set-match-data (list opoint (point))) t))))
                 (error nil))
               (forward-sexp 1)
               (setq opoint  (point)
                     found   t))
              (t
               (if (looking-at "\\(\\sw\\|\\s_\\)")
                   (forward-sexp 1)
                 (forward-char 1)))))
      found)))

(setq enable-recursive-minibuffers t)
(defun completing-read-in-region (start end collection &optional predicate)
   "Prompt for completion of region in the minibuffer if non-unique.
  Use as a value for `completion-in-region-function'."
   (let* ((initial (buffer-substring-no-properties start end))
          (all (completion-all-completions initial collection predicate
                                           (length initial)))
          (completion (cond
                       ((atom all) nil)
                       ((and (consp all) (atom (cdr all))) (car all))
                       (t (completing-read
                           "Completion: " collection predicate t initial)))))
     (cond (completion (completion--replace start end completion) t)
           (t (message "No completion") nil))))
 (setq completion-in-region-function #'completing-read-in-region)

(defun my/eshell-send-cmd-async ()
  (interactive)
  (let ((cmd (string-trim (buffer-substring-no-properties eshell-last-output-end (progn (end-of-line) (point))))))
    (unless (eshell-head-process)
      (delete-region eshell-last-output-end (point))
      (insert (format "async-shell-command \"%s\"" cmd)))
    )
  )

(use-package eshell
  :config
  (add-to-list 'eshell-modules-list 'eshell-tramp)
  (setq my/eshell-vi-state-modify-map (make-sparse-keymap))
  (setq my/eshell-insert-state-modify-map (make-sparse-keymap))

  (define-key my/eshell-vi-state-modify-map (kbd "C-<return>") #'my/eshell-send-cmd-async)
  (define-key my/eshell-vi-state-modify-map " ma" #'my/eshell-send-cmd-async)
  (define-key my/eshell-insert-state-modify-map (kbd "C-<return>") #'my/eshell-send-cmd-async)

  (viper-modify-major-mode 'eshell-mode 'vi-state my/eshell-vi-state-modify-map)
  (viper-modify-major-mode 'eshell-mode 'insert-state my/eshell-insert-state-modify-map)
  )

(when (member "IosevkaCustom Nerd Font Propo" (font-family-list))
  (set-face-attribute 'default nil :font "IosevkaCustom Nerd Font Propo" :height 130))
(when (member "Iosevka Etoile" (font-family-list))
  (set-face-attribute 'variable-pitch nil :font "Iosevka Etoile" :height 130))

(setq modus-themes-headings
      '((1 . (rainbow overline background variable-pitch 1.25))
        (2 . (rainbow background variable-pitch 1.15))
        (3 . (rainbow bold variable-pitch 1.1))
        (t . (semilight variable-pitch 1.05))))

(setq modus-themes-bold-constructs t)
(setq modus-themes-italic-constructs t)
(setq modus-themes-org-blocks 'gray-background)
(load-theme 'modus-operandi)

(defun find-git-dir (dir)
 "Search up the directory tree looking for a .git folder."
 (cond
  ((eq major-mode 'dired-mode) "Dired")
  ((not dir) "process")
  ((string= dir "/") "no-git")
  (t (vc-root-dir))))

(defun git-tabbar-buffer-groups ()
  "Groups tabs in tabbar-mode by the git repository they are in."
  (list (find-git-dir (buffer-file-name (current-buffer)))))

(setq org-directory "~/orgmode/")
(setq org-attach-id-dir (concat (file-name-as-directory org-directory) (file-name-as-directory ".attach")))
(setq org-todo-keywords '((sequence "TODO(t)" "WIP(w)" "|" "DONE" "CANCELLED")))
(setq org-attach-use-interitance t)

(setq org-startup-indented t)
(setq org-indent-indentation-per-level 4)
(setq org-startup-folded nil) ;; to respect VISIBILITY property just can't be 'showeverything, see: org-cycle-set-startup-visibility 

;; allow dabbrev expand on tab when in insert mode
(defun line-before-point-empty-p ()
  (string-blank-p (buffer-substring-no-properties (point-at-bol) (point))))

(use-package org
  :config
  (progn
    (setq org-image-actual-width '(300))
    (setq org-goto-interface 'outline-path-completionp)
    (setq org-outline-path-complete-in-steps nil)
    (setq org-return-follows-link t)
    (setq my/org-vi-state-modify-map (make-sparse-keymap))

    (define-key my/org-vi-state-modify-map "zi" #'org-toggle-inline-images)
    (define-key my/org-vi-state-modify-map " si" #'org-goto)
    (define-key my/org-vi-state-modify-map " oaa" #'org-agenda)

    (define-key my/org-vi-state-modify-map " msl" #'org-demote-subtree)
    (define-key my/org-vi-state-modify-map " msh" #'org-promote-subtree)

    (define-key my/org-vi-state-modify-map " maa" #'org-attach)
    (define-key my/org-vi-state-modify-map " mA" #'org-archive-subtree)

    (define-key my/org-vi-state-modify-map " mds" #'org-schedule)
    (define-key my/org-vi-state-modify-map " mdd" #'org-deadline)

    (define-key my/org-vi-state-modify-map " msr" #'org-refile)

    (define-key my/org-vi-state-modify-map " mll" #'org-insert-link)
    (define-key my/org-vi-state-modify-map " nl" #'org-store-link)

    (viper-modify-major-mode 'org-mode 'vi-state my/org-vi-state-modify-map)

    (define-key org-mode-map "\t"
                (lambda (arg)
                  (interactive "P")
                  (if (and (not (line-before-point-empty-p)) (string= viper-current-state "insert-state"))
                      (dabbrev-expand arg)
                    (org-cycle arg))))))

(use-package orderless :ensure nil :pin gnu
  :config
  (setq completion-styles '(orderless basic) completion-category-overrides '((file (styles basic partial-completion))))
  (defun my-icomplete-styles () (setq-local completion-styles '(orderless)))
  (add-hook 'icomplete-minibuffer-setup-hook 'my-icomplete-styles)
  (define-key minibuffer-local-completion-map " " #'self-insert-command))

(use-package avy :ensure nil :pin gnu
  :config
  (define-key viper-vi-basic-map "gss" #'avy-goto-char-2)
  (define-key viper-vi-basic-map "gs/" #'avy-goto-char-timer))

(use-package which-key :ensure nil :pin gnu
  :config
  (which-key-mode))

(when (not (require 'hurl-mode nil 'noerrror))
  (package-vc-install "https://github.com/JasZhe/hurl-mode"))
(use-package hurl-mode :mode "\\.hurl\\'")

(when (not (require 'window-stool nil 'noerrror))
  (package-vc-install "https://github.com/JasZhe/window-stool"))
(use-package window-stool
  :config
  (add-hook 'prog-mode-hook #'window-stool-mode)
  (add-hook 'org-mode-hook #'window-stool-mode))

(use-package web-mode :ensure nil :pin gnu :defer t
  :mode "\\.gohtml\\'"
  :config
  (setq web-mode-engines-alist '(("go" . "\\.gohtml\\'") ("svelte" . "\\.svelte\\'")))
  )

(use-package pdf-tools :ensure nil :pin gnu
  :config
    (setq my/pdf-vi-state-modify-map (make-sparse-keymap))
    (define-key my/pdf-vi-state-modify-map "o" #'pdf-outline)
    (define-key my/pdf-vi-state-modify-map "H" #'pdf-view-fit-height-to-window)
    (define-key my/pdf-vi-state-modify-map "W" #'pdf-view-fit-width-to-window)
    (setq pdf-view-resize-factor 1.10)
    (define-key my/pdf-vi-state-modify-map "+" #'pdf-view-enlarge)
    (define-key my/pdf-vi-state-modify-map "-" #'pdf-view-shrink)

    (viper-modify-major-mode 'pdf-view-mode 'vi-state my/pdf-vi-state-modify-map)
  )

(use-package magit :ensure nil :pin gnu
   :config
   (define-key my/leader-prefix-map "gg" #'magit)
   (setq my/magit-vi-state-modify-map
         (make-composed-keymap
          (list my/viper-vi-basic-motion-keymap
                my/viper-vi-motion-g-keymap
                my/viper-vi-motion-leader-keymap)
          magit-mode-map))
   (define-key my/magit-vi-state-modify-map "x" #'magit-discard)
   (define-key my/magit-vi-state-modify-map "p" #'magit-push)

   (viper-modify-major-mode 'magit-status-mode 'vi-state my/magit-vi-state-modify-map)
   )
