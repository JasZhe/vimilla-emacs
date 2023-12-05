(defun my/vc-git-editor-command (command)
  "command is a git subcommand that requires an editor.
example usage: (my/vc-git-editor-command \"rebase -i HEAD~3\")"
  (interactive "P")
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

(winner-mode)

(define-key global-map (kbd "\C-w") nil)

(define-key global-map (kbd "\C-wu") #'winner-undo)
(define-key global-map (kbd "\C-wr") #'winner-redo)

(define-key global-map (kbd "\C-w<")
            (lambda (arg) (interactive "P") (shrink-window-horizontally (if arg arg 1))))
(define-key global-map (kbd "\C-w>")
            (lambda (arg) (interactive "P") (enlarge-window-horizontally (if arg arg 1))))

(define-key global-map (kbd "\C-w-")
            (lambda (arg) (interactive "P") (shrink-window (if arg arg 1))))
(define-key global-map (kbd "\C-w+")
            (lambda (arg) (interactive "P") (enlarge-window (if arg arg 1))))

(define-key global-map "\C-wv" #'split-window-horizontally)
(define-key global-map "\C-ws" #'split-window-vertically)

(define-key global-map "\C-wq" #'delete-window)
(define-key global-map "\C-w\C-w" #'other-window)

(define-key global-map "\C-wl" #'windmove-right)
(define-key global-map "\C-w\C-l" #'windmove-right)

(define-key global-map "\C-wh" #'windmove-left)
(define-key global-map "\C-w\C-h" #'windmove-left)

(define-key global-map "\C-wk" #'windmove-up)
(define-key global-map "\C-w\C-k" #'windmove-up)

(define-key global-map "\C-wj" #'windmove-down)
(define-key global-map "\C-w\C-j" #'windmove-down)

(define-key global-map "\C-w=" #'balance-windows)

(define-key global-map (kbd "\C-wo") #'maximize-window)
(define-key global-map "\C-w\C-o" #'delete-other-windows)

(which-function-mode)

(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))

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

(defface org-block-begin-line
  '((t (:underline "#A7A6AA" :foreground "#008ED1" :background "#EAEAFF")))
  "Face used for the line delimiting the begin of source blocks.")
(defface org-block-background
  '((t (:background "#FFFFEA")))
  "Face used for the source block background.")
(defface org-block-end-line
  '((t (:overline "#A7A6AA" :foreground "#008ED1" :background "#EAEAFF")))
  "Face used for the line delimiting the end of source blocks.")

(defface org-level-1
  '((t (:inherit outline-1 :height 1.25)))
  "Face used for level 1 headlines.")
(defface org-level-2
  '((t (:inherit outline-2 :height 1.15)))
  "Face used for level 2 headlines.")
(defface org-level-3
  '((t (:inherit outline-3 :height 1.10)))
  "Face used for level 3 headlines.")
(defface org-level-4
  '((t (:inherit outline-4 :height 1.05)))
  "Face used for level 4 headlines.")
(defface org-level-5
  '((t (:inherit outline-5 :height 1.0)))
  "Face used for level 5 headlines.")

(setq org-startup-indented t)
(setq org-indent-indentation-per-level 4)

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
