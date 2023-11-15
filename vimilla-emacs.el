;; [[file:vimilla-emacs.org::*Mac settings][Mac settings:1]]
(setq mac-option-modifier 'meta)
;; Mac settings:1 ends here

;; [[file:vimilla-emacs.org::*General Settings][General Settings:1]]
(setq viper-mode t)
(require 'viper)
(require 'rect)
(fido-vertical-mode)
(viper-mode)
(global-hl-line-mode)
(global-visual-line-mode)

(keymap-set minibuffer-local-completion-map "TAB" #'icomplete-force-complete)
(keymap-set global-map "C-z" #'viper-mode) ;; C-z to suspend frame is annoying with viper

(setq scroll-margin 8)
(setq visual-bell t)
(setq ring-bell-function 'ignore)
(setq scroll-preserve-screen-position t)
;; General Settings:1 ends here

;; [[file:vimilla-emacs.org::*advice to highlight matches with viper search][advice to highlight matches with viper search:1]]
(advice-add #'viper-search :after
            (lambda (string &rest args)
              (hi-lock-face-buffer string)))
;; advice to highlight matches with viper search:1 ends here

;; [[file:vimilla-emacs.org::*optional incremental occur, similar to swiper][optional incremental occur, similar to swiper:1]]
;; keep highlighting after isearch
(setq lazy-highlight-cleanup nil)

;; be explicit about using this advice
(setq my/ioccur-p nil)
(defun my/ioccur (arg)
  (interactive "P")
  (setq my/ioccur-p t)
  (isearch-forward arg))

(add-hook 'isearch-mode-hook
         (lambda ()
            (if my/ioccur-p
                (advice-add #'isearch-printing-char :after
                            (lambda (&rest args)
                              (if isearch-regexp (isearch-occur isearch-regexp)
                                (isearch-occur isearch-string))))
              (advice-mapc `(lambda (fun props) (advice-remove 'isearch-printing-char fun)) 'isearch-printing-char))))

(add-hook 'isearch-mode-end-hook (lambda () (setq my/ioccur-p nil)))
;; optional incremental occur, similar to swiper:1 ends here

;; [[file:vimilla-emacs.org::*in buffer completion][in buffer completion:1]]
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
;; in buffer completion:1 ends here

;; [[file:vimilla-emacs.org::*xref][xref:1]]
(use-package xref
  :config
  (progn
    (setq xref-search-program 'ripgrep)
    (setq xref-show-xrefs-function #'xref-show-definitions-completing-read)      
    (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
    )
  )
;; xref:1 ends here

;; [[file:vimilla-emacs.org::*some more basic elisp highlighting][some more basic elisp highlighting:1]]
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

;; (add-hook 'emacs-lisp-mode-hook
;; 	  (lambda ()
;; 	    (font-lock-add-keywords nil
;; 				    '((my-fl . 'font-lock-constant-face)) 'append)))
;; some more basic elisp highlighting:1 ends here

;; [[file:vimilla-emacs.org::*Tab bar][Tab bar:1]]
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
;; (setq tabbar-buffer-groups-function 'git-tabbar-buffer-groups)
;; Tab bar:1 ends here

;; [[file:vimilla-emacs.org::*go use treesit][go use treesit:1]]
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
;; go use treesit:1 ends here

;; [[file:vimilla-emacs.org::*Org][Org:1]]
(setq org-directory "~/orgmode/")
(setq org-attach-id-dir (concat (file-name-as-directory org-directory) ".attach"))
(defface org-block-begin-line
  '((t (:underline "#A7A6AA" :foreground "#008ED1" :background "#EAEAFF")))
  "Face used for the line delimiting the begin of source blocks.")

(defface org-block-background
  '((t (:background "#FFFFEA")))
  "Face used for the source block background.")

(defface org-block-end-line
  '((t (:overline "#A7A6AA" :foreground "#008ED1" :background "#EAEAFF")))
  "Face used for the line delimiting the end of source blocks.")

(setq org-startup-indented t)
(setq org-indent-indentation-per-level 4)

;; allow dabbrev expand on tab when in insert mode
(defun line-before-point-empty-p ()
  (string-blank-p (buffer-substring-no-properties (point-at-bol) (point))))

(use-package org
  :config
  (progn
    (setq org-goto-interface 'outline-path-completionp)
    (setq org-outline-path-complete-in-steps nil)
    (setq org-return-follows-link t)

    (setq my-org-modifier-map (make-sparse-keymap))
    (define-key my-org-modifier-map " si" #'org-goto)
    (define-key my-org-modifier-map " msl" #'org-demote-subtree)
    (define-key my-org-modifier-map " msh" #'org-promote-subtree)
    (define-key my-org-modifier-map " oaa" #'org-agenda)
    (viper-modify-major-mode 'org-mode 'vi-state my-org-modifier-map)

    (define-key org-mode-map "\t"
                (lambda (arg)
                  (interactive "P")
                  (if (and (not (line-before-point-empty-p)) (string= viper-current-state "insert-state"))
                      (dabbrev-expand arg)
                    (org-cycle arg))))))
;; Org:1 ends here
