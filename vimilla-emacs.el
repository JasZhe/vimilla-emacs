;; [[file:vimilla-emacs.org::*Mac settings][Mac settings:1]]
(setq mac-option-modifier 'meta)
;; Mac settings:1 ends here

;; [[file:vimilla-emacs.org::*General Settings][General Settings:1]]
(add-to-list 'completion-styles 'substring)
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

;; [[file:vimilla-emacs.org::*in buffer completion][in buffer completion:1]]
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
(setq xref-show-xrefs-function #'xref-show-definitions-completing-read)
(setq xref-show-definitions-function #'xref-show-definitions-completing-read)
;; xref:1 ends here

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

;; [[file:vimilla-emacs.org::*go us treesit][go us treesit:1]]
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
;; go us treesit:1 ends here

;; [[file:vimilla-emacs.org::*Org][Org:1]]
(setq org-startup-indented t)
(setq org-indent-indentation-per-level 4)
;; Org:1 ends here
