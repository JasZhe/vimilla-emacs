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

  (setq visual-bell t)
  (setq ring-bell-function 'ignore)
  (setq scroll-preserve-screen-position t)
;; General Settings:1 ends here

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

;; [[file:vimilla-emacs.org::*Org][Org:1]]
  (setq org-startup-indented t)
  (setq org-indent-indentation-per-level 4)
;; Org:1 ends here
