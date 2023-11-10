;; [[file:vimilla-emacs.org::*Mac settings][Mac settings:1]]
(setq mac-option-modifier 'meta)
;; Mac settings:1 ends here

;; [[file:vimilla-emacs.org::*Mac settings][Mac settings:2]]
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
;; Mac settings:2 ends here

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
;; Org:1 ends here
