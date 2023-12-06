(setq my/global-viper-state 'vi)
(defun set-global-viper-state (arg)
  (cond ((eq my/global-viper-state 'vi) (viper-change-state-to-vi))
        ((eq my/global-viper-state 'emacs) (viper-change-state-to-emacs))
        ((eq my/global-viper-state 'ins) (viper-change-state-to-insert))
        (t (viper-change-state-to-vi))
  ))
(add-to-list 'window-state-change-functions #'set-global-viper-state)

;; prefer the following to be in whatever state I'm already in                                       
(setq viper-emacs-state-mode-list (remove 'Custom-mode viper-emacs-state-mode-list))                 
(setq viper-emacs-state-mode-list (remove 'dired-mode viper-emacs-state-mode-list))                  
(setq viper-emacs-state-mode-list (remove 'occur-mode viper-emacs-state-mode-list))                  
(setq viper-emacs-state-mode-list (remove 'help-mode viper-emacs-state-mode-list))                   
(setq viper-emacs-state-mode-list (remove 'completion-list-mode viper-emacs-state-mode-list))
(setq viper-emacs-state-mode-list (remove 'completion-list-mode viper-emacs-state-mode-list))


;; then remove all emacs states and replace with insert states                                       
(setq viper-insert-state-mode-list (append viper-emacs-state-mode-list viper-insert-state-mode-list))
(setq viper-emacs-state-mode-list nil)

(setq viper-inhibit-startup-message 't)
(setq viper-expert-level '5)

(add-hook 'viper-insert-state-hook (lambda ()
                                     (global-hl-line-mode -1)
                                     (setq my/global-viper-state 'ins)
                                     (when (not (display-graphic-p)) (send-string-to-terminal "\033[6 q"))
                                     (setq viper-ex-style-editing nil)))

;; otherwise hl-line-mode stays off after running an ex command like :w

(add-hook 'viper-minibuffer-exit-hook (lambda () (global-hl-line-mode) (when (not (display-graphic-p)) (send-string-to-terminal "\033[0 q"))))

(add-hook 'viper-vi-state-hook (lambda ()
                                 (global-hl-line-mode)
                                 (setq my/global-viper-state 'vi)
                                 (set-face-attribute 'hl-line nil :background "LightCyan1")
                                 (when (not (display-graphic-p)) (send-string-to-terminal "\033[0 q"))))
(add-hook 'viper-emacs-state-hook (lambda ()
                                    (global-hl-line-mode)
                                    (setq my/global-viper-state 'emacs)
                                    (set-face-attribute 'hl-line nil :background "LavenderBlush1")
                                    (when (not (display-graphic-p)) (send-string-to-terminal "\033[0 q"))))

(add-hook 'minibuffer-mode-hook #'viper-change-state-to-insert)
(add-hook 'minibuffer-exit-hook #'viper-change-state-to-vi)
(setq viper-insert-state-cursor-color nil)

(setq viper-want-ctl-h-help 't)
(setq viper-fast-keyseq-timeout 100)

;; (advice-mapc `(lambda (fun props) (advice-remove 'viper-intercept-ESC-key fun)) 'viper-intercept-ESC-key)
(advice-add 'viper-intercept-ESC-key :after #'deactivate-mark)
(advice-add 'viper-intercept-ESC-key :after (lambda () (ignore-errors (abort-minibuffers))))
(advice-add 'viper-intercept-ESC-key :after (lambda () (ignore-errors (cua-clear-rectangle-mark))))
(advice-add 'viper-intercept-ESC-key :after (lambda () (lazy-highlight-cleanup t)))
(advice-add 'viper-intercept-ESC-key :after (lambda ()
                                              (dolist (hist viper-search-history)
                                                (hi-lock-unface-buffer hist))))

;; add to global marks when window stuff happens so we can switch back to prev position
(setq window-scroll-functions nil)
;; not perfect but good enough, need to also make sure region not active, so we don't reset the region on scroll
(add-to-list 'window-scroll-functions (lambda (window _)
                                        (when (and (not (region-active-p)) (eq window (selected-window)))
                                          (push-mark nil t nil))))
(setq window-buffer-change-functions nil)
(add-to-list 'window-buffer-change-functions (lambda (_)
                                               (with-current-buffer (other-buffer)
                                                 (push-mark nil t nil))))
(define-key viper-vi-basic-map "\C-i" #'xref-go-forward)
(define-key viper-vi-basic-map "\t" nil)
(define-key viper-vi-basic-map "\C-o"
            (lambda ()
              (interactive)
              (condition-case nil
                  (xref-go-back)
                (error
                 (pop-global-mark)
                 nil))
            ))

(define-key viper-vi-basic-map "k" #'previous-line)
(define-key viper-vi-basic-map "j" #'next-line)

(define-key viper-vi-basic-map (kbd "RET") nil)
(define-key viper-vi-basic-map "q" nil)

(setq selected-start-line -1)
(add-hook 'activate-mark-hook (lambda () (setq selected-start-line (line-number-at-pos))))
;; (advice-mapc `(lambda (fun props) (advice-remove 'next-line fun)) 'next-line)
(advice-add 'next-line :around
            (lambda (orig-fun &rest args)
              (interactive)
              ;; because now we're not getting the last newline
              (if (< (line-number-at-pos) selected-start-line)
                  (setq extra-line-after-yank t)
                (setq extra-line-after-yank nil))

              (if my/line-selection-p
                  (cond
                   ((= (line-number-at-pos) selected-start-line)
                    (progn
                      (beginning-of-line)
                      (set-mark-command nil)
                      (end-of-line)
                      (apply orig-fun args)
                      (end-of-line)
                      ))
                   ((= (+ (line-number-at-pos) 1) selected-start-line)
                    (progn
                      (apply orig-fun args)
                      (beginning-of-line)
                      (set-mark-command nil)
                      (end-of-line)))
                   ((< (line-number-at-pos) selected-start-line)
                    (apply orig-fun args))
                   (t 
                    (progn
                      (apply orig-fun args)
                      (end-of-line)))
                   )
                (apply orig-fun args))))

(advice-add 'previous-line :around
            (lambda (orig-fun &rest args)
              (interactive)
              (if (< (line-number-at-pos) selected-start-line)
                  (setq extra-line-after-yank t)
                (setq extra-line-after-yank nil))
              (if my/line-selection-p
                  (cond 
                   ((= (line-number-at-pos) selected-start-line)
                    (progn
                      (end-of-line)
                      (set-mark-command nil)
                      (beginning-of-line)
                      (apply orig-fun args)
                      (beginning-of-line)))
                   ((> (line-number-at-pos) selected-start-line)
                    (apply orig-fun args)
                    (end-of-line))		   
                   ((= (- (line-number-at-pos) 1) selected-start-line)
                    (progn 
                    (apply orig-fun args)
                    (end-of-line)
                    (set-mark-command nil)
                    (beginning-of-line)))
                   (t
                    (progn
                      (apply orig-fun args)
                      (beginning-of-line))))
                (apply orig-fun args))))
;; (advice-mapc `(lambda (fun props) (advice-remove 'previous-line fun)) 'previous-line)

(setq my/line-selection-p nil)
(setq my/lines-selected 0)

(add-hook 'deactivate-mark-hook (lambda () (setq my/line-selection-p nil)))

(defun my/select-lines (arg)
  "go to beginning of line and select rectangle mark and also set line selection flag"
  (interactive "p")
  (setq my/line-selection-p t)
  (beginning-of-line)
  (set-mark-command nil)
  (end-of-line))

(defun my/set-mark-command (arg)
  "set mark, and also unset line selection flag"
  (interactive "P")
  (setq my/line-selection-p nil)
  (set-mark-command arg))

(defun my/visual-block (arg)
  "set rectangle mark, and also unset line selection flag"
  (interactive "P")
  (setq my/line-selection-p nil)
  (rectangle-mark-mode arg))

(define-key viper-vi-basic-map "v" nil)
(define-key viper-vi-basic-map "v" #'my/set-mark-command)
(define-key viper-vi-basic-map "V" nil)
(define-key viper-vi-basic-map "V" #'my/select-lines)
(define-key viper-vi-basic-map "\C-v" #'my/visual-block)

;;(advice-mapc `(lambda (fun props) (advice-remove 'viper-ex fun)) 'viper-ex)
(advice-add 'viper-ex :around
            (lambda (orig-fun &rest args)
              (let ((current-prefix-arg t))
                (if (use-region-p) (apply orig-fun current-prefix-arg args)
                  (apply orig-fun args)))))

;; (advice-mapc `(lambda (fun props) (advice-remove 'viper-join-lines fun)) 'viper-join-lines)
(advice-add 'viper-join-lines :around
            (lambda (orig-fun arg &rest args)
              (interactive "P")
              (if (use-region-p)
                  (let* ((start (region-beginning))
                        (end (region-end))
                        (numlines (count-lines start end)))
                    (goto-char start)
                    (apply orig-fun `(,numlines)))
                (apply orig-fun `(,arg)))))

(setq my/line-yank-p nil)
(defun viper-delete-region-or-motion-command (arg)
  "convenience function for deleting a region, including rectangles"
  (interactive "P")
  (if (use-region-p)
      (let ((start (region-beginning)) (end (region-end)))
        (if rectangle-mark-mode
            (kill-rectangle start end arg)
          (progn
            (forward-char)
            (if my/line-selection-p
                (setq my/line-yank-p t)
              (setq my/line-yank-p nil))
            (kill-region start end t))))
    (viper-command-argument arg)))

(defun viper-copy-region-or-motion-command (arg)
  "convenience function for yanking a region, including rectangles"
  (interactive "P")
  (if (use-region-p)
      (let ((start (region-beginning)) (end (region-end)))
        (if rectangle-mark-mode
            (copy-rectangle-as-kill start end)
          (progn
            (forward-char)
            (if my/line-selection-p
                (setq my/line-yank-p t)
              (setq my/line-yank-p nil))
            (copy-region-as-kill start end t)
            (backward-char))
          ))
    (viper-command-argument arg)))

(defun viper-paste-into-region (arg)
  "if region is active, delete region before pasting
respects rectangle mode in a similar way to vim/doom"
  (interactive "P")
  (cond (my/line-yank-p
         (progn
           (viper-open-line nil)
           (viper-change-state-to-vi)
           (when (use-region-p) (delete-active-region))
           (yank)
           (forward-line)))
        ((and (not killed-rectangle) (use-region-p))
         (progn
           (let ((start (region-beginning)))
             (forward-char)
             (delete-active-region)
             (yank))))
        (killed-rectangle
         (progn 
           (yank-rectangle)
           (setq killed-rectangle nil)))
        (t (yank arg))))

(define-key viper-vi-basic-map "d" #'viper-delete-region-or-motion-command)
(define-key viper-vi-basic-map "y" #'viper-copy-region-or-motion-command)
(define-key viper-vi-basic-map "p" #'viper-paste-into-region)

(define-key viper-vi-basic-map "u" #'undo-only)
(define-key viper-vi-basic-map (kbd "C-r") #'undo-redo)
(define-key viper-vi-basic-map (kbd "C-M-r")  #'isearch-backward)

(setq my/g-prefix-map (make-sparse-keymap))
(define-key viper-vi-basic-map "g" my/g-prefix-map)
(define-key my/g-prefix-map "g" (lambda () (interactive) (viper-goto-line 1)))

(define-key my/g-prefix-map "k" #'viper-previous-line)
(define-key my/g-prefix-map "j" #'viper-next-line)

(define-key my/g-prefix-map "t" #'tab-bar-switch-to-next-tab)
(define-key my/g-prefix-map "T" #'tab-bar-switch-to-prev-tab)

(define-key my/g-prefix-map "zz" #'cua-rectangle-mark-mode)

(setq my/leader-prefix-map (make-sparse-keymap))
(define-key viper-vi-basic-map " " my/leader-prefix-map)

(define-key my/leader-prefix-map ","
            (lambda (arg)
              "switch to project buffer, with prefix argument, switch to any buffer"
              (interactive "P")
              (if arg (ido-switch-buffer)
                (project-switch-to-buffer (project--read-project-buffer)))))
(define-key my/leader-prefix-map "u" #'universal-argument)
(define-key universal-argument-map " u" #'universal-argument-more)

(define-key my/leader-prefix-map "F" #'project-find-file)
(define-key my/leader-prefix-map "G" #'project-find-regexp) ;; good enough

(define-key my/leader-prefix-map "oe" #'eshell)
(define-key my/leader-prefix-map "os" #'shell)

(define-key my/leader-prefix-map "pp" #'project-switch-project)
(define-key my/leader-prefix-map "pe" #'project-eshell)
(define-key my/leader-prefix-map "ps" #'project-shell)
(define-key my/leader-prefix-map "pd" #'project-forget-project)

(define-key my/leader-prefix-map "hk" #'describe-key)
(define-key my/leader-prefix-map "hf" #'describe-function)
(define-key my/leader-prefix-map "hv" #'describe-variable)
(define-key my/leader-prefix-map "hm" #'describe-mode)
(define-key my/leader-prefix-map "ho" #'describe-symbol)

(define-key my/leader-prefix-map "br" #'revert-buffer)
(define-key my/leader-prefix-map "bp" #'previous-buffer)
(define-key my/leader-prefix-map "bn" #'next-buffer)
(define-key my/leader-prefix-map "bi" #'ibuffer)

(define-key my/leader-prefix-map "\tn" #'tab-bar-new-tab)
(define-key my/leader-prefix-map "\td" #'tab-bar-close-tab)
(define-key my/leader-prefix-map "\tr" #'tab-bar-rename-tab)

(define-key my/leader-prefix-map "ss" #'my/ioccur)
(define-key my/leader-prefix-map "si" #'imenu)

(setq bookmark-use-annotations t)

; note the call-interactively does pass the prefix args
(defun my/set-project-bookmark ()
  (interactive)
  (minibuffer-with-setup-hook
      (lambda ()
        (let ((prefix (concat (project-name (project-current)) ": ")))
          (when (project-name (project-current))
            (insert prefix))))
        (call-interactively 'bookmark-set))
)

(defun my/jump-to-project-bookmark ()
  (interactive)
  (minibuffer-with-setup-hook
      (lambda ()
        (let ((prefix (concat (project-name (project-current)) ": ")))
          (when (project-name (project-current))
            (insert prefix))))
        (call-interactively 'bookmark-jump))
)

(setq bookmark-save-flag 1)
(setq bookmark-use-annotations t)
(setq bookmark-automatically-show-annotations nil)

(define-key my/leader-prefix-map "nrf" #'my/jump-to-project-bookmark)
(define-key my/leader-prefix-map "nrl" #'list-bookmarks)
(define-key my/leader-prefix-map "nri" #'bookmark-set)
(define-key my/leader-prefix-map "nrn" #'bookmark-set)
(define-key my/leader-prefix-map "nrd" #'bookmark-delete)
(define-key my/leader-prefix-map "bmm" #'my/set-project-bookmark)
(define-key my/leader-prefix-map "bmj" #'my/jump-to-project-bookmark)

(define-key my/leader-prefix-map "ff" #'find-file)

(define-key my/leader-prefix-map "cd" #'xref-find-definitions)
(define-key viper-vi-basic-map "gd" #'xref-find-definitions)

(define-key my/leader-prefix-map "cD" #'xref-find-references)
(define-key viper-vi-basic-map "gD" #'xref-find-references)

(define-key my/leader-prefix-map "cf" #'eglot-format-buffer)
(define-key my/leader-prefix-map "xf" #'eglot-format-buffer)

(define-key viper-vi-basic-map "H"
            (lambda (arg) (interactive "P")
              (if arg (viper-window-top arg)
                (viper-window-top (+ scroll-margin 1)))))
(define-key viper-vi-basic-map "L"
            (lambda (arg) (interactive "P")
              (if arg (viper-window-bottom arg)
                (viper-window-bottom (+ scroll-margin 1)))))
(define-key viper-vi-basic-map "zz" #'recenter-top-bottom)

(advice-mapc `(lambda (fun props) (advice-remove 'viper-goto-line fun)) 'viper-goto-line)
(advice-add 'viper-goto-line :around
            (lambda (orig-fun &rest args)
              (cl-letf (((symbol-function 'deactivate-mark) (lambda (&optional _) nil)))
                (apply orig-fun args))))

(add-hook 'prog-mode-hook #'hs-minor-mode)
(define-key viper-vi-basic-map "zC" #'hs-hide-all)
(define-key viper-vi-basic-map "zO" #'hs-show-all)
(define-key viper-vi-basic-map "zo" #'hs-show-block)
(define-key viper-vi-basic-map "zc" #'hs-hide-block)
(define-key viper-vi-basic-map "za" #'hs-toggle-hiding)

(advice-mapc `(lambda (fun props) (advice-remove 'viper-brac-function fun)) 'viper-brac-function)
(advice-add 'viper-brac-function :around
            (lambda (orig-fun &rest args)
              (let ((char (read-char)))
                (cond ((viper= ?b char) (previous-buffer))
                      (t
                       ;; hack so that we can override read-char and only need input once
                       (cl-letf (((symbol-function 'read-char) (lambda (_ _ _) char)))
                         (apply orig-fun args)
                         )
                       )
                ))))
(advice-mapc `(lambda (fun props) (advice-remove 'viper-key-function fun)) 'viper-key-function)
(advice-add 'viper-ket-function :around
            (lambda (orig-fun &rest args)
              (let ((char (read-char)))
                (cond ((viper= ?b char) (next-buffer))
                      (t
                       ;; hack so that we can override read-char and only need input once
                       (cl-letf (((symbol-function 'read-char) (lambda (_ _ _) char)))
                         (apply orig-fun args)
                         )
                       )
                ))))

(define-key global-map "\C-xvf" #'vc-pull)
(define-key global-map "\C-xvF" #'my/vc-git-fetch)

(define-key global-map "\C-xv\C-ri" #'my/vc-git-rebase-i)
(define-key global-map "\C-xv\C-ra" #'my/vc-git-rebase-abort)
(define-key global-map "\C-xv\C-rc" #'my/vc-git-rebase-continue)


(setq my/vc-vi-state-modify-map (make-sparse-keymap))
(define-key my/vc-vi-state-modify-map "d" #'log-view-diff)
(define-key my/vc-vi-state-modify-map "a" #'log-view-annotate-version)
(define-key my/vc-vi-state-modify-map "f" #'log-view-find-revision)
(define-key my/vc-vi-state-modify-map "D" #'log-view-diff-changeset)

(viper-modify-major-mode 'log-view-mode 'vi-state my/vc-vi-state-modify-map)
(viper-modify-major-mode 'vc-git-log-view-mode 'vi-state my/vc-vi-state-modify-map)

(setq my/dired-vi-state-modify-map (make-sparse-keymap))
(define-key my/dired-vi-state-modify-map "-" #'dired-up-directory)
(define-key my/dired-vi-state-modify-map "m" #'dired-mark)
(define-key my/dired-vi-state-modify-map "D" #'dired-do-delete)
(define-key my/dired-vi-state-modify-map "C" #'dired-do-copy)
(define-key my/dired-vi-state-modify-map "R" #'dired-do-rename)

(viper-modify-major-mode 'dired-mode 'vi-state my/dired-vi-state-modify-map)
