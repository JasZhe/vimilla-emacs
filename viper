(setq my/global-viper-state 'vi)
(defun set-global-viper-state (arg)
  (cond ((eq my/global-viper-state 'vi) (viper-change-state-to-vi))
        ((eq my/global-viper-state 'emacs) (viper-change-state-to-emacs))
        ((eq my/global-viper-state 'insert) (viper-change-state-to-insert))
        (t (viper-change-state-to-vi))
  ))

(add-hook 'viper-vi-state-hook (lambda () (setq my/global-viper-state 'vi)))
(add-hook 'viper-emacs-state-hook (lambda () (setq my/global-viper-state 'emacs)))
(add-hook 'viper-insert-state-hook (lambda () (setq my/global-viper-state 'insert)))
(add-to-list 'window-state-change-functions #'set-global-viper-state)

(setq viper-emacs-state-mode-list nil)
(setq viper-insert-state-mode-list nil)

(setq viper-inhibit-startup-message 't)
(setq viper-expert-level '5)

(add-hook 'viper-insert-state-hook (lambda ()
                                     (global-hl-line-mode -1)
                                     (when (not (display-graphic-p)) (send-string-to-terminal "\033[6 q"))
                                     (setq viper-ex-style-editing nil)))

(add-hook 'viper-minibuffer-exit-hook (lambda () (global-hl-line-mode) (when (not (display-graphic-p)) (send-string-to-terminal "\033[0 q"))))

(add-hook 'viper-vi-state-hook (lambda ()
                                 (global-hl-line-mode)
                                 (set-face-attribute 'hl-line nil :underline nil)
                                 (set-face-attribute 'hl-line nil :box nil)
                                 (when (not (display-graphic-p)) (send-string-to-terminal "\033[0 q"))))
(add-hook 'viper-emacs-state-hook (lambda ()
                                    (global-hl-line-mode)
                                    (if (display-graphic-p)
                                        (set-face-attribute 'hl-line nil :box t)
                                      (set-face-attribute 'hl-line nil :underline t))
                                    (when (not (display-graphic-p)) (send-string-to-terminal "\033[0 q"))))

(add-hook 'minibuffer-mode-hook #'viper-change-state-to-emacs)
(add-hook 'minibuffer-exit-hook #'viper-change-state-to-vi)
(setq viper-insert-state-cursor-color nil)

;; This is so backspace/delete goes backward directories instead of just deleting characters
(setq my/minibuffer-modify-map (make-sparse-keymap))
(define-key my/minibuffer-modify-map (kbd "<backspace>") #'icomplete-fido-backward-updir)
(define-key my/minibuffer-modify-map (kbd "<DEL>") #'icomplete-fido-backward-updir)
(viper-modify-major-mode 'minibuffer-mode 'insert-state my/minibuffer-modify-map)
(viper-modify-major-mode 'minibuffer-mode 'emacs-state my/minibuffer-modify-map)

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

(setq my/mark-ring '())
(setq my/mark-ring-max-size 16)
(setq my/mark-ring-current-pos 0)
(setq my/moving-in-progress nil)

;; only for file visiting marks
(defun my/push-mark-advice (&optional _ _ _)
  (unless my/moving-in-progress
    (let* ((new-mark (copy-marker (mark-marker)))
           (buf (marker-buffer new-mark)))
      (when (buffer-file-name buf)
        ;; transpose on mark ring pos
        (setq my/mark-ring (append (cl-subseq my/mark-ring my/mark-ring-current-pos)
                                   (cl-subseq my/mark-ring 0 my/mark-ring-current-pos)))
        ;; existing mark will be added after
        (setq my/mark-ring
              (seq-filter (lambda (m)
                            (and m (marker-buffer m) (marker-position m)
                                 (not (and (= (marker-position m) (marker-position new-mark))
                                           (eq (marker-buffer m) buf)))))
                          my/mark-ring))

        (when (gt= (length my/mark-ring) my/mark-ring-max-size)
          (setq my/mark-ring (butlast my/mark-ring)))

        (cl-pushnew new-mark my/mark-ring)
        (setq my/mark-ring-current-pos 0)))))
(advice-add 'push-mark :after #'my/push-mark-advice)

(defun my/move-to-mark (m)
  (when m
    (let* ((buf (marker-buffer m))
           (position (marker-position m))
           (my/moving-in-progress t))
      (if buf
          (progn
            (set-buffer buf)
            ;; same as pop-global-mark
            (or (and (gt= position (point-min))
                     (lt= position (point-max)))
                (if widen-automatically
                    (widen)
                  (error "mark position is outside accessible part of buffer %s"
                         (buffer-name buffer))))
            (goto-char position)
            (switch-to-buffer buf))
        (message "No buf for marker %s." m)))))

(defun my/mark-ring-forward ()
  (interactive)
  ;; when we try to go "back" we want to basically drop a marker where we were
  ;; so we can go "forward" to it later
  (when (and (eql my/mark-ring-current-pos 0)
             (not (and
                   (eql (marker-buffer (elt my/mark-ring 0)) (current-buffer))
                   (eql (marker-position (elt my/mark-ring 0)) (point)))))
    (push-mark))

  (when (and (eql
              (marker-buffer (elt my/mark-ring my/mark-ring-current-pos))
              (current-buffer))
             (eql
              (marker-position (elt my/mark-ring my/mark-ring-current-pos))
              (point)))
    (unless (eql my/mark-ring-current-pos (length my/mark-ring))
      (cl-incf my/mark-ring-current-pos)))
  (my/move-to-mark (elt my/mark-ring my/mark-ring-current-pos)))

(defun my/mark-ring-backward ()
  (interactive)
  (when (gt my/mark-ring-current-pos 0)
    (cl-decf my/mark-ring-current-pos)
    (my/move-to-mark (elt my/mark-ring my/mark-ring-current-pos))))

;; some weird hack to distinguish tab and C-i in gui, broken in terminal
;; we only want to do this in vi state so we get tab completion and stuff in insert/emacs state for tty
(add-hook 'viper-vi-state-hook (lambda () (define-key input-decode-map "\C-i" [C-i])))
(add-hook 'viper-emacs-state-hook (lambda () (define-key input-decode-map "\C-i" nil)))
(add-hook 'viper-insert-state-hook (lambda () (define-key input-decode-map "\C-i" nil)))
;; this is basically visual state hook
(add-hook 'activate-mark-hook (lambda () (define-key input-decode-map "\C-i" nil)))
(add-hook 'deactivate-mark-hook (lambda () (define-key input-decode-map "\C-i" [C-i])))

(define-key viper-vi-basic-map [C-i] #'my/mark-ring-backward)
(define-key viper-vi-basic-map "\t" nil)
(define-key viper-vi-basic-map "\C-o" #'my/mark-ring-forward)

(defun viper-previous-line (arg)
  "Go to previous line."
  (interactive "P")
  (let ((val (viper-p-val arg))
        (com (viper-getCom arg)))
    (if com (viper-move-marker-locally 'viper-com-point (point)))
    ;; do not use forward-line! need to keep column
    ;; REDEFINE: remove setting line-move-visual to nil
    (with-no-warnings (previous-line val))
    ;; END OF REDEFINE
    (if viper-ex-style-motion
        (if (and (eolp) (not (bolp))) (backward-char 1)))
    (setq this-command 'previous-line)
    (if com (viper-execute-com 'viper-previous-line val com))))

(defun viper-next-line (arg)
  "Go to next line."
  (interactive "P")
  (let ((val (viper-p-val arg))
        (com (viper-getCom arg)))
    (if com (viper-move-marker-locally 'viper-com-point (point)))
    ;; do not use forward-line! need to keep column
    ;; REDEFINE: remove setting line-move-visual to nil
    (with-no-warnings (next-line val))
    ;; END OF REDEFINE
    (if viper-ex-style-motion
        (if (and (eolp) (not (bolp))) (backward-char 1)))
    (setq this-command 'next-line)
    (if com (viper-execute-com 'viper-next-line val com))))


(advice-mapc `(lambda (fun props) (advice-remove 'viper-goto-eol fun)) 'viper-goto-eol)
(advice-add 'viper-goto-eol :around
            (lambda (orig-fun &rest args)
              (if visual-line-mode
                  (cl-letf (((symbol-function 'end-of-line) 'end-of-visual-line))
                    (apply orig-fun args))
                (apply orig-fun args))))

(defun check-if-on-visually-split-line ()
  (let ((first-logical-end
         (save-excursion (beginning-of-line) (end-of-visual-line) (point)))
        (current-end (save-excursion (end-of-visual-line) (point))))
    (> current-end first-logical-end)))

(defun viper-bol-and-skip-white (arg)
  "Beginning of line at first non-white character."
  (interactive "P")
  (let ((val (viper-p-val arg))
        (com (viper-getcom arg)))
    (if com (viper-move-marker-locally 'viper-com-point (point)))
    (if visual-line-mode
        (progn 
          (if (and (check-if-on-visually-split-line))
              (if (= val 1)
                  (beginning-of-visual-line val)
                (beginning-of-visual-line (1+ val)))
            (if (= val 1)
                (backward-to-indentation (1- val))
              (beginning-of-visual-line (1+ val)))))
      (progn
        (forward-to-indentation (1- val))
        (if com (viper-execute-com 'viper-bol-and-skip-white val com))))))

(define-key viper-vi-basic-map (kbd "RET")
            `(menu-item "" browse-url-at-point
                        :filter ,(lambda (cmd) (if (thing-at-point-url-at-point) cmd))))
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
            (progn 
              (setq my/line-yank-p nil)
              ;; like vim, we want to include the current cursor char
              (kill-rectangle start (1+ end) arg))
          (progn
            ;; this hacky bit is because when we move backwards from point, we want to include the position we started the mark on like in vim
            ;; even though visually we won't see it, functionally it'll behave the same
            (if (> (point) (mark-marker))
                (forward-char)
              (let ((m (mark-marker)))
                (set-marker m (1+ m))))
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
            (progn 
              (setq my/line-yank-p nil)
              (copy-rectangle-as-kill start (1+ end)))
          (progn
            (if (> (point) (mark-marker))
                (forward-char)
              (let ((m (mark-marker)))
                (set-marker m (1+ m))))
            (if my/line-selection-p
                (setq my/line-yank-p t)
              (setq my/line-yank-p nil))
            (copy-region-as-kill start end t)
            (when (> (point) (mark-marker)) (backward-char)))
          ))
    (viper-command-argument arg)))

(defun viper-paste-into-region (arg)
  "if region is active, delete region before pasting
respects rectangle mode in a similar way to vim/doom"
  (interactive "P")
  (cond (my/line-yank-p
         (progn
           (if (use-region-p)
               (delete-active-region)
             (viper-open-line nil))
           (viper-change-state-to-vi) ; cause viper-open-line takes us to insert
           (yank)

           ;; we want the newline at the end when the yanked text is multiline
           ;; but we want to remove the additional newline if the yanked text is
           ;; just a single line
           (when (not (string-match ".*\n.+" (cl-first kill-ring)))
             (forward-line)
             (delete-char -1)
             (forward-line -1)
             (end-of-line))
           ))
        ((and (not killed-rectangle) (use-region-p))
         (progn
           (let ((start (region-beginning)))
             ;; vim pastes "after" the cursor, at least that's what I'm used to
             (forward-char)
             (delete-active-region)
             (yank))))
        (killed-rectangle
         (progn
           (forward-char)
           (yank-rectangle)
           (setq killed-rectangle nil)))
        ;; if we're on an empty line, we want to just yank without moving forward
        (t (unless (eq (point) (line-end-position)) (forward-char)) (yank arg))))

(define-key viper-vi-basic-map "d" #'viper-delete-region-or-motion-command)
(define-key viper-vi-basic-map "y" #'viper-copy-region-or-motion-command)
(define-key viper-vi-basic-map "p" #'viper-paste-into-region)
(define-key viper-vi-basic-map (kbd "s-v") #'viper-paste-into-region)
(define-key viper-insert-basic-map (kbd "s-v") #'viper-paste-into-region)
(define-key global-map (kbd "s-v") #'viper-paste-into-region)

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
            (lambda () (interactive) (project-switch-to-buffer (project--read-project-buffer))))
(define-key my/leader-prefix-map "<" #'switch-to-buffer)

(define-key my/leader-prefix-map "u" #'universal-argument)
(define-key universal-argument-map " u" #'universal-argument-more)

(define-key my/leader-prefix-map "F" #'project-find-file)
(define-key my/leader-prefix-map "G" #'my/igrep)
(define-key my/leader-prefix-map "X" #'org-capture)

(define-key my/leader-prefix-map "x"
            (lambda () (interactive)
              (split-window-vertically)
              (windmove-down)
              (scratch-buffer)))

(define-key my/leader-prefix-map "oe" #'eshell)
(define-key my/leader-prefix-map "os" #'shell)

(define-key my/leader-prefix-map "pp" #'project-switch-project)
(define-key my/leader-prefix-map "pe" #'project-eshell)
(define-key my/leader-prefix-map "ps" #'project-shell)
(define-key my/leader-prefix-map "pd" #'project-forget-project)
(define-key my/leader-prefix-map "px" #'flymake-show-project-diagnostics)

(defun my/flymake-diagnostics-at-point ()
  (interactive)
  (let ((diags (flymake-diagnostics (point))))
    (if (not (seq-empty-p diags))
        (message "%s"
                 (cl-reduce (lambda (acc d) (concat acc (flymake--diag-text d)))
                            (flymake-diagnostics (point))
                            :initial-value ""))
      (message "No diagnostics at point."))))

(define-key my/leader-prefix-map "cx" #'my/flymake-diagnostics-at-point)
(define-key my/leader-prefix-map "cX" #'flymake-show-buffer-diagnostics)

(define-key my/leader-prefix-map "hk" #'describe-key)
(define-key my/leader-prefix-map "hf" #'describe-function)
(define-key my/leader-prefix-map "hv" #'describe-variable)
(define-key my/leader-prefix-map "hm" #'describe-mode)
(define-key my/leader-prefix-map "ho" #'describe-symbol)

(define-key my/leader-prefix-map "br" #'revert-buffer)
(define-key my/leader-prefix-map "bp" #'previous-buffer)
(define-key my/leader-prefix-map "bn" #'next-buffer)
(define-key my/leader-prefix-map "bi" #'ibuffer)

(setq my/tab-prefix-map (make-sparse-keymap))
(define-key my/leader-prefix-map "\t" my/tab-prefix-map)
(define-key my/leader-prefix-map [C-i] my/tab-prefix-map) ;; so it works in terminal
(define-key my/tab-prefix-map "n" #'tab-bar-new-tab)
(define-key my/tab-prefix-map "d" #'tab-bar-close-tab)
(define-key my/tab-prefix-map "r" #'tab-bar-rename-tab)

(define-key my/leader-prefix-map "ss" #'my/ioccur)
;; not sure why but we need to rescan the imenu for our igrep xref buffer
(define-key my/leader-prefix-map "si"
            (lambda () (interactive)
              (imenu--menubar-select imenu--rescan-item)
              (call-interactively 'imenu)))

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

(setq my/viper-vi-basic-motion-keymap (make-sparse-keymap))
(define-key my/viper-vi-basic-motion-keymap "h" #'viper-backward-char)
(define-key my/viper-vi-basic-motion-keymap "l" #'viper-forward-char)
(define-key my/viper-vi-basic-motion-keymap "j" #'next-line)
(define-key my/viper-vi-basic-motion-keymap "k" #'previous-line)
(define-key my/viper-vi-basic-motion-keymap "w" #'viper-forward-word)
(define-key my/viper-vi-basic-motion-keymap "b" #'viper-backward-word)
(define-key my/viper-vi-basic-motion-keymap "e" #'viper-end-of-word)
(define-key my/viper-vi-basic-motion-keymap "v" #'my/set-mark-command)
(define-key my/viper-vi-basic-motion-keymap "V" #'my/select-lines)
(define-key my/viper-vi-basic-motion-keymap (kbd "C-v") #'my/visual-block)
(define-key my/viper-vi-basic-motion-keymap "y" #'viper-copy-region-or-motion-command)
(define-key my/viper-vi-basic-motion-keymap "^" #'viper-bol-and-skip-white)
(define-key my/viper-vi-basic-motion-keymap "$" #'viper-goto-eol)
(define-key my/viper-vi-basic-motion-keymap (kbd "C-d") #'viper-scroll-up)
(define-key my/viper-vi-basic-motion-keymap "\C-w" my-window-map)

(setq my/viper-vi-extra-motion-keymap my/viper-vi-basic-motion-keymap)
(define-key my/viper-vi-extra-motion-keymap "W" #'viper-forward-Word)
(define-key my/viper-vi-extra-motion-keymap "B" #'viper-backward-Word)
(define-key my/viper-vi-extra-motion-keymap "E" #'viper-end-of-Word)

(define-key my/viper-vi-extra-motion-keymap "f" #'viper-find-char-forward)
(define-key my/viper-vi-extra-motion-keymap "F" #'viper-find-char-backward)
(define-key my/viper-vi-extra-motion-keymap "t" #'viper-goto-char-forward)
(define-key my/viper-vi-extra-motion-keymap "T" #'viper-goto-char-backward)

(setq my/viper-vi-motion-g-keymap (make-sparse-keymap))
(define-key my/viper-vi-motion-g-keymap "g" my/g-prefix-map)
(define-key my/viper-vi-motion-g-keymap "G" #'viper-goto-line)

(setq my/viper-vi-motion-leader-keymap (make-sparse-keymap))
(define-key my/viper-vi-motion-leader-keymap " " my/leader-prefix-map)

(define-key my/leader-prefix-map "cd" #'xref-find-definitions)
(define-key viper-vi-basic-map "gd" #'xref-find-definitions)
(define-key viper-vi-basic-map "gI" #'eglot-find-implementation)

(define-key my/leader-prefix-map "cD" #'xref-find-references)
(define-key viper-vi-basic-map "gD" #'xref-find-references)

(define-key my/leader-prefix-map "cr" #'eglot-rename)
(define-key my/leader-prefix-map "fm" #'eglot-format-buffer)
(define-key my/leader-prefix-map "ca" #'eglot-code-actions)

(define-key viper-vi-basic-map "K" #'eldoc)
(define-key prog-mode-map (kbd "C-<return>") #'default-indent-new-line)

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

;; if the region is active already, we don't want to move mark or else it behaves strangely with out selection
(defun my/advise-viper-goto-line (orig-fun &rest args)
  (if (region-active-p)
      (cl-letf (((symbol-function 'deactivate-mark)
                 (lambda (&optional _) nil))
                ((symbol-function 'viper-move-marker-locally)
                 (lambda (_ _ &optional _) nil))
                ((symbol-function 'push-mark)
                 (lambda (&optional _ _ _) nil)))
        (let ((prev-line-number (line-number-at-pos)))
          (apply orig-fun args)

          (when my/line-selection-p
            ;; this means we're moving up so need to go to beg of line at the end
            (if (and (car args) (< (car args) prev-line-number))
                (beginning-of-line)
              (end-of-line)))))
    (apply orig-fun args)))

(advice-add 'viper-goto-line :around #'my/advise-viper-goto-line)

(add-hook 'prog-mode-hook #'hs-minor-mode)
(define-key viper-vi-basic-map "zC" #'hs-hide-all)
(define-key viper-vi-basic-map "zO" #'hs-show-all)
(define-key viper-vi-basic-map "zo" #'hs-show-block)
(define-key viper-vi-basic-map "zc" #'hs-hide-block)
(define-key viper-vi-basic-map "za" #'hs-toggle-hiding)

;; [ - backwards
(advice-mapc `(lambda (fun props) (advice-remove 'viper-brac-function fun)) 'viper-brac-function)
(advice-add 'viper-brac-function :around
            (lambda (orig-fun &rest args)
              (let ((char (read-char)))
                (cond ((viper= ?b char) (previous-buffer))
                      ((viper= ?t char) (tab-bar-switch-to-prev-tab))
                      ((viper= ?e char) (call-interactively 'flymake-goto-prev-error))
                      (t
                       ;; hack so that we can override read-char and only need input once
                       (cl-letf (((symbol-function 'read-char) (lambda (_ _ _) char)))
                         (apply orig-fun args)
                         )
                       )
                      ))))
;; ] - forwards
(advice-mapc `(lambda (fun props) (advice-remove 'viper-key-function fun)) 'viper-key-function)
(advice-add 'viper-ket-function :around
            (lambda (orig-fun &rest args)
              (let ((char (read-char)))
                (cond ((viper= ?b char) (next-buffer))
                      ((viper= ?t char) (tab-bar-switch-to-next-tab))
                      ((viper= ?e char) (call-interactively 'flymake-goto-next-error))
                      (t
                       ;; hack so that we can override read-char and only need input once
                       (cl-letf (((symbol-function 'read-char) (lambda (_ _ _) char)))
                         (apply orig-fun args)
                         )
                       )
                      ))))

(define-key global-map "\C-xvf" #'vc-pull)
(define-key global-map "\C-xvF" #'my/vc-git-fetch)

(define-key global-map "\C-xve" #'my/vc-git-editor-command)
(define-key global-map "\C-xvRi" #'my/vc-git-rebase-i)
(define-key global-map "\C-xvRa" #'my/vc-git-rebase-abort)
(define-key global-map "\C-xvRc" #'my/vc-git-rebase-continue)

(define-key global-map "\C-xvSs" #'vc-git-stash)
(define-key global-map "\C-xvSS" #'vc-git-stash-show)
(define-key global-map "\C-xvSp" #'vc-git-stash-pop)
(define-key global-map "\C-xvSa" #'vc-git-stash-apply)
(define-key global-map "\C-xvSd" #'vc-git-stash-delete)
(use-package vc-git :defer t
  :config
  (setq my/vc-log-vi-state-modify-map
        (make-composed-keymap
         nil
         (make-composed-keymap
          (list my/viper-vi-basic-motion-keymap
                my/viper-vi-motion-g-keymap
                my/viper-vi-motion-leader-keymap)
          vc-git-log-view-mode-map)))
  (define-key my/vc-log-vi-state-modify-map (kbd "C-j") #'log-view-msg-next)
  (define-key my/vc-log-vi-state-modify-map (kbd "C-k") #'log-view-msg-prev)
  (viper-modify-major-mode 'vc-git-log-view-mode 'vi-state my/vc-log-vi-state-modify-map))

(use-package vc-dir :defer t
  :config
  (setq my/vc-dir-vi-state-modify-map
        (make-composed-keymap
         nil 
         (make-composed-keymap
          (list my/viper-vi-basic-motion-keymap
                my/viper-vi-motion-g-keymap
                my/viper-vi-motion-leader-keymap)
          vc-dir-mode-map)))
  (define-key my/vc-dir-vi-state-modify-map "x" #'vc-dir-hide-state)
  (viper-modify-major-mode 'vc-dir-mode 'vi-state my/vc-dir-vi-state-modify-map))

(use-package diff-mode :defer t
  :config
  (setq my/diff-mode-vi-state-map
        (make-composed-keymap
         nil 
         (make-composed-keymap
          (list my/viper-vi-basic-motion-keymap
                my/viper-vi-motion-g-keymap
                my/viper-vi-motion-leader-keymap)
          diff-mode-map)))
  (define-key my/diff-mode-vi-state-map [C-i] #'diff-hunk-next)
  (define-key my/diff-mode-vi-state-map (kbd "C-j") #'diff-hunk-next)
  (define-key my/diff-mode-vi-state-map (kbd "C-k") #'diff-hunk-prev)
  (viper-modify-major-mode 'diff-mode 'vi-state my/diff-mode-vi-state-map))

(use-package dired :defer t
  :config
  (add-hook 'dired-mode-hook #'auto-revert-mode)
  (setq my/dired-vi-state-modify-map
        (make-composed-keymap
         nil
         (make-composed-keymap
          (list my/viper-vi-basic-motion-keymap
                my/viper-vi-motion-g-keymap
                my/viper-vi-motion-leader-keymap)
          dired-mode-map)))
  (define-key my/dired-vi-state-modify-map "-" #'dired-up-directory)
  (define-key my/dired-vi-state-modify-map "C" #'dired-do-copy)
  (viper-modify-major-mode 'dired-mode 'vi-state my/dired-vi-state-modify-map))

(use-package ibuffer :defer t
  :config
  (setq my/ibuffer-vi-state-modify-map
        (make-composed-keymap
         nil
         (make-composed-keymap
          (list my/viper-vi-basic-motion-keymap
                my/viper-vi-motion-g-keymap
                my/viper-vi-motion-leader-keymap)
          ibuffer-mode-map)))
  (viper-modify-major-mode 'ibuffer-mode 'vi-state my/ibuffer-vi-state-modify-map))

(setq my/elisp-vi-state-modify-map (make-sparse-keymap))
(define-key my/elisp-vi-state-modify-map " meb" #'eval-buffer)
(viper-modify-major-mode 'emacs-lisp-mode 'vi-state my/elisp-vi-state-modify-map)

(use-package comint :defer t
  :config
  (define-key viper-comint-mode-modifier-map (kbd "C-d") #'viper-scroll-up)
  )
