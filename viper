(setq my/g-prefix-map (make-sparse-keymap))
(define-key viper-vi-basic-map "g" my/g-prefix-map)
(define-key my/g-prefix-map "g" (lambda () (interactive) (viper-goto-line 1)))

(define-key my/g-prefix-map "k" #'viper-previous-line)
(define-key my/g-prefix-map "j" #'viper-next-line)

(define-key my/g-prefix-map "t" #'tab-bar-switch-to-next-tab)
(define-key my/g-prefix-map "T" #'tab-bar-switch-to-prev-tab)

(define-key my/g-prefix-map "zz" #'cua-rectangle-mark-mode)

(defvar viper-leader-map (make-sparse-keymap))
(define-key viper-vi-basic-map " " viper-leader-map)
(define-key viper-insert-basic-map (kbd "M-SPC") viper-leader-map)
(define-key viper-insert-basic-map (kbd "C-w") #'kill-region)


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
(define-key my/viper-vi-motion-leader-keymap " " viper-leader-map)

(defun viper--create-and-set-mode (mode)
  "Create a major-mode modification map in viper for MODE.
  Should not overwrite existing modifications to major mode.
  MODE is a major mode symbol."
  (let ((modified-map-sym (intern (concat "viper-modified-" (symbol-name mode) "-map" )))
        (major-mode-map (symbol-value (intern (concat (symbol-name mode) "-map"))))
        (viper-base-mappings (list my/viper-vi-basic-motion-keymap
                                   my/viper-vi-motion-g-keymap
                                   my/viper-vi-motion-leader-keymap)))
    ;; without the extra make-composed-keymap indirection
    ;; we will either modify the base mappings which is bad because then
    ;; p will do magit-push instead of yank for instance
    ;; in the case where viper-base-mappings is the second argument
    ;; (set modified-map-sym (make-composed-keymap viper-base-mappings major-mode-map))
    
    ;; or in the other case, we won't be able to override the viper base mappings
    ;; i.e. having p only do magit-push in magit buffers

    ;; this indirection way lets us basically make a sparse-keymap
    ;; so that any define-keys just go into the sparse keymap without changing our viper-base-mappings
    ;; with a parent thats a combination of major-mode + viper-base
    ;; where viper-base has precendence over major-mode
    ;; i.e. we want viper movement in magit, not the magit command for j
    (set modified-map-sym (make-composed-keymap nil
                                                (make-composed-keymap
                                                 viper-base-mappings
                                                 major-mode-map)))
    
    (viper-modify-major-mode mode 'vi-state (symbol-value modified-map-sym))
    modified-map-sym))


(defun viper--unquote (form)
  (while (memq (car-safe form) '(quote function))
    (setq form (cadr form)))
  form)

(defun viper--map-process (rest)
  (let ((normal-state-map-sym)
        (insert-state-map-sym)
        (current-map-sym))
    (while rest
      (let ((key (pop rest)))
        (cond ((keywordp key)
               (pcase key
                 (:leader (setq current-map-sym 'viper-leader-map))
                 (:mode
                  (setq normal-state-map-sym (viper--create-and-set-mode (pop rest)))
                  (message "mode %s" normal-state-map-sym))
                 (:n
                  (if normal-state-map-sym 
                      (setq current-map-sym normal-state-map-sym)
                    (setq current-map-sym 'viper-vi-basic-map))
                  )
                 ))
              (current-map-sym
               (let ((cmd (pop rest)))
                 (when (commandp cmd)
                   (define-key (symbol-value current-map-sym) (kbd key) cmd))
                 )))
        ))))

(defun viper-map! (&rest rest)
  (viper--map-process rest))

(defface mode-line-green
  (if (facep 'modus-themes-fg-green-warmer)
      '((t :inherit modus-themes-fg-green-warmer))
    '((t :foreground "green")))
  "face used for modeline"
  :group 'basic-faces)

(defface mode-line-red
  (if (facep 'modus-themes-fg-red-warmer)
      '((t :inherit modus-themes-fg-red-warmer))
    '((t :foreground "red")))
  "face used for modeline"
  :group 'basic-faces)

(setq-default global-mode-string (delq 'viper-mode-string global-mode-string))
(setq-default viper-vi-state-id (propertize viper-vi-state-id 'face '(:inherit mode-line-green :weight bold)))
(setq-default viper-emacs-state-id (propertize viper-emacs-state-id 'face 'warning))
(setq-default viper-replace-state-id (propertize viper-replace-state-id 'face '(:inherit mode-line-red :weight bold)))

(setq viper-case-fold-search t)

(setq my/global-viper-state 'vi)
(defun set-global-viper-state ()
  (cond ((eq my/global-viper-state 'vi) (viper-change-state-to-vi))
        ((eq my/global-viper-state 'emacs) (viper-change-state-to-emacs))
        ((eq my/global-viper-state 'insert) (viper-change-state-to-insert))
        (t (viper-change-state-to-vi))
        ))

(add-hook 'viper-vi-state-hook (lambda ()
                                 (unless (minibuffer-window-active-p (selected-window))
                                   (setq my/global-viper-state 'vi))))
(add-hook 'viper-emacs-state-hook (lambda ()
                                    (unless (minibuffer-window-active-p (selected-window))
                                      (setq my/global-viper-state 'emacs))))
(add-hook 'viper-insert-state-hook (lambda ()
                                     (unless (minibuffer-window-active-p (selected-window))
                                       (setq my/global-viper-state 'insert))))
(add-to-list 'window-state-change-functions
             (lambda (_)
               (if (minibuffer-window-active-p (selected-window))
                   (viper-change-state-to-insert)
                 (set-global-viper-state))))

(setq viper-emacs-state-mode-list nil)
(setq viper-insert-state-mode-list nil)

(setq viper-inhibit-startup-message 't)
(setq viper-expert-level '5)

(add-hook 'viper-insert-state-hook (lambda ()
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

(setq viper-insert-state-cursor-color nil)

;; This is so backspace/delete goes backward directories instead of just deleting characters
(setq my/minibuffer-modify-map (make-sparse-keymap))
(define-key my/minibuffer-modify-map (kbd "<backspace>") #'icomplete-fido-backward-updir)
(define-key my/minibuffer-modify-map (kbd "<DEL>") #'icomplete-fido-backward-updir)
(viper-modify-major-mode 'minibuffer-mode 'insert-state my/minibuffer-modify-map)
(viper-modify-major-mode 'minibuffer-mode 'emacs-state my/minibuffer-modify-map)

(define-key viper-insert-basic-map (kbd "M-<tab>") #'completion-at-point)

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

(defun delete-bottom-side-window ()
  (interactive)
  (when (eq viper-current-state 'vi-state) 
    (when-let ((side-window (window-with-parameter 'window-side 'bottom))
               (buffer-major-mode (with-current-buffer (window-buffer side-window) major-mode)))
      (unless (or (eq 'eshell-mode buffer-major-mode) (eq 'shell-mode buffer-major-mode))
        (delete-window side-window)))))
(advice-add 'viper-intercept-ESC-key :before #'delete-bottom-side-window)

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

(if (display-graphic-p)
    (progn
      (keyboard-translate ?\C-i ?\H-i)
      (define-key viper-vi-basic-map (kbd "H-i") #'my/mark-ring-backward)
      (define-key viper-vi-basic-map "\t" nil)
      (define-key viper-vi-basic-map "\C-o" #'my/mark-ring-forward)
      )
  (progn
    (define-key viper-vi-basic-map [C-i] #'my/mark-ring-backward)
    (define-key viper-vi-basic-map "\C-o" #'my/mark-ring-forward)
    )
  )

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

(defun viper-call-underlying-keymap-cmd (&optional alt-cmd)
  "Temporarily change to emacs state, and see what the underlying keybinding is for `show-invoking-keys'.
   If the underlying command is like an \"insert-command\" then we either do nothing or execute ALT-CMD."
  (interactive)
  (let ((curr-state my/global-viper-state))
    (viper-change-state-to-emacs)
    (setq my/global-viper-state curr-state)
    (when-let ((local-cmd (key-binding (this-command-keys))))
      ;; need to call this BEFORE the local-cmd
      ;; in the case where local-cmd is debugger-quit
      ;; which will exit out of this function so we stay in emacs state
      (set-global-viper-state)
      (if (string-match-p "\\(.*insert-command\\|newline\\)" (symbol-name local-cmd))
          (when (commandp alt-cmd) (call-interactively alt-cmd))
        
        (call-interactively local-cmd))
      )
    ))

(defun maybe-open-url-at-pt ()
  (interactive)
  (let ((maybe-url (help-at-pt-kbd-string)))
    (if (and maybe-url (string-match "https?://.*" maybe-url))
        (eww-browse-url maybe-url)
      (viper-call-underlying-keymap-cmd nil)
      ))
  )

(define-key viper-vi-basic-map (kbd "RET")
            (lambda ()
              (interactive)
              (viper-call-underlying-keymap-cmd #'maybe-open-url-at-pt)
              )
            )

(defun my/macro ()
  "Don't like the default `viper-register-macro'.
Prefer it to behave more like vim/evil mode's version."
  (interactive)
  (if defining-kbd-macro
      (progn 
        (end-kbd-macro)
        (viper-set-register-macro my/macro-register))
    (setq my/macro-register (downcase (read-char)))
    (call-interactively #'start-kbd-macro)
    )
  )

(define-key viper-vi-basic-map "q"
            (lambda ()
              (interactive)
              (viper-call-underlying-keymap-cmd #'my/macro)
              ))

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
(define-key viper-vi-basic-map (kbd "C-v") #'my/visual-block)

;;(advice-mapc `(lambda (fun props) (advice-remove 'viper-ex fun)) 'viper-ex)
(advice-add 'viper-ex :around
            (lambda (orig-fun &rest args)
              (let ((current-prefix-arg t))
                (if (use-region-p) (apply orig-fun current-prefix-arg args)
                  (apply orig-fun args)))))

;; (advice-mapc `(lambda (fun props) (advice-remove 'viper-join-lines fun)) 'viper-join-lines)
(defun viper-join-lines-region-advice (orig-fun arg &rest args)
  (interactive "P")
  (if (use-region-p)
      (let* ((start (region-beginning))
             (end (region-end))
             (numlines (count-lines start end)))
        (goto-char start)
        (apply orig-fun `(,numlines)))
    (apply orig-fun `(,arg))))
(advice-add 'viper-join-lines :around #'viper-join-lines-region-advice)

(setq my/line-yank-p nil)
(defun viper-delete-region-or-motion-command (arg)
  "convenience function for deleting a region, including rectangles"
  (interactive "P")
  (if (use-region-p)
      (let ((start (region-beginning)) (end (region-end)))
        (if rectangle-mark-mode
            (progn 
              (setq my/line-yank-p nil)
              (kill-rectangle start end arg))
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
              (copy-rectangle-as-kill start end))
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

(defun dumb-change-surrounding ()
  "Basic surrounding change function.
        The first char read, is the surrounding to find.
        The second char read, is the new surrounding character.
        Some DWIM here regarding parentheses and brackets."
  (interactive)
  (let* ((delim (char-to-string (read-char "find")))
         (replace-start-1 (char-to-string (read-char "replace")))
         (replace-start (cond ((string= replace-start-1 "]") "[") ;; replacing closing with opening
                              ((string= replace-start-1 ")") "(")
                              (t replace-start-1)))

         (replace-end (cond ((string= replace-start "[") "]") ;; replacing opening with closing
                            ((string= replace-start "(") ")")
                            (t replace-start))))

    (cond ((or (string= delim "(") (string= delim "["))
           (search-backward delim (save-excursion (backward-paragraph) (point)))
           (save-excursion (forward-sexp) (delete-char -1)
                           (insert replace-end))
           (delete-char 1)
           (insert replace-start))
          (t
           (save-excursion
             (search-backward delim (save-excursion (backward-paragraph) (point)))
             (delete-char 1)
             (insert replace-start)
             (search-forward delim (save-excursion (forward-paragraph) (point)))
             (delete-char -1)
             (insert replace-end)
             )
           )
          )))

(defun viper-command-advice (orig-fun &rest args)
  "Frontload one of the read-char calls so we can attach our own functions.
    Subsequent calls to read-char use the original implementation.
  See: https://stackoverflow.com/questions/67850020/how-to-call-the-original-function-in-a-cl-letf-overridden-function"
  (let ((char (read-char))
        (num-read-char-calls 0))
    (cond ((and (eq last-command-event ?c) (viper= ?s char)) (dumb-change-surrounding))
          ((and (eq last-command-event ?<) (viper= ?< char))
           (indent-rigidly (line-beginning-position) (line-end-position) (- tab-width)))
          ((and (eq last-command-event ?>) (viper= ?> char))
           (indent-rigidly (line-beginning-position) (line-end-position) tab-width))
          (t
           (cl-letf ((old-read-char (symbol-function 'read-char))
                     ((symbol-function 'read-char)
                      (lambda (&rest _)
                        (if (gt num-read-char-calls 0)
                            (progn 
                              (funcall old-read-char))
                          (progn
                            (setq num-read-char-calls (1+ num-read-char-calls))
                            char)))))
             (apply orig-fun args))
           ))))

(define-key viper-vi-basic-map (kbd ">") (lambda ()
                                           (interactive)
                                           (if (region-active-p)
                                               (let ((beg (region-beginning))
                                                     (end (region-end))
                                                     (deactivate-mark nil))
                                                 (indent-rigidly (region-beginning)
                                                                 (region-end) 
                                                                 tab-width))
                                             (call-interactively #'viper-command-argument))))

(define-key viper-vi-basic-map (kbd "<") (lambda ()
                                           (interactive)
                                           (if (region-active-p)
                                               (let ((beg (region-beginning))
                                                     (end (region-end))
                                                     (deactivate-mark nil))
                                                 (indent-rigidly (region-beginning)
                                                                 (region-end) 
                                                                 (- tab-width)))
                                             (call-interactively #'viper-command-argument))))


(advice-add 'viper-command-argument :around #'viper-command-advice)

(define-key viper-vi-basic-map "u" #'undo-only)
(define-key viper-vi-basic-map (kbd "C-r") #'undo-redo)
(define-key viper-vi-basic-map (kbd "C-S-r")  #'isearch-backward)
;; replaces move-to-window-line-top-bottom but we use H M L in vi anyways
(define-key viper-vi-basic-map (kbd "M-r")  #'isearch-backward) 
(define-key viper-vi-basic-map (kbd "C-M-r") #'isearch-backward-regexp)

(viper-map! :leader ","
            (lambda () (interactive)
              (project-switch-to-buffer (project--read-project-buffer)))
            "<" #'switch-to-buffer
            "u" #'universal-argument
            "F" #'project-find-file "G" #'project-find-regexp
            "X" #'org-capture
            "x" (lambda () (interactive)
                  (split-window-vertically)
                  (windmove-down)
                  (scratch-buffer)))

(viper-map! :leader
            "pp" #'project-switch-project "pd" #'project-forget-project
            "pe" #'project-eshell "ps" #'project-shell 
            "px" #'flymake-show-project-diagnostics)

(viper-map! :leader "'"
            (lambda () (interactive)
              (minibuffer-with-setup-hook
                  (lambda () (previous-history-element 1))
                (call-interactively 'project-find-regexp))))

(viper-map! :leader
            "hk" #'describe-key
            "hK" #'describe-keymap
            "hf" #'describe-function
            "hv" #'describe-variable
            "hm" #'describe-mode
            "ho" #'describe-symbol)

(viper-map! :leader
            "br" #'revert-buffer
            "bp" #'previous-buffer
            "bn" #'next-buffer
            "bi" #'ibuffer)

(viper-map! :leader
            "<tab>n" #'tab-bar-new-tab
            "<tab>d" #'tab-bar-close-tab
            "<tab>r" #'tab-bar-rename-tab
            "<tab>." #'tab-bar-switch-to-tab
            "<tab>[" #'tab-bar-switch-to-next-tab
            "<tab>]" #'tab-bar-switch-to-prev-tab)

(viper-map! :leader 
            "nrl" #'list-bookmarks
            "nri" #'bookmark-set
            "nrn" #'bookmark-set
            "nrd" #'bookmark-delete)

(setq notes-directory "~/notes/")

(defun my/open-simple-notes ()
  (interactive)
  (let* ((files (directory-files notes-directory t directory-files-no-dot-files-regexp))
         (open-or-create (completing-read "open/create note:" files)))
    (if (memq open-or-create files)
        (find-file open-or-create)
      (find-file (concat notes-directory open-or-create))
      )
    )
  )
(viper-map! :leader "do" #'my/open-simple-notes)

(viper-map! :leader "Nt" #'newsticker-treeview)

(viper-map! :leader "ff" #'find-file)

(viper-map! :leader "gg" (lambda (arg) (interactive "P")
                                        (vc-dir
                                         (if arg
                                             (file-truename
                                              (read-directory-name "VC status for directory: "
                                                                   default-directory nil t nil))
                                           (or (vc-root-dir) default-directory))
                                         (when (equal arg '(16))
                                           (intern
                                            (completing-read
                                             "Use VC backend: "
                                             (mapcar (lambda (b) (list (symbol-name b)))
                                                     vc-handled-backends)
                                             nil t nil nil))
                                           )
                                         )))

(use-package xref :defer t
  :config
  (viper-map! :leader "cd" #'xref-find-definitions
              "cD" #'xref-find-references
              "cj" #'xref-find-apropos

              :n "gd" (lambda () (interactive)
                        (condition-case err (call-interactively #'xref-find-definitions)
                          (error
                           ;; fallback to dumb jump
                           (let ((xref-backend-functions #'dumb-jump-xref-activate))
                             (call-interactively #'xref-find-definitions))
                           )
                          ))
              "gD" #'xref-find-references
              )
  )
(use-package eglot :defer t
  :config
  (viper-map! :leader "cr" #'eglot-rename
              "cf" #'eglot-format-buffer
              "ca" #'eglot-code-actions
              :n "gI" #'eglot-find-implementation
              ))

(viper-map! :n "K" #'eldoc)
(define-key prog-mode-map (kbd "C-<return>") #'default-indent-new-line)

(setq show-paren-highlight-openparen t)
(setq show-paren-when-point-inside-paren t)
(defun show-paren--locate-near-paren-ad (orig-fun &rest args)
  "Locate an unescaped paren \"near\" point to show.
If one is found, return the cons (DIR . OUTSIDE), where DIR is 1
for an open paren, -1 for a close paren, and OUTSIDE is the buffer
position of the outside of the paren.  Otherwise return nil."
  (if (eq my/global-viper-state 'vi)
      (let* ((before (show-paren--categorize-paren (point))))
        (when (or
               (eq (car before) 1)
               (eq (car before) -1))
          before))
    (funcall orig-fun)))

(advice-add 'show-paren--locate-near-paren :around #'show-paren--locate-near-paren-ad)

(viper-map! :n "H"
            (lambda (arg) (interactive "P")
              (if arg (viper-window-top arg)
                (viper-window-top (+ scroll-margin 1))))
            "L"
            (lambda (arg) (interactive "P")
              (if arg (viper-window-bottom arg)
                (viper-window-bottom (+ scroll-margin 1))))
            "zz" #'recenter-top-bottom)

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

(use-package hideshow
  :config
  (viper-map! :n "zC" #'hs-hide-all "zO" #'hs-show-all
              "zo" #'hs-show-block "zc" #'hs-hide-block
              "za" #'hs-toggle-hiding))

;; local alist that can be used as part of a major mode hook to
;; add pseudo keybinds to brac and ket
(setq brac-char-cmd-alist '())
;; [ - backwards
(defun viper-brac-advice (orig-fun &rest args)
  (let ((char (read-char)))
    (cond ((viper= ?b char) (previous-buffer))
          ((viper= ?t char) (tab-bar-switch-to-prev-tab))
          ((viper= ?e char) (call-interactively 'flymake-goto-prev-error))
          (t
           (let ((other-cmd
                  (cdr (cl-find-if (lambda (e)
                                     (viper= (car e) char))
                                   brac-char-cmd-alist))))
             (if other-cmd
                 (call-interactively other-cmd)
               ;; hack so that we can override read-char and only need input once
               (cl-letf (((symbol-function 'read-char) (lambda (_ _ _) char)))
                 (apply orig-fun args))))))))
(advice-add 'viper-brac-function :around #'viper-brac-advice)

(setq ket-char-cmd-alist '())
;; ] - forwards
(defun viper-ket-advice (orig-fun &rest args)
  (let ((char (read-char)))
    (cond ((viper= ?b char) (next-buffer))
          ((viper= ?t char) (tab-bar-switch-to-next-tab))
          ((viper= ?e char) (call-interactively 'flymake-goto-next-error))
          (t
           (let ((other-cmd
                  (cdr (cl-find-if (lambda (e)
                                     (viper= (car e) char))
                                   ket-char-cmd-alist))))
             (if other-cmd
                 (call-interactively other-cmd)
               ;; hack so that we can override read-char and only need input once
               (cl-letf (((symbol-function 'read-char) (lambda (_ _ _) char)))
                 (apply orig-fun args))))))))
(advice-add 'viper-ket-function :around #'viper-ket-advice)

(use-package ediff :autoload (ediff-file-checked-in-p))

(defun viper-maybe-checkout (buf)
  (let ((file (expand-file-name (buffer-file-name buf)))
        (checkout-function (key-binding "\C-x\C-q")))
    (if (and (ediff-file-checked-in-p file)
             (or (beep 1) t)
             (y-or-n-p
              (format
               "File %s is checked in.  Check it out? "
               (abbreviate-file-name file))))
        (with-current-buffer buf
          (command-execute checkout-function)))))

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
  (viper-map! :mode 'vc-dir-mode
              :n "x" #'vc-dir-hide-state)
  )

(use-package diff-mode :defer t
  :config
  (add-hook 'diff-mode-hook #'outline-minor-mode)
  (set-face-foreground 'diff-refine-added "green1")
  (set-face-background 'diff-refine-added "green4")

  (set-face-foreground 'diff-refine-removed "red1")
  (set-face-background 'diff-refine-removed "red4"))

(use-package diff-mode :after outline
  :config
  (viper-map! :mode 'diff-mode
              :n "<tab>" #'outline-cycle
              "<backtab>" #'outline-cycle-buffer
              "<return>" #'diff-goto-source
              "C-j" #'diff-hunk-next
              "C-k" #'diff-hunk-prev
              "C-S-j" #'diff-file-next
              "C-S-k" #'diff-file-prev))

(use-package vc-annotate :defer t
  :config
  (viper-map! :mode 'vc-annotate-mode
              :n "C-j" #'vc-annotate-next-revision
              "C-k" #'vc-annotate-prev-revision
              "L" #'vc-annotate-show-log-revision-at-line))
(add-hook 'vc-annotate-mode-hook #'viper-mode)

(use-package dired :defer t
  :config
  (add-hook 'dired-mode-hook #'auto-revert-mode)
  (viper-map! :mode 'dired-mode
              :n "-" #'dired-up-directory
              "C" #'dired-do-copy
              "K" #'dired-kill-subdir))

(use-package ibuffer :defer t
  :config
  (viper-map! :mode 'ibuffer-mode :n "sp" #'ibuffer-pop-filter "sn" #'ibuffer-filter-by-name))

(viper-map! :mode 'emacs-lisp-mode :n "SPC meb" #'eval-buffer)

(use-package comint :defer t
  :config
  (define-key viper-comint-mode-modifier-map (kbd "C-d") #'viper-scroll-up))
