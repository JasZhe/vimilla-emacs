;;; -*- lexical-binding: t; -*-

(unless (boundp 'package-archives)
  (package-initialize))

(require 'cl-lib)
(unless (fboundp 'use-package)
  (defmacro use-package (&rest body)
    "Very hacky macro to wrap each statement in a"
    `(progn 
    ,@(cl-map 'list
            (lambda (stmt)
              (message "%s" stmt)
              `(ignore-errors ,stmt)
              )
            body
      )
    )
  )
  )

;; Main use is to have my key bindings have the highest priority
;; https://github.com/kaushalmodi/.emacs.d/blob/master/elisp/modi-mode.el

(defvar my-mode-map (make-sparse-keymap)
  "Keymap for `my-mode'.")

;;;###autoload
(define-minor-mode my-mode
  "A minor mode so that my key settings override annoying major modes."
  ;; If init-value is not set to t, this mode does not get enabled in
  ;; `fundamental-mode' buffers even after doing \"(global-my-mode 1)\".
  ;; More info: http://emacs.stackexchange.com/q/16693/115
  :init-value t
  :lighter " my-mode"
  :keymap my-mode-map)

;;;###autoload
(define-globalized-minor-mode global-my-mode my-mode my-mode)

;; https://github.com/jwiegley/use-package/blob/master/bind-key.el
;; The keymaps in `emulation-mode-map-alists' take precedence over
;; `minor-mode-map-alist'
(add-to-list 'emulation-mode-map-alists `((my-mode . ,my-mode-map)))

;; Turn off the minor mode in the minibuffer
(defun turn-off-my-mode ()
  "Turn off my-mode."
  (my-mode -1))

(defun my/vc-git-editor-command (command)
  "command is a git subcommand that requires an editor.
 example usage: (my/vc-git-editor-command \"rebase -i HEAD~3\")"
  (interactive "P")
  (unless server-mode (server-force-delete) (server-mode))
  (let ((command (if command command (read-string "command: git "))))
    (compile (concat "GIT_EDITOR=\"emacsclient\" bash -c \"git " command "\""))))

(defun my/vc-git-rebase-i (&optional branch)
  "if branch isn't supplied from arg, prompt for it"
  (interactive)
  (let ((revision (if branch branch (read-string "revision: "))))
    (my/vc-git-editor-command (concat "rebase -i " revision))))

(defun my/vc-git-rebase-abort ()
  (interactive)
  (compile "git rebase --abort"))

(defun my/vc-git-rebase-continue ()                  
  (interactive)                                 
  (compile "git rebase --continue"))

(defun my/vc-git-fetch ()                  
  (interactive)                                  
  (compile "git fetch -v"))

(defun my/vc-get-remote ()
  (let* ((remote-names (split-string (shell-command-to-string "git remote")))
         (remote-urls
          (mapcar (lambda (r)
                    (cons r 
                          (string-trim
                           (shell-command-to-string (concat "git remote get-url " r)))))
                  remote-names))
         (annotation-fn (lambda (candidate) (cdr (assoc candidate remote-urls))))
         (completion-extra-properties `(:annotation-function ,annotation-fn))
         (remote (completing-read "Remote: " remote-urls)))
    remote))

(defun my/vc-set-remote-url ()
  (interactive)
  (let* ((remote (my/vc-get-remote))
         (curr-url (string-trim
                    (shell-command-to-string (concat "git remote get-url " remote))))

         (new-url (minibuffer-with-setup-hook (lambda () (insert curr-url))
                    (read-string (concat "Set url for remote <" remote ">: ")))))
    (compile (concat "git remote set-url " remote " " new-url))))

(defun my/vc-set-branch-upstream ()
  (interactive)
  (let* ((remote (my/vc-get-remote))
         (curr-branch (car (vc-git-branches)))
         (upstream (minibuffer-with-setup-hook
                       (lambda () (insert remote "/" curr-branch))
                     (read-string (concat "set branch <" curr-branch "> upstream to: ")))))
    (compile (concat "git branch -u " upstream))))

(defun my/vc-add-remote ())
(defun my/vc-delete-remote ())
(defun my/vc-set-remote-url ())

(defun vc-git-common-ancestor-diff (files &optional rev1 rev2 buffer _async)
  "Get a difference report using Git between two revisions of FILES."
  (let (process-file-side-effects
        (command "diff"))
    (vc-git--asciify-coding-system)
    (if rev2
        ;; Diffing against the empty tree.
        (unless rev1 (setq rev1 "4b825dc642cb6eb9a060e54bf8d69288fbee4904"))
      (setq command "diff-index")
      (unless rev1 (setq rev1 "HEAD")))
    (if vc-git-diff-switches
        (apply #'vc-git-command (or buffer "*vc-diff*")
           1 ; bug#21969
               files
               command
               "--exit-code"
               (append (vc-switches 'git 'diff)
                       (list "-p" (concat (or rev1 "HEAD") "..." rev2) "--")))
      (vc-git-command (or buffer "*vc-diff*") 1 files
                      "difftool" "--exit-code" "--no-prompt" "-x"
                      (concat "diff "
                              (mapconcat #'identity
                                         (vc-switches nil 'diff) " "))
                      (concat rev1 "..." rev2) "--"))))

(defun vc-git-diff-advice (orig-fun &rest args)
  (if (and (eq this-command #'vc-root-version-diff) (not current-prefix-arg))
      (apply #'vc-git-common-ancestor-diff args)
    (apply orig-fun args))
  )
(advice-add #'vc-git-diff :around #'vc-git-diff-advice)

(defalias 'gt #'>)
(defalias 'gt= #'>=)
(defalias 'lt #'<)
(defalias 'lt= #'<=)
(defun neq (obj1 obj2)
  "Convenience for not 'eq'"
  (not (eq obj1 obj2)))

(use-package ediff :defer t
  :config
  (advice-remove 'ediff-quit #'disable-y-or-n-p)
  (defun disable-y-or-n-p (orig-fun &rest args)
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) t)))
      (apply orig-fun args)))
  (advice-add 'ediff-quit :around #'disable-y-or-n-p)

  (setq ediff-keep-variants nil)
  (setq ediff-window-setup-function #'ediff-setup-windows-plain)
  (setq ediff-split-window-function #'split-window-horizontally))

(defun vc-ediff-file-at-point ()
  (interactive)
  (when (eq major-mode 'diff-mode)
    (setq my-ediff-prior-window-configuration (current-window-configuration))
    (let ((old-revision (first diff-vc-revisions))
          (new-revision (second diff-vc-revisions))
          (file-to-diff (save-window-excursion
                          (diff-goto-source)
                          (buffer-file-name))))
      (vc-version-ediff `(,file-to-diff) old-revision new-revision))))

(add-hook 'ediff-quit-hook
          (lambda () (when (window-configuration-p my-ediff-prior-window-configuration)
                       (set-window-configuration my-ediff-prior-window-configuration)))
          100)

(when (eq system-type 'gnu/linux)
  (setq x-super-keysym 'meta)
  (setq x-meta-keysym 'super))

(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)
(define-key global-map (kbd "s-/") #'comment-line)

;; terminal stuff, C-/ in case we don't have iterm config
(define-key global-map (kbd "C-/") #'comment-line)
(define-key global-map (kbd "C-_") #'comment-line)
(define-key input-decode-map "\e[1;P9" (kbd "s-/"))

;; mac ligatures
(when (fboundp 'mac-auto-operator-composition-mode)
  (mac-auto-operator-composition-mode))

(defun macos-term-select-text-to-clipboard (text)
  (unless (eq system-type 'gnu/linux)
    (shell-command (concat "echo \"" text "\" | pbcopy" ))))

;; ITERM2 MOUSE SUPPORT
(unless (or window-system (daemonp))
  (require 'mouse)
  (xterm-mouse-mode t)
  (defun track-mouse (e)) 
  (setq mouse-sel-mode t)
  (setq interprogram-cut-function #'macos-term-select-text-to-clipboard)) ;; good enough

(winner-mode)

(defun my/set-transparency-in-terminal ()
  (interactive)
  (unless (string= (face-background 'default) "unspecified-bg")
    (setq prev-default-face-bg (face-background 'default)))
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))

(defun my/unset-transparency-in-terminal ()
  (interactive)
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default prev-default-face-bg (selected-frame))))

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
(set-frame-parameter nil 'alpha-background 85)

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

(global-set-key (kbd "<pinch>") 'ignore)
(global-set-key (kbd "<C-wheel-up>") 'ignore)
(global-set-key (kbd "<C-wheel-down>") 'ignore)

(setq gc-cons-threshold most-positive-fixnum)

;; Lower threshold back to 8 MiB (default is 800kB)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 1024 1024 16)))) ;; 16MB

(run-with-idle-timer 2 t #'garbage-collect)

(setq inhibit-startup-screen t)

(use-package menu-bar :defer t :config (menu-bar-mode 0))
(use-package tool-bar :defer t :config (tool-bar-mode 0))

(setq viper-mode t)
(require 'viper)
(require 'rect)

(use-package scroll-bar :defer t)
(viper-mode)
(global-hl-line-mode)
(global-auto-revert-mode)
(setq auto-revert-verbose nil)
(global-visual-line-mode)
(when (fboundp #'global-visual-wrap-prefix-mode)
  (global-visual-wrap-prefix-mode)
  (setq visual-wrap-extra-indent 4))

(when (fboundp #'kill-ring-deindent-mode)
  (kill-ring-deindent-mode))

(add-hook 'prog-mode-hook (lambda () (modify-syntax-entry ?_ "-") (modify-syntax-entry ?_ "_")))

(setq revert-without-query '(".*")) ;; allow reverting without confirm
(setq column-number-mode t)
(setq scroll-margin 8)
(setq visual-bell t)
(setq ring-bell-function 'ignore)
(setq scroll-preserve-screen-position t)
(setq eval-expression-print-level nil)
(setq eval-expression-print-length nil)

(ignore-errors
  (make-directory (concat (file-name-directory user-init-file) ".local/"))
  (make-directory (concat (file-name-directory user-init-file) ".local/autosave/"))
  (make-directory (concat (file-name-directory user-init-file) ".local/backups/")))

(setq auto-save-file-name-transforms 
      `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" "/Users/jason.z/.emacs.d/.local/cache/autosave/tramp-\\2" t)
        (".*" ,(concat (file-name-directory user-init-file) ".local/autosave/\\1") t)))
(setq back-directory-alist `((".*" ,(concat (file-name-directory user-init-file) ".local/backups/"))))

(define-key global-map (kbd "C-S-p") #'yank-from-kill-ring)

(setq auto-save-visited-interval 7)
(auto-save-visited-mode)

(setq backup-directory-alist `(("." . "~/.emacs.d/emacs-backup")))
  (setq backup-by-copying t)
  (setq delete-old-versions t
        kept-new-versions 6
        kept-old-versions 2
        version-control t)

(defun bedrock--backup-file-name (fpath)
  "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
  (let* ((backupRootDir (concat user-emacs-directory "emacs-backup/"))
         (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path
         (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") )))
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath))
(setopt make-backup-file-name-function 'bedrock--backup-file-name)

;;; ICOMPLETE
(use-package icomplete
  :bind (:map icomplete-minibuffer-map
              ("C-n" . icomplete-forward-completions)
              ("C-p" . icomplete-backward-completions)
              ("C-v" . icomplete-vertical-toggle)
              ("RET" . icomplete-force-complete-and-exit))
  :config
  (setq icomplete-delay-completions-threshold 0)
  (setq icomplete-compute-delay 0)
  (setq icomplete-show-matches-on-no-input t)
  (setq icomplete-hide-common-prefix nil)
  (setq icomplete-prospects-height 10)
  (setq icomplete-separator " . ")
  (setq icomplete-with-completion-tables t)
  (setq icomplete-in-buffer t)
  (setq icomplete-max-delay-chars 0)
  (setq icomplete-scroll t)
  (advice-add 'completion-at-point
              :after #'minibuffer-hide-completions)

  (defcustom icomplete-vertical-selected-prefix-marker "» "
    "Prefix string used to mark the selected completion candidate.
If `icomplete-vertical-render-prefix-marker' is t, the string
setted here is used as a prefix of the currently selected entry in the
list.  It can be further customized by the face
`icomplete-vertical-selected-prefix-face'."
    :type 'string
    :group 'icomplete
    :version "31")

  (defcustom icomplete-vertical-unselected-prefix-marker "  "
    "Prefix string used on the unselected completion candidates.
If `icomplete-vertical-render-prefix-marker' is t, the string
setted here is used as a prefix for all unselected entries in the list.
list.  It can be further customized by the face
`icomplete-vertical-unselected-prefix-face'."
    :type 'string
    :group 'icomplete
    :version "31")

  (defcustom icomplete-vertical-in-buffer-adjust-list t
    "Control whether in-buffer completion should align the cursor position.
If this is t and `icomplete-in-buffer' is t, and `icomplete-vertical-mode'
is activated, the in-buffer vertical completions are shown aligned to the
cursor position when the completion started, not on the first column, as
the default behaviour."
    :type 'boolean
    :group 'icomplete
    :version "31")

  (defcustom icomplete-vertical-render-prefix-marker t
    "Control whether a marker is added as a prefix to each candidate.
If this is t and `icomplete-vertical-mode' is activated, a marker,
controlled by `icomplete-vertical-selected-prefix-marker' is shown
as a prefix to the current under selection candidate, while the
remaining of the candidates will receive the marker controlled
by `icomplete-vertical-unselected-prefix-marker'."
    :type 'boolean
    :group 'icomplete
    :version "31")

  (defface icomplete-vertical-selected-prefix-face
    '((t :inherit font-lock-keyword-face :weight bold :foreground "cyan"))
    "Face used for the prefix set by `icomplete-vertical-selected-prefix-marker'."
    :group 'icomplete
    :version "31")

  (defface icomplete-vertical-unselected-prefix-face
    '((t :inherit font-lock-keyword-face :weight normal :foreground "gray"))
    "Face used for the prefix set by `icomplete-vertical-unselected-prefix-marker'."
    :group 'icomplete
    :version "31")

  (defun icomplete-vertical--adjust-lines-for-column (lines buffer data)
    "Adjust the LINES to align with the column in BUFFER based on DATA."
    (if icomplete-vertical-in-buffer-adjust-list
        (let ((column
               (with-current-buffer buffer
                 (save-excursion
                   (goto-char (car data))
                   (current-column)))))
          (dolist (l lines)
            (add-text-properties
             0 1 `(display ,(concat (make-string column ?\s) (substring l 0 1)))
             l))
          lines)
      lines))

  (defun icomplete-vertical--add-marker-to-selected (comp)
    "Add markers to the selected/unselected COMP completions."
    (if (and icomplete-vertical-render-prefix-marker
             (get-text-property 0 'icomplete-selected comp))
        (concat (propertize icomplete-vertical-selected-prefix-marker
                            'face 'icomplete-vertical-selected-prefix-face)
                comp)
      (concat (propertize icomplete-vertical-unselected-prefix-marker
                          'face 'icomplete-vertical-unselected-prefix-face)
              comp)))

  (cl-defun icomplete--render-vertical
      (comps md &aux scroll-above scroll-below
             (total-space ; number of mini-window lines available
              (1- (min
                   icomplete-prospects-height
                   (truncate (max-mini-window-lines) 1)))))
    ;; Welcome to loopapalooza!
    ;;
    ;; First, be mindful of `icomplete-scroll' and manual scrolls.  If
    ;; `icomplete--scrolled-completions' and `icomplete--scrolled-past'
    ;; are:
    ;;
    ;; - both nil, there is no manual scroll;
    ;; - both non-nil, there is a healthy manual scroll that doesn't need
    ;;   to be readjusted (user just moved around the minibuffer, for
    ;;   example);
    ;; - non-nil and nil, respectively, a refiltering took place and we
    ;;   may need to readjust them to the new filtered `comps'.
    (when (and icomplete-scroll
               icomplete--scrolled-completions
               (null icomplete--scrolled-past))
      (cl-loop with preds
               for (comp . rest) on comps
               when (equal comp (car icomplete--scrolled-completions))
               do
               (setq icomplete--scrolled-past preds
                     comps (cons comp rest))
               (completion--cache-all-sorted-completions
                (icomplete--field-beg)
                (icomplete--field-end)
                comps)
               and return nil
               do (push comp preds)
               finally (setq icomplete--scrolled-completions nil)))
    ;; Then, in this pretty ugly loop, collect completions to display
    ;; above and below the selected one, considering scrolling
    ;; positions.
    (cl-loop with preds = icomplete--scrolled-past
             with succs = (cdr comps)
             with space-above = (- total-space
                                   1
                                   (cl-loop for (_ . r) on comps
                                            repeat (truncate total-space 2)
                                            while (listp r)
                                            count 1))
             repeat total-space
             for neighbor = nil
             if (and preds (> space-above 0)) do
             (push (setq neighbor (pop preds)) scroll-above)
             (cl-decf space-above)
             else if (consp succs) collect
             (setq neighbor (pop succs)) into scroll-below-aux
             while neighbor
             finally (setq scroll-below scroll-below-aux))
    ;; Halfway there...
    (let* ((selected (propertize (car comps) 'icomplete-selected t))
           (chosen (append scroll-above (list selected) scroll-below))
           (tuples (icomplete--augment md chosen))
           max-prefix-len max-comp-len lines nsections)
      (add-face-text-property 0 (length selected)
                              'icomplete-selected-match 'append selected)
      ;; Figure out parameters for horizontal spacing
      (cl-loop
       for (comp prefix) in tuples
       maximizing (length prefix) into max-prefix-len-aux
       maximizing (length comp) into max-comp-len-aux
       finally (setq max-prefix-len max-prefix-len-aux
                     max-comp-len max-comp-len-aux))
      ;; Serialize completions and section titles into a list
      ;; of lines to render
      (cl-loop
       for (comp prefix suffix section) in tuples
       when section
       collect (propertize section 'face 'icomplete-section) into lines-aux
       and count 1 into nsections-aux
       for comp = (icomplete-vertical--add-marker-to-selected comp)
       when (get-text-property 0 'icomplete-selected comp)
       do (add-face-text-property 0 (length comp)
                                  'icomplete-selected-match 'append comp)
       collect (concat prefix
                       (make-string (max 0 (- max-prefix-len (length prefix))) ? )
                       (completion-lazy-hilit comp)
                       (make-string (max 0 (- max-comp-len (length comp))) ? )
                       suffix)
       into lines-aux
       finally (setq lines lines-aux
                     nsections nsections-aux))
      ;; Kick out some lines from the beginning due to extra sections.
      ;; This hopes to keep the selected entry more or less in the
      ;; middle of the dropdown-like widget when `icomplete-scroll' is
      ;; t.  Funky, but at least I didn't use `cl-loop'
      (setq lines
            (nthcdr
             (cond ((<= (length lines) total-space) 0)
                   ((> (length scroll-above) (length scroll-below)) nsections)
                   (t (min (ceiling nsections 2) (length scroll-above))))
             lines))

      ;; HACK TO BE ABLE TO USE M-<tab> TO USE NORMAL MINIBUFFER COMPLETION
      (my-mode)
      (when icomplete--in-region-buffer
        (setq lines (icomplete-vertical--adjust-lines-for-column
                     lines icomplete--in-region-buffer completion-in-region--data)))
      ;; At long last, render final string return value.  This may still
      ;; kick out lines at the end.
      (concat " \n"
              (cl-loop for l in lines repeat total-space concat l concat "\n")))))

(fido-vertical-mode)

(use-package savehist
  :init
  (savehist-mode))

(if (gt= emacs-major-version 30)
    (setq completions-sort 'historical)
  (setq completions-sort nil) ;; no sorting makes completion faster
  )

(define-key minibuffer-local-completion-map "\t" #'icomplete-force-complete)
(define-key minibuffer-local-completion-map (kbd "C-<return>") #'viper-exit-minibuffer)
(define-key global-map (kbd "C-z") #'viper-mode) ;; C-z to suspend frame is annoying with viper

(setq completion-styles '(partial-completion basic) completion-category-overrides nil completion-category-defaults nil)
;; need this hook otherwise i think fido setup or something overrides the completion which is annoying
(defun my-icomplete-styles () (setq-local completion-styles '(partial-completion basic)))
(add-hook 'icomplete-minibuffer-setup-hook 'my-icomplete-styles)

(defvar my-icomplete-prev-command nil)
(defun my-icomplete-save ()
  "save the prvious icomplete session"
  (setq my-icomplete-prev-command this-command)
  (add-hook 'post-command-hook #'my-icomplete-exit-save-input nil 'local))

(defvar my-icomplete-prev-input "")
(defun my-icomplete-exit-save-input ()
  (setq my-icomplete-prev-input (minibuffer-contents-no-properties)))

(add-hook 'icomplete-minibuffer-setup-hook 'my-icomplete-save)

(defun my-icomplete-repeat ()
  (interactive)
  (when (and (not (equal my-icomplete-prev-command #'my-icomplete-repeat))
             (commandp my-icomplete-prev-command))
    (minibuffer-with-setup-hook
        (lambda () (insert my-icomplete-prev-input))
      (call-interactively my-icomplete-prev-command))))

(viper-map! :leader "'" #'my-icomplete-repeat)

;; insert * at the beginning so we don't have to match exactly at the beginning
;; but only in the icomplete minibuffer so we don't clash with viper minibuffer and stuff
;; NOTE: command category can slow down M-x
(defun icomplete-partial-completion-setup ()
  (unless (or 
           (eq (icomplete--category) 'file)
           )
    (insert "*")))

(add-hook 'icomplete-minibuffer-setup-hook #'icomplete-partial-completion-setup)

(defvar icomplete-current-command nil)
;; insert wild card to sorta emulate orderless

(defun icomplete-partial-completion-insert-wildcard ()
  (interactive)
  (if (memq icomplete-current-command
            (list 'viper-ex
                  'eww))
      (insert " ")
    (insert "*")))

(define-key icomplete-minibuffer-map " " #'icomplete-partial-completion-insert-wildcard)
;; this allows us to still insert spaces
(define-key viper-insert-basic-map (kbd "M-SPC SPC") (lambda () (interactive) (insert " ")))

(defun my/minibuffer-setup-hook ()
  (setq icomplete-current-command this-command)
  ;; (setq-local icomplete-show-matches-on-no-input nil)
  )

(add-hook 'minibuffer-setup-hook #'my/minibuffer-setup-hook 100)

(advice-add #'viper-search :after
            (lambda (string &rest args)
              (hi-lock-face-buffer string)))

;; keep highlighting after isearch
(setq lazy-highlight-cleanup nil)

(defun my/ioccur-minibuf-after-edit (beg end len)
  (setq my/ioccur-string (buffer-substring-no-properties (1+ (length my/ioccur-prompt-string)) (point-max)))
  (when (gt (length (string-replace ".*" "" my/ioccur-string)) 2)
    (ignore-errors (occur-1 my/ioccur-string
                            my/ioccur-nlines-arg
                            (list my/occur-buffer)))))

(setq my/ioccur-prompt-string "Find: ")
(setq my/ioccur-string "")

(defun my/ioccur (arg)
  "Run a pseudo interactive grep, which will incrementally update the xref buffer based on minibuffer input.
With a prefix-arg run normally and specfiy a directory"
  (interactive "P")
  (setq my/ioccur-string "")
  (setq my/occur-buffer (current-buffer))
  (setq my/ioccur-nlines-arg (when arg (prefix-numeric-value arg)))
  (minibuffer-with-setup-hook
      (lambda ()
        (local-set-key (kbd "SPC") (lambda () (interactive) (insert ".*")))
        (add-hook 'after-change-functions #'my/ioccur-minibuf-after-edit nil 'local))
    (occur-1 (read-regexp my/ioccur-prompt-string)
             my/ioccur-nlines-arg
             (list my/occur-buffer))))

(viper-map! :mode 'occur-mode
            :n "i" #'occur-edit-mode)

(viper-map! :leader
            "ss" #'my/ioccur
            ;; not sure why but we need to rescan the imenu for our igrep xref buffer
            "si" (lambda () (interactive)
                   (imenu--menubar-select imenu--rescan-item)
                   (call-interactively 'imenu)))

(defun ripgrep ()
  (interactive)
  (call-interactively 'grep))

(defun rripgrep ()
  (interactive)
  (call-interactively 'rgrep))

(defun rripgrep-multiline ()
  (interactive)
  (call-interactively 'rgrep))

(defun rgrep-multiline ()
  (interactive)
  (grep-apply-setting 'grep-command "grep -Pazo --color=auto -nH --null -e ")
  (call-interactively 'rgrep))

(defun grep-options-advice ()
  "A convenient way for us to put different options depending on the grep command being run.
See notes:emacs-notes-and-tips for more details."
  (cond ((or (eq this-command 'ripgrep) (eq this-command 'rripgrep))
         (progn
           (grep-apply-setting 'grep-command "rg -nS --no-heading ") ;; for normal single file grep
           (grep-apply-setting 'grep-find-template "find <D> <X> -type f <F> -exec rg <C> -nS --no-heading -H  <R> /dev/null {} +"))) ;; for rgrep; uses grep-find-template
        ((eq this-command 'rripgrep-multiline)
         (progn
           (grep-apply-setting 'grep-find-template "find <D> <X> -type f <F> -exec rg <C> -nSU --no-heading -H  <R> /dev/null {} +")))
        ((eq this-command 'rgrep-multiline)
         (progn
           (grep-apply-setting 'grep-find-template "find -H <D> <X> -type f <F> -exec grep -zo <C> -nH --null -e <R> \\{\\} +")))
        (t (progn ;; defaults in case I want to change them later to do something different, otherwise don't really need this last case
             (grep-apply-setting 'grep-find-template "find -H <D> <X> -type f <F> -exec grep <C> -nH --null -e <R> \\{\\} +")
             (grep-apply-setting 'grep-command "grep --color=auto -nH --null -e ")))
        )
  )

(advice-add #'grep-compute-defaults :before #'grep-options-advice)

(require 'dabbrev)
;; #'dabbrev-completion resets the global variables first so we do the same
(advice-add #'dabbrev-capf :before #'dabbrev--reset-global-variables)
(add-hook 'completion-at-point-functions #'dabbrev-capf 100)

(use-package xref
  :config
  (setq xref-search-program
        (cond ((executable-find "rg") 'ripgrep)
              ((executable-find "ugrep") 'ugrep)
              (t 'grep)))
  (setq xref-show-xrefs-function #'xref-show-definitions-completing-read)
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read)

 
(defun search-advice (orig-fun regexp)
  (let ((xref-show-xrefs-function #'xref--show-xref-buffer))
    (minibuffer-with-setup-hook
        (lambda ()
          ;; for some reason this doesn't apply in xref find apropos but that's honestly ok
          ;; cause it uses a space separated list of words anyways
          (local-set-key (kbd "M-SPC") (lambda () (interactive) (insert " ")))
          (local-set-key (kbd "SPC") (lambda () (interactive) (insert ".*"))))
      (funcall orig-fun regexp))))
(advice-add 'project-find-regexp :around #'search-advice)
(advice-add 'xref-find-apropos :around #'search-advice)
(advice-add 'previous-history-element :after #'end-of-line) ;; usually we want to go to end of line



  (setq my/xref-vi-state-modify-map
        (make-composed-keymap
         nil
         (make-composed-keymap
          (list my/viper-vi-basic-motion-keymap
                my/viper-vi-motion-g-keymap
                my/viper-vi-motion-leader-keymap)
          xref--xref-buffer-mode-map)))
  (viper-modify-major-mode 'xref--xref-buffer-mode 'vi-state my/xref-vi-state-modify-map))

(setq enable-recursive-minibuffers t)
(setq icomplete-in-buffer t)
(setq minibuffer-visible-completions t)
(when (fboundp 'global-completion-preview-mode) (global-completion-preview-mode))

;; for default completion behavior in the *completions* buffer, should we decide to use it
(define-key completion-in-region-mode-map (kbd "C-n") #'minibuffer-next-completion)
(define-key completion-in-region-mode-map (kbd "C-p") #'minibuffer-previous-completion)
(when (boundp 'minibuffer-visible-completions-map)
  (define-key minibuffer-visible-completions-map (kbd "C-n") #'minibuffer-next-completion)
  (define-key minibuffer-visible-completions-map (kbd "C-p") #'minibuffer-previous-completion))
(setq completions-format 'one-column)
(setq completions-max-height 20)
(setq completions-header-format nil)

(define-error 'no-completions "completing-read-in-region: No completions")
(defun completing-read-in-region (start end collection &optional predicate)
  "Prompt for completion of region in the minibuffer if non-unique.
  Use as a value for `completion-in-region-function'."
  (let* ((initial (buffer-substring-no-properties start end))
         (limit (car (completion-boundaries initial collection predicate "")))
         (all (completion-all-completions initial collection predicate (length initial)))
         ;; when the completion candidate list a single one, for some reason completing-read
         ;; will delete a bunch of lines.
         ;; to couteract this, we basically undo an atomic change and set the completion variable
         (completion (cond
                      ((atom all) nil)
                      ((and (consp all) (atom (cdr all)))
                       (concat (substring initial 0 limit) (car all)))
                      (t
                       (setq completion 
                             (catch 'done
                               (atomic-change-group 
                                 (let ((completion
                                        (completing-read "Completion: " collection predicate nil initial)))
                                   (throw 'done completion)))))))))
    (cond (completion (completion--replace start end completion) t)
          (t (signal 'no-completions nil)))))
;;(setq completion-in-region-function #'completing-read-in-region)
(setq completion-in-region-function #'completion--in-region) ;; this is default


;; similar to doom's +corfu/move-to-minibuffer
(define-key my-mode-map (kbd "M-<tab>")
            (lambda ()
              (interactive)
              (unwind-protect
                  (let ((completion-in-region-function
                         (if completion-in-region--data #'completing-read-in-region #'completion--in-region)))
                    (call-interactively #'complete-symbol))
                )))

(setq tab-always-indent 'complete)

(defun indent-current-line ()
  "Indent the line. Stolen from the middle section of `indent-for-tab-command'."
  (or (not (eq (indent--funcall-widened indent-line-function) 'noindent))
      (indent--default-inside-comment)
      (when (or (lt= (current-column) (current-indentation))
                (not (eq tab-always-indent 'complete)))
        (indent--funcall-widened (default-value 'indent-line-function)))))

;; want backtab to undo our indents basically but not do completion
(defun indent-for-backtab-command ()
  "Want backtab to reindent the line if we've overintended with tab.
Otheriwse, shift backwards by tab-width."
  (interactive)
  (if-let ((cmd (ignore-errors
                  (lookup-key (symbol-value (intern (concat (symbol-name major-mode) "-map")))
                              [backtab]))))
      (call-interactively cmd)
    (let ((text-before-pt (buffer-substring-no-properties (line-beginning-position) (point)))
          ;; get the would be indented column then undo the changes and restore point
          (indented-column (catch 'done (save-excursion
                                          (atomic-change-group
                                            (indent-current-line)
                                            (throw 'done (current-column)))))))

      ;; only deindent if we're equal to or before the would-be indented column
      ;; otherwise, go back to the intended indented position
      (if (lt= (current-column) indented-column)
          (indent-rigidly (if (region-active-p) (region-beginning) (line-beginning-position))
                          (if (region-active-p) (region-end) (line-end-position))
                          (- tab-width))
        (indent-current-line)))))

(defun indent-for-tab-check-empty-before-point (orig-fun &rest args)
  "Around advice for `indent-for-tab-command'.
Insert tabs DWIM style if the current position is already indented and
we're at the beginning of the text on the current line.
ORIG-FUN is `indent-for-tab-command' and ARGS is prefix-arg for that."
  (let ((text-before-pt (buffer-substring-no-properties (line-beginning-position) (point)))
        ;; get the would be indented column then undo the changes and restore point
        (indented-column (catch 'done (save-excursion
                                        (atomic-change-group
                                          (indent-current-line)
                                          (throw 'done (current-column)))))))
    ;; only indent if we're equal to or past the would-be indented column
    (if (and (gt= (current-column) indented-column)
             (string-blank-p text-before-pt))
        (insert-tab current-prefix-arg)
      (apply orig-fun args))))

(advice-add 'indent-for-tab-command :around #'indent-for-tab-check-empty-before-point)
(define-key viper-insert-basic-map [backtab] #'indent-for-backtab-command)

(use-package vc :config
  (setq-default vc-handled-backends '(SVN Git Hg))
  (setq vc-git-diff-switches '("--histogram" "--diff-algorithm=histogram"))
  )

;; fallback to hunk based diff
(setq diff-font-lock-syntax 'hunk-also)

(defun diff-kill-hunk-a (og-fn &rest args)
  (let* ((stage-buf (get-buffer-create "*vc-stage-diff*"))
         (current-vc-revisions diff-vc-revisions)
         (beg-of-file-and-junk (save-excursion (diff-beginning-of-file-and-junk) (point)))
         (hunk-file-b (car (diff-hunk-file-names)))
         (hunk-file-a (cadr (diff-hunk-file-names)))
         (end-of-file-and-junk (save-excursion
                                 (diff-beginning-of-file-and-junk)
                                 (re-search-forward (concat "--- " hunk-file-a))
                                 (re-search-forward (concat "+++ " hunk-file-b))
                                 (point)))
         (file-and-junk (buffer-substring beg-of-file-and-junk (1+ end-of-file-and-junk)))
         (hunk-header-re (rx-to-string
                          `(: bol "@@ -" (group (1+ digit)) "," (1+ digit)
                              " +" (group (1+ digit)) "," (1+ digit) " @@")))
         (hunk-header (save-excursion
                        (re-search-backward hunk-header-re)
                        (buffer-substring-no-properties
                         (line-beginning-position)
                         (line-end-position))))
         (hunk-header-start (progn
                              (string-match hunk-header-re hunk-header)
                              (string-to-number
                               (substring hunk-header (match-beginning 1) (match-end 1)))))
         (hunk-bounds (diff-bounds-of-hunk))
         (file-bounds (ignore-errors (diff-bounds-of-file)))
         ;; If the current hunk is the only one for its file, kill the
         ;; file header too.
         (hunk-and-file-killed (save-excursion
                                 (and file-bounds
                                      (progn (goto-char (car file-bounds))
                                             (= (progn (diff-hunk-next) (point))
                                                (car hunk-bounds)))
                                      (progn (goto-char (cadr hunk-bounds))
                                             ;; bzr puts a newline after the last hunk.
                                             (while (looking-at "^\n")
                                               (forward-char 1))
                                             (= (point) (cadr file-bounds))))))
         )
    ;;(message "killed hunk header start %s %s" hunk-header-start hunk-header)
    ;; kill-ring-deindent-mode can mess with whitespace in killed hunks
    ;; which can cause patches to not apply
    (if (and (boundp 'kill-ring-deindent-mode) kill-ring-deindent-mode)
        (progn
          (kill-ring-deindent-mode -1)
          (apply og-fn args)
          (kill-ring-deindent-mode))
      (apply og-fn args))
    (with-current-buffer stage-buf
      (display-buffer stage-buf)
      (diff-mode)

      (ignore-errors
        (setq-local diff-vc-backend (car (vc-deduce-fileset t)))
        (setq-local diff-vc-revisions current-vc-revisions))
      (diff-mode)

      ;; if the file-headers already exist don't do anything
      (let* ((found-file (condition-case nil
                             (save-excursion
                               (goto-char (point-min))
                               (re-search-forward (concat "--- " hunk-file-a))
                               (re-search-forward (concat "+++ " hunk-file-b))
                               t)
                           (error nil))))
        (message "found-file %s %s" found-file file-and-junk)
        (unless (or found-file hunk-and-file-killed)
          (goto-char (point-max))
          (insert file-and-junk))
        
        (goto-char (point-min))

        (message "wee %s %s %s" (point) hunk-file-a hunk-file-b )
        (condition-case nil
            (progn 
              (re-search-forward (concat "--- " hunk-file-a))
              (re-search-forward (concat "+++ " hunk-file-b))
              (message "wee %s %s %s" (point) hunk-file-a hunk-file-b )
              (forward-line)
              (beginning-of-line))
          (error (message "WTF %s %s" hunk-file-a hunk-file-b ))
          )
        

        ;; we should be in the right file in the stage buffer now
        (ignore-errors
          (let ((start-of-next-file
                 (save-excursion
                   (condition-case nil
                       (progn 
                         (re-search-forward diff-file-header-re)
                         (diff-beginning-of-file-and-junk)
                         (point)
                         )
                     ;; no next file
                     (error (point-max))
                     )
                   )
                 ))
            
            ;; insert the hunk at the correct position
            ;; using hunk headers eg: @@ -0,0 +1,3 @@
            (while
                (progn
                  (message "before diff hunk next")
                  (diff-hunk-next)
                  ;;(message "after diff hunk next")
                  ;; don't go past the expected file
                  (if (> (point) start-of-next-file)
                      (progn
                        (message "after diff hunk next2a")
                        (goto-char start-of-next-file) nil)
                    ;;(message "after diff hunk next2")
                    (let* ((stage-hunk-header (buffer-substring-no-properties
                                               (line-beginning-position)
                                               (line-end-position)))
                           (stage-hunk-header-start
                            (progn
                              ;;(message "after diff hunk next2b")
                              (string-match hunk-header-re stage-hunk-header)
                              (string-to-number
                               (substring stage-hunk-header (match-beginning 1) (match-end 1)))
                              )
                            ))
                      (message "stage hunk header start %s %s %s" stage-hunk-header-start
                               hunk-header-start
                               (> hunk-header-start stage-hunk-header-start))
                      (> hunk-header-start stage-hunk-header-start)
                      )
                    )
                  )
              ))
          )
        ;;(ignore-errors (diff-hunk-next))
        (message "yanking after %s"
                 (buffer-substring-no-properties (line-beginning-position)
                                                 (line-end-position)))
        (when (looking-at hunk-header-re)
          (message "goto prev line %s"
                   (buffer-substring-no-properties (line-beginning-position)
                                                   (line-end-position)))
          (previous-line))
        (end-of-line)
        
        (yank)
        (save-excursion
          (diff-hunk-prev)

          ;; don't want to deal with the headers and they sometimes cause
          ;; "patch fragment without header" errors
          ;; not sure, maybe something to do with our ordering?
          (re-search-forward hunk-header-re)
          (delete-region (point) (line-end-position)) 
          )
        )
      (run-hooks 'vc-diff-finish-functions)

      (diff-setup-whitespace)
      (diff-setup-buffer-type)
      )
    )
  )

;; (vc-call-backend 'git 'diff "/home/jason/.emacs.d/vimilla-emacs.org" "HEAD^" "HEAD" (current-buffer) 1)

(add-to-list 'display-buffer-alist
             '("\\*vc-stage-diff\\*"
               (display-buffer-below-selected)))

(defun vc-diff-internal-ad (&rest args)
  (ignore-errors (kill-buffer "*vc-stage-diff*"))
  (clone-buffer "*vc-stage-diff*")
  (with-current-buffer (get-buffer-create "*vc-stage-diff*")
    (read-only-mode -1)
    (erase-buffer)
    (vc-setup-buffer (current-buffer))
    )
  )

(advice-add #'diff-hunk-kill :around #'diff-kill-hunk-a)
(advice-add #'vc-diff-internal :after #'vc-diff-internal-ad)

(use-package speedbar :defer t
  :config
  (setq speedbar-show-unknown-files t)
  (setq speedbar-frame-parameters (delete '(minibuffer) speedbar-frame-parameters))
  (setq speedbar-update-flag nil)
  (setq my/speedbar-vi-state-modify-map (make-sparse-keymap))
  (define-key my/speedbar-vi-state-modify-map (kbd "<tab>") #'speedbar-toggle-line-expansion)
  (define-key my/speedbar-vi-state-modify-map (kbd "C-i") #'speedbar-toggle-line-expansion)
  (define-key my/speedbar-vi-state-modify-map (kbd "-") #'speedbar-up-directory)
  (viper-modify-major-mode 'speedbar-mode 'vi-state my/speedbar-vi-state-modify-map))

(use-package ibuffer :defer t
  :config
  ;; add project level grouping
  (defun set-ibuffer-project-groups ()
    (setq ibuffer-saved-filter-groups
          (list (let ((l (seq-filter #'identity
                                     (cl-mapcar
                                      (lambda (p)
                                        (let* ((project (project--find-in-directory (car p)))
                                               (pname (project-name project))
                                               (pbufs (cl-find-if (lambda (b) (buffer-file-name b)) (project-buffers project))))
                                          (when pbufs
                                            `( ,pname (filename . ,pname)))))
                                      (seq-filter
                                       (lambda (p) (project-buffers (project--find-in-directory (car p))))
                                       project--list)))))
                  (add-to-list 'l "projects"))))
    (ibuffer-switch-to-saved-filter-groups "projects"))
  (add-hook 'ibuffer-hook #'set-ibuffer-project-groups))

(setq bookmark-use-annotations t)
(setq bookmark-save-flag 1)
(setq bookmark-automatically-show-annotations nil)

                                        ; note the call-interactively does pass the prefix args
(defun my/set-project-bookmark ()
  (interactive)
  (minibuffer-with-setup-hook
      (lambda ()
        (let ((prefix (concat (project-name (project-current)) ": ")))
          (when (project-name (project-current))
            (insert prefix))))
    (call-interactively 'bookmark-set)))

(defun my/jump-to-project-bookmark ()
  (interactive)
  (minibuffer-with-setup-hook
      (lambda ()
        (let ((prefix (concat (project-name (project-current)) ": ")))
          (when (project-name (project-current))
            (insert prefix))))
    (call-interactively 'bookmark-jump)))


(viper-map! :leader 
            "nrf" #'my/jump-to-project-bookmark
            "bmm" #'my/set-project-bookmark
            "bmj" #'my/jump-to-project-bookmark)

(defun split-string-at-first-match (string regex)
  (let ((pos (string-match regex string)))
    (if pos
        (list (substring string 0 pos)
              (substring string (+ pos (length (match-string 0 string)))))
      (list string))))
                             
(defun copy-env-vars-from-shell-1 (cmd)
  (mapc (lambda (env-var-string)
          (let* ((split (split-string-at-first-match env-var-string "="))
                 (name (cl-first split))
                 (val (cl-second split)))
            (when (and name val)
              (setq val (string-replace " " "\\ " val))
              (setenv name val)
              (when (string-equal "PATH" name)
                (setq exec-path (append (parse-colon-path val) (list exec-directory)))
                ;; eshell path
                (setq-default eshell-path-env val)
                (when (fboundp 'eshell-set-path) (eshell-set-path val))))))
        (split-string (shell-command-to-string cmd) "\n")))

(defun copy-env-vars-from-shell ()
  (interactive)
  (copy-env-vars-from-shell-1 "bash --login -i -c printenv"))

(defun get-docker-env-vars ()
  "Gets the environment variables set by ENV in dockerfile by looking at /proc/1/environ.
    Meant for eshell in mind."
  (interactive)
  (mapc (lambda (env-var-string)
          (let* ((split (split-string env-var-string "="))
                 (name (cl-first split))
                 (val (cl-second split)))
            (unless (string-equal "TERM" name)
              (if (string-equal "PATH" name)
                  (progn
                    ;; eshell path
                    (setq eshell-path-env val)
                    (when (fboundp 'eshell-set-path) (eshell-set-path val)))
                (setenv name val)))))
        (split-string (shell-command-to-string "tr \'\\0\' \'\\n\' < /proc/1/environ") "\n")))

(use-package tramp :defer t
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (setq enable-remote-dir-locals t)


  (setq tramp-ssh-controlmaster-options (concat "-o ControlPath=/tmp/ssh-ControlPath-%%r@%%h:%%p " "-o ControlMaster=auto -o ControlPersist=yes"))
  (setq tramp-use-ssh-controlmaster-options t)

  (defun dired-do-delete-advice-remote (orig-fun &rest args)
    ;; this way we use the default value as opposed to the alternative of
    ;; setting delete-by-moving-to-trash to the value of (file-remote-p default-directory)
    (if (file-remote-p default-directory)
        (let ((delete-by-moving-to-trash nil))
          (apply orig-fun args)
          )
      (apply orig-fun args)
      )
    )
  (advice-add 'dired-internal-do-deletions :around #'dired-do-delete-advice-remote)
  )

(with-eval-after-load 'tramp
  (add-to-list 'tramp-methods
               '("sshx11"
                 (tramp-login-program        "ssh")
                 (tramp-login-args           (("-l" "%u") ("-p" "%p") ("%c")
                                              ("-e" "none") ("-X") ("%h")))
                 (tramp-async-args           (("-q")))
                 (tramp-remote-shell         "/bin/sh")
                 (tramp-remote-shell-login   ("-l"))
                 (tramp-remote-shell-args    ("-c"))
                 (tramp-gw-args              (("-o" "GlobalKnownHostsFile=/dev/null")
                                              ("-o" "UserKnownHostsFile=/dev/null")
                                              ("-o" "StrictHostKeyChecking=yes")
                                              ("-o" "ForwardX11=yes")))
                 (tramp-default-port         22)))
  (tramp-set-completion-function "sshx11" tramp-completion-function-alist-ssh))

(add-hook 'prog-mode-hook #'flymake-mode)
(setq treesit-font-lock-level 4)
(setq-default indent-tabs-mode nil)
(which-function-mode)
(setq which-func-display 'header)
(add-hook 'prog-mode-hook
          (lambda ()
            (unless (eq major-mode 'web-mode)
              (electric-pair-local-mode))))
(add-hook 'prog-mode-hook #'hs-minor-mode)

(use-package which-key :defer 2
  :config
  (which-key-mode))

(defun my/flymake-diagnostics-at-point ()
  (interactive)
  (let ((diags (flymake-diagnostics (point))))
    (if (not (seq-empty-p diags))
        (message "%s"
                 (cl-reduce (lambda (acc d) (concat acc (flymake--diag-text d)))
                            (flymake-diagnostics (point))
                            :initial-value ""))
      (message "No diagnostics at point."))))

(use-package flymake :defer t
  :config
  (viper-map! :leader
              "cx" #'my/flymake-diagnostics-at-point
              "cX" #'flymake-show-buffer-diagnostics)
  (viper-map! :n "C-c x" #'my/flymake-diagnostics-at-point)
  )

(add-to-list 'display-buffer-alist '((major-mode . compilation-mode)
                                     (display-buffer-in-side-window)))

(add-to-list 'display-buffer-alist
             '((or (major-mode . flymake-project-diagnostics-mode)
                   (major-mode . flymake-diagnostics-buffer-mode))
               (display-buffer-in-side-window)))

(add-to-list 'display-buffer-alist
             '("\\*eldoc.*\\*"
               (display-buffer-in-side-window)))

(add-to-list 'display-buffer-alist
             '((major-mode . help-mode)
               (display-buffer-in-side-window)
               (window-height . 0.35)))

(add-to-list 'display-buffer-alist
             '((major-mode . messages-buffer-mode)
               (display-buffer-in-side-window)
               (window-height . 0.15)))

(define-derived-mode arduino-mode c-mode "arduino"
  "My own mode which is a wrapper for c-mode for editing arduino files.")
(use-package eglot :defer t
  :config
  (add-to-list 'eglot-server-programs '(arduino-mode . ("~/go/1.22.2/bin/arduino-language-server"
                                                        "-clangd" "/usr/bin/clangd"
                                                        "-cli" "/opt/homebrew/bin/arduino-cli"
                                                        "-cli-config" "/Users/jasonzhen/Library/Arduino15/arduino-cli.yaml"
                                                        "-fqbn" "arduino:avr:uno"))))

(add-hook 'arduino-mode-hook #'eglot-ensure)

(add-to-list 'auto-mode-alist '("\\.ino\\'" . arduino-mode))

(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
(add-hook 'go-ts-mode-hook #'eglot-ensure)

(use-package go-ts-mode :defer t
  :config
  (setq go-ts-mode-indent-offset tab-width))

(defun unset-go-env-vars ()
  "This is needed so that for example, if one project has a go work file but the other doesn't,
  we don't still use the other project's go work file."
  (mapc (lambda (env-var-string)
          (let* ((split (split-string env-var-string "="))
                 (name (cl-first split)))
            (when (and name (not (string-empty-p name)))
              (setenv name ""))))
        (split-string (shell-command-to-string "bash --login -c \"go env\"") "\n")))

(defun copy-go-env-vars-from-shell ()
  (interactive)
  (unset-go-env-vars)
  (copy-env-vars-from-shell)
  (mapc (lambda (env-var-string)
          (let* ((split (split-string env-var-string "="))
                 (name (cl-first split))
                 (val (cl-second split)))
            (when (and name val (not (string-empty-p name)) (not (string-empty-p val)))
              (setenv name (string-trim val "[ '\"]" "[ '\"]")))))
        (split-string (shell-command-to-string "bash --login -c \"go env\"") "\n"))
  (call-interactively 'eglot-reconnect))

(defvar +go-test-last nil
  "The last test run.")

(defun +go--spawn (cmd)
  (save-selected-window
    (compile cmd)))

(defun +go--run-tests (args)
  (let ((cmd (concat "go test -test.v " args)))
    (setq +go-test-last (concat "cd " default-directory ";" cmd))
    (+go--spawn cmd)))

(defun +go/test-single ()
  "Run single test at point."
  (interactive)
  (if (string-match "_test\\.go" buffer-file-name)
      (save-excursion
        (re-search-backward "^func[ ]+\\(([[:alnum:]]*?[ ]?[*]?[[:alnum:]]+)[ ]+\\)?\\(Test[[:alnum:]_]+\\)(.*)")
        (+go--run-tests (concat "-run" "='^\\Q" (match-string-no-properties 2) "\\E$'")))
    (error "Must be in a _test.go file")))

(defun +go/test-file ()
  "Run all tests in current file."
  (interactive)
  (if (string-match "_test\\.go" buffer-file-name)
      (save-excursion
        (goto-char (point-min))
        (let ((func-list))
          (while (re-search-forward "^func[ ]+\\(([[:alnum:]]*?[ ]?[*]?[[:alnum:]]+)[ ]+\\)?\\(Test[[:alnum:]_]+\\)(.*)" nil t)
            (push (match-string-no-properties 2) func-list))
          (+go--run-tests (concat "-run" "='^(" (string-join func-list "|")  ")$'"))))
    (error "Must be in a _test.go file")))

(use-package go-ts-mode :defer t
  :config
  (setq my/go-vi-state-modify-map (make-sparse-keymap))
  (define-key my/go-vi-state-modify-map " mts" #'+go/test-single)
  (define-key my/go-vi-state-modify-map " mtf" #'+go/test-file)
  (viper-modify-major-mode 'go-ts-mode 'vi-state my/go-vi-state-modify-map))

(add-hook 'python-mode-hook (lambda () (setq-local tab-width python-indent-offset)))
(add-hook 'python-ts-mode-hook (lambda () (setq-local tab-width python-indent-offset)))

(defvar before-venv-process-environment nil
  "Process environment with no venv")

(defvar before-venv-exec-path nil
  "Exec path with no venv")

(defvar venv-mode-line-indicator "")


(defun copy-pipenv-vars-from-shell ()
  (interactive)
  (if before-venv-process-environment
      (setq process-environment before-venv-process-environment)
    (setq before-venv-process-environment process-environment))

  (copy-env-vars-from-shell-1 "bash --login -i -c \"pipenv run printenv\""))


(defun copy-python-venv-vars-from-shell ()
  (interactive)
  (if before-venv-exec-path
      (setq exec-path (copy-sequence exec-path))
    (setq before-venv-exec-path (copy-sequence exec-path)))

  (if before-venv-process-environment
      (setq process-environment (copy-sequence before-venv-process-environment))
    (setq before-venv-process-environment (copy-sequence process-environment)))

  (let ((venv-name (read-directory-name "venv folder:" nil nil nil nil)))
    (setq venv-mode-line-indicator (concat "venv:" (project-name (project-current nil venv-name))))
    (add-to-list 'mode-line-misc-info '(t venv-mode-line-indicator)) 
    (copy-env-vars-from-shell-1 (format "bash --login -i -c \". %s && printenv\""
                                        (concat venv-name "bin/activate")))))


(defun deactivate-python-venv ()
  (interactive)
  (setq exec-path before-venv-exec-path)
  (setq process-environment before-venv-process-environment)
  (setq before-venv-process-environment nil
        before-venv-exec-path nil) 

  (setq-default eshell-path-env (mapconcat #'identity before-venv-exec-path ":"))
  (when (fboundp 'eshell-set-path)
    (eshell-set-path (mapconcat #'identity before-venv-exec-path ":")))
  (setq mode-line-misc-info (delete '(t venv-mode-line-indicator) mode-line-misc-info)))

(setq-default tab-width 4)

(use-package js :defer t
  :config
  (setq js-indent-level 4)
  (add-hook 'js-mode-hook #'eglot-ensure))

(use-package typescript-ts-mode :defer t
  :config
  (setq typescript-ts-mode-indent-offset 4)
  (add-hook 'typescript-ts-mode-hook #'eglot-ensure))

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))

(use-package elisp-mode :defer t
  :config
  (require 'advice) ;; for ad-get-orig-definition

  (defun +emacs-lisp-highlight-vars-and-faces (end)
    "Match defined variables and functions.

  Functions are differentiated into special forms, built-in functions and
  library/userland functions"
    (catch 'matcher
      (while (re-search-forward "\\(?:\\sw\\|\\s_\\)+" end t)
        (let ((ppss (save-excursion (syntax-ppss))))
          (cond ((nth 3 ppss)  ; strings
                 (search-forward "\"" end t))
                ((nth 4 ppss)  ; comments
                 (forward-line +1))
                ((let ((symbol (intern-soft (match-string-no-properties 0))))
                   (and (cond ((null symbol) nil)
                              ((eq symbol t) nil)
                              ((keywordp symbol) nil)
                              ((special-variable-p symbol)
                               (setq +emacs-lisp--face 'font-lock-variable-name-face))
                              ((and (fboundp symbol)
                                    (eq (char-before (match-beginning 0)) ?\()
                                    (not (memq (char-before (1- (match-beginning 0)))
                                               (list ?\' ?\`))))
                               (let ((unaliased (indirect-function symbol)))
                                 (unless (or (macrop unaliased)
                                             (special-form-p unaliased))
                                   (let (unadvised)
                                     (while (not (eq (setq unadvised (ad-get-orig-definition unaliased))
                                                     (setq unaliased (indirect-function unadvised)))))
                                     unaliased)
                                   (setq +emacs-lisp--face
                                         (if (subrp unaliased)
                                             'font-lock-constant-face
                                           'font-lock-function-name-face))))))
                        (throw 'matcher t)))))))
      nil))

  (font-lock-add-keywords 'emacs-lisp-mode `((+emacs-lisp-highlight-vars-and-faces . +emacs-lisp--face)) 'append))

(defun my/eshell-in-bottom-side-window (arg)
  (interactive "P")
  (let ((eshell-buffer (save-window-excursion (eshell))))
    (select-window (display-buffer-in-side-window eshell-buffer '()))))

(defun my/eshell-send-cmd-async ()
  (interactive)
  (let ((cmd (string-trim (buffer-substring-no-properties eshell-last-output-end (progn (end-of-line) (point))))))
    (unless (eshell-head-process)
      (delete-region eshell-last-output-end (point))
      (insert (format "async-shell-command \"%s\"" cmd)))))

(use-package eshell :defer t
  :config
  (add-to-list 'eshell-modules-list 'eshell-tramp)
  (setq my/eshell-vi-state-modify-map (make-sparse-keymap))
  (setq my/eshell-insert-state-modify-map (make-sparse-keymap))

  (define-key my/eshell-vi-state-modify-map (kbd "C-<return>") #'my/eshell-send-cmd-async)
  (define-key my/eshell-vi-state-modify-map " ma" #'my/eshell-send-cmd-async)
  (define-key my/eshell-insert-state-modify-map (kbd "C-<return>") #'my/eshell-send-cmd-async)
  (define-key my/eshell-insert-state-modify-map (kbd "M-<return>") #'my/eshell-send-cmd-async)

  (defun slurp (f)
    (with-temp-buffer
      (insert-file-contents f)
      (buffer-substring-no-properties (point-min) (point-max))))

  (define-key my/eshell-insert-state-modify-map (kbd "C-r")
              (lambda ()
                (interactive)
                (let ((selected (completing-read "History: "
                                                 (cl-remove-if-not
                                                  (lambda (elem)
                                                    (text-properties-at 0 elem))
                                                  (ring-elements eshell-history-ring)))))
                  (when selected 
                    (end-of-line)
                    (eshell-kill-input)
                    (insert selected)))))
  (viper-modify-major-mode 'eshell-mode 'vi-state my/eshell-vi-state-modify-map)
  (viper-modify-major-mode 'eshell-mode 'insert-state my/eshell-insert-state-modify-map))

(use-package eshell :after consult :config
  (define-key my/eshell-insert-state-modify-map (kbd "C-r") #'consult-history))

(viper-map! :leader "oe" #'my/eshell-in-bottom-side-window "oE" #'eshell)

(defun my/shell-in-bottom-side-window (arg)
  (interactive "P")
  (let ((shell-buffer (save-window-excursion (shell))))
    (select-window (display-buffer-in-side-window shell-buffer '()))))

(viper-map! :leader "os" #'my/shell-in-bottom-side-window "oS" #'shell)

(use-package shell :defer t
  :config
  
  (setq my/shell-insert-state-modify-map (make-sparse-keymap))

  (define-key my/shell-insert-state-modify-map (kbd "<up>") #'comint-previous-input)
  (define-key my/shell-insert-state-modify-map (kbd "<down>") #'comint-next-input)
  (define-key my/shell-insert-state-modify-map (kbd "C-r")
              (lambda ()
                (interactive)
                (let ((selected (completing-read "History: "
                                                 (cl-remove-if-not
                                                  (lambda (elem)
                                                    (text-properties-at 0 elem))
                                                  (ring-elements comint-input-ring)))))
                  (when selected
                    (end-of-line)
                    (comint-kill-input)
                    (insert selected)))))
  (viper-modify-major-mode 'shell-mode 'insert-state my/shell-insert-state-modify-map))

(use-package shell :after consult :config
  (define-key my/shell-insert-state-modify-map (kbd "C-r") #'consult-history))

(when (display-graphic-p) ;; only matter for gui emacs
  (setq current-font-height 160)
  (defun set-fonts ()
    (message "setting fonts")
    (cond ((member "FantasqueSansM Nerd Font Propo" (font-family-list))
           (ignore-errors (set-face-attribute 'default nil :font "FantasqueSansM Nerd Font Propo" :height current-font-height)))
          ((member "IosevkaCustom Nerd Font Propo" (font-family-list))
           (ignore-errors (set-face-attribute 'default nil :font "FantasqueSansM Nerd Font Propo" :height current-font-height)))
          (t (message "None of my preferred mono fonts found, will use defaults")))

    (cond ((member "Comic Neue" (font-family-list))
           (set-face-attribute 'variable-pitch nil :font "Comic Neue" :height current-font-height))
          ((member "Iosevka Etoile" (font-family-list))
           (set-face-attribute 'variable-pitch nil :font "Iosevka Etoile" :height current-font-height))
          (t (message "None of my preferred variable pitch fonts found, will use defaults")))

    (ignore-errors (set-fontset-font t 'emoji "Noto Color Emoji" nil 'append))
    (ignore-errors (set-fontset-font t 'emoji "Apple Color Emoji" nil 'append))
    (ignore-errors (set-fontset-font t 'unicode "Iosevkacustom Nerd Font Propo" nil 'append)))

  (add-hook 'after-make-frame-functions #'set-fonts)
  ;; for some reason this is kinda busted in emacs daemon
  (add-hook 'emacs-startup-hook (lambda () (remove-hook 'after-make-frame-functions #'set-fonts)))
  (set-fonts)

  (defun my/set-font-size ()
    (interactive)
    (let ((new-size (string-to-number
                     (minibuffer-with-setup-hook
                         (lambda () (insert (number-to-string current-font-height)))
                       (read-string "Edit font size: ")))))
      (setq current-font-height new-size)
      (set-face-attribute 'default nil :height new-size)
      (set-face-attribute 'variable-pitch nil :height new-size))))

(setq modus-themes-headings
      '((1 . (rainbow overline background variable-pitch 1.25))
        (2 . (rainbow background variable-pitch 1.15))
        (3 . (rainbow bold variable-pitch 1.1))
        (t . (semilight variable-pitch 1.05))))


(setq modus-themes-bold-constructs t)
(setq modus-themes-italic-constructs t)
(setq modus-themes-org-blocks 'gray-background)
(condition-case nil
    (progn 
      (load-theme 'modus-vivendi-tinted t)
      (set-cursor-color "#f78fe7"))
  (error (progn
           (load-theme 'modus-vivendi t)
           (set-cursor-color "white")))) 

(use-package diff :defer t
  :config
  ;; for some reason modus gets rid of diff-header
  (set-face-attribute 'diff-header nil :background "gray80"))

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

(defun get-file-buffers-in-window ()
  (seq-filter #'buffer-file-name
              (delete-dups (mapcar #'window-buffer
                                   (window-list-1 (frame-first-window)
                                                  'nomini)))))

(defun tab-bar-tab-name-projects ()
  (let ((file-buffers (get-file-buffers-in-window)))
    (if file-buffers
        (mapconcat #'identity
                   (delete-dups
                    (cl-mapcar (lambda (b)
                                 (with-current-buffer b
                                   (if (project-current)
                                       (project-name (project-current))
                                     (buffer-name))))
                               file-buffers))
                   ", ")
      (tab-bar-tab-name-current))))

(setq tab-bar-tab-name-function #'tab-bar-tab-name-projects)
;; (truncate-string-to-width (tab-bar-tab-name-all) (/ (frame-width) (length (tab-bar-tabs))))

(defun get-tab-names (&rest _)
  (interactive "P")
  (message "%s"
           (mapconcat
            (lambda (tab)
              (let* ((current-tab-p (eq (car tab) 'current-tab))
                     (idx (number-to-string (1+ (tab-bar--tab-index tab))))
                     (tab-name1 (concat "[" idx "] " (cdr (cl-second tab))))
                     (tab-name (if current-tab-p (propertize tab-name1 'face '(:inherit font-lock-builtin-face :underline t)) tab-name1)))
                tab-name))
            (tab-bar-tabs)
            "  ")))

(advice-add 'tab-bar-new-tab :after #'get-tab-names)
(advice-add 'tab-bar-close-tab :after #'get-tab-names)
(advice-add 'tab-bar-select-tab :after #'get-tab-names)

(viper-map! :leader "<tab><tab>" #'get-tab-names
            "<tab>{" (lambda (arg)
                       (interactive "P")
                       (tab-bar-move-tab (if arg (- arg) -1))
                       (get-tab-names))
            "<tab>}" (lambda (arg)
                       (interactive "P")
                       (tab-bar-move-tab (if arg arg 1))
                       (get-tab-names))
            )

(setq tab-bar-show nil)
(tab-bar-mode)

(defun rename-tab-to-current-project (dir)
  (message (project-name (project-current)))
  (tab-bar-rename-tab (project-name (project-current))))

(advice-add 'project-switch-project :after #'rename-tab-to-current-project)

(setq browse-url-browser-function 'eww-browse-url)
(add-hook 'eww-after-render-hook 'eww-readable)

(add-to-list 'display-buffer-alist
             '("\\*eww\\*"
               (display-buffer-in-side-window)
               (window-height . 0.5)))

(defun save-narrowing-info (pos1 pos2)
  (setq narrowed-pos1 (line-number-at-pos pos1))
  (setq narrowed-pos2 (line-number-at-pos pos2)))

(defun modeline-setup ()

  (defface mode-line-pink
    (if (facep 'modus-themes-fg-magenta-cooler)
        '((t :inherit modus-themes-fg-magenta-cooler))
      '((t :foreground "magenta")))
    "face used for modeline"
    :group 'basic-faces)

  (defface mode-line-cyan
    (if (facep 'modus-themes-fg-cyan-cooler)
        '((t :inherit modus-themes-fg-cyan-cooler))
      '((t :foreground "cyan1")))
    "face used for modeline"
    :group 'basic-faces)

  (advice-add 'narrow-to-region :before #'save-narrowing-info)

  (setq project-mode-line t)
  (setq-default mode-line-buffer-identification
                `(:eval
                  (let ((s (format-mode-line
                            (propertized-buffer-identification (buffer-name)))))
                    (when (and (boundp 'uniquify-managed) uniquify-managed)
                      (unless (string= (buffer-name) (uniquify-buffer-base-name))
                        (let ((base-len (length (uniquify-buffer-base-name)))
                              (full-len (length (buffer-name)))
                              (pre (eq uniquify-buffer-name-style 'post-forward-angle-brackets)))
                          (let ((start (if pre 0 base-len))
                                (end (if pre (- full-len base-len) full-len)))
                            (set-text-properties base-len full-len '(face (:inherit mode-line-cyan :weight bold)) s)))))
                    s)))

  (defvar viper-mode-string "") ;; will be loaded later unless we go away from viper mode

  (set-face-attribute 'mode-line-buffer-id nil :inherit 'mode-line-pink :weight 'bold)
  (setq project-mode-line-face 'package-name)
  (setq project-file-history-behavior 'relativize)

  (setq-default mode-line-format '("%e" mode-line-front-space
                                   (:eval (propertize viper-mode-string)) ;; not sure why we need this, but otherwise the props don't show up
                                   ;; kbd macro info
                                   (:eval (when defining-kbd-macro
                                            (concat mode-line-defining-kbd-macro
                                                    (propertize (format "@%s" (char-to-string evil-this-macro)) 'face 'success))))
                                   mode-line-window-dedicated
                                   mode-line-modified mode-line-remote
                                   (project-mode-line project-mode-line-format) " "
                                   mode-line-buffer-identification " "
                                   mode-line-position "<" (:eval (format "%d" (line-number-at-pos (point-max))))
                                   (:eval (when (buffer-narrowed-p) (format " >%d:%d<" narrowed-pos1 narrowed-pos2))) " "
                                   ;; selection position info
                                   (:eval (when (region-active-p)
                                            (propertize (concat
                                                         (number-to-string (1+ (abs (- (line-number-at-pos (point)) (line-number-at-pos (mark)))))) "L"
                                                         (number-to-string (1+ (abs (- (current-column) (save-excursion (goto-char (mark)) (current-column)))))) "C")
                                                        'face 'warning)))
                                   " " (:propertize mode-name face (:weight bold :inherit mode-line-pink)) " "
                                   vc-mode
                                   mode-line-misc-info mode-line-end-spaces))

  (column-number-mode)
  (line-number-mode)
  (size-indication-mode)
  )

(add-hook 'after-make-frame-functions #'modeline-setup)
(add-hook 'emacs-startup-hook #'modeline-setup)


(defun shorten-vc-mode-line (file &optional backend)
  (when (stringp vc-mode)
    (let ((nobackend (truncate-string-to-width
                      (replace-regexp-in-string
                       (format "^ %s:" (vc-backend buffer-file-name))
                       " " vc-mode)
                      15 0 nil nil)))
      (setq vc-mode nobackend))))

(advice-add #'vc-mode-line :after #'shorten-vc-mode-line)

(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-ts-mode))

(setq org-directory "~/orgmode/")
(setq org-attach-id-dir (concat (file-name-as-directory org-directory) (file-name-as-directory ".attach")))
(setq org-todo-keywords '((sequence "TODO(t)" "WIP(w)" "|" "DONE" "CANCELLED")))
(setq org-attach-use-interitance t)

(setq org-startup-indented t)
(setq org-indent-indentation-per-level 4)
(setq org-startup-folded nil) ;; to respect VISIBILITY property just can't be 'showeverything, see: org-cycle-set-startup-visibility

(setq org-agenda-files (list "~/orgmode/notes/20240118T135401--work-tracking__agenda.org"))
;; steal doom's todo keywords
(setq org-todo-keywords
      '((sequence
         "TODO(t)"  ; A task that needs doing & is ready to do
         "PROJ(p)"  ; A project, which usually contains other tasks
         "LOOP(r)"  ; A recurring task
         "STRT(s)"  ; A task that is in progress
         "WAIT(w)"  ; Something external is holding up this task
         "HOLD(h)"  ; This task is paused/on hold because of me
         "IDEA(i)"  ; An unconfirmed and unapproved task or notion
         "|"
         "DONE(d)"  ; Task successfully completed
         "KILL(k)") ; Task was cancelled, aborted, or is no longer applicable
        (sequence
         "[ ](T)"   ; A task that needs doing
         "[-](S)"   ; Task is in progress
         "[?](W)"   ; Task is being held up or paused
         "|"
         "[X](D)")  ; Task was completed
        (sequence
         "|"
         "OKAY(o)"
         "YES(y)"
         "NO(n)"))
      org-todo-keyword-faces
      '(("[-]"  . +org-todo-active)
        ("STRT" . +org-todo-active)
        ("[?]"  . +org-todo-onhold)
        ("WAIT" . +org-todo-onhold)
        ("HOLD" . +org-todo-onhold)
        ("PROJ" . +org-todo-project)
        ("NO"   . +org-todo-cancel)
        ("KILL" . +org-todo-cancel)))
(setq org-agenda-window-setup 'current-window)
(setq org-agenda-restore-windows-after-quit t)

;; allow dabbrev expand on tab when in insert mode
(defun line-before-point-empty-p ()
  (string-blank-p (buffer-substring-no-properties (point-at-bol) (point))))

(use-package org :defer t
  :config
  (progn
    (setq org-image-actual-width '(300))
    (setq org-goto-interface 'outline-path-completionp)
    (setq org-outline-path-complete-in-steps nil)
    (setq org-return-follows-link t)
    (advice-add 'org-agenda-get-restriction-and-command :around
                (lambda (orig-fun &rest args)
                  (cl-letf (((symbol-function 'delete-other-windows) (lambda () nil)))
                    (apply orig-fun args))))
    (add-to-list 'display-buffer-alist
                 '("\\*Agenda Commands\\*"
                   (display-buffer-in-side-window)))
    (add-to-list 'display-buffer-alist
                 '("\\*Calendar\\*"
                   (display-buffer-in-side-window)))
    (add-to-list 'display-buffer-alist
                 '("\\*Org Src.*\\*"
                   (display-buffer-in-side-window)
                   (window-height . 0.4)))

    ;; fix newline and indent in src blocks, took this from doom
    (defun org-fix-newline-and-indent-in-src-blocks-ad (&rest _args)
      (when (and org-src-tab-acts-natively
                 (org-in-src-block-p t))
        (save-window-excursion
          (save-excursion
            (org-babel-do-in-edit-buffer
             (set-mark (point-min))
             (goto-char (point-max))
             (let ((inhibit-message t))
               (call-interactively #'indent-region))))))
      )
    (advice-add #'org-return :after #'org-fix-newline-and-indent-in-src-blocks-ad)
    
    (viper-map! :mode 'org-mode
                :n "zi" #'org-toggle-inline-images
                "SPC si" #'org-goto "SPC oaa" #'org-agenda
                "SPC msl" #'org-demote-subtree "SPC msh" #'org-promote-subtree
                "SPC msk" #'org-move-subtree-up "SPC msj" #'org-move-subtree-down
                "SPC maa" #'org-attach "SPC mA" #'org-archive-subtree
                "SPC mds" #'org-schedule "SPC mdd" #'org-deadline
                "SPC msr" #'org-refile "SPC mll" #'org-insert-link
                "SPC nl" #'org-store-link
                "<tab>" (lambda ()
                          (interactive)
                          ;; want org cycle if region active for indenting, or heading for collapsing
                          (if (or (org-at-heading-p) (region-active-p) (org-at-property-block-p) (org-at-property-drawer-p) (org-at-block-p) (org-at-drawer-p))
                              (call-interactively (lookup-key org-mode-map "\t"))
                            (call-interactively (lookup-key viper-vi-basic-map [C-i]))))
                )
    ))

(require 'dash (concat user-emacs-directory "vendor/dash.el"))
(require 's (concat user-emacs-directory "vendor/s.el"))
(require 'dumb-jump (concat user-emacs-directory "vendor/dumb-jump.el"))
(require 'coterm (concat user-emacs-directory "vendor/coterm.el"))
(require 'wgrep (concat user-emacs-directory "vendor/wgrep.el"))
(require 'vundo (concat user-emacs-directory "vendor/vundo.el"))
(require 'avy (concat user-emacs-directory "vendor/avy.el"))

(setq dumb-jump-force-searcher 'rg)

(coterm-mode)

(use-package avy
  :config
  (define-key viper-vi-basic-map "gss" #'avy-goto-char-2)
  (define-key viper-vi-basic-map "gs/" #'avy-goto-char-timer)
  (define-key viper-vi-basic-map "gs " #'avy-goto-char-timer))

(use-package vundo
  :config
  (setq vundo-vi-modify-map vundo-mode-map)
  (define-key vundo-vi-modify-map "h" #'vundo-backward)
  (define-key vundo-vi-modify-map "l" #'vundo-forward)
  (define-key vundo-vi-modify-map "k" #'vundo-previous)
  (define-key vundo-vi-modify-map "j" #'vundo-next)
  (define-key vundo-vi-modify-map "d" #'vundo-diff)
  (viper-modify-major-mode 'vundo-mode 'vi-state vundo-vi-modify-map))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(window-stool))
 '(package-vc-selected-packages
   '((window-stool :vc-backend Git :url "https://github.com/JasZhe/window-stool")))
 '(safe-local-variable-values
   '((eval add-hook 'after-save-hook
           (lambda nil
             (org-babel-tangle))
           nil t))))
