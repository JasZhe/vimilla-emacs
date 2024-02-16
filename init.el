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

(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)
(define-key global-map (kbd "s-/") #'comment-line)

;; terminal stuff, C-/ in case we don't have iterm config
(define-key global-map (kbd "C-/") #'comment-line)
(define-key global-map (kbd "C-_") #'comment-line)
(define-key input-decode-map "\e[1;P9" (kbd "s-/"))

(defun macos-term-select-text-to-clipboard (text)
  (shell-command (concat "echo \"" text "\" | pbcopy" )))

;; ITERM2 MOUSE SUPPORT
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (defun track-mouse (e)) 
  (setq mouse-sel-mode t)
  (setq interprogram-cut-function #'macos-term-select-text-to-clipboard)) ;; good enough

(winner-mode)

(defun my/set-transparency-in-terminal ()
  (interactive)
  (unless (display-graphic-p (selected-frame))
    (setq prev-default-face-bg (face-background 'default))
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

(setq gc-cons-threshold most-positive-fixnum)

;; Lower threshold back to 8 MiB (default is 800kB)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (expt 2 23))))

(setq inhibit-startup-screen t)  
(tool-bar-mode 0)
(setq viper-mode t)
(require 'viper)
(require 'rect)

(scroll-bar-mode -1)
(viper-mode)
(global-hl-line-mode)
(global-auto-revert-mode)
(setq auto-revert-verbose nil)
(global-visual-line-mode)
(add-hook 'prog-mode-hook (lambda () (modify-syntax-entry ?_ "-") (modify-syntax-entry ?_ "_")))

(setq column-number-mode t)
(setq scroll-margin 8)
(setq visual-bell t)
(setq ring-bell-function 'ignore)
(setq scroll-preserve-screen-position t)
(setq eval-expression-print-level nil)
(setq eval-expression-print-length nil)

(fido-vertical-mode)

(keymap-set minibuffer-local-completion-map "TAB" #'icomplete-force-complete)
(define-key minibuffer-local-completion-map (kbd "C-<return>") #'viper-exit-minibuffer)
(keymap-set global-map "C-z" #'viper-mode) ;; C-z to suspend frame is annoying with viper

(setq completion-styles '(partial-completion basic) completion-category-overrides nil completion-category-defaults nil)
(defun my-icomplete-styles () (setq-local completion-styles '(partial-completion basic)))
(add-hook 'icomplete-minibuffer-setup-hook 'my-icomplete-styles)

;; insert * at the beginning so we don't have to match exactly at the beginning
;; but only in the icomplete minibuffer so we don't clash with viper minibuffer and stuff
(defun icomplete-partial-completion-setup ()
  (unless (or (eq (icomplete--category) 'file))
    (insert "*")))
(add-hook 'icomplete-minibuffer-setup-hook #'icomplete-partial-completion-setup)

;; insert wild card to sorta emulate orderless
(defun icomplete-partial-completion-insert-wildcard ()
  (interactive)
  (unless (eq last-command 'viper-ex)
    (insert "*")))

(define-key icomplete-minibuffer-map " " #'icomplete-partial-completion-insert-wildcard)
;; this allows us to still insert spaces
(define-key icomplete-minibuffer-map (kbd "M-SPC") (lambda () (interactive) (insert " ")))

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

(defun my/igrep-minibuf-after-edit (beg end len)
  (setq my/igrep-string (buffer-substring-no-properties (1+ (length my/igrep-prompt-string)) (point-max)))
  (when (gt (length (string-replace ".*" "" my/igrep-string)) 2)
    (cl-letf (((symbol-function 'pop-to-buffer) (lambda (buf &optional _ _) (display-buffer buf))))
      (ignore-errors (project-find-regexp my/igrep-string)))))

(setq my/igrep-prompt-string "Find in proj: ")
(setq my/igrep-string "")

(defun my/igrep (arg)
  "Run a pseudo interactive grep, which will incrementally update the xref buffer based on minibuffer input.
With a prefix-arg run normally and specfiy a directory"
  (interactive "P")
  (setq my/igrep-string "")
  (if arg
      (let ((current-prefix-arg '(4)))
        (call-interactively #'project-find-regexp))
    (let ((xref-show-xrefs-function #'xref--show-xref-buffer))
      (minibuffer-with-setup-hook
          (lambda ()
            (local-set-key (kbd "SPC") (lambda () (interactive) (insert ".*")))
            (add-hook 'after-change-functions #'my/igrep-minibuf-after-edit nil 'local))
        (project-find-regexp (read-regexp my/igrep-prompt-string))))))

(defun ripgrep ()
  (interactive)
  (call-interactively 'grep))

(defun rripgrep ()
  (interactive)
  (call-interactively 'rgrep))

(advice-add
 #'grep-compute-defaults
 :before (lambda ()
           (if (or (eq this-command 'ripgrep) (eq this-command 'rripgrep))
               (progn
                 (grep-apply-setting 'grep-command "rg -nS --no-heading ")
                 (grep-apply-setting 'grep-find-template "find <D> <X> -type f <F> -exec rg <C> --no-heading -H  <R> /dev/null {} +"))
             (progn
               (grep-apply-setting 'grep-find-template "find -H <D> <X> -type f <F> -exec grep <C> -nH --null -e <R> \\{\\} +")
               (grep-apply-setting 'grep-command "grep --color=auto -nH --null -e")))))

(require 'dabbrev)
;; #'dabbrev-completion resets the global variables first so we do the same
(advice-add #'dabbrev-capf :before #'dabbrev--reset-global-variables)
(add-hook 'completion-at-point-functions #'dabbrev-capf 100)

(setq xref-search-program 'ripgrep)
(setq xref-show-xrefs-function #'xref-show-definitions-completing-read)
(setq xref-show-definitions-function #'xref-show-definitions-completing-read)

(use-package xref :defer t
  :config
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
        (t (message "No completion") nil))))
(setq completion-in-region-function #'completing-read-in-region)

(advice-add 'indent-for-tab-command
            :after (lambda (&optional arg)
                     (when (memq (get-char-code-property (char-before) 'general-category)
                                   '(Po Ll Lu Lo Lt Lm Mn Mc Me Nl))
                       (complete-symbol arg))))

(defun copy-env-vars-from-shell ()
  (interactive)
  (mapc (lambda (env-var-string)
          (let* ((split (split-string env-var-string "="))
                 (name (cl-first split))
                 (val (cl-second split)))
            (setenv name val)
            (when (string-equal "PATH" name)
              (setq exec-path (append (parse-colon-path val) (list exec-directory)))
              ;; eshell path
              (setq-default eshell-path-env val)
              (when (fboundp 'eshell-set-path) (eshell-set-path val)))))
        (split-string (shell-command-to-string "bash --login -c printenv"))))

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
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(add-hook 'prog-mode-hook #'flymake-mode)
(setq treesit-font-lock-level 4)
(setq-default indent-tabs-mode nil)
(which-function-mode)
(add-hook 'prog-mode-hook
          (lambda ()
            (unless (eq major-mode 'web-mode)
              (electric-pair-local-mode))))

(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
(add-hook 'go-ts-mode-hook #'eglot-ensure)

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

(use-package js :defer t
  :config
  (setq js-indent-level tab-width)
  (add-hook 'js-mode #'eglot-ensure))
(use-package typescript-ts-mode :defer t
  :config
  (setq typescript-ts-mode-indent-offset tab-width))

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))

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

(defun my/eshell-send-cmd-async ()
  (interactive)
  (let ((cmd (string-trim (buffer-substring-no-properties eshell-last-output-end (progn (end-of-line) (point))))))
    (unless (eshell-head-process)
      (delete-region eshell-last-output-end (point))
      (insert (format "async-shell-command \"%s\"" cmd)))
    )
  )

(use-package eshell :defer t
  :config
  (add-to-list 'eshell-modules-list 'eshell-tramp)
  (setq my/eshell-vi-state-modify-map (make-sparse-keymap))
  (setq my/eshell-insert-state-modify-map (make-sparse-keymap))

  (define-key my/eshell-vi-state-modify-map (kbd "C-<return>") #'my/eshell-send-cmd-async)
  (define-key my/eshell-vi-state-modify-map " ma" #'my/eshell-send-cmd-async)
  (define-key my/eshell-insert-state-modify-map (kbd "C-<return>") #'my/eshell-send-cmd-async)

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
  (viper-modify-major-mode 'eshell-mode 'insert-state my/eshell-insert-state-modify-map)
  )

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

(use-package eglot :defer t
  :config
  (require 'eglot)
  (require 'jsonrpc)
  (eval-when-compile (require 'cl-lib))

  (defun eglot-booster-plain-command (com)
    "Test if command COM is a plain eglot server command."
    (and (consp com)
         (not (integerp (cadr com)))
         (not (seq-intersection '(:initializationOptions :autoport) com))))

  (defun eglot-booster ()
    "Boost plain eglot server programs with emacs-lsp-booster.
  The emacs-lsp-booster program must be compiled and available on
  variable `exec-path'.  Only local stdin/out based lsp servers can
  be boosted."
    (interactive)
    (unless (executable-find "emacs-lsp-booster")
      (user-error "The emacs-lsp-booster program is not installed"))
    (if (get 'eglot-server-programs 'lsp-booster-p)
        (message "eglot-server-programs already boosted.")
      (let ((cnt 0)
            (orig-read (symbol-function 'jsonrpc--json-read)))
        (dolist (entry eglot-server-programs)
          (cond
           ((functionp (cdr entry))
            (cl-incf cnt)
            (let ((fun (cdr entry)))
              (setcdr entry (lambda (&rest r) ; wrap function
                              (let ((res (apply fun r)))
                                (if (eglot-booster-plain-command res)
                                    (cons "emacs-lsp-booster" res)
                                  res))))))
           ((eglot-booster-plain-command (cdr entry))
            (cl-incf cnt)
            (setcdr entry (cons "emacs-lsp-booster" (cdr entry))))))
        (defalias 'jsonrpc--json-read
          (lambda ()
            (or (and (= (following-char) ?#)
                     (let ((bytecode (read (current-buffer))))
                       (when (byte-code-function-p bytecode)
                         (funcall bytecode))))
                (funcall orig-read))))
        (message "Boosted %d eglot-server-programs" cnt))
      (put 'eglot-server-programs 'lsp-booster-p t)))
  ;; need to run it on eglot load
  (eglot-booster))

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
(use-package vc :defer t
  :config
  ;; for some reason modus gets rid of diff-header
  (set-face-attribute 'diff-header nil :background "gray80"))

(midnight-mode)

(defun load-light-theme ()
  (load-theme 'modus-operandi t))

(defun load-dark-theme ()
  (load-theme 'modus-vivendi t))

(defun load-dark-theme1 ()
  (load-dark-theme))

(defun auto-light-dark-midnight-setup ()
  (run-at-time "0:00" t #'load-dark-theme)
  (run-at-time "10:00" t #'load-light-theme)
  (run-at-time "16:00" t #'load-dark-theme1))

(add-hook 'midnight-hook #'auto-light-dark-midnight-setup)

(auto-light-dark-midnight-setup)

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

(setq browse-url-browser-function 'eww-browse-url)
(add-hook 'eww-after-render-hook 'eww-readable)

(setq org-directory "~/orgmode/")
(setq org-attach-id-dir (concat (file-name-as-directory org-directory) (file-name-as-directory ".attach")))
(setq org-todo-keywords '((sequence "TODO(t)" "WIP(w)" "|" "DONE" "CANCELLED")))
(setq org-attach-use-interitance t)

(setq org-startup-indented t)
(setq org-indent-indentation-per-level 4)
(setq org-startup-folded nil) ;; to respect VISIBILITY property just can't be 'showeverything, see: org-cycle-set-startup-visibility

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

    (define-key my/org-vi-state-modify-map " nl" #'org-store-link)

    (define-key org-mode-map "\t"
                (lambda (arg)
                  (interactive "P")
                  (if (and (not (line-before-point-empty-p)) (string= viper-current-state "insert-state"))
                      (dabbrev-expand arg)
                    (org-cycle arg))))

    ;; for terminal issues with C-i
    (define-key my/org-vi-state-modify-map [C-i]
                (lambda ()
                  (interactive)
                  ;; want org cycle if region active for indenting, or heading for collapsing
                  (if (or (org-at-heading-p) (region-active-p))
                      (call-interactively (lookup-key org-mode-map "\t"))
                    (call-interactively (lookup-key viper-vi-basic-map [C-i])))))

    (viper-modify-major-mode 'org-mode 'vi-state my/org-vi-state-modify-map)))

(setq native-comp-async-report-warnings-errors 'silent)

(use-package avy :ensure nil :pin gnu :defer 2
  :config
  (define-key viper-vi-basic-map "gss" #'avy-goto-char-2)
  (define-key viper-vi-basic-map "gs/" #'avy-goto-char-timer))

(use-package which-key :ensure t :pin gnu :defer 2
  :config
  (which-key-mode))

(when (not (require 'hurl-mode nil 'noerrror))
  (package-vc-install "https://github.com/JasZhe/hurl-mode"))
(use-package hurl-mode :mode "\\.hurl\\'")

(when (not (require 'window-stool nil 'noerrror))
  (package-vc-install "https://github.com/JasZhe/window-stool"))
(use-package window-stool :defer 2
  :config
  (setq window-stool-use-overlays nil)
  (add-hook 'prog-mode-hook #'window-stool-mode)
  (add-hook 'org-mode-hook #'window-stool-mode))

(use-package pdf-tools :ensure nil :pin gnu
  :mode "\\.pdf\\'"
  :config
    (setq my/pdf-vi-state-modify-map (make-sparse-keymap))
    (define-key my/pdf-vi-state-modify-map "o" #'pdf-outline)
    (define-key my/pdf-vi-state-modify-map "H" #'pdf-view-fit-height-to-window)
    (define-key my/pdf-vi-state-modify-map "W" #'pdf-view-fit-width-to-window)
    (define-key my/pdf-vi-state-modify-map "j" #'pdf-view-next-line-or-next-page)
    (define-key my/pdf-vi-state-modify-map "k" #'pdf-view-previous-line-or-previous-page)
   (setq pdf-view-resize-factor 1.10)
    (define-key my/pdf-vi-state-modify-map "+" #'pdf-view-enlarge)
    (define-key my/pdf-vi-state-modify-map "-" #'pdf-view-shrink)

    (viper-modify-major-mode 'pdf-view-mode 'vi-state my/pdf-vi-state-modify-map)
  )

(rassq-delete-all 'git-rebase-mode auto-mode-alist)

(when (not (require 'web-mode nil 'noerrror))
  (package-vc-install '(web-mode :url "https://github.com/fxbois/web-mode"
                                 :rev "82847071ce93293bdb7945db08d970f13fd883cf")))
(use-package web-mode :ensure nil :pin gnu
  :mode "\\.gohtml\\'"
  :config
  (setq web-mode-engines-alist '(("go" . "\\.gohtml\\'") ("svelte" . "\\.svelte\\'"))))

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
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
