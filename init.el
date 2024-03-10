;;; -*- lexical-binding: t; -*-

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
          (lambda () (setq gc-cons-threshold (* 1024 1024 16)))) ;; 16MB

(run-with-idle-timer 2 t #'garbage-collect)

(setq inhibit-startup-screen t)
(menu-bar-mode 0)
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

(fido-vertical-mode)

(keymap-set minibuffer-local-completion-map "TAB" #'icomplete-force-complete)
(define-key minibuffer-local-completion-map (kbd "C-<return>") #'viper-exit-minibuffer)
(keymap-set global-map "C-z" #'viper-mode) ;; C-z to suspend frame is annoying with viper

(setq completion-styles '(partial-completion basic) completion-category-overrides nil completion-category-defaults nil)
;; need this hook otherwise i think fido setup or something overrides the completion which is annoying
(defun my-icomplete-styles () (setq-local completion-styles '(partial-completion basic)))
(add-hook 'icomplete-minibuffer-setup-hook 'my-icomplete-styles)

(defvar my-icomplete-prev-command nil)
(defun my-icomplete-save ()
  "save the previous icomplete session"
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

(define-key my/leader-prefix-map "'" #'my-icomplete-repeat)

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

(setq xref-search-program
      (cond ((executable-find "rg") 'ripgrep)
            ((executable-find "ugrep") 'ugrep)
            (t 'grep)))
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

(setq tab-always-indent 'complete)

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

(defun copy-env-vars-from-shell-1 (cmd)
  (mapc (lambda (env-var-string)
          (let* ((split (split-string env-var-string "="))
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
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(add-hook 'prog-mode-hook #'flymake-mode)
(setq treesit-font-lock-level 4)
(setq-default indent-tabs-mode nil)
(which-function-mode)
(add-hook 'prog-mode-hook
          (lambda ()
            (unless (eq major-mode 'web-mode)
              (electric-pair-local-mode))))

(add-to-list 'display-buffer-alist
             '("\\*eldoc.*\\*"
               (display-buffer-in-side-window)))

(add-to-list 'display-buffer-alist
             '("\\*help\\*"
               (display-buffer-in-side-window)
               (window-height . 0.35)))

(add-to-list 'display-buffer-alist
             '("\\*Messages\\*"
               (display-buffer-in-side-window)
               (window-height . 0.15)))

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

(defun copy-pipenv-vars-from-shell ()
  (interactive)
  (copy-env-vars-from-shell-1 "bash --login -i -c \"pipenv run printenv\""))

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

(setq current-font-height 130)
(when (member "IosevkaCustom Nerd Font Propo" (font-family-list))
  (set-face-attribute 'default nil :font "IosevkaCustom Nerd Font Propo" :height current-font-height))
(when (member "Iosevka Etoile" (font-family-list))
  (set-face-attribute 'variable-pitch nil :font "Iosevka Etoile" :height current-font-height))

(defun my/set-font-size ()
  (interactive)
  (let ((new-size (string-to-numberr
                   (minibuffer-with-setup-hook
                       (lambda () (insert (number-to-string current-font-height)))
                     (read-string "Edit font size: ")))))
    (setq current-font-height new-size)
    (set-face-attribute 'default nil :height new-size)
    (set-face-attribute 'variable-pitch nil :height new-size)))

(cond ((member "Apple Color Emoji" (font-family-list))
       (set-fontset-font t '(#x27F0 . #x1FAFF) "Apple Color Emoji" nil 'append))
      ((member "Noto Color Emoji" (font-family-list))
       (set-fontset-font t '(#x27F0 . #x1FAFF) "Noto Color Emoji" nil 'append)))

(use-package modus-themes :ensure t :pin gnu)

(setq modus-themes-headings
      '((1 . (rainbow overline background variable-pitch 1.25))
        (2 . (rainbow background variable-pitch 1.15))
        (3 . (rainbow bold variable-pitch 1.1))
        (t . (semilight variable-pitch 1.05))))


(setq modus-themes-bold-constructs t)
(setq modus-themes-italic-constructs t)
(setq modus-themes-org-blocks 'gray-background)
;; (load-theme 'modus-operandi)
;; (use-package vc :defer t
;;   :config
;;   ;; for some reason modus gets rid of diff-header
;;   (set-face-attribute 'diff-header nil :background "gray80"))

(midnight-mode)

(defun load-light-theme ()
  (condition-case nil
      (progn 
        (load-theme 'modus-operandi-tinted t)
        (set-cursor-color "#a60000"))
    (error (progn
             (load-theme 'modus-operandi t)
             (set-cursor-color "black")))))

(defun load-dark-theme ()
  (condition-case nil
      (progn
        (load-theme 'modus-vivendi-tinted t)
        (set-cursor-color "#f78fe7"))
    (error
     (progn
       (load-theme 'modus-vivendi t)
       (set-cursor-color "white")))))

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
  (message "%s |"
           (mapconcat
            (lambda (tab)
              (let* ((current-tab-p (eq (car tab) 'current-tab))
                     (tab-name1 (cdr (cl-second tab)))
                     (tab-name (if current-tab-p (propertize tab-name1 'face '(:inherit isearch)) tab-name1)))
                tab-name))
            (tab-bar-tabs)
            " | ")))

(advice-add 'tab-bar-new-tab :after #'get-tab-names)
(advice-add 'tab-bar-close-tab :after #'get-tab-names)
(advice-add 'tab-bar-select-tab :after #'get-tab-names)

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

(use-package newst-backend :defer t
  :config
  (setq newsticker-url-list
        '(("CBC Toronto" "https://www.cbc.ca/webfeed/rss/rss-canada-toronto" nil nil nil)
          ("CBC Canada" "https://www.cbc.ca/webfeed/rss/rss-canada" nil nil nil)
          ("CBC Politics" "https://www.cbc.ca/webfeed/rss/rss-politics" nil nil nil)
          ("CBC Business" "https://www.cbc.ca/webfeed/rss/rss-business" nil nil nil)
          ("CBC Technology" "https://www.cbc.ca/webfeed/rss/rss-technology" nil nil nil)
          ("Toronto Star" "https://www.thestar.com/search/?f=rss&t=article&c=news/gta*&l=50&s=start_time&sd=desc" nil nil nil)
          ("Reuters North America" "https://www.reutersagency.com/feed/?best-regions=north-america&post_type=best" nil nil nil)
          ("Reuters Politics" "https://www.reutersagency.com/feed/?best-topics=political-general&post_type=best" nil nil nil)
          ("Reuters Tech" "https://www.reutersagency.com/feed/?best-topics=tech&post_type=best" nil nil nil)
          ("Reuters Business" "https://www.reutersagency.com/feed/?best-topics=business-finance&post_type=best" nil nil nil)
          ("Hacker News" "https://news.ycombinator.com/rss")
          ("Reddit - Emacs" "https://old.reddit.com/r/emacs/.rss")
          ("Sacha Chau Emacs" "https://sachachua.com/blog/feed/" nil nil nil)
          ("Karthinks" "https://karthinks.com/index.xml" nil nil nil))))

(use-package newst-treeview :defer t
  :config
  (setq newsticker-groups
        '("Feeds"
          ("CBC" "CBC Toronto" "CBC Canada" "CBC Politics" "CBC Business" "CBC Technology")
          "Toronto Star"
          ("Reuters" "Reuters North America" "Reuters Politics" "Reuters Tech" "Reuters Business")
          ("Emacs" "Sacha Chau Emacs" "Karthinks")))
  (newsticker--treeview-tree-update))

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
                  (if (or (org-at-heading-p) (region-active-p) (org-at-property-block-p) (org-at-property-drawer-p))
                      (call-interactively (lookup-key org-mode-map "\t"))
                    (call-interactively (lookup-key viper-vi-basic-map [C-i])))))

    (viper-modify-major-mode 'org-mode 'vi-state my/org-vi-state-modify-map)))

(unless (boundp 'package-archives)
  (package-initialize))

(setq native-comp-async-report-warnings-errors 'silent)

(use-package vertico :ensure t :pin gnu
  :config
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  (setq vertico-cycle t)
  (setq vertico-count 12)
  (setq vertico-resize nil)
  (fido-vertical-mode -1)
  (icomplete-mode -1)
  (define-key vertico-map (kbd "RET") #'vertico-exit)
  (define-key my/leader-prefix-map "'" #'vertico-repeat)
  (vertico-mode))

(use-package orderless :ensure t :pin gnu :after icomplete
  :config
  ;; stolen from doom
  (defun +vertico-orderless-dispatch (pattern _index _total)
    (cond
     ;; Ensure $ works with Consult commands, which add disambiguation suffixess
     ((string-suffix-p "$" pattern)
      `(orderless-regexp . ,(concat (substring pattern 0 -1) "[\x200000-\x300000]*$")))
     ;; Ignore single !
     ((string= "!" pattern) `(orderless-literal . ""))
     ;; Without literal
     ((string-prefix-p "!" pattern) `(orderless-without-literal . ,(substring pattern 1)))
     ;; Character folding
     ((string-prefix-p "%" pattern) `(char-fold-to-regexp . ,(substring pattern 1)))
     ((string-suffix-p "%" pattern) `(char-fold-to-regexp . ,(substring pattern 0 -1)))
     ;; Initialism matching
     ((string-prefix-p "`" pattern) `(orderless-initialism . ,(substring pattern 1)))
     ((string-suffix-p "`" pattern) `(orderless-initialism . ,(substring pattern 0 -1)))
     ;; Literal matching
     ((string-prefix-p "=" pattern) `(orderless-literal . ,(substring pattern 1)))
     ((string-suffix-p "=" pattern) `(orderless-literal . ,(substring pattern 0 -1)))
     ;; Flex matching
     ((string-prefix-p "~" pattern) `(orderless-flex . ,(substring pattern 1)))
     ((string-suffix-p "~" pattern) `(orderless-flex . ,(substring pattern 0 -1)))))

  (setq completion-styles '(orderless basic) completion-category-overrides nil completion-category-defaults nil)
  (setq orderless-style-dispatchers '(+vertico-orderless-dispatch))
  (setq orderless-component-separator "[ &]")
  (define-key icomplete-minibuffer-map " " #'self-insert-command)
  (remove-hook 'icomplete-minibuffer-setup-hook #'icomplete-partial-completion-setup)
  (remove-hook 'icomplete-minibuffer-setup-hook 'my-icomplete-styles)
  (defun my-icomplete-styles () (setq-local completion-styles '(orderless basic)))
  (remove-hook 'icomplete-minibuffer-setup-hook #'icomplete-partial-completion-setup)
  (add-hook 'icomplete-minibuffer-setup-hook 'my-icomplete-styles))

(use-package marginalia :ensure t :pin gnu
  :config (marginalia-mode))

(use-package consult :ensure t :pin gnu
  :config
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref
        completion-in-region-function #'consult-completion-in-region)
  (define-key my/leader-prefix-map "," #'consult-project-buffer)
  (define-key my/leader-prefix-map "G"
              (lambda () (interactive)
                (call-interactively (if (executable-find "rg") #'consult-ripgrep #'consult-grep))))
  (define-key my/leader-prefix-map "cx" #'consult-flymake)
  (define-key my/leader-prefix-map "ss" #'consult-line)
  (define-key my/leader-prefix-map "si" #'consult-imenu))

(use-package company :ensure t :pin gnu
  :config
  ;; window-stool overlays + company are broken
  (add-hook 'company-mode-hook (lambda () (when window-stool-use-overlays (setq window-stool-use-overlays nil) (revert-buffer))))

  (setq company-require-match nil
        company-dabbrev-other-buffers nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil))

(use-package cape :ensure t :pin gnu)

(use-package corfu :ensure t
  :init (global-corfu-mode)
  :config
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-noninterruptible)

  (setq corfu-auto t)
  (setq corfu-cycle t)
  (setq corfu-quit-no-match t)
  (setq corfu-quit-at-boundary t)
  (defun corfu-move-to-minibuffer ()
    (interactive)
    (pcase completion-in-region--data
      (`(,beg ,end ,table ,pred ,extras)
       (let ((completion-extra-properties extras)
             completion-cycle-threshold completion-cycling)
         (consult-completion-in-region beg end table pred)))))
  (define-key corfu-map (kbd "M-m") #'corfu-move-to-minibuffer)
  (define-key corfu-map (kbd "C-M-i") #'corfu-move-to-minibuffer)
  (define-key corfu-map (kbd "M-<tab>") #'corfu-move-to-minibuffer)
  (add-to-list 'corfu-continue-commands #'corfu-move-to-minibuffer))

(use-package corfu-terminal :ensure t
  :config
  (unless (display-graphic-p) (corfu-terminal-mode)))

(use-package avy :ensure t :pin gnu :defer 2
  :config
  (define-key viper-vi-basic-map "gss" #'avy-goto-char-2)
  (define-key viper-vi-basic-map "gs/" #'avy-goto-char-timer))

(use-package vundo :ensure t :pin gnu
  :config
  (setq vundo-vi-modify-map vundo-mode-map)
  (define-key vundo-vi-modify-map "h" #'vundo-backward)
  (define-key vundo-vi-modify-map "l" #'vundo-forward)
  (define-key vundo-vi-modify-map "k" #'vundo-previous)
  (define-key vundo-vi-modify-map "j" #'vundo-next)
  (define-key vundo-vi-modify-map "d" #'vundo-diff)
  (viper-modify-major-mode 'vundo-mode 'vi-state vundo-vi-modify-map))

(use-package which-key :ensure t :pin gnu :defer 2
  :config
  (which-key-mode))

(when (not (require 'hurl-mode nil 'noerrror))
  (package-vc-install "https://github.com/JasZhe/hurl-mode"))
(use-package hurl-mode :mode "\\.hurl\\'"
  :config
  (add-to-list 'display-buffer-alist
               '("\\*hurl-response*\\*"
                 (display-buffer-in-side-window)
                 (window-height . 0.4))))

(when (not (require 'window-stool nil 'noerrror))
  (package-vc-install "https://github.com/JasZhe/window-stool"))
(use-package window-stool :defer 2
  :config
  (setq window-stool-use-overlays t)
  (add-hook 'org-mode-hook #'window-stool-mode)
  (add-hook 'prog-mode-hook #'window-stool-mode))

(use-package magit :ensure t :pin nongnu :defer 3
  :config
  (add-to-list 'auto-mode-alist '("/git-rebase-todo\\'" . git-rebase-mode))

  (define-key my/leader-prefix-map "gg" #'magit)
  (setq my/magit-vi-state-modify-map
        (make-composed-keymap
         nil
         (make-composed-keymap 
          (list my/viper-vi-basic-motion-keymap
                my/viper-vi-motion-g-keymap
                my/viper-vi-motion-leader-keymap)
          magit-mode-map)))
  (define-key my/magit-vi-state-modify-map (kbd "C-w") my-window-map)
  (define-key my/magit-vi-state-modify-map "x" #'magit-discard)
  (define-key my/magit-vi-state-modify-map "`" #'magit-process-buffer)
  (define-key my/magit-vi-state-modify-map "E" #'magit-ediff)
  (define-key my/magit-vi-state-modify-map (kbd "C-l") #'magit-log)
  (define-key my/magit-vi-state-modify-map (kbd "C-b") #'magit-branch)
  (define-key my/magit-vi-state-modify-map "p" #'magit-push)
  (define-key my/magit-vi-state-modify-map "F" #'magit-pull)
  (define-key my/magit-vi-state-modify-map " gF" #'magit-fetch)
  ;; for terminal issues with C-i
  (define-key my/magit-vi-state-modify-map [C-i]
              (lambda ()
                (interactive) (call-interactively (lookup-key magit-mode-map "\t"))))

  (viper-modify-major-mode 'magit-status-mode 'vi-state my/magit-vi-state-modify-map)
  (define-key magit-diff-mode-map (kbd "C-w") my-window-map)

  (add-to-list 'display-buffer-alist
               '("magit:.*"
                 (display-buffer-same-window))))

(rassq-delete-all 'git-rebase-mode auto-mode-alist)

(use-package diff-hl :ensure t :pin gnu :after viper
  :config
  (global-diff-hl-mode)
  (unless (display-graphic-p) (diff-hl-margin-mode))

  (add-to-list 'display-buffer-alist
               '("\\*diff-hl\\*" (display-buffer-in-side-window)))

  (define-key my/leader-prefix-map "gr" #'diff-hl-revert-hunk)
  (define-key my/leader-prefix-map "gs" #'diff-hl-show-hunk)
  (add-to-list 'brac-char-cmd-alist '(?d . (lambda () (interactive) (diff-hl-previous-hunk))))
  (add-to-list 'brac-char-cmd-alist '(?D . (lambda () (interactive) (diff-hl-show-hunk-previous))))

  (add-to-list 'ket-char-cmd-alist '(?d . (lambda () (interactive) (diff-hl-next-hunk))))
  (add-to-list 'ket-char-cmd-alist '(?D . (lambda () (interactive) (diff-hl-show-hunk-next))))
  (use-package magit :config
    (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)))

(use-package denote :ensure t :pin gnu :defer 2
  :config
  (setq denote-directory "~/orgmode/notes")
  (define-key my/leader-prefix-map "do" #'denote-open-or-create)
  (define-key my/leader-prefix-map "dd" #'denote-open-or-create)
  (define-key my/leader-prefix-map "dt" #'denote-type)
  (define-key my/leader-prefix-map "dn" #'denote))

(when (not (require 'web-mode nil 'noerrror))
  (package-vc-install '(web-mode :url "https://github.com/fxbois/web-mode"
                                 :rev "82847071ce93293bdb7945db08d970f13fd883cf")))
(use-package web-mode :ensure nil :pin gnu
  :mode "\\.gohtml\\'"
  :config
  (setq web-mode-engines-alist '(("go" . "\\.gohtml\\'") ("svelte" . "\\.svelte\\'"))))

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
