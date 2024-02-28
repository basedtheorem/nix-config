;;; dev.el --- Developer configuration

;;; Contents:
;;;
;;;  - Languages
;;;  - Common file types
;;;  - Eglot (built-in LSP client)
;;;  - Misc. DX enhancements

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Languages
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Nix
(use-package nix-mode
  :ensure t
  ;; :mode "\\.nix\\'"
  :init
  (setq-default nix-nixfmt-bin "nixfmt")
  ;; (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))
  (defun nix-ts-format-before-save ()
    "Add this to `before-save-hook' to run nixfmt when saving."
    (when (derived-mode-p 'nix-ts-mode)
      (nix-format-buffer)))
  )

(use-package nix-ts-mode
  :ensure t
  :hook
  (before-save . nix-ts-format-before-save)
  :after (nix-mode)
  :mode "\\.nix\\'"
  :init
  (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-ts-mode))
  (defun nix-ts-override-indent ()
    (local-set-key (kbd "TAB") 'nix-indent-line))
  )

;; Fish shell
(use-package fish-mode :ensure t)

;; Elisp
(use-package emacs-lisp-mode
  :ensure nil
  :init
  (defmacro debug/with-advice (adlist &rest body)
    "Execute BODY with temporary advice in ADLIST.

Each element of ADLIST should be a list of the form
  (SYMBOL WHERE FUNCTION [PROPS])
suitable for passing to `advice-add'.  The BODY is wrapped in an
`unwind-protect' form, so the advice will be removed even in the
event of an error or nonlocal exit."
    (declare (debug ((&rest (&rest form)) body))
             (indent 1))
    `(progn
       ,@(mapcar (lambda (adform)
                   (cons 'advice-add adform))
                 adlist)
       (unwind-protect (progn ,@body)
         ,@(mapcar (lambda (adform)
                     `(advice-remove ,(car adform) ,(nth 2 adform)))
                   adlist))))

  (defun debug/call-logging-hooks (command &optional verbose)
    "Call COMMAND, reporting every hook run in the process.
Interactively, prompt for a command to execute.

Return a list of the hooks run, in the order they were run.
Interactively, or with optional argument VERBOSE, also print a
message listing the hooks."
    (interactive "CCommand to log hooks: \np")
    (let* ((log     nil)
           (logger (lambda (&rest hooks)
                     (setq log (append log hooks nil)))))
      (my/with-advice
          ((#'run-hooks :before logger))
        (call-interactively command))
      (when verbose
        (message
         (if log "Hooks run during execution of %s:"
           "No hooks run during execution of %s.")
         command)
        (dolist (hook log)
          (message "> %s" hook)))
      log))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Common file types
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init
  (setq markdown-command "multimarkdown")
  :config
  :hook ((markdown-mode . visual-line-mode)))

(setq markdown-mode-map (make-sparse-keymap))
(setq gfm-mode-map (make-sparse-keymap))

(use-package yaml-mode :ensure t)

(use-package json-mode :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Eglot (built-in LSP client)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package eglot
  :ensure nil
  :config
  (defun check-envrc ()
    (when (and (
                string= 'on envrc--status)
               (string= major-mode 'nix-ts-mode))
      (eglot-ensure)))

  ;; Don't log every event (massive perf boost)
  (fset #'jsonrpc--log-event #'ignore)

  ;; Sometimes necessary to tell Eglot where to find the LSP
  (add-to-list 'eglot-server-programs '(nix-ts-mode . ("nil")))
  :hook
  (envrc-mode-on . check-envrc)
  ((python-mode ruby-mode elixir-mode) . eglot)
  :custom
  (eglot-send-changes-idle-time 0.1)
  (eglot-extend-to-xref t) ; Activate Eglot in referenced non-project files
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Misc. DX enhancements
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emacs
  :config
  ;; Treesitter modes
  (setq major-mode-remap-alist
        '((yaml-mode . yaml-ts-mode)
          (bash-mode . bash-ts-mode)
          (js2-mode . js-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (json-mode . json-ts-mode)
          (css-mode . css-ts-mode)
          (c++-mode . c++-ts-mode)
          (c-mode . c-ts-mode)
          (python-mode . python-ts-mode)
          (nix-mode . nix-ts-mode)
          ))
  :init
  (setopt display-line-numbers-width 3)
  (setq-default electric-indent-inhibit t)    ; Disable auto-indenting current line
  :hook
  ;; (prog-mode . display-line-numbers-mode)  ; Line numbers in prog-mode
  (prog-mode . electric-pair-mode)            ; Auto parenthesis matching
  )

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (daemonp)
    (exec-path-from-shell-initialize)))

;; Highlight current block scope. Disabled by default because
;; it's distracting, but its useful in heavily nested structures.
(use-package hl-block-mode
  :ensure t
  :commands (hl-block-mode)
  :init
  (setq hl-block-bracket nil)          ; Match all brackets.
  (setq hl-block-single-level t)       ; Only one pair of brackets.
  (setq hl-block-multi-line t)
  (setq hl-block-style 'color-tint)    ; Highlight only the brackets.
  (setq hl-block-color-tint "#020202"))

;; Direnv integration
    (use-package envrc
      :ensure t
      :config
      (envrc-global-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Indentation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lrns/setup-indent (n)
  ;; java/c/c++
  (setq-local c-basic-offset n)
  ;; web development
  (setq-local coffee-tab-width n) ; coffeescript
  (setq-local javascript-indent-level n) ; javascript-mode
  (setq-local js-indent-level n) ; js-mode
  (setq-local js2-basic-offset n) ; js2-mode, in latest js2-mode, it's alias of js-indent-level
  (setq-local web-mode-markup-indent-offset n) ; web-mode, html tag in html file
  (setq-local web-mode-css-indent-offset n) ; web-mode, css in html file
  (setq-local web-mode-code-indent-offset n) ; web-mode, js code in html file
  (setq-local css-indent-offset n) ; css-mode
  )

(defun lrns/personal-code-style ()
  (interactive)
  ;; use space instead of tab
  (setq indent-tabs-mode nil)
  ;; indent 2 spaces width
  (lrns/setup-indent 2))

(defun lrns/setup-develop-environment ()
  (interactive)
  (lrns/personal-code-style))

;; prog-mode-hook requires emacs24+
(add-hook 'prog-mode-hook 'lrns/setup-develop-environment)
;; a few major-modes does NOT inherited from prog-mode
(add-hook 'lua-mode-hook 'lrns/setup-develop-environment)
(add-hook 'web-mode-hook 'lrns/setup-develop-environment)
(add-hook 'text-mode-hook 'lrns/setup-develop-environment)
(add-hook 'markdown-mode-hook 'lrns/setup-develop-environment)

;;; dev.el ends here
