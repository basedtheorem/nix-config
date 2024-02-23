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
  :init
  (setq-default nix-nixfmt-bin "nixfmt")
  (defun nix-ts-format-before-save ()
    "Add this to `before-save-hook' to run nixfmt when saving."
    (when (derived-mode-p 'nix-ts-mode)
      (nix-format-buffer))))

(use-package nix-ts-mode
  :ensure t
  :after (nix-mode)
  :mode "\\.nix\\'"
  :init
  :hook
  (before-save . nix-ts-format-before-save)
  (before-save . nix-format-before-save)
  (nix-ts-mode . eglot-ensure))

;; Fish shell
(use-package fish-mode :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Common file types
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :hook ((markdown-mode . visual-line-mode)))

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
  ;; Don't log every event (massive perf boost)
  (fset #'jsonrpc--log-event #'ignore)

  ;; Sometimes necessary to tell Eglot where to find the LSP
  (add-to-list 'eglot-server-programs
               '(haskell-mode . ("haskell-language-server-wrapper" "--lsp")))
  (add-to-list 'eglot-server-programs '(nix-ts-mode . ("nil")))
  :hook
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
          (python-mode . python-ts-mode)))
  :init
  (setopt display-line-numbers-width 3)
  (setq-default electric-indent-inhibit t)    ; Disable auto-indenting current line
  :hook
  ;; (prog-mode . display-line-numbers-mode)  ; Line numbers in prog-mode
  (prog-mode . electric-pair-mode)            ; Auto parenthesis matching
  )

;; Direnv integration
(use-package envrc
  :ensure t
  :config
  (envrc-global-mode))

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

;;; dev.el ends here
