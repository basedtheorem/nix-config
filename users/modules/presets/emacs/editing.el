;;; editing.el --- UX enhancements when editing text

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Highlighting
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Highlight the current line
(let ((hl-line-hooks '(text-mode-hook prog-mode-hook)))
  (mapc (lambda (hook) (add-hook hook 'hl-line-mode)) hl-line-hooks))

(global-subword-mode +1)                  ; Make forward-word also work with camelCase


;; Make `([{}])`, etc. easier to distinguish
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; Highlight closest pair of parentheses
(use-package highlight-parentheses
  :ensure t
  :init
  (setq highlight-parentheses-colors '("White"))
  (setq highlight-parentheses-background-colors '("#004545"))
  (setq show-paren-mode nil)
  :config
  (global-highlight-parentheses-mode t)
  )

;; Colorise indentation bars & highlight current indent
(use-package indent-bars
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-no-descend-string t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  (indent-bars-treesit-wrap '((python argument_list parameters ; for python, as an example
              list list_comprehension
              dictionary dictionary_comprehension
              parenthesized_expression subscript)))
  :config
  ;; Disabled in terminal due to bug
  (if (window-system)
    (add-hook 'prog-mode-hook 'indent-bars-mode))
  (setq-default
   indent-bars-color '(highlight :face-bg t :blend 0.15)
   indent-bars-pattern "."
   indent-bars-width-frac 0.1
   indent-bars-pad-frac 0.1
   indent-bars-zigzag nil
   ;; indent-bars-starting-column 0
   indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 1)
   indent-bars-highlight-current-depth '(:blend 0.5)
   indent-bars-display-on-blank-lines t)
  )

;; Highlight occurences of word under cursor when idle
(use-package idle-highlight-mode
  :ensure t
  :config
  (setq idle-highlight-idle-time 0.2)
  (setq idle-highlight-exclude-point t)
  (idle-highlight-global-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Misc. editing enhancements
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(add-hook 'text-mode-hook 'visual-line-mode)  ; Wrap lines when working with text
(add-hook 'text-mode-hook 'subword-mode)      ; Make forward-word also work with camelCase

;; Clean up whitespace on save
(use-package ws-butler
  :ensure t
  :hook (before-save . ws-butler-global-mode))

;; Modify search results en masse
(use-package wgrep
  :ensure t
  :config
  (setq wgrep-auto-save-buffer t))

;; A fast, powerful text search that works across files
(use-package deadgrep
  :ensure t
  :bind
  ("<f7>" . #'deadgrep))

;; Writable deadgrep buffer that applies the changes to files
(use-package wgrep-deadgrep
  :ensure t)

;;; editing.el ends here
