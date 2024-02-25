;;; base.el --- Base enhancements

;;; Contents:
;;;
;;;  - Motion aids
;;;  - Discovery aids
;;;  - Embark & Consult
;;;  - Minibuffer and completion
;;;  - Theme
;;;  - Modeline

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Motion aids
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package avy
  :ensure t
  :demand t
  :bind (("C-y j" . avy-goto-line)
         ("s-j"   . avy-goto-char-timer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Discovery aids
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Shows popup of keybindings when typing long key sequences
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; Better help defaults
(use-package helpful
  :ensure t
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h x") #'helpful-command)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Embark & Consult
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package consult
  :ensure t
  :bind (
         ;; Drop-in replacements
         ("C-x b" . consult-buffer)     ; orig. switch-to-buffer
         ("M-y"   . consult-yank-pop)   ; orig. yank-pop
         ;; Searching
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)       ; Alternative: rebind C-s to use
         ("M-s s" . consult-line)       ; consult-line instead of isearch, bind
         ("M-s L" . consult-line-multi) ; isearch to M-s s
         ("M-s o" . consult-outline)
         ;; Isearch integration
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)   ; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history) ; orig. isearch-edit-string
         ("M-s l" . consult-line)            ; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)      ; needed by consult-line to detect isearch
         )
  :config
  ;; Narrowing lets you restrict results to certain groups of candidates
  (setq consult-narrow-key "<"))

(use-package embark
  :ensure t
  :demand t
  :after avy
  :bind (("C-y a" . embark-act))
  :init
  ;; Run embark when using avy
  (defun bedrock/avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  ;; After invoking avy-goto-char-timer, hit "." to run embark at the next
  ;; candidate selected
  (setf (alist-get ?. avy-dispatch-alist) 'bedrock/avy-action-embark))

(use-package embark-consult
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Minibuffer and completion
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Better vertical completion for minibuffer commands
(use-package vertico
  :ensure t
  :init
  ;; You'll want to make sure that e.g. fido-mode isn't enabled
  (vertico-mode))

(use-package vertico-directory
  :after vertico
  :bind (:map vertico-map
              ("M-DEL" . vertico-directory-delete-word)))

;; Annotations for minibuffer
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

;; Popup completion-at-point
(use-package corfu
  :ensure t
  :init (global-corfu-mode)
  :custom
  (corfu-auto nil)
  (corfu-cycle t)
  (corfu-separator ?\s)
  (corfu-quit-no-match t)
  :bind
  (:map corfu-map
        ("SPC" . corfu-insert-separator)
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous)
        ("<esc>" . corfu-quit)))

(use-package corfu-popupinfo
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.25 . 0.1))
  (corfu-popupinfo-hide nil)
  :config
  (corfu-popupinfo-mode))

;; Make corfu popup come up in terminal overlay
(use-package corfu-terminal
  :if (not (display-graphic-p))
  :ensure t
  :config
  (corfu-terminal-mode))

;; Icons for corfu
(use-package kind-icon
  :if (display-graphic-p)
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; Fancy completion-at-point functions
;; #TODO
(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package eshell
  :init
  (defun setup-eshell ()
    ;; Work-around to bind "C-r" in the keymap
    (keymap-set eshell-mode-map "C-r" 'consult-history))
  :hook (eshell-mode . setup-eshell))

;; Completion style
(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless)))

;; Spell-check
(use-package flyspell
  :ensure nil
  :init
  (setq flyspell-issue-message-flag nil)
  :hook (text-mode . flyspell-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Theme
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Good default theme
;; (use-package emacs
;;   ;; Light: modus-operandi, dark: modus-vivendi
;;   :config
;;   (load-theme 'modus-vivendi))

;; Black & red theme
(use-package ef-themes
  :ensure t
  :init
  (set-face-attribute 'default nil :family "Sarasa Mono HC" :height 130);
  (set-face-attribute 'bold nil :weight 'Semibold)
  (setq ef-themes-common-palette-overrides
        '((fg-main "#ffffff")
          (bg-main "#000000")
          (comment "#696969")
          (string "#ba6e6e")
          (cursor "#ffffff")
          (variable "#cc635c")
          (constant "#f6726a")
          (builtin "#ac4742")
          (bg-mode-line "#1c1618")
          (bg-hl-line "#101010")))
  :config
  (load-theme 'ef-tritanopia-dark :no-confirm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Modeline
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Minimal modeline config
(use-package doom-modeline
  :ensure t
  :init
  (setq doom-modeline-icon nil)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-time-icon nil)
  (setq doom-modeline-buffer-state-icon t)
  (setq doom-modeline-buffer-modification-icon nil)
  (setq doom-modeline-unicode-fallback t)
  (setq doom-modeline-modal nil)
  (setq doom-modeline-gnus nil)
  (setq doom-modeline-modal-modern-icon nil)
  (setq doom-modeline-percent-position nil)
  (setq doom-modeline-time t)
  (setq display-time-format "%T")
  (doom-modeline-mode 1))

;; Modeline toggle
(use-package hide-mode-line
  :ensure t
  :config
  ;; No mode line in terminal by default
  (if (not (window-system))
    (global-hide-mode-line-mode)))

;;; base.el ends here
