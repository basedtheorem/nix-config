;;; Contents:
;;;
;;;  - Basic settings
;;;  - Minibuffer/completion settings
;;;  - UI tweaks
;;;  - Tab-bar configuration
;;;  - Theme
;;;  - Modeline

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Basic settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Package initialization
(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

;; Minimal welcome screen
(setopt inhibit-splash-screen t)
(setq initial-scratch-message "")
(defun display-startup-echo-area-message ()
  (message "Press 'C-o' to open a file."))

(setopt initial-major-mode 'fundamental-mode)  ; default mode for the *scratch* buffer
(setopt display-time-default-load-average nil) ; this information is useless for most

;; Automatically reread from disk if the underlying file changes
(setopt auto-revert-avoid-polling t)
(setopt auto-revert-interval 5)
(setopt auto-revert-check-vc-info t)
(global-auto-revert-mode)

;; Save history of minibuffer
(savehist-mode)

;; Restore cursor position
(save-place-mode 1)

;; Whitespace command will show the following:
(setq whitespace-style '(face spaces tabs space-mark tab-mark))

;; Fix archaic defaults
(setopt sentence-end-double-space nil)

;; Make right-click show context menu
(when (display-graphic-p)
  (context-menu-mode))

;; Don't litter file system with *~ backup files
(defun backup-file-name (fpath)
  "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
  (let* ((backupRootDir "~/.emacs.d/emacs-backup/")
         (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path
         (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") )))
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath))
(setopt make-backup-file-name-function 'backup-file-name)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Disable `xterm-paste` since it doesn't replace region with pasted contents, only appends to it
(define-key global-map [xterm-paste] #'cua-paste)

;; Allow mouse clicks when run in terminal
(when (not (window-system))
  (xterm-mouse-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Minibuffer/completion settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Discovery aids
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; Shorter prompts
(fset 'yes-or-no-p 'y-or-n-p)

(setopt enable-recursive-minibuffers t)                     ; Use the minibuffer whilst in the minibuffer
(keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete) ; TAB acts more like how it does in the shell
(setopt completion-cycle-threshold 1)                       ; TAB cycles candidates
(setopt completions-detailed t)                             ; Show annotations
(setopt tab-always-indent 'complete)                        ; When I hit TAB, try to complete, otherwise, indent
(setopt completion-styles '(basic initials substring))      ; Different styles to match input to candidates
(setopt completion-auto-help 'always)                       ; Open completion always; `lazy' another option
(setopt completions-max-height 20)                          ; This is arbitrary
(setopt completions-detailed t)
(setopt completions-format 'one-column)
(setopt completions-group t)
(setopt completion-auto-select 'second-tab)                 ; Much more eager

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   UI tweaks
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Mode line information
(setopt line-number-mode t)                      ; Show current line in modeline
(setopt column-number-mode t)                    ; Show column as well

(setopt x-underline-at-descent-line nil)         ; Prettier underlines
(setopt switch-to-buffer-obey-display-actions t) ; Make switching buffers more consistent

(setopt show-trailing-whitespace nil)            ; By default, don't underline trailing spaces
(setopt indicate-buffer-boundaries 'left)        ; Show buffer top and bottom in the margin

;; Enable horizontal scrolling
(setopt mouse-wheel-tilt-scroll t)
(setopt mouse-wheel-flip-direction t)

;; Menu bar
(menu-bar-mode -1)

(setq-default indent-tabs-mode nil)
(setopt tab-width 2)
(setq scroll-margin 6)
(setq scroll-conservatively 1000)
(setq scroll-preserve-screen-position 'always)  ; Keep scroll position when navigating

;; Misc. UI tweaks
(blink-cursor-mode 1)                           ; Steady cursor
(pixel-scroll-precision-mode)                   ; Smooth scrolling

;; Use common keystrokes by default
(cua-mode)

;; Display line numbers in programming mode
;(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setopt display-line-numbers-width 3)

;; Wrap lines when working with text
(add-hook 'text-mode-hook 'visual-line-mode)

;; Highlight the current line
(let ((hl-line-hooks '(text-mode-hook prog-mode-hook)))
  (mapc (lambda (hook) (add-hook hook 'hl-line-mode)) hl-line-hooks))

(setq-default show-help-function nil
              use-file-dialog nil
              use-dialog-box nil
              pop-up-windows nil)
(tooltip-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Add some breathing room around frame
(setq default-frame-alist
      (append (list
               '(min-height . 1)  '(height . 45)
               '(min-width  . 1)  '(width  . 101)
               '(vertical-scroll-bars  . nil)
               '(internal-border-width . 24) ;; frame padding around the text
               '(left-fringe    . 0)
               '(right-fringe   . 0)
               '(menu-bar-lines . 0)
               '(tool-bar-lines . 0)
               '(ns-transparent-titlebar . t)))
      line-spacing 0.2)

;; Disable visual alarm when, e.g., backspacing at start of buffer.
(setopt visible-bell nil ring-bell-function #'ignore)


;; Persistent frame geometry
(defun save-frameg ()
  "Gets the current frame's geometry and saves to ~/.emacs.frameg."
  (let ((frameg-font (frame-parameter (selected-frame) 'font))
        (frameg-left (frame-parameter (selected-frame) 'left))
        (frameg-top (frame-parameter (selected-frame) 'top))
        (frameg-width (frame-parameter (selected-frame) 'width))
        (frameg-height (frame-parameter (selected-frame) 'height))
        (frameg-file (expand-file-name ".emacs.frameg" user-emacs-directory)))
    (with-temp-buffer
      ;; Turn off backup for this file
      (make-local-variable 'make-backup-files)
      (setq make-backup-files nil)
      (insert
       ";;; This file stores the previous emacs frame's geometry.\n"
       ";;; Last generated " (current-time-string) ".\n"
       "(setq initial-frame-alist\n"
       ;; " '((font . \"" frameg-font "\")\n"
       " '("
       (format " (top . %d)\n" (max frameg-top 0))
       (format " (left . %d)\n" (max frameg-left 0))
       (format " (width . %d)\n" (max frameg-width 0))
       (format " (height . %d)))\n" (max frameg-height 0)))
      (when (file-writable-p frameg-file)
        (write-file frameg-file)))))

(defun load-frameg ()
  "Loads ~/.emacs.frameg which should load the previous frame's geometry."
  (let ((frameg-file (expand-file-name ".emacs.frameg" user-emacs-directory)))
    (when (file-readable-p frameg-file)
      (load-file frameg-file))))

;; Use persistent frame geometry only when there is a window system
(if window-system
    (progn
      (add-hook 'after-init-hook 'load-frameg)
      (add-hook 'kill-emacs-hook 'save-frameg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Tab-bar configuration
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default tab-bar-show nil)

;; Add the time to the tab-bar, if visible
(add-to-list 'tab-bar-format 'tab-bar-format-align-right 'append)
(add-to-list 'tab-bar-format 'tab-bar-format-global 'append)
(setopt display-time-format "%a %F %T")
(setopt display-time-interval 1)
(display-time-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Theme
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For light theme
;; (use-package emacs
;;   ;; Light: modus-operandi, dark: modus-vivendi
;;  :config
;;  (load-theme 'modus-operandi))

;; Default black/red theme
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

(use-package hide-mode-line
  :ensure t
  :config
  ;; No mode line in terminal by default
  (if (not (window-system))
    (global-hide-mode-line-mode)))
