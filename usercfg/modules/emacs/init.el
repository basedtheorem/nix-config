;;; init.el --- Sane config without third-party packages

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

;; Set XDG dirs
(defvar user-emacs-config-directory
  (concat (getenv "HOME") "/.config/emacs"))

(defvar user-emacs-data-directory
  (concat (getenv "HOME") "/.local/share/emacs"))

(defvar user-emacs-cache-directory
  (concat (getenv "HOME") "/.cache/emacs"))


(setopt
  ;; Minimal welcome screen
  inhibit-startup-screen t
  initial-scratch-message ""
  ;; Don't create ~/.emacs.d/
  user-emacs-directory user-emacs-data-directory)

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

;; What the whitespace command will show
(setq whitespace-style '(face spaces tabs space-mark tab-mark))

;; Fix archaic defaults
(setopt sentence-end-double-space nil)

;; Remove whitespace at EOF and end of lines
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Make right-click show context menu
(when (display-graphic-p)
  (context-menu-mode))

;; Don't clutter file system with *~ backup files
(let ((backup-dir (concat user-emacs-data-directory "/backups"))
      (auto-saves-dir (concat user-emacs-cache-directory "/auto-saves/")))
  (dolist (dir (list backup-dir auto-saves-dir))
    (when (not (file-directory-p dir))
      (make-directory dir t)))
  (setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
        auto-save-list-file-prefix (concat auto-saves-dir ".saves-")
        tramp-backup-directory-alist `((".*" . ,backup-dir))
        tramp-auto-save-directory auto-saves-dir))
(setq-default
      backup-by-copying t    ; Don't delink hardlinks
      delete-old-versions t
      version-control t      ; Use version numbers on backups
      kept-new-versions 5
      kept-old-versions 2)

;; Allow mouse clicks when running in the terminal
;; DISABLED because it interferes with (delete-selection-mode t)
;(when (not (window-system))
;  (xterm-mouse-mode))

;; Show current line & column in modeline
(setopt line-number-mode t)
(setopt column-number-mode t)

;; Make paste actually *overwrite* the selection
(delete-selection-mode 1)

(setq
 ;; Disable creation of lock-files named .#<filaname>
 create-lockfiles nil

 ;; Middle click pastes at point, not at where mouse is
 mouse-yank-at-point t

 ;; "yes-or-no-p" => "to y-or-n-p"
 use-short-answers t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Minibuffer/completion settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Shorter prompts
(fset 'yes-or-no-p 'y-or-n-p)

(setopt enable-recursive-minibuffers t)                     ; Use the minibuffer whilst in the minibuffer
(keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete) ; TAB acts more like how it does in the shell
(setopt completion-cycle-threshold 1)                       ; TAB cycles candidates
(setopt completions-detailed t)                             ; Show annotations
(setopt tab-always-indent 'complete)                        ; If TAB try to complete, else indent
(setopt completion-styles '(basic initials substring))      ; Different styles to match input to candidates
(setopt completion-auto-help 'always)                       ; Open completion always; other opt: `lazy'
(setopt completions-max-height 20)
(setopt completions-format 'one-column)
(setopt completions-group t)
(setopt completion-auto-select 'second-tab)                 ; Much more eager

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   UI tweaks
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setopt x-underline-at-descent-line nil)         ; Prettier underlines
(setopt switch-to-buffer-obey-display-actions t) ; Make switching buffers more consistent
(setopt show-trailing-whitespace nil)            ; Don't underline trailing spaces
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

;; Hide unimportant UI functions
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

;; Don't show the tab-bar
(setq-default tab-bar-show nil)

;; Add the time to the tab-bar, if visible
(add-to-list 'tab-bar-format 'tab-bar-format-align-right 'append)
(add-to-list 'tab-bar-format 'tab-bar-format-global 'append)
(setopt display-time-format "%a %F %T")
(setopt display-time-interval 1)
(display-time-mode)

;;; init.el ends here
