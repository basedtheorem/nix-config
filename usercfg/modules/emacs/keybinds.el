;;; keybinds.el --- Supercharged insert-mode keybindings

;;; Contents:
;;;
;;;  - Local keybinds
;;;  - Clipboard
;;;  - History
;;;  - Search & replace
;;;  - Misc. keybinds

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Local keybinds
;;;   - These don't rely on external packages.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emacs
  :bind (
         :map lrns/keys-keymap
         ("C-s" . 'save-buffer)
         ("C-S-o" . 'find-file)
         ("C-M-o" . 'lrns/find-file-rec)
         ("C-q" . 'lrns/kill-window-or-quit)
         ("M-l" . 'kill-whole-line)
         ("C-l" . 'lrns/select-current-line-and-forward-line)
         ("C-M-l" . 'lrns/select-current-line-and-backward-line)
         ("<backtab>" . 'lrns/untab-region)
         ("C-=" . 'lrns/zoom-in)
         ("C--" . 'lrns/zoom-out)
         ("C-<up>" . 'backward-paragraph)
         ("C-<down>" . 'forward-paragraph)
         ("C-p" . execute-extended-command)
         ("M-p" . eval-expression)
         ("C-<right>" . 'lrns/forward-word)
         ("C-<left>" . 'lrns/backward-word)
         ("C-a" . mark-whole-buffer)
         ("C-<backspace>" . 'lrns/backward-kill-word)
         ("C-<delete>" . 'lrns/forward-kill-word)
         ("C-/" . 'lrns/comment-line)
         ("M-/" . 'lrns/comment-line)                    ; #TODO: make this comment up
         ("M-<left>" . 'previous-buffer)
         ("M-<right>" . 'next-buffer)
         ("M-<return>" . 'default-indent-new-line)       ; New line when inside comments
         ("M-w" . 'whitespace-mode)
         ("C-c" . 'cua-copy)
         ("C-v" . 'cua-paste)
         ("C-|" . 'lrns/shell-command)
         :map emacs-lisp-mode-map
         ("C-r" . 'eval-defun))
  :config
  ;; `xterm-paste` does NOT replace region with pasted contents,
  ;; only appends to it, so swap with cua-paste
  (define-key global-map [xterm-paste] #'cua-paste)
  :init
  ;; Use common keystrokes by default
  (cua-mode)
  ;; Use C-y as prefix key instead of C-c which is mapped to copy
  (global-set-key (kbd "C-y") nil)

  (defvar lrns/keys-keymap (make-keymap)
    "Keymap for lrns/keys-mode")

  (define-minor-mode lrns/keys-mode
    "Minor mode for my personal keybindings."
    :init-value t
    :global t
    :keymap lrns/keys-keymap)

  ;; The keymaps in `emulation-mode-map-alists' take precedence over
  ;; `minor-mode-map-alist'
  (add-to-list 'emulation-mode-map-alists
               `((lrns/keys-mode . ,lrns/keys-keymap)))


  (defun lrns/find-file-rec ()
    "Find a file in the current working directory recursively."
    (interactive)
    (find-file
     (completing-read "Find file: "
                      (apply #'process-lines find-files-program))))
  (setq find-files-program
        (cond ((executable-find "rg") '("rg" "--color=never" "--files"))
              ((executable-find "find") '("find" "-type" "f"))))

  (defun lrns/kill-window-or-quit ()
    (interactive)
    (let ((file-count (cl-count-if (lambda (b)
                                     (buffer-file-name b))
                                   (buffer-list))))
      (if (minibufferp)
          (meow-minibuffer-quit)
        (if (>= (count-windows) 2)
            (kill-buffer)
          (if (or (= 0 file-count) (and buffer-file-name (>= 1 file-count)))
              (save-buffers-kill-terminal)
            ;; Fallback
            (if (> (length (visible-frame-list)) 2)
                ;; Other frames are open, only kill terminal not buffer
                (save-buffers-kill-terminal)
              (kill-buffer))))
        )))

  (defun lrns/select-current-line-and-forward-line ()
    "Select the current line and move the cursor by ARG lines IF
    no region is selected.
    If a region is already selected when calling this command, only move
    the cursor by ARG lines."
    (interactive)
    (beginning-of-line)
    (setq this-command-keys-shift-translated t)
    (execute-extended-command nil "end-of-line")
    (execute-extended-command nil "forward-char"))

  (defun lrns/select-current-line-and-backward-line ()
    (interactive)
    (end-of-line)
    (setq this-command-keys-shift-translated t)
    (execute-extended-command nil "end-of-line")
    (execute-extended-command nil "forward-char"))

  (defun lrns/indent-region(numSpaces)
    (progn
      ;; Default to start and end of current line
      (setq regionStart (line-beginning-position))
      (setq regionEnd (line-end-position))
      ;; If there's a selection, use that instead of the current line
      (when (use-region-p)
        (setq regionStart (region-beginning))
        (setq regionEnd (region-end))
        )

      (save-excursion                           ; Restore the position afterwards
        (goto-char regionStart)                 ; Go to the start of region
        (setq start (line-beginning-position))  ; Save the start of the line
        (goto-char regionEnd)                   ; Go to the end of region
        (setq end (line-end-position))          ; Save the end of the line

        (indent-rigidly start end numSpaces)    ; Indent between start and end
        (setq deactivate-mark nil)              ; Restore the selected region
        )))

  (defun lrns/indent-lines(&optional N)
    (interactive "p")
    (indent-rigidly (line-beginning-position)
                    (line-end-position)
                    (* (or N 1) tab-width)))

  (defun lrns/untab-region (&optional N)
    (interactive "p")
    (lrns/indent-region (* (* (or N 1) tab-width)-1)))

  (defun lrns/tab-region (N)
    (interactive "p")
    (if (use-region-p)
        ;; Region was selected, call indent-region
        (lrns/indent-region (* (or N 1) tab-width))
      ;; Else insert spaces as expected
      (lrns/indent-lines N)
      ))

  (defun lrns/zoom-in ()
    "Increase font size by 10 points"
    (interactive)
    (set-face-attribute 'default nil
                        :height
                        (+ (face-attribute 'default :height)
                           10)))

  (defun lrns/zoom-out ()
    "Decrease font size by 10 points"
    (interactive)
    (set-face-attribute 'default nil
                        :height
                        (- (face-attribute 'default :height)
                           10)))

  (defun lrns/forward-word (&optional arg)
    "Move point to the end of the next word or string of
    non-word-constituent characters.
    Do it ARG times if ARG is positive, or -ARG times in the opposite
    direction if ARG is negative. ARG defaults to 1.
    Respects `subword-mode'."
    (interactive "^p")
    (if (> arg 0)
        (dotimes (_ arg)
          ;; First, skip whitespace ahead of point
          (when (looking-at-p "[ \t\n]")
            (skip-chars-forward " \t\n"))
          (unless (= (point) (point-max))
            ;; If point is at the beginning of a word, skip
            (if (looking-at-p "\\sw")
                (if subword-mode
                    (subword-right)
                  (skip-syntax-forward "w"))
              ;; Else point is at the beginning of a string of
              ;; symbols. Move forward to another whitespace char,
              ;; word-constituent char, or to the end of the buffer.
              (if (re-search-forward "\n\\|\\s-\\|\\sw" nil t)
                  (backward-char)
                (goto-char (point-max))))))
      (dotimes (_ (- arg))
        (when (looking-back "[ \t\n]")
          (skip-chars-backward " \t\n"))
        (unless (= (point) (point-min))
          (if (looking-back "\\sw")
              (if subword-mode
                  (subword-left)
                (skip-syntax-backward "w"))
            (if (re-search-backward "\n\\|\\s-\\|\\sw" nil t)
                (forward-char)
              (goto-char (point-min))))))))

  (defun lrns/backward-word (&optional arg)
    "Move point to the beginning of the previous word or string of
    non-word-constituent characters.
    Do it ARG times if ARG is positive, or -ARG times in the opposite
    direction if ARG is negative. ARG defaults to 1."
    (interactive "^p")
    (lrns/forward-word (- arg)))

  (defun lrns/backward-kill-word ()
    "Remove all whitespace if the character behind the cursor is whitespace, otherwise remove a word."
    (interactive)
    (if (use-region-p)
        (cua-delete-region)
    (cond
     ((looking-back (rx (char word)) 1)
      (lrns/delete-backward-word))
     ((looking-back (rx (char blank)) 1)
      (delete-horizontal-space t))
     (t
      (backward-delete-char 1)))))

  (defun lrns/forward-kill-word ()
    "Remove all whitespace if the character ahead the cursor is whitespace, otherwise remove a word."
    (interactive)
    (cond
     ((looking-at (rx (char word)) 1)
      (lrns/delete-forward-word))
     ((looking-at (rx (char blank)) 1)
      (delete-horizontal-space))
     (t
      (delete-forward-char 1))))

  (defun lrns/delete-backward-word ()
    (interactive "*")
    (push-mark)
    (backward-word)
    (delete-region (point) (mark)))

  (defun lrns/delete-forward-word ()
    (interactive "*")
    (push-mark)
    (forward-word)
    (delete-region (point) (mark)))

  (defun current-line-empty-p ()
    (string-match-p "\\`\\s-*$" (thing-at-point 'line)))

  (defun lrns/comment-line (n)
    (interactive "p")
    "If line is empty, insert comment. Run regular cmd otherwise."
    (if (current-line-empty-p)
        (comment-dwim nil)
      (comment-line n)))

  (defun lrns/shell-command (command arg)
    "Unifies `shell-command' and `shell-command-on-region'.
  Will preview output in mini-buffer,use C-u to (re)place.
  If no region is selected, run a shell command just like M-x
  shell-command (M-!).
  If no region is selected and an argument is a passed, run a shell command
  and place its output after the mark as in C-u M-x `shell-command' (C-u
  M-!).
  If a region is selected pass the text of that region to the
  shell and replace the text in that region with the output of the shell
  command as in C-u M-x `shell-command-on-region' (C-u M-|).
  If a region is selected AND an argument is passed (via C-u) send output
  to another buffer instead of replacing the text in region."
    (interactive (list (read-from-minibuffer
                        "Shell command: " nil nil nil 'shell-command-history)
                       current-prefix-arg))
    (let ((p (if mark-active (region-beginning) 0))
          (m (if mark-active (region-end) 0)))
      (if (= p m)
          ;; No active region
          (if (eq arg nil)
              (shell-command command)
            (shell-command command t))
        ;; Active region
        (if (eq arg nil)
            (shell-command-on-region p m command t t)
          (shell-command-on-region p m command)))))
  )

;; Use smooth scrolling on page-down and view
(use-package pixel-scroll
  :ensure nil
  :custom
  (pixel-scroll-precision-interpolation-factor 1.0)
  :bind
  ("C-e" . lrns/pixel-recenter-top-bottom)
  ;; I frequently hit this keybind on accident, so make it
  ;; do the same as `<next>`
  ("C-<next>" . 'pixel-scroll-interpolate-down)
  :hook
  (dashboard-after-initialize . pixel-scroll-precision-mode)
  :config
  (setq pixel-scroll-precision-interpolate-page t)
  (defun lrns/pixel-recenter-top-bottom ()
    "Similar to `recenter-top-bottom' but with pixel scrolling.
     Does not work in TTY."
    (interactive)
    (if (not (window-system))
        (recenter-top-bottom)
      (let* ((current-row (cdr (nth 6 (posn-at-point))))
             (target-row (save-window-excursion
                           (recenter-top-bottom)
                           (cdr (nth 6 (posn-at-point)))))
             (distance-in-pixels (* (- target-row current-row) (line-pixel-height))))
        (pixel-scroll-precision-interpolate distance-in-pixels)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Clipboard
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use xclip for copy and paste
(use-package xclip
:ensure t
:init
(xclip-mode 1))

;; Send every copy in TTY to clipboard
(use-package clipetty
  :ensure t
  :hook (after-init . global-clipetty-mode))

;; Copy & paste, home key
(use-package ergoemacs-mode
  :ensure t
  :init
  (setq ergoemacs-end-of-comment-line nil)              ; Move to start of line immediately
  (setq ergoemacs-use-beginning-or-end-of-line-only t)  ; Only go to beginning/end of line
  (defun lrns/beginning-of-line ()
    "Move point to first non-whitespace character or beginning-of-line.
    If point was already at that position, move point to beginning of line."
    (interactive)
    (let ((oldpos (point)))
      (ergoemacs-beginning-of-line-or-what)
      (and (= oldpos (point))
           (back-to-indentation))))
  :bind
  ("<home>" . 'lrns/beginning-of-line))

;; Use Kitty terminal key protocol
(use-package kkp
  :ensure t
  :config
  (global-kkp-mode +1)
  ;; Map the Alt keyboard modifier to Alt (and not to Meta).
  ;(setq kkp-alt-modifier 'alt)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   History
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; More intuitive undo
(use-package undo-fu
  :ensure t
  :config
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "C-z")   'undo-fu-only-undo)
  (global-set-key (kbd "C-S-z") 'undo-fu-only-redo))

;; Persistent undo history
(use-package undo-fu-session
  :ensure t
  :config
  (undo-fu-session-global-mode))

;; Undo history displayed as a tree
(use-package vundo
  :ensure t
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols))

;; Move point to locations containing recent changes
(use-package goto-chg
  :ensure t
  :bind
  ("C-," . 'goto-last-change)
  ("C-." . 'goto-last-change-reverse))

;; Show recent files
(use-package recentf
  :init
  (setq recentf-max-saved-items 100)
  :config
  (recentf-mode t)
  :bind
  ("C-o" . 'recentf))

;; Create bookmarks on lines
(use-package bm
  :ensure t
  :demand t
  :init
  (setq bm-restore-repository-on-load t)
  :bind
  ("<f5>" . 'bm-toggle)
  ("<f6>" . 'bm-next)
  ("S-<f6>" . 'bm-previous)
  :config
  ;; Allow cross-buffer 'next' bookmark
  (setq bm-cycle-all-buffers nil)
  ;; Delete bookmark after navigating to it
  (setq temporary-bookmark-p nil)
  ;; Save bookmarks
  (setq-default bm-buffer-persistence t)
  ;; Where to store persistant files
  (setq bm-repository-file "~/tmp/emacs/bm-repository")
  ;; Saving bookmarks
  (add-hook 'kill-buffer-hook #'bm-buffer-save)
  ;; Loading the repository from file when on start up.
  (add-hook 'after-init-hook 'bm-repository-load)
  ;; Saving the repository to file when on exit.
  ;; kill-buffer-hook is not called when Emacs is killed, so we
  ;; must save all bookmarks first.
  (add-hook 'kill-emacs-hook #'(lambda nil
                                 (bm-buffer-save-all)
                                 (bm-repository-save)))
  ;; The `after-save-hook' is not necessary to use to achieve persistence,
  ;; but it makes the bookmark data in repository more in sync with the file
  ;; state.
  (add-hook 'after-save-hook #'bm-buffer-save)
  ;; Restoring bookmarks
  (add-hook 'find-file-hooks   #'bm-buffer-restore)
  (add-hook 'after-revert-hook #'bm-buffer-restore)
  ;; The `after-revert-hook' is not necessary to use to achieve persistence,
  ;; but it makes the bookmark data in repository more in sync with the file
  ;; state. This hook might cause trouble when using packages
  ;; that automatically reverts the buffer (like vc after a check-in).
  ;; This can easily be avoided if the package provides a hook that is
  ;; called before the buffer is reverted (like `vc-before-checkin-hook').
  ;; Then new bookmarks can be saved before the buffer is reverted.
  ;; Make sure bookmarks is saved before check-in (and revert-buffer)
  (add-hook 'vc-before-checkin-hook #'bm-buffer-save))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Search & replace
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Intuitive search command
(use-package ctrlf
  :ensure t
  :bind (("C-f" . ctrlf-forward-default)
         ("S-<f3>" . ctrlf-backward-default)
         ("<f3>" . ctrlf-forward-default)
         ;; Regexp search
         ("C-S-f" . ctrlf-forward-alternate)
         :map ctrlf-minibuffer-mode-map
         ("<up>" . ctrlf-backward-default)
         ("<down>" . ctrlf-forward-default)
         ("S-<return>" . ctrlf-backward-default)
         ("<return>" . ctrlf-forward-default)
         ("<RET>" . ctrlf-forward-default)
         ("<escape>" . minibuffer-exit))
  )

;; Visualise replacements before committing
(use-package visual-regexp
  :ensure t
  :config
  ;; `!' -> replace all, `y' -> replace one, `n' -> skip (or next)
  (define-key global-map (kbd "C-M-f") 'vr/query-replace))

;; Use Python regex engine for search & replace
(use-package visual-regexp-steroids
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Misc. keybinds
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Duplicate lines below/above
(use-package move-dup
  :ensure t
  :bind
  ("M-S-<down>" . move-dup-duplicate-down)
  ("M-S-<up>" . move-dup-duplicate-up))

;; Move lines up/down
(use-package drag-stuff
  :ensure t
  :config
  (drag-stuff-global-mode 1)
  :bind
  ("M-<up>" . drag-stuff-up)
  ("M-<down>" . drag-stuff-down))

(use-package multiple-cursors
  :ensure t
  :bind
  ("C-M-<down>" . 'mc/mark-next-like-this)
  ("C-M-<up>" . 'mc/mark-previous-like-this))

;; Create new line without modifying current line
(use-package crux
  :ensure t
  :bind (
         :map cua-global-keymap
         ("C-S-<return>" . 'crux-smart-open-line-above)
         ("C-<return>" . 'crux-smart-open-line)))

;;; keybinds.el ends here
