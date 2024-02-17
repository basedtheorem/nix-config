(add-to-list 'load-path "~/config/emacs/ergoemacs-mode")
(require 'ergoemacs-mode) ;; Intuitive commands
(require 'meow-util)

(use-package emacs
  :bind (("C-s" . 'save-buffer)
				 ("C-c" . 'ergoemacs-copy)
				 ("C-v" . 'ergoemacs-paste)
				 ("C-S-o" . 'find-file)
				 ("C-M-o" . 'find-file-rec)
				 ("C-q" . 'lrns/kill-window-or-quit)
				 ("C-l" . #'lrns/select-current-line-and-forward-line)
				 ("<home>" . 'lrns/beginning-of-line)
				 ("<backtab>" . 'lrns/untab-region)
				 ("C-=" . 'lrns/zoom-in)
				 ("C--" . 'lrns/zoom-out)
				 ("C-p" . execute-extended-command)
         ("C-<right>" . 'lrns/forward-word)
         ("C-<left>" . 'lrns/backward-word)
				 ("C-a" . mark-whole-buffer)
				 ("C-<backspace>" . 'lrns/backward-kill-word)
				 ("C-<delete>" . 'lrns/forward-kill-word)
				 ("C-/" . 'comment-line)
				 ("M-<left>" . 'previous-buffer)
				 ("M-<right>" . 'next-buffer)
         ("M-w" . 'whitespace-mode)
				 ("C-|" . 'lrns/shell-command)
				 :map emacs-lisp-mode-map
				 ("C-r" . 'eval-defun)
				 :map meow-insert-state-keymap
				 ("C-<escape>" . 'meow-insert-exit)
				 ("C-g" . 'meow-goto-line)
         ("<escape>" . 'lrns/meow-insert-exit)))

(use-package xclip
  :ensure t
  :init
  (xclip-mode 1))

(use-package clipetty
  :ensure t
  :hook (after-init . global-clipetty-mode))

;; Use Kitty terminal key protocol
(use-package kkp
  :ensure t
  :config
  (global-kkp-mode +1)
  ;; Map the Alt keyboard modifier to Alt (and not to Meta).
  ;(setq kkp-alt-modifier 'alt)
  )

(use-package crux
  :ensure t
  :bind (
				 :map cua-global-keymap
				 ("C-S-<return>" . 'crux-smart-open-line-above)
				 ("C-<return>" . 'crux-smart-open-line)))

;; Use smooth scrolling when recentering the view
(use-package pixel-scroll
  :ensure nil
  :custom
  (pixel-scroll-precision-interpolation-factor 1.0)
  :bind
  (("C-e" . recenter-top-bottom)
   ([remap scroll-up-command]   . lrns/pixel-scroll-up-command)
   ([remap scroll-down-command] . lrns/pixel-scroll-down-command)
   ([remap recenter-top-bottom] . lrns/pixel-recenter-top-bottom)
   ([remap recenter-top-bottom] . lrns/pixel-recenter-top-bottom))
  :hook
  (dashboard-after-initialize . pixel-scroll-precision-mode)
  :config
  (defun lrns/pixel-scroll-up-command ()
    "Similar to `scroll-up-command' but with pixel scrolling."
    (interactive)
    (pixel-scroll-precision-interpolate (- (* lrns/default-scroll-lines (line-pixel-height)))))
  (defun lrns/pixel-scroll-down-command ()
    "Similar to `scroll-down-command' but with pixel scrolling."
    (interactive)
    (pixel-scroll-precision-interpolate (* lrns/default-scroll-lines (line-pixel-height))))
  (defun lrns/pixel-recenter-top-bottom ()
    "Similar to `recenter-top-bottom' but with pixel scrolling."
    (interactive)
    (if (not (window-system))
        (recenter-top-bottom)
      (let* ((current-row (cdr (nth 6 (posn-at-point))))
             (target-row (save-window-excursion
                           (recenter-top-bottom)
                           (cdr (nth 6 (posn-at-point)))))
             (distance-in-pixels (* (- target-row current-row) (line-pixel-height))))
        (pixel-scroll-precision-interpolate distance-in-pixels)))))

;; Show recent files
(use-package recentf
  :ensure t
  :init
  (setq recentf-max-saved-items 100)
  :config
  (recentf-mode t)
  :bind
  ("C-o" . 'recentf))

(defun find-file-rec ()
  "Find a file in the current working directory recursively."
  (interactive)
  (find-file
   (completing-read "Find file: "
                    (apply #'process-lines find-files-program))))
(setq find-files-program
      (cond ((executable-find "rg") '("rg" "--color=never" "--files"))
            ((executable-find "find") '("find" "-type" "f"))))

(use-package goto-chg
  :ensure t
  :bind
  ("C-," . 'goto-last-change)
  ("C-." . 'goto-last-change-reverse))

(use-package multiple-cursors
  :ensure t
  :bind
  ("C-M-<down>" . 'mc/mark-next-like-this)
  ("C-M-<up>" . 'mc/mark-previous-like-this))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package vundo
  ;; Visual Undo
  ;; f   to go forward
  ;; b   to go backward
  ;; n   to go to the node below when at a branch point
  ;; p   to go to the node above
  ;; a   to go back to the last branching point
  ;; e   to go forward to the end/tip of the branch
  ;; l   to go to the last saved node
  ;; r   to go to the next saved node
  ;; m   to mark the current node for diff
  ;; u   to unmark the marked node
  ;; d   to show a diff between the marked (or parent) and current nodes
  ;; q   to quit, you can also type C-g
  :ensure t
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols))

(use-package undo-fu-session
  :ensure t
  :config
  (undo-fu-session-global-mode))

;; Better undo
(use-package undo-fu
  :ensure t
  :config
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "C-z")   'undo-fu-only-undo)
  (global-set-key (kbd "C-S-z") 'undo-fu-only-redo))

(use-package visual-regexp
  :ensure t
  :config
  ;; `!' -> replace all, `y' -> replace one, `n' -> skip (or next)
  (define-key global-map (kbd "C-M-f") 'vr/query-replace))

;; Use Python regex engine
(use-package visual-regexp-steroids
  :ensure t)

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
	;; Allow cross-buffer 'next'
  (setq bm-cycle-all-buffers nil)
	;; Delete bookmark after navigating to it
  (setq temporary-bookmark-p nil)
  ;; Save bookmarks
  (setq-default bm-buffer-persistence t)
  ;; Where to store persistant files
  (setq bm-repository-file "~/config/emacs/bm-repository")
  ;; Loading the repository from file when on start up.
  (add-hook 'after-init-hook 'bm-repository-load)
  ;; Saving bookmarks
  (add-hook 'kill-buffer-hook #'bm-buffer-save)
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

(use-package drag-stuff
	:ensure t
	:config
	(drag-stuff-global-mode 1)
	:bind
	("M-<up>" . drag-stuff-up)
	("M-<down>" . drag-stuff-down))

(use-package ws-butler
  :ensure t
  :config
  (add-hook 'before-save-hook #'ws-butler-global-mode))

(use-package move-dup
	:ensure t
	:bind
	("M-S-<down>" . move-dup-duplicate-down)
	("M-S-<up>" . move-dup-duplicate-up))

(use-package ctrlf
  :ensure t
  :bind (("C-f" . ctrlf-forward-default)
	       ("S-<f3>" . ctrlf-backward-default)
	       ("<f3>" . ctrlf-forward-default)
         ("C-S-f" . ctrlf-forward-alternate)
	       :map ctrlf-minibuffer-mode-map
         ("<up>" . ctrlf-backward-default)
         ("<down>" . ctrlf-forward-default)
	       ("S-<return>" . ctrlf-backward-default)
         ("<return>" . ctrlf-forward-default)
         ("<RET>" . ctrlf-forward-default)
         ("<escape>" . minibuffer-exit))
  )
(setq ctrlf--active-p nil)

(use-package highlight-indent-guides
  ;; Not working on emacs 29+
  :disabled
  :ensure t
  :hook ((prog-mode) . highlight-indent-guides-mode)
  :config
  (setq-default highlight-indent-guides-delay 0)
  (setq-default highlight-indent-guides-method 'character
                highlight-indent-guides-auto-enabled t
                highlight-indent-guides-responsive 'top)
  (set-face-foreground 'highlight-indent-guides-character-face "#393939")
  (set-face-foreground 'highlight-indent-guides-top-character-face "#808080")
  (if (window-system)
      (setq highlight-indent-guides-character ?•)
    (setq highlight-indent-guides-character ? ))
  )

(use-package hl-block-mode
  ;; Buggy
  :disabled
  :ensure t
  :commands (hl-block-mode)
  :config
  (setq hl-block-bracket nil) ;; Match all brackets.
  (setq hl-block-single-level t) ;; Only one pair of brackets.
  (setq hl-block-multi-line t)
  (setq hl-block-style 'color-tint)
  (setq hl-block-delay 0.1)
  (setq hl-block-tint "#000000")
  :hook ((prog-mode) . hl-block-mode)
  )

(defun meow-backward-line (n)
  "Like `meow-line', but always backward."
  (interactive "p")
  (let* ((n (if (not (meow--direction-backward-p))
								(- n)
							n)))
		(meow-line n)))

(defun meow-forward-line (n)
  "Like `meow-line', but always forward."
  (interactive "p")
  (let* ((n (if (meow--direction-backward-p)
								(- n)
							n)))
		(meow-line n)))

(defun lrns/meow-insert-exit ()
  "Cancel selection before exiting insert mode."
  (interactive)
  (when (use-region-p)
    (meow-cancel-selection))
  (mc/keyboard-quit)
  (meow-insert-exit))


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

(defun lrns/forward-word (&optional arg)
  "Move point to the end of the next word or string of
non-word-constituent characters.
Do it ARG times if ARG is positive, or -ARG times in the opposite
direction if ARG is negative. ARG defaults to 1."
  (interactive "^p")
  (if (> arg 0)
      (dotimes (_ arg)
        ;; First, skip whitespace ahead of point
        (when (looking-at-p "[ \t\n]")
          (skip-chars-forward " \t\n"))
        (unless (= (point) (point-max))
          ;; Now, if we're at the beginning of a word, skip it…
          (if (looking-at-p "\\sw")
              (skip-syntax-forward "w")
            ;; …otherwise it means we're at the beginning of a string of
            ;; symbols. Then move forward to another whitespace char,
            ;; word-constituent char, or to the end of the buffer.
            (if (re-search-forward "\n\\|\\s-\\|\\sw" nil t)
                (backward-char)
              (goto-char (point-max))))))
    (dotimes (_ (- arg))
      (when (looking-back "[ \t\n]")
        (skip-chars-backward " \t\n"))
      (unless (= (point) (point-min))
        (if (looking-back "\\sw")
            (skip-syntax-backward "w")
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

(defun lrns/select-current-line-and-forward-line (arg)
  "Select the current line and move the cursor by ARG lines IF
no region is selected.
If a region is already selected when calling this command, only move
the cursor by ARG lines."
  (interactive "p")
  (when (not (use-region-p))
    (forward-line 0)
    (set-mark-command nil))
  (forward-line arg))


(defun lrns/backward-kill-word ()
	"Remove all whitespace if the character behind the cursor is whitespace, otherwise remove a word."
  (interactive)
  (cond
   ((looking-back (rx (char word)) 1)
    (lrns/delete-backward-word))
   ((looking-back (rx (char blank)) 1)
    (delete-horizontal-space t))
   (t
    (backward-delete-char 1))))

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


(defun lrns/beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.
  If point was already at that position, move point to beginning of line."
  (interactive)
  (let ((oldpos (point)))
    (ergoemacs-beginning-of-line-or-what)
    (and (= oldpos (point))
         (back-to-indentation))))

(defun lrns/copy-line-above (&optional N)
	(interactive "p")
	(crux-duplicate-current-line-or-region N)
	(previous-line))

(defun lrns/copy-line-below (&optional N)
	(interactive "p"))

(defun lrns/kill-window-or-quit ()
  (interactive)
  (if (>= (count-windows) 2)
      (delete-window)
    (save-buffers-kill-terminal)))

(defun lrns/indent-region(numSpaces)
  (progn
                                        ; default to start and end of current line
    (setq regionStart (line-beginning-position))
    (setq regionEnd (line-end-position))
    ;; If there's a selection, use that instead of the current line
    (when (use-region-p)
      (setq regionStart (region-beginning))
      (setq regionEnd (region-end))
      )

    (save-excursion ; restore the position afterwards
      (goto-char regionStart) ; go to the start of region
      (setq start (line-beginning-position)) ; save the start of the line
      (goto-char regionEnd) ; go to the end of region
      (setq end (line-end-position)) ; save the end of the line

      (indent-rigidly start end numSpaces) ; indent between start and end
      (setq deactivate-mark nil) ; restore the selected region
      )))

(defun lrns/indent-lines(&optional N)
	(interactive "p")
	(indent-rigidly (line-beginning-position)
									(line-end-position)
									(* (or N 1) tab-width)))

(defun lrns/untab-region (&optional N)
  (interactive "p")
  (lrns/indent-region (* (* (or N 1) tab-width)-1)))

(defun  lrns/tab-region (N)
  (interactive "p")
  (if (use-region-p)
      (lrns/indent-region (* (or N 1) tab-width)) ; region was selected, call indent-region
    (lrns/indent-lines N); else insert spaces as expected
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

(setq ergoemacs-end-of-comment-line nil)
(setq ergoemacs-use-beginning-or-end-of-line-only t)
(setq whitespace-style '(face spaces tabs space-mark tab-mark))
