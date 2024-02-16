(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 800000)))

(eval-when-compile
  (require 'use-package))

(use-package ergoemacs-mode
  :ensure t
  :config
  ;(require 'ergoemacs-mode)
  :bind(bind-key* "C-l" 'ergoemacs-select-current-line))

(use-package meow
  :ensure t
  :init
  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-colemak)
    (meow-motion-overwrite-define-key
     ;; Use e to move up, n to move down.
     ;; Since special modes usually use n to move down, we only overwrite e here.
     '("e" . meow-prev)
     '("<escape>" . ignore))
    (meow-leader-define-key
     '("?" . meow-cheatsheet)
     ;; To execute the originally e in MOTION state, use SPC e.
     '("e" . "H-e")
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument))
    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("1" . meow-expand-1)
     '("2" . meow-expand-2)
     '("3" . meow-expand-3)
     '("4" . meow-expand-4)
     '("5" . meow-expand-5)
     '("6" . meow-expand-6)
     '("7" . meow-expand-7)
     '("8" . meow-expand-8)
     '("9" . meow-expand-9)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '("/" . meow-visit)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("d" . meow-delete)
     '("e" . meow-prev)
     '("E" . meow-prev-expand)
     '("f" . meow-find)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-right)
     '("I" . meow-right-expand)
     '("j" . meow-join)
     '("k" . meow-kill)
     '("l" . meow-line)
     '("L" . meow-goto-line)
     '("m" . meow-mark-word)
     '("M" . meow-mark-symbol)
     '("n" . meow-next)
     '("N" . meow-next-expand)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-yank)
     '("q" . meow-quit)
     '("r" . meow-replace)
     '("s" . meow-insert)
     '("S" . meow-open-above)
     '("t" . meow-till)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("v" . meow-search)
     '("w" . meow-next-word)
     '("W" . meow-next-symbol)
     '("x" . meow-delete)
     '("X" . meow-backward-delete)
     '("y" . meow-save)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("<escape>" . ignore)))
  :config
  ;(require 'meow)
  (meow-setup)
  (meow-global-mode 1))

(use-package xclip
  :ensure t
  :config
  ;(require 'xclip)
  (xclip-mode 1))


;(defun kill-window-or-quit ()
;  (interactive)
;  (if (>= (count-windows) 2)
;      (delete-window)
;    (server-edit)))
;
;(setq inhibit-startup-message t)
;(menu-bar-mode -1)
;(tool-bar-mode -1)
;(global-display-line-numbers-mode -1)
;(load-theme 'modus-vivendi)
;
;(bind-key* "C-s" 'save-buffer)
;(bind-key* "C-q" 'kill-window-or-quit)
;(bind-key* "C-z" 'ergoemacs-undo)
;(bind-key* "C-y" 'ergoemacs-redo)
;(bind-key* "C-x" 'ergoemacs-cut-line-or-region)
;(bind-key* "C-c" 'ergoemacs-copy-line-or-region)
;(bind-key* "C-v" 'xterm-paste)
;
;(save-place-mode 1)
;(setq save-place-forget-unreadable-files nil)
