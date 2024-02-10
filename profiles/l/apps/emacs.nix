{ inputs, pkgs, ...}:

{
  # https://web.archive.org/web/20240209082003/https://esrh.me/posts/2021-11-27-emacs-config
  #TODO: emacsGit-nox once set up
  #TODO: colemak -> colemak-dh
  programs.emacs = {
    enable = true;
    package = pkgs.emacs-git;
    extraPackages = epkgs: [
    ];
#     extraConfig = ''
#       (setq ring-bell-function 'ignore)
#       (push '(tool-bar-lines . 0) default-frame-alist)
#       (push '(menu-bar-lines . 0) default-frame-alist)
#       (scroll-bar-mode -1)
#       (column-number-mode)
#       (show-paren-mode)
#       (defun show-paren--locate-near-paren-ad ()
#         "Locate an unescaped paren \"near\" point to show.
#       If one is found, return the cons (DIR . OUTSIDE), where DIR is 1
#       for an open paren, -1 for a close paren, and OUTSIDE is the buffer
#       position of the outside of the paren.  Otherwise return nil."
#         (let* ((before (show-paren--categorize-paren (point))))
#           (when (or
#              (eq (car before) 1)
#              (eq (car before) -1))
#             before)))
#
#       (advice-add 'show-paren--locate-near-paren
#                   :override #'show-paren--locate-near-paren-ad)
#
#       (global-hl-line-mode)
#
#       ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;
#       (defun meow-setup ()
#         (setq meow-cheatsheet-layout meow-cheatsheet-layout-colemak-dh)
#         (meow-motion-overwrite-define-key
#          ;; Use e to move up, n to move down.
#          ;; Since special modes usually use n to move down, we only overwrite e here.
#          '("e" . meow-prev)
#          '("<escape>" . ignore))
#         (meow-leader-define-key
#          '("?" . meow-cheatsheet)
#          ;; To execute the originally e in MOTION state, use SPC e.
#          '("e" . "H-e")
#          '("1" . meow-digit-argument)
#          '("2" . meow-digit-argument)
#          '("3" . meow-digit-argument)
#          '("4" . meow-digit-argument)
#          '("5" . meow-digit-argument)
#          '("6" . meow-digit-argument)
#          '("7" . meow-digit-argument)
#          '("8" . meow-digit-argument)
#          '("9" . meow-digit-argument)
#          '("0" . meow-digit-argument))
#         (meow-normal-define-key
#          '("0" . meow-expand-0)
#          '("1" . meow-expand-1)
#          '("2" . meow-expand-2)
#          '("3" . meow-expand-3)
#          '("4" . meow-expand-4)
#          '("5" . meow-expand-5)
#          '("6" . meow-expand-6)
#          '("7" . meow-expand-7)
#          '("8" . meow-expand-8)
#          '("9" . meow-expand-9)
#          '("-" . negative-argument)
#          '(";" . meow-reverse)
#          '("," . meow-inner-of-thing)
#          '("." . meow-bounds-of-thing)
#          '("[" . meow-beginning-of-thing)
#          '("]" . meow-end-of-thing)
#          '("/" . meow-visit)
#          '("a" . meow-append)
#          '("A" . meow-open-below)
#          '("b" . meow-back-word)
#          '("B" . meow-back-symbol)
#          '("c" . meow-change)
#          '("d" . meow-delete)
#          '("e" . meow-prev)
#          '("E" . meow-prev-expand)
#          '("f" . meow-find)
#          '("g" . meow-cancel-selection)
#          '("G" . meow-grab)
#          '("h" . meow-left)
#          '("H" . meow-left-expand)
#          '("i" . meow-right)
#          '("I" . meow-right-expand)
#          '("j" . meow-join)
#          '("k" . meow-kill)
#          '("l" . meow-line)
#          '("L" . meow-goto-line)
#          '("m" . meow-mark-word)
#          '("M" . meow-mark-symbol)
#          '("n" . meow-next)
#          '("N" . meow-next-expand)
#          '("o" . meow-block)
#          '("O" . meow-to-block)
#          '("p" . meow-yank)
#          '("q" . meow-quit)
#          '("r" . meow-replace)
#          '("s" . meow-insert)
#          '("S" . meow-open-above)
#          '("t" . meow-till)
#          '("u" . meow-undo)
#          '("U" . meow-undo-in-selection)
#          '("v" . meow-search)
#          '("w" . meow-next-word)
#          '("W" . meow-next-symbol)
#          '("x" . meow-delete)
#          '("X" . meow-backward-delete)
#          '("y" . meow-save)
#          '("z" . meow-pop-selection)
#          '("'" . repeat)
#          '("<escape>" . ignore)))
#
#       (require 'meow)
#       (meow-setup)
#       (meow-global-mode 1)
#     '';
  };
}
