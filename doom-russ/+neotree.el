;;; private/russ/+neotree.el -*- lexical-binding: t; -*-

(map!
 (:after neotree
   :map neotree-mode-map
   :n "g"         nil
   :n [tab]       #'neotree-quick-look
   :n "RET"       #'neotree-enter
   :n [backspace] #'evil-window-prev
   :n "c"         #'neotree-create-node
   :n "j"         #'neotree-next-line
   :n "k"         #'neotree-previous-line
   :n "h"         #'+neotree/collapse-or-up
   :n "l"         #'+neotree/expand-or-open
   :n "J"         #'neotree-select-next-sibling-node
   :n "K"         #'neotree-select-previous-sibling-node
   :n "H"         #'neotree-select-up-node
   :n "L"         #'neotree-select-down-node
   :n "G"         #'evil-goto-line
   :n "gg"        #'evil-goto-first-line
   :n "v"         #'neotree-enter-vertical-split
   :n "s"         #'neotree-enter-horizontal-split
   :n "q"         #'neotree-hide
   :n "R"         #'neotree-refresh
   :n "r"         #'neotree-rename-node
   :n "d"         #'neotree-delete-node
   ))

(defun +russ/neotree-find-current-file ()
  "Select current buffer in Neotree.

If the current buffer is neotree, this closes neotree.

Returns nil if neotree was closed, t if it was opened."
  (interactive)
  (if (and (neo-global--window-exists-p)
           (eq (current-buffer) (neo-global--get-buffer)))
      (progn
        (neotree-hide)
        nil)
    (let ((project-dir (projectile-project-root))
            (file-name (buffer-file-name)))

        (neotree-show)
        (if project-dir
            (if (neo-global--window-exists-p)
                (progn
                (neotree-dir project-dir)
                (neotree-find file-name)))
        (message "Could not find git project root."))
        t)))

(defun +russ/neotree-reveal-current-file ()
  "Reveal current buffer in Neotree.

If the current buffer is neotree, this closes neotree."
  (interactive)
  (if (eq t (+russ/neotree-find-current-file))
    (neotree-enter)))
