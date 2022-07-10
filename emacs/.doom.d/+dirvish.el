;;; ../dotfiles/emacs/.doom.d/+dirvish.el -*- lexical-binding: t; -*-


;; https://github.com/alexluigit/dirvish
(use-package! dirvish
  :config
  (setq dired-kill-when-opening-new-dired-buffer t) ; added in emacs 28
  (setq dired-clean-confirm-killing-deleted-buffers nil)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  ;; (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t)
  (setq dired-listing-switches
        "-AGhlv --group-directories-first")

  (setq dirvish-attributes '(git-msg))
  (setq dirvish-hide-details t) ;; toggleable via dirvish-dispatch `? ('

  (map! :map dirvish-mode-map
        :n "b" #'dirvish-goto-bookmark
        :n "z" #'dirvish-show-history
        :n "f" #'dirvish-file-info-menu
        :n "F" #'dirvish-toggle-fullscreen
        :n "l" #'dired-find-file
        :n "h" #'dired-up-directory
        :n "?" #'dirvish-dispatch
        :localleader
        "h" #'dired-omit-mode))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dired
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package! dired
  :init
  (when (string= system-type "darwin")
    (setq dired-use-ls-dired nil))
  :config
  (enable-command 'dired-find-alternate-file)
  (map!
   ;; :n "-" #'dired-jump
   :n "-" #'dirvish
   :map dired-mode-map
   :n "-"        #'dired-up-directory
   :n "<return>" #'dired-find-alternate-file
   :n "/"        #'dired
   :n "q"        (cmd! (quit-window t))))
