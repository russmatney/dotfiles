;;; private/hlissner/config.el -*- lexical-binding: t; -*-

(when (featurep 'evil)
  (load! +bindings)  ; my key bindings
  (load! +commands)) ; my custom ex commands

(defvar +hlissner-dir
  (file-name-directory load-file-name))

(defvar +hlissner-snippets-dir
  (expand-file-name "snippets/" +hlissner-dir))

;;
(after! doom-themes
  ;; Since Fira Mono doesn't have an italicized variant, highlight it instead
  (set-face-attribute 'italic nil
                      :weight 'ultra-light
                      :foreground "#ffffff"
                      :background (doom-color 'current-line)))


(after! evil-mc
  ;; if I'm in insert mode, chances are I want cursors to resume
  (add-hook! 'evil-mc-before-cursors-created
    (add-hook 'evil-insert-state-entry-hook #'evil-mc-resume-cursors nil t))
  (add-hook! 'evil-mc-after-cursors-deleted
    (remove-hook 'evil-insert-state-entry-hook #'evil-mc-resume-cursors t)))


;; Don't use default snippets, use mine.
(after! yasnippet
  (setq yas-snippet-dirs
        (append (list '+hlissner-snippets-dir)
                (delete 'yas-installed-snippets-dir
                        yas-snippet-dirs))))

