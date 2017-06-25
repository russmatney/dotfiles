;;; init-extra.el --- Misc config without a home
;;; Commentary:
;;;    - https://emacs.stackexchange.com/questions/2189/how-can-i-prevent-a-command-from-using-specific-windows
;;; Code:

;;; buffer-wide find/replace toggle
(use-package iedit)

(use-package ag)

(use-package which-key
  :ensure t
  :defer t
  :diminish which-key-mode
  :init
  (setq which-key-sort-order 'which-key-key-order-alpha)
  :bind* (("M-m ?" . which-key-show-top-level))
  :config
  (which-key-mode)
  (which-key-add-key-based-replacements
    "M-m ?" "top level bindings"))


(use-package popup-imenu
  :config (define-key popup-isearch-keymap (kbd "<escape>") 'popup-isearch-cancel)
)

(use-package imenu-anywhere)


(savehist-mode 1) ;; remembers minibuffer histories between sessions


;;; focus mode
(defvar bzg-big-fringe-mode nil)
(define-minor-mode bzg-big-fringe-mode
  "Minor mode to use big fringe in the current buffer."
  :init-value nil
  :global t
  :variable bzg-big-fringe-mode
  :group 'editing-basics
  (if (not bzg-big-fringe-mode)
      (set-fringe-style nil)
    (set-fringe-mode
     (/ (- (frame-pixel-width)
           (* 100 (frame-char-width)))
        2))))

;; Now activate this global minor mode
;; (bzg-big-fringe-mode 1)

;; invoke to see all keybindings in a mode
(use-package discover-my-major)


;; C-u M-x align-regexp RET SPC RET RET RET y RET
(defun align-multi-column-by-wsp (p1 p2)
  (interactive "r")
  (align-regexp p1 p2 ",\\(\\s-*\\)" 1 1 t)
  (evil-visual)
)

(evil-leader/set-key
   "," 'align-multi-column-by-wsp
)


(require 'server)
(setq server-socket-dir "~/.emacs.d/server")
(server-start)

(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
)

(add-to-list 'auto-mode-alist '("zshrc" . shell-script-mode))

(defun toggle-window-dedicated ()
  "Control whether or not Emacs is allowed to display another
buffer in current window."
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window (not (window-dedicated-p window))))
       "%s: Can't touch this!"
     "%s is up for grabs.")
   (current-buffer)))


(use-package multi-line
  :config
  (global-set-key (kbd "C-c d") 'multi-line))


(provide 'init-extra)
;;; init-extra.el ends here
