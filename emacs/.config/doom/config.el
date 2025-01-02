;;; .doom.d/config.el -*- lexical-binding: t; -*-

(defmacro comment (&rest _)
  "Comment out one or more s-expressions."
  nil)

;; duped in init.el
(setq mac-command-modifier      'super
      ns-command-modifier       'super
      mac-option-modifier       'meta
      ns-option-modifier        'meta

      ;; required to send M-x instead of mac unicode shortcuts
      ;; TODO why isn't this being set at startup? perhaps it's overwritten later?
      ;; [[file:~/.emacs.d/lisp/doom-keybinds.el::mac-right-option-modifier 'none][maybe overwritten here?]]
      mac-right-option-modifier 'meta
      ns-right-option-modifier 'meta)


;; Fonts ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (not IS-MAC)
    (setq doom-font (font-spec :family "RobotoMono Nerd Font" :size 20 :slant 'normal)
          doom-variable-pitch-font (font-spec :family "Hack Nerd Font" :slant 'normal)
          doom-serif-font (font-spec :family "Hack Nerd Font" :slant 'normal)
          doom-symbol-font (font-spec :family "DejaVuSansMono Nerd Font Mono")
          doom-big-font (font-spec :family "SpaceMono Nerd Font" :size 24 :slant 'normal)))

(comment
 (doom/increase-font-size 1)
 (doom/decrease-font-size 1))

;; theme
;; (setq doom-theme 'doom-one-light)
;; (setq doom-theme 'doom-solarized-light)
;; (setq doom-theme 'doom-monokai-machine)
;; (setq doom-theme 'doom-city-lights)
(setq doom-theme 'doom-moonlight)
;; (setq doom-theme 'doom-monokai-pro)

;; Backup Config
(setq backup-directory-alist `(("." . "~/.emacs-tmp/")))
(setq auto-save-file-name-transforms `((".*" "~/.emacs-tmp/" t)))

;; PATH
(add-to-list 'exec-path "/home/russ/.nix-profile/bin")
(add-to-list 'exec-path "/home/russ/.pyenv/shims")

;; Auto revert-mode
(global-auto-revert-mode t)

;; https://github.com/d12frosted/homebrew-emacs-plus/issues/383
(if IS-MAC
    (setq insert-directory-program "gls" dired-use-ls-dired t))

;; format on save
(setq +format-on-save-enabled-modes t)

;; Spaces over tabs
(setq c-basic-indent 2)
(setq c-default-style "linux")
(setq tab-width 2)
(setq-default indent-tabs-mode nil)

(set-face-attribute 'default nil :height 100)

;; Turn off line wrapping
(setq-default truncate-lines 1)

;; 80 chars
(setq whitespace-line-column 100
      whitespace-style
      '(face trailing lines-tail))

(setq-default fill-column 80)
(auto-fill-mode 1)

;; autofill mode for text-mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; turn on whitespace mode
(global-whitespace-mode t)
;; but not in org
(setq whitespace-global-modes '(not org-mode ink-mode markdown-mode magit-mode))
;; turn on whitespace cleanup
(add-hook! 'before-save-hook 'whitespace-cleanup)

;; eval max len
(setq eval-expression-print-length 200)


;; handle very long lines (so-long)
(defun doom-buffer-has-long-lines-p ()
  "Fix for dired sometimes asking for comment syntax."
  (when comment-use-syntax
    (so-long-detected-long-line-p)))

(setq so-long-predicate #'doom-buffer-has-long-lines-p)


(after! dtrt-indent
  (add-to-list 'dtrt-indent-hook-mapping-list '(typescript-tsx-mode javascript typescript-indent-level))
  (add-to-list 'dtrt-indent-hook-mapping-list '(typescript-mode javascript typescript-indent-level)))

;; dir-locals

;; from https://emacs.stackexchange.com/questions/13080/reloading-directory-local-variables
(defun my-reload-dir-locals-for-current-buffer ()
  "Reload dir locals for the current buffer."
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

(defun my-reload-dir-locals-for-all-buffer-in-this-directory ()
  "For every buffer with the same `default-directory` as the current buffer's,
reload dir-locals."
  (interactive)
  (let ((dir default-directory))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (equal default-directory dir))
        (my-reload-dir-locals-for-current-buffer)))))


;;;;;;;;;;;;;;;;;;;;;
;; modeline

(use-package! doom-modeline
  :config
  (doom-modeline-def-modeline 'main
    '(bar workspace-name window-number modals matches check buffer-info
          remote-host buffer-position word-count parrot selection-info)
    '(objed-state misc-info persp-name battery grip irc
                  mu4e gnus github debug lsp minor-modes input-method
                  indent-info buffer-encoding major-mode process vcs)))

;;;;;;;;;;;;;;;;;;;;;
;; evil

(use-package! evil
  :preface
  (setq evil-ex-substitute-global t))


;; https://github.com/Malabarba/aggressive-indent-mode/issues/138
;; https://github.com/Malabarba/aggressive-indent-mode/issues/137
;; https://discordapp.com/channels/406534637242810369/406624667496087572/714350381324304446

(defadvice! kill-aggressive-indent-timers (l r &rest _)
  :override #'aggressive-indent--keep-track-of-changes
  (when aggressive-indent-mode
    (push (list l r) aggressive-indent--changed-list)
    (when (timerp aggressive-indent--idle-timer)
      (cancel-timer aggressive-indent--idle-timer))
    (setq aggressive-indent--idle-timer
          (run-with-idle-timer
           aggressive-indent-sit-for-time
           nil #'aggressive-indent--indent-if-changed (current-buffer)))))

(set-popup-rules!
  ;; tryna hide/prevent any kind of popup from showing...
  '(("^\\*clawebb\\*" :quit t :ttl 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; other config files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load! "+corfu")
(load! "+hydra")
(load! "+bindings")
(load! "+langs")
(load! "+lisp-editing")
(load! "+clojure")
(load! "+org-custom")
(load! "+custom")
(load! "+wakatime")
(load! "+vertico")
(load! "+magit")
(load! "+projectile")
(load! "+dirvish")
(load! "+fabb")
(load! "+ink")
(load! "+godot")
;; (load! "+kubernetes")
(load! "+discord")
;; (load! "+gerbil")
;; (load! "+exwm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package! nov-mode
  :mode "\\.epub$")

(use-package! logview)

;;;;;;;;;;;;;;;;;;;;;;;;
;; js/web

(setq typescript-indent-level 2)
(setq js-indent-level 2)
(setq css-indent-offset 2)

;; (when (not startup-notified)
;;   (require 'notifications)
;;   (notifications-notify
;;    :title "Emacs started!"
;;    :body "Start your engines.")
;;   (setq startup-notified t))
