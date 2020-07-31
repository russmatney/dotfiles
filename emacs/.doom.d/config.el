;;; .doom.d/config.el -*- lexical-binding: t; -*-

;;; Private keys'n'such
(load! "+private")

;; Basic Config
(setq backup-directory-alist `(("." . "~/.emacs-tmp/")))
(setq auto-save-file-name-transforms `((".*" "~/.emacs-tmp/" t)))

;; PATH
(add-to-list 'exec-path "/home/russ/.nix-profile/bin")
(add-to-list 'exec-path "/home/russ/.pyenv/shims")

;; Auto revert-mode
(global-auto-revert-mode t)

;; transparency
;; (+russ/transparency 85)

;; autofill mode for text-mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fonts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq doom-font (font-spec :family "RobotoMono Nerd Font" :size 20)
      doom-variable-pitch-font (font-spec :family "Hack Nerd Font")
      doom-unicode-font (font-spec :family "DejaVuSansMono Nerd Font Mono")
      doom-big-font (font-spec :family "SpaceMono Nerd Font" :size 24))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general format/whitespace
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
(setq whitespace-line-column 80
      whitespace-style
      '(face trailing lines-tail))
(setq-default fill-column 80)
(auto-fill-mode 1)
;; turn on whitespace mode
(global-whitespace-mode t)
;; but not in org
(setq whitespace-global-modes '(not org-mode))
;; turn on whitespace cleanup
(add-hook! 'before-save-hook 'whitespace-cleanup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc package config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq +ivy-buffer-icons t)

;; Add '--hidden' to rg command to include hidden files in search
;; Note that `echo ".git/" >> ~/.ignore` will exclude .git from these searches
(setq counsel-rg-base-command
      "rg -zS -T jupyter -T svg -T lock -T license --no-heading --line-number --color never %s .")

;; modeline

(use-package! doom-modeline
  :config
  (doom-modeline-def-modeline 'main
    '(bar workspace-name window-number modals matches checker buffer-info
          remote-host buffer-position word-count parrot selection-info)
    '(objed-state misc-info persp-name battery grip irc
              mu4e gnus github debug lsp minor-modes input-method
              indent-info buffer-encoding major-mode process vcs)))

;; company
(use-package! company
  :config
  (setq
   company-idle-delay 0.5
   company-minimum-prefix-length 5))

(use-package deft
      :after org
      :custom
      (deft-recursive t)
      (deft-directory org-roam-directory))

(use-package org-roam-server
  :ensure t
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8888
        org-roam-server-export-inline-images t
        org-roam-server-authenticate nil
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20))


(use-package! monroe
  :config
  (setq monroe-nrepl-server-cmd "jeejah"
        monroe-nrepl-server-cmd-args "--fennel --port 7777 --debug"
        monroe-nrepl-server-project-file ".gitignore"))

(defun fennel-enable-monroe ()
  (monroe-interaction-mode t))

(use-package! fennel-mode
  :hook (fennel-mode . rainbow-delimiters-mode)
  :config
  ;; (add-hook 'fennel-mode-hook #'fennel-enable-monroe)
  (setq fennel-mode-switch-to-repl-after-reload nil))

(use-package! friar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; other config files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load! "+bindings")
(load! "+langs")
(load! "+lisp-editing")
(load! "+clojure")
(load! "+org-custom")
(load! "+wakatime")
(load! "+custom")
;; (load! "+exwm")




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc fixes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
