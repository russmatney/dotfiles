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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; other config files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load! "+bindings")
(load! "+langs")
(load! "+lisp-editing")
(load! "+clojure")
(load! "+org-custom")
(load! "+wakatime")
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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#191919" "#e45649" "#50a14f" "#986801" "#4078f2" "#a626a4" "#0184bc" "#383a42"])
 '(custom-safe-themes
   (quote
    ("5b809c3eae60da2af8a8cfba4e9e04b4d608cb49584cb5998f6e4a1c87c057c4" "5d09b4ad5649fea40249dd937eaaa8f8a229db1cec9a1a0ef0de3ccf63523014" "3d3807f1070bb91a68d6638a708ee09e63c0825ad21809c87138e676a60bda5d" "6177ecbffb8f37756012c9ee9fd73fc043520836d254397566e37c6204118852" "3577ee091e1d318c49889574a31175970472f6f182a9789f1a3e9e4513641d86" "9f15d03580b08dae41a1e5c1f00d1f1aa99fea121ca32c28e2abec9563c6e32c" "25f1b2ace87d23d803b42267fafdc38b31472e444c2aaa9069aa2c06be8955b2" "bf387180109d222aee6bb089db48ed38403a1e330c9ec69fe1f52460a8936b66" "425cf02839fa7c5ebd6cb11f8074f6b8463ae6ed3eeb4cf5a2b18ffc33383b0b" "1623aa627fecd5877246f48199b8e2856647c99c6acdab506173f9bb8b0a41ac" "e1ef2d5b8091f4953fe17b4ca3dd143d476c106e221d92ded38614266cea3c8b" "82360e5f96244ce8cc6e765eeebe7788c2c5f3aeb96c1a765629c5c7937c0b5b" "c83c095dd01cde64b631fb0fe5980587deec3834dc55144a6e78ff91ebc80b19" "dde8c620311ea241c0b490af8e6f570fdd3b941d7bc209e55cd87884eb733b0e" "bc836bf29eab22d7e5b4c142d201bcce351806b7c1f94955ccafab8ce5b20208" "79278310dd6cacf2d2f491063c4ab8b129fee2a498e4c25912ddaa6c3c5b621e" "9efb2d10bfb38fe7cd4586afb3e644d082cbcdb7435f3d1e8dd9413cbe5e61fc" "54cf3f8314ce89c4d7e20ae52f7ff0739efb458f4326a2ca075bf34bc0b4f499" "d71aabbbd692b54b6263bfe016607f93553ea214bc1435d17de98894a5c3a086" "3c2f28c6ba2ad7373ea4c43f28fcf2eed14818ec9f0659b1c97d4e89c99e091e" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "2f1518e906a8b60fac943d02ad415f1d8b3933a5a7f75e307e6e9a26ef5bf570" "0809c08440b51a39c77ec5529f89af83ab256a9d48107b088d40098ce322c7d8" "6e2d579b02aadc933f434003f49d269d004f5c7094eb53658afbacc817761d83" "10461a3c8ca61c52dfbbdedd974319b7f7fd720b091996481c8fb1dded6c6116" "49ec957b508c7d64708b40b0273697a84d3fee4f15dd9fc4a9588016adee3dad" default)))
 '(fci-rule-color "#383a42")
 '(flycheck-javascript-flow-args nil)
 '(jdee-db-active-breakpoint-face-colors (cons "#f0f0f0" "#4078f2"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#f0f0f0" "#50a14f"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#f0f0f0" "#9ca0a4"))
 '(objed-cursor-color "#e45649")
 '(safe-local-variable-values
   (quote
    ((eval require
           (quote org-roam-dev))
     (cider-known-endpoints
      ("bb" "localhost" "1667"))
     (cider-known-endpoints
      ("yodo-clj-server" "localhost" "7888")
      ("yodo-cljs-shadow" "localhost" "4201"))
     (cider-known-endpoints
      ("yodo-cljs-shadow" "localhost" "4201"))
     (checkdoc-package-keywords-flag)
     (eval progn
           (pp-buffer)
           (indent-buffer)))))
 '(vc-annotate-background "#fafafa")
 '(vc-annotate-color-map
   (list
    (cons 20 "#50a14f")
    (cons 40 "#688e35")
    (cons 60 "#807b1b")
    (cons 80 "#986801")
    (cons 100 "#ae7118")
    (cons 120 "#c37b30")
    (cons 140 "#da8548")
    (cons 160 "#c86566")
    (cons 180 "#b74585")
    (cons 200 "#a626a4")
    (cons 220 "#ba3685")
    (cons 240 "#cf4667")
    (cons 260 "#e45649")
    (cons 280 "#d2685f")
    (cons 300 "#c07b76")
    (cons 320 "#ae8d8d")
    (cons 340 "#383a42")
    (cons 360 "#383a42")))
 '(vc-annotate-very-old-color nil)
 '(wakatime-api-key "dadfb163-294d-4248-83db-358ac307867e")
 '(wakatime-cli-path "wakatime")
 '(wakatime-python-bin nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
