
(setq org-todo-keywords
       '((sequence "TODO"
                   "WORKFLOW" "FIX" "FANCY" "RESEARCH"
                   "IDEA" "MOVE" "INFO" "BLOG" "POST" "JACK"
                   "OPENSOURCE"
                   "STORYX"
                   "PROTOTYPE"
                   "TRIAGE"
                   "|"               ;; <------- more likely to be typed
                   "TODOLOL"         ;; at the center
                   "INACTIVE"
                   "REMINDER"
                   "SCHEDULED"
                   "BLOCKED"         ;; closer to 0 == quicker scrolling
                   "DONE"            ;; <------- closer for Shift-<left> wrapping
        ))
)

(setq org-src-fontify-natively t)

(defun my/tangle-dotfiles ()
  "If the current file is in '~/.dotfiles', the code blocks are tangled"
  (when (equal (file-name-directory (directory-file-name buffer-file-name))
               (concat (getenv "HOME") "/dotfiles/emacs.d/"))
    (org-babel-tangle)
    (message "%s tangled" buffer-file-name)))

(add-hook 'after-save-hook #'my/tangle-dotfiles)

(setq org-directory "~/Dropbox/todo/")
(setq org-mobile-inbox-for-pull "~/Dropbox/todo/inbox.org")
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
(setq org-mobile-files '("~/Dropbox/todo"))

;;(add-hook 'after-init-hook 'org-mobile-pull)
(add-hook 'kill-emacs-hook 'org-mobile-push)

(setq org-default-notes-file (concat org-directory "/captured.org"))
     (define-key global-map "\C-cc" 'org-capture)

(setq org-capture-templates
      '(("t" "Todo (any)" entry (file+headline "~/Dropbox/todo/todo.org" "Captured")
             "* TODO %?\n  Captured: %U\n  %a")
        ("w" "Workflow todo" entry (file+headline "~/Dropbox/todo/todo.org" "Workflow")
             "** WORKFLOW %?\n  Captured: %U\n  %a")
        ("c" "Chore todo" entry (file+headline "~/Dropbox/todo/todo.org" "Chore")
             "** CHORE %?\n  Captured: %U\n  %a")
        ("o" "Open Source idea" entry (file+headline "~/Dropbox/todo/todo.org" "Open Source")
             "** OPENSOURCE %?\n  Captured: %U\n  %a")
        ("r" "Read/Research" entry (file+headline "~/Dropbox/todo/todo.org" "Read")
             "** READ %?\n  Captured: %U\n  %a")
        ("u" "Urbint" entry (file+headline "~/Dropbox/todo/todo.org" "Urbint")
             "** URBINT %?\n  Captured: %U\n  %a")
        ("g" "Triage" entry (file+headline "~/Dropbox/todo/todo.org" "Triage")
             "** TRIAGE %?\n  Captured: %U\n  %a")
        ("b" "Blog" entry (file+headline "~/Dropbox/todo/todo.org" "Blog")
             "** BLOG %?\n  Captured: %U\n  %a")
      )
)

(setq org-log-done 'time)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

;; Install 'use-package' if necessary
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Enable use-package
(eval-when-compile
  (require 'use-package))

;; Always download missing use-package packages
(setq use-package-always-ensure t)

(load-theme 'atom-one-dark t)

(setq inhibit-startup-screen t)
(find-file "~/dotfiles/emacs.d/init.org")
(split-window-right)
(find-file-other-window "~/Dropbox/todo/todo.org")

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(global-auto-revert-mode t)

(use-package highlight-indent-guides
  :config
  (setq highlight-indent-guides-method 'character)
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
)

(setq-default show-trailing-whitespace t)

(global-hl-line-mode 1)

(setq visible-bell nil)
;; (setq visible-bell 1)

(use-package zoom-frm
  :config
  (global-set-key (kbd "s-=") 'zoom-frm-in)
  (global-set-key (kbd "s--") 'zoom-frm-out)
  (global-set-key (kbd "s-0") 'zoom-frm-unzoom)
)

;; auto-save-files not in same dir as original
(setq backup-directory-alist `(("." . "~/.emacs/auto-save-list")))

(setq ns-auto-hide-menu-bar t)

(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq-default indent-tabs-mode nil)

(set-frame-font "Operator Mono 12")

(use-package seethru
  :config
  (global-set-key (kbd "s-+") (lambda () (interactive) (seethru-relative 5)))
  (global-set-key (kbd "s-_") (lambda () (interactive) (seethru-relative -5)))
  (global-set-key (kbd "s-)") (lambda () (interactive) (seethru 100)))
  (global-set-key (kbd "s-(") (lambda () (interactive) (seethru 0)))
)

(setq ns-use-native-fullscreen nil)
(global-set-key (kbd "<s-return>") 'toggle-frame-fullscreen)

;; default full screen
(setq default-frame-alist
    '((fullscreen . fullboth) (fullscreen-restore . fullheight)))

;; Scrolling Settings
(setq scroll-step 1)
(setq scroll-conservatively 10000)

;; line numbers
(global-linum-mode t)

;; line wrap
(setq-default word-wrap t)
;; (toggle-truncate-lines 1)

(use-package evil
  :commands (evil-mode local-evil-mode)
  :bind (:map evil-motion-state-map
         ("<return>" . nil)
         ("<tab>" . nil)
         ("SPC" . nil)
         ("M-." . nil)
         ("*" . helm-swoop)
         ("(" . backward-sexp)
         (")" . forward-sexp)
         ("K" . nil)

         :map evil-normal-state-map
         ("<return>" . nil)
         ("<tab>" . nil)
         ("M-." . nil)
         ("*" . helm-swoop)
         ("C-p" . helm-projectile)
         ("K" . nil)

         :map evil-visual-state-map
         ("g c" . evilnc-comment-or-uncomment-lines)

         :map evil-ex-map
         ("e" . helm-find-files)
         ("b" . helm-buffers-list)
         ("tb" . alchemist-mix-test-this-buffer)
         ("tap" . alchemist-mix-test-at-point)
         ("lt" . alchemist-mix-rerun-last-test)
        )

  :init
  (progn
    (setq evil-default-cursor t)
    (setq evil-shift-width 2)

    (use-package evil-leader
      :init (global-evil-leader-mode)

      :config
      (progn
        (setq evil-leader/in-all-states t)

        (evil-leader/set-leader "<SPC>")

        (evil-leader/set-key
         "<SPC>" 'evil-switch-to-windows-last-buffer
         "c" 'evilnc-comment-or-uncomment-lines
         "n" 'neotree-find
         "N" 'neotree-find
         "w" 'save-buffer
         "W" 'delete-trailing-whitespace
         "k" 'kill-buffer
         "b" 'helm-mini
         "p" 'helm-mini
         "S" 'helm-projectile-ag
         "s" 'split-window-below
         "-" 'split-window-below
         "_" 'split-window-below
         "v" 'split-window-right
         "\\" 'split-window-right
         "|" 'split-window-right
         "x" 'alchemist-mix
         "r" 'alchemist-mix-rerun-last-test
         "l" 'alchemist-mix-rerun-last-test
         "t" 'alchemist-project-toggle-file-and-tests
         "T" 'alchemist-mix-test-this-buffer
         "q" 'evil-window-delete
         "=" 'balance-windows
         "a" 'ace-window
         ">" 'evil-window-increase-width
         "<" 'evil-window-decrease-width
         )))

    (evil-mode 1))

  :config
  (progn

    ;; esc should always quit: http://stackoverflow.com/a/10166400/61435
    (define-key evil-normal-state-map [escape] 'keyboard-quit)
    (define-key evil-visual-state-map [escape] 'keyboard-quit)
    (define-key minibuffer-local-map [escape] 'abort-recursive-edit)
    (define-key minibuffer-local-ns-map [escape] 'abort-recursive-edit)
    (define-key minibuffer-local-completion-map [escape] 'abort-recursive-edit)
    (define-key minibuffer-local-must-match-map [escape] 'abort-recursive-edit)
    (define-key minibuffer-local-isearch-map [escape] 'abort-recursive-edit)


    (with-eval-after-load 'evil
        (defalias #'forward-evil-word #'forward-evil-symbol))
  )

)

(use-package evil-surround
  :config
  (global-evil-surround-mode 1)
)

;; Window movement
(global-set-key (kbd "C-l") 'windmove-right)
(global-set-key (kbd "C-h") 'windmove-left)
(global-set-key (kbd "C-k") 'windmove-up)
(global-set-key (kbd "C-j") 'windmove-down)

(defadvice split-window-below (after restore-balanace-below activate)
  (balance-windows))

(defadvice split-window-right (after restore-balance-right activate)
  (balance-windows))

(defadvice delete-window (after restore-balance activate)
  (balance-windows))

(use-package ace-window
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
)

(use-package popwin
  :config

  (add-to-list 'popwin:special-display-config '("^\\*helm.*\\*$" :regexp t))

  (defun helm-popwin-help-mode-off ()
    "Turn `popwin-mode' off for *Help* buffers."
    (when (boundp 'popwin:special-display-config)
      (popwin:display-buffer helm-buffer t)
      (customize-set-variable 'popwin:special-display-config
                              (delq 'help-mode popwin:special-display-config))))

  (defun helm-popwin-help-mode-on ()
    "Turn `popwin-mode' on for *Help* buffers."
    (when (boundp 'popwin:special-display-config)
      (customize-set-variable 'popwin:special-display-config
                              (add-to-list 'popwin:special-display-config 'help-mode nil #'eq))))

  (add-hook 'helm-after-initialize-hook #'helm-popwin-help-mode-off)
  (add-hook 'helm-cleanup-hook #'helm-popwin-help-mode-on)

  (push '("^\\*helm.*\\*$" :regexp t :height 50) popwin:special-display-config)

)

;; (use-package golden-ratio
;;   :config
;;     (golden-ratio-mode 1)
;;     (setq golden-ratio-auto-scale nil)
;;     (setq golden-ratio-adjust-factor .5
;;       golden-ratio-wide-adjust-factor .9)
;; )

(use-package smart-mode-line
  :config
  (setq sml/no-confirm-load-theme t)
  (sml/setup)
)

(use-package helm
  :bind (
    ("M-x" . helm-M-x)
    ("C-x C-f" . helm-find-files)
    ("C-x f" . helm-projectile)
    ("M-y" . helm-show-kill-ring)
    ("C-x b" . helm-mini)
    ("C-x C-b" . helm-buffers-list)

    :map helm-map
    ([backtab] . helm-previous-source)
    ([tab] . helm-next-source)
    ("C-j" . helm-next-line)
    ("C-k" . helm-previous-line)
    ("C-?" . describe-key)
    ([escape] . helm-keyboard-quit)

    :map helm-find-files-map
    ("C-l" . helm-execute-persistent-action)
    ("C-h" . helm-find-files-up-one-level)
    ("C-?" . describe-key)

    :map helm-read-file-map
    ("C-l" . helm-execute-persistent-action)
    ("C-h" . helm-find-files-up-one-level)
    ("C-?" . describe-key)
  )

  :init (helm-mode 1)

  :config
  (progn
    (setq helm-buffers-fuzzy-matching t helm-recentf-fuzzy-match t)

    (setq helm-semantic-fuzzy-match t helm-imenu-fuzzy-match t)

    (setq helm-locate-fuzzy-match t)

    (add-to-list 'helm-mini-default-sources
      (helm-build-sync-source "Org Files"
        :action 'helm-type-file-actions
        :candidates '(
          "~/dotfiles/emacs.d/init.org"
          "~/Dropbox/todo/todo.org"
          "~/Dropbox/todo/notes.org"
          "~/Dropbox/Writing/writing-february-2017.org"
          "~/Dropbox/Writing/triage.org"
        )
      )
      'append)

    (use-package helm-projectile
      :config
      (progn
        (helm-projectile-on))
    )

    (setq helm-boring-buffer-regexp-list
      (quote ( "\\Minibuf.+\\*"
               "\\` "
               "\\*.+\\*"
             )
      )
    )

    ;; TODO: doesn't work for un'opened' files, only existing buffers
    (defun helm-buffer-switch-to-new-window (_candidate)
      "Display buffers in new windows."
      ;; Select the bottom right window
      (require 'winner)
      (select-window (car (last (winner-sorted-window-list))))
      ;; Display buffers in new windows
      (dolist (buf (helm-marked-candidates))
        (select-window (split-window-right))
        (switch-to-buffer buf))
      ;; Adjust size of windows
      (balance-windows))

    (add-to-list 'helm-type-buffer-actions
                '("Display buffer(s) in new window(s) `M-o'" .
                  helm-buffer-switch-new-window) 'append)

    (defun helm-buffer-switch-new-window ()
      (interactive)
      (with-helm-alive-p
        (helm-quit-and-execute-action 'helm-buffer-switch-to-new-window)))

    (define-key helm-map (kbd "M-o") #'helm-buffer-switch-new-window)

  )
)

(use-package avy)

(use-package ag)

(use-package helm-ag)

(use-package alchemist
  :config
    (setq alchemist-goto-elixir-source-dir "/usr/local/share/src/elixir")
    (setq alchemist-goto-erlang-source-dir "/usr/local/share/src/otp")

    (setq alchemist-test-display-compilation-output t)
    ;;(setq alchemist-hooks-test-on-save t)
    (setq alchemist-hooks-compile-on-save t)

    ;; fix to return from erlang dives
    (defun custom-erlang-mode-hook ()
        "Jump to and from Elixir, Erlang, Elixir files."
        (define-key erlang-mode-map (kbd "M-,") 'alchemist-goto-jump-back))
    (add-hook 'erlang-mode-hook 'custom-erlang-mode-hook)
)

(add-to-list 'display-buffer-alist
             `(,(rx bos (or "*alchemist test report*"
                            "*alchemist mix*"
                            "*alchemist help*"))
                    (display-buffer-reuse-window)
                    (inhibit-switch-frame t)
                    (reusable-frames . visible)))

(use-package company
  :bind ("<tab>" . company-complete-common)
        ("<escape>" . company-abort)

  :config
  (setq company-show-numbers t)
  (setq company-idle-delay 0)
  (setq company-selection-wrap-around t)

  (dotimes (i 10)
    (define-key company-active-map (kbd (format "C-%d" i)) 'company-complete-number))

  (define-key company-active-map (kbd "<tab>") 'company-complete-selection)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-j") 'company-select-next)
  (define-key company-active-map (kbd "C-k") 'company-select-previous)

  (global-company-mode)
)

(use-package flycheck
  :config
  (global-flycheck-mode)

  ; Flycheck Mix Settings
  (use-package flycheck-mix
    :init
    (flycheck-mix-setup))

  ;; Flycheck Credo Settings
  (use-package flycheck-credo
    :init
    (flycheck-credo-setup))
  )

(use-package magit
  :init (progn)
  :config (progn (use-package evil-magit))
)

(use-package neotree
  :config
  (progn

    ;; theme
    (use-package all-the-icons
      ;; install fonts from this package too
    )
    (setq neo-theme (if (display-graphic-p) 'icons 'arrow))

    ;; evil mappings
    (evil-set-initial-state 'neotree-mode 'normal)

    (evil-define-key 'normal neotree-mode-map
    (kbd "RET") 'neotree-enter
    ;;(kbd "TAB") 'neotree-enter
    (kbd "c")   'neotree-create-node
    (kbd "r")   'neotree-rename-node
    (kbd "d")   'neotree-delete-node
    (kbd "j")   'neotree-next-line
    (kbd "k")   'neotree-previous-line
    (kbd "R")   'neotree-refresh
    (kbd "C")   'neotree-change-root
    (kbd "H")   'neotree-hidden-file-toggle
    (kbd "q")   'neotree-hide
    (kbd "s")   'neotree-enter-horizontal-split
    (kbd "v")   'neotree-enter-vertical-split
    ))

    ;; neo vc integration
    (setq neo-vc-integration '(face char))

    ;; Patch to fix vc integration
    (defun neo-vc-for-node (node)
    (let* ((backend (vc-backend node))
      (vc-state (when backend (vc-state node backend))))
      ;; (message "%s %s %s" node backend vc-state)
      (cons (cdr (assoc vc-state neo-vc-state-char-alist))
        (cl-case vc-state
          (up-to-date       neo-vc-up-to-date-face)
          (edited           neo-vc-edited-face)
          (needs-update     neo-vc-needs-update-face)
          (needs-merge      neo-vc-needs-merge-face)
          (unlocked-changes neo-vc-unlocked-changes-face)
          (added            neo-vc-added-face)
          (removed          neo-vc-removed-face)
          (conflict         neo-vc-conflict-face)
          (missing          neo-vc-missing-face)
          (ignored          neo-vc-ignored-face)
          (unregistered     neo-vc-unregistered-face)
          (user             neo-vc-user-face)
          (t                neo-vc-default-face)
        )
      )
    )
  )
)

(use-package projectile
  :config
  (progn
    (setq projectile-switch-project-action 'projectile-find-file)
    (projectile-mode)
  )
)

(use-package helm-swoop
  :bind (
    :map helm-swoop-map
    ([tab] . helm-next-line)
    ([backtab] . helm-previous-line)
  )

  :config
    ;; If this value is t, split window inside the current window
    (setq helm-swoop-split-with-multiple-windows nil)

    ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
    (setq helm-swoop-split-direction 'split-window-vertically)

    ;; If there is no symbol at the cursor, use the last used words instead.
    (setq helm-swoop-pre-input-function
      (lambda ()
        (let (($pre-input (thing-at-point 'symbol)))
          (if (eq (length $pre-input) 0)
              helm-swoop-pattern ;; this variable keeps the last used words
            $pre-input))))
)

(use-package iedit)

(use-package evil-nerd-commenter)
