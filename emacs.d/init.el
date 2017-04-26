
(setq org-src-fontify-natively t)

;;(org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((lisp . t))
;;)

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
;;(add-hook 'kill-emacs-hook 'org-mobile-push)

(setq org-default-notes-file (concat org-directory "/captured.org"))
     (define-key global-map "\C-cc" 'org-capture)

(setq org-capture-templates
      '(("t" "Todo (any)" entry (file+headline "~/Dropbox/todo/todo.org" "Captured")
             "* TODO %?\n  Captured: %U\n  %a")
        ("r" "Read/Research" entry (file+headline "~/Dropbox/todo/todo.org" "Read")
             "* READ %?\n  Captured: %U\n  %a")
      )
)

(setq org-log-done 'time)

(setq org-agenda-files
      '("todo.org" "done.org" "read.org"))

(setq org-refile-use-outline-path 'file)

;; (eval-after-load 'org
;;   (progn
;;     (global-set-key (kbd "C-k") 'windmove-up)
;;     (global-set-key (kbd "C-j") 'windmove-down)))

(fset 'yes-or-no-p 'y-or-n-p)

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

;; Upgrade all packages
(defun package-upgrade-all ()
  "Upgrade all packages automatically without showing *Packages* buffer."
  (interactive)
  (package-refresh-contents)
  (let (upgrades)
    (cl-flet ((get-version (name where)
                (let ((pkg (cadr (assq name where))))
                  (when pkg
                    (package-desc-version pkg)))))
      (dolist (package (mapcar #'car package-alist))
        (let ((in-archive (get-version package package-archive-contents)))
          (when (and in-archive
                     (version-list-< (get-version package package-alist)
                                     in-archive))
            (push (cadr (assq package package-archive-contents))
                  upgrades)))))
    (if upgrades
        (when (yes-or-no-p
               (message "Upgrade %d package%s (%s)? "
                        (length upgrades)
                        (if (= (length upgrades) 1) "" "s")
                        (mapconcat #'package-desc-full-name upgrades ", ")))
          (save-window-excursion
            (dolist (package-desc upgrades)
              (let ((old-package (cadr (assq (package-desc-name package-desc)
                                             package-alist))))
                (package-install package-desc)
                (package-delete  old-package)))))
      (message "All packages are up to date"))))

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

(column-number-mode)
(show-paren-mode)
(eldoc-mode)

;; (use-package evil-visual-mark-mode)

(use-package evil
  :commands (evil-mode local-evil-mode)
  :bind (:map evil-motion-state-map
         ("<tab>" . evil-indent-line)
         ("<return>" . nil)
         ("SPC" . nil)
         ("M-." . nil)
         ("(" . backward-sexp)
         (")" . forward-sexp)
         ("K" . nil)

         :map evil-normal-state-map
         ("S-<tab>" . org-cycle)
         ("<tab>" . evil-indent-line)
         ("<return>" . nil)
         ("M-." . nil)
         ("K" . nil)

         :map evil-visual-state-map
         ("g c" . evilnc-comment-or-uncomment-lines)

         :map evil-ex-map
         ("e" . helm-find-files)
         ("tn" . neotree-toggle)
         ("tap" . alchemist-mix-test-at-point)
         ("tl" . toggle-truncate-lines)
         ("lt" . alchemist-mix-rerun-last-test)
         ("ag" . helm-projectile-ag)
         ("Ag" . helm-projectile-ag)
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
      "<SPC>" 'evil-switch-to-windows-last-buffer ;; TODO this command doesn't toggle properly after helm-semantic-or-imenu
      "a" 'ace-window
      "b" 'helm-mini
      "c" 'evilnc-comment-or-uncomment-lines
      "e" 'helm-M-x
      "f" 'helm-find-files
      "i" 'popup-imenu
      "I" 'helm-imenu-anywhere
      "k" 'kill-buffer
      "l" 'alchemist-mix-rerun-last-test
      "n" 'neotree-find-current-file
      "N" 'neotree-reveal-current-buffer
      "o" 'projectile-multi-occur
      "p" 'helm-projectile
      "qn" 'neotree-toggle
      "qq" 'evil-window-delete
      "r" 'org-ctrl-c-ctrl-c
      "S" 'helm-projectile-ag
      "s" 'split-window-below
      "t" 'alchemist-project-toggle-file-and-tests
      "T" 'alchemist-mix-test-this-buffer
      "v" 'split-window-right
      "w" 'save-buffer
      "Wl" '(lambda () (interactive) (evil-window-move-far-right))
      "WL" '(lambda () (interactive) (evil-window-move-far-right))
      "Wh" '(lambda () (interactive) (evil-window-move-far-left))
      "WH" '(lambda () (interactive) (evil-window-move-far-left))
      "x" 'helm-M-x
      "=" 'balance-windows
      "-" 'split-window-below
      "_" 'split-window-below
      "\\" 'split-window-right
      "|" 'split-window-right
      ">" '(lambda () (interactive) (evil-window-increase-width 20))
      "<" '(lambda () (interactive) (evil-window-decrease-width 20))
    )
  )
)

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

;; (load-theme 'atom-one-dark)
(use-package doom-themes
  :init
  ;;; Settings (defaults)
  (setq doom-enable-bold t
      doom-enable-italic t

      ;; doom-one specific settings
      doom-one-brighter-comments t
  )

  (setq org-fontify-whole-heading-line t
      org-fontify-done-headline t
      org-fontify-quote-and-verse-blocks t)

  ;; brighter source buffers (that represent files)
  (add-hook 'find-file-hook 'doom-buffer-mode-maybe)
  ;; if you use auto-revert-mode
  (add-hook 'after-revert-hook 'doom-buffer-mode-maybe)
  ;; you can brighten other buffers (unconditionally) with:
  (add-hook 'ediff-prepare-buffer-hook 'doom-buffer-mode)

  ;; brighter minibuffer when active
  (add-hook 'minibuffer-setup-hook 'doom-brighten-minibuffer)

  :config
  (load-theme 'doom-one t)

  ;; Enable custom neotree theme
  (require 'doom-neotree)
  ;; Enable nlinum line highlighting
  (require 'doom-nlinum)
)

(use-package smart-mode-line
  :config
  (setq sml/no-confirm-load-theme t)
  (sml/setup)
)

(use-package helm
  :bind (
    ("M-x" . helm-M-x)
    ("M-y" . helm-show-kill-ring)
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
    (setq helm-buffers-fuzzy-matching t
          helm-recentf-fuzzy-match t
          helm-semantic-fuzzy-match t
          helm-imenu-fuzzy-match t
          helm-locate-fuzzy-match t
          helm-M-x-fuzzy-match t)

    (defvar helm-source-emacs-commands
      (helm-build-sync-source "Emacs commands"
        :candidates (lambda ()
                      (let ((cmds))
                        (mapatoms
                        (lambda (elt) (when (commandp elt) (push elt cmds))))
                        cmds))
        :coerce #'intern-soft
        :action #'command-execute)
      "A simple helm source for Emacs commands.")

    (defvar helm-source-emacs-commands-history
      (helm-build-sync-source "Emacs commands history"
        :candidates (lambda ()
                      (let ((cmds))
                        (dolist (elem extended-command-history)
                          (push (intern elem) cmds))
                        cmds))
        :coerce #'intern-soft
        :action #'command-execute)
      "Emacs commands history")

    (defvar helm-source-my-org-files
      (helm-build-sync-source "Org Files"
        :action 'helm-type-file-actions
        :candidates '(
          "~/dotfiles/emacs.d/init.org"
          "~/Dropbox/todo/todo.org"
          "~/Dropbox/todo/notes.org"
          "~/Dropbox/Writing/writing-march-2017.org"
          "~/Dropbox/Writing/triage.org"
          "~/Dropbox/todo/blog.org"
          "~/Dropbox/todo/storyx.org"
          "~/Dropbox/todo/opensource.org"
          "~/Dropbox/todo/urbint.org"
        )
      )
    )

    (use-package helm-ls-git)

    (setq helm-mini-default-sources '(helm-source-buffers-list
                                      helm-source-recentf
                                      helm-source-ls-git-status
                                      helm-source-projectile-projects
                                      helm-source-my-org-files
                                      helm-source-emacs-commands-history
                                      helm-source-emacs-commands
                                      helm-source-buffer-not-found))

    (use-package helm-projectile
      :config
      (progn
        (helm-projectile-on)

        (setq helm-projectile-sources-list
          '(helm-source-projectile-buffers-list
            helm-source-projectile-recentf-list
            helm-source-projectile-files-list
            helm-source-projectile-projects
            helm-source-my-org-files
           )
        )
      )
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

(use-package helm-ag
  :config
  (custom-set-variables
  '(helm-ag-ignore-patterns '(".*//doc//.*'"))
  )
)

(use-package alchemist
  :config
    (setq alchemist-goto-elixir-source-dir "/usr/local/share/src/elixir")
    (setq alchemist-goto-erlang-source-dir "/usr/local/share/src/otp")

    (setq alchemist-test-display-compilation-output t)
    ;;(setq alchemist-hooks-test-on-save t)
    ;;(setq alchemist-hooks-compile-on-save t)

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
  :bind ("<escape>" . company-abort)

  :config
  (setq company-show-numbers t)
  (setq company-idle-delay 0)
  (setq company-selection-wrap-around t)

  (dotimes (i 10)
    (define-key company-active-map (kbd (format "C-%d" i)) 'company-complete-number))

  (define-key company-active-map (kbd "<tab>") 'evil-indent-line)
  (define-key company-active-map (kbd "C-h") 'evil-indent-line)
  (define-key company-active-map (kbd "C-l") 'company-complete-selection)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-j") 'company-select-next)
  (define-key company-active-map (kbd "C-k") 'company-select-previous)

  (add-hook 'after-init-hook 'global-company-mode)
)

(use-package flycheck
  :config
  (global-flycheck-mode)

  (require 'flycheck-mix)
  (add-to-list 'flycheck-checkers 'elixir-mix t)

  (require 'flycheck-credo)
  (setq flycheck-elixir-credo-strict t)
  (add-to-list 'flycheck-checkers 'elixir-credo t)

  (flycheck-add-next-checker 'elixir-mix '(error . elixir-credo))
)

(use-package magit
  :init (progn)
  :config (progn (use-package evil-magit))
)

(use-package neotree
  :init
  (setq neo-smart-open t)

  :config
  (progn

    (setq-default neo-show-hidden-files t)
    (setq-default neo-window-fixed-size nil)
    (setq-default neo-window-width 40)

    (defun neotree-find-current-file ()
      "Reveal current buffer in Neotree."
      (interactive)
      (let ((project-dir (projectile-project-root))
            (file-name (buffer-file-name)))

        (neotree-show)
        (if project-dir
            (if (neo-global--window-exists-p)
                (progn
                  (neotree-dir project-dir)
                  (neotree-find file-name))))
      (message "Could not find git project root.")))

    (defun neotree-reveal-current-buffer ()
      "Reveal current buffer in Neotree."
      (interactive)
      (let ((project-dir (projectile-project-root))
            (file-name (buffer-file-name)))

        (neotree-show)
        (if project-dir
            (if (neo-global--window-exists-p)
                (progn
                  (neotree-dir project-dir)
                  (neotree-find file-name)
                  (evil-window-mru)))
      (message "Could not find git project root."))))


    ;; theme
    (use-package all-the-icons
      ;; install fonts from this package too
    )
    (setq neo-theme (if (display-graphic-p) 'icons 'arrow))


    (defun helm-ag-neotree-node ()
      "Run Helm-ag on Neotree directory."
      (interactive)
      (let* ((search-root (neo-buffer--get-filename-current-line)))
        (if search-root
            ;; search directory
            (progn
              (evil-window-right 1)
              (helm-ag search-root))
          (message "Could not find directory at point."))))

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
    (kbd "p")   'helm-ag-neotree-node
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

(use-package slime
  :config
  (add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
  (add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
  (setq inferior-lisp-program "clisp")
)

(use-package popup-imenu
  :config (define-key popup-isearch-keymap (kbd "<escape>") 'popup-isearch-cancel)
)

(use-package imenu-anywhere)

;; (ido-mode 1)
;; (setq ido-everywhere t)
;; (setq ido-enable-flex-matching t)

(setq recentf-max-saved-items 50)
(run-at-time (current-time) 300 'recentf-save-list)

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

;; (use-package desktop+
;;   :config
;;   (global-define-key (kbd "M-s") 'desktop+-load))

(setq js-indent-level 2)

(setq css-indent-offset 2)
