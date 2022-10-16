;;; completion/corfu/config.el -*- lexical-binding: t; -*-

;; Corfu completion module


;; Reset lsp-completion provider
(add-hook 'doom-init-modules-hook
          (lambda ()
            (after! lsp-mode
              (setq lsp-completion-provider :none))))

;; Pad before lsp modeline error info
(add-hook 'lsp-mode-hook
          (lambda ()
            (setf (caadr
                   (assq 'global-mode-string mode-line-misc-info))
                  " ")))

;; Set orderless filtering for LSP-mode completions
(add-hook 'lsp-completion-mode-hook
          (lambda ()
            (setf (alist-get 'lsp-capf completion-category-defaults) '((styles . (orderless))))))


;; from corfu readme
(defun corfu-move-to-minibuffer ()
  (interactive)
  (let ((completion-extra-properties corfu--extra)
        completion-cycle-threshold completion-cycling)
    (apply #'consult-completion-in-region completion-in-region--data)))


;; Set bindings
(map! :i "C-@" #'completion-at-point
      :i "C-SPC" #'completion-at-point
      (:prefix "C-x"
       :i "C-k" #'cape-dict
       :i "C-f" #'cape-file
       :i "s" #'cape-ispell
       :i "C-n" #'cape-keyword
       :i "C-s" #'dabbrev-completion))

;; Fallback cleanly to consult in TUI
(setq-default completion-in-region-function #'consult-completion-in-region)

(use-package corfu
  :custom
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-auto nil)
  (corfu-on-exact-match nil)
  (corfu-quit-no-match 'separator)
  (corfu-preselect-first nil)
  (corfu-min-width 40)
  (corfu-max-width 60)
  :hook
  (doom-first-buffer . global-corfu-mode)
  :bind (:map corfu-map
         ("SPC" . corfu-insert-separator)
         ("TAB" . corfu-next)
         ([tab] . corfu-next)
         ("S-TAB" . corfu-previous)
         ([backtab] . corfu-previous)
         ("C-l" . corfu-insert)
         ("C-h" . corfu-move-to-minibuffer)))

(use-package corfu-doc
  :hook
  (corfu-mode . corfu-doc-mode)
  :bind (:map corfu-map
         ("M-n" . corfu-doc-scroll-down)
         ("M-p" . corfu-doc-scroll-up)
         ("M-d" . corfu-doc-toggle)))

(use-package orderless
  :when (modulep! +orderless)
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package cape
  :defer t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file-capf)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev-capf)
  (add-to-list 'completion-at-point-functions #'cape-keyword-capf))

(setq completion-cycle-threshold 1)

;; Enable indentation+completion using the TAB key.
;; Completion is often bound to M-TAB.
(setq tab-always-indent 'complete)

;; Dirty hack to get c completion running
;; Discussion in https://github.com/minad/corfu/issues/34
(when (equal tab-always-indent 'complete)
  (map! :map c-mode-base-map
        :i [remap c-indent-line-or-region] #'completion-at-point))


;; https://discourse.doomemacs.org/t/new-completion-corfu-module/2685/12
(defadvice! +corfu--org-return (orig) :around '+org/return
  (if (and (modulep! :completion corfu) ;; omit if not using via a module
           corfu-mode
           (>= corfu--index 0)) ;; translates to "there are candidates to select"
      (corfu-insert)
    (funcall orig)))

(use-package! corfu-history
  :after corfu
  :hook (corfu-mode . (lambda ()
                        (corfu-history-mode 1)
                        (savehist-mode 1)
                        (add-to-list 'savehist-additional-variables 'corfu-history))))


(use-package! corfu-quick
  :after corfu
  :bind (:map corfu-map
         ("M-q" . corfu-quick-complete)
         ("C-q" . corfu-quick-insert)))
