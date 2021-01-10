;;; ~/dotfiles/emacs/.doom.d/+clojure.el -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'dash)


(defmacro comment (&rest _)
  "Comment out one or more s-expressions."
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; systemic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Fix docstring highlighting for `defsys`
(put 'defsys 'clojure-doc-string-elt 2)

;; The below functions allow you to control systemic from Emacs.
;; Personally, I have found binding them to keys to be very convenient.
(defun systemic/restart ()
  "Restarts all systemic systems"
  (interactive)
  (cider-interactive-eval "(systemic.core/restart!)"))

(defun systemic/start ()
  "Starts all systemic systems"
  (interactive)
  (cider-interactive-eval "(systemic.core/start!)"))

(defun systemic/stop ()
  "Stops all systemic systems"
  (interactive)
  (cider-interactive-eval "(systemic.core/stop!)"))

(defun wing-sync-libs ()
  (interactive)
  (cider-interactive-eval "(wing.repl/sync-libs)"))

(defun cider-user-go ()
  (interactive)
  (cider-interactive-eval "(user/go)"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cider company bindings fix
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/company-mode/company-quickhelp/issues/17
;; https://github.com/hlissner/doom-emacs/issues/2610

(let* ((bindings
        '("C-s" company-filter-candidates
          "C-h" company-show-doc-buffer
          "C-j" company-select-next
          "C-k" company-select-previous
          "C-l" company-complete-selection
          "<down>" company-select-next
          "<up>" company-select-previous
          "<right>" company-complete-selection
          "RET" company-complete-selection
          "C-p" company-other-backend
          "C-n" company-other-backend
          "TAB" company-complete-common-or-cycle
          ))
       (unset-bindings (mapcar (lambda (value)
                                 (if (stringp value)
                                     value
                                   nil))
                               bindings)))

  (defun custom/unset-company-bindings (&rest _args)
    (apply 'general-define-key
           :keymaps 'override
           :states  'insert
           unset-bindings))

  (defun custom/set-company-bindings (&rest _args)
    (apply 'general-define-key
           :keymaps 'override
           :states  'insert
           bindings)))

(after! cider
  (add-hook 'company-completion-started-hook 'custom/set-company-bindings)
  (add-hook 'company-completion-finished-hook 'custom/unset-company-bindings)
  (add-hook 'company-completion-cancelled-hook 'custom/unset-company-bindings))

;; fix company box in cider
(after! company-box
  (add-function
   :after
   (symbol-function 'company-box-doc--show)
   (lambda (_ frame)
     (let* ((doc-frame (frame-parameter frame 'company-box-doc-frame)))
       (when (frame-visible-p doc-frame)
         (make-frame-visible (company-box--get-frame)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cider helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun russ/switch-cider-connection ()
  "Buries the current cider repl buffer, promoting other cider conns."
  (interactive)
  (bury-buffer (cider-current-repl))
  (cider-switch-to-repl-buffer)
  (cider-switch-to-last-clojure-buffer)
  (cider-refresh-dynamic-font-lock))

(defun rs/cider-cycle-buffer-type ()
  "Cycles between clojure, clojurescript, and clojurec repl types"
  (interactive)
  (if (string= "cljc" (file-name-extension (buffer-file-name)))
      (let ((repl-open? (get-buffer-window (cider-current-repl-buffer)))
            (window (selected-window)))
        (setq clojure-verify-major-mode nil)
        (cond
         ((eq 'clojurec-mode major-mode) (clojure-mode))
         ((eq 'clojure-mode major-mode) (clojurescript-mode))
         ((eq 'clojurescript-mode major-mode) (clojurec-mode)))
        (when (and repl-open? (not (eq 'clojurec-mode major-mode)))
          (cider-switch-to-repl-buffer)
          (select-window window)))
    (message (concat "Cycle repl type called from non .cljc file" (buffer-file-name)))))

(defun rs/cider-clear-all-buffers ()
  "Clear all cider buffers"
  (interactive)
  (let ((inhibit-read-only 't))
    (dolist (repl (cider-repls))
      (with-current-buffer repl
        (cider-repl--clear-region (point-min) cider-repl-prompt-start-mark)
        (cider-repl--clear-region cider-repl-output-start cider-repl-output-end)
        (when (< (point) cider-repl-input-start-mark)
          (goto-char cider-repl-input-start-mark))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cider bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(map!
 (:after cider-mode
   (:leader
     :desc "Lookup documentation at point" :n  "d"  #'cider-doc
     :desc "Jump to definition at point"   :n  "l"  #'cider-find-var)
   (:map cider-mode-map
     (:leader
       :n "DEL" #'ivy-cider-browse-ns
       :n "\\" #'ivy-cider-apropos
       (:desc "Cider" :prefix "c"
         :n  "'"  #'cider-jack-in
         :n  "\"" #'cider-jack-in-cljs

         :n  "l"  #'cider-load-this-file
         :n  "b"  #'cider-eval-buffer

         :n  "B"  #'cider-switch-to-repl-buffer
         :n  "n"  #'cider-repl-set-ns
         :n  "j"  #'cider-find-var
         (:desc "docs" :prefix "d"
           :desc "Browse Namespace" :n  "n" #'cider-browse-ns
           :desc "Browse Spec"      :n  "s" #'cider-browse-spec
           :desc "Essential Ref"    :n  "f" #'clojure-essential-ref)
         :n  "h"  #'cider-doc
         :n  "c"  #'rs/cider-clear-all-buffers
         :n  "i"  #'cider-inspect-last-result
         :n  "p"  #'cider-eval-sexp-at-point
         :n  "f"  #'cider-eval-defun-at-point
         :n  "t"  #'cider-test-run-ns-tests
         :n  "T"  #'cider-test-run-test

         :n  "r"  #'rs/cider-cycle-buffer-type
         :n  "s"  #'russ/switch-cider-connection
         :n  "g"  #'cider-user-go

         :n  "m"  #'clojure-move-to-let)))
   (:after cider-browse-ns-mode
     (:map cider-browse-ns-mode-map
       :n "RET"       #'cider-browse-ns-operate-at-point))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cider reload this file, on-save
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cider-load-this-file ()
  (interactive)
  (cider-load-file (buffer-file-name)))

(defun clj-file-p ()
  (string-match-p (rx (or
                       (and ".clj" eol)
                       (and ".cljc" eol)))
                  (buffer-file-name)))

(defun cider-eval-if-cider-buffer ()
  (interactive)
  (if (and
       (boundp 'cider-mode)
       cider-mode
       cider-eval-on-save
       (clj-file-p))
      (cider-load-this-file)))

(setq cider-eval-on-save t)

(defun cider-toggle-eval-on-save ()
  (interactive)
  (let ((new-val (not cider-eval-on-save)))
    (print! "setting eval-on-save: %s" new-val)
    (setq cider-eval-on-save new-val)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flycheck
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package! flycheck-clj-kondo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; company-css-classes-backend
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package! company-css-classes)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clojure and cider mode configs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package! clojure-mode
  :mode "\\.clj$"
  :mode "\\.edn$"
  :mode "\\(?:build\\|profile\\)\\.boot$"
  :mode ("\\.cljs$" . clojurescript-mode)
  :mode ("\\.cljc$" . clojurec-mode)

  :config
  (require 'flycheck-clj-kondo)

  (add-hook
   'cider-mode-hook
   '(lambda ()
      (add-hook
       'after-save-hook
       #'cider-eval-if-cider-buffer)))

  (set-company-backend!
    'clojurescript-mode
    '(company-capf company-yasnippet company-flow company-css-classes-backend))

  (set-company-backend!
    'clojurec-mode
    '(company-capf company-yasnippet company-flow company-css-classes-backend))

  (setq cljr-magic-require-namespaces
        '(("io" . "clojure.java.io")
          ("sh" . "clojure.java.shell")
          ("set" . "clojure.set")
          ("str" . "cuerdas.core")
          ("path" . "pathetic.core")
          ("walk" . "clojure.walk")
          ("zip" . "clojure.zip")
          ("async" . "clojure.core.async")
          ("component" . "com.stuartsierra.component")
          ("sql" . "honeysql.core")
          ("csv" . "clojure.data.csv")
          ("json" . "cheshire.core")
          ("s" . "clojure.spec.alpha")
          ("rf" . "re-frame.core")
          ("r" . "reagent.core")
          ("t" . "tick.alpha.api"))

        clojure-align-forms-automatically t

        cider-save-file-on-load t

        cider-default-cljs-repl 'shadow
        cider-offer-to-open-cljs-app-in-browser nil
        cider-auto-jump-to-error nil
        cider-auto-select-error-buffer nil
        cider-auto-select-test-report-buffer nil
        cider-test-show-report-on-success t
        ;; cider-session-name-template "%j:%S"
        )

  (set-file-template!
   'clojure-mode :trigger "__deps.edn" :mode 'clojure-mode))

(use-package! ivy-cider
  :after cider-mode)

(use-package! clojure-essential-ref)
(use-package! clojure-essential-ref-nov
  :init
  (setq clojure-essential-ref-nov-epub-path
        "~/Dropbox/books/Clojure_The_Essential_Reference_v30.epub")
  (setq clojure-essential-ref-default-browse-fn
        #'clojure-essential-ref-nov-browse))

;; from @glittershark
;; https://cs.tvl.fyi/depot/-/blob/users/glittershark/emacs.d/+bindings.el#L1256-1267
(defun grfn/cider-copy-last-result ()
  (interactive)
  (cider-interactive-eval
   "*1"
   (nrepl-make-response-handler
    (current-buffer)
    (lambda (_ value)
      (kill-new value)
      (message "Copied last result (%s) to clipboard"
               (if (= (length value) 1) "1 char"
                 (format "%d chars" (length value)))))
    nil nil nil)))
