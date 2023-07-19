;;; ../dotfiles/emacs/.doom.d/+org-babel-clojure.el -*- lexical-binding: t; -*-

;; ref: https://git.jeremydormitzer.com/jdormit/dotfiles/commit/5f9dbe53cea2b37fc89cc49f858f98387da99576

(with-eval-after-load 'ob-clojure
  (defcustom org-babel-clojure-backend nil
    "Backend used to evaluate Clojure code blocks."
    :group 'org-babel
    :type '(choice
            (const :tag "cider" cider)
            (const :tag "bb" bb)
            (const :tag "Not configured yet" nil)))

  (defun elisp->clj (in)
    (cond
     ((listp in) (concat "[" (s-join " " (mapcar #'elisp->clj in)) "]"))
     (t (format "%s" in))))

  (defun ob-clojure-eval-with-bb (expanded params)
    "Evaluate EXPANDED code block with PARAMS using babashka."
    (unless (executable-find "bb")
      (user-error "Babashka not installed"))
    (let* ((stdin (let ((stdin (cdr (assq :stdin params))))
                    (when stdin
                      (elisp->clj
                       (org-babel-ref-resolve stdin)))))
           (input (cdr (assq :input params)))
           (file (make-temp-file "ob-clojure-bb" nil nil expanded))
           (command (concat (when stdin (format "echo %s | " (shell-quote-argument stdin)))
                            (format "bb %s -f %s"
                                    (cond
                                     ((equal input "edn") "")
                                     ((equal input "text") "-i")
                                     (t ""))
                                    (shell-quote-argument file))))
           (result (shell-command-to-string command)))
      (s-trim result)))

  (defun org-babel-execute:clojure (body params)
    "Execute a block of Clojure code with Babel."
    (unless org-babel-clojure-backend
      (user-error "You need to customize org-babel-clojure-backend"))
    (let* ((expanded (org-babel-expand-body:clojure body params))
           (result-params (cdr (assq :result-params params)))
           result)
      (setq result
            (cond
             ((eq org-babel-clojure-backend 'cider)
              (ob-clojure-eval-with-cider expanded params))
             ((eq org-babel-clojure-backend 'bb)
              (ob-clojure-eval-with-bb expanded params))))
      (org-babel-result-cond result-params
        result
        (condition-case nil (org-babel-script-escape result)
          (error result)))))

  (customize-set-variable 'org-babel-clojure-backend 'bb))

(add-hook 'org-mode-hook (lambda () (require 'ob-clojure)))
