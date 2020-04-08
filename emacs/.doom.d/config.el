;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Author     : russmatney
;; CreatedAt  : 15 April 2018
;; ModifiedAt : 15 April 2018
;; Status     : Usable
;;
;; Configuration for doom and emacs at large.
;;
;; Misc and otherwise uncategorized details are located here.
;; Note the use of `(load! +filename)` to load sibling files.
;;

;;; Private keys'n'such
(load! "+private")

;; Basic Config
(setq backup-directory-alist `(("." . "~/.emacs-tmp/")))
(setq auto-save-file-name-transforms `((".*" "~/.emacs-tmp/" t)))

;; Spaces over tabs
(setq c-basic-indent 2)
(setq c-default-style "linux")
(setq tab-width 2)
(setq-default indent-tabs-mode nil)

;; Auto revert-mode
(global-auto-revert-mode t)

;; Turn off line wrapping
(setq-default truncate-lines 1)

;; 80 chars! be diligent!
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

(setq +ivy-buffer-icons t)

;; zen-mode for focused writing
(use-package! zen-mode)

(set-face-attribute 'default nil :height 100)

;; PATH
(add-to-list 'exec-path "/home/russ/.nix-profile/bin")
(add-to-list 'exec-path "/home/russ/.pyenv/shims")

;; Add '--hidden' to rg command to include hidden files in search
;; Note that `echo ".git/" >> ~/.ignore` will exclude .git from these searches
(setq counsel-rg-base-command
      "rg -zS -T jupyter -T svg -T lock -T license --no-heading --line-number --color never %s .")

(setq +format-on-save-enabled-modes t)

;; load bindings
(load! "+bindings")

;; load programming language configs
(load! "+langs")
(load! "+lisp-editing")
(load! "+clojure")
(load! "+css-classes-backend")

;; load prose tools (includes markdown)
(load! "+org-custom")

(load! "+wakatime")
(+wakatime-autostart-h)

(load! "+exwm")

(defun grfn/insert-new-src-block ()
  (interactive)
  (let* ((current-src-block (org-element-at-point))
         (src-block-head (save-excursion
                           (goto-char (org-element-property
                                       :begin current-src-block))
                           (thing-at-point 'line t)))
         (point-to-insert
          (if-let (results-loc (org-babel-where-is-src-block-result))
              (save-excursion
                (goto-char results-loc)
                (org-element-property
                 :end
                 (org-element-at-point)))
            (org-element-property :end (org-element-at-point)))))
    (goto-char point-to-insert)
    (insert "\n")
    (insert src-block-head)
    (let ((contents (point-marker)))
      (insert "\n#+END_SRC\n")
      (goto-char contents))))

(defun grfn/+org-insert-item (orig direction)
  (interactive)
  (if (and (org-in-src-block-p)
           (equal direction 'below))
      (grfn/insert-new-src-block)
    (funcall orig direction)))

(advice-add #'+org/insert-item :around #'grfn/+org-insert-item)

;; from glittershark
(cl-defstruct pull-request url number title author repository)

(defun grfn/alist->plist (alist)
  (->> alist
       (-mapcat (lambda (pair)
                  (list (intern (concat ":" (symbol-name (car pair))))
                        (cdr pair))))))

(defun grfn/review-requests ()
  (let ((resp (ghub-graphql "query reviewRequests {
    reviewRequests: search(
      type:ISSUE,
      query: \"is:open is:pr review-requested:glittershark archived:false\",
      first: 100
    ) {
      issueCount
      nodes {
        ... on PullRequest {
          url
          number
          title
          author {
            login
            ... on User { name }
          }
          repository {
            name
            owner { login }
          }
        }
      }
    }
  }")))
    (->> resp
         (alist-get 'data)
         (alist-get 'reviewRequests)
         (alist-get 'nodes)
         (-map
          (lambda (pr)
            (apply
             #'make-pull-request
             (grfn/alist->plist pr)))))))

(defun grfn/pr->org-headline (level pr)
  (check-type level integer)
  (check-type pr pull-request)
  (format "%s TODO Review %s's PR on %s/%s: %s :pr:
SCHEDULED: <%s>"
          (make-string level ?*)
          (->> pr (pull-request-author) (alist-get 'name))
          (->> pr (pull-request-repository)
               (alist-get 'owner)
               (alist-get 'login))
          (->> pr (pull-request-repository) (alist-get 'name))
          (org-make-link-string
           (pull-request-url pr)
           (pull-request-title pr))
          (format-time-string "%Y-%m-%d %a")))

(defun grfn/org-headlines-from-review-requests (level)
  "Create org-mode headlines at LEVEL from all review-requested PRs on Github"
  (interactive "*nLevel: ")
  (let* ((prs (grfn/review-requests))
         (text (mapconcat (apply-partially #'grfn/pr->org-headline level) prs "\n")))
    (save-mark-and-excursion
      (insert text))
    (org-align-tags 't)))
