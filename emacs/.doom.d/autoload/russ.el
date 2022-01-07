;;; ~/dotfiles/emacs/.doom.d/autoload/russ.el -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'dash)
(require 's)


(defmacro comment (&rest _)
  "Comment out one or more s-expressions."
  nil)

;; require-match set to nil
;;;###autoload
(defun russ/counsel-projectile-find-file (&optional arg dwim)
  "Jump to a file in the current project.

With a prefix ARG, invalidate the cache first.  If DWIM is
non-nil, use completion based on context."
  (interactive "P")
  (if (and (eq projectile-require-project-root 'prompt)
           (not (projectile-project-p)))
      (counsel-projectile-find-file-action-switch-project)
    (projectile-maybe-invalidate-cache arg)
    (let* ((project-files (projectile-current-project-files))
           (files (and dwim (projectile-select-files project-files))))
      (ivy-read (projectile-prepend-project-name "Find file: ")
                (or files project-files)
                :matcher counsel-projectile-find-file-matcher
                :require-match nil
                :sort counsel-projectile-sort-files
                :action counsel-projectile-find-file-action
                :caller 'counsel-projectile-find-file))))

;;;###autoload
(defun russ/doom-project-find-file (dir)
  "Jump to a file in DIR (searched recursively).

If DIR is not a project, it will be indexed (but not cached)."
  (interactive)
  (unless (file-directory-p dir)
    (error "Directory %S does not exist" dir))
  (unless (file-readable-p dir)
    (error "Directory %S isn't readable" dir))
  (let* ((default-directory (file-truename (expand-file-name dir)))
         (project-root (doom-project-root default-directory))
         (projectile-project-root default-directory)
         (projectile-enable-caching projectile-enable-caching))
    (cond ((and project-root (file-equal-p project-root projectile-project-root))
           (unless (doom-project-p projectile-project-root)
             ;; Disable caching if this is not a real project; caching
             ;; non-projects easily has the potential to inflate the projectile
             ;; cache beyond reason.
             (setq projectile-enable-caching nil))
           (call-interactively
            ;; Intentionally avoid `helm-projectile-find-file', because it runs
            ;; asynchronously, and thus doesn't see the lexical
            ;; `default-directory'
            (if (doom-module-p :completion 'ivy)
                #'russ/counsel-projectile-find-file
              #'projectile-find-file)))
          ((fboundp 'counsel-file-jump) ; ivy only
           (call-interactively #'counsel-file-jump))
          ((project-current)
           (project-find-file-in nil (list default-directory) nil))
          ((fboundp 'helm-find-files)
           (call-interactively #'helm-find-files))
          ((call-interactively #'find-file)))))

;;;###autoload
(defun russ/open-yodo-file ()
  "Browse yodo project"
  (interactive)
  (doom-project-find-file "~/russmatney/yodo"))

;;;###autoload
(defun russ/open-emacs-config-file ()
  "Browse your emacs config."
  (interactive)
  (doom-project-find-file "~/.doom.d"))

;;;###autoload
(defun russ/open-org-file ()
  "Browse your org-dir."
  (interactive)
  (doom-project-find-file "~/Dropbox/todo/"))

;;;###autoload
(defun russ/open-writing-file ()
  "Browse your org-dir."
  (interactive)
  (doom-project-find-file "~/Dropbox/Writing/"))

;;;###autoload
(defun russ/open-doom-file ()
  "Open a file in doom itself."
  (interactive)
  (doom-project-find-file "~/.emacs.d/"))

;;;###autoload
(defun russ/open-dotfile ()
  "Open a file from my dotfiles itself."
  (interactive)
  (doom-project-find-file "~/dotfiles/"))

;;;###autoload
(defun russ/open-awesomewm-source ()
  "Open a file from awesomewm's local repo."
  (interactive)
  (doom-project-find-file "~/awesomeWM/awesome/lib/"))

;;;###autoload
(defun russ/transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

;; transparency
;; (russ/transparency 85)

;;;###autoload
(defun russ/delete-numbered-workspace-names ()
  "Deletes #.* workspaces (usually created by mistake)"
  (interactive)
  (let ((to-delete (-filter
                    (-partial 's-starts-with? "#")
                    (+workspace-list-names))))
    (-each to-delete '+workspace-delete)))

;;;###autoload
(defun russ/open-workspace (name)
  ;; Support opening emacs in a workspace
  (require 'persp-mode)
  (persp-mode t)
  (+workspace-switch name t)
  (russ/delete-numbered-workspace-names)
  ;; (russ/projectile-open-file-from-project)
  )

(comment
 (russ/open-workspace "cli-bindings"))

;;;###autoload
;; Set ids on multiple org commands
(defun russ/org-set-headline-ids (start end)
  "Executes 'org-id-get-create' for org headlines in the region."
  (interactive (list
                (if (region-active-p) (region-beginning) (point-min))
                (if (region-active-p) (region-end) (point-max))))
  (save-excursion
    (goto-char start)
    (when (org-at-heading-p)
      (org-id-get-create))
    (while (re-search-forward org-heading-regexp end t)
      (when (org-at-heading-p)
        (org-id-get-create)))))

(comment
 org-loop-over-headlines-in-active-region
 org-id-add-location
 (org-id-locations-load)
 org-id-locations-file)


;;;###autoload
(defun russ/last-screenshot ()
  (let ((filename (shell-command-to-string "ls ~/Screenshots | sort -V | tail -n 1")))
    (s-trim filename)))
