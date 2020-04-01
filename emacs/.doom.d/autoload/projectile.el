;;; private/russ/autoload/projectile.el -*- lexical-binding: t; -*-

(defmacro comment (&rest _)
  "Comment out one or more s-expressions."
  nil)

(defun get-roots ()
  (require 'org)
  (let ((contents
         (with-temp-buffer
           (insert-file-contents "~/Dropbox/todo/repos.org")
           (delay-mode-hooks (org-mode))
           (org-element-parse-buffer 'headline t))))
    contents))

(defun repo-ids ()
  (cl-map 'list
          (lambda (repo-id)
            (s-prepend "~/" repo-id))
          (org-element-map (get-roots) 'headline
            (lambda (hl)
              (org-element-property :title hl)))))

(comment
 (s-prepend "~/" "hello/world"))

(defun flatten (list-of-lists)
  (apply #'append list-of-lists))

(defun get-projects ()
  (let ((all-proj-paths (flatten (mapcar 'file-expand-wildcards (repo-ids)))))
    (-distinct
     (mapcar 'directory-file-name
             (append all-proj-paths projectile-known-projects)))))


;;;###autoload
(defun rs/projectile-switch-project-workspace ()
  "Use projectile prompt to find or switch projects in a workspace tab."
  (interactive)
  (require 'projectile)
  (require 'counsel-projectile)
  (let ((all-projects (get-projects)))
    (ivy-read
     (projectile-prepend-project-name "Switch to project: ") all-projects
     :preselect (and (projectile-project-p)
                     (abbreviate-file-name (projectile-project-root)))
     :require-match nil
     :action
     (lambda (project-path)
       (let ((project-name
              (file-name-nondirectory
               (directory-file-name project-path))))
         (if (+workspace-exists-p project-name)
             (+workspace-switch project-name)
           (progn (+workspace-switch project-name t)
                  (counsel-projectile-switch-project-action project-path))))))))

(defun russ/projectile-open-file-from-project ()
  "Use projectile prompt to find a file in another project."
  (interactive)
  (require 'projectile)
  (require 'counsel-projectile)
  (let ((all-projects (get-projects)))
    (ivy-read
     (projectile-prepend-project-name "Open file from project: ") all-projects
     :preselect (and (projectile-project-p)
                     (abbreviate-file-name (projectile-project-root)))
     :require-match nil
     :action
     (lambda (project-path)
       (doom-project-find-file project-path)))))
