;;; private/russ/autoload/projectile.el -*- lexical-binding: t; -*-

(setq github-roots '("~/russmatney/*"
                     "~/urbint/*"
                     "~/rschmukler/*"
                     "~/bolasblack/*"
                     "~/clojure/*"
                     "~/duct-framework/*"
                     "~/hlissner/*"
                     "~/jacekschae/*"
                     "~/l3nz/*"
                     "~/lambduh/*"
                     "~/nubank/*"
                     "~/oakes/*"
                     "~/rafaelrinaldi/*"
                     "~/smblott-github/*"
                     "~/walkable-server/*"))

(defun flatten (list-of-lists)
  (apply #'append list-of-lists))

(defun get-projects ()
  (let ((all-proj-paths (flatten (mapcar 'file-expand-wildcards github-roots))))
    (-distinct
     (mapcar 'directory-file-name
             (append all-proj-paths projectile-known-projects)))))

;; (comment
;;  (get-projects)

;;  (directory-file-name "mydir/")

;;  (-distinct (append local-projects projectile-known-projects)))

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
               (directory-file-name project-path))
              ))
         (if (+workspace-exists-p project-name)
             (+workspace-switch project-name)
           (progn (+workspace-switch project-name t)
                  (counsel-projectile-switch-project-action project-path))))))))
