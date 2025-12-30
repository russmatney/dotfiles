
(defun straight-list-packages-to-csv ()
  "List all packages in straight.el's recipe cache with their GitHub URLs and save to a CSV file."
  (interactive)
  (require 'straight)
  (let ((csv-file (expand-file-name "straight-packages.csv" user-emacs-directory)))
    (with-temp-buffer
      (insert "Package,URL\n") ;; CSV header
      (maphash (lambda (package plist)
                 (let ((repo (plist-get plist :repo))
                       (host (plist-get plist :host)))
                   (when (and repo (string= host "github"))
                     (let ((github-url (format "https://github.com/%s" repo)))
                       (insert (format "%s,%s\n" package github-url))))))
               straight--recipe-cache)
      (write-region (point-min) (point-max) csv-file))
    (message "Package list saved to %s" csv-file)))

(comment
 (straight-list-packages-to-csv))
