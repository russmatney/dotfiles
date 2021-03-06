;;; ../dotfiles/emacs/.doom.d/autoload/org.el -*- lexical-binding: t; -*-

;; ref: https://org-roam.discourse.group/t/creating-an-org-roam-note-from-an-existing-headline/978
;;;###autoload
(defun russ/org-refile-to-new-note ()
  "Create an Org-roam note from the current headline and jump to it.

Normally, insert the headline’s title using the ’#title:’ file-level property
and delete the Org-mode headline. However, if the current headline has a
Org-mode properties drawer already, keep the headline and don’t insert
‘#+title:'. Org-roam can extract the title from both kinds of notes, but using
‘#+title:’ is a bit cleaner for a short note, which Org-roam encourages."
  (interactive)
  ;; TODO carry over tags, title, id?
  ;; TODO save the original file after running
  (let ((title (nth 4 (org-heading-components)))
        ;; (tags (nth 5 (org-heading-components)))
        (has-properties (org-get-property-block)))
    (org-cut-subtree)
    (org-roam-find-file title nil nil 'no-confirm)
    (org-paste-subtree)
    (unless has-properties
      (kill-line)
      (while (outline-next-heading)
        (org-promote)))
    (goto-char (point-min))
    (when has-properties
      (kill-line)
      (kill-line))
    (org-save-all-org-buffers)))

;;;###autoload
(defun russ/org-refile-to-existing-note ()
  "Refiles to an existing roam note."
  (interactive)
  (let ((org-refile-targets
         `((,(file-expand-wildcards "~/Dropbox/todo/garden/*.org")
            :maxlevel . 4))))
    (call-interactively #'org-refile)))
