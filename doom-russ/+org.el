;;; private/russ/+org.el -*- lexical-binding: t; -*-


(setq org-directory (expand-file-name "~/Dropbox/todo/"))


(defun +russ/org-capture-hook ()
  (setq org-capture-templates '(("t" "Todo [inbox]" entry
                                 (file+headline "~/Dropbox/todo/inbox.org" "Tasks")
                                 "* TODO %i%?")
                                ("T" "Tickler" entry
                                 (file+headline "~/Dropbox/todo/tickler.org" "Tickler")
                                 "* %i%? \n %U"))

        org-agenda-files '("~/Dropbox/todo/inbox.org"
                           "~/Dropbox/todo/gtd.org"
                           "~/Dropbox/todo/tickler.org")))

(after! org-capture
  (+russ/org-capture-hook))

(setq org-refile-targets '(("~/Dropbox/todo/gtd.org" :maxlevel . 3)
                           ("~/Dropbox/todo/someday.org" :level . 1)
                           ("~/Dropbox/todo/tickler.org" :maxlevel . 2)))

(map!
 (:after org
   (:map org-mode-map
     "C-j"    #'evil-window-down
     "C-k"    #'evil-window-up
     "M-h"    nil
     "M-RET"    #'doom/toggle-fullscreen
     "A-RET"    #'org-insert-item
     "A-t"    #'org-set-tags
     )))

(map!
 (:after org-capture
   (:map org-capture-mode-map
     [remap evil-save-and-close]          #'org-capture-finalize
     [remap evil-save-modified-and-close] #'org-capture-finalize
     [remap evil-quit]                    #'org-capture-kill)))

(setq org-archive-location (concat "~/Dropbox/todo/archive/" (format-time-string "%Y-%m") ".org::"))


;;; ob-elixir.el --- org-babel functions for elixir evaluation

;; Copyright (C) 2015 Victor Olinasc

;; URL: https://github.com/victorolinasc/ob-elixir
;; Author: Victor Olinasc
;; Keywords: literate programming, reproducible research
;; Package-Requires: ((emacs "24"))
;; Homepage: http://orgmode.org

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; Org-mode language support for elixir. Currently this only supports
;; the external compilation and execution of elixir code blocks (i.e.,
;; no session support). This code is inspired by ob-java.el in org-mode
;; sources.

;;; Code:
(require 'ob)

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("elixir" . "exs"))

(defun org-babel-execute:elixir (body params)
  (let* ((src-file "orgmode_elixir_src.exs")
         (vars (org-babel-variable-assignments:elixir params))
     (full-body (org-babel-expand-body:generic body params vars))
     (results (progn (with-temp-file src-file (insert full-body))
                         (org-babel-eval
                          (concat "elixir" " " src-file) ""))))
    (org-babel-reassemble-table
     (org-babel-elixir-table-or-string results)
     (org-babel-pick-name
      (cdr (assoc :colname-names params)) (cdr (assoc :colnames params)))
     (org-babel-pick-name
      (cdr (assoc :rowname-names params)) (cdr (assoc :rownames params))))))

;; Helpers, borrowed liberally from `ob-python'

(defun org-babel-variable-assignments:elixir (params)
  "Return a list of Elixir statements assigning the block's variables."
  (mapcar
   (lambda (pair)
     (format "%s = %s"
         (car pair)
         (org-babel-elixir-var-to-elixir (cdr pair))))
   (unless 'org-babel--get-vars
     ;; For backwards compatibility
     (mapcar #'cdr (org-babel-get-header params :var))
     (org-babel--get-vars params))))

(defun org-babel-elixir-var-to-elixir (var)
  "Convert an elisp value to an Elixir variable.
Convert an elisp value, VAR, into a string of Elixir source code
specifying a variable of the same value."
  (if (listp var)
      (concat "[" (mapconcat #'org-babel-elixir-var-to-elixir var ", ") "]")
    (if (equal var 'hline)
    "nil"  ; replace with variable?
      (format
       (if (and (stringp var) (string-match "[\n\r]" var)) "\"\"%S\"\"" "%S")
       (if (stringp var) (substring-no-properties var) var)))))

(defun org-babel-elixir-table-or-string (results)
  "Convert RESULTS into an appropriate elisp value."
  (org-babel-script-escape (org-babel-elixir-trim-string results)))

(defun org-babel-elixir-trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
White space here is any of: space, tab, Emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string "\\`[ \t\n]*" ""
                            (replace-regexp-in-string "[ \t\n]*\\'" "" string)))

(provide 'ob-elixir)
;;; ob-elixir.el ends here

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (sh . t)
   (elixir . t)
   (org . t)
   (ditaa . t))
 )

