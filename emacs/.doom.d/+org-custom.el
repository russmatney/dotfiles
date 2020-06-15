;;;  -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markdown
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(map! :after markdown-mode
      :map markdown-mode-map
      "M-p"    nil
      "M-n"    nil

      :map evil-markdown-mode-map
      "M-p"    nil
      "M-n"    nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-archive-location (concat "~/Dropbox/todo/archive/" (format-time-string "%Y-%m") ".org::"))

;; allow refiling into a file without choosing a headline
(setq org-refile-use-outline-path 'file)
(setq org-refile-allow-creating-parent-nodes t)
(setq org-refile-targets
      '(("~/Dropbox/todo/todo.org" :maxlevel . 2)
        ("~/Dropbox/todo/specs.org" :maxlevel . 2))

      org-agenda-files (file-expand-wildcards "~/Dropbox/todo/*.org"))


(advice-add 'org-archive-subtree
            :after
            (lambda (&rest _)
              (org-save-all-org-buffers)))

(advice-add 'org-refile
            :after
            (lambda (&rest _)
              (org-save-all-org-buffers)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org Bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; org
(map! :after org
      :map org-mode-map
      "M-j"    nil
      "M-k"    nil
      "M-h"    nil
      "M-l"    nil
      :nm "M-j"    nil
      :nm "M-k"    nil
      :nm "M-h"    nil
      :nm "M-l"    nil
      "M-v"    #'evil-paste-after
      "M-RET"  #'org-insert-item
      "M-t"    #'org-set-tags-command
      "TAB"    #'+org/toggle-fold

      :map evil-org-agenda-mode-map
      :nm "M-j"    nil
      :nm "M-k"    nil
      :nm "M-h"    nil
      :nm "M-l"    nil)

(map! :after evil-org
      :map evil-org-mode-map
      :nm "M-j"    nil
      :nm "M-k"    nil
      :nm "M-h"    nil
      :nm "M-l"    nil

      :map evil-normal-state-map
      "z w"    #'widen

      :map evil-org-agenda-mode-map
      :nm "M-j"    nil
      :nm "M-k"    nil)


(map! :after markdown
      :map markdown-mode-map
      "M-n" nil
      "M-p" nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org Capture
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun roam-template-head ()
  (let ((uuid (shell-command-to-string "uuidgen")))
    (format "#+ID: %s#+TITLE: ${title}\n" uuid)))

;; #+HUGO_SLUG: ${slug}

;; org capture
(map! :after org-capture
      :map org-capture-mode-map
      [remap evil-save-and-close]          #'org-capture-finalize
      [remap evil-save-modified-and-close] #'org-capture-finalize
      [remap evil-quit]                    #'org-capture-kill)

(after! org-capture
  (setq org-capture-templates
        '(("t" "Todo [inbox]" entry
           (file+headline "~/Dropbox/todo/inbox.org" "Tasks")
           "* [ ] %i%?")))

;; https://github.com/org-roam/org-roam/issues/217
(setq org-roam-capture-templates
        '(("d" "default" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "%<%Y%m%d%H%M%S>-${slug}"
           :head
           "#+ID: %(shell-command-to-string \"uuidgen\")#+TITLE: ${title}
#+hugo_base_dir: ~/russmatney/hugo-roam\n"
           :unnarrowed t))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org Clubhouse
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(map!
 :leader
   :desc "notes" :prefix "n"
   :desc "add story to clubhouse" :n "c" #'org-clubhouse-create-story)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org Roam export
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; from https://github.com/org-roam/org-roam/issues/484
(after! (org org-roam)
    (defun my/org-roam--backlinks-list (file)
      (if (org-roam--org-roam-file-p file)
          (--reduce-from
           (concat acc (format "- [[file:%s][%s]]\n"
                               (file-relative-name (car it) org-roam-directory)
                               (org-roam--get-title-or-slug (car it))))
           "" (org-roam-db-query [:select [from]
                                  :from links
                                  :where (= to $s1)
                                  :and from :not :like $s2] file "%private%"))
        ""))
    (defun my/org-export-preprocessor (_backend)
      (let ((links (my/org-roam--backlinks-list (buffer-file-name))))
        (unless (string= links "")
          (save-excursion
            (goto-char (point-max))
            (insert (concat "\n* Backlinks\n" links))))))
    (add-hook 'org-export-before-processing-hook 'my/org-export-preprocessor))
