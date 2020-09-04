;;; ~/dotfiles/emacs/.doom.d/+exwm.el -*- lexical-binding: t; -*-

;; Disable menu-bar, tool-bar and scroll-bar to increase the usable space.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;; Also shrink fringes to 1 pixel.
(fringe-mode 1)

;; Turn on `display-time-mode' if you don't use an external bar.
(setq display-time-default-load-average nil)
(display-time-mode t)

;; Emacs server is not required to run EXWM but it has some interesting uses
;; (see next section).
(server-start)

;;;; Below are configurations for EXWM.

;; Load EXWM.
(require 'exwm)
(require 'exwm-systemtray)
(exwm-systemtray-enable)

;; Set the initial number of workspaces (they can also be created later).
(setq exwm-workspace-number 2)

;; All buffers created in EXWM mode are named "*EXWM*". You may want to
;; change it in `exwm-update-class-hook' and `exwm-update-title-hook', which
;; are run when a new X window class name or title is available.  Here's
;; some advice on this topic:
;; + Always use `exwm-workspace-rename-buffer` to avoid naming conflict.
;; + For applications with multiple windows (e.g. GIMP), the class names of
;;    all windows are probably the same.  Using window titles for them makes
;;   more sense.
;; In the following example, we use class names for all windows expect for
;; Java applications and GIMP.
(add-hook 'exwm-update-class-hook
          (lambda ()
            (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                        (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-class-name))))
(add-hook 'exwm-update-title-hook
          (lambda ()
            (when (or (not exwm-instance-name)
                      (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                      (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-title))))

;; Bind "s-&" to launch applications ('M-&' also works if the output
;; buffer does not bother you).
(exwm-input-set-key
 (kbd "s-&")
 (lambda (command)
   (interactive (list (read-shell-command "$ ")))
   (start-process-shell-command command nil command)))

;; Bind "s-<f2>" to "slock", a simple X display locker.
(exwm-input-set-key
 (kbd "s-<f2>")
 (lambda ()
   (interactive)
   (start-process "" nil "/usr/bin/slock")))

;; Bind "s-r" to exit char-mode and fullscreen mode.
(exwm-input-set-key (kbd "s-r") 'exwm-reset)

;; Bind "s-w" to switch workspace interactively.
(exwm-input-set-key (kbd "s-w") 'exwm-workspace-switch)

;; Bind "s-0" to "s-9" to switch to a workspace by its index.
(mapc
    (lambda (i)
      (exwm-input-set-key (kbd (format "s-%d" i))
                          (lambda ()
                              (interactive)
                              (print (format "switching to workspace %d" i))
                              (exwm-workspace-switch-create i))))
  (number-sequence 0 9))

;; scratchpad keybindings
(exwm-input-set-key (kbd "s-u") (lambda ()
                                  (interactive)
                                  (print "journal toggle!")))


;; run a desktop app
(exwm-input-set-key
 (kbd "s-SPC")
 (lambda ()
   (interactive)
   (start-process "" nil "rofi" "-show" "drun" "-modi" "drun")))

;; ralphie rofi
(defun ralphie-rofi ()
  (interactive)
  (start-process "" nil "ralphie" "rofi"))
(exwm-input-set-key (kbd "s-x") 'ralphie-rofi)

;; open browser
(defun open-browser ()
  (interactive)
  (start-process "" nil "firefox"))
(exwm-input-set-key (kbd "s-b") 'open-browser)

;; open terminal
(defun alacritty ()
  (interactive)
  (start-process "" nil "alacritty"))
(exwm-input-set-key (kbd "s-<return>") 'alacritty)


;; volume helpers from https://github.com/ch11ng/exwm/issues/409
(defun my/adjust-volume (delta)
  "Adjust sound volume by DELTA."
  (shell-command-to-string (format "amixer set Master %s" delta)))

(defmacro my/volume-command (delta)
  "Return a command modifying sound volume by DELTA."
  `(lambda () (interactive) (my/adjust-volume ,delta)))

;; volume control
(exwm-input-set-key (kbd "<XF86AudioRaiseVolume>") (my/volume-command "5%+"))
(exwm-input-set-key (kbd "<XF86AudioLowerVolume>") (my/volume-command "5%-"))

;; To add a key binding only available in line-mode, simply define it in
;; `exwm-mode-map'.  The following example shortens 'C-c q' to 'C-q'.
(define-key exwm-mode-map [?\C-q] #'exwm-input-send-next-key)

;; The following example demonstrates how to use simulation keys to mimic
;; the behavior of Emacs.  The value of `exwm-input-simulation-keys` is a
;; list of cons cells (SRC . DEST), where SRC is the key sequence you press
;; and DEST is what EXWM actually sends to application.  Note that both SRC
;; and DEST should be key sequences (vector or string).
;; (setq exwm-input-simulation-keys
;;       '(
;;         ;; movement
;;         ([?\C-b] . [left])
;;         ([?\M-b] . [C-left])
;;         ([?\C-f] . [right])
;;         ([?\M-f] . [C-right])
;;         ([?\C-p] . [up])
;;         ([?\C-n] . [down])
;;         ([?\C-a] . [home])
;;         ([?\C-e] . [end])
;;         ([?\M-v] . [prior])
;;         ([?\C-v] . [next])
;;         ([?\C-d] . [delete])
;;         ([?\C-k] . [S-end delete])
;;         ;; cut/paste.
;;         ([?\C-w] . [?\C-x])
;;         ([?\M-w] . [?\C-c])
;;         ([?\C-y] . [?\C-v])
;;         ;; search
;;         ([?\C-s] . [?\C-f])))

;; You can hide the minibuffer and echo area when they're not used, by
;; uncommenting the following line.
;;(setq exwm-workspace-minibuffer-position 'bottom)

;; Do not forget to enable EXWM. It will start by itself when things are
;; ready.  You can put it _anywhere_ in your configuration.
(exwm-enable)
