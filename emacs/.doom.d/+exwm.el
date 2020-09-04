;;; ~/dotfiles/emacs/.doom.d/+exwm.el -*- lexical-binding: t; -*-

(defmacro comment (&rest _)
  "Comment out one or more s-expressions."
  nil)

;; Emacs server is not required to run EXWM but it has some interesting uses
;; (see next section).
(server-start)

;;;; Below are configurations for EXWM.

;; Load EXWM.
(require 'exwm)
;; (require 'exwm-systemtray)
;; (exwm-systemtray-enable)

;; Set the initial number of workspaces (they can also be created later).
(setq exwm-workspace-number 2)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Workspaces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; cycle workspaces by index
(defun exwm-workspace-next (&optional reverse)
  (interactive "P")
  (let ((fn (if reverse #'- #'+)))
    (exwm-workspace-switch
     (mod (apply fn (list exwm-workspace-current-index 1))
          (length exwm-workspace--list)))))

(comment
     (mod (apply #'+ (list 1 exwm-workspace-current-index))
          (length exwm-workspace--list)))

;; workspace cycling
(exwm-input-set-key (kbd "s-n") (lambda () (interactive) (exwm-workspace-next t)))
(exwm-input-set-key (kbd "s-p") (lambda () (interactive) (exwm-workspace-next nil)))


;; scratchpad keybindings
(exwm-input-set-key (kbd "s-u") (lambda () (interactive) (print "journal toggle!")))
(exwm-input-set-key (kbd "s-y") (lambda () (interactive) (print "yodo toggle!")))
(exwm-input-set-key (kbd "s-t") (lambda () (interactive) (print "browser toggle!")))
(exwm-input-set-key (kbd "s-r") (lambda () (interactive) (print "notes toggle!")))


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; volume control
;; volume helpers from https://github.com/ch11ng/exwm/issues/409
(defun my/adjust-volume (delta)
  "Adjust sound volume by DELTA."
  (shell-command-to-string (format "amixer set Master %s" delta)))

(defmacro my/volume-command (delta)
  "Return a command modifying sound volume by DELTA."
  `(lambda () (interactive) (my/adjust-volume ,delta)))

(exwm-input-set-key (kbd "<XF86AudioRaiseVolume>") (my/volume-command "5%+"))
(exwm-input-set-key (kbd "<XF86AudioLowerVolume>") (my/volume-command "5%-"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; brightness

(defun my/increase-brightness ()
  "Increase brightness"
  (interactive)
  (shell-command-to-string "light -A 5"))

(defun my/decrease-brightness ()
  "Increase brightness"
  (interactive)
  (shell-command-to-string "light -U 5"))

(exwm-input-set-key (kbd "<XF86MonBrightnessDown>") 'my/decrease-brightness)
(exwm-input-set-key (kbd "<XF86MonBrightnessUp>") 'my/increase-brightness)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Client bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; To add a key binding only available in line-mode, simply define it in
;; `exwm-mode-map'.  The following example shortens 'C-c q' to 'C-q'.
(define-key exwm-mode-map (kbd "C-q") #'exwm-input-send-next-key)
(define-key exwm-mode-map (kbd "C-c") nil)

(define-key exwm-mode-map (kbd "s-f") #'exwm-floating-toggle-floating)
(define-key exwm-mode-map (kbd "s-q") #'kill-buffer-and-window)

;; super+click+drag to move
(setq exwm-input-move-event 's-down-mouse-1
      exwm-input-resize-event 'M-down-mouse-1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; app startup hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


(add-hook 'exwm-manage-finish-hook
          (lambda ()
            (when (and exwm-class-name
                       (string= exwm-class-name "Firefox"))
              (exwm-input-set-local-simulation-keys nil))))

(setq exwm-layout-show-all-buffers t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clean up UI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Disable menu-bar, tool-bar and scroll-bar to increase the usable space.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;; Also shrink fringes to 1 pixel.
(fringe-mode 2)

;; Turn on `display-time-mode' if you don't use an external bar.
(setq display-time-default-load-average nil)
(display-time-mode t)


;; Do not forget to enable EXWM. It will start by itself when things are
;; ready.  You can put it _anywhere_ in your configuration.
(exwm-enable)
