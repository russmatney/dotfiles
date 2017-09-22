;;; private/russ/+commands.el -*- lexical-binding: t; -*-

(defalias 'ex! 'evil-ex-define-cmd)

(ex! "x"           #'evil-save-modified-and-close)

;; Dealing with buffers
(evil-set-command-properties #'+workspace/cleanup :ex-bang t)

(ex! "clean[up]"   #'+workspace/cleanup)
(ex! "k[ill]"      #'doom/kill-this-buffer)
(ex! "k[ill]all"   #'+russ:kill-all-buffers)
(ex! "k[ill]m"     #'+russ:kill-matching-buffers)
(ex! "k[ill]o"     #'doom/kill-other-buffers)
(ex! "l[ast]"      #'doom/popup-restore)
(ex! "m[sg]"       #'view-echo-area-messages)

;; Project navigation
(ex! "a"           #'projectile-find-other-file)
(ex! "cd"          #'+russ:cd)

;; search
(cond ((featurep! :completion ivy)
       (ex! "ag"       #'+ivy:ag)
       (ex! "agc[wd]"  #'+ivy:ag-cwd)
       (ex! "rg"       #'+ivy:rg)
       (ex! "rgc[wd]"  #'+ivy:rg-cwd)
       (ex! "sw[iper]" #'+ivy:swiper)
       (ex! "t[odo]"   #'+ivy:todo))
      ((featurep! :completion helm)
       (ex! "ag"       #'+helm:ag)
       (ex! "agc[wd]"  #'+helm:ag-cwd)
       (ex! "rg"       #'+helm:rg)
       (ex! "rgc[wd]"  #'+helm:rg-cwd)
       (ex! "sw[oop]"  #'+helm:swoop)
       (ex! "t[odo]"   #'+helm:todo)))

;; Project tools
(ex! "er[rors]"    #'flycheck-list-errors)

;; File operations
(ex! "cp"          #'+evil:copy-this-file)
(ex! "mv"          #'+evil:move-this-file)
(ex! "rm"          #'+evil:delete-this-file)

;; Sessions/tabs
(ex! "sclear"      #'+workspace/kill-session)
(ex! "sl[oad]"     #'+workspace:load-session)
(ex! "ss[ave]"     #'+workspace:save-session)
(ex! "tabc[lose]"  #'+workspace:delete)
(ex! "tabclear"    #'doom/kill-all-buffers)
(ex! "tabl[ast]"   #'+workspace/switch-to-last)
(ex! "tabload"     #'+workspace:load)
(ex! "tabn[ew]"    #'+workspace:new)
(ex! "tabn[ext]"   #'+workspace:switch-next)
(ex! "tabp[rev]"   #'+workspace:switch-previous)
(ex! "tabr[ename]" #'+workspace:rename)
(ex! "tabs"        #'+workspace/display)
(ex! "tabsave"     #'+workspace:save)

;; Org-mode
(ex! "capture"     #'+org:capture)
(ex! "org"         #'+org:capture) ;; TODO go to gtd
