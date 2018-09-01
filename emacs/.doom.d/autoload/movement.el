;;;  -*- lexical-binding: t; -*-


;;;###autoload
(defun rm/move-window-right ()
   ""
   (interactive)
   (if (window-in-direction 'right)
     (evil-window-right 1)
     (shell-command-to-string "chunkc tiling::window --focus east")
   )
)

;;;###autoload
(defun rm/move-window-left ()
   ""
   (interactive)
   (if (window-in-direction 'left)
     (evil-window-left 1)
     (shell-command-to-string "chunkc tiling::window --focus west")
   )
)

;;;###autoload
(defun rm/move-window-up ()
   ""
   (interactive)
   (if (window-in-direction 'above)
     (evil-window-up 1)
     (shell-command-to-string "chunkc tiling::window --focus north")
   )
)

;;;###autoload
(defun rm/move-window-down ()
   ""
   (interactive)
   (if (window-in-direction 'below)
     (evil-window-down 1)
     (shell-command-to-string "chunkc tiling::window --focus south")
   )
)
