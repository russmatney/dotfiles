;;; ../../dotfiles/emacs/.config/doom/+ai.el -*- lexical-binding: t; -*-

(use-package! eca
  :bind (("C-c e" . eca))
  :config
  ;; Optional: Configure chat window position
  ;; (setq eca-chat-use-side-window t)
  ;; (setq eca-chat-window-side 'right)

  ;; Optional: Auto-add repository context
  ;; (setq eca-chat-auto-add-repomap t)

  ;; Optional: Set completion delay
  ;; (setq eca-completion-idle-delay 0.5)

  ;; Optional: Custom model selection
  ;; (setq eca-chat-custom-model "claude-sonnet-4")

  ;; Optional: Pass extra args for debugging
  ;; (setq eca-extra-args '("--verbose"))
  )

(use-package! aidermacs
  :bind (("C-c a" . aidermacs-transient-menu))

  :config
  (setq aidermacs-use-architect-mode t)
  ;; (setq aidermacs-default-model "ollama_chat/llama3.1-claude:latest")
  (setq aidermacs-default-model "ollama_chat/gemma3:1b")
  ;; (setq aidermacs-default-model "openrouter/deepseek/deepseek-r1:free")

  ;; Optional: Set specific model for architect reasoning
  ;; (setq aidermacs-architect-model "ollama_chat/devstral:24b")

  ;; Optional: Set specific model for code generation
  ;; (setq aidermacs-editor-model "ollama_chat/llama3.1-claude:latest")
  )

(use-package! copilot
  ;; consider more modes, but don't opt into everything
  :hook
  (gdscript-mode . copilot-mode)
  (csharp-ts-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("C-TAB" . 'copilot-accept-completion)
              ("C-<tab>" . 'copilot-accept-completion)
              ("C-l" . 'copilot-accept-completion))
  :config
  ;; (setq copilot-idle-delay 1) ;; ?
  ;; (setq copilot-idle-delay 2)
  (setq copilot-idle-delay 0.5)
  )
