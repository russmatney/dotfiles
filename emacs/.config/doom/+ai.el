;;; ../../dotfiles/emacs/.config/doom/+ai.el -*- lexical-binding: t; -*-

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
  :hook (gdscript-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("C-TAB" . 'copilot-accept-completion)
              ("C-<tab>" . 'copilot-accept-completion)
              ("C-l" . 'copilot-accept-completion))
  :config
  ;; (setq copilot-idle-delay 1) ;; ?
  (setq copilot-idle-delay 2)
  )
