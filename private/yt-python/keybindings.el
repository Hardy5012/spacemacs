(add-hook 'python-mode-hook
          (lambda ()
            (which-key-add-major-mode-key-based-replacements 'python-mode
              "C-c r" "anaconda find reference cmds"
              "C-c C-t" "skeleton"
              "C-c !" "flycheck")
            (define-key python-mode-map (kbd "C-c C-b") 'python-add-breakpoint)
            (define-key python-mode-map (kbd "C-c C-d") 'sphinx-doc)
            (define-key python-mode-map (kbd "C-c C-c") 'python-shell-send-buffer-switch)))
