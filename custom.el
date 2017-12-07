(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#0a0814" "#f2241f" "#67b11d" "#b1951d" "#4f97d7" "#a31db1" "#28def0" "#b2b2b2"])
 '(evil-want-Y-yank-to-eol nil)
 '(org-agenda-files (quote ("/home/huaming_li/Dropbox/org-notes/note.org")))
 '(package-selected-packages
   (quote
    (csv-mode git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter diff-hl ranger ibuffer-projectile graphviz-dot-mode engine-mode dockerfile-mode docker tablist docker-tramp wrap-region visual-regexp-steroids visual-regexp tiny peep-dired paredit nodejs-repl highlight-global helm-github-stars find-file-in-project etags-select discover-my-major makey cmake-font-lock browse-at-remote blog-admin 4clojure org-brain evil-org winum symon string-inflection password-generator org-category-capture impatient-mode evil-lion editorconfig cmake-ide levenshtein fuzzy let-alist nginx-mode realgud test-simple loc-changes load-relative company-auctex auctex web-beautify livid-mode skewer-mode simple-httpd json-mode json-snatcher json-reformat js2-refactor multiple-cursors js2-mode js-doc company-tern dash-functional tern coffee-mode unfill package-build names chinese-word-at-point powerline spinner log4e gntp ctable orglue epic htmlize parent-mode haml-mode gitignore-mode flx goto-chg diminish pkg-info request-deferred deferred epl web-completion-data pos-tip bind-map bind-key packed pythonic dash pinyinlib ace-jump-mode popup ivy-purpose hide-comnt helm-purpose window-purpose imenu-list pcache flyspell-correct highlight org zoutline iedit projectile counsel swiper ycmd yasnippet anaconda-mode ace-window auto-complete company anzu smartparens magit magit-popup git-commit with-editor evil undo-tree flycheck request helm helm-core async hydra ivy avy markdown-mode org-mac-link alert f s pug-mode org-plus-contrib youdao-dictionary yapfify yaml-mode ws-butler window-numbering which-key wgrep web-mode volatile-highlights vi-tilde-fringe uuidgen use-package toc-org tagedit stickyfunc-enhance srefactor sphinx-doc spacemacs-theme spaceline solarized-theme smex smeargle slim-mode scss-mode sass-mode restart-emacs rainbow-delimiters quelpa pyvenv pytest pyenv-mode py-isort protobuf-mode prodigy popwin pip-requirements persp-mode pcre2el paradox pangu-spacing orgit org-projectile org-present org-pomodoro org-octopress org-download org-bullets open-junk-file neotree mwim move-text monokai-theme mmm-mode markdown-toc magit-gitflow macrostep lorem-ipsum live-py-mode lispy linum-relative link-hint less-css-mode jade-mode ivy-hydra info+ indent-guide ido-vertical-mode hy-mode hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation help-fns+ helm-themes helm-swoop helm-pydoc helm-projectile helm-mode-manager helm-make helm-gtags helm-gitignore helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag google-translate google-c-style golden-ratio gnuplot gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md ggtags flyspell-correct-ivy flyspell-correct-helm flycheck-ycmd flycheck-pos-tip flx-ido find-by-pinyin-dired fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu emmet-mode elisp-slime-nav dumb-jump disaster define-word cython-mode counsel-projectile company-ycmd company-web company-statistics company-quickhelp company-c-headers company-anaconda column-enforce-mode cmake-mode clean-aindent-mode clang-format chinese-wbim auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile aggressive-indent adaptive-wrap ace-pinyin ace-link ace-jump-helm-line ac-ispell)))
 '(projectile-other-file-alist
   (quote
    (("cpp" "h" "hpp" "ipp")
     ("ipp" "h" "hpp" "cpp")
     ("hpp" "h" "ipp" "cpp" "cc")
     ("cxx" "h" "hxx" "ixx")
     ("ixx" "h" "hxx" "cxx")
     ("hxx" "h" "ixx" "cxx")
     ("c" "h")
     ("m" "h")
     ("mm" "h")
     ("h" "c" "cc" "cpp" "ipp" "hpp" "cxx" "ixx" "hxx" "m" "mm")
     ("cc" "h" "hh" "hpp")
     ("hh" "cc")
     ("vert" "frag")
     ("frag" "vert")
     (nil "lock" "gpg")
     ("lock" "")
     ("gpg" ""))))
 '(safe-local-variable-values
   (quote
    ((eval setq flycheck-clang-include-path
           (list
            (expand-file-name "~/github/lytz/stockbs_svr/stockbs_svr/")
            (expand-file-name "~/github/lytz/ground_15_04_09/")
            (expand-file-name "/usr/local/protobuf/include/")
            (expand-file-name "/home/huaming_li/github/redisclient/src/")))
     (company-clang-arguments "-std=c++11" "-I/home/huaming_li/github/lytz/stockbs_svr/stockbs_svr/" "-I/home/huaming_li/github/lytz/ground_15_04_09/" "-I/usr/local/protobuf/include/" "-I/home/huaming_li/github/redisclient/src/")
     (company-clang-arguments "-std=c++11" "-I/home/huaming_li/github/lytz/stockbs_svr/stockbs_svr/" "-I/home/huaming_li/rts3/src/common/ground_15_04_09/" "-I/usr/local/protobuf/include/" "-I/home/huaming_li/github/redisclient/src/")
     (eval setq flycheck-clang-include-path
           (list
            (expand-file-name "~/github/lytz/stockbs_svr/stockbs_svr/")
            (expand-file-name "~/rts3/src/common/ground_15_04_09/")
            (expand-file-name "/usr/local/protobuf/include/")
            (expand-file-name "/home/huaming_li/github/redisclient/src/")))
     (company-clang-arguments "-std=c++11" "-I/home/huaming_li/github/lytz/stockbs_svr/stockbs_svr/" "-I/home/huaming_li/rts3/src/common/ground_15_04_09/" "-I/usr/local/protobuf/include/" "/home/huaming_li/github/redisclient/src/")
     (eval setq flycheck-clang-include-path
           (list
            (expand-file-name "~/github/lytz/stockbs_svr/stockbs_svr/")
            (expand-file-name "~/rts3/src/common/ground_15_04_09/")
            (expand-file-name "/usr/local/protobuf/include/")))
     (company-clang-arguments "-std=c++11" "-I/home/huaming_li/github/lytz/stockbs_svr/stockbs_svr/" "-I/home/huaming_li/rts3/src/common/ground_15_04_09/" "-I/usr/local/protobuf/include/")
     (eval setq flycheck-clang-include-path
           (list
            (expand-file-name "~/github/lytz/Trading_Repo/FutureServer/src/")
            (expand-file-name "~/github/lytz/rlib-cpp/")
            (expand-file-name "~/github/lytz/")
            (expand-file-name "~/github/lytz/Trading_Repo/FutureServer/libs/tradeapi/")
            (expand-file-name "/usr/local/protobuf/include/")))
     (company-clang-arguments "-std=c++11" "-I/home/huaming_li/github/lytz/Trading_Repo/FutureServer/src/" "-I/home/huaming_li/github/lytz/rlib-cpp/" "-I/home/huaming_li/github/lytz/" "-I/home/huaming_li/github/lytz/Trading_Repo/FutureServer/libs/tradeapi/" "-I/usr/local/protobuf/include/")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
