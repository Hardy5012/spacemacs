(setq yelang-programming-packages
      '(
        css-mode
		sphinx-doc
        paredit
        lispy
        cmake-font-lock
        cmake-mode
        flycheck
        nodejs-repl
        (nodejs-repl-eval :location local)
        js2-mode
        js2-refactor
        json-mode
        racket-mode
        yasnippet
        web-mode
        js-doc
        (cc-mode :location built-in)
        ;; flycheck-clojure
        ;; etags-select
        (emacs-lisp :location built-in)
        ;; clojure-mode
        company
        (eldoc :location built-in)
        dumb-jump
        robe
				rust
		  ;; irony
		  ;; company-irony
		  ;; flycheck-irony
		  ;; company-irony-c-headers
        ))

(defun yelang-programming/init-sphinx-doc ()
  	  (use-package sphinx-doc
              :defer t
              ))

(defun yelang-programming/post-init-robe ()
  (progn
    (add-hook 'inf-ruby-mode-hook 'spacemacs/toggle-auto-completion-on)
    (defun yelang/ruby-send-current-line (&optional print)
      "Send the current line to the inferior Ruby process."
      (interactive "P")
      (ruby-send-region
       (line-beginning-position)
       (line-end-position))
      (when print (ruby-print-result)))

    (defun yelang/ruby-send-current-line-and-go ()
      (interactive)
      (yelang/ruby-send-current-line)
      (ruby-switch-to-inf t))

    (defun yelang/start-inf-ruby-and-robe ()
      (interactive)
      (when (not (get-buffer "*ruby*"))
        (inf-ruby))
      (robe-start))

    (dolist (mode '(ruby-mode enh-ruby-mode))
      (spacemacs/set-leader-keys-for-major-mode mode
        "sb" 'ruby-send-block
        "sB" 'ruby-send-buffer
        "sl" 'yelang/ruby-send-current-line
        "sL" 'yelang/ruby-send-current-line-and-go
        "sI" 'yelang/start-inf-ruby-and-robe))))


(defun yelang-programming/post-init-dumb-jump ()
  (setq dumb-jump-selector 'ivy)
  (defun my-dumb-jump ()
    (interactive)
    (evil-set-jump)
    (dumb-jump-go))
  (global-set-key (kbd "C-s-g") 'my-dumb-jump))


(defun yelang-programming/post-init-emacs-lisp ()
    (remove-hook 'emacs-lisp-mode-hook 'auto-compile-mode))


(defun yelang-programming/post-init-js-doc ()
  (setq js-doc-mail-address "huaming.li5012@gmai.com"
        js-doc-author (format "Yelang <%s>" js-doc-mail-address)
        js-doc-url "http://www.yelang.com"
        js-doc-license "MIT")

 (defun my-js-doc-insert-function-doc-snippet ()
    "Insert JsDoc style comment of the function with yasnippet."
    (interactive)

    (with-eval-after-load 'yasnippet
      (js-doc--beginning-of-defun)

      (let ((metadata (js-doc--function-doc-metadata))
            (field-count 1))
        (yas-expand-snippet
         (concat
          js-doc-top-line
          " * ${1:Function description.}\n"
          (format "* @method %s\n" (nth-value 1 (split-string (which-function) "\\.")))
          (mapconcat (lambda (param)
                       (format
                        " * @param {${%d:Type of %s}} %s - ${%d:Parameter description.}\n"
                        (incf field-count)
                        param
                        param
                        (incf field-count)))
                     (cdr (assoc 'params metadata))
                     "")
          (when (assoc 'returns metadata)
            (format
             " * @returns {${%d:Return Type}} ${%d:Return description.}\n"
             (incf field-count)
             (incf field-count)))
          (when (assoc 'throws metadata)
            (format
             " * @throws {${%d:Exception Type}} ${%d:Exception description.}\n"
             (incf field-count)
             (incf field-count)))
          js-doc-bottom-line))))))


(defun yelang-programming/init-ctags-update ()
  (use-package ctags-update
    :init
    :defer t
    :config
    (spacemacs|hide-lighter ctags-auto-update-mode)))

;; nodejs-repl is much better now.
;; (defun yelang-programming/init-js-comint ()
;;   (use-package js-comint
;;     :init
;;     (progn
;;       ;; http://stackoverflow.com/questions/13862471/using-node-js-with-js-comint-in-emacs
;;       (setq inferior-js-mode-hook
;;             (lambda ()
;;               ;; We like nice colors
;;               (ansi-color-for-comint-mode-on)
;;               ;; Deal with some prompt nonsense
;;               (add-to-list
;;                'comint-preoutput-filter-functions
;;                (lambda (output)
;;                  (replace-regexp-in-string "\033\\[[0-9]+[GKJ]" "" output)))))
;;       (setq inferior-js-program-command "node"))))

(defun yelang-programming/post-init-web-mode ()
  (with-eval-after-load "web-mode"
    (web-mode-toggle-current-element-highlight)
    (web-mode-dom-errors-show))
  (setq company-backends-web-mode '((company-dabbrev-code
                                     company-keywords
                                     company-etags)
                                    company-files company-dabbrev)))



(defun yelang-programming/post-init-yasnippet ()
  (progn
    (set-face-background 'secondary-selection "gray")
    (setq-default yas-prompt-functions '(yas-ido-prompt yas-dropdown-prompt))
    (mapc #'(lambda (hook) (remove-hook hook 'spacemacs/load-yasnippet)) '(prog-mode-hook
                                                                      org-mode-hook
                                                                      markdown-mode-hook))

    (spacemacs/add-to-hooks 'yelang/load-yasnippet '(prog-mode-hook
                                                            markdown-mode-hook
                                                            org-mode-hook))
    ))

(defun yelang-programming/post-init-racket-mode ()
  (progn
    (eval-after-load 'racket-repl-mode
      '(progn
         (define-key racket-repl-mode-map (kbd "]") nil)
         (define-key racket-repl-mode-map (kbd "[") nil)))

    (add-hook 'racket-mode-hook #'(lambda () (lispy-mode 1)))
    (add-hook 'racket-repl-mode-hook #'(lambda () (lispy-mode t)))
    ;; (add-hook 'racket-repl-mode-hook #'(lambda () (smartparens-mode t)))
    ))

(defun yelang-programming/post-init-json-mode ()
  (add-to-list 'auto-mode-alist '("\\.tern-project\\'" . json-mode))
  (add-to-list 'auto-mode-alist '("\\.fire\\'" . json-mode))
  (add-to-list 'auto-mode-alist '("\\.fire.meta\\'" . json-mode))
  (spacemacs/set-leader-keys-for-major-mode 'json-mode
    "ti" 'my-toggle-web-indent))


(defun yelang-programming/init-nodejs-repl ()
  (use-package nodejs-repl
    :init
    :defer t))

(defun yelang-programming/init-flycheck-package ()
  (use-package flycheck-package))

(defun yelang-programming/init-lispy ()
  (use-package lispy
    :defer t
    :init
    (progn
      (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
      (add-hook 'ielm-mode-hook (lambda () (lispy-mode 1)))
      (add-hook 'inferior-emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
      ;; (add-hook 'spacemacs-mode-hook (lambda () (lispy-mode 1)))
      (add-hook 'clojure-mode-hook (lambda () (lispy-mode 1)))
      (add-hook 'scheme-mode-hook (lambda () (lispy-mode 1)))
      (add-hook 'cider-repl-mode-hook (lambda () (lispy-mode 1)))
      )
    :config
    (progn
      (push '(cider-repl-mode . ("[`'~@]+" "#" "#\\?@?")) lispy-parens-preceding-syntax-alist)

      (spacemacs|hide-lighter lispy-mode)
      (define-key lispy-mode-map (kbd "s-j") 'lispy-splice)
      (define-key lispy-mode-map (kbd "s-k") 'paredit-splice-sexp-killing-backward)

      (with-eval-after-load 'cider-repl
        (define-key cider-repl-mode-map (kbd "C-s-j") 'cider-repl-newline-and-indent))

      (add-hook
       'minibuffer-setup-hook
       'conditionally-enable-lispy)
      (define-key lispy-mode-map (kbd "s-m") 'lispy-mark-symbol)
      (define-key lispy-mode-map (kbd "s-1") 'lispy-describe-inline)
      (define-key lispy-mode-map (kbd "s-2") 'lispy-arglist-inline))))


(defun yelang-programming/init-cmake-font-lock ()
  (use-package cmake-font-lock
    :defer t))

(defun yelang-programming/post-init-cmake-mode ()
   (use-package cmake-mode
    :mode (("CMakeLists\\.txt\\'" . cmake-mode) ("\\.cmake\\'" . cmake-mode))
    :init (push 'company-cmake company-backends-cmake-mode)))


(defun yelang-programming/post-init-flycheck ()
  (with-eval-after-load 'flycheck
    (progn
      (setq flycheck-display-errors-delay 0.9)
      (setq flycheck-idle-change-delay 2.0)
      )))

(defun yelang-programming/post-init-eldoc ()
  (setq eldoc-idle-delay 0.4))


(defun yelang-programming/post-init-js2-refactor ()
  (progn
    (spacemacs/set-leader-keys-for-major-mode 'js2-mode
      "r>" 'js2r-forward-slurp
      "r<" 'js2r-forward-barf)))

(defun yelang-programming/post-init-js2-mode ()
  (progn
    (add-hook 'js2-mode-hook 'my-setup-develop-environment)
    (add-hook 'web-mode-hook 'my-setup-develop-environment)

    (spacemacs|define-jump-handlers js2-mode)
    (add-hook 'spacemacs-jump-handlers-js2-mode 'etags-select-find-tag-at-point)

    (setq company-backends-js2-mode '((company-dabbrev-code :with company-keywords company-etags)
                                      company-files company-dabbrev))

    (yelang|toggle-company-backends company-tern)

    (spacemacs/set-leader-keys-for-major-mode 'js2-mode
      "tb" 'yelang/company-toggle-company-tern)

    (add-hook 'js2-mode-hook 'my-js2-mode-hook)

    ;; add your own keywords highlight here
    (font-lock-add-keywords 'js2-mode
                            '(("\\<\\(cc\\)\\>" 1 font-lock-type-face)))

    (spacemacs/declare-prefix-for-mode 'js2-mode "ms" "repl")

    (with-eval-after-load 'js2-mode
      (progn
        ;; these mode related variables must be in eval-after-load
        ;; https://github.com/magnars/.emacs.d/blob/master/settings/setup-js2-mode.el
        (setq-default js2-allow-rhino-new-expr-initializer nil)
        (setq-default js2-auto-indent-p nil)
        (setq-default js2-enter-indents-newline nil)
        (setq-default js2-global-externs '("module" "ccui" "require" "buster" "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON"))
        (setq-default js2-idle-timer-delay 0.2)
        (setq-default js2-mirror-mode nil)
        (setq-default js2-strict-inconsistent-return-warning nil)
        (setq-default js2-include-rhino-externs nil)
        (setq-default js2-include-gears-externs nil)
        (setq-default js2-concat-multiline-strings 'eol)
        (setq-default js2-rebind-eol-bol-keys nil)
        (setq-default js2-auto-indent-p t)

        (setq-default js2-bounce-indent nil)
        (setq-default js-indent-level 4)
        (setq-default js2-basic-offset 4)
        (setq-default js-switch-indent-offset 4)
        ;; Let flycheck handle parse errors
        (setq-default js2-mode-show-parse-errors nil)
        (setq-default js2-mode-show-strict-warnings nil)
        (setq-default js2-highlight-external-variables t)
        (setq-default js2-strict-trailing-comma-warning nil)

        (add-hook 'web-mode-hook 'my-web-mode-indent-setup)

        (spacemacs/set-leader-keys-for-major-mode 'js2-mode
          "ti" 'my-toggle-web-indent)
        (spacemacs/set-leader-keys-for-major-mode 'js-mode
          "ti" 'my-toggle-web-indent)
        (spacemacs/set-leader-keys-for-major-mode 'web-mode
          "ti" 'my-toggle-web-indent)
        (spacemacs/set-leader-keys-for-major-mode 'css-mode
          "ti" 'my-toggle-web-indent)

        (spacemacs/declare-prefix-for-mode 'js2-mode "mt" "toggle")
        (spacemacs/declare-prefix-for-mode 'js-mode "mt" "toggle")
        (spacemacs/declare-prefix-for-mode 'web-mode "mt" "toggle")
        (spacemacs/declare-prefix-for-mode 'css-mode "mt" "toggle")


        (eval-after-load 'tern-mode
          '(spacemacs|hide-lighter tern-mode))
        ))

    (evilified-state-evilify js2-error-buffer-mode js2-error-buffer-mode-map)

    ))

(defun yelang-programming/post-init-css-mode ()
  (progn
    (dolist (hook '(css-mode-hook sass-mode-hook less-mode-hook))
      (add-hook hook 'rainbow-mode))

    (defun css-imenu-make-index ()
      (save-excursion
        (imenu--generic-function '((nil "^ *\\([^ ]+\\) *{ *$" 1)))))

    (add-hook 'css-mode-hook
              (lambda ()
                (setq imenu-create-index-function 'css-imenu-make-index)))))

(defun yelang-programming/post-init-tagedit ()
  (add-hook 'web-mode-hook (lambda () (tagedit-mode 1))))

;; For each extension, define a function yelang/init-<extension-name>
;;
(defun yelang-programming/init-doxymacs ()
  "Initialize doxymacs"
  (use-package doxymacs
    :init
    (add-hook 'c-mode-common-hook 'doxymacs-mode)
    :config
    (progn
      (add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)
      (spacemacs|hide-lighter doxymacs-mode))))

;; https://atlanis.net/blog/posts/nodejs-repl-eval.html
(defun yelang-programming/init-nodejs-repl-eval ()
  (use-package nodejs-repl-eval
    :commands (nodejs-repl-eval-buffer nodejs-repl-eval-dwim nodejs-repl-eval-function)
    :init
    (progn
      (spacemacs/declare-prefix-for-mode 'js2-mode
                                         "ms" "REPL")
      (spacemacs/set-leader-keys-for-major-mode 'js2-mode
        "sb" 'nodejs-repl-eval-buffer
        "sf" 'nodejs-repl-eval-function
        "sd" 'nodejs-repl-eval-dwim))
    :defer t
    ))


(defun yelang-programming/post-init-cc-mode ()
  (progn
    ;; (setq company-backends-c-mode-common '((company-dabbrev-code :with company-keywords company-gtags company-etags)
    ;;                                        company-files company-dabbrev))
    ;; (add-hook 'c++-mode-hook 'my-setup-develop-environment)
    ;; (add-hook 'c-mode-hook 'my-setup-develop-environment)
		;; (add-hook 'c++-mode-hook 'ycmd-mode)
		;; (add-hook 'c++-mode-hook 'irony-mode)

    ;; http://stackoverflow.com/questions/23553881/emacs-indenting-of-c11-lambda-functions-cc-mode
    (defadvice c-lineup-arglist (around my activate)
      "Improve indentation of continued C++11 lambda function opened as argument."
      (setq ad-return-value
            (if (and (equal major-mode 'c++-mode)
                     (ignore-errors
                       (save-excursion
                         (goto-char (c-langelem-pos langelem))
                         ;; Detect "[...](" or "[...]{". preceded by "," or "(",
                         ;;   and with unclosed brace.
                         (looking-at ".*[(,][ \t]*\\[[^]]*\\][ \t]*[({][^}]*$"))))
                0                       ; no additional indent
              ad-do-it)))               ; default behavior


    (setq c-default-style "linux") ;; set style to "linux"
    (setq c-basic-offset 4)
    (c-set-offset 'substatement-open 0)
    ;; (with-eval-after-load 'c++-mode
    ;;   (define-key c++-mode-map (kbd "s-.") 'company-ycmd))
    )

  )

(defun yelang-programming/init-flycheck-clojure ()
  (use-package flycheck-clojure
    :defer t
    :init
    (eval-after-load 'flycheck '(flycheck-clojure-setup))))

;; (defun yelang-programming/post-init-ycmd ()
;;   (progn
;;     (setq ycmd-tag-files 'auto)
;;     (setq ycmd-request-message-level -1)
;;     (set-variable 'ycmd-server-command `("python" ,(expand-file-name "~/github/ycmd/ycmd/")))
;;     (setq company-backends-c-mode-common '((company-c-headers
;;                                             company-dabbrev-code
;;                                             company-keywords
;;                                             company-gtags :with company-yasnippet)
;;                                            company-files company-dabbrev ))

;;     (yelang|toggle-company-backends company-ycmd)
;;     (eval-after-load 'ycmd
;;       '(spacemacs|hide-lighter ycmd-mode))

;;     (spacemacs/set-leader-keys-for-major-mode 'c-mode
;;       "tb" 'yelang/company-toggle-company-ycmd)
;;     (spacemacs/set-leader-keys-for-major-mode 'c++-mode
;;       "tb" 'yelang/company-toggle-company-ycmd)
;; 	))

;; when many project has the need to use tags, I will give etags-table and etags-update a try
(defun yelang-programming/init-etags-select ()
  (use-package etags-select
    :init
    (progn
      (define-key evil-normal-state-map (kbd "gf")
        (lambda () (interactive) (find-tag (find-tag-default-as-regexp))))

      (define-key evil-normal-state-map (kbd "gb") 'pop-tag-mark)

      (define-key evil-normal-state-map (kbd "gn")
        (lambda () (interactive) (find-tag last-tag t)))

      (evilified-state-evilify etags-select-mode etags-select-mode-map)
      (spacemacs/set-leader-keys-for-major-mode 'js2-mode
        "gd" 'etags-select-find-tag-at-point))))

(defun yelang-programming/init-gulpjs ()
  (use-package gulpjs
    :init
    (progn
      (defun zilong/build-engine ()
        (interactive)
        (gulpjs-start-task-with-file-name "~/Github/fireball/app.js"))

      (spacemacs/set-leader-keys "ags" 'gulpjs-start-task)
      (spacemacs/set-leader-keys "agS" 'zilong/build-engine)
      (spacemacs/set-leader-keys "agr" 'gulpjs-restart-task))))


(defun yelang-programming/init-paredit ()
  (use-package paredit
    :commands (paredit-wrap-round
               paredit-wrap-square
               paredit-wrap-curly
               paredit-splice-sexp-killing-backward)
    :init
    (progn

      (bind-key* "s-(" #'paredit-wrap-round)
      (bind-key* "s-[" #'paredit-wrap-square)
      (bind-key* "s-{" #'paredit-wrap-curly)
      )))

(defun yelang-programming/post-init-company ()
  (progn
    (setq company-minimum-prefix-length 1
          company-idle-delay 0.08)

    (when (configuration-layer/package-usedp 'company)
      (spacemacs|add-company-backends :modes shell-script-mode makefile-bsdmake-mode sh-mode lua-mode nxml-mode conf-unix-mode json-mode graphviz-dot-mode))
    ))

(defun yelang-programming/init-irony ()
  (use-package irony
    :defer t
    :init
    (progn
      (add-hook 'c++-mode-hook 'irony-mode)
      (add-hook 'c-mode-hook 'irony-mode)
      (add-hook 'objc-mode-hook 'irony-mode)
      (add-hook 'irony-mode-hook
                (lambda ()
                  (define-key irony-mode-map [remap completion-at-point]
                    'irony-completion-at-point-async)
                  (define-key irony-mode-map [remap complete-symbol]
                    'irony-completion-at-point-async)))
      (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
      (spacemacs|diminish irony-mode " Ⓘ" " I"))))

(defun yelang-programming/init-company-irony ()
  (use-package company-irony
    :defer t
    :init
    (progn
	  (setq company-backends (delete 'company-semantic company-backends))
      (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
      (add-hook 'irony-mode-hook 'company-mode))))

(defun yelang-programming/init-flycheck-irony ()
  (use-package flycheck-irony
    ;; :defer t                            ; fix this ???
    :init
    (progn
      (eval-after-load 'flycheck
        '(add-to-list 'flycheck-checkers 'irony))
      (add-hook 'irony-mode-hook 'flycheck-mode))))

(defun yelang-programming/company-irony-c-headers ()
  (use-package company-irony
    :defer t
    :init
    (progn
	 (eval-after-load 'company
  '(add-to-list
    'company-backends '(company-irony-c-headers company-irony))) )))
