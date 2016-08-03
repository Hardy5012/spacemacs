;;; packages.el --- yt-cc layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: huaming_li <huaming_li@localhost>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `yt-cc-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `yt-cc/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `yt-cc/pre-init-PACKAGE' and/or
;;   `yt-cc/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst yt-cc-packages
  '(
    (cc-mode :location built-in)
    )
  "The list of Lisp packages required by the yt-cc layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(defun yt-cc/post-init-cc-mode ()
  (progn
    ;; (setq company-backends-c-mode-common '((company-dabbrev-code :with company-keywords company-etags)
    ;;                                        company-files company-dabbrev))
    ;; (spacemacs/set-leader-keys-for-major-mode 'c++-mode
    ;;   "gd" 'etags-select-find-tag-at-point)

    ;; (defun my-project-name-contains-substring (REGEX)
    ;;   (let ((dir (if (buffer-file-name)
    ;;                  (file-name-directory (buffer-file-name))
    ;;                "")))
    ;;     (string-match-p REGEX dir)))

    ;; (defun my-create-tags-if-needed (SRC-DIR &optional FORCE)
    ;;   "return the full path of tags file"
    ;;   (let ((dir (file-name-as-directory (file-truename SRC-DIR)))
    ;;         file)
    ;;     (setq file (concat dir "TAGS"))
    ;;     (when (or FORCE (not (file-exists-p file)))
    ;;       (message "Creating TAGS in %s ..." dir)
    ;;       (shell-command
    ;;        (format "ctags -f %s -e -R %s" file dir)))
    ;;     file))

    ;; (defvar my-tags-updated-time nil)

    ;; (defun my-update-tags ()
    ;;   (interactive)
    ;;   "check the tags in tags-table-list and re-create it"
    ;;   (dolist (tag tags-table-list)
    ;;     (my-create-tags-if-needed (file-name-directory tag) t)))

    ;; (defun my-auto-update-tags-when-save ()
    ;;   (interactive)
    ;;   (cond
    ;;    ((not my-tags-updated-time)
    ;;     (setq my-tags-updated-time (current-time)))
    ;;    ((< (- (float-time (current-time)) (float-time my-tags-updated-time)) 300)
    ;;     ;; < 300 seconds
    ;;     ;; do nothing
    ;;     )
    ;;    (t
    ;;     (setq my-tags-updated-time (current-time))
    ;;     (my-update-tags)
    ;;     (message "updated tags after %d seconds." (- (float-time (current-time)) (float-time my-tags-updated-time))))))

    ;; (defun my-setup-develop-environment ()
    ;;   (interactive)
    ;;   (when (my-project-name-contains-substring "guanghui")
    ;;     (cond
    ;;      ((my-project-name-contains-substring "cocos2d-x")
    ;;       ;; C++ project don't need html tags
    ;;       (setq tags-table-list (list (my-create-tags-if-needed "~/cocos2d-x/cocos"))))
    ;;      ((my-project-name-contains-substring "Github/fireball")
    ;;       (message "load tags for fireball engine repo...")
    ;;       ;; html project donot need C++ tags
    ;;       (setq tags-table-list (list (my-create-tags-if-needed "~/Github/fireball/engine/cocos2d")))))))

    ;; (add-hook 'after-save-hook 'my-auto-update-tags-when-save)
    ;; (add-hook 'js2-mode-hook 'my-setup-develop-environment)
    ;; (add-hook 'web-mode-hook 'my-setup-develop-environment)
    ;; (add-hook 'c++-mode-hook 'my-setup-develop-environment)
    ;; (add-hook 'c-mode-hook 'my-setup-develop-environment)


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
  ;; company backend should be grouped
  )

;;; packages.el ends here
