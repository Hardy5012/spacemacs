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
    cmake-mode
    company
    company-c-headers
    ;; irony
    ;; company-irony
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
    ;; (setq company-async-timeout 5)
    ;; (setq company-backends-c-mode-common '((company-dabbrev-code :with company-keywords company-etags)
    ;;                                         company-files company-dabbrev))
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
(defun yt-cc/init-cmake-mode ()
  (use-package cmake-mode
    :mode (("CMakeLists\\.txt\\'" . cmake-mode) ("\\.cmake\\'" . cmake-mode))
    :init (push 'company-cmake company-backends-cmake-mode)))

;; (defun yt-cc/init-irony ()
;;   (use-package irony 
;;     :diminish irony-mode
;;     :defer t
;;     :init
;;     (progn
;;       (add-hook 'c++-mode-hook 'irony-mode)
;;       (add-hook 'c-mode-hook 'irony-mode)
;;       ;;see https://github.com/Sarcasm/irony-mode/issues/154#issuecomment-100649914
;;       ;;just use .clang_complete from now on
;;       ;; cannnot support json format. it is unstable at <2015-05-11 一>


;;       ;; replace the 'completion at point ' and 'complete-symbol' bindings in
;;       ;; irony mode's buffers ny irony-mode's function
;;       (defun my-irony-mode-hook ()
;;         (define-key irony-mode-map [remap completion-at-point]
;;           'irony-completion-at-point-async)
;;         (define-key irony-mode-map [remap complete-symbol]
;;           'irony-completion-at-point-async))
;;       (add-hook 'irony-mode-hook 'my-irony-mode-hook)
;;       (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
;;       (spacemacs|diminish irony-mode " Ⓘ" " I"))))

 ;; (defun yt-cc/init-company-irony ()
 ;;   (use-package company-irony
 ;;     :defer t)
 ;;   ;; (push '(company-irony company-yasnippet company-keywords company-gtags) company-backends-c-mode-common)
 ;;   )

;; (when (configuration-layer/layer-usedp 'auto-completion)
;;   (defun yt-cc/post-init-company ()
;;     ;; push this backend by default
;;     ;; (push '(company-irony :with company-yasnippet)
;;     ;;       company-backends-c-mode-common)
;;     (push 'company-irony company-backends-c-mode-common)
;;     (spacemacs|add-company-hook c-mode-common)
;;     (spacemacs|add-company-hook cmake-mode)
;;     (setq company-idle-delay 0.08)
;;     (setq company-minimum-prefix-length 1)
;;     ;; .clang_complete file loading
;;     ;; Sets the arguments for company-clang based on a project-specific text file.

;;     ;; START Based on the Sarcasm/irony-mode compilation database code.
;;     (defun company-mode/find-clang-complete-file ()
;;       (when buffer-file-name
;;         (let ((dir (locate-dominating-file buffer-file-name ".clang_complete")))
;;           (when dir
;;             (concat (file-name-as-directory dir) ".clang_complete")))))

;;     (defun company-mode/load-clang-complete-file (cc-file)
;;       "Load the flags from CC-FILE, one flag per line."
;;       (let ((invocation-dir (expand-file-name (file-name-directory cc-file)))
;;             (case-fold-search nil)
;;             compile-flags)
;;         (with-temp-buffer
;;           (insert-file-contents cc-file)
;;           ;; Replace relative paths with absolute paths (by @trishume)
;;           ;; (goto-char (point-min))
;;           (while (re-search-forward "\\(-I\\|-isystem\n\\)\\(\\S-\\)" nil t)
;;             (replace-match (format "%s%s" (match-string 1)
;;                                    (expand-file-name (match-string 2) invocation-dir))))
;;           ;; Trn lines into a list
;;           (setq compile-flags
;;                 ;; remove whitespaces at the end of each line, if any
;;                 (mapcar #'(lambda (line)
;;                             (if (string-match "[ \t]+$" line)
;;                                 (replace-match "" t t line)
;;                               line))
;;                         (split-string (buffer-string) "\n" t))))
;;         compile-flags))
;;     ;; END Back to things written by @trishume

;;     (defun company-mode/more-than-prefix-guesser ()
;;       (unless company-clang-arguments
;;         (let* ((cc-file (company-mode/find-clang-complete-file))
;;                (flags (if cc-file (company-mode/load-clang-complete-file cc-file) '())))
;;           (setq-local company-clang-arguments flags)
;;           (setq flycheck-clang-args flags)))
;;       (company-clang-guess-prefix))

;;     (setq company-clang-prefix-guesser 'company-mode/more-than-prefix-guesser))

;;     (defun yt-cc/init-company-c-headers ()
;;     (use-package company-c-headers
;;       :if (configuration-layer/package-usedp 'company)
;;       :defer t
;;       :init (push 'company-c-headers company-backends-c-mode-common)))
;;   ;; (defun c-c++/init-gdb-mi ()
;;   ;;   (use-package gdb-mi
;;   ;;     :defer t
;;   ;;     :init
;;   ;;     (setq
;;   ;;      ;; use gdb-many-windows by default when `M-x gdb'
;;   ;;      gdb-many-windows t
;;   ;;      ;; Non-nil means display source file containing the main routine at startup
;;   ;;      gdb-show-main t)))
;;     )
;;; packages.el ends here
