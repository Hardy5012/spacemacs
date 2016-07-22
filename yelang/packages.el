;;; packages.el --- yelang layer packages file for Spacemacs.
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
;; added to `yelang-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `yelang/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `yelang/pre-init-PACKAGE' and/or
;;   `yelang/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst yelang-packages
  '(
    evil
    org
    org-octopress
    )
  "The list of Lisp packages required by the yelang layer.

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

(defun yelang/post-init-evil ()
  (progn
    (setcdr evil-insert-state-map nil)
    (define-key evil-insert-state-map [escape] 'evil-normal-state)
    )
  )
(defun yelang/post-init-org ()
  (setq org-agenda-dir "~/Dropbox/org-notes")
  (setq org-agenda-file-gtd (expand-file-name "gtd.org" org-agenda-dir))
  (setq org-agenda-file-note (expand-file-name "note.org" org-agenda-dir))
  (setq org-agenda-file-gtd-archive (expand-file-name "gtd.org_archive.org" org-agenda-dir))
  (setq org-agenda-files `(,org-agenda-file-gtd ,org-agenda-file-gtd-archive ,org-agenda-file-note))

  (setq org-default-notes-file org-agenda-file-gtd)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "INBOX(i)" "|" "WAITTING(w)" "NOTE(n)""DONE(d)")
          (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
          (sequence "|" "CANCELLED(c)")))

  (setq org-refile-targets
        '(("gtd.org" :maxlevel . 1)))

  (setq org-log-into-drawer t)

  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline org-agenda-file-gtd "Daily Tasks")
           "* TODO %?\n  %i\n"
           :empty-lines 1)
          ("i" "Inbox" entry (file+headline org-agenda-file-gtd "Inbox")
           "* INBOX %?\n  %i\n"
           :empty-lines 1)
          ("n" "Quick Notes" entry (file+headline org-agenda-file-note "Quick notes")
           "* NOTE %?\n  %i\n %U"
           :empty-lines 1)
          ("b" "Blog Ideas" entry (file+headline org-agenda-file-gtd "Blog Ideas")
           "* TODO %?\n  %i\n %U"
           :empty-lines 1)
          ("w" "work" entry (file+headline org-agenda-file-gtd "Programming")
           "* TODO %?\n  %i\n %U"
           :empty-lines 1)
          ("j" "Journal Entry"
           entry (file+datetree "~/Dropbox/org-notes/journal.org")
           "* %?"
           :empty-lines 1)))

  (setq org-agenda-custom-commands
        '(
          ("i" "Inbox" todo "INBOX")
          ("w" . " 任务安排 ")
          ("wa" " 重要且紧急的任务 " tags-todo "+PRIORITY=\"A\"")
          ("wb" " 重要且不紧急的任务 " tags-todo "-Weekly-Monthly-Daily+PRIORITY=\"B\"")
          ("wc" " 不重要且紧急的任务 " tags-todo "+PRIORITY=\"C\"")
          ("b" "Blog" tags-todo "BLOG")
          ("p" . " 项目安排 ")
          ("pw" tags-todo "PROJECT+WORK+CATEGORY=\"programming\"")
          ("pl" tags-todo "PROJECT+DREAM+CATEGORY=\"codefalling\"")
          ("W" "Weekly Review"
           ((stuck "")            ;; review stuck projects as designated by org-stuck-projects
            (tags-todo "PROJECT") ;; review all projects (assuming you use todo keywords to designate projects)
            ))))

  (defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-log-states)  ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

  (add-hook'org-after-todo-statistics-hook 'org-summary-todo)
  ;; used by org-clock-sum-today-by-tags
  (defun filter-by-tags ()
    (let ((head-tags (org-get-tags-at)))
      (member current-tag head-tags)))

  (defun org-clock-sum-today-by-tags (timerange &optional tstart tend noinsert)
    (interactive "P")
    (let* ((timerange-numeric-value (prefix-numeric-value timerange))
           (files (org-add-archive-files (org-agenda-files)))
           (include-tags'("PROG" "EMACS" "DREAM" "WRITING" "MEETING" "BLOG"
                          "LIFE" "PROJECT"))
           (tags-time-alist (mapcar (lambda (tag) `(,tag . 0)) include-tags))
           (output-string "")
           (tstart (or tstart
                       (and timerange (equal timerange-numeric-value 4) (- (org-time-today) 86400))
                       (and timerange (equal timerange-numeric-value 16) (org-read-date nil nil nil "Start Date/Time:"))
                       (org-time-today)))
           (tend (or tend
                     (and timerange (equal timerange-numeric-value 16) (org-read-date nil nil nil "End Date/Time:"))
                     (+ tstart 86400)))
           h m file item prompt donesomething)
      (while (setq file (pop files))
        (setq org-agenda-buffer (if (file-exists-p file)
                                    (org-get-agenda-file-buffer file)
                                  (error "No such file %s" file)))
        (with-current-buffer org-agenda-buffer
          (dolist (current-tag include-tags)
            (org-clock-sum tstart tend'filter-by-tags)
            (setcdr (assoc current-tag tags-time-alist)
                    (+ org-clock-file-total-minutes (cdr (assoc current-tag tags-time-alist)))))))
      (while (setq item (pop tags-time-alist))
        (unless (equal (cdr item) 0)
          (setq donesomething t)
          (setq h (/ (cdr item) 60)
                m (- (cdr item) (* 60 h)))
          (setq output-string (concat output-string (format "[-%s-] %.2d:%.2d\n" (car item) h m)))))
      (unless donesomething
        (setq output-string (concat output-string "[-Nothing-] Done nothing!!!\n")))
      (unless noinsert
        (insert output-string))
      output-string))

  (eval-after-load 'org
    '(progn
       (global-set-key (kbd "C-c a") 'org-agenda)
       (define-key org-mode-map (kbd "s-p") 'org-priority)
       (define-key global-map (kbd "<f9>") 'org-capture)
       (global-set-key (kbd "C-c b") 'org-iswitchb)
       (define-key evil-normal-state-map (kbd "C-c C-w") 'org-refile)
       (evil-leader/set-key-for-mode'org-mode
        "owh" 'plain-org-wiki-helm
        "owf" 'plain-org-wiki)
       (require 'ob-js)
       (require 'ob-shell)
       )
    )
  ;; Resume clocking task when emacs is restarted
  (org-clock-persistence-insinuate)
  ;; Save the running clock and all clock history when exiting Emacs, load it on startup
  (setq org-clock-persist t)
  ;; Do not prompt to resume an active clock
  (setq org-clock-persist-query-resume nil)

  ;; Sync with google calander
  (setq org-caldav-url "https://www.google.com/calendar/dav")
  (setq org-caldav-calendar-id "huaming.li5012@gmail.com")
  (setq org-caldav-files org-agenda-files)
  (setq org-icalendar-date-time-format ";TZID=%Z:%Y%m%dT%H%M%S")

  )
(defun yelang/init-org-octopress ()
  (use-package org-octopress
    :commands (org-octopress org-octopress-setup-publish-project)
    :init
    (progn
      (evilified-state-evilify org-octopress-summary-mode org-octopress-summary-mode-map)
      (add-hook 'org-octopress-summary-mode-hook
                #'(lambda () (local-set-key (kbd "q") 'bury-buffer)))
      (setq org-blog-dir "/home/huaming_li/github/blog/")
      (setq org-octopress-directory-top org-blog-dir)
      (setq org-octopress-directory-posts (concat org-blog-dir "source/_posts"))
      (setq org-octopress-directory-org-top org-blog-dir)
      (setq org-octopress-directory-org-posts (concat org-blog-dir "blog"))
      (setq org-octopress-setup-file (concat org-blog-dir "setupfile.org"))

      (defun yelang/org-save-and-export ()
        (interactive)
        (org-octopress-setup-publish-project)
        (org-publish-project "octopress" t))

      )))
;;; packages.el ends here
