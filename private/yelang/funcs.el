(require 'cl)
(defun yelang/load-my-layout ()
  (interactive)
  (persp-load-state-from-file (concat persp-save-dir "ylayouts")))

(defun yelang/save-my-layout ()
  (interactive)
  (persp-save-state-to-file (concat persp-save-dir "ylayouts")))
(defun yelang/helm-hotspots ()
  "helm interface to my hotspots, which includes my locations,
org-files and bookmarks"
  (interactive)
  (helm :buffer "*helm: utities*"
        :sources `(,(yelang//hotspots-sources))))

(defun yelang//hotspots-sources ()
  "Construct the helm sources for my hotspots"
  `((name . "Mail and News")
    (candidates . (("Calendar" . (lambda ()  (browse-url "https://www.google.com/calendar/render")))
                   ;;("RSS" . elfeed)
                   ("Blog" . org-octopress)
                   ;;("Github" . (lambda() (helm-github-stars)))
                   ;;("Calculator" . (lambda () (helm-calcul-expression)))
                   ;;("Run current flie" . (lambda () (yelang/run-current-file)))
                   ("Agenda" . (lambda () (org-agenda "" "a")))
                   ;;("sicp" . (lambda() (browse-url "http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-4.html#%_toc_start")))
                   ))
    (candidate-number-limit)
    (action . (("Open" . (lambda (x) (funcall x)))))))

;; Screenshot
(defun yelang//insert-org-or-md-img-link (prefix imagename)
  (if (equal (file-name-extension (buffer-file-name)) "org")
      (insert (format "[[%s%s]]" prefix imagename))
    (insert (format "![%s](%s%s)" imagename prefix imagename))))

(defun yelang/capture-screenshot (basename)
  "Take a screenshot into a time stamped unique-named file in the
  same directory as the org-buffer/markdown-buffer and insert a link to this file."
  (interactive "sScreenshot name: ")
  (if (equal basename "")
      (setq basename (format-time-string "%Y%m%d_%H%M%S")))
  (setq fullpath
        (concat (file-name-directory (buffer-file-name))
                "../source/img/"
                (file-name-base (buffer-file-name))
                "_"
                basename))
  (setq relativepath
        (concat (file-name-base (buffer-file-name))
                "_"
                basename
                ".png"))
  (if (file-exists-p (file-name-directory fullpath))
      (progn
        (setq final-image-full-path (concat fullpath ".png"))
        (call-process "screencapture" nil nil nil "-s" final-image-full-path)
        (if (executable-find "convert")
            (progn
              (setq resize-command-str (format "convert %s -resize 800x600 %s" final-image-full-path final-image-full-path))
              (shell-command-to-string resize-command-str)))
        (yelang//insert-org-or-md-img-link "https://zilongshanren.com/img/" relativepath))
    (progn
      (call-process "screencapture" nil nil nil "-s" (concat basename ".png"))
      (yelang//insert-org-or-md-img-link "./" (concat basename ".png"))))
  (insert "\n"))

(defun get-word-boundary ()
 "Return the boundary of the current word.
 The return value is of the form: (cons pos1 pos2).
 "
 (save-excursion
  (let (p1 p2)
   (progn
    (skip-chars-backward "-A-Za-z0-9_") ;; here you can choose which symbols to use
    (setq p1 (point))
    (skip-chars-forward "-A-Za-z0-9_") ;; put the same here
    (setq p2 (point)))
   (cons p1 p2)
  ))
)
(defun select-word ()
"Mark the url under cursor."
(interactive)
;  (require 'thingatpt)
(let (bds)
  (setq bds (get-word-boundary))

  (set-mark (car bds))
  (goto-char (cdr bds))
  )
)
(global-set-key [double-mouse-1] 'select-word)
