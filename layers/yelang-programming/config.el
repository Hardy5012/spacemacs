(add-hook 'python-mode-hook
          (lambda ()
            (which-key-add-major-mode-key-based-replacements 'python-mode
              "C-c r" "anaconda find reference cmds"
              "C-c C-t" "skeleton"
              "C-c !" "flycheck")
	(set-variable 'python-indent-guess-indent-offset nil)
            (define-key python-mode-map (kbd "C-c C-b") 'python-add-breakpoint)
            (define-key python-mode-map (kbd "C-c C-d") 'sphinx-doc)
            (define-key python-mode-map (kbd "C-c C-c") 'python-shell-send-buffer-switch)))

(spacemacs|add-toggle iimage
  :status iimage-mode
  :on (iimage-mode)
  :off (iimage-mode -1)
  :documentation "Enable iimage mode"
  :evil-leader "oti")

(add-hook 'term-mode-hook 'yelang/ash-term-hooks)

;; reformat your json file, it requires python
(defun beautify-json ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
                             "python -mjson.tool" (current-buffer) t)))




(add-to-list 'auto-mode-alist (cons (concat "\\." (regexp-opt
                                                   '("xml"
                                                     "xsd"
                                                     "rng"
                                                     "xslt"
                                                     "xsl")
                                                   t) "\\'") 'nxml-mode))
(setq nxml-slash-auto-complete-flag t)



(add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))
(add-to-list 'auto-mode-alist '("\\.c\\'" . c++-mode))



;; return nil to write content to file
(defun yelang/untabify-buffer ()
  (interactive)
  (save-excursion
    (untabify (point-min) (point-max)) nil))

(add-hook 'c++-mode-hook
          #'(lambda ()
             (add-hook 'write-contents-hooks
                       'yelang/untabify-buffer nil t)))

(setq auto-mode-alist
      (append
       '(("\\.mak\\'" . makefile-bsdmake-mode))
       auto-mode-alist))


(defmacro yelang|toggle-company-backends (backend)
  "Push or delete the backend to company-backends"
  (let ((funsymbol (intern (format "yelang/company-toggle-%S" backend))))
    `(defun ,funsymbol ()
       (interactive)
       (if (eq (car company-backends) ',backend)
           (setq-local company-backends (delete ',backend company-backends))
         (push ',backend company-backends)))))
