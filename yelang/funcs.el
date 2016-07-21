
(defun yelang/load-my-layout ()
  (interactive)
  (persp-load-state-from-file (concat persp-save-dir "ylayouts")))

(defun yelang/save-my-layout ()
  (interactive)
  (persp-save-state-to-file (concat persp-save-dir "ylayouts")))
