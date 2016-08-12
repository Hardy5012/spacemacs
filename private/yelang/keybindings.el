(global-set-key (kbd "C-c a") 'org-agenda)
(spacemacs/set-leader-keys "oll" 'yelang/load-my-layout)
(spacemacs/set-leader-keys "ols" 'yelang/save-my-layout)
(spacemacs/set-leader-keys "oy" 'youdao-dictionary-search-at-point+)
(spacemacs/set-leader-keys "op" 'yelang/org-save-and-export)
(if (configuration-layer/layer-usedp 'helm)
    (progn (global-set-key (kbd "<f1>") 'yelang/helm-hotspots)
           (spacemacs/set-leader-keys "oo" 'yelang/helm-hotspots)))

(bind-key* "M-s o" 'occur-dwim)
(bind-key* "C-=" 'er/expand-region)


(spacemacs/set-leader-keys "od" 'occur-dwim)

