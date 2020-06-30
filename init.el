(require 'package)
(package-initialize)
(org-babel-load-file "~/.emacs.d/settings.org")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(column-number-mode t)
 '(custom-enabled-themes nil)
 '(package-selected-packages
   (quote
    (yaml-mode org-bullets haskell-mode markdown-preview-mode markdown-mode ess web-mode org auctex ##))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Rekha" :foundry "PfEd" :slant normal :weight normal :height 98 :width normal))))
 '(font-lock-string-face ((t (:foreground "tomato" :slant italic)))))
