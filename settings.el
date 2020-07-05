(add-to-list 'package-archives
'("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives 
'("org" . "https://orgmode.org/elpa/"))

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(setq inhibit-startup-screen t)

(setq make-backup-files nil)
(setq auto-save-default nil)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'dracula t)
(setq-default column-number-mode t)
(setq-default display-line-numbers t)
(add-to-list 'default-frame-alist' (font . "Fira Code Retina-12"))
(add-to-list 'default-frame-alist' (set-background-color "#292d3e"))
(add-to-list 'default-frame-alist' (set-foreground-color "#bbc5ff"))
(set-background-color "#292d3e")
(set-foreground-color "#bbc5ff")

(setq dracula-enlarge-headings nil)

(defun fira-code-mode--make-alist (list)
"Generate prettify-symbols alist from LIST."
(let ((idx -1))
(mapcar
(lambda (s)
(setq idx (1+ idx))
(let* ((code (+ #Xe100 idx))
(width (string-width s))
(prefix ())
(suffix '(?\s (Br . Br)))
(n 1))
(while (< n width)
(setq prefix (append prefix '(?\s (Br . Bl))))
(setq n (1+ n)))
(cons s (append prefix suffix (list (decode-char 'ucs code))))))
list)))

(defconst fira-code-mode--ligatures
'("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\"
"{-" "[]" "::" ":::" ":=" "!!" "!=" "!==" "-}"
"--" "---" "-->" "->" "->>" "-<" "-<<" "-~"
"#{" "#[" "##" "###" "####" "#(" "#?" "#_" "#_("
".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*"
"/**" "/=" "/==" "/>" "//" "///" "&&" "||" "||="
"|=" "|>" "^=" "$>" "++" "+++" "+>" "=:=" "=="
"===" "==>" "=>" "=>>" "<=" "=<<" "=/=" ">-" ">="
">=>" ">>" ">>-" ">>=" ">>>" "<*" "<*>" "<|" "<|>"
"<$" "<$>" "<!--" "<-" "<--" "<->" "<+" "<+>" "<="
"<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<" "<~"
"<~~" "</" "</>" "~@" "~-" "~=" "~>" "~~" "~~>" "%%"
))

(defvar fira-code-mode--old-prettify-alist)



(defun fira-code-mode--enable ()
"Enable Fira Code ligatures in current buffer."
(setq-local fira-code-mode--old-prettify-alist prettify-symbols-alist)
(setq-local prettify-symbols-alist (append (fira-code-mode--make-alist fira-code-mode--ligatures) fira-code-mode--old-prettify-alist))
(prettify-symbols-mode t))

(defun fira-code-mode--disable ()
"Disable Fira Code ligatures in current buffer."
(setq-local prettify-symbols-alist fira-code-mode--old-prettify-alist)
(prettify-symbols-mode -1))

(define-minor-mode fira-code-mode
"Fira Code ligatures minor mode"
:lighter " Fira Code"
(setq-local prettify-symbols-unprettify-at-point 'right-edge)
(if fira-code-mode
(fira-code-mode--enable)
(fira-code-mode--disable)))

(defun fira-code-mode--setup ()
"Setup Fira Code Symbols"
(set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol"))

(provide 'fira-code-mode)

(defun my-cc-style()
(c-set-style "linux")
(c-set-offset 'innamespace '4)
(c-set-offset 'inextern-lang '0)
(c-set-offset 'inline-open '0)
(c-set-offset 'label '*)
(c-set-offset 'access-label '/)
(setq c-basic-offset 4)
(setq tab-width 4)
(setq indent-tabs-mode nil)
(setq electric-pair-mode 1)
)
(add-hook 'c++-mode-hook 'my-cc-style)
(add-hook 'c-mode-hook 'my-cc-style)

(setq python-shell-interpreter "python3")

(defun my-local-electric-pair-mode ()
(make-variable-buffer-local 'electric-pair-mode)
(electric-pair-mode +1)
(make-variable-buffer-local 'fira-code-mode)
(fira-code-mode +1)
(make-variable-buffer-local 'flymake-mode)
(flymake-mode +1))

(add-hook 'TeX-mode-hook 'my-local-electric-pair-mode)
(add-hook 'Emacs-Lisp-mode-hook 'my-local-electric-pair-mode)
(add-hook 'c-mode-hook 'my-local-electric-pair-mode)
(add-hook 'sh-mode-hook 'my-local-electric-pair-mode)
(add-hook 'html-mode-hook 'my-local-electric-pair-mode)
(add-hook 'js-mode-hook 'my-local-electric-pair-mode)
(add-hook 'c++-mode-hook 'my-local-electric-pair-mode)
(add-hook 'python-mode-hook 'my-local-electric-pair-mode)

(defun my_org_style()
(make-variable-buffer-local 'org-bullet-mode)
(org-bullets-mode))
(add-hook 'org-mode-hook 'my_org_style)

(require 'org)
(require 'ox-latex)
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted) 

(setq org-latex-pdf-process
'("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
"pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
"pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(setq org-src-fontify-natively t)

(org-babel-do-load-languages
'org-babel-load-languages
'((R . t)
(latex . t)))

(org-babel-do-load-languages
'org-babel-load-languages
'(
(C . t)
(haskell . t)
(python . t)))

(setq js-switch-indent-offset 4)

(require 'direx)
(require 'popwin)
(push '(direx:direx-mode :position left :width 35 :dedicated t)
popwin:special-display-config)
(global-set-key (kbd "C-x C-j") 'direx:jump-to-directory-other-window)
(popwin-mode 1)
