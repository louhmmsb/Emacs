(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives 
	     '("org" . "https://orgmode.org/elpa/"))

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(setq inhibit-startup-screen t)

(defun my-frame-tweaks (&optional frame)
  "My personal frame tweaks."
  (unless frame
    (setq frame (selected-frame)))
  (when frame
    (with-selected-frame frame
      (when (display-graphic-p)
	(tool-bar-mode -1)))))
;; For the case that the init file runs after the frame has been created.
;; Call of emacs without --daemon option.
(my-frame-tweaks) 
;; For the case that the init file runs before the frame is created.
;; Call of emacs with --daemon option.
(add-hook 'after-make-frame-functions #'my-frame-tweaks t)

(setq disabled-command-function nil)

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
  (c-set-offset 'case-label '+)
  (setq c-basic-offset 4)
  (setq tab-width 4)
  (setq indent-tabs-mode nil)
  (setq electric-pair-mode 1)
  )
(add-hook 'c++-mode-hook 'my-cc-style)
(add-hook 'c-mode-hook 'my-cc-style)
(add-hook 'c++-mode-hook 'company-mode)

(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

(setq python-shell-interpreter "python3")

(defun my-local-electric-pair-mode ()
  (make-variable-buffer-local 'electric-pair-mode)
  (electric-pair-mode +1)
  (make-variable-buffer-local 'fira-code-mode)
  ;;(fira-code-mode +1)
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
(add-hook 'dart-mode-hook 'my-local-electric-pair-mode)

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

(unless (boundp 'org-export-latex-classes)
  (setq org-export-latex-classes nil))
(add-to-list 'org-export-latex-classes
  '("beamer"
     "\\documentclass[11pt]{beamer}\n
      \\mode<{{{beamermode}}}>\n
      \\usetheme{{{{beamertheme}}}}\n
      \\usecolortheme{{{{beamercolortheme}}}}\n
      \\beamertemplateballitem\n
      \\setbeameroption{show notes}
      \\usepackage[utf8]{inputenc}\n
      \\usepackage[T1]{fontenc}\n
      \\usepackage{hyperref}\n
      \\usepackage{color}
      \\usepackage{listings}
      \\lstset{numbers=none,language=[ISO]C++,tabsize=4,
  frame=single,
  basicstyle=\\small,
  showspaces=false,showstringspaces=false,
  showtabs=false,
  keywordstyle=\\color{blue}\\bfseries,
  commentstyle=\\color{red},
  }\n
      \\usepackage{verbatim}\n
      \\institute{{{{beamerinstitute}}}}\n          
       \\subject{{{{beamersubject}}}}\n"

     ("\\section{%s}" . "\\section*{%s}")

     ("\\begin{frame}[fragile]\\frametitle{%s}"
       "\\end{frame}"
       "\\begin{frame}[fragile]\\frametitle{%s}"
       "\\end{frame}")))

  (add-to-list 'org-export-latex-classes

  '("letter"
     "\\documentclass[11pt]{letter}\n
      \\usepackage[utf8]{inputenc}\n
      \\usepackage[T1]{fontenc}\n
      \\usepackage{color}"

     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
     ("\\paragraph{%s}" . "\\paragraph*{%s}")
     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(setq js-switch-indent-offset 4)

(use-package lsp-mode :ensure t)
(use-package lsp-dart 
  :ensure t 
  :hook (dart-mode . lsp))
(with-eval-after-load 'projectile
  (add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml")
  (add-to-list 'projectile-project-root-files-bottom-up "BUILD"))
;;(require 'projectile)
;;(add-to-list 'projectile-project-root-files-bottom-up "pubspec.yaml")
;;(add-to-list 'projectile-project-root-files-bottom-up "BUILD")
(setq lsp-dart-flutter-sdk-dir "~/snap/flutter/common/flutter/")

(use-package hover
  :after dart-mode
  :bind (:map dart-mode-map
	      ("C-M-z" . #'hover-run-or-hot-reload)
	      ("C-M-x" . #'hover-run-or-hot-restart))
  :init
  (setq hover-hot-reload-on-save t))

(use-package lsp-mode
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
	 (dart-mode . lsp)
	 ;;(c++-mode . lsp)
	 ;; if you want which-key integration
	 (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)
;;(use-package lsp-ui :commands lsp-ui-mode)

(global-set-key (kbd "M-p") 'company-capf)

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 3))

(use-package company-irony
  :ensure t
  :config
  (require 'company)
  (add-to-list 'company-backends 'company-irony))

(use-package irony
  :ensure t
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(with-eval-after-load 'company
  (add-hook 'c++-mode-hook 'company-mode)
  (add-hook 'c-mode-hook 'company-mode))

(define-skeleton org-latex-skeleton
  "Skeleton for Latex exporting org files"
  "Title: "
  "#+TITLE: " str \n
  ;;"#+AUTHOR: " (setq v1 (skeleton-read "Author Name: ")) \n
  "#+AUTHOR: " (shell-command-to-string "git config user.name")
  "#+EMAIL: " (shell-command-to-string "git config user.email")
  "#+LANGUAGE: pt-br" \n \n
  "#+LATEX_HEADER: \\usepackage[hyperref, x11names]{xcolor}" \n
  "#+LATEX_HEADER: \\hypersetup{colorlinks = true, urlcolor = SteelBlue4, linkcolor = black}" \n
  "#+LATEX_HEADER: \\usepackage[AUTO]{babel}" \n
  "#+LATEX_HEADER: \\usepackage{geometry}" \n
  "#+LATEX_HEADER: \\geometry{verbose,a4paper,left=2cm,top=2cm,right=3cm,bottom=3cm}" \n
  "#+latex_class_options: [11pt]"
  )

(add-hook 'org-mode-hook
	  (lambda() (local-set-key "\C-csol" 'org-latex-skeleton)))

(define-skeleton c++-marathon-header-skeleton
  "Skleton for c++ marathon header"
  nil
  "#include<bits/stdc++.h>" \n
  "using namespace std;" \n
  "#define si(x) scanf(\"%d\", &x);" \n
  "#define pi(x) printf(\"%d\\n\", x);" \n
  "#define pb(x) push_back(x)" \n
  "#define mp(x, y) make_pair(x, y)" \n \n  
  "typedef long long int ll;" \n
  "typedef unsigned long long int ull;" \n \n
  _
  )

(add-hook 'c++-mode-hook
	  (lambda() (local-set-key "\C-cscm" 'c++-marathon-header-skeleton)))

(require 'direx)
(require 'popwin)
(push '(direx:direx-mode :position left :width 35 :dedicated t)
      popwin:special-display-config)
(global-set-key (kbd "C-x C-j") 'direx:jump-to-directory-other-window)
(popwin-mode 1)

(require 'server)
(unless (server-running-p)
  (server-start))
