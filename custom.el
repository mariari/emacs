(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#504545" "#ad8572" "#a9df90" "#aaca86" "#91a0b3" "#ab85a3" "#afcfef" "#bdbdb3"])
 '(ansi-term-color-vector
   [unspecified "#3f3f3f" "#dca3a3" "#5f7f5f" "#e0cf9f" "#7cb8bb" "#dc8cc3" "#7cb8bb" "#dcdccc"] t)
 '(beacon-color "#eab4484b8035")
 '(clojure-align-binding-forms
   '("let" "when-let" "when-some" "if-let" "if-some" "binding" "loop" "doseq" "for" "with-open" "with-local-vars" "with-redefs" "bind"))
 '(clojure-align-cond-forms '("condp" "cond" "cond->" "cond->>" "case" "are" "match"))
 '(clojure-align-forms-automatically t)
 '(clojure-defun-indents '(defn defun bind))
 '(column-number-mode t)
 '(company-ghc-show-info t)
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(coq-prog-args
   '("-R" "/home/loli/Documents/Workspace/proofs/coq/cpdt/cpdt/src" "Cpdt"))
 '(custom-enabled-themes '(my-zenburn))
 '(custom-safe-themes
   '("e6df46d5085fde0ad56a46ef69ebb388193080cc9819e2d6024c9c6e27388ba9" "53269dd72d299a2c74adee2647b6314ad1ebe3ab627ab8655c353a17c4393835" default))
 '(evil-emacs-state-cursor '("#E57373" hbar) t)
 '(evil-insert-state-cursor '("#E57373" bar) t)
 '(evil-normal-state-cursor '("#FFEE58" box) t)
 '(evil-visual-state-cursor '("#C5E1A5" box) t)
 '(fci-rule-color "#383838")
 '(flycheck-disabled-checkers nil)
 '(flyspell-default-dictionary "en_US")
 '(fringe-mode '(1 . 1) nil (fringe))
 '(fstar-executable "fstar.exe")
 '(gdb-many-windows t t)
 '(glasses-separate-parentheses-p nil)
 '(gnus-logo-colors '("#2fdbde" "#c0c0c0") t)
 '(haskell-font-lock-symbols t)
 '(haskell-font-lock-symbols-alist
   '(("\\" . "λ")
     ("not" . "¬")
     ("()" . "∅")
     ("&&" . "∧")
     ("||" . "∨")
     ("sqrt" . "√")
     ("undefined" . "⊥")
     ("pi" . "π")
     ("~>" . "⇝")
     ("-<" . "↢")
     ("::" . "∷")
     ("." "∘" haskell-font-lock-dot-is-not-composition)
     ("forall" . "∀")))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines nil)
 '(helm-boring-file-regexp-list
   '("\\.checked$" "\\.hints$" "\\.beam$" "\\.vee$" "\\.jam$" "\\.hi$" "\\.pho$" "\\.phi$" "\\.glob$" "\\.vo$" "\\.cmti$" "\\.cmt$" "\\.annot$" "\\.cmi$" "\\.cmxa$" "\\.cma$" "\\.cmx$" "\\.cmo$" "\\.o$" "~$" "\\.bin$" "\\.lbin$" "\\.so$" "\\.a$" "\\.ln$" "\\.blg$" "\\.bbl$" "\\.elc$" "\\.lof$" "\\.glo$" "\\.idx$" "\\.lot$" "\\.svn\\(/\\|$\\)" "\\.hg\\(/\\|$\\)" "\\.git\\(/\\|$\\)" "\\.bzr\\(/\\|$\\)" "CVS\\(/\\|$\\)" "_darcs\\(/\\|$\\)" "_MTN\\(/\\|$\\)" "\\.fmt$" "\\.tfm$" "\\.class$" "\\.fas$" "\\.lib$" "\\.mem$" "\\.x86f$" "\\.sparcf$" "\\.dfsl$" "\\.pfsl$" "\\.d64fsl$" "\\.p64fsl$" "\\.lx64fsl$" "\\.lx32fsl$" "\\.dx64fsl$" "\\.dx32fsl$" "\\.fx64fsl$" "\\.fx32fsl$" "\\.sx64fsl$" "\\.sx32fsl$" "\\.wx64fsl$" "\\.wx32fsl$" "\\.fasl$" "\\.ufsl$" "\\.fsl$" "\\.dxl$" "\\.lo$" "\\.la$" "\\.gmo$" "\\.mo$" "\\.toc$" "\\.aux$" "\\.cp$" "\\.fn$" "\\.ky$" "\\.pg$" "\\.tp$" "\\.vr$" "\\.cps$" "\\.fns$" "\\.kys$" "\\.pgs$" "\\.tps$" "\\.vrs$" "\\.pyc$" "\\.pyo$"))
 '(highlight-indent-guides-auto-enabled nil)
 '(highlight-parentheses-background-colors '(nil))
 '(highlight-parentheses-colors '("#45c8e7" "#c666b4" "#22c986"))
 '(highlight-symbol-colors
   '("#FFEE58" "#C5E1A5" "#80DEEA" "#64B5F6" "#E1BEE7" "#FFCC80"))
 '(highlight-symbol-foreground-color "#E0E0E0")
 '(highlight-tail-colors '(("#eab4484b8035" . 0) ("#424242" . 100)))
 '(inhibit-startup-screen t)
 '(intero-pop-to-repl nil)
 '(jdee-server-dir "~/Programming/Java/target")
 '(linum-format 'dynamic)
 '(lisp-lambda-list-keyword-alignment t)
 '(lisp-lambda-list-keyword-parameter-alignment t)
 '(lispy-no-space t)
 '(main-line-color1 "#191919")
 '(main-line-color2 "#111111")
 '(matlab-fill-code nil)
 '(matlab-fill-fudge-hard-maximum 110)
 '(merlin-eldoc-occurrences nil)
 '(muse-project-alist nil)
 '(nrepl-message-colors
   '("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3"))
 '(nyan-wavy-trail t)
 '(org-agenda-files
   '("~/Notes/notes.org" "~/Notes/Cooking.org" "~/Documents/Notes/Org Notes/Misc/Research/articles.org"))
 '(org-pretty-entities t)
 '(org-startup-indented t)
 '(package-selected-packages
   '(dired-ranger ranger zenburn-theme erc-colorize erc-hl-nicks erc-image erc-yt eldoc scheme-complete geiser-racket geiser-chez ac-geiser geiser geiser-mit macrostep-geiser gemini-mode bnf-mode fuel poporg racket-mode edit-indirect hledger-mode ledger-mode transient ivy company-ledger use-package esup vterm popwin tuareg xah-math-input undo-tree smartparens sly-macrostep sly-asdf rainbow-mode rainbow-delimiters nyan-mode merlin-eldoc magit lsp-ui lsp-haskell lispy linum-relative iy-go-to-char highlight-parentheses helm-company flycheck evil company-coq))
 '(pdf-view-midnight-colors '("#DCDCCC" . "#383838"))
 '(pos-tip-background-color "#3a513a513a51")
 '(pos-tip-foreground-color "#9E9E9E")
 '(powerline-color1 "#191919")
 '(powerline-color2 "#111111")
 '(proof-multiple-frames-enable t)
 '(rainbow-identifiers-cie-l*a*b*-lightness 30)
 '(rainbow-identifiers-cie-l*a*b*-saturation 35)
 '(safe-local-variable-values
   '((eval setq byte-compile-not-obsolete-vars
      '(display-buffer-function))
     (eval when
      (require 'rainbow-mode nil t)
      (rainbow-mode 1))
     (eval cl-flet
      ((enhance-imenu-lisp
        (&rest keywords)
        (dolist
            (keyword keywords)
          (add-to-list 'lisp-imenu-generic-expression
                       (list
                        (purecopy
                         (concat
                          (capitalize keyword)
                          (if
                              (string=
                               (substring-no-properties keyword -1)
                               "s")
                              "es" "s")))
                        (purecopy
                         (concat "^\\s-*("
                                 (regexp-opt
                                  (list
                                   (concat "define-" keyword))
                                  t)
                                 "\\s-+\\(" lisp-mode-symbol-regexp "\\)"))
                        2)))))
      (enhance-imenu-lisp "bookmarklet-command" "class" "command" "ffi-method" "function" "mode" "parenscript" "user-class"))
     (Base . 10)
     (Syntax . Common-Lisp)
     (Package . Maxima)
     (eval cl-flet
      ((enhance-imenu-lisp
        (&rest keywords)
        (dolist
            (keyword keywords)
          (add-to-list 'lisp-imenu-generic-expression
                       (list
                        (purecopy
                         (concat
                          (capitalize keyword)
                          (if
                              (string=
                               (substring-no-properties keyword -1)
                               "s")
                              "es" "s")))
                        (purecopy
                         (concat "^\\s-*("
                                 (regexp-opt
                                  (list
                                   (concat "define-" keyword))
                                  t)
                                 "\\s-+\\(" lisp-mode-symbol-regexp "\\)"))
                        2)))))
      (enhance-imenu-lisp "bookmarklet-command" "class" "command" "function" "mode" "parenscript" "user-class"))
     (Syntax . ANSI-Common-Lisp)
     (Package . FSet)))
 '(scroll-bar-mode nil)
 '(speedbar-show-unknown-files t)
 '(sr-speedbar-auto-refresh nil)
 '(sr-speedbar-default-width 27)
 '(sr-speedbar-max-width 30)
 '(sr-speedbar-right-side t)
 '(sr-speedbar-skip-other-window-p nil)
 '(tabbar-background-color "#353335333533")
 '(tool-bar-mode nil)
 '(tuareg-display-buffer-on-eval nil)
 '(tuareg-indent-align-with-first-arg t)
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   '((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3")))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(when (not (facep (aref ansi-term-color-vector 0)))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "PragmataPro" :foundry "fsdf" :slant normal :weight normal :height 113 :width normal))))
 '(erc-nick-default-face ((t (:foreground "orchid" :weight bold)))))
