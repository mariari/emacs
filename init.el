;; Commentary:
;;; Code:

;;; Use straight.el and use-package for package management
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(setq use-package-always-defer t)

;;; Separate custom variables out into their own file.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(setq byte-compile-warnings '(cl-functions))

(set-fontset-font t nil (font-spec :size 20 :name "WenQuanYi Micro Hei Mono:antialias=true"))


(global-set-key (kbd "C-S-<iso-lefttab>") 'indent-relative)


(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "nyxt")

(menu-bar-mode -1)
(delete-selection-mode 1)

;;; Custom functions ----------------------------------------------------

(defun which-active-modes ()
  "Give a message of which minor modes are enabled in the current buffer."
  (interactive)
  (let ((active-modes))
    (mapc (lambda (mode) (condition-case nil
                        (if (and (symbolp mode) (symbol-value mode))
                            (add-to-list 'active-modes mode))
                      (error nil)))
          minor-mode-list)
    (message "Active modes are %s" active-modes)))

(global-so-long-mode 1)

(defun my-multi-add-hook (function hooks)
  (mapc (lambda (hook)
          (add-hook hook function))
        hooks))

(defun tf-toggle-show-trailing-whitespace ()
  "Toggle show-trailing-whitespace between t and nil"
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace)))

(setq-default show-trailing-whitespace (not show-trailing-whitespace))

(add-hook 'Info-mode-hook
          (lambda ()
            (tf-toggle-show-trailing-whitespace)))

;;; Package Configuration  ------------------------------------------------
;; Cheap hack for, admittedly odd looking, but nice shorthand
(defmacro %use-package (&rest args)
  (declare (indent 1))
  (eval `(backquote (use-package ,@args))))

(defun gen-hooks (modes hook)
  (mapcar (lambda (x) (cons x hook)) modes))

;;; Core Emacs Improvements
(use-package linum-relative
  :hook ((prog-mode org-mode) . linum-relative-mode ))

(use-package ace-window
  :bind ("M-p" . ace-window))

(use-package popwin
  :defer nil
  :config
  (popwin-mode 1))

(use-package undo-tree
  :defer nil
  :config
  (global-undo-tree-mode 1))

(use-package nyan-mode
  :defer nil
  :config
  (nyan-mode 1))

(use-package ivy)

(use-package transient)

(use-package edit-indirect)

;; Useful for refactoring code do
;; ag-project-regexp
(use-package ag
  :defer nil)

;; then run wgrep-change-to-wrgrep-cmode C-cp to make the buffer editable
(use-package wgrep
  :after ag)

(use-package hydra
  :defer nil
  :config
  (defhydra hydra-zoom (global-map "<f2>")
    "zoom"
    ("g" text-scale-increase "in")
    ("l" text-scale-decrease "out")))

(global-unset-key (kbd "C-x c"))

(use-package helm
    :custom
  (setq helm-boring-file-regexp-list
        (append '("\\.checked$"
                  "\\.hints$")
                helm-boring-file-regexp-list))
  :config
  (setq helm-ff-skip-boring-files t)
  (setq helm-locate-fuzzy-match nil)

  (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
        helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
        helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
        helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
        Helm-ff-file-name-history-use-recentf t)
  (setq helm-autoresize-max-height 30)
  (helm-mode 1)
  ;; if you don't defer this isn't valid?!
  (helm-autoresize-mode 1)
  :bind (("C-c h" . helm-command-prefix)
         ("M-x" . helm-M-x)
         ("C-x b" . helm-buffers-list)
         ("C-x C-f" . helm-find-files)
         ("C-c h" . helm-command-prefix)
         ;; For locate and find. Find is for current directory while locate is for everything and
         ;; I've added fuzzy... to get unfuzzy results add ""
         ("C-z" . helm-find)
         ("C-x w" . helm-do-grep-ag)
         :map helm-map
         ("<tab>" . helm-execute-persistent-action) ; rebind tab to run persistent action
         ("C-i" . helm-execute-persistent-action)   ; make TAB works in terminal
         ("C-z" . helm-select-action)))             ; list actions with C-z

;; (use-package helm-autoresize-mode
;;   :hook
;;   ((helm-mode . helm-autoresize-mode))
;;   :config
;;   (helm-autoresize-mode 1)
;;   :defer nil
;;   :straight nil
;;   :after helm)

(use-package helm-company
    :after helm)

(use-package woman
  :bind
  (:map help-map
   ("W" . woman)))

;;; Text Processing
(use-package flyspell
  :config
  (setq flyspell-default-dictionary "en_US")
  :hook ((prog-mode . flyspell-prog-mode)
         (org-mode . flyspell-mode)))

(use-package bidi
  :config
  (setq-default bidi-paragraph-direction 'left-to-right)
  (setq bidi-inhibit-bpa t))

(use-package poporg)
(use-package org
  :hook ((org-mode . auto-fill-mode))
  :mode ("\\.\\(org\\|txt\\)$" . org-mode)
  :config

  (defun my-org-retrieve-url-from-point ()
    "Copies the URL from an org link at the point"
    (interactive)
    (let ((plain-url (url-get-url-at-point)))
      (if plain-url
          (progn
            (kill-new plain-url)
            (message (concat "Copied: " plain-url)))
        (let* ((link-info (assoc :link (org-context)))
               (text (when link-info
                       (buffer-substring-no-properties
                        (or (cadr link-info) (point-min))
                        (or (caddr link-info) (point-max))))))
          (if (not text)
              (error "Oops! Point isn't in an org link")
            (string-match org-link-bracket-re text)
            (let ((url (substring text (match-beginning 1) (match-end 1))))
              (kill-new url)
              (message (concat "Copied: " url))))))))

  (add-hook 'org-mode-hook              ;improve this
            (lambda ()
              (turn-on-auto-fill)
              (visual-line-mode 1)
              (define-key org-mode-map (kbd "C-c e") 'my-org-retrieve-url-from-point)
              (require 'ox-latex)
              (unless (boundp 'org-latex-classes)
                (setq org-latex-classes nil))
              (setf org-export-headline-levels 4)
              (add-to-list 'org-latex-classes
                           '("article"
                             "\\documentclass{article}"
                             ("\\section{%s}" . "\\section*{%s}")
                             ("\\subsection{%s}" . "\\subsection*{%s}")
                             ("\\paragraph{%s}" . "\\paragraph*{%s}")))
              (setq org-latex-inputenc-alist '(("utf8" . "utf8x")))
              (setq org-latex-default-packages-alist
                    (cons '("mathletters" "ucs" nil) org-latex-default-packages-alist))
              (setq org-latex-listings 'minted
                    org-latex-pdf-process
                    '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                      "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))))

  (setq org-src-fontify-natively t)

  (setq org-todo-keywords
        '((sequence "TODO" "|" "DONE" "CREDIBLE" "POOR" "MIXED")))

  :bind (("C-c f" . auto-fill-mode)
         ("C-c e" . my-org-retrieve-url-from-point)))

; Use spaces, not tabs.
(setq-default indent-tabs-mode nil)
(setq indent-tabs-mode nil)

;;; Tools and Utilities
(use-package magit
  :bind ("C-x g" . magit-status))

(use-package gemini-mode)

(use-package company-ledger)
(use-package ledger-mode)
(use-package hledger-mode)


(use-package dired
  :straight nil
  :config
  (setq dired-dwim-target t))
(use-package dired-ranger
  :after dired)

(use-package ranger)

(use-package vterm
  :hook
  (vterm-mode . (lambda ()
                  (setq show-trailing-whitespace nil))))

(use-package esup)

(use-package erc
  :straight nil
  :custom
  (erc-hide-list '("JOIN" "PART" "QUIT"))
  (erc-lurker-hide-list '("JOIN" "PART" "QUIT"))
  :config
  (defface erc-notice-face
    '((default :weight bold)
      (((class color) (min-colors 88)) :foreground "SlateBlue")
      (t :foreground "blue"))
    "ERC face for notices."
    :group 'erc-faces)

  (erc-spelling-mode 1)
  (erc-track-mode t)
  (erc-log-mode)
  (setq erc-track-showcount t)
  (setq erc-track-use-faces t)
  (setq erc-track-priority-faces-only 'all)

  ;; odd but this has to be, if the bottom bar is to be colored
  ;; properly
  (setq erc-modules '(autoaway autojoin button completion list
                               log replace ring spelling hl-nicks netsplit
                               fill button match track readonly networks
                               ring autojoin noncommands irccontrols
                               move-to-prompt stamp menu list))

  (setq erc-keywords '("\\berc[-a-z]*\\b"))
  (setq erc-current-nick-highlight-type 'all)
  (setq erc-prompt "λ>")
  (setq erc-track-mode t)
  (setq erc-track-showcount t)
  (setq
   erc-track-faces-priority-list
   (append '(erc-current-nick-face erc-keyword-face)
           erc-track-faces-priority-list))
  :hook
  (erc-mode . (lambda ()
                (nyan-mode -1)
                (setq show-trailing-whitespace nil))))
(use-package erc-hl-nicks
  :after erc)
(use-package erc-image
  :after erc)
(use-package erc-yt)
(use-package erc-colorize)

;;; Navigation
(use-package evil
    :defer nil
    :config
    (setq evil-disable-insert-state-bindings t)
    (setq evil-insert-state-map (make-sparse-keymap))
    (define-key evil-insert-state-map (kbd "<escape>") 'evil-normal-state) ; improve this?
    (evil-mode 1))

;; (defun my/fix-evil-hiding-minor-mode-map (&rest _args)
;;   "See `https://github.com/syl20bnr/spacemacs/issues/9391'"
;;   (let ((mjm-keymap (intern-soft (format "%s-map" major-mode))))
;;     (when mjm-keymap
;;       (setq evil-mode-map-alist
;; 	    (cl-loop for (c . k) in evil-mode-map-alist
;; 		     unless (and (eq c t) (eq k (symbol-value mjm-keymap)))
;; 		     collect (cons c k))))))
;; (advice-add 'evil-normalize-keymaps :after #'my/fix-evil-hiding-minor-mode-map)

(use-package iy-go-to-char
  :bind (("C-c f" . iy-go-up-to-char)
         ("C-q" . iy-go-up-to-char)
         ("C-S-q" . iy-go-up-to-char-backward)
         ("C-c F" . iy-go-up-to-char-backward)
         ("C-c ;" . iy-go-to-or-up-to-continue)
         ("C-c ," . iy-go-to-or-up-to-continue-backward)))

;;; Programming -----------------------------------------------------------
;; General
(use-package lsp-mode
    :config
  (setq lsp-ui-doc-enable nil)
  (setq lsp-keymap-prefix "C-c s")
  (define-key lsp-mode-map (kbd "C-c s") lsp-command-map)
  :hook (lsp-mode . (lambda ()
                      (let ((lsp-keymap-prefix "C-c s"))
                        (lsp-enable-which-key-integration))))
  :hook (c-mode-common . lsp))

(use-package yasnippet
  :hook (lsp-mode . yas-minor-mode))

(use-package lsp-ui
    :after lsp-mode
    :bind (:map lsp-mode-map
                ("C-c C-c" . lsp-ui-doc-glance))  )

(use-package eldoc)

(use-package flycheck)

(use-package company
  :defer nil
  :config
  (global-company-mode 1)
  :bind (("C-<tab>" . company-manual-begin)
         :map company-active-map
         ("<tab>" . company-manual-begin)))

(use-package rainbow-delimiters)

(use-package rainbow-mode)

(use-package highlight-parentheses
  :defer nil
  :config
  (global-highlight-parentheses-mode 1))

(use-package flash-paren
  :defer nil
  :config
  (flash-paren-mode 1))

(use-package smartparens
  :defer nil
  :config
  (smartparens-global-mode 1))

(use-package xah-math-input
  :straight (:host github :repo "emacsmirror/xah-math-input")
  :defer nil
  :config
  (global-xah-math-input-mode 1)
  (xah-math-input--add-cycle ["::" "∷"])
  (xah-math-input--add-cycle ["-o" "⊸"]))

;;; Languages (In alphabetical order)
;; BNF
(use-package bnf-mode)

;; C/C++
(setq-default c-basic-offset 4)

(use-package gdb-mi
  :config
  (setq gdb-many-windows t
        gdb-show-main t)
  (defadvice gdb-display-buffer (after undedicate-gdb-display-buffer)
    (set-window-dedicated-p ad-return-value nil))
  (ad-activate 'gdb-display-buffer)

  (defadvice gdb-set-window-buffer (after undedicate-gdb-set-window-buffer (name &optional ignore-dedi window))
    (set-window-dedicated-p window nil))
  (ad-activate 'gdb-set-window-buffer))


(use-package irony
  :hook ((c++-mode c-mode objc-mode)
         (irony-mode . irony-cdb-autosetup-compile-options)))

(use-package company-irony
  :config
  (defun c++-mode-config ()
    "Configuration for c++-mode-hook."
    (local-set-key (kbd "<M-tab>") 'company-irony))
  :hook (c++-mode . c++-mode-config))

(use-package ggtags
  :hook c++-mode)
(use-package helm-gtags
  :hook c++-mode)

;; Coq
(use-package company-coq
  :hook coq-mode)

;; Elixir
(use-package alchemist)
(use-package elixir-mode
  :hook alchemist-mode)

;; F*
(use-package fstar-mode
  :config
  (setq fstar-subp-prover-args
        '("--cache_checked_modules" "--record_hints" "--use_hints")))

;; Factor
(use-package fuel
  :config
  ;; on arch linux!
  (setq fuel-listener-factor-binary "/bin/factor-vm")
  (setq fuel-listener-factor-image "~/.factor/factor/factor.image"))

;; Lisp
(setq *lisp-modes*
      '(common-lisp-mode
        emacs-lisp-mode
        lisp-interaction-mode
        emacs-lisp-mode
        lisp-mode
        scheme-mode
        racket-mode
        lfe-mode
        hy-mode
        clojure-mode
        cider-repl-mode
        racket-repl-mode
        slime-repl-mode
        inferior-lisp-mode
        mrepl-mode
        sly-mrepl-mode
        comint-mode))

(%use-package lispy
  :hook ,(gen-hooks *lisp-modes* 'lispy-mode)
  :config
  ;; temporary removal
  (setq lispy-use-sly t)
  )

(use-package geiser
  :config
  (setq geiser-active-implementations '(racket chez mit))
  (setq geiser-chez-binary "scheme")
  (setq geiser-debug-jump-to-debug-p nil)
  (setq geiser-debug-show-debug-p nil))

(use-package macrostep-geiser)
(use-package ac-geiser)
(use-package geiser-mit)

(use-package geiser-chez)
(use-package geiser-racket)

(use-package racket-mode
  :config
  (defmacro racket-delete (function-name)
    "a flawed way to remove the buffer creation the racket repl does"
    `(progn
       (advice-add ',function-name :after
                   (lambda ()
                     (when (condition-case nil
                               (select-window (get-buffer-window racket-repl-buffer-name t))
                             (error nil))
                       (delete-windows-on "*Racket REPL*" t))))
       (lambda () (interactive)
         (,function-name)
         (when (condition-case nil
                   (select-window (get-buffer-window racket-repl-buffer-name t))
                 (error nil))
           (delete-window)))))

  (define-key racket-mode-map (kbd "C-c C-k")
    (racket-delete racket-run-and-switch-to-repl))
  (define-key racket-mode-map (kbd "C-M-x")
    (racket-delete racket-send-definition))
  (define-key racket-mode-map (kbd "C-c C-r")
    (racket-delete racket-send-region))
  (define-key racket-mode-map (kbd "C-c C-c")
    (racket-delete racket-send-definition))
  (define-key racket-mode-map (kbd "C-c C-m")
    (racket-delete racket-run-module-at-point)))

  ;; (define-key racket-mode-map (kbd "C-x C-e")
  ;;   (lambda () (interactive) (racket-send-last-sexp) (delete-windows-on "*Racket REPL*" t)))

(use-package scheme-complete)



;; (straight-use-package '(sly :source el-get))

(use-package sly
    :config
  (setq inferior-lisp-program "ros run  -l ~/.sbclrc")
  ;; (setq inferior-lisp-program "clasp")
  )

;; (use-package slime
;;     :config
;;   (slime-setup '(slime-asdf
;;                     slime-autodoc
;;                     slime-editing-commands
;;                     slime-fancy
;;                     slime-fontifying-fu
;;                     slime-fuzzy
;;                     slime-indentation
;;                     slime-mdot-fu
;;                     slime-package-fu
;;                     slime-references
;;                     slime-repl
;;                     slime-sbcl-exts
;;                     slime-scratch
;;                  slime-xref-browser))
;;   (setq inferior-lisp-program "ros run  -l ~/.sbclrc"))

;; (use-package sly-asdf :straight nil :ensure t)
;; (use-package sly-macrostep :straight nil :ensure t)

; CL indentation and font things

;; (setq lisp-indent-function 'common-lisp-indent-function)

;; (put 'defmodule-named 'common-lisp-indent-function
;;      (list 4 '&lambda '&lambda '&body))

;; (put 'defmodule 'common-lisp-indent-function
;;      (list 4 '&lambda 2 '&body))

;; (font-lock-add-keywords
;;  'lisp-mode
;;  '(("defmodule"
;;     2 font-lock-function-name-face font-lock-preprocessor-face t)))


;; Haskell
(use-package haskell-mode
  :bind (:map haskell-mode-map
              ("C-c C-r" . haskell-process-reload)
              ("C-c C-z" . haskell-interactive-switch-back)
              ("C-c C-t" . haskell-session-change-target)
              ("C-c C-l" . haskell-process-load-file))
  :hook (haskell-mode . lsp))
(use-package lsp-haskell
  :after lsp)

;; Ocaml
(use-package merlin-eldoc)
(use-package merlin
  :hook
  (((tuareg-mode caml-mode tuareg-interactive-mode) . merlin-mode)
   ((tuareg-mode tuareg-interactive-mode) . merlin-eldoc-setup)))

(use-package opam)
(use-package tuareg)
(use-package caml-mode
  :config

  (defun opam-env ()
    (interactive nil)
    (dolist (var (car (read-from-string (shell-command-to-string "opam config env --sexp"))))
      (setenv (car var) (cadr var))))

  (add-hook 'caml-mode-hook
            (lambda ()
              (require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
              (require 'ocamlformat)
              (define-key tuareg-mode-map (kbd "C-M-<tab>") #'ocamlformat)
              (add-hook 'before-save-hook #'ocamlformat-before-save)
              (setq tuareg-indent-align-with-first-arg t)
              (setq merlin-ac-setup 'easy)
              (opam-env)
              (autoload 'merlin-mode "merlin" "Merlin mode" t)
              (with-eval-after-load 'company
                (add-to-list 'company-backends #'utop-company-backend)))))

(use-package ocamlformat)

(use-package ocp-indent
  :config
  (setf ocp-indent-path "ocp-indent"))

;; Themes
(use-package zenburn-theme)

;;; Pretty Lambda ---------------------------------------------------------

(defun my-pretty-lambda ()
  "Make some word or string show as pretty Unicode symbols."
  (setq prettify-symbols-alist
        '(("lambda"     . 955)        ; λ
          ("/="         . 8800)       ; ≠
          ("forall"     . 8704)       ; ∀
          ;; ("member"     . 8712)     ; ∈
          ("*"          . ?×)
          ;; (">="           . ?≥)
          ;; ("<="           . ?≤)
          ("<<"           . ?⊣)         ; shen lisp
          (">>"           . ?⊢)         ; shen lisp
          ("union"        . ?∪)
          ("intersection" . ?∩)
          ("or"           . ?∨)
          ("and"          . ?∧)
          ("&"            . ?∧)         ; shen lisp
          ("not"          . ?¬)
          ("sqrt"         . ?√)
          ("compose"      . ?∘)
          ("comp"         . ?∘)
          ("floor"        . ?⌊)
          ("ceiling"      . ?⌈)
          ;; ("when"         . ?⟺)
          ;; ("null"         . ?∅)
          ("reduce #'+"   . ?∑)         ; Common Lisp
          ("apply +"      . ?∑)         ; racket and scheme
          ("apply *"      . ?∏)         ; racket and scheme
          ("reduce #'*"   . ?∏)         ; Common Lisp
          ("Integer"      . ?ℤ)
          ("integer"      . ?ℤ)
          ("Boolean"      . ?𝔹)
          ("Flonum"       . ?𝔻)
          ("True"         . ?𝑇)
          ("False"        . ?𝐹)
          ("Natural"      . ?ℕ))))

(defun my-pretty-clojure ()
  "Make some word or string show as pretty Unicode symbols."
  (setq prettify-symbols-alist
        '(("fn"         . 955)          ; λ
          ("not="       . 8800)         ; ≠
          ("forall"     . 8704)         ; ∀
          ;; (">="           . ?≥)
          ;; ("<="           . ?≤)
          ("*"          . ?×)
          ("union"        . ?∪)
          ("intersection" . ?∩)
          ("or"           . ?∨)
          ("and"          . ?∧)
          ("not"          . ?¬)
          ("sqrt"         . ?√)
          ("compose"      . ?∘)
          ("comp"         . ?∘)
          ("floor"        . ?⌊)
          ("ceiling"      . ?⌈)
          ;; ("when"         . ?⟺)
          ;; ("null"         . ?∅)
          ("apply +"      . ?∑)         ; racket and scheme
          ("apply *"      . ?∏)         ; racket and scheme
          ("Integer"      . ?ℤ)
          ("integer"      . ?ℤ)
          ("Boolean"      . ?𝔹)
          ("Flonum"       . ?𝔻)
          ("True"         . ?𝑇)
          ("False"        . ?𝐹)
          ("Natural"      . ?ℕ))))

(defun my-pretty-lambda-haskell ()
  "Make some word or string show as pretty Unicode symbols."
  (setq prettify-symbols-alist
        '(("lambda"    . 955)           ; λ
          ("\\"        . 955)           ; λ
          ("forall"    . 8704)          ; ∀
          ("/="        . 8800)          ; ≠
          ("`notElem`" . 8713)          ; ∉
          ("`elem`"    . 8712)          ; ∈
          ("*"          . ?×)
          ;; (">="          . ?≥)
          ;; ("<="          . ?≤)
          ("`union`"     . ?∪)
          ("`intersect`" . ?∩)
          ("not"         . ?¬)
          ("sqrt"        . ?√)
          ("=="          . ?≡)
          ;; (" . "         . (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Bc . Bc) ?\s (Bc . Bc) ?∘))
          ;; ("."           . ?∘)
          ("||"          . ?∨)
          ("&&"          . ?∧)
          ("floor"       . ?⌊)
          ("ceiling"     . ?⌈)
          ("sum"         . ?∑)
          ("product"     . ?∏)
          ("empty"       . ?∅)
          ("undefined"   . ?⊥)
          ("Integer"     . ?ℤ)
          ("Bool"        . ?𝔹)
          ("Rational"    . ?ℚ)
          ("Double"      . ?𝔻)
          ;; ("True"        . ?𝑇)
          ;; ("False"       . ?𝐹)
          )))

(defun my-pretty-lambda-ocaml ()
  "Make some word or string show as pretty Unicode symbols."
  (setq prettify-symbols-alist
        '(("lambda"    . 955)           ; λ
          ("forall"    . 8704)          ; ∀
          ("`notElem`" . 8713)          ; ∉
          ("`elem`"    . 8712)          ; ∈
          ("*"          . ?×)
          ;; ("then"         . ?⟾)
          ;; ("else"         . ?⟾)
          ;; (">="          . ?≥)
          ;; ("<="          . ?≤)
          ("`union`"     . ?∪)
          ("`intersect`" . ?∩)
          ("not"         . ?¬)
          ("sqrt"        . ?√)
          ;; (" . "         . (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Bc . Bc) ?\s (Bc . Bc) ?∘))
          ;; ("."           . ?∘)
          ("||"          . ?∨)
          ("&&"          . ?∧)
          ("floor"       . ?⌊)
          ("ceiling"     . ?⌈)
          ("sum"         . ?∑)
          ("product"     . ?∏)
          ("empty"       . ?∅)
          ("undefined"   . ?⊥)
          ("Integer"     . ?ℤ)
          ("bool"        . ?𝔹)
          ("rational"    . ?ℚ)
          ("double"      . ?𝔻)
          ;; ("true"        . ?𝑇)
          ;; ("False"       . ?𝐹)
          )))

(defun my-pretty-elixir ()
  (setq prettify-symbols-alist
        '(("lambda"    . 955)           ; λ
          ("fn"        . 955)           ; λ
          ;; ("!="        . 8800)       ; ≠
          ("in"        . 8712)          ; ∈
          ("*"          . ?×)
          ("when"        . ?⟺)
          ;; (">="          . ?≥)
          ;; ("<="          . ?≤)
          ("union"       . ?∪)
          ("intersect"   . ?∩)
          ("not"         . ?¬)
          ("sqrt"        . ?√)
          ("=="          . ?≡)
          ("||"          . ?∨)
          ("&&"          . ?∧)
          ("or"          . ?∨)
          ("and"         . ?∧)
          ("floor"       . ?⌊)
          ("ceiling"     . ?⌈)
          ("sum"         . ?∑)
          ("product"     . ?∏)
          ("empty?"      . ?∅)
          ("undefined"   . ?⊥)
          ("Integer"     . ?ℤ)
          ("Float"       . ?𝔻)
          ("true"        . ?𝑇)
          ("false"       . ?𝐹)
          ("do"          . ?{)
          ("end"         . ?}))))
;;  ℱ ℬ ℤ* ℤ+ ℤ+
(defun my-pretty-algol ()
  "Make some word or string show as pretty Unicode symbols."
  (setq prettify-symbols-alist
        '(("=="          . ?≡)
          ("in"        . 8712)
          ("*"          . ?×)
          ;; ("!="        . 8800) ; ≠
          ("union"       . ?∪)
          ;; (">="          . ?≥)
          ("compose"     . ?∘)
          ;; ("<="          . ?≤)
          ("!"           . ?¬)
          ("not"           . ?¬)
          ("||"          . ?∨)
          ("or"          . ?∨)
          ("&&"          . ?∧)
          ("and"          . ?∧)
          ;; ("|"           . ?⋎)
          ("floor"        . ?⌊)
          ("ceiling"      . ?⌈)
          ("ceil"         . ?⌈)
          ;; ("&"           . ?⋏)
          ("sqrt"        . ?√)
          ("'\\0'"       . ?∅)
          ("NULL"        . ?∅)
          ("int"         . ?ℤ)
          ("Integer"     . (?ℤ (Br . Bl) ?+))
          ("Boolean"     . ?𝔹)
          ("double"      . ?𝔻))))


;; Enable ligatures without prettify-symbols
;; For Emacs 25

(provide 'add-pragmatapro-symbol-keywords)
(defconst pragmatapro-fontlock-keywords-alist
  (mapcar (lambda (regex-char-pair)
            `(,(car regex-char-pair)
              (0 (prog1 ()
                   (compose-region (match-beginning 1)
                                   (match-end 1)
                                   ,(concat (list ?\C-i)
                                            (list (decode-char 'ucs (cadr regex-char-pair)))))))))

          '(("\\(!!\\)"               #XE720)
            ("[^=:<]\\(!=\\)"         #XE721)
            ("[^=:<]\\(==\\)"               #XE801)
            ("\\(!==\\)"              #XE722)

            ;; custom
            ("\\(--\\)"               #XE7A0)
            ("\\(==>\\)"               #XE803)
            ("[^|]\\(->\\)"           #XE7A4)
            ("\\(<->\\)"              #XE129)
            ;; ("\\(-->\\)"              #XE113)
            ("[^|]\\(->>\\)"          #XE7A5)
            ("\\(>>\\)"              #XE822)
            ;; ("\\(<\\?>\\)"            #XE7E3)
            ("[^=:<]\\(=>\\)"         #XE804)
            ("\\(=>>\\)"               #XE806)
            ("\\(>-\\)"              #XE820)
            ;; ("\\(s!!!\\)"              #XE723)
            ;; ("\\(!≡\\)"               #XE724)
            ;; ("\\(!≡≡\\)"              #XE725)
            ;; ("[^<]\\(!>\\)"           #XE726)
            ("\\(#(\\)"               #XE740)
            ("\\(#_\\)"               #XE741)
            ("\\(#{\\)"               #XE742)
            ;; ("\\(#\\?\\)"             #XE743)
            ;; ("[^<]\\(#>\\)"           #XE744)
            ;; ("\\(##\\)"               #XE745)
            ("\\(%=\\)"               #XE750)
            ;; ("[^<]\\(%>\\)"           #XE751)
            ("\\(&%\\)"               #XE760)
            ("\\(&&\\)"               #XE761)
            ("\\(&\\*\\)"             #XE762)
            ("\\(&\\+\\)"             #XE763)
            ("\\(&-\\)"               #XE764)
            ("\\(&/\\)"               #XE765)
            ("\\(&=\\)"               #XE766)
            ("\\(&&&\\)"              #XE767)
            ;; ("[^<]\\(&>\\)"           #XE768)
            ("\\(\\*\\*\\*\\)"        #XE780)
            ("\\(\\*=\\)"             #XE781)
            ("\\(\\*/\\)"             #XE782)
            ("[^<]\\(\\*>\\)"         #XE783)
            ("\\(\\+\\+\\)"           #XE790)
            ("\\(\\+\\+\\+\\)"        #XE791)
            ("[^\\+]\\(\\+=\\)"       #XE792)
            ("[^<]\\(\\+>\\)"         #XE793)
            ("\\(\\+\\+=\\)"          #XE794)
            ("[^-]\\(-<\\)"           #XE7A1)
            ("\\(-<<\\)"              #XE7A2)
            ("\\(-=\\)"               #XE7A3)
            ;; ("\\(---\\)"              #XE7A6)
            ;; ("\\(-\\+-\\)"            #XE7A8)
            ("\\(-\\\\/\\)"           #XE7A9)
            ("[^\\^]\\(\\.\\.\\)"     #XE7B0)
            ("\\(\\.\\.\\.\\)"        #XE7B1)
            ("\\(\\.\\.<\\)"          #XE7B2)
            ;; ("\\(\\.>\\)"             #XE7B3)
            ;; ("\\(\\.~\\)"             #XE7B4)
            ("\\(\\.=\\)"             #XE881)
            ("\\(/\\*\\)"             #XE7C0)
            ("\\(//\\)"               #XE7C1)
            ("[^<]\\(/>\\)"           #XE7C2)
            ("[^=]\\(/=\\)"           #XE7C3)
            ("\\(/==\\)"              #XE7C4)
            ("\\(///\\)"              #XE7C5)
            ;; ("\\(/\\*\\*\\)"          #XE7C6)
            ("\\(::\\)"               #XE7D0)
            ("\\(:=\\)"               #XE7D1)
            ;; ("[^≡]\\(:≡\\)"           #XE7D2)
            ;; ("\\(:>\\)"               #XE7D3)
            ;; ("\\(:=>\\)"              #XE7D4)
            ("\\(<\\*\\)"             #XE7E1)
            ("\\(<\\*>\\)"            #XE7E2)
            ("[^<]\\(<-\\)"           #XE7E4)
            ("[^-]\\(<<\\)"           #XE7E5)
            ("\\(<<<\\)"              #XE7E6)
            ("\\(<<=\\)"              #XE7E7)
            ("[^<]\\(<=\\)"           #XE7E8)
            ("[^<]\\(>=\\)"           #XE821)
            ("\\(<=>\\)"              #XE7E9)
            ("\\(<>\\)"               #XE7EA)
            ("\\(<<-\\)"              #XE7EC)
            ("\\(<|\\)"               #XE7ED)
            ("\\(<|>\\)"              #XE7EB)
            ("\\(<=<\\)"              #XE7EE)
            ("[^<]\\(<~\\)"           #XE75F)
            ("\\(<~~\\)"              #XE7F0)
            ("\\(<<~\\)"              #XE7F1)
            ("\\(<\\$\\)"             #XE7F2)
            ("\\(<\\$>\\)"            #XE7E0)
            ("\\(<\\+\\)"             #XE7F3)
            ("\\(<\\+>\\)"            #XE7E3)
            ;; ("\\(<~>\\)"              #XE800)
            ;; ("\\(<\\*\\*>\\)"         #XE801);; <**>
            ("\\(<<\\^\\)"            #XE802)
            ;; ("\\(<!\\)"               #XE803)
            ;; ("\\(<!>\\)"              #XE7F4)
            ;; ("\\(<@\\)"               #XE804)
            ("\\(=~\\)"               #XE805)
            ;; ("\\(<#\\)"               #XE805)
            ;; ("\\(<#>\\)"              #XE7F6)
            ("\\(<%\\)"               #XE806)
            ;; ("\\(<%>\\)"              #XE7F7)
            ;; ("[^<]\\(<\\^\\)"         #XE807)
            ;; ("\\(<&\\)"               #XE808)
            ;; ("\\(<&>\\)"              #XE7F9)
            ;; ("\\(<\\?\\)"             #XE809)
            ;; ("\\(<\\.\\)"             #XE80A)
            ;; ("\\(<\\.>\\)"            #XE7FB)
            ;; ("\\(</\\)"               #XE80B)
            ;; ("\\(</>\\)"              #XE7FC)
            ;; ("\\(<\\\\\\)"            #XE80C)
            ;; ("\\(<\"\\)"              #XE80D)
            ;; ("\\(<\">\\)"             #XE7FE)
            ;; ("\\(<:\\)"               #XE80E)
            ;; ("\\(<:>\\)"              #XE7FF)
            ;; ("\\(<->\\)"              #XE80F)
            ;; ("\\(<!--\\)"             #XE810)
            ;; ("\\(<--\\)"              #XE811)
            ;; ("\\(<~<\\)"              #XE812)
            ;; ("\\(<==>\\)"             #XE813)
            ;; ("\\(==<\\)"              #XE820)
            ;; ("[^/!<=>]\\(==\\)[^><=]" #XE821)
            ("[^<]\\(>>-\\)"          #XE823)
            ("\\(>>>\\)"               #XE825)
            ("\\(>=>\\)"              #XE826)
            ;; ("\\(=/=\\)"              #XE827)
            ;; ("[^!]\\(≡≡\\)"           #XE830)
            ;; ("\\(≡≡≡\\)"              #XE831)
            ;; ("\\(≡:≡\\)"              #XE832)
            ;; ("[^>]\\(>-\\)"           #XE840)
            ;; ("\\(>=\\)"               #XE841)
            ;; ("[^=-]\\(>>\\)"          #XE842)
            ;; ("\\(>>-\\)"              #XE843)
            ;; ("\\(>==\\)"              #XE844)
            ;; ("\\(>>>\\)"              #XE845)
            ;; ("\\(>=>\\)"              #XE846)
            ("\\(>>\\^\\)"            #XE847)
            ("\\(\\?\\?\\)"           #XE840)
            ("\\(\\?~\\)"             #XE841)
            ("\\(\\?=\\)"             #XE842)
            ;; ("\\(\\?>\\)"            #XE883)
            ;; ("\\(\\?\\?\\?\\)"        #XE864)
            ("\\(\\^=\\)"             #XE848)
            ("\\(\\^\\.\\)"           #XE849)
            ("\\(\\^\\?\\)"           #XE84A)
            ("\\(\\^\\.\\.\\)"        #XE84B)
            ;; ("\\(\\^<<\\)"            #XE86C)
            ;; ("\\(\\^>\\)"             #XE86E)
            ;; ("\\(\\^>>\\)"            #XE86D)
            ;; ("\\(<\\^>\\)"            #XE7F8)
            ("[^\\\\]\\(\\\\\\\\\\)"  #XE850)
            ("[^<]\\( ~>\\)"        #XE77F)
            ("\\(<\\\\>\\)"           #XE7FD)
            ("\\(\\\\/-\\)"           #XE872)
            ;; ("\\(@>\\)"               #XE877)
            ;; ("\\(<@>\\)"              #XE7F5)
            ("\\(|=\\)"               #XE860)
            ("\\(\\.~\\)"               #XE880)
            ("\\(||\\)"               #XE861)
            ("[^<]\\(|>\\)"            #XE862)
            ("\\(|||\\)"              #XE863)
            ("\\(|\\+|\\)"            #XE884)
            ;; ("\\(|->\\)"              #XE885)
            ;; ("\\(|-->\\)"             #XE886)
            ;; ("\\(|=>\\)"              #XE887)
            ;; ("\\(|==>\\)"             #XE888)
            ;; ("\\(~=\\)"               #XE890)
            ;; ("[^~<]\\(~>\\)"          #XE891)
            ;; ("\\(~~>\\)"              #XE892)
            ;; ("\\(~>>\\)"              #XE893)
            ;; ("[^<]\\(\">\\)"          #XE8B0)
            ("\\(>>=\\)"                   #XE824)
            ("\\(=<<\\)"                   #XE800))))

(defun add-pragmatapro-symbol-keywords ()
  (font-lock-add-keywords nil pragmatapro-fontlock-keywords-alist))

(add-hook 'prog-mode-hook
          #'add-pragmatapro-symbol-keywords)

(add-hook 'haskell-interactive-mode-hook
          #'add-pragmatapro-symbol-keywords)

(add-hook 'comint-mode-hook
          #'add-pragmatapro-symbol-keywords)

;; breaks lispy in the sly repl?
;; (my-multi-add-hook 'my-pretty-lambda
;;                    '(scheme-mode-hook
;;                      org-mode-hook
;;                      lisp-mode-hook
;;                      comint-mode-hook
;;                      racket-mode-hook
;;                      hy-mode-hook
;;                      sly-mrepl-mode-hook
;;                      shen-mode-hook
;;                      inferior-shen-mode-hook
;;                      ;; slime-repl-mode-hook
;;                      ))

(my-multi-add-hook 'my-pretty-clojure '(clojure-mode-hook cider-repl-mode-hook))

(my-multi-add-hook 'my-pretty-lambda-haskell '(haskell-mode-hook haskell-interactive-mode-hook))
(my-multi-add-hook 'my-pretty-lambda-ocaml '(ocaml-mode-hook tuareg-mode-hook))

(my-multi-add-hook 'my-pretty-algol '(c-mode-hook java-mode-hook c++-mode-hook py-mode-hook python-mode-hook))
(my-multi-add-hook 'my-pretty-elixir '(elixir-mode-hook alchemist-iex-mode-hook))

(global-prettify-symbols-mode 1)
(put 'dired-find-alternate-file 'disabled nil)
