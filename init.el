; Commentary:
;;; Code:

;;; Use straight.el and use-package for package management
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(setq use-package-always-defer t)

;;; Separate custom variables out into their own file.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(setq byte-compile-warnings '(cl-functions))

;; DFPYuanW7\-ZhuIn:antialias=true
;; (set-fontset-font t nil (font-spec :size 20 :name "WenQuanYi Micro Hei Mono:antialias=true"))
;; (set-fontset-font t nil  (font-spec :size 20 :name "HanWangKaiMediumChuIn:antialias=true"))
(set-fontset-font t nil  (font-spec :size 20 :name "DFPYuanW7-ZhuIn:antialias=true"))

(set-fontset-font "fontset-default" 'han                 ; works
                  "DFPYuanW7-ZhuIn:antialias=true" nil 'prepend)

(set-fontset-font "fontset-default" 'cjk-misc            ; works
                  "DFPYuanW7-ZhuIn:antialias=true" nil 'prepend)

;; (set-fontset-font t nil  (font-spec :size 20 :name "王漢宗中楷體注音:antialias=true"))
;;  我4
(global-set-key (kbd "C-S-<iso-lefttab>") 'indent-relative)

(setf use-default-font-for-symbols nil)
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "nyxt")

(menu-bar-mode -1)

(setq max-lisp-eval-depth 10000)
(setq max-specpdl-size 10000)

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
            (turn-off-evil-mode)
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

(use-package right-click-context
  :defer nil
  :config
  (right-click-context-mode 1))

;; Useful for refactoring code do
;; ag-project-regexp
(use-package ag
  :defer nil)

(use-package project
  :bind (("C-x C-d" . project-find-file)))

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
  ;; :straight (:host github :repo "emacs-straight/org-mode")
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
  :bind ("C-x g" . magit-status)
  ;; why did this suddenly act up!?
  :config
  (setq with-editor-emacsclient-executable nil))

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
  (setq lsp-keymap-prefix "C-c")
  (define-key lsp-mode-map (kbd "C-c") lsp-command-map)
  (add-to-list 'exec-path "~/.emacs.d/deps/elixir-ls")
  (add-to-list 'exec-path "~/.emacs.d/deps/erlang_ls")
  (define-key lsp-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (define-key lsp-mode-map (kbd "C-c u") #'lsp-ui-imenu)
  (define-key lsp-mode-map (kbd "C-c h t") #'lsp-treemacs-type-hierarchy)
  (define-key lsp-mode-map (kbd "C-c h c") #'lsp-treemacs-call-hierarchy)
  ;; (define-key lsp-mode-map (kbd "C-x h c") #'lsp-treemacs-hierarchy)
  :hook (lsp-mode . (lambda ()
                      (let ((lsp-keymap-prefix "C-c"))
                        (lsp-enable-which-key-integration))))
  :hook (c-mode-common . lsp)
  :hook (rustic-mode . lsp)
  :hook (rust-mode . lsp)
  :hook (elixir-mode . lsp)
  :hook (erlang-mode . lsp))

(use-package which-key
  :after lsp-mode)


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

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package rainbow-delimiters)

(use-package rainbow-mode)

(defun my-file-contents (filename)
  "Return the contents of FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))


(use-package gpt
  :defer nil
  :config
  (setq gpt-openai-engine "gpt-4o"))

(require 'gpt)
(setq gpt-openai-key (my-file-contents "~/.emacs.d/key"))
(global-set-key (kbd "C-c g") 'gpt-dwim-all-buffers)
(global-set-key (kbd "M-C-g") 'gpt-dwim)

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
  :straight (:host github :repo "emacsattic/xah-math-input")
  :defer nil
  :config
  (global-xah-math-input-mode 1)
  (xah-math-input--add-cycle ["::" "∷"])
  (xah-math-input--add-cycle ["-o" "⊸"]))

;;; Languages (In alphabetical order)

(use-package agda2-mode
  :config (setq agda2-version "2.6.2"))

;; Assembly

(use-package nasm-mode)


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

(use-package proof-general
  :hook coq-mode)

;; Elixir
;; Sadly alchemist is deprecated, so we have to deal with lsp ☹
;; (use-package alchemist)
(use-package dap-mode
  :config
  (add-hook 'dap-stopped-hook
            (lambda (arg) (call-interactively #'dap-hydra)))
  (dap-register-debug-template
   "Elixir debug all tests"
   ;; running all tests
   (list :type "Elixir"
         :cwd nil
         :request "launch"
         :startApps t
         :program nil
         :name "Elixir debug all tests"
         :dap-server-path '("~/.emacs.d/deps/elixir-ls/debugger.sh")))
  ;;  misc test
  (dap-register-debug-template
   "Elixir Misc Debug Single Test: block"
   (list :type "Elixir"
         :cwd nil
         :request "launch"
         :program nil
         :name "Elixir Debug Single Test: Ref Integrity Test"
         :startApps t
         :dap-server-path '("~/.emacs.d/deps/elixir-ls/debugger.sh")
         ;; tune this to your test
         :taskArgs '("test/narwhal_test.exs:45")
         :requireFiles '("test/**/test_helper.exs"
                         "test/narwhal_test.exs:45"))))

(use-package erlang)

(use-package elixir-ls
  :straight (:host github :repo "elixir-lsp/elixir-ls"))

(use-package elixir-mode
  ;; doesn't work for some odd reason
  ;; :hook lsp
  :hook inf-elixir
  ;; :hook dap-mode
  :config
  (require 'dap-elixir)
  ;; (add-hook 'elixir-mode-hook 'eglot-ensure)
  )

(use-package apprentice :straight (:host github
                                         :repo "Sasanidas/Apprentice"))


(use-package inf-elixir
  :bind (("C-c i i" . 'inf-elixir)
         ("C-c i p" . 'inf-elixir-project)
         ("C-c i l" . 'inf-elixir-send-line)
         ("C-c i r" . 'inf-elixir-send-region)
         ("C-c C-l" . 'inf-elixir-send-buffer)
         ("C-c C-r" . 'inf-elixir-reload-module)
         ("C-c i R" . 'inf-elixir-reload-module)
         ("C-c i b" . 'inf-elixir-send-buffer)))

;; (use-package eglot
;;   :config
;;   (add-to-list 'eglot-server-programs '(elixir-mode "elixir-ls")))

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
  (setq fuel-listener-factor-image "~/.factor/factor/factor.image")
  :defer nil)


(use-package forth-mode
  :config
  (add-hook 'forth-interaction-mode-hook
          (lambda ()
            (tf-toggle-show-trailing-whitespace))))


;; Hoon
(use-package hoon-mode
  :straight (:host github :repo "urbit/hoon-mode.el")
  :config
  ;; (setq eldoc-echo-area-prefer-doc-buffer t)
  )

;; Java
(use-package lsp-java
  :hook (java-mode . lsp))

;; Poplog
(use-package pop-mode
  :straight (:host github :repo "mariari/pop-mode"))
(use-package inferior-pop-mode
  :straight (:host github :repo "mariari/pop-mode"))
(use-package pop-help-mode
  :straight (:host github :repo "mariari/pop-mode"))

;; Lisp

;; ACL2
;; (push "~/Documents/Workspace/Lisp/acl2/books/emacs" load-path)
;; (load "~/Documents/Workspace/Lisp/acl2/books/emacs/emacs-acl2.el")
;; (setq inferior-acl2-program "~/Documents/Workspace/Lisp/acl2/saved_acl2")


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
        ;; comint-mode
        sly-mrepl-mode))

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

(use-package protobuf-mode)

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

(use-package rust-mode)

(use-package rustic
  :custom
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  :config
  (rustic-doc-mode t)
  (setf lsp-rust-server 'rust-analyzer))

;; (straight-use-package '(sly :source el-get))

;; (push "~/holder/lisp-system-browser/" load-path)

;; (use-package window-layout)

;; (use-package lisp-system-browser
;;   :straight (:host github :repo "enometh/lisp-system-browser" :branch :madhu)
;;   :config
;;   (sly-contrib)
;;   (push 'system-browser sly-contribs)
;;   (sly-enable-contrib 'system-browser))

(use-package eglot-argot
  :straight (:host gitlab :repo "GrammaTech/Mnemosyne/eglot-argot"))

(use-package sly
  :config
  (setq inferior-lisp-program "ros run  -l ~/.sbclrc")
  ;; (add-to-list 'sly-contribs 'sly-asdf 'append)
  (font-lock-add-keywords
   'lisp-mode
   '(("ctypecase-of" . font-lock-keyword-face)
     ("etypecase-of" . font-lock-keyword-face)
     ("typecase-of" . font-lock-keyword-face)
     ("match-of" . font-lock-keyword-face)
     ("match" . font-lock-keyword-face)))
  ;; ACL2
  (setf sly-lisp-implementations
        `((ros ("ros" "run" "-l"  "~/.sbclrc"))
          (acl2 ("~/.local/bin/acl2") :init sly-init-using-acl2)))

  (defun sly-init-using-acl2 (port-filename coding-system)
    "Return a string to initialize Lisp using ASDF.
     Fall back to `sly-init-using-slynk-loader' if ASDF fails."
    (format ":q\n\n%s\n\n(lp)\n\n"
            (sly-init-using-asdf port-filename coding-system)))
  (defun sly-init-using-acl2 (port-filename coding-system)
    "Return a string to initialize Lisp using ASDF.
Fall back to `sly-init-using-slynk-loader' if ASDF fails."
    (format "%S\n\n%S\n\n%S %S\n\n"
            '(set-raw-mode-on!)
            ;; sly-init-using-asdf
            `(cond ((ignore-errors
                      (funcall 'require "asdf")
                      (funcall (read-from-string "asdf:version-satisfies")
                               (funcall (read-from-string "asdf:asdf-version"))
                               "2.019"))
                    (push (pathname ,(sly-to-lisp-filename (sly-slynk-path)))
                          (symbol-value
                           (read-from-string "asdf:*central-registry*")))
                    (funcall
                     (read-from-string "asdf:load-system")
                     :slynk)
                    (funcall
                     (read-from-string "slynk:start-server")
                     ,(sly-to-lisp-filename port-filename)))
                   (t
                    ,(read (sly-init-using-slynk-loader port-filename
                                                        coding-system))))
            :set-raw-mode nil)))

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

(use-package sly-asdf)
(use-package sly-macrostep)
;; (use-package sly-stepper)

; CL indentation and font things

;; (setq lisp-indent-function 'common-lisp-indent-function)

;; (put 'defmodule-named 'common-lisp-indent-function
;;      (list 4 '&lambda '&lambda '&body))

;; (put 'defmodule 'common-lisp-indent-function
;;      (list 4 '&lambda 2 '&body))

;; 2 font-lock-function-name-face font-lock-preprocessor-face t

;; Idris
;; Idris 1.3.4
;; (use-package idris-mode)

;; Idris 2

(use-package idris2 :straight (:host github :repo "idris-community/idris2-mode"))


;; Haskell
;; (use-package haskell-mode
;;   :bind (:map haskell-mode-map
;;               ("C-c C-r" . haskell-process-reload)
;;               ("C-c C-z" . haskell-interactive-switch-back)
;;               ("C-c C-t" . haskell-session-change-target)
;;               ("C-c C-l" . haskell-process-load-file))
;;   :hook (haskell-mode . lsp))
;; (use-package lsp-haskell
;;   :after lsp)

;; Juvix
;; (use-package juvix :straight (:host github
;;                                     :repo "anoma/juvix"
;;                                     :files ("juvix-mode/*")))

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
          ;; ("when"        . ?⟺)
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


(setq warning-suppress-types '((comp)))
(setq warning-minimum-level :emergency)

;; Enable ligatures without prettify-symbols
;; For Emacs 25


(use-package emacs-pragmatapor-ligatures
  :straight (:host github :repo "lumiknit/emacs-pragmatapro-ligatures")
  :defer nil
  :config
  (require 'pragmatapro-lig)
  (add-hook 'prog-mode-hook 'pragmatapro-lig-mode)
  (add-hook 'text-mode-hook 'pragmatapro-lig-mode)
  (add-hook 'haskell-interactive-mode 'pragmatapro-lig-mode))

(use-package el-patch
  :defer nil
  :config
  ;; Respect the value of `lexical-binding'.
  (defun el-patch-eval-template (name type)
    (interactive (el-patch--select-template))
    (eval (el-patch--resolve-template name type)
          lexical-binding)))

(defun use-fancy-splash-screens-p () t)
(use-package fancy-splash-animation
  :straight nil :defer nil
  :config
  (setq fancy-splash-image "~/.emacs.d/alice-cookie.gif")
  (el-patch-define-and-eval-template
   (defun fancy-splash-head)
   (let* ...
     (el-patch-add
       (when (image-multi-frame-p img)
         (image--set-property img :height 300)
         (image--set-property img :mask '(heuristic t))
         (setq image-width (car (image-size img)))
         (image-animate img 0 t)))
     (when img
       ...))))



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
