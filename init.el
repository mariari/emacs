;;; Commentary:
;;; Code:

(require 'package)
(package-initialize)

(setq byte-compile-warnings '(cl-functions))

(setq package-archives
      '(("melpa" . "http://melpa.org/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")))

(setq lisp-indent-function 'common-lisp-indent-function)

(put 'defmodule-named 'common-lisp-indent-function
     (list 4 '&lambda '&lambda '&body))
(put 'defmodule 'common-lisp-indent-function
     (list 4 '&lambda 2 '&body))

(setq use-package-always-defer t)

;; Profiling
;; (setq esup-depth 0)

;; Custom functions ----------------------------------------------------

(defun which-active-modes ()
  "Give a message of which minor modes are enabled in the current buffer."
  (interactive)
  (let ((active-modes))
    (mapc (lambda (mode) (condition-case nil
                             (if (and (symbolp mode) (symbol-value mode))
                                 (add-to-list 'active-modes mode))
                           (error nil) ))
          minor-mode-list)
    (message "Active modes are %s" active-modes)))

(global-so-long-mode 1)

;; Bidi customization ---------------------------------------------------

(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; Flash paren----------------------------------------------------------
(load-library "~/.emacs.d/flash-paren.el")
(flash-paren-mode 1)

;; ERC-----------------------------------------------------------------
;; (defadvice erc-track-find-face (around erc-track-find-face-promote-query activate)
;;   (if (erc-query-buffer-p)
;;       (setq ad-return-value (intern "erc-current-nick-face"))
;;     ad-do-it))


;; (setq erc-format-query-as-channel-p t
;;       erc-track-priority-faces-only 'nil
;;       erc-track-faces-priority-list '(erc-error-face
;;                                       erc-current-nick-face
;;                                       erc-keyword-face
;;                                       erc-nick-msg-face
;;                                       erc-direct-msg-face
;;                                       erc-dangerous-host-face
;;                                       erc-notice-face
;;                                       erc-prompt-face))
(defface erc-notice-face
  '((default :weight bold)
    (((class color) (min-colors 88)) :foreground "SlateBlue")
    (t :foreground "blue"))
  "ERC face for notices."
  :group 'erc-faces)

(use-package erc
    :custom
  (erc-hide-list '("JOIN" "PART" "QUIT"))
  (erc-lurker-hide-list '("JOIN" "PART" "QUIT"))
  :config

  (erc-spelling-mode 1)
  (erc-track-mode t)
  (erc-log-mode)
  (setq erc-track-showcount t)
  (setq erc-track-use-faces t)
  (setq erc-track-priority-faces-only 'all)
  ;; odd but this has to be, if the bottom bar is to be colored
  ;; properly
  (setq erc-modules
        '(autoaway autojoin button completion list log replace ring spelling hl-nicks netsplit
          fill button match track readonly networks ring autojoin noncommands irccontrols move-to-prompt stamp menu list))
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


;; Magit---------------------------------------------------------------
(global-set-key (kbd "C-x g") 'magit-status)

;; memory fix for broken system(?) ------------------------------------
(setq gc-cons-threshold (* 100 1024))
;; --------------------------------------------------------------------
(font-lock-add-keywords
 'lisp-mode
 '(("defmodule"
    2 font-lock-function-name-face font-lock-preprocessor-face t)))

;; enable global modes ------------------------------------------------
(helm-mode 1)
(nyan-mode 1)
(global-company-mode 1)
(smartparens-global-mode 1)
;; (show-smartparens-global-mode t)
(global-undo-tree-mode)
(delete-selection-mode 1)
(evil-mode 1)
(global-highlight-parentheses-mode t)
;;--------------------------------------------------------------------

;;custom functions ---------------------------------------------------
(defun my-multi-add-hook (function hooks)
  (mapc (lambda (hook)
          (add-hook hook function))
        hooks))
;; Company------------------------------------------------------------
(with-eval-after-load 'company
  (define-key company-active-map (kbd "<tab>") 'company-manual-begin))
(global-set-key (kbd "C-<tab>") 'company-manual-begin)

(yas-minor-mode)
;;White-Space---------------------------------------------------------
(defun tf-toggle-show-trailing-whitespace ()
  "Toggle show-trailing-whitespace between t and nil"
  (interactive)
  (setq show-trailing-whitespace (not show-trailing-whitespace)))

(setq-default show-trailing-whitespace (not show-trailing-whitespace))
;---------------------------------------------------------------------
;;Ansi-tem-------------------------------------------------------------
;; (global-set-key (kbd "C-x t") 'ansi-term)
;;---------------------------------------------------------------------

;;Evil-----------------------------------------------------------------
(setq evil-insert-state-map (make-sparse-keymap))
(define-key evil-insert-state-map (kbd "<escape>") 'evil-normal-state)
;;---------------------------------------------------------------------

;;Menu-bar-------------------------------------------------------------
(menu-bar-mode -1)
;;Ace------------------------------------------------------------------
(global-set-key (kbd "M-p") 'ace-window)
;;Default browser------------------------------------------------------
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "nyxt")
;;Hydra----------------------------------------------------------------
(require 'hydra)
(defhydra hydra-zoom (global-map "<f2>")
  "zoom"
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out"))
;; Auto Complete ------------------------------------------------------
;; (require 'auto-complete-config)
;; (global-set-key (kbd "M-S-<iso-lefttab>") 'auto-complete)
;; (setq ac-auto-start nil)
;; (ac-quick-help 0)
;; (setq ac-show-menu-immediately-on-auto-complete t)
;;--------------------------------------------------------------------

;;Tabs-AreEvil--------------------------------------------------------
(setq-default indent-tabs-mode nil)
(setq indent-tabs-mode nil)
;;--------------------------------------------------------------------

;;pretty lambda-------------------------------------------------------
(defun my-pretty-lambda ()
  "Make some word or string show as pretty Unicode symbols."
  (setq prettify-symbols-alist
        '(("lambda"     . 955)          ; λ
          ("/="         . 8800)          ; ≠
          ("forall"     . 8704)         ; ∀
          ;; ("member"     . 8712)     ; ∈
          ("*"          . ?×)
          ;; (">="           . ?≥)
          ;; ("<="           . ?≤)
          ("<<"           . ?⊣)          ; shen lisp
          (">>"           . ?⊢)          ; shen lisp
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

(defun my-prett-eylixir ()
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

(my-multi-add-hook 'my-pretty-lambda
                   '(scheme-mode-hook
                     org-mode-hook
                     lisp-mode-hook
                     common-mode-hook
                     inferior-lisp-mode-hook
                     comint-mode-hook
                     racket-mode-hook
                     hy-mode-hook
                     shen-mode-hook
                     inferior-shen-mode-hook
                     ;; slime-repl-mode-hook
                     ))

(my-multi-add-hook 'my-pretty-clojure '(clojure-mode-hook cider-repl-mode-hook))

(my-multi-add-hook 'my-pretty-lambda-haskell '(haskell-mode-hook haskell-interactive-mode-hook))
(my-multi-add-hook 'my-pretty-lambda-ocaml '(ocaml-mode-hook tuareg-mode-hook))

(my-multi-add-hook 'my-pretty-algol '(c-mode-hook java-mode-hook c++-mode-hook py-mode-hook python-mode-hook))
(my-multi-add-hook 'my-pretty-elixir '(elixir-mode-hook alchemist-iex-mode-hook))

(global-prettify-symbols-mode 1)
;;--------------------------------------------------------------------
;; Dictionary--------------------------------------------------------
;; (global-set-key (kbd "C-c d") 'dictionary-lookup-definition)
;; Poetry mode-------------------------------------------------------
;; (load-library "~/.emacs.d/elpa/poetry-mode/poetry.elc")
;; (global-set-key (kbd "C-c r") 'poetry-rhyme-word)
;; (global-set-key (kbd "C-c w") 'poetry-find-rhyme)

;; tiny-expand keybinding---------------------------------------------
;; (global-set-key (kbd "C-S-l") 'tiny-expand)
;;--------------------------------------------------------------------
;;helm config----------------------------------------------------------
(require 'helm)

(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
;; For finding files M-x, and finding things in general
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(setq helm-ff-skip-boring-files t)
(customize-set-variable 'helm-boring-file-regexp-list
                        (append
                         (list
                          "\\.checked$"
                          "\\.hints$")
                         helm-boring-file-regexp-list))


;; For locate and find. Find is for current directory while locate is for everything and
;; I've added fuzzy... to get unfuzzy results add ""
(global-set-key (kbd "C-z") 'helm-find)

(global-set-key (kbd "C-x w") 'helm-locate)
(setq helm-locate-fuzzy-match nil)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      Helm-ff-file-name-history-use-recentf t)

(global-set-key (kbd "C-:") 'sr-speedbar-toggle)
(global-set-key (kbd "C-S-<iso-lefttab>") 'indent-relative)
;;---------------------------------------------------------------------

;; linenumber config---------------------------------------------------
(add-hook 'prog-mode-hook (lambda () (linum-relative-mode 1)))
(add-hook 'org-mode-hook (lambda () (linum-relative-mode 1)))
;;---------------------------------------------------------------------


;;Lispy setup----------------------------------------------------------
(add-hook 'common-lisp-mode-hook (lambda () (lispy-mode 1)))
(add-hook 'emacs-lisp-mode-hook 'lispy-mode)
(add-hook 'lisp-interaction-mode 'lispy-mode)
(add-hook 'emacs-lisp-mode-hook 'lispy-mode)
(add-hook 'lisp-mode-hook       'lispy-mode)
(add-hook 'scheme-mode-hook     'lispy-mode)
(add-hook 'racket-mode-hook     'lispy-mode)
(add-hook 'lfe-mode-hook        'lispy-mode)
(add-hook 'hy-mode-hook         'lispy-mode)
(add-hook 'clojure-mode-hook 'parinfer-mode)
(add-hook 'cider-repl-mode-hook 'lispy-mode)
;; (add-hook 'clojure-mode-hook 'lispy-mode)
(add-hook 'cider-repl-mode-hook 'lispy-mode)
(add-hook 'racket-repl-mode-hook 'lispy-mode)
;; (add-hook 'slime-repl-mode-hook 'lispy-mode)
(add-hook 'mrepl-mode-hook 'lispy-mode)
(add-hook 'inferior-lisp-mode-hook 'lispy-mode)
(add-hook 'mrepl-mode-hook 'lispy-mode)
(add-hook 'comint-mode-hook 'lispy-mode)

(setq lispy-use-sly t)
;; (require 'parinfer)
;; (setq parinfer-auto-switch-indent-mode-when-closing nil)
;; (setq parinfer-extensions '(defaults smart-yank lispy smart-tab evil))

;; (add-hook 'clojure-mode-hook (lambda () smartparens-mode))
;;---------------------------------------------------------------------

;; Factor -------------------------------------------------------------

;; on arch linux!
(setq fuel-listener-factor-binary "/bin/factor-vm")
(setq fuel-listener-factor-image "~/.factor/factor/factor.image")
;;---------------------------------------------------------------------
;; Clojure setup-------------------------------------------------------

;; Scheme Setup--------------------------------------------------------

(setq geiser-active-implementations '(racket chez mit))
(setq geiser-chez-binary "scheme")
(setq geiser-debug-jump-to-debug-p nil)

(setq geiser-debug-show-debug-p nil)

;; Racket Setup--------------------------------------------------------
;; (require 'racket-mode)
;; (with-eval-after-load)
;; (add-to-list 'auto-mode-alist '("\\.\\(rkt\\)$" . racket-mode))
;; (add-hook 'racket-mode-hook
;;           (lambda ()
;;             (require 'racket-mode)
;;             (define-key racket-mode-map (kbd "C-c C-k")
;;               (lambda () (interactive) (racket-run-and-switch-to-repl) (delete-window)))

;;             (define-key racket-mode-map (kbd "C-M-x")
;;               (lambda () (interactive) (racket-send-definition) (delete-windows-on "*Racket REPL*" t)))

;;             (define-key racket-mode-map (kbd "C-x C-e")
;;               (lambda () (interactive) (racket-send-last-sexp) (delete-windows-on "*Racket REPL*" t)))))

;;Slime Setup----------------------------------------------------------
(setq inferior-lisp-program "ros run  -l ~/.sbclrc")
;; (setq inferior-lisp-program "sbcl")

;; (add-hook 'lisp-mode-hook
;;           (lambda ()
;;             (slime-mode f)
;;             ;; (slime-setup '(slime-fancy slime-company))
;;             ))

;; (add-hook 'slime-mode-hook 'set-up-slime-ac)
;; (add-hook 'slime-mode-hook (lambda () (auto-complete-mode 1)))
;; (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)

;; (eval-after-load "auto-complete"
;;   '(add-to-list 'ac-modes 'slime-repl-mode))

;; (setq slime-contribs '(slime-fancy))
;;org mode config------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.\\(org\\|txt\\)$" . org-mode))

(global-set-key "\C-cf" 'auto-fill-mode)
(global-set-key "\C-c/" 'poporg-dwim)

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

(add-hook 'org-mode-hook
          (lambda ()
            (turn-on-auto-fill)
            (visual-line-mode 1)
            (define-key org-mode-map (kbd "C-c e") 'my-org-retrieve-url-from-point)
            (require 'ox-latex)
            (unless (boundp 'org-latex-classes)
              (setq org-latex-classes nil))
            (add-to-list 'org-latex-classes
                         '("article"
                           "\\documentclass{article}"
                           ("\\section{%s}" . "\\section*{%s}")
                           ("\\subsection{%s}" . "\\subsection*{%s}")
                           ("\\paragraph{%s}" . "\\paragraph*{%s}")))
            (setq org-latex-inputenc-alist '(("utf8" . "utf8x")))
            (setq org-latex-default-packages-alist
                  (cons '("mathletters" "ucs" nil) org-latex-default-packages-alist))

            (add-to-list 'org-export-latex-packages-alist '("" "listings"))
            (add-to-list 'org-export-latex-packages-alist '("" "color"))
            (add-to-list 'org-export-latex-packages-alist '("" "minted"))
            (setq org-latex-listings 'minted
                  org-latex-pdf-process
                  '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                    "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))))

(setq org-src-fontify-natively t)

;;---------------------------------------------------------------------

;;flyspell config------------------------------------------------------
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'org-mode-hook  (lambda () (flyspell-mode 1)))
(setq flyspell-default-dictionary "en_US")
;;---------------------------------------------------------------------
;;multiplecursors-----------------------------------------------------
;; (require 'multiple-cursors)
;; ;; iy-go to  chars
;; (add-to-list 'mc/cursor-specific-vars 'iy-go-to-char-start-pos)
;; (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-M->") 'mc/unmark-next-like-this)
;; ;; Going to next and previous key
;; (global-set-key (kbd "C-c C-p") 'mc/skip-to-previous-like-this)
;; (global-set-key (kbd "C-c C-n") 'mc/skip-to-next-like-this)

;; (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C-M-<") 'mc/unmark-previous-like-this)

;; (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
;; (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

;;--------------------------------------------------------------------
;; iy-go-to-char------------------------------------------------------
(require 'iy-go-to-char)
(global-set-key (kbd "C-c f") 'iy-go-up-to-char)
(global-set-key (kbd "C-q") 'iy-go-up-to-char)

(global-set-key (kbd "C-S-q") 'iy-go-up-to-char-backward)

(global-set-key (kbd "C-c F") 'iy-go-up-to-char-backward)

(global-set-key (kbd "C-c ;") 'iy-go-to-or-up-to-continue)
(global-set-key (kbd "C-c ,") 'iy-go-to-or-up-to-continue-backward)
;;--------------------------------------------------------------------
;;org-mode keywords---------------------------------------------------
(setq org-todo-keywords
      '((sequence "TODO" "|" "DONE" "CREDIBLE" "POOR" "MIXED")))
;;--------------------------------------------------------------------

;;Java----------------------------------------------------------------
;; (add-hook 'java-mode-hook 'meghanada-mode)
;; (setq c-default-style
;;       '((java-mode . "cc-mode")))

;;--------------------------------------------------------------------

;; Elixir-------------------------------------------------------------
;; (require 'elixir-mode)
(eval-after-load "elixir-mode"
  '(progn
    (add-hook 'elixir-mode-hook 'alchemist-mode)))
;; (add-hook 'alchemist-mode-hook 'evil-mode)
;; -------------------------------------------------------------------
;; Haskell------------------------------------------------------------
;; deprecated
;; (add-hook 'haskell-mode-hook 'intero-mode)

;; when this can handle stack projects
;; (setq haskell-process-type 'stack-ghci)

;; (add-hook 'haskell-mode-hook 'dante-mode)

;; (add-hook 'dante-mode-hook
;;    '(lambda () (flycheck-add-next-checker 'haskell-dante
;;                 '(warning . haskell-hlint))))

(setq lsp-keymap-prefix "C-c")

(eval-after-load "lsp-mode"
  '(progn
    ;; (require 'lsp-ui-mode)
    ;; (setq lsp-ui-sideline-enable nil)
    (define-key lsp-mode-map  (kbd "C-c C-c") #'lsp-ui-doc-glance)
    (setq lsp-ui-doc-enable nil)))


(eval-after-load "interactive-haskell-mode"
  '(progn
    (define-key interactive-haskell-mode-map (kbd "C-c C-r") 'haskell-process-reload)
    (define-key haskell-interactive-mode-map (kbd "C-c C-r") 'haskell-process-reload)))

(eval-after-load "haskell-mode"
  '(progn
    (require 'lsp-mode)
    (add-hook 'haskell-mode-hook 'lsp)
    ;; (define-key interactive-haskell-mode-map (kbd "C-c C-r") 'haskell-process-reload)
    ;; (define-key haskell-interactive-mode-map (kbd "C-c C-r") 'haskell-process-reload)
    (define-key haskell-mode-map (kbd "C-c C-r") 'haskell-process-reload)
    (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch-back)
    (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-session-change-target)
    (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)))

;; (haskell-process-args-stack-ghci
;;  '("--ghci-options=-ferror-spans" "--no-build" "--no-load" ""))

;;--------------------------------------------------------------------
;; FStar--------------------------------------------------------------

(setq fstar-subp-prover-args '("--cache_checked_modules" "--record_hints" "--use_hints"))

;; Ocaml--------------------------------------------------------------
(defun opam-env ()
  (interactive nil)
  (dolist (var (car (read-from-string (shell-command-to-string "opam config env --sexp"))))
    (setenv (car var) (cadr var))))

(add-hook 'tuareg-mode-hook 'merlin-mode)
(add-hook 'caml-mode-hook 'merlin-mode)
(add-hook 'tuareg-mode-hook 'merlin-eldoc-setup)
(add-hook 'tuareg-interactive-mode-hook 'merlin-eldoc-setup)
(add-hook 'tuareg-interactive-mode-hook 'merlin-mode)

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
              (add-to-list 'company-backends #'utop-company-backend))))

(setf ocp-indent-path "ocp-indent")

(require 'popwin)
(popwin-mode 1)
;;--------------------------------------------------------------------
;; verilog------------------------------------------------------------
;; (setq verilog-tool 'verilog-linter)

;; (setq verilog-linter "iverilog")
;; (setq verilog-coverage "covered")
;;--------------------------------------------------------------------
;;C-------------------------------------------------------------------
(setq gdb-many-windows t gdb-show-main t)
;; Force gdb-mi to not dedicate any windows
(defadvice gdb-display-buffer (after undedicate-gdb-display-buffer)
  (set-window-dedicated-p ad-return-value nil))
(ad-activate 'gdb-display-buffer)

(defadvice gdb-set-window-buffer (after undedicate-gdb-set-window-buffer (name &optional ignore-dedi window))
  (set-window-dedicated-p window nil))
(ad-activate 'gdb-set-window-buffer)

(setq-default c-basic-offset 4)
;;--------------------------------------------------------------------
;;C++-----------------------------------------------------------------
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
(add-hook 'c++-mode-hook 'ggtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)

(defun c++-mode-config ()
  "Configuration for c++-mode-hook."
  (local-set-key (kbd "<M-tab>") 'company-irony))

(add-hook 'c++-mode-hook 'c++-mode-config)
;;Rainbow parenthesis-------------------------------------------------
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'comint-mode-hook 'rainbow-delimiters-mode)
(add-hook 'haskell-interactive-mode-hook 'rainbow-delimiters-mode)
;;--------------------------------------------------------------------

;;More Parens Options-------------------------------------------------

;; (require 'autopair)
;; (add-hook 'highlight-parentheses-mode-hook
;;           '(lambda ()
;;              (setq autopair-handle-action-fns
;;                    (append
;;                     (if autopair-handle-action-fns
;;                         autopair-handle-action-fns
;;                       '(autopair-default-handle-action))
;;                     '((lambda (action pair pos-before)
;;                         (hl-paren-color-update)))))))

;;----------------------------------------------------------------------
;; Proof General------------------------------------------------------
(add-hook 'coq-mode-hook #'company-coq-mode)


;; (load "~/.emacs.d/lisp/PG/generic/proof-site")

;; (setq isabelle-program-name-override "/bin/isabelle")
;;--------------------------------------------------------------------

;; MATH---------------------------------------------------------------
(global-xah-math-input-mode 1)
(xah-math-input--add-cycle ["::" "∷"])
(xah-math-input--add-cycle ["-o" "⊸"])

;;-------------------------------------------------------------------
;;---------------------------------------------------------------------
;; For emacs25

;; Enable ligatures without prettify-symbols

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

(set-fontset-font t nil (font-spec :size 20 :name "WenQuanYi Micro Hei Mono:antialias=true"))
;; (set-fontset-font t nil (font-spec :size 20 :name "Noto Sans Mono CJK TC"))
;; was commented out
;; (add-hook 'org-mode-hook
;;           #'add-pragmatapro-symbol-keywords)
;; ------------------------------------------------------------------------------------------------------------

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
 '(helm-autoresize-max-height 30)
 '(helm-autoresize-mode t)
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
   '((eval cl-flet
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

;; Removing current theme---------------------------------------------------
;; (add-hook 'window-setup-hook 'remove-current-theme)

;; '(default ((t (:family "PragmataPro" :foundry "fsdf" :slant normal :weight normal :height 113 :width normal))))
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
