;;; General setup and defaults

;; Keep Emacs backup files out of source tree
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  )

(setq auto-save-file-name-transforms
          `((".*" ,temporary-file-directory t)))

(add-to-list 'load-path "~/.emacs.d/lisp/")

;; Enable clipboard to kill ring
(setq x-select-enable-clipboard t)
(setq x-select-enable-primary t)

;; Hide tool and menu bars at the top of graphical emacs
(tool-bar-mode 0)

;; Disable anti-aliasing on Mac
;(setq mac-allow-anti-aliasing nil)

;; Hide scroll bars
(toggle-scroll-bar -1)

(defun my/disable-scroll-bars (frame)
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil))))
(add-hook 'after-make-frame-functions 'my/disable-scroll-bars)

(defun my/set-font (&optional frame)
  (with-selected-frame (or frame (selected-frame))
    (pcase window-system
      ('ns (set-frame-font "Monaco-17")))))

(my/set-font)
(add-hook 'after-make-frame-functions 'my/set-font)

;; This should hide menu on server started in TTY
;; (unless (display-graphic-p)
;;   (menu-bar-mode -1))

(menu-bar-mode -1)
;; Disable menu bar only on TTY frames. This should handle
;; when a server creates new frames on TTY or GUI.
;; (defun contextual-menubar (&optional frame)
;;   "Display the menubar in FRAME (default: selected frame) if on a
;;    graphical display, but hide it if in terminal."
;;   (interactive)
;;   (set-frame-parameter frame 'menu-bar-lines
;;                              (if (display-graphic-p frame)
;;                                   1 0)))
;; (add-hook 'after-make-frame-functions 'contextual-menubar)

(require 'package)

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))

(package-initialize)

;; If package has never been run, refresh package directory
;; Running this every time as I'd like slows it down too much.
(when (not package-archive-contents)
  (package-refresh-contents))

(setq my-packages '(clojure-mode cargo cider coffee-mode color-theme-modern company dockerfile-mode
                                 elm-mode exec-path-from-shell fill-column-indicator flycheck
                                 flycheck-clj-kondo flycheck-rust ggtags go-mode groovy-mode haml-mode
                                 haskell-mode json-mode js2-mode magit
                                 markdown-mode nyan-mode ob-restclient
                                 paredit rainbow-delimiters racer restclient rust-mode
                                 sass-mode simpleclip slim-mode terraform-mode tide web-mode yaml-mode))

(dolist (package my-packages)
  (unless (package-installed-p package)
    (package-install package)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((restclient . t)))

(require 'flycheck-clj-kondo)

(when (not (eq system-type 'windows-nt))
  (exec-path-from-shell-initialize))

(require 'simpleclip)
(simpleclip-mode 1)

(global-set-key (kbd "<ESC> C-v") 'simpleclip-paste)
(global-set-key (kbd "<ESC> C-c") 'simpleclip-copy)
(global-set-key (kbd "<ESC> C-x") 'simpleclip-cut)

;; Don't use tabs for indent by default
(setq-default indent-tabs-mode nil)

;; Nyan cat scroll indicator on graphical emacs
(nyan-mode)

;; Suppress beeping on right and left touchpad scroll
(global-set-key [wheel-right] 'ignore)
(global-set-key [wheel-left] 'ignore)
(global-set-key [double-wheel-right] 'ignore)
(global-set-key [double-wheel-left] 'ignore)
(global-set-key [triple-wheel-right] 'ignore)
(global-set-key [triple-wheel-left] 'ignore)

;; Company mode on everything: http://company-mode.github.io/
(add-hook 'after-init-hook 'global-company-mode)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Displays a rule across the right edge to indicate a certain
;; column number.
(require 'fill-column-indicator)

;; TODO: set appropriate columns per mode type
(setq fci-rule-column 81)

;; This seemed to cause some problems. Not using it by default.
;; (add-hook 'after-change-major-mode-hook 'fci-mode)

;; Highlights parens and other enclosing characters
(show-paren-mode 1)

(add-hook 'before-save-hook 'my-prog-nuke-trailing-whitespace)
(defun my-prog-nuke-trailing-whitespace ()
  (when (derived-mode-p 'prog-mode)
    (delete-trailing-whitespace)))

;;; Linting

;; http://www.flycheck.org/manual/latest/index.html
(require 'flycheck)

(setq flycheck-eslintrc "~/.eslintrc")

;; turn on flychecking globally
(add-hook 'after-init-hook #'global-flycheck-mode)

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(json-jsonlist)))

;; don't complain about elisp documentation
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;;; Text-based settings and helpers

(xterm-mouse-mode)
(define-key global-map [select] 'end-of-line)

;;; Org mode
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;;; Term settings

(eval-after-load "term"
  '(define-key term-raw-map (kbd "C-c C-y") 'term-paste))

;; TODO: Not sure if this is working
(setq explicit-bash-args '("--noediting" "--login" "-i"))

;;; X Settings

;; Cut and paste with clipboard in X-based terminals
(load-library "xclip")
(require 'xclip)

;;; Mac settings
(when (string= system-type "darwin")
  ;; Macs are now GNU based and ls doesn't have --dired option
  ;; Suppresses --dired on ls only on Mac
  (setq dired-use-ls-dired nil)
  (setq mac-option-key-is-meta nil)
  (setq mac-command-key-is-meta t)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier nil)

  ;; https://github.com/purcell/exec-path-from-shell
  ;; only need exec-path-from-shell on OSX
  ;; this hopefully sets up path and other vars better
  (exec-path-from-shell-initialize))

;; ;; I kind of prefer this, but nobody seems to be able to read it on
;; ;; shared screen sessions.
;; (defvar macosx-p (string-match "darwin" (symbol-name system-type)))
;; (unless (eq window-system nil)
;;   (cond (macosx-p (setq mac-allow-anti-aliasing nil))))

;;; Windows settings

(when (eq system-type 'windows-nt)
  ;; Fix Windows grep path to use MSYS
  (setenv "PATH"
	  (concat
           ;; TODO: see if there's an MSYS_HOME or similar variable
	   ;; Change this with your path to MSYS bin directory
	   "D:\\MinGW\\bin;"
           "C:\\gnu_tools\\bin;"
	   (getenv "PATH"))))

;;; Web Mode settings

(require 'web-mode)

(setq web-mode-enable-auto-quoting nil)

(setq web-mode-content-types-alist
  '(("jsx" . "\\.js[x]?\\'")))

;; adjust indents for web-mode to 2 spaces
(defun my-web-mode-hook ()
  "My customizations for Web mode"
  ;;; http://web-mode.org/

  ;;; Set indents
  (setq js-indent-level 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-attr-indent-offset 2))
(add-hook 'web-mode-hook  'my-web-mode-hook)

;; ;; TODO: see what this does and see if you like it.
;; (defadvice web-mode-highlight-part (around tweak-jsx activate)
;;   (if (equal web-mode-content-type "jsx")
;;       (let ((web-mode-enable-part-face nil))
;;         ad-do-it)
;;     ad-do-it))

;; File type hooks
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss$" . sass-mode))
(add-to-list 'auto-mode-alist '("\\.js[x]?$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.hbs$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.env$" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.avsc$" . json-mode))

;;; TypeScript
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (setq typescript-indent-level
        (or (plist-get (tide-tsfmt-options) ':indentSize) 2))
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
;; enable typescript-tslint checker
(flycheck-add-mode 'typescript-tslint 'web-mode)

;;; Ruby Mode settings

(setq ruby-indent-level 2)

;;; JSON Mode settings
(add-hook 'json-mode-hook
          (lambda ()
            ; Make variable local so it doesn't conflict with JS mode
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2)))

;;; Coffee Mode settings

(setq coffee-tab-width 2)

;;; Ido settings
;; Ido is what enables selecting buffers and files with parts of the names
;; https://www.emacswiki.org/emacs/InteractivelyDoThings
(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;;; LISP Mode settings

(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
(add-hook 'clojure-mode-hook          #'enable-paredit-mode)
(add-hook 'cider-repl-mode-hook       #'enable-paredit-mode)

(add-to-list 'auto-mode-alist '("\\.el$" . lisp-mode))

(defun cygwin-shell ()
  "Run cygwin bash in shell mode."
  (interactive)
  (let ((explicit-shell-file-name "D:/cygwin64/bin/bash"))
    (call-interactively 'shell)))

;;; Cider settings
(defun my-cider-repl-mode-hook ()
  "My customizations for Cider mode"
  (define-key cider-repl-mode-map (kbd "C-S-<down>") 'cider-repl-next-input)
  (define-key cider-repl-mode-map (kbd "C-S-<up>") 'cider-repl-previous-input))
(add-hook 'cider-repl-mode-hook 'my-cider-repl-mode-hook)

(cider-auto-test-mode 0)

;; Rust Programming
;; Much of this comes from http://julienblanchard.com/2016/fancy-rust-development-with-emacs/

;;; Add shortcuts to run Cargo commands
(add-hook 'rust-mode-hook 'cargo-minor-mode)

;;; Add formatting shortcut keys
(add-hook 'rust-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c <tab>") #'rust-format-buffer)))

;;; Racer autocomplete setup
(setq racer-cmd "~/.cargo/bin/racer") ;; Rustup binaries PATH
(setq racer-rust-src-path "~/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src") ;; Rust source code PATH

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)

; C Programming
; Mostly from http://tuhdo.github.io/c-ide.html

(require 'ggtags)
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
              (ggtags-mode 1))))

(define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
(define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
(define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
(define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
(define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
(define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)

(define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)

; The following is from http://sachachua.com/blog/2016/04/keep-emacs-alive-x-crashes-running-background-daemon/
(defun my/ssh-refresh ()
  "Reset the environment variable SSH_AUTH_SOCK"
  (interactive)
  (let (ssh-auth-sock-old (getenv "SSH_AUTH_SOCK"))
    (setenv "SSH_AUTH_SOCK"
            (car (split-string
                  (shell-command-to-string
                   "ls -t $(find /tmp/ssh-* -user $USER -name 'agent.*' 2> /dev/null)"))))
    (message
     (format "SSH_AUTH_SOCK %s --> %s"
             ssh-auth-sock-old (getenv "SSH_AUTH_SOCK")))))
(my/ssh-refresh)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (cobalt)))
 '(custom-safe-themes
   (quote
    ("780c67d3b58b524aa485a146ad9e837051918b722fd32fd1b7e50ec36d413e70" "b4fd44f653c69fb95d3f34f071b223ae705bb691fb9abaf2ffca3351e92aa374" "f831c1716ebc909abe3c851569a402782b01074e665a4c140e3e52214f7504a0" "9a3c51c59edfefd53e5de64c9da248c24b628d4e78cc808611abd15b3e58858f" "11e5e95bd3964c7eda94d141e85ad08776fbdac15c99094f14a0531f31a156da" "05d009b7979e3887c917ef6796978d1c3bbe617e6aa791db38f05be713da0ba0" "b6f06081b007b57be61b82fb53f27315e2cf38fa690be50d6d63d2b62a408636" "80a23d559a5c5343a0882664733fd2c9e039b4dbf398c70c424c8d6858b39fc5" "2d5c40e709543f156d3dee750cd9ac580a20a371f1b1e1e3ecbef2b895cf0cd2" "7bd626fcc9fbfb44186cf3f08b8055d5a15e748d5338e47f9391d459586e20db" "a455366c5cdacebd8adaa99d50e37430b0170326e7640a688e9d9ad406e2edfd" "d9e811d5a12dec79289c5bacaecd8ae393d168e9a92a659542c2a9bab6102041" "595099e6f4a036d71de7e1512656e9375dd72cf60ff69a5f6d14f0171f1de9c1" "1a094b79734450a146b0c43afb6c669045d7a8a5c28bc0210aba28d36f85d86f" "6e03b7f86fcca5ce4e63cda5cd0da592973e30b5c5edf198eddf51db7a12b832" "fe349b21bb978bb1f1f2db05bc87b2c6d02f1a7fe3f27584cd7b6fbf8e53391a" "da8e6e5b286cbcec4a1a99f273a466de34763eefd0e84a41c71543b16cd2efac" "e26e879d250140e0d4c4d5ab457c32bcb29742599bd28c1ce31301344c6f2a11" "1342a81078bdac27f80b86807b19cb27addc1f9e4c6a637a505ae3ba4699f777" "72c530c9c8f3561b5ab3bf5cda948cd917de23f48d9825b7a781fe1c0d737f2f" "b71da830ae97a9b70d14348781494b6c1099dbbb9b1f51494c3dfa5097729736" "6a674ffa24341f2f129793923d0b5f26d59a8891edd7d9330a258b58e767778a" "8530b2f7b281ea6f263be265dd8c75b502ecd7a30b9a0f28fa9398739e833a35" "cdc2a7ba4ecf0910f13ba207cce7080b58d9ed2234032113b8846a4e44597e41" "2047464bf6781156ebdac9e38a17b97bd2594b39cfeaab561afffcbbe19314e2" "fb09acc5f09e521581487697c75b71414830b1b0a2405c16a9ece41b2ae64222" "67b11ee5d10f1b5f7638035d1a38f77bca5797b5f5b21d16a20b5f0452cbeb46" "0f302165235625ca5a827ac2f963c102a635f27879637d9021c04d845a32c568" "45482e7ddf47ab1f30fe05f75e5f2d2118635f5797687e88571842ff6f18b4d5" "4c8372c68b3eab14516b6ab8233de2f9e0ecac01aaa859e547f902d27310c0c3" "5eb4b22e97ddb2db9ecce7d983fa45eb8367447f151c7e1b033af27820f43760" "db510eb70cf96e3dbd48f5d24de12b03db30674ea0853f06074d4ccf7403d7d3" default)))
 '(package-selected-packages
   (quote
    (helm ob-restclient flycheck-clj-kondo racer cargo flymake-rust rust-mode markdown-mode dockerfile-mode terraform-mode restclient yaml-mode web-mode slim-mode simpleclip sass-mode rainbow-delimiters paredit nyan-mode magit json-mode js2-mode haskell-mode go-mode ggtags flycheck fill-column-indicator exec-path-from-shell elm-mode company color-theme-modern color-theme coffee-mode cider)))
 '(safe-local-variable-values
   (quote
    ((magit-todos-exclude-globs "*.html" "*.org" "*.md" "*.map")
     (eval progn
           (put
            (quote s/defn)
            (quote clojure-doc-string-elt)
            2)
           (define-clojure-indent
             (puppetlabs\.trapperkeeper\.core/defservice
              (quote
               (:defn
                (:defn))))
             (trapperkeeper/defservice
              (quote
               (:defn
                (:defn))))
             (tk/defservice
              (quote
               (:defn
                (:defn))))
             (defservice
               (quote
                (:defn
                 (:defn))))
             (context 2)
             (DELETE 2)
             (GET 2)
             (PATCH 2)
             (POST 2)
             (PUT 2)))
     (cljr-favor-prefix-notation . t)
     (eval progn
           (put
            (quote s/defn)
            (quote clojure-doc-string-elt)
            2)
           (define-clojure-indent
             (puppetlabs\.trapperkeeper\.core/defservice
              (quote
               (:defn
                (:defn))))
             (trapperkeeper/defservice
              (quote
               (:defn
                (:defn))))
             (tk/defservice
              (quote
               (:defn
                (:defn))))
             (defservice
               (quote
                (:defn
                 (:defn))))))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
