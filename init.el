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

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(add-to-list 'load-path "~/.emacs.d/lisp/")

;; Hide tool and menu bars at the top of graphical emacs
(tool-bar-mode 0)

;; This should hide menu on server started in TTY
(unless (display-graphic-p)
  (menu-bar-mode -1))

;; Disable menu bar only on TTY frames. This should handle
;; when a server creates new frames on TTY or GUI.
(defun contextual-menubar (&optional frame)
  "Display the menubar in FRAME (default: selected frame) if on a
   graphical display, but hide it if in terminal."
  (interactive)
  (set-frame-parameter frame 'menu-bar-lines
                             (if (display-graphic-p frame)
                                  1 0)))
(add-hook 'after-make-frame-functions 'contextual-menubar)

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

;; If package has never been run, refresh package directory
;; Running this every time as I'd like slows it down too much.
(when (not package-archive-contents)
  (package-refresh-contents))

(setq my-packages '(clojure-mode cider rainbow-delimiters paredit color-theme
			  exec-path-from-shell company coffee-mode
			  magit slim-mode web-mode sass-mode haml-mode nyan-mode
			  flycheck js2-mode json-mode exec-path-from-shell
                          fill-column-indicator web-mode slim-mode sass-mode
                          rainbow-delimiters paredit nyan-mode magit json-mode
                          js2-mode ggtags flycheck exec-path-from-shell company
                          color-theme coffee-mode cider elm-mode haskell-mode
                          ggtags))

(dolist (package my-packages)
  (unless (package-installed-p package)
    (package-install package)))

;; Sets font size in graphical Emacs
(set-face-attribute 'default nil :height 145)

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

(require 'color-theme)
(color-theme-initialize)

;; (color-theme-deep-blue)

;; Railscast theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/site-lisp/themes/")
(load-file "~/.emacs.d/site-lisp/themes/color-theme-railscasts.el")

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

;;; Text-based settings and helpers

(xterm-mouse-mode)
(define-key global-map [select] 'end-of-line)

;;; Term settings

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
	   (getenv "PATH"))))

;;; Web Mode settings

(require 'web-mode)

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
(add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . web-mode))

;;; Ruby Mode settings

(setq ruby-indent-level 2)

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

(defun cygwin-shell ()
  "Run cygwin bash in shell mode."
  (interactive)
  (let ((explicit-shell-file-name "D:/cygwin64/bin/bash"))
    (call-interactively 'shell)))

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
