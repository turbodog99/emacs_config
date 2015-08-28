(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  )

;; This gives you a tab of 2 spaces
(custom-set-variables '(coffee-tab-width 2))

(setq auto-save-file-name-transforms
          `((".*" ,temporary-file-directory t)))

(defvar macosx-p (string-match "darwin" (symbol-name system-type)))
(unless (eq window-system nil)
  (cond (macosx-p (setq mac-allow-anti-aliasing nil))))

(set-face-attribute 'default nil :height 160)

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(setq my-packages '(clojure-mode cider rainbow-delimiters paredit color-theme
			  exec-path-from-shell company coffee-mode
			  magit slim-mode))

(dolist (package my-packages)
  (unless (package-installed-p package)
    (package-install package)))

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))

(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)

(defun turn-on-paredit () (paredit-mode 1))
(add-hook 'clojure-mode-hook 'turn-on-paredit)
(add-hook 'cider-repl-mode-hook #'paredit-mode)

(show-paren-mode 1)

(require 'color-theme)
(color-theme-initialize)
(color-theme-tty-dark)

(tool-bar-mode 0)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
    (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
    (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
    (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
    (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
    (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
    (add-hook 'scheme-mode-hook           #'enable-paredit-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("3164a65923ef23e0f3dff9f9607b4da1e07ef1c3888d0f6878feef6c28357732" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(defun cygwin-shell ()
  "Run cygwin bash in shell mode."
  (interactive)
  (let ((explicit-shell-file-name "D:/cygwin64/bin/bash"))
    (call-interactively 'shell)))
