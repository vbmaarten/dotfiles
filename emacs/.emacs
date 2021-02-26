(setq inhibit-startup-message t)
(defalias 'yes-or-no-p 'y-or-n-p)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 0)

(menu-bar-mode -1)

(setq visible-bell t)
(setq ring-bell-function 'asc:flash-background)
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
	                 ("org" . "https://orgmode.org/elpa")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
(use-package doom-themes)
(load-theme 'doom-palenight t)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(use-package magit)

(use-package org)

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bind-key-column-widths '(22 . 40))
 '(custom-safe-themes
   '("3e27c4a8de1ea4e0e7195815ef9ddba53a7dd5cdd1279b0309e9f8e9553be3b7" default))
 '(package-selected-packages
   '(vscode-dark-plus-theme treemacs-icons-dired treemacs-projectile treemacs package-safe-delete all-the-icons wn-mode wm-mode powerline no-littering typescript-mode calc-at-point windresize counsel doom-themes doom-palenight which-key ivy projectile org-bullets magit gruvbox-theme use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(column-number-mode)
(global-display-line-numbers-mode t)

(dolist (mode '(org-mode-hook term-mode-hook eshell-mode-hook))
  (add-hook mode(lambda() (display-line-numbers-mode 0))))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init)

(setq projectile-project-search-path '("~/Projects/"))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;; NOTE: If you want to move everything out of the ~/.emacs.d folder
;; reliably, set `user-emacs-directory` before loading no-littering!
;(setq user-emacs-directory "~/.cache/emacs")

(use-package no-littering)

;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(use-package powerline
  :init
  (powerline-default-theme))

(use-package wn-mode
  :init
  (wn-mode))

(use-package all-the-icons
  :init
    (unless (member "all-the-icons" (font-family-list))
    (all-the-icons-install-fonts t)))

(use-package treemacs
  :demand t
  :init
  (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action)
  :config
  (treemacs-git-mode 'simple)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  
  )

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-icons-dired
  :after (treemacs dired)
  :config (treemacs-icons-dired-mode))


(add-hook 'projectile-after-switch-project-hook 'treemacs-display-current-project-exclusively)
