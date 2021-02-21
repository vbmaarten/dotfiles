(setq inhibit-startup-message t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 0)

(menu-bar-mode -1)

(setq visible-bell t)
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
	                 ("org" . "https://orgmode.org/elpa")))

(package-initialize)
(unless package-archive-contents (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(unless (package-installed-p 'gruvbox-theme)
  (package-install 'gruvbox-theme))

(unless (package-installed-p 'magit)
  (package-install 'magit ))

(unless (package-installed-p 'gruvbox-theme)
  (package-install 'gruvbox-theme))

(unless (package-installed-p 'org-bullets)
  (package-install 'org-bullets))

(unless (package-installed-p 'projectile)
  (package-install 'projectile))

(unless (package-installed-p 'ivy)
  (package-install 'ivy))

1(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(use-package magit)
(use-package gruvbox-theme)
(use-package org)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         p("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(load-theme 'gruvbox t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (ivy projectile org-bullets magit gruvbox-theme use-package))))
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
