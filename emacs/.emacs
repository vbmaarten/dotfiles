(setq inhibit-startup-message t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 0)

(menu-bar-mode -1)

(setq visible-bell t)
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")))

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

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(use-package magit)
(use-package gruvbox-theme)
(load-theme 'gruvbox t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (magit gruvbox-theme use-package))))
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
