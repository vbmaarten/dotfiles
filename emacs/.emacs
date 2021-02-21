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
