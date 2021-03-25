;; Configure some default emacs settings
(setq inhibit-startup-message t)
(defalias 'yes-or-no-p 'y-or-n-p)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 0)

(menu-bar-mode -1)

(setq visible-bell t)
(setq ring-bell-function 'asc:flash-background)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(column-number-mode)
;; Setup package to install use package
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
	                 ("org" . "https://orgmode.org/elpa")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents (package-refresh-contents))

;; Install and configure  use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Install packages
(use-package doom-themes
  :config
  (load-theme 'doom-palenight t))

(use-package doom-modeline
  :config
  (doom-modeline-mode))


(use-package which-key
  :init (which-key-mode)
  :diminish which-kgey-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package counsel
  :config
  (ivy-mode 1)
  :bind (("C-s" . swiper-isearch)
	 ("M-x" . counsel-M-x)
	 ("C-x b" . ivy-switch-buffer)))

(use-package magit)

(use-package org)
(setq org-startup-indented t)

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package pdf-tools)

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :after evil
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init)

(setq projectile-project-search-path '("~/Projects/"))

;; NOTE: If you want to move everything out of the ~/.emacs.d folder
;; reliably, set `user-emacs-directory` before loading no-littering!
;(setq user-emacs-directory "~/.cache/emacs")

(use-package no-littering)

;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(use-package wn-mode
  :init
  (wn-mode))

(use-package all-the-icons
  :init
    (unless (member "all-the-icons" (font-family-list))
    (all-the-icons-install-fonts t)))

(use-package treemacs
  :demand t
  :config
  (treemacs-git-mode 'simple)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action)  
  :bind
  (("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-magit
  :after (treemacs))

(use-package treemacs-all-the-icons
  :after (treemacs all-the-icons)
  :config
  (treemacs-load-theme "all-the-icons")
  )

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-icons-dired
  :after (treemacs dired)
  :config (treemacs-icons-dired-mode))


(add-hook 'projectile-after-switch-project-hook 'treemacs-display-current-project-exclusively)

(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-ivy)

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package git-gutter)


(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

(use-package git-gutter)

(use-package company)

(add-hook 'prog-mode-hook 'git-gutter-mode)
(add-hook 'prog-mode-hook 'company-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(use-package general)
(use-package windresize)

(general-define-key
 "C-c w" 'windresize)

(use-package eyebrowse
  :demand
  :init
  (eyebrowse-mode 1)
  :config
  (setq eyebrowse-new-workspace t)
 )

(general-define-key
 :states '(normal visual insert emacs)
 :prefix "C-SPC"
 "0" '(eyebrowse-switch-to-window-config-0 :which-key "Default workspace") 
 "1" '(eyebrowse-switch-to-window-config-1 :which-key "Workspace 1")
 "2" '(eyebrowse-switch-to-window-config-2 :which-key "Workspace 2")
 "3" '(eyebrowse-switch-to-window-config-3 :which-key "Workspace 3")
 "4" '(eyebrowse-switch-to-window-config-4 :which-key "Workspace 4")
 "5" '(eyebrowse-switch-to-window-config-5 :which-key "Workspace 5")
 "6" '(eyebrowse-switch-to-window-config-6 :which-key "Workspace 6")
 "7" '(eyebrowse-switch-to-window-config-7 :which-key "Workspace 7")
 "8" '(eyebrowse-switch-to-window-config-8 :which-key "Workspace 8")
 "9" '(eyebrowse-switch-to-window-config-9 :which-key "Workspace 9")
 "t" '(:ignore t :which-key "Toggles")
 "tt" 'treemacs
 "a" 'avy-goto-char
 )
(use-package json-mode)
(add-to-list 'auto-mode-alist '("\\.json\\'" . 'json-mode))

(use-package ace-window
  :bind ("M-o" . ace-window))

(use-package ace-jump-mode
  :bind ("C-c j" . ace-jump-mode
	 ))

(use-package yasnippet
  :init
  (yas-global-mode 1))

(use-package yasnippet-snippets)

(use-package sublimity
  :init
  (require 'sublimity-scroll)
  (sublimity-mode 1))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1) 
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  (evil-set-initial-state 'doc-view-mode 'emacs)
  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))
