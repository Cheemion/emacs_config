;;; init file
(setq inhibit-startup-message t)
(scroll-bar-mode -1) ; Disble visible scrollbar
(tool-bar-mode 1)
(tooltip-mode -1)
(set-fringe-mode 10) ; Give some breathing room
(menu-bar-mode 1)
(setq visible-bell t) ; emacs will not bee everyday, but slash bell
;; Make ESC quit prompts, the same with VIM
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(fset 'yes-or-no-p 'y-or-n-p)
(global-set-key (kbd "<f5>") 'revert-buffer)
(setq gdb-many-windows 1)
(setq split-height-threshold nil)
(setq split-width-threshold 0)
;; show line number
(global-display-line-numbers-mode t)
(dolist (mode '(shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;;Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "http://mirrors.cloud.tencent.com/elpa/melpa/")
			 ("MELPA Stable" . "http://stable.melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; words search suggestion, space could be used, type a s could search alike spin
(use-package ivy
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
 	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))
(ivy-mode 1)
;; mode line package
(use-package all-the-icons) ;; run M-x all-the-icons-install-fonts the first time on a new machine
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))
(setq doom-modeline-minor-modes t) ;; whether display the minor modes in the mode-line

;;rainbow delimiter
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;which-key, tell you which key is availble by pop up screen
(use-package which-key
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3))

;; gives better looking interface of ivy information about commands
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history)))

;; helpful is an alternative to the built-in Emacs help
(use-package helpful
  :ensure t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; doom-theme
(use-package doom-themes)
(load-theme 'doom-molokai t)

;;evil
;; C-w pops up a lot of window related features
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-d-scroll t)
  :config
  (evil-mode 1))
(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
(define-key evil-normal-state-map (kbd "C-p") 'previous-line)
(define-key evil-normal-state-map (kbd "C-n") 'next-line)
(define-key evil-normal-state-map (kbd "C-a") 'move-beginning-of-line)
(define-key evil-normal-state-map (kbd "C-e") 'move-end-of-line)
(define-key evil-normal-state-map (kbd "M-.") 'xref-find-definitions)
;;extra evil functionality, such as press q to quit pop up panel
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; beacon for flashing the current line while switch buffers
(use-package beacon
  :ensure t
  :config
    (beacon-mode 1))

;; expand region
(use-package expand-region
:ensure t
:config
(global-set-key (kbd "C-=") 'er/expand-region))

;; syntak check
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))


;; manage project
(use-package projectile
:config (projectile-mode)
:custom ((projectile-completion-system 'ivy))
:bind-keymap
("C-c p" . projectile-command-map)
:init
(when (file-directory-p "~/projects")
  (setq projectile-project-search-path '("~/projects")))
(setq projectile-switch-project-action #'projectile-dired))

;; git
(use-package magit
:commands (magit-status magit-get-current-branch)
:custom
(magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; lsp
;;pip install 'python-lsp-server[all]'
;;sudo apt-get install clangd-12
;;usage C-c l g r and others
(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         ;;(python-mode . lsp-deferred)
         (c++-mode . lsp-deferred)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom(lsp-ui-doc-po))
;; treemacs
(use-package treemacs)
(use-package  lsp-treemacs
  :after lsp)
(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)
(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)
(setq treemacs-width 20)
;; company
(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode))
(add-hook 'after-init-hook 'global-company-mode)

;; commenting
(use-package evil-nerd-commenter
  :bind ("C-c C-c" . evilnc-comment-or-uncomment-lines))

;; Yasnippet
(use-package yasnippet
  :init
  (setq yas-snippet-dirs '("~/.emacs.d/my_snippets/"))
  :config
  (yas-global-mode))
 (defun check-expansion ()
    (save-excursion
      (if (looking-at "\\_>") t
        (backward-char 1)
        (if (looking-at "\\.") t
          (backward-char 1)
          (if (looking-at "->") t nil)))))

  (defun do-yas-expand ()
    (let ((yas/fallback-behavior 'return-nil))
      (yas/expand)))

  (defun tab-indent-or-complete ()
    (interactive)
    (if (minibufferp)
        (minibuffer-complete)
      (if (or (not yas/minor-mode)
              (null (do-yas-expand)))
          (if (check-expansion)
              (company-complete-common)
            (indent-for-tab-command)))))
(global-set-key (kbd "M-/") 'tab-indent-or-complete)

;; multiple line, usage C-d
(use-package evil-multiedit)
(evil-multiedit-default-keybinds)


(use-package undo-tree
  :ensure t
  :after evil
  :diminish
  :config
  (evil-set-undo-system 'undo-tree)
  (global-undo-tree-mode 1))
;;  windows control
(use-package winum
  :ensure t)
(global-set-key  (kbd "M-0") 'treemacs-select-window)
(global-set-key  (kbd "M-1") 'winum-select-window-1)
(global-set-key  (kbd "M-2") 'winum-select-window-2)
(global-set-key  (kbd "M-3") 'winum-select-window-3)
(global-set-key  (kbd "M-4") 'winum-select-window-4)
(global-set-key  (kbd "M-5") 'winum-select-window-5)
(global-set-key  (kbd "M-6") 'winum-select-window-6)
(global-set-key  (kbd "M-7") 'winum-select-window-7)
(global-set-key  (kbd "M-8") 'winum-select-window-8)
(winum-mode)



;; winner mode for save window layouts
(use-package winnder-mode
  :ensure nil
  :bind (:map evil-window-map
	      ("u" . winner-undo)
	      ("U" . winner-redo))
  :config
  (winner-mode))

;; diminish 
(use-package diminish)
(diminish 'company-mode)
(diminish 'ivy-mode)
(diminish 'flycheck-mode)
(diminish 'yas-minor-mode)
(diminish 'company-mode)


