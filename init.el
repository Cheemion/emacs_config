(setq inhibit-startup-message t)
(scroll-bar-mode -1) ; Disble visible scrollbar
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10) ; Give some breathing room
(menu-bar-mode -1)
(setq visible-bell t) ; emacs will not bee everyday, but slash bell


;; Make ESC quit prompts, the same with VIM
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(fset 'yes-or-no-p 'y-or-n-p)
(global-set-key (kbd "<f5>") 'revert-buffer)
;; show line number
(global-display-line-numbers-mode t)
(dolist (mode '(shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;;Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "http://mirrors.cloud.tencent.com/elpa/melpa/")
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
;;use to record commands, enable the mode and use clm/toggle xxx to show up the commands windows
;(use-package command-log-mode)

;; code completion, space could be used, type a s could search alike spin
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
;;(define-key evil-normal-state-map (kbd "C-a") 'move-beginning-of-line)
;(define-key evil-normal-state-map (kbd "C-e") 'move-end-of-line)
;; extra evil functionality, such as press q to quit pop up panel
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

;; manage project
(use-package projectile
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map))
