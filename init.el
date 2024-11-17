(setq inhibit-startup-message t)

; --- Simplify UI --- ;

(scroll-bar-mode -1)    ; Disable visible scrollbar
(tool-bar-mode -1)      ; Disable the toolbar
(tooltip-mode -1)       ; Disable tooltips
(set-fringe-mode 10)    ; Give some breathing room

(menu-bar-mode -1)      ; Disable the menu bar

;; (set-frame-font "RobotoMono Nerd Font 140" nil t)
(add-to-list 'default-frame-alist '(font . "Iosevka Nerd Font 14"))

;; Line number
(setq display-line-numbers-width 3) ; Adjust the number as needed
(add-hook 'prog-mode-hook (lambda ()
                            (setq display-line-numbers-type 'relative)
                            (display-line-numbers-mode 1)))

;; Stop the screen shifting
(setq display-line-numbers-width-start t)

;; Set scroll margin to 8 lines
(setq scroll-margin 8)

;; Optional: Make scrolling less jumpy
(setq scroll-conservatively 101)

;; Optional: Disable scroll acceleration
(setq scroll-step 1)

; Set theme
(load-theme 'wombat)

;; Disable backup files
(setq make-backup-files nil)

; Esc to quit
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

; --- Initialize Packages --- ;

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize packages for non-linux platforms
;(unless (package-installed-p 'use-package)
;  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

; --- Install Packages --- ;

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
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-seaarch-kill))
  :config
  (ivy-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)) ; don't start searches with ^

(use-package evil
  :ensure t
  :config
  ;; Scrolling
  (defun my-scroll-half-page-down-and-recenter ()
    "Scroll half page down and recenter cursor."
    (interactive)
    (evil-scroll-down (/ (window-body-height) 2))
    (recenter))

  (defun my-scroll-half-page-up-and-recenter ()
    "Scroll half page up and recenter cursor."
    (interactive)
    (evil-scroll-up (/ (window-body-height) 2))
    (recenter))

  ;; Bind the functions to C-d and C-u in normal mode
  (define-key evil-normal-state-map (kbd "C-d") 'my-scroll-half-page-down-and-recenter)
  (define-key evil-normal-state-map (kbd "C-u") 'my-scroll-half-page-up-and-recenter)
  (evil-mode 1))  ; Enable evil-mode globally

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-height 25)  ; Set the height of the modeline
  (doom-modeline-bar-width 3) ; Set the width of the bar
  (doom-modeline-icon t) ; Enable icons (requires support for icons)
  (doom-modeline-major-mode-icon t) ; Show major mode icon
  (doom-modeline-buffer-state-icon t) ; Show buffer state icons
  (doom-modeline-enable-word-count t) ; Show word count when editing text
  (doom-modeline-buffer-file-name-style 'truncate-with-project) ; Customize buffer file name style
  (doom-modeline-minor-modes nil) ; Hide minor modes for a cleaner look
  (doom-modeline-env-version t) ; Show environment version (e.g., Python version)
  (doom-modeline-lsp t) ; Show LSP status
  (doom-modeline-vcs-max-length 20)) ; Set max length for version control info
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(delete-selection-mode nil)
 '(package-selected-packages '(doom-modeline evil counsel ivy)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
