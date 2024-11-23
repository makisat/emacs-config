(setq inhibit-startup-message t)

;; Simplify UI

(scroll-bar-mode -1)    ; Disable visible scrollbar
(tool-bar-mode -1)      ; Disable the toolbar
(tooltip-mode -1)       ; Disable tooltips
(set-fringe-mode 10)    ; Give some breathing room

(menu-bar-mode -1)      ; Disable the menu bar

;; Set font
(add-to-list 'default-frame-alist '(font . "Iosevka Nerd Font 14"))

;; Line number
(add-hook 'prog-mode-hook (lambda ()
			                (global-display-line-numbers-mode t)
                            (setq display-line-numbers-type 'relative)))

;; Stop the screen shifting
(setq display-line-numbers-width-start t)

;; Set scroll margin to 8 lines
(setq scroll-margin 8)

;; Optional: Make scrolling less jumpy
(setq scroll-conservatively 101)

;; Optional: Disable scroll acceleration
(setq scroll-step 1)

;; Disable backup files
(setq make-backup-files nil)

;; Esc to quit
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Tab and indetation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; Initialize Packages

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
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

;; Install Packages

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

;; Description for commands
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	     ("C-x b" . counsel-ibuffer)
	     ("C-x C-f" . counsel-find-file)
	     :map minibuffer-local-map
	     ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)) ; don't start searches with ^

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.5))

(use-package general
  :ensure t
  :config
  ;; Create a leader key
  (general-create-definer my-leader-def
    :keymaps '(normal visual emacs)
    :prefix "SPC")

  ;; Bind some global keys
  (my-leader-def
    "." '(counsel-find-file :which-key "find file")
    "fs" '(save-buffer :which-key "save file")
    "b"  '(:ignore t :which-key "buffers")
    "bb" '(counsel-switch-buffer :which-key "switch buffer")
    "bp" '(previous-buffer :which-key "previous buffer")
    "bk" '(kill-buffer :which-key "kill buffer")
    "w"  '(:ignore t :which-key "windows")
    "wv" '(split-window-below :which-key "split below")
    "ws" '(split-window-right :which-key "split right")
    "wc" '(delete-window :which-key "close window")))

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-redo)
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

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package helpful
  :ensure t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

(use-package doom-themes
  :ensure t
  ;; Transparent backgrourd
  :config
  (add-to-list 'default-frame-alist '(alpha-background . 70))
  (load-theme 'doom-tokyo-night t))

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

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Org-mode basic setup
(use-package org
  :ensure t
  :init
  (setq org-agenda-span 'week
	    org-agenda-start-day "+0d"
	    org-agenda-show-all-dates t ;; Show all dates in agenda
        org-agenda-start-on-weekday nil ;; Ensure custom start-day works
	    org-agenda-window-setup 'current-window ;; Open agenda in the current window
        org-agenda-hide-tags-regexp ".*"
	    org-agenda-skip-scheduled-if-done t
	    org-agenda-skip-deadline-if-done t
	    org-agenda-skip-timestamp-if-done t
	    org-agenda-skip-scheduled-if-deadline-is-shown t
	    org-agenda-skip-timestamp-if-deadline-is-shown t)
  
  :hook ((org-agenda-mode . olivetti-mode)
	     (org-agenda-mode . (lambda () (display-line-numbers-mode 0))))

  :config
  ;; org agenda file
  (setq org-agenda-files '("~/Sync/org/todos.org"))

  ;; Enable org-indent mode by default
  (setq org-startup-indented t)

  ;; Automatically wrap lines in org-mode
  (setq org-startup-truncated nil)

  ;; Enable syntax highlighting for source blocks
  (setq org-src-fontify-natively t)

  ;; Enable editing of source code blocks
  (setq org-edit-src-content-indentation 2)

  ;; Keybindings for Org Mode
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c l" . org-store-link)))

;; Use org-modern for better Org Mode visuals
(use-package org-modern
  :ensure t
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda)
	     (org-mode . (lambda () (display-line-numbers-mode 0))))

  :config
  ;; Customize org-modern settings
  (setq org-modern-star '("•" "◦" "▸")    ;; Customize bullet points
        org-modern-label-border 0.2)       ;; Add padding to labels
                                        ;org-modern-table nil              ;; Disable table enhancement if you prefer plain tables
                                        ; org-modern-todo nil               ;; Keep the default TODO styles
                                        ;org-modern-progress nil           ;; Disable progress bars if not needed
                                        ;org-modern-priority nil)          ;; Disable priority enhancement
  ;; Adjust line spacing for better readability
  (setq line-spacing 0.2))

(use-package olivetti
  :hook (org-mode . olivetti-mode)
  :config
  (setq olivetti-body-width 100))

;; Custom set variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("456697e914823ee45365b843c89fbc79191fdbaff471b29aad9dcbe0ee1d5641" "014cb63097fc7dbda3edf53eb09802237961cbb4c9e9abd705f23b86511b0a69" "48042425e84cd92184837e01d0b4fe9f912d875c43021c3bcb7eeb51f1be5710" "4594d6b9753691142f02e67b8eb0fda7d12f6cc9f1299a49b819312d6addad1d" default))
 '(delete-selection-mode nil)
 '(package-selected-packages
   '(org-agenda olivetti evil-collection org-modern doom-themes helpful ivy-rich which-key rainbow-delimiters doom-modeline evil counsel ivy)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
