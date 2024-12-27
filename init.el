;; -------------- ;;
;; Initial setups ;;
;; -------------- ;;

;; No start up message
(setq inhibit-startup-message t)

;; Remove some visuals
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)

;; Transparent background
(set-frame-parameter nil 'alpha-background 80)

;; ESC for quit
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Line numbers
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)

(dolist (mode '(org-mode-hook
                 term-mode-hook
                 eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq display-line-numbers-width-start t) ;; Stop the screen shifting
(setq scroll-conservatively 10000)
(setq scroll-margin 8)

;; Change the font
(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 140)

;; ----------------- ;;
;; Setup use-package ;;
;; ----------------- ;;

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(require 'use-package)
(setq use-package-always-ensure t)

;; ---------------- ;;
;; Install packages ;;
;; ---------------- ;;

;; Mini Buffer ;;

(use-package ivy
  :bind (:map ivy-minibuffer-map
          ("TAB" . ivy-alt-done))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file))
  :config
  (setq ivy-initial-inputs-alist nil))

(use-package smex) ; enable history for counsel-M-x

;; UI ;;

(use-package which-key
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 3))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; Keybinds ;;

;; Change the scrolling behavior
(defun max/half-down ()
  "Scroll half page down and recenter cursor."
  (interactive)
  (evil-scroll-down (/ (window-body-height) 2))
  (recenter))

(defun max/half-up ()
  "Scroll half page up and recenter cursor."
  (interactive)
  (evil-scroll-up (/ (window-body-height) 2))
  (recenter))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-undo-system 'undo-redo)
  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t)
  :config
  (evil-mode 1)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-global-set-key 'normal (kbd "C-d") 'max/half-down)
  (evil-global-set-key 'normal (kbd "C-u") 'max/half-up))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-commentary
  :config
  (evil-commentary-mode 1))

(use-package general
  :config
  (general-evil-setup)

  ;; SPC leader key
  (general-create-definer max/leader-def
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (max/leader-def
    "."  '(counsel-find-file :which-key "find file")
    "f"  '(:ignore t :which-key "files")
    "fs" '(save-buffer :which-key "save file")
    "b"  '(:ignore t :which-key "buffers")
    "bb" '(counsel-switch-buffer :which-key "switch buffer")
    "bk" '(kill-buffer :which-key "kill buffer")))

;; Visuals ;;

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)

  (load-theme 'doom-tomorrow-night t)

  (doom-themes-org-config))

(use-package doom-modeline
  :init (doom-modeline-mode 1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; -------------------- ;;
;; Custom Set Variables ;;
;; -------------------- ;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" default))
 '(delete-selection-mode nil)
 '(package-selected-packages '(evil-commentary general evil-collection ivy)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
