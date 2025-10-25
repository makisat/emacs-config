;; Better defaults
(setq inhibit-startup-message t)        ; Skip startup screen
(tool-bar-mode -1)                      ; Disable toolbar
(menu-bar-mode -1)                      ; Disable menu bar
(scroll-bar-mode -1)                    ; Disable scrollbar
(setq ring-bell-function 'ignore)       ; Disable bell

;; Line numbers and columns
(setq display-line-numbers-type 'relative)  ; Relative line numbers
(global-display-line-numbers-mode t)        ; Show line numbers
(column-number-mode nil)                    ; Show column in mode line

;; Theme
(load-theme 'wombat)

;; Better scrolling
(setq scroll-conservatively 100)        ; Smooth scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)

;; Show matching parentheses
(show-paren-mode t)

;; Auto-save and backup files in one place
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save/" t)))

;; Refresh buffers when files change on disk
(global-auto-revert-mode t)

;; Remember cursor position in files
(save-place-mode t)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Delete selection when typing
(delete-selection-mode t)

;; Automatic pair closing
(electric-pair-mode t)

;; Remember recently edited files
(recentf-mode t)
(setq recentf-max-saved-items 10)
