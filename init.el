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
(setq display-line-numbers-width 3)

;; Theme
(load-theme 'modus-vivendi-tritanopia)
(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 140)
(add-to-list 'default-frame-alist '(alpha . 80))

;; Show matching parentheses
(show-paren-mode t)

;; Auto-save and backup files in one place
(setq backup-directory-alist '(("." . "~/.config/emacs/backups")))
(setq auto-save-file-name-transforms '((".*" "~/.config/emacs/auto-save/" t)))

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
(savehist-mode 1)

;; Shortcuts
(eval-after-load 'dired
  '(define-key dired-mode-map (kbd "C-w") 'wdired-change-to-wdired-mode))

(global-set-key (kbd "C-.") 'duplicate-line)

;; Straight.el bootstrap
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(setq package-enable-at-startup nil)
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(use-package vertico
  :init (vertico-mode)
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)))

(use-package marginalia
  :init (marginalia-mode))

;; Orderless - flexible completion style
(use-package orderless
  
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; Corfu - in-buffer completion popup
(use-package corfu
  :custom
  (corfu-cycle t)                   ; Cycle through candidates
  (corfu-auto t)                    ; Enable auto completion
  (corfu-auto-delay 0.2)            ; Delay before showing completions
  (corfu-auto-prefix 2)             ; Minimum prefix length for auto completion
  (corfu-separator ?\s)             ; Orderless field separator
  (corfu-quit-no-match 'separator)  ; Don't quit if there's no match
  (corfu-preview-current nil)       ; Don't preview current candidate
  (corfu-preselect 'prompt)         ; Preselect the prompt
  (corfu-on-exact-match nil)        ; Don't auto-expand exact matches
  (corfu-scroll-margin 5)           ; Margin when scrolling
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous)
        ("RET" . corfu-insert)
        ("M-d" . corfu-show-documentation)
        ("M-l" . corfu-show-location))
  :init
  (global-corfu-mode))

;; Cape - completion-at-point extensions
(use-package cape
    :init
  ;; Add useful completion-at-point functions
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  :bind
  (("C-c p p" . completion-at-point) ;; capf
   ("C-c p t" . complete-tag)        ;; etags
   ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
   ("C-c p h" . cape-history)
   ("C-c p f" . cape-file)
   ("C-c p k" . cape-keyword)
   ("C-c p s" . cape-elisp-symbol)
   ("C-c p e" . cape-elisp-block)
   ("C-c p a" . cape-abbrev)
   ("C-c p l" . cape-line)
   ("C-c p w" . cape-dict)
   ("C-c p :" . cape-emoji)
   ("C-c p \\" . cape-tex)
   ("C-c p _" . cape-tex)
   ("C-c p ^" . cape-tex)
   ("C-c p &" . cape-sgml)
   ("C-c p r" . cape-rfc1345)))

;; Corfu terminal support (for terminal Emacs)
(use-package corfu-terminal
  :if (not (display-graphic-p))
  :config
  (corfu-terminal-mode +1))

;; Add icons to Corfu
(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package vterm
  :custom
  ;; Performance and behavior
  (vterm-max-scrollback 10000)
  (vterm-buffer-name-string "vterm %s")
  (vterm-kill-buffer-on-exit t)
  (vterm-clear-scrollback-when-clearing t)
  
  ;; Shell configuration
  (vterm-shell (executable-find "fish"))
  (vterm-timer-delay 0.01)
  
  ;; Don't query on exit
  (vterm-always-compile-module t)
  
  :bind
  (;("C-c C-t" . vterm)
   (:map vterm-mode-map ; Open new vterm
        ("C-q" . vterm-send-next-key)  ; Send next key literally
        ("M-," . vterm-copy-mode)))       ; Enter copy mode

  
  :config
  ;; Don't use line numbers in vterm
  (add-hook 'vterm-mode-hook
            (lambda ()
              (display-line-numbers-mode -1)
              (setq-local global-hl-line-mode nil))))

;; Multi-vterm for managing multiple terminals
(use-package multi-vterm
  ;:after vterm
  :bind
  (("C-c t t" . multi-vterm)
   ("C-c t n" . multi-vterm-next)
   ("C-c t p" . multi-vterm-prev)))

;; vterm-toggle for quick terminal access
(use-package vterm-toggle
  :after vterm
  :custom
  (vterm-toggle-fullscreen-p nil)
  (vterm-toggle-scope 'project)
  :bind
  (("C-`" . vterm-toggle)
   ("C-~" . vterm-toggle-cd)
   :map vterm-mode-map
   ("C-`" . vterm-toggle)))

