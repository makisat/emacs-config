;; Better defaults
(setq inhibit-startup-message t)         ; Skip startup screen
(tool-bar-mode -1)                       ; Disable toolbar
(menu-bar-mode -1)                       ; Disable menu bar
(scroll-bar-mode -1)                     ; Disable scrollbar
(setq ring-bell-function 'ignore)        ; Disable bell
(add-to-list 'default-frame-alist '(undecorated . t)) ; Disable titlebar
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Line numbers and columns
(setq display-line-numbers-type 'relative)  ; Relative line numbers
(global-display-line-numbers-mode t)        ; Show line numbers
(column-number-mode nil)                    ; Show column in mode line
(setq display-line-numbers-width 3)

;; Visual
(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 140)
(add-to-list 'default-frame-alist '(alpha-background . 80))

;; Auto-save and backup files in one place
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save/" t)))

;; Refresh buffers when files change on disk
(global-auto-revert-mode t)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Remember recently edited files
(recentf-mode t)
(setq recentf-max-saved-items 10)
(savehist-mode 1)

;; Shortcuts
(eval-after-load 'dired
  '(define-key dired-mode-map (kbd "C-w") 'wdired-change-to-wdired-mode))

(global-set-key (kbd "C-.") 'duplicate-line)

(global-set-key (kbd "C-c a") 'org-agenda)

;; Add registers
(global-set-key (kbd "C-c SPC 1") (lambda () (interactive) (point-to-register ?1) (message "Added 1")))
(global-set-key (kbd "C-c SPC 2") (lambda () (interactive) (point-to-register ?2) (message "Added 2")))
(global-set-key (kbd "C-c SPC 3") (lambda () (interactive) (point-to-register ?3) (message "Added 3")))
(global-set-key (kbd "C-c SPC 4") (lambda () (interactive) (point-to-register ?4) (message "Added 4")))

;; Jump between the registers
(global-set-key (kbd "C-c 1") (lambda () (interactive) (jump-to-register ?1)))
(global-set-key (kbd "C-c 2") (lambda () (interactive) (jump-to-register ?2)))
(global-set-key (kbd "C-c 3") (lambda () (interactive) (jump-to-register ?3)))
(global-set-key (kbd "C-c 4") (lambda () (interactive) (jump-to-register ?4)))

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

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package corfu
  :custom
  (corfu-cycle t)                   ; Cycle through candidates
  (corfu-auto t)                    ; Enable auto completion
  (corfu-auto-delay 0.2)            ; Delay before showing completions
  (corfu-auto-prefix 2)             ; Minimum prefix length for auto completion
  ;; (corfu-separator ?\s)             ; Orderless field separator
  ;; (corfu-quit-no-match 'separator)  ; Don't quit if there's no match
  ;; (corfu-preview-current nil)       ; Don't preview current candidate
  ;; (corfu-preselect 'prompt)         ; Preselect the prompt
  ;; (corfu-on-exact-match nil)        ; Don't auto-expand exact matches
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
  ;; :init
  ;; (global-corfu-mode))
  :hook (prog-mode . corfu-mode))

(use-package cape
  :init
  ;; Add useful completion-at-point functions
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  ;;(add-to-list 'completion-at-point-functions #'cape-dabbrev)
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

;; (use-package vterm
;;   :custom
;;   ;; Performance and behavior
;;   (vterm-max-scrollback 1000)
;;   (vterm-buffer-name-string "vterm %s")
;;   (vterm-kill-buffer-on-exit t)
;;   (vterm-clear-scrollback-when-clearing t)
  
;;   ;; Shell configuration
;;   (vterm-shell (executable-find "fish"))
;;   (vterm-timer-delay 0.01)
  
;;   ;; Don't query on exit
;;   (vterm-always-compile-module t)
;;   :bind
;;   ("C-c t" . vterm)

;;   :config
;;   (add-hook 'vterm-mode-hook
;;             (lambda ()
;;               (display-line-numbers-mode -1)
;;               (setq-local global-hl-line-mode nil))))

(use-package move-text
  :config
  (move-text-default-bindings)
  (defun indent-region-advice (&rest ignored)
    (let ((deactivate deactivate-mark))
      (if (region-active-p)
          (indent-region (region-beginning) (region-end))
        (indent-region (line-beginning-position) (line-end-position)))
      (setq deactivate-mark deactivate)))

  (advice-add 'move-text-up :after 'indent-region-advice)
  (advice-add 'move-text-down :after 'indent-region-advice))

(use-package multiple-cursors
  :bind (("C-c m" . mc/mark-pop)
         ("C->"   . mc/mark-next-like-this)
         ("C-<"   . mc/mark-previous-like-this)
	 ("C-S-<mouse-1>" . mc/add-cursor-on-click)
         :map mc/keymap
         ("<return>" . nil)))

(use-package projectile
  :init
  (projectile-mode t)
  :bind
  (:map projectile-mode-map
	("C-x p" . projectile-command-map)))

(use-package which-key
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 3))

(use-package magit
  :bind
  ("C-c g" . magit))

(use-package diff-hl
  :hook
  ((prog-mode . diff-hl-mode)
   (dired-mode . diff-hl-dired-mode)
   (magit-pre-refresh . diff-hl-magit-pre-refresh)
   (magit-post-refresh . diff-hl-magit-post-refresh))
  :custom
  (diff-hl-side 'left)
  (diff-hl-draw-borders nil)
  :config
  (global-diff-hl-mode))

(use-package mozc
  :config (setq default-input-method "japanese-mozc"))

(use-package org
  :custom
  ;; Directory settings
  (org-agenda-files '("~/Sync/OrgNotes/"))
  
  ;; Appearance
  (org-hide-emphasis-markers t)  ; Hide markup markers like * and /
  (org-pretty-entities t)
  (org-startup-indented t)       ; Enable org-indent-mode by default
  ; (org-startup-folded 'content)  ; Start with content visible
  (org-cycle-separator-lines 2)
  
  ;; Editing behavior
  (org-return-follows-link t)
  (org-src-tab-acts-natively t)
  (org-src-preserve-indentation t)
  (org-edit-src-content-indentation 0)
  (org-src-fontify-natively t)   ; Syntax highlight in source blocks
  (org-confirm-babel-evaluate nil)
  (org-support-shift-select t)
  :hook ((org-mode . visual-line-mode)
         (org-mode . flyspell-mode)
         (org-mode . (lambda () (display-line-numbers-mode -1)))
         (org-agenda-mode . (lambda () (display-line-numbers-mode -1))))
  :bind ("C-c a" . org-agenda))

;; Start the agenda on the current day (not previous Monday)
(setq org-agenda-start-on-weekday nil)

;; To make latex-preview to work
(require 'cl-lib)

(with-eval-after-load 'org
  (setq org-latex-default-packages-alist
        (cl-remove-if (lambda (entry)
                        (string-match-p "ulem" (nth 1 entry))) ; package name is 2nd element
                      org-latex-default-packages-alist)))

(use-package olivetti
  :hook ((org-mode . olivetti-mode)
         (org-agenda-mode . olivetti-mode)))

(use-package doom-themes
  :config (load-theme 'doom-tomorrow-night t))

(use-package doom-modeline
  :init (doom-modeline-mode 1))

;; Better org bullets
(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package org-modern
  :config
  (global-org-modern-mode)
  (setq org-modern-star nil))

(use-package indent-bars
  :hook (prog-mode . indent-bars-mode))

;; Org roam for note-taking (Zettelkasten method)
(use-package org-roam
  :custom
  (org-roam-directory "~/Sync/RoamNotes/")
  (org-roam-completion-everywhere t)
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n c" . org-roam-capture)
   ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (org-roam-db-autosync-mode))

;; Nerd icons (modern alternative to all-the-icons)
(use-package nerd-icons
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

;; Nerd icons for dired
(use-package nerd-icons-dired
  :after nerd-icons
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package yasnippet
  :config (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package racket-mode)

(use-package slime
  :config (setq inferior-lisp-program "sbcl"))

(use-package rust-mode)

(use-package zig-mode)

(use-package go-mode)

(use-package markdown-mode)

(use-package web-mode)
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . tsx-ts-mode))

(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-ignored-server-capabilities :inlayHintProvider))

(global-set-key (kbd "C-c t")
  (lambda ()
    (interactive)
    (start-process "ghostty" nil "ghostty" "--window-inherit-working-directory")))

;; (add-to-list 'auto-mode-alist '("\\.c\\'" . c-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-ts-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("4594d6b9753691142f02e67b8eb0fda7d12f6cc9f1299a49b819312d6addad1d"
     "7771c8496c10162220af0ca7b7e61459cb42d18c35ce272a63461c0fc1336015"
     default))
 '(whitespace-style
   '(face trailing tabs spaces lines newline missing-newline-at-eof empty
          indentation space-after-tab space-before-tab space-mark
          tab-mark newline-mark)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
