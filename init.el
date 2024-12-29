;; Move custom set variables
(setq custom-file "~/.config/emacs/custom.el")
(load custom-file 'no-error 'no-message)

;; Set backup and auto-save files directory
(setq backup-directory-alist
      '(("." . "~/.config/emacs/backup-files")))
(make-directory "~/.config/emacs/backup-files/" t)

(setq auto-save-file-name-transforms
      `((".*" "~/.config/emacs/auto-save-files/" t)))

(make-directory "~/.config/emacs/auto-save-files/" t)

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

;; Line numbers
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)

(dolist (mode '(org-mode-hook
                org-agenda-mode-hook
                term-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq display-line-numbers-width-start t) ;; Stop the screen shifting

(setq scroll-conservatively 10000)
(setq scroll-margin 8)

;; Set tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default tab-always-indent nil)

;; Change the font
(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 140)

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package ivy
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
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

(use-package projectile
  :config (projectile-mode 1)
  :custom (projectile-completion-system 'ivy))

(use-package counsel-projectile
  :config
  (counsel-projectile-mode)
  (setq projectile-enable-caching t))

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

(use-package magit)

(use-package general
  :config
  ;; SPC leader key
  (global-unset-key (kbd "M-SPC"))
  (general-create-definer max/leader-def
    :prefix "M-SPC")

  (max/leader-def
    "."  '(counsel-find-file :which-key "find file")
    "f"  '(:ignore t :which-key "files")
    "ff" '(counsel-find-file :which-key "find file")
    "fs" '(save-buffer :which-key "save file")
    "b"  '(:ignore t :which-key "buffers")
    "bb" '(counsel-switch-buffer :which-key "switch buffer")
    "bk" '(kill-buffer :which-key "kill buffer")
    "bp" '(previous-buffer :which-key "preivous buffer")
    "bn" '(next-buffer :which-key "next buffer")
    "p"  'projectile-command-map
    "g"  '(:ignore t :which-key "magit")
    "gg" '(magit :which-key "magit status")
    "gd" '(magit-diff :which-key "magit diff")
    "m"  'bookmark-map))

;; Custom backspace
(defun max/backspace-whitespace-to-tab-stop ()
  "Delete whitespace backwards to the next tab-stop, otherwise delete one character."
  (interactive)
  (if (or indent-tabs-mode (use-region-p)
          (> (point)
             (save-excursion
               (back-to-indentation)
               (point))))
      (call-interactively 'backward-delete-char)
    (let ((step (% (current-column) tab-width))
          (pt (point)))
      (when (zerop step)
        (setq step tab-width))
      ;; Account for edge case near beginning of buffer.
      (setq step (min (- pt 1) step))
      (save-match-data
        (if (string-match "[^\t ]*\\([\t ]+\\)$"
                          (buffer-substring-no-properties
                           (- pt step) pt))
            (backward-delete-char (- (match-end 1)
                                     (match-beginning 1)))
          (call-interactively 'backward-delete-char))))))

(general-define-key
 :keymaps 'prog-mode-map
 "<backspace>" 'max/backspace-whitespace-to-tab-stop)

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

(use-package org
  :hook
  (org-mode . visual-line-mode)
  (org-mode . org-indent-mode)
  (org-mode . yas-minor-mode))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "◆" "◉" "○" "◆")))

(custom-set-faces
 '(org-todo ((t (:background "light green" :foreground "black" :weight bold))))
 '(org-done ((t (:background "gray30" :foreground "white" :weight bold)))))

;; Org babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)))

(setq org-confirm-babel-evaluate nil)

(use-package olivetti
  :after org
  :hook ((org-mode org-agenda-mode) . olivetti-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :config
  (lsp-enable-which-key-integration t)
  (general-define-key
   :prefix "M-SPC"
   "l"  lsp-command-map
   "lk" 'lsp-ui-doc-show))

(use-package lsp-ui
  :custom (lsp-ui-doc-position 'at-point))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind
  (:map company-active-map
        ("<tab>" . company-complete-selection))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package yasnippet
  :hook (lsp-mode . yas-minor-mode)
  :bind (:map yas-minor-mode-map
              ("C-<tab>" . yas-expand))
  :config (yas-reload-all))

(add-hook 'emacs-lisp-mode-hook 'company-mode)

(defun max/go-mode-hook ()
  "My go mode hook"
  (interactive)
  (setq indent-tabs-mode nil)
  (setq tab-width 4)
  (setq tab-always-indent nil)

  (whitespace-mode 1)
  (setq whitespace-newline 'newline-mark))  

(use-package go-mode
  :mode "\\.go\\'"
  :hook
  (go-mode . lsp-deferred)
  (go-mode . max/go-mode-hook))
