;;; a tidied-up version of my .emacs file

;; add MELPA
(require 'package)
(add-to-list 'package-archives
                          '("melpa" . "https://melpa.milkbox.net/packages/"))
(when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
      (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; packages
(use-package diminish :demand t)
(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))
(use-package auto-complete :defer t)
(use-package clang-format :defer t)
(use-package haskell-mode :defer t)
(use-package go-mode :defer t)
(use-package rust-mode :defer t)

;; add a directory for loading lisp files
(add-to-list 'load-path "~/.emacs.d/lisp")

;; set jdee server directory
(setq jdee-server-dir "~/.emacs.d/jdee-server-dir")

;; clang-format
(global-set-key (kbd "C-<tab>") 'clang-format-region)
(global-set-key (kbd "C-M-<tab>") 'clang-format-buffer)
;; don't indent namespaces

;; jedi setup for Python
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook (lambda ()
                              (auto-complete-mode 1)
                              (setq ac-sources '(ac-source-jedi-direct))))
(setq jedi:complete-on-dot t)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :custom
  (flycheck-python-pycompile-executable "python3")
  (flycheck-check-syntax-automatically '(mode-enabled save)))

(use-package cc-mode
  :config
  (add-hook 'c++-mode-hook (lambda ()
                             (c-set-offset 'innamespace [0])))
  (add-hook 'c++-mode-hook (lambda ()
                             (setq flycheck-gcc-language-standard "c++17"))))

(use-package ivy
  :defer t
  :diminish
  :custom
  (ivy-use-virtual-buffers t))

(use-package counsel
  :after ivy
  :diminish
  :bind (("C-c g" . 'counsel-git)
         ("C-c j" . 'counsel-git-grep)
         ("C-c k" . 'counsel-ag)
         ("C-x l" . 'counsel-locate)))

(use-package abbrev
  :defer t
  :diminish abbrev-mode)

(use-package magit
  :defer t
  :bind (("C-x g" . 'magit-status)))

(use-package evil
  :demand t
  :diminish undo-tree-mode
  :config
  (evil-mode t)
  (define-key evil-normal-state-map (kbd "C-r") 'isearch-backward))

(use-package midnight
  :bind (("C-c z" . clean-buffer-list)))

;; yes/no to y/n
(defalias 'yes-or-no-p 'y-or-n-p)

(use-package desktop
  :demand t
  :init (desktop-save-mode 1)
  :custom
  (desktop-restore-eager 1))

;; copy to clipboard
(setq x-select-enable-clipboard t)

;; remove junk characters in compilation window
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;; ibuffer
(defalias 'list-buffers 'ibuffer)

;; hide toolbar, scroll-bar, menu-bar
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(menu-bar-mode -1)

;; no blinking cursor
(setq blink-cursor-mode nil)

;; show columns
(setq column-number-mode t)

;; comments
(global-set-key (kbd "C-;") 'comment-dwim)

;; trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Fullscreen
(defun toggle-fullscreen ()
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                 '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                 '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
)
(toggle-fullscreen)

;; disable Ctrl-Z
(global-unset-key (kbd "C-z"))

;; Auto-indent
(define-key global-map (kbd "RET") 'newline-and-indent)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Font stuff
(set-frame-font "Inconsolata 12" nil t)

;; Theme settings
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(defadvice load-theme (before theme-dont-propagate activate)
  (mapcar #'disable-theme custom-enabled-themes))

;; Rebind C-w to backward-kill-word
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

;; compile bindings
(global-set-key (kbd "C-x j") 'compile)
(global-set-key (kbd "<f5>")  'compile)

;; open header file in other window
(setq ff-search-directories '("." "../include" "../src"))
(defun ssbl/open-header-in-other-window ()
  "Opens the .h or .c file for the current window's file in other window."
  (interactive)
  (ff-find-other-file 't)
  (other-window 1))
(global-set-key (kbd "C-x C-h") 'ssbl/open-header-in-other-window)
