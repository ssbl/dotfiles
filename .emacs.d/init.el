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
(use-package haskell-mode :defer t)
(use-package go-mode :defer t)
(use-package rust-mode :defer t)
(use-package modern-cpp-font-lock :defer t)

;; add a directory for loading lisp files
(add-to-list 'load-path "~/.emacs.d/lisp")

;; set jdee server directory
(setq jdee-server-dir "~/.emacs.d/jdee-server-dir")

(use-package clang-format
  :defer t
  :after cc-mode
  :bind (("C-<tab>"   . clang-format-region)
         ("C-M-<tab>" . clang-format-buffer)))

(use-package jedi
  :defer t
  :preface
  (defun my-python-mode-hook ()
    (jedi:setup)
    (auto-complete-mode 1)
    (setq ac-sources '(ac-source-jedi-direct)))
  :hook (python-mode . my-python-mode-hook)
  :custom
  (jedi:complete-on-dot t))

(use-package ycmd
  :ensure t
  :init
  (set-variable 'ycmd-server-command '("python" "/home/shubham/src/ycmd/ycmd"))
  (set-variable 'ycmd-extra-conf-whitelist '("~/.ycm_extra_conf.py"))
  (set-variable 'ycmd-global-config
		(expand-file-name "~/.ycm_extra_conf.py")))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :custom
  (flycheck-python-pycompile-executable "python3")
  (flycheck-check-syntax-automatically '(mode-enabled save)))

(use-package flycheck-ycmd
  :ensure t
  :init (flycheck-ycmd-setup))

(use-package eyebrowse
  :ensure t
  :init (eyebrowse-mode t))

(use-package projectile
  :ensure t
  :diminish)

(use-package cc-mode
  :preface
  (defun my-c-mode-common-hook ()
    (setq c-block-comment-prefix " * ")
    (setq c-basic-offset 4))
  (defun my-c-mode-hook ()
    ;; placeholder
    )
  (defun my-c++-mode-hook ()
    (ycmd-mode)
    (c-set-offset 'innamespace [0])
    (setq flycheck-clang-include-path
          (mapcar #'expand-file-name '("../include" "../src")))
    (setq flycheck-clang-language-standard "c++17")
    (modern-c++-font-lock-mode))
  :hook (c-mode . my-c-mode-hook)
  :hook (c++-mode . my-c++-mode-hook)
  :config
  (setq c-mode-common-hook 'my-c-mode-common-hook)
  (setq flycheck-c/c++-clang-executable "/usr/bin/clang-7"))

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

(use-package smart-newline
  :diminish
  :bind (("C-j" . smart-newline))
  :commands smart-newline-mode)

(use-package server
  :init
  (add-hook 'after-init-hook 'server-start))

(use-package midnight
  :bind (("C-c z" . clean-buffer-list))
  :custom
  (clean-buffer-list-delay-special 7))

;; yes/no to y/n
(defalias 'yes-or-no-p 'y-or-n-p)

(use-package desktop
  :demand t
  :init (desktop-save-mode 1)
  :custom
  (desktop-restore-eager 1))

(use-package auto-revert
  :defer t
  :diminish)

;; copy to clipboard
(setq select-enable-clipboard t)

;; remove junk characters in compilation window
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;; ibuffer
(defalias 'list-buffers 'ibuffer)

;; hide toolbar, scroll-bar, menu-bar
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(menu-bar-mode -1)
(horizontal-scroll-bar-mode -1)

;; no blinking cursor
(setq blink-cursor-mode nil)

;; show columns
(setq column-number-mode t)

;; comments
(global-set-key (kbd "C-;") 'comment-dwim)

;; trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; disable Ctrl-Z
(global-unset-key (kbd "C-z"))

;; Auto-indent
;; (define-key global-map (kbd "RET") 'newline-and-indent)
;; (setq-default indent-tabs-mode nil)
;; (setq-default tab-width 4)

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
