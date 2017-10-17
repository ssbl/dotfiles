;;; a tidied-up version of my .emacs file

;; add MELPA
(require 'package)
(add-to-list 'package-archives
                          '("melpa" . "https://melpa.milkbox.net/packages/"))
(when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
      (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(setq package-list
      '(auto-complete
        rtags
        company
        company-irony
        company-irony-c-headers
        company-rtags
        irony
        clang-format
        flycheck
        auctex
        go-mode
        haskell-mode
        rust-mode
        cmake-ide))

(package-initialize)

(unless package-archive-contents package-refresh-contents)
(dolist (package package-list)
  (unless (package-installed-p package) (package-install package)))

;; add a directory for loading lisp files
(add-to-list 'load-path "~/.emacs.d/lisp")

;; set jdee server directory
(setq jdee-server-dir "~/.emacs.d/jdee-server-dir")

;; autocomplete
(ac-config-default)
(require 'c++-include-files)
(require 'company)
(require 'company-irony-c-headers)
(setq company-backends (delete 'company-semantic company-backends))

;; cmake-ide
(require 'rtags)
(require 'company-rtags)
(setq rtags-completions-enabled t)
(setq rtags-autostart-diagnostics t)
(cmake-ide-setup)
(setq cmake-ide-flags-c++ (append '("-std=c++14") c++-include-paths))

;; irony-mode
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
(setq company-backends (delete 'company-semantic company-backends))
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; clang-format
(global-set-key (kbd "C-<tab>") 'clang-format-region)
(global-set-key (kbd "C-M-<tab>") 'clang-format-buffer)
;; don't indent namespaces
(defun disable-namespace-indent ()
  (c-set-offset 'innamespace [0]))
(add-hook 'c++-mode-hook 'disable-namespace-indent)

;; jedi setup for Python
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook (lambda ()
                              (setq ac-sources '(ac-source-jedi-direct))))
(setq jedi:complete-on-dot t)

;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-check-syntax-automatically '(mode-enabled save))
(add-hook 'c++-mode-hook (lambda ()
                           (setq flycheck-clang-language-standard "c++14")))

;; ido-mode
(require 'ido)
(ido-mode t)
(global-set-key (kbd "M-l") 'ido-switch-buffer)

;; yes/no to y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;; save buffers open when closing Emacs
(desktop-save-mode 1)

;; copy to clipboard
(setq x-select-enable-clipboard t)

;; remove junk characters in compilation window
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;; ibuffer
(defalias 'list-buffers 'ibuffer)

;; hide toolbar, scroll-bar, menu-bar
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(setq menu-bar-mode nil)

;; no blinking cursor
(setq blink-cursor-mode nil)

;; show columns
(setq column-number-mode t)

;; comments
(global-set-key (kbd "C-;") 'comment-dwim)
(setq comment-style 'multi-line)

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
(set-frame-font "Ubuntu Mono 11" nil t)

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
(defun ssbl-open-header-in-other-window ()
  "Opens the .h or .c file for the current window's file in other window."
  (interactive)
  (ff-find-other-file 't)
  (other-window 1))
(global-set-key (kbd "C-x C-h") 'ssbl-open-header-in-other-window)

;; header completion for C and C++
(add-hook 'c++-mode-hook (lambda ()
                           (auto-complete-mode 0)
                           (company-mode 1)

                           (add-to-list 'company-backends
                                        '(company-irony-c-headers
                                          company-irony
                                          company-rtags
                                          company-clang))))
(add-hook 'c-mode-hook (lambda ()
                         (auto-complete-mode nil)
                         (add-to-list 'company-backends
                                      '(company-irony-c-headers
                                        company-irony))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-view-program-selection
   (quote
    (((output-dvi has-no-display-manager)
      "dvi2tty")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "Okular")
     (output-html "xdg-open"))))
 '(custom-safe-themes
   (quote
    ("0ec1d50ee7c886bd065aacff1a6a5034a32357c89a07561fd14f64dfcbf0cf6d" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
