;;; c++-include-files.el --- Find include path for ac-clang

;;; Commentary:

;; Taken from https://github.com/xiaohanyu/oh-my-emacs/blob/master/modules/ome-cc.org#auto-complete-clang

;;; Code:

(setq search-command "echo | g++ -v -x c++ -E - 2>&1 | grep -A20 starts | grep include | grep -v search")
(setq c++-include-paths
      (mapcar
       (lambda (item) (concat "-I" item))
       (split-string (shell-command-to-string search-command))))

(provide 'c++-include-files)
;;; c++-include-files.el ends here
