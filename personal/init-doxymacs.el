(setq toplevel-dir (file-name-directory (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path (concat toplevel-dir "/vendor/doxymacs"))

(require 'doxymacs)

;; syntax highlighting for doxygen keywords.
(defun my-doxymacs-font-lock-hook ()
  (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
      (doxymacs-font-lock)))
(add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)

;; start doxymacs mode in C/C++
(add-hook 'c-mode-common-hook'doxymacs-mode)
