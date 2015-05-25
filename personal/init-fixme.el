;; Make FIXME stand out
(setq fixme-modes
      '(latex-mode makefile-mode c++-mode emacs-lisp-mode sh-mode text-mode
                   org-mode cmake-mode))
(make-face 'font-lock-fixme-face)
(mapc (lambda (mode)
        (font-lock-add-keywords
         mode
         '(("\\<\\(FIXME\\|TODO\\)" 1 'font-lock-fixme-face t))))
      fixme-modes)
(modify-face 'font-lock-fixme-face "Red" "Yellow" nil t nil t nil nil)
