(prelude-require-package 'wsd-mode)
(prelude-require-package 'sx)
(push '(undo discard-info) warning-suppress-types)

; (setq sml/theme 'smart-mode-line-powerline)
; (sml/setup)


(defun counsel-yank-bash-history ()
  "Yank the bash history"
  (interactive)
  (let (hist-cmd collection val)
    (shell-command "history -r") ; reload history
    (setq collection
          (nreverse
           (split-string (with-temp-buffer (insert-file-contents (file-truename "~/.bash_history"))
                                           (buffer-string))
                         "\n"
                         t)))
    (when (and collection (> (length collection) 0)
               (setq val (if (= 1 (length collection)) (car collection)
                           (ivy-read (format "Bash history:") collection))))
      (kill-new val)
      (message "%s => kill-ring" val))))

(prelude-require-package 'workgroups2)
(workgroups-mode 1)

(prelude-require-package 'git-timemachine)
(prelude-require-package 'gh)

(global-set-key (kbd "C-s") 'swiper)
(setq ivy-display-style 'fancy)

(global-set-key (kbd "M-t") 'avy-goto-word-1)

(setq magit-completing-read-function 'ivy-completing-read)
(setq projectile-completion-system 'ivy)


(define-key smartparens-mode-map (kbd "M-<up>") nil)
(define-key smartparens-mode-map (kbd "M-<down>") nil)
(define-key smartparens-mode-map (kbd "C-<right>") nil)
(define-key smartparens-mode-map (kbd "C-<left>") nil)
(define-key smartparens-mode-map (kbd "C-M-<right>") nil)
(define-key smartparens-mode-map (kbd "C-M-<left>") nil)

(prelude-require-package 'windmove)
(windmove-default-keybindings 'meta)
(setq framemove-hook-into-windmove t)
(prelude-require-package 'framemove)
; (framemove-default-keybindings)

(add-to-list 'display-buffer-alist
             `(,(rx bos "*Flycheck errors*" eos)
               (display-buffer-reuse-window
                display-buffer-in-side-window)
               (reusable-frames . visible)
               (side            . bottom)
               (window-height   . 0.4)))

(prelude-require-package 'pretty-symbols)
