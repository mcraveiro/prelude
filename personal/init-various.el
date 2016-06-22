(prelude-require-package 'wsd-mode)
(prelude-require-package 'sx)
(prelude-require-package 'jq-mode)
(prelude-require-package 'swiper)
(prelude-require-package 'smart-mode-line)
(prelude-require-package 'powerline)
(prelude-require-package 'smart-mode-line-powerline-theme)
(prelude-require-package 'persistent-scratch)
(prelude-require-package 'csharp-mode)
(prelude-require-package 'psvn)
(persistent-scratch-setup-default)
(powerline-default-theme)
(require 'warnings)
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

;; Give details about white space usage
(autoload 'whitespace-mode "whitespace" "Toggle whitespace visualization." t)
(autoload 'whitespace-toggle-options
  "whitespace" "Toggle local `whitespace-mode' options." t)

;; What to highlight
(setq whitespace-style
      '(face tabs trailing lines-tail space-before-tab empty space-after-tab
             tab-mark))

;; Indicate if empty lines exist at end of the buffer
(set-default 'indicate-empty-lines t)

;; do not use global mode whitespace
(global-whitespace-mode 0)
(setq whitespace-global-modes nil)

;; Show whitespaces on these modes
(add-hook 'sh-mode-hook 'whitespace-mode)
(add-hook 'snippet-mode-hook 'whitespace-mode)
(add-hook 'tex-mode-hook 'whitespace-mode)
(add-hook 'sql-mode-hook 'whitespace-mode)
(add-hook 'ruby-mode-hook 'whitespace-mode)
(add-hook 'diff-mode-hook 'whitespace-mode)
(add-hook 'c-mode-common-hook 'whitespace-mode)
(add-hook 'cmake-mode-hook 'whitespace-mode)
(add-hook 'emacs-lisp-mode-hook 'whitespace-mode)
(add-hook 'dos-mode-hook 'whitespace-mode)
(add-hook 'org-mode-hook 'whitespace-mode)
(add-hook 'js-mode-hook 'whitespace-mode)
(add-hook 'js2-mode-hook 'whitespace-mode)

(setq jq-interactive-default-options "")

(prelude-require-package 'neotree)
(global-set-key [f8] 'neotree-toggle)
(setq neo-window-width 50)

;; Every time when the neotree window is opened, it will try to find current
;; file and jump to node.
(setq-default neo-smart-open t)

;; Do not allow neotree to be the only open window
(setq-default neo-dont-be-alone t)

(setq neo-theme 'classic) ; 'classic, 'nerd, 'ascii, 'arrow
(setq neo-vc-integration '(face char))

;; Patch to fix vc integration
(defun neo-vc-for-node (node)
  (let* ((backend (vc-backend node))
         (vc-state (when backend (vc-state node backend))))
    ;; (message "%s %s %s" node backend vc-state)
    (cons (cdr (assoc vc-state neo-vc-state-char-alist))
          (cl-case vc-state
            (up-to-date       neo-vc-up-to-date-face)
            (edited           neo-vc-edited-face)
            (needs-update     neo-vc-needs-update-face)
            (needs-merge      neo-vc-needs-merge-face)
            (unlocked-changes neo-vc-unlocked-changes-face)
            (added            neo-vc-added-face)
            (removed          neo-vc-removed-face)
            (conflict         neo-vc-conflict-face)
            (missing          neo-vc-missing-face)
            (ignored          neo-vc-ignored-face)
            (unregistered     neo-vc-unregistered-face)
            (user             neo-vc-user-face)
            (t                neo-vc-default-face)))))

;; ido bookmarks
(defun ido-bookmark-jump()
  "Uses ido to search for the bookmark"
  (interactive)
  (bookmark-jump
   (bookmark-get-bookmark
    (ido-completing-read "find bookmark: " (bookmark-all-names)))))

(global-set-key (kbd "C-x r b") 'ido-bookmark-jump)
