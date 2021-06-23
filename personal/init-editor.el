;;; init-editor.el --- Emacs Prelude: General editor configuration
;;
;; Copyright © 2015 Marco Craveiro
;;
;; Author: Marco Craveiro <marco_craveiro@gmail.com>
;; URL: https://github.com/mcraveiro/prelude
;; Version: 0.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; General editor configuration

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

;; set the current frame background and font.
;; (set-background-color "black")
;; (set-frame-font "Inconsolata Bold 8")
;; 'Droid Sans Mono', 'monospace', monospace, 'Droid Sans Fallback'
                                        ; (set-frame-font "DejaVu Sans Mono")
                                        ; (set-frame-font "Source Code Pro 8")

                                        ; set the font and background for all other frames.
;; (add-to-list 'default-frame-alist
;;              '(background-color . "black")
;;              '(font .  "Inconsolata Bold 10"))


;; (set-frame-font "Inconsolata Bold 10")
;; (set-frame-font "Cascadia Code 10")
;; for demo
;; (set-frame-font "Cascadia Code 10")
;; (set-frame-font "Hack 8")

;; (set-frame-font "Droid Sans Mono 8")

(setq redis-cli-executable "/home/marco/local/bin/redis-cli-raw.sh")

;; (set-frame-font "Fira Code 10")
;; (set-frame-font "DejaVu Sans Mono")
;; (set-frame-font "Cascadia Code 9")

(setq default-frame-alist
      '(
        (background-color . "black")
        (font . "Hack 8")
        ))

(fset 'test
      (lambda (&optional arg) "Keyboard macro."
        (interactive "p")
        (kmacro-exec-ring-item
         (quote ([down down end] 0 "%d")) arg)))


(setq prelude-guru nil)

(require 'prelude-erc)
(require 'prelude-ido) ;; Super charges Emacs completion for C-x C-f and more
(require 'prelude-ivy) ;; A mighty modern alternative to ido
(require 'prelude-helm) ;; Interface for narrowing and search
(require 'prelude-helm-everywhere) ;; Enable Helm everywhere
(require 'prelude-company)
(require 'prelude-c)
(require 'prelude-emacs-lisp)
(require 'prelude-lisp)
(require 'prelude-lsp)
(require 'prelude-org) ;; Org-mode helps you keep TODO lists, notes and more
(require 'prelude-shell)
(require 'prelude-xml)

(require 'highlight-indent-guides)
(add-hook 'c-mode-common-hook 'highlight-indent-guides-mode)
(add-hook 'cmake-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-method 'character)

(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)
(prelude-require-package 'org-brain)

(setq org-brain-path "~/Development/phd/doc/")
(setq org-id-track-globally t)
(setq org-id-locations-file "~/.emacs.d/.org-id-locations")
(setq org-brain-visualize-default-choices 'all)
(setq org-brain-title-max-length 12)

;; (prelude-require-package 'openwith)
;; (openwith-mode t)
;; (setq openwith-associations '(("\\.pdf\\'" "evince" (file))))
(setq-default gdb-display-io-nopopup t)

; (global-linum-mode nil)
; (setq linum-format "%4d\u2502 ")

(prelude-require-package 'company-box)
(prelude-require-package 'company-shell)
(add-hook 'company-mode-hook 'company-box-mode)
(add-to-list 'company-backends 'company-shell)

(defun company-eshell-history (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-eshell-history))
    (prefix (and (eq major-mode 'eshell-mode)
                 (let ((word (company-grab-word)))
                   (save-excursion
                     (eshell-bol)
                     (and (looking-at-p (s-concat word "$")) word)))))
    (candidates (remove-duplicates
                 (->> (ring-elements eshell-history-ring)
                      (remove-if-not (lambda (item) (s-prefix-p arg item)))
                      (mapcar 's-trim))
                 :test 'string=))
    (sorted t)))

(add-to-list 'company-backends 'company-eshell-history)

(setq company-dabbrev-other-buffers nil)
(setq company-dabbrev-ignore-case nil)
(setq company-dabbrev-downcase nil)
(setq company-selection-wrap-around t)
(setq company-idle-delay 2)

(prelude-require-package 'company-restclient)
(prelude-require-package 'highlight-indent-guides)
(prelude-require-package 'cycbuf)
(prelude-require-package 'lsp-java)
(prelude-require-package 'ssh)

(prelude-require-package 'plantuml-mode)
(setq plantuml-default-exec-mode 'jar)
(setq plantuml-indent-level 4)
(setq plantuml-output-type "png")
(setq image-auto-resize nil)
(prelude-require-package 'plantuml-mode)
;; (setq plantuml-java-args
;;       '("-DPLANTUML_LIMIT_SIZE=65535" "-Djava.awt.headless=true" "-jar" "--illegal-access=deny"))
(with-eval-after-load "plantuml-mode"
  (add-to-list 'plantuml-java-args "-DPLANTUML_LIMIT_SIZE=65535"))

(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))

(with-eval-after-load "org"
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml)))

(setq plantuml-jar-path "/usr/share/plantuml/plantuml.jar")
(setq org-plantuml-jar-path "/usr/share/plantuml/plantuml.jar")

(prelude-require-package 'flycheck-plantuml)
(with-eval-after-load 'flycheck
  (require 'flycheck-plantuml)
  (flycheck-plantuml-setup))

(prelude-require-package 'eyebrowse)
(prelude-require-package 'git-gutter)
(global-git-gutter-mode)
(prelude-require-package 'yasnippet)
(prelude-require-package 'helm)
(global-set-key (kbd "M-x") 'helm-M-x)
(prelude-require-package 'helm-flyspell)
(prelude-require-package 'flyspell-popup)
(global-set-key  (kbd "M-$") 'flyspell-popup-correct)

(prelude-require-package 'helm-systemd)
(prelude-require-package 'helm-c-yasnippet)
(setq helm-yas-space-match-any-greedy t)
(prelude-require-package 'helm-aws)
(prelude-require-package 'helm-company)
(prelude-require-package 'helm-google)
(prelude-require-package 'helm-grepint)
(prelude-require-package 'helm-projectile)
(prelude-require-package 'ibuffer-git)
(prelude-require-package 'json-mode)
(prelude-require-package 'spaceline)
(prelude-require-package 'all-the-icons)
(prelude-require-package 'all-the-icons-ivy)
(prelude-require-package 'all-the-icons-dired)
(prelude-require-package 'spaceline-all-the-icons)
(prelude-require-package 'protobuf-mode)
(prelude-require-package 'jump-tree)
(prelude-require-package 'docker)
(prelude-require-package 'docker-tramp)
(prelude-require-package 'treemacs)
(prelude-require-package 'lsp-treemacs)
(prelude-require-package 'treemacs-magit)
(prelude-require-package 'treemacs-icons-dired)
(prelude-require-package 'treemacs-projectile)
(prelude-require-package 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

(setq helm-autoresize-mode t)
(define-key helm-find-files-map (kbd "<tab>")
  'helm-execute-persistent-action)
(define-key helm-find-files-map (kbd "C-<backspace>")
  'helm-find-files-up-one-level)

(setq treemacs-width 50)
(treemacs-icons-dired-mode)
(treemacs-git-mode 'extended)
(treemacs-tag-follow-mode 1)
(setq treemacs-position 'left)

(setq doom-variable-pitch-font (font-spec :family "Cascadia Code" :size 9))
(global-jump-tree-mode)

(prelude-require-package 'rainbow-mode)
(add-hook 'python-mode-hook #'rainbow-mode)

(require 'w3m-load)
(require 'w3m)

(defun choose-browser (url &rest args)
  (interactive "sURL: ")
  (if (y-or-n-p "Use external browser? ")
      (browse-url-generic url)
    (w3m-browse-url url)))

(setq browse-url-browser-function 'choose-browser)
;; (global-set-key "\C-xm" 'browse-url-at-point)
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

(global-unset-key (kbd "C-c y"))
(global-set-key (kbd "C-c y") 'helm-yas-complete)
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(yas-global-mode 1)
(yas-load-directory "~/.emacs.d/personal/data/yasnippets")

(prelude-require-package 'wgrep)
(prelude-require-package 'function-args)
(prelude-require-package 'pretty-mode)
(prelude-require-package 'helm)
(prelude-require-package 'helm-flycheck)
(prelude-require-package 'helm-ispell)
(prelude-require-package 'helm-proc)
(prelude-require-package 'pretty-mode)
(global-pretty-mode 1)

(require 'cycbuf)
;;  (global-set-key [(meta right)]       'cycbuf-switch-to-next-buffer)
;; (global-set-key [(meta left)]        'cycbuf-switch-to-previous-buffer)
(global-set-key [(f6)] 'cycbuf-switch-to-next-buffer-no-timeout)
(global-set-key [(f7)]  'cycbuf-switch-to-previous-buffer-no-timeout)

(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(add-hook 'c-mode-common-hook
          (lambda()
            (local-set-key (kbd "C-c o")
                           'helm-projectile-find-other-file)
            (add-to-list 'compilation-error-regexp-alist
                         '("^\\(..*\\)(\\([0-9]+\\): Throw" 1 2)))
          (add-to-list 'compilation-error-regexp-alist
                       '("^\\(..*\\)(\\([0-9]+\\): last checkpoint" 1 2)))
; (global-set-key (kbd "M-o") 'helm-projectile-find-other-file)

(setq erc-join-buffer 'bury)
(add-hook 'erc-mode-hook (lambda () (erc-fill-mode nil)))
; (smartparens-global-mode t)

;; (setq powerline-height 18)
;; (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
;; (setq-default powerline-default-separator 'slant)
;; (setq spaceline-separator-dir-left '(right . right))
;; (setq spaceline-separator-dir-right '(right . right))

;; (require 'spaceline-config)
;; (spaceline-toggle-buffer-size-off)
;; (spaceline-spacemacs-theme)
;; (setq spaceline-buffer-encoding-abbrev-p nil
;;       spaceline-window-numbers-unicode t
;;       spaceline-line-column-p nil
;;       spaceline-buffer-id-p nil
;;       spaceline-minor-modes-separator nil)
(spaceline-all-the-icons-theme)
(spaceline-all-the-icons--setup-package-updates)
(spaceline-all-the-icons--setup-git-ahead)
(powerline-reset)

(setq spaceline-all-the-icons-separator-type 'none)
;; (spaceline-all-the-icons--setup-neotree)

;(prelude-require-package 'doom-modeline)
;(doom-modeline-mode 1)

;; (use-package spaceline-all-the-icons
;;              :straight t
;;              :after spaceline
;;              :config (spaceline-all-the-icons-theme)
;;              (spaceline-all-the-icons--setup-neotree))

;; disable scroll bar
(scroll-bar-mode -1)

;; disable tool tips
(when window-system
(tooltip-mode -1))

;; time and date
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(display-time)

;; coding system
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Make all "yes or no" prompts show "y or n" instead
(fset 'yes-or-no-p 'y-or-n-p)

;; confirm exit
(global-set-key
 (kbd "C-x C-c")
 '(lambda ()
    (interactive)
    (if (y-or-n-p-with-timeout "Do you really want to exit Emacs ?" 4 nil)
        (save-buffers-kill-emacs))))

(prelude-require-package 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; disable stack traces on errors (the annoying backtrace buffer)
(setq stack-trace-on-error nil)

;; repeat pop mark command without the need for C-u
(setq set-mark-command-repeat-pop t)

;; Do not use clever window splitting algorithms.
;; (setq split-width-threshold nil)

;;
;; Tabs
;;
(defun build-tab-stop-list (width)
  (let ((num-tab-stops (/ 80 width))
        (counter 1)
        (ls nil))
    (while (<= counter num-tab-stops)
      (setq ls (cons (* width counter) ls))
      (setq counter (1+ counter)))
    (nreverse ls)))

;; Spaces only for indentation
(set-default 'indent-tabs-mode nil)

;; Tab size
(setq tab-width 4)
(setq-default tab-width 4)
(setq standard-indent 4)
(setq-default tab-stop-list (build-tab-stop-list tab-width))
(setq tab-stop-list (build-tab-stop-list tab-width))

;;
;; Remove tabs from buffer
;;
(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

;;
;; Indent entire buffer
;;
(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

;; Start in full screen.
(prelude-fullscreen)

(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

(defun sm-try-smerge ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^<<<<<<< " nil t)
      (smerge-mode 1))))

(add-hook 'find-file-hook 'sm-try-smerge t)

;;
;; improvements to mark commands
;;
(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))
(global-set-key (kbd "C-`") 'push-mark-no-activate)

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))
(global-set-key (kbd "M-`") 'jump-to-mark)

(defun exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))
(define-key global-map [remap exchange-point-and-mark]
  'exchange-point-and-mark-no-activate)

(defun space2underscore-region (start end)
  "Replace space by underscore in region."
  (interactive "r")
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    (while (search-forward " " nil t) (replace-match "_"))))

(defun underscore2space-region (start end)
  "Replace underscore by space in region."
  (interactive "r")
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    (while (search-forward "_" nil t) (replace-match " "))))

(defun replace-underscore-space-toggle ()
  "Replace underscore/space in the current region or line.
If the current line contains more “_” char than space,
then replace them to space, else replace space to _.
If there's a text selection, work on the selected text."
  (interactive)
  (let (li bds)
    (setq bds
          (if (region-active-p)
              (cons (region-beginning) (region-end))
            (bounds-of-thing-at-point 'line)))
    (setq li (buffer-substring-no-properties (car bds) (cdr bds)))
    (if (> (count 32 li) (count 95 li))
        (progn (replace-string " " "_" nil (car bds) (cdr bds)))
      (progn (replace-string "_" " " nil (car bds) (cdr bds))))))

(defun cycle-hyphen-underscore-space ()
  "Cyclically replace {underscore, space, hypen} chars current
 line or text selection.  When called repeatedly, this command
 cycles the {“ ”, “_”, “-”} characters."
  (interactive)
  ;; this function sets a property 「'state」. Possible values are 0
  ;; to length of charList.
  (let (mainText charList p1 p2 currentState nextState changeFrom
             changeTo startedWithRegion-p )

    (if (region-active-p)
        (progn
          (setq startedWithRegion-p t )
          (setq p1 (region-beginning))
          (setq p2 (region-end))
          )
      (progn (setq startedWithRegion-p nil )
             (setq p1 (line-beginning-position))
             (setq p2 (line-end-position)) ) )

    (setq charList (list " " "_" "-" ))

    (setq currentState
          (if (get 'cycle-hyphen-underscore-space 'state)
              (get 'cycle-hyphen-underscore-space 'state) 0))
    (setq nextState (% (+ currentState (length charList) 1) (length charList)))

    (setq changeFrom (nth currentState charList))
    (setq changeTo (nth nextState charList))

    (setq mainText
          (replace-regexp-in-string changeFrom changeTo
                                    (buffer-substring-no-properties p1 p2)))
    (delete-region p1 p2)
    (insert mainText)

    (put 'cycle-hyphen-underscore-space 'state nextState)

    (when startedWithRegion-p
      (goto-char p2)
      (set-mark p1)
      (setq deactivate-mark nil))))

(global-set-key (kbd "C-c C--") 'cycle-hyphen-underscore-space)

(defun diff-buffer-with-associated-file ()
  "View the differences between BUFFER and its associated file.
This requires the external program \"diff\" to be in your `exec-path'.
Returns nil if no differences found, 't otherwise."
  (interactive)
  (let ((buf-filename buffer-file-name)
        (buffer (current-buffer)))
    (unless buf-filename
      (error "Buffer %s has no associated file" buffer))
    (let ((diff-buf (get-buffer-create
                     (concat "*Assoc file diff: "
                             (buffer-name)
                             "*"))))
      (with-current-buffer diff-buf
        (setq buffer-read-only nil)
        (erase-buffer))
      (let ((tempfile (make-temp-file "buffer-to-file-diff-")))
        (unwind-protect
            (progn
              (with-current-buffer buffer
                (write-region (point-min) (point-max) tempfile nil 'nomessage))
              (if (zerop
                   (apply #'call-process "diff" nil diff-buf nil
                          (append
                           (when (and (boundp 'ediff-custom-diff-options)
                                      (stringp ediff-custom-diff-options))
                             (list ediff-custom-diff-options))
                           (list buf-filename tempfile))))
                  (progn
                    (message "No differences found")
                    nil)
                (progn
                  (with-current-buffer diff-buf
                    (goto-char (point-min))
                    (if (fboundp 'diff-mode)
                        (diff-mode)
                      (fundamental-mode)))
                  (display-buffer diff-buf)
                  t)))
          (when (file-exists-p tempfile)
            (delete-file tempfile)))))))

;; tidy up diffs when closing the file
(defun kill-associated-diff-buf ()
  (let ((buf (get-buffer (concat "*Assoc file diff: "
                             (buffer-name)
                             "*"))))
    (when (bufferp buf)
      (kill-buffer buf))))

(add-hook 'kill-buffer-hook 'kill-associated-diff-buf)

(global-set-key (kbd "C-c C-=") 'diff-buffer-with-associated-file)

(defun de-context-kill (arg)
  "Kill buffer"
  (interactive "p")
  (if (and (buffer-modified-p)
             buffer-file-name
             (not (string-match "\\*.*\\*" (buffer-name)))
             ;; erc buffers will be automatically saved
             (not (eq major-mode 'erc-mode))
             (= 1 arg))
    (let ((differences 't))
      (when (file-exists-p buffer-file-name)
        (setq differences (diff-buffer-with-associated-file)))

      (if (y-or-n-p (format "Buffer %s modified; Kill anyway? " buffer-file-name))
          (progn
            (set-buffer-modified-p nil)
            (kill-buffer (current-buffer)))))
    (if (and (boundp 'gnuserv-minor-mode)
           gnuserv-minor-mode)
        (gnuserv-edit)
      (set-buffer-modified-p nil)
      (kill-buffer (current-buffer)))))

(global-set-key (kbd "C-x k") 'de-context-kill)

;; ediff

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-diff-options "-w")
;; (setq ediff-split-window-function
;;      (if (> (frame-width) 150)
;;          'split-window-horizontally
;;        'split-window-vertically))

;; (setq diff-switches "-u")
;; (setq ediff-custom-diff-options "-U3")
;; (setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; (add-hook 'ediff-startup-hook 'ediff-toggle-wide-display)
;; (add-hook 'ediff-cleanup-hook 'ediff-toggle-wide-display)
;; (add-hook 'ediff-suspend-hook 'ediff-toggle-wide-display)

(defvar ediff-do-hexl-diff nil
  "variable used to store trigger for doing diff in hexl-mode")

(defadvice ediff-files-internal
  (around ediff-files-internal-for-binary-files activate)
  "catch the condition when the binary files differ the reason
for catching the error out here (when re-thrown from the inner
advice) is to let the stack continue to unwind before we start
the new diff otherwise some code in the middle of the stack
expects some output that isn't there and triggers an error"
  (let ((file-A (ad-get-arg 0))
        (file-B (ad-get-arg 1))
        ediff-do-hexl-diff)
    (condition-case err
        (progn
          ad-do-it)
      (error
       (if ediff-do-hexl-diff
           (let ((buf-A (find-file-noselect file-A))
                 (buf-B (find-file-noselect file-B)))
             (with-current-buffer buf-A
               (hexl-mode 1))
             (with-current-buffer buf-B
               (hexl-mode 1))
             (ediff-buffers buf-A buf-B))
         (error (error-message-string err)))))))

(defadvice ediff-setup-diff-regions
  (around ediff-setup-diff-regions-for-binary-files activate)
  "when binary files differ, set the variable "
  (condition-case err
      (progn
        ad-do-it)
    (error
     (setq ediff-do-hexl-diff
           (and (string-match-p "^Errors in diff output.  Diff output is in.*"
                                (error-message-string err))
                (string-match-p "^\\(Binary \\)?[fF]iles .* and .* differ"
                                (buffer-substring-no-properties
                                 (line-beginning-position)
                                 (line-end-position)))))
     (error (error-message-string err)))))

;; FIXME: for SSH
(defun fmq-compilation-finish (buffer status))

;; (defun fmq-compilation-finish (buffer status)
;;   (call-process "notify-send" nil nil nil
;;                 "-t" "2"
;;                 "-i" "emacs"
;;                 "Compilation finished in Emacs"
;;                 status))

;; (setq compilation-finish-functions
;;      (append compilation-finish-functions
;;              '(fmq-compilation-finish)))

;; (setq zenburn-override-colors-alist
;;       '(("zenburn-bg+05" . "#282828")
;;         ("zenburn-bg"    . "#000000")
;;         ("zenburn-bg+1"  . "#2F2F2F")
;;         ("zenburn-bg+2"  . "#3F3F3F")
;;         ("zenburn-bg+3"  . "#4F4F4F")))

;; use variable-pitch fonts for some headings and titles
; (setq zenburn-use-variable-pitch t)

;; scale headings in org-mode
;;(setq zenburn-scale-org-headlines t)
(org-babel-do-load-languages
 'org-babel-load-languages '((C . t)))

(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
(org-babel-do-load-languages
 'org-babel-load-languages '((plantuml . t)))


;; scale headings in outline-mode
;(setq zenburn-scale-outline-headlines t)
;(load-theme 'zenburn t)


(prelude-require-package 'eyebrowse)
(eyebrowse-mode t)

;; (eval-after-load
;;     'company
;;   '(add-to-list 'company-backends #'company-omnisharp))

(defun my-csharp-mode-setup ()
  (lsp-mode)
  (company-mode)
  (flycheck-mode)

  (setq indent-tabs-mode nil)
  (setq c-syntactic-indentation t)
  (c-set-style "ellemtel")
  (setq c-basic-offset 4)
  (setq truncate-lines t)
  (setq tab-width 4)
  (setq evil-shift-width 4)

                                        ;csharp-mode README.md recommends this too
                                        ;(electric-pair-mode 1)       ;; Emacs 24
                                        ;(electric-pair-local-mode 1) ;; Emacs 25

  ;; (local-set-key (kbd "C-c r r") 'omnisharp-run-code-action-refactoring)
  (local-set-key (kbd "C-c C-c") 'recompile))

(add-hook 'csharp-mode-hook 'my-csharp-mode-setup t)
;; (custom-set-variables '(gnus-select-method (quote (nnreddit ""))))

;; (setq gnus-thread-sort-functions
;;       '(gnus-thread-sort-by-number
;;         (not gnus-thread-sort-by-date)))
;; (setq gnus-thread-sort-functions '(not gnus-thread-sort-by-date))

(defun radian--undo-tree-suppress-undo-history-saved-message
    (undo-tree-save-history &rest args)
  (let ((inhibit-message t))
    (apply undo-tree-save-history args)))

;; Suppress the message saying that the undo history could not be
;; loaded because the file changed outside of Emacs.

(defun radian--undo-tree-suppress-buffer-modified-message
    (undo-tree-load-history &rest args)
  (let ((inhibit-message t))
    (apply undo-tree-load-history args)))

(advice-add #'undo-tree-load-history :around
            #'radian--undo-tree-suppress-buffer-modified-message)

(prelude-require-package 'elfeed)
(prelude-require-package 'elfeed-goodies)
(elfeed-goodies/setup)
(setq elfeed-goodies/entry-pane-position 'bottom)
;; (setq elfeed-goodies/entry-pane-size 0.5)
(setq elfeed-feeds
      '("https://my.cdash.org/rss/SubmissionRSSMASD%20Project%20-%20C++%20Reference%20Implementation.xml"
        "https://github.com/MASD-Project/csharp_ref_impl/tags/master.atom"
        "https://github.com/MASD-Project/csharp_ref_impl/releases/master.atom"
        "https://github.com/MASD-Project/csharp_ref_impl/commits/master.atom"
        "https://github.com/MASD-Project/dogen/tags/master.atom"
        "https://github.com/MASD-Project/dogen/releases/master.atom"
        "https://github.com/MASD-Project/dogen/commits/master.atom"
        "https://github.com/MASD-Project/cpp_ref_impl/tags/master.atom"
        "https://github.com/MASD-Project/cpp_ref_impl/releases/master.atom"
        "https://github.com/MASD-Project/cpp_ref_impl/commits/master.atom"
        "https://my.cdash.org/rss/SubmissionRSSMASD+Project+-+C%2B%2B+Reference+Implementation.xml"
        "https://my.cdash.org/viewFeed.php?projectid=1245"
        "https://my.cdash.org/rss/SubmissionRSSMASD%20Project%20-%20Dogen.xml"))

(setq auth-sources '("~/.authinfo.gpg"))

(defun my-fetch-password (&rest params)
  (require 'auth-source)
  (let ((match (car (apply 'auth-source-search params))))
    (if match
        (let ((secret (plist-get match :secret)))
          (if (functionp secret)
              (funcall secret)
            secret))
      (error "Password not found for %S" params))))

(defun my-nickserv-password (server)
  (my-fetch-password :login "mcraveiro" :machine "irc.gitter.im"))

(setq irc-debug-log t)

(setq circe-network-options
      `(("Gitter"
         :tls t
         :nick "mcraveiro"
         :pass 0cd77e23b4acfc2c6522af6db4713ad2a23b3da3
         :server-buffer-name "⇄ gitter"
         :host "irc.gitter.im"
         :service "6697")))

;; (setq elfeed-show-entry-switch #'popwin:elfeed-show-entry
;;       elfeed-show-entry-delete #'popwin:elfeed-kill-buffer
;;       elfeed-search-header-function #'feed-reader/search-header)

;; (prelude-require-package 'centaur-tabs)
;; (centaur-tabs-group-by-projectile-project)
;; (centaur-tabs-change-fonts "Cascadia Code" 120)
;; (setq centaur-tabs-style "bar"
;;       centaur-tabs-height 32
;;       centaur-tabs-set-icons t
;;       centaur-tabs-set-modified-marker t
;;       centaur-tabs-show-navigation-buttons t
;;       centaur-tabs-set-bar 'under
;;       x-underline-at-descent-line t)
;; (centaur-tabs-headline-match)
;; ;; (setq centaur-tabs-gray-out-icons 'buffer)
;; ;; (centaur-tabs-enable-buffer-reordering)
;; ;; (setq centaur-tabs-adjust-buffer-order t)
;; (centaur-tabs-mode t)
;; (setq centaur-tabs-set-bar 'under)
;; (setq x-underline-at-descent-line t)
;; (setq centaur-tabs-gray-out-icons 'buffer)
;; (setq centaur-tabs-height 40)
;; (setq centaur-tabs-style "wave")
;; (setq uniquify-separator "/")
;; (setq uniquify-buffer-name-style 'forward)

;; (defun centaur-tabs-buffer-groups ()
;;   "`centaur-tabs-buffer-groups' control buffers' group rules.
;;  Group centaur-tabs with mode if buffer is derived from
;;  `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode'
;;  `magit-mode'.  All buffer name start with * will group to
;;  \"Emacs\".  Other buffer group by `centaur-tabs-get-group-name'
;;  with project name."
;;   (list
;;    (cond
;;     ;; ((not (eq (file-remote-p (buffer-file-name)) nil))
;;     ;; "Remote")
;;     ((or (string-equal "*" (substring (buffer-name) 0 1))
;;         (memq major-mode '(magit-process-mode
;;                            magit-status-mode
;;                            magit-diff-mode
;;                            magit-log-mode
;;                            magit-file-mode
;;                            magit-blob-mode
;;                            magit-blame-mode
;;                            )))
;;      "Emacs")
;;     ((derived-mode-p 'prog-mode)
;;      "Editing")
;;     ((derived-mode-p 'dired-mode)
;;      "Dired")
;;     ((memq major-mode '(helpful-mode
;;                 help-mode))
;;      "Help")
;;     ((memq major-mode '(org-mode
;;                         org-agenda-clockreport-mode
;;                         org-src-mode
;;                         org-agenda-mode
;;                         org-beamer-mode
;;                         org-indent-mode
;;                         org-bullets-mode
;;                         org-cdlatex-mode
;;                         org-agenda-log-mode
;;                         diary-mode))
;;      "OrgMode")
;;     (t
;;      (centaur-tabs-get-group-name (current-buffer))))))

;; (add-hook 'dired-mode-hook 'centaur-tabs-local-mode)
;; :hook
;; (dashboard-mode . centaur-tabs-local-mode)
;; (term-mode . centaur-tabs-local-mode)
;; (calendar-mode . centaur-tabs-local-mode)
;; (org-agenda-mode . centaur-tabs-local-mode)
;; (helpful-mode . centaur-tabs-local-mode)
;; :bind
;;    ("C-<prior>" . centaur-tabs-backward)
;;    ("C-<next>" . centaur-tabs-forward)
;;    ("C-c t s" . centaur-tabs-counsel-switch-group)
;;    ("C-c t p" . centaur-tabs-group-by-projectile-project)
;;    ("C-c t g" . centaur-tabs-group-buffer-groups)
;;    (:map evil-normal-state-map
;;       ("g t" . centaur-tabs-forward)
;;       ("g T" . centaur-tabs-backward))

(with-eval-after-load "ispell"
  ;; Configure `LANG`, otherwise ispell.el cannot find a 'default
  ;; dictionary' even though multiple dictionaries will be configured
  ;; in next line.
  ;; (setenv "LANG" "en_GB") ;; en_GB.UTF-8
  (setq ispell-program-name "hunspell")
  ;; Configure German, Swiss German, and two variants of English.
  (setq ispell-dictionary "en_GB,pt_PT")
  ;; ispell-set-spellchecker-params has to be called
  ;; before ispell-hunspell-add-multi-dic will work
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_GB,pt_PT")
  ;; For saving words to the personal dictionary, don't infer it from
  ;; the locale, otherwise it would save to ~/.hunspell_de_DE.
  (setq ispell-personal-dictionary "~/.hunspell_personal"))

;; The personal dictionary file has to exist, otherwise hunspell will
;; silently not use it.
(unless (file-exists-p ispell-personal-dictionary)
  (write-region "" nil ispell-personal-dictionary nil 0)
  )

(setq eshell-prompt-regexp "^[^#$\n]*[#$] "
      eshell-prompt-function
      (lambda nil
        (concat
         "[" (user-login-name) "@" (system-name) " "
         (if (string= (eshell/pwd) (getenv "HOME"))
             "~" (eshell/basename (eshell/pwd)))
         "]"
         (if (= (user-uid) 0) "# " "$ "))))

(defun m-eshell-hook ()

                                        ; define control p, control n and the up/down arrow
  (define-key eshell-mode-map [(control p)] 'eshell-previous-matching-input-from-input)
  (define-key eshell-mode-map [(control n)] 'eshell-next-matching-input-from-input)

  (define-key eshell-mode-map [up] 'previous-line)
  (define-key eshell-mode-map [down] 'next-line)
  )

(add-hook 'eshell-mode-hook 'm-eshell-hook)

(prelude-require-package 'helpful)
(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
;; Lookup the current symbol at point. C-c C-d is a common keybinding
;; for this in lisp modes.
(global-set-key (kbd "C-c C-d") #'helpful-at-point)

;; Look up *F*unctions (excludes macros).
;;
;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
;; already links to the manual, if a function is referenced there.

;; Look up *C*ommands.
;;
;; By default, C-h C is bound to describe `describe-coding-system'. I
;; don't find this very useful, but it's frequently useful to only
;; look at interactive functions.
(global-set-key (kbd "C-h C") #'helpful-command)

(prelude-require-package 'eshell-git-prompt)
(eshell-git-prompt-use-theme 'powerline)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq eshell-prompt-function                                                                    ;;
;;       (lambda ()                                                                                     ;;
;;         (concat                                                                                 ;;
;;          (propertize "┌─[" 'face `(:foreground "green"))                                        ;;
;;          (propertize (user-login-name) 'face `(:foreground "red"))                              ;;
;;          (propertize "@" 'face `(:foreground "green"))                                          ;;
;;          (propertize (system-name) 'face `(:foreground "blue"))                                 ;;
;;          (propertize "]──[" 'face `(:foreground "green"))                                       ;;
;;          (propertize (format-time-string "%H:%M" (current-time)) 'face `(:foreground "yellow")) ;;
;;          (propertize "]──[" 'face `(:foreground "green"))                                       ;;
;;          (propertize (concat (eshell/pwd)) 'face `(:foreground "white"))                        ;;
;;          (propertize "]\n" 'face `(:foreground "green"))                                        ;;
;;          (propertize "└─>" 'face `(:foreground "green"))                                        ;;
;;          (propertize (if (= (user-uid) 0) " # " " $ ") 'face `(:foreground "green"))            ;;
;;          )))                                                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(prelude-require-package 'define-word)
(require 'define-word)
(defun cunene/display-word (&rest args)
  "Create a buffer for display word instead of using messages."
  (interactive)
  (let
      ((buffer (get-buffer-create "Define Word")))
    (set-buffer buffer)
    (erase-buffer)
    (set-buffer-major-mode buffer)
    (apply 'insert args)
    (display-buffer buffer))
  )

(setq define-word-displayfn-alist
      (cl-loop for (service . _) in define-word-services
               collect (cons service #'cunene/display-word)))

(prelude-require-package 'popper)
(setq popper-reference-buffers
      '("\\*Messages\\*" "Output\\*$" "Define Word" help-mode helpful-mode
        "^\\*RE-Builder\\*$" "^\\*Kill Ring\\*$" "^\\*Calendar\\*$"
        "^\\*WoMan-Log\\*$" "^\\*Apropos\\*$" "^\\*Completions\\*$"))
(popper-mode +1)

(prelude-require-package 'visual-regexp)
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)
;; if you use multiple-cursors, this is for you:
(define-key global-map (kbd "C-c m") 'vr/mc-mark)

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
(global-set-key (kbd "C-c C-r") 'recompile)

;;; init-editor.el ends here
