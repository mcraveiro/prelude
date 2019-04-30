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
                                        ; (set-frame-font "DejaVu Sans Mono")
                                        ; (set-frame-font "Source Code Pro 8")

                                        ; set the font and background for all other frames.
;; (add-to-list 'default-frame-alist
;;              '(background-color . "black")
;;              '(font .  "Inconsolata Bold 10"))

;; (set-frame-font "Inconsolata Bold 18")
(setq default-frame-alist
      '(
        (background-color . "black")
        (font . "Inconsolata Bold 8")
        ))

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

(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)
(prelude-require-package 'org-brain)

(setq org-brain-path "~/Development/phd/doc/")
(setq org-id-track-globally t)
(setq org-id-locations-file "~/.emacs.d/.org-id-locations")
(setq org-brain-visualize-default-choices 'all)
(setq org-brain-title-max-length 12)

(prelude-require-package 'openwith)
(openwith-mode t)
(setq openwith-associations '(("\\.pdf\\'" "evince" (file))))

(prelude-require-package 'eyebrowse)
(prelude-require-package 'git-gutter)
(global-git-gutter-mode)
(prelude-require-package 'yasnippet)
(prelude-require-package 'helm)
(global-set-key (kbd "M-x") 'helm-M-x)
(prelude-require-package 'helm-flyspell)

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

(setq treemacs-width 70)
(treemacs-icons-dired-mode)
(treemacs-git-mode 'extended)
(treemacs-tag-follow-mode 1)


(global-jump-tree-mode)

(prelude-require-package 'rainbow-mode)
(add-hook 'python-mode-hook #'rainbow-mode)

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
          (lambda() (local-set-key (kbd "C-c o")
                              'helm-projectile-find-other-file)))
; (global-set-key (kbd "M-o") 'helm-projectile-find-other-file)

(setq erc-join-buffer 'bury)
(add-hook 'erc-mode-hook (lambda () (erc-fill-mode nil)))
(smartparens-global-mode t)

;(spaceline-all-the-icons-theme)
;(setq spaceline-all-the-icons-separator-type 'arrow)
;(prelude-require-package 'doom-modeline)
; (doom-modeline-mode 1)

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

(setq compilation-finish-functions
      (append compilation-finish-functions
              '(fmq-compilation-finish)))

;; (setq zenburn-override-colors-alist
;;       '(("zenburn-bg+05" . "#282828")
;;         ("zenburn-bg"    . "#000000")
;;         ("zenburn-bg+1"  . "#2F2F2F")
;;         ("zenburn-bg+2"  . "#3F3F3F")
;;         ("zenburn-bg+3"  . "#4F4F4F")))

;; use variable-pitch fonts for some headings and titles
; (setq zenburn-use-variable-pitch t)

;; scale headings in org-mode
;(setq zenburn-scale-org-headlines t)

;; scale headings in outline-mode
;(setq zenburn-scale-outline-headlines t)
;(load-theme 'zenburn t)


(prelude-require-package 'eyebrowse)
(eyebrowse-mode t)

;;; init-editor.el ends here
