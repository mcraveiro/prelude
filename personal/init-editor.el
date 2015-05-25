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
(set-background-color "black")
(set-frame-font "Inconsolata Bold 16" nil t)

;; set the font and background for all other frames.
(add-to-list 'default-frame-alist
             '(background-color . "black")
             '(font .  "Inconsolata Bold 16"))

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

;;; init-editor.el ends here
