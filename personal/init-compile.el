;;; init-compile.el --- Emacs Prelude: Personal compile configuration
;;
;; Copyright © 2015 Marco Craveiro
;;
;; Author: Marco Craveiro <marco_craveiro@gmail.com>
;; URL: https://github.com/mcraveiro/prelude
;; Version: 0.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Personal compile configuration

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
(prelude-require-package 'compile)

;; Compilation command
(setq compile-command "make ")

;; Make the compilation and grep windows smaller.
(if (not window-system)
    (setq compilation-window-height 8)
  (setq compilation-window-height 14))

;; Scroll the compilation buffer automatically.
(setq compilation-scroll-output t)

;; Key bindings
(global-set-key (kbd "C-c c") 'compile)

;; Only go to error messages
(setq compilation-skip-threshold 2)

;; If a compilation buffer is already open, use it
(setq-default display-buffer-reuse-frames t)

;;
;; support for microsoft and mono compilation errors
;;

;; microsoft visual C/C++ errors
(add-to-list
 'compilation-error-regexp-alist-alist
 '(msvc-error
   "\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) \
: \\(error\\|warning\\) C[0-9]+:" 1 3))

;; mono and microsoft's C# errors
(add-to-list
 'compilation-error-regexp-alist-alist
 '(mcs-error
   "^\\(..*\\)(\\([0-9]+\\),\\([0-9]+\\)): error" 1 2 3 2))

;; mono and microsoft's C# warnings
(add-to-list
 'compilation-error-regexp-alist-alist
 '(mcs-warning
   "^\\(..*\\)(\\([0-9]+\\),\\([0-9]+\\)): warning" 1 2 3 1))

(add-to-list
 'compilation-error-regexp-alist-alist
 '(t4-error
   "^\\(..*\\)(\\([0-9]+\\),\\([0-9]+\\)): ERROR" 1 2 3))

(add-to-list 'compilation-error-regexp-alist 'msvc-error)
(add-to-list 'compilation-error-regexp-alist 'mcs-error)
(add-to-list 'compilation-error-regexp-alist 'mcs-warning)
(add-to-list 'compilation-error-regexp-alist 't4-error)

(when (require 'ansi-color nil t)
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))

;; (require 'ansi-color)
;; (defun colorize-compilation-buffer ()
;;   (toggle-read-only)
;;   (ansi-color-apply-on-region compilation-filter-start (point))
;;   (toggle-read-only))
;; (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;;; init-compile.el ends here
