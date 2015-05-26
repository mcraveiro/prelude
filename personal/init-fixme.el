;;; init-fixme.el --- Emacs Prelude: Personal FIXME mode
;;
;; Copyright © 2015 Marco Craveiro
;;
;; Author: Marco Craveiro <marco_craveiro@gmail.com>
;; URL: https://github.com/mcraveiro/prelude
;; Version: 0.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Personal FIXME mode

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
(modify-face 'font-lock-fixme-face "red" "yellow" nil t nil t nil nil)

;;; init-fixme.el ends here
