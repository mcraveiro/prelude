;;; init-highlight-symbol.el --- Emacs Prelude: Personal highlight-symbol configuration
;;
;; Copyright © 2015 Marco Craveiro
;;
;; Author: Marco Craveiro <marco_craveiro@gmail.com>
;; URL: https://github.com/mcraveiro/prelude
;; Version: 0.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Personal highlight-symbol configuration

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

;; Highlight symbol at point.
(prelude-require-package 'highlight-symbol)

;; Key bindings
(global-set-key (kbd "<f12>") 'highlight-symbol-at-point)
(global-set-key (kbd "C-<f12>") 'highlight-symbol-next)
(global-set-key (kbd "M-<f12>") 'highlight-symbol-prev)

;;; init-highlight-symbol.el ends here...
