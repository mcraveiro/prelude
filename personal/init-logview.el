;;; init-logview.el --- Emacs Prelude: Personal Org-mode configuration
;;
;; Copyright © 2016 Marco Craveiro
;;
;; Author: Marco Craveiro <marco_craveiro@gmail.com>
;; URL: https://github.com/mcraveiro/prelude
;; Version: 0.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Personal LogView configuration

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
(prelude-require-package 'logview)

;; (setq 'logview-additional-timestamp-formats
;;       '("boost log"
;;                (regexp . "[0-9]\\{4\\}-[01][0-9]-[0-3][0-9] [012][0-9]:[0-5][0-9]:[0-5][0-9][.,][0-9]\\{3\\}")
;;                (aliases "yyyy-MM-dd HH:mm:ss.UUUUUU")))

;; (setq logview-additional-submodes
;;       '(("dogen"
;;          (format . "TIMESTAMP [LEVEL] [NAME]")
;;          (levels . "SLF4J")
;;          (timestamp "boost log"))))

(add-to-list 'auto-mode-alist '("\\.log\\'" . logview-mode))
