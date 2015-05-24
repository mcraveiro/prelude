;; Copyright (C) 2015  Marco Craveiro
;;
;; This file is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; Cunene is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with init.el.  If not, see <http://www.gnu.org/licenses/>.

;; Top-level personal directory
(setq personal-dir (file-name-directory (or (buffer-file-name) load-file-name)))

;; List of customisation files
(setq personal-files
      '(
        "prelude-personal-fonts.el"
        "prelude-personal-ibuffer.el"
        )
      )

;; Load the customisation files
(while personal-files
  (load (concat personal-dir (car personal-files)))
  (setq personal-files (cdr personal-files)))
