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

;; set the current frame background and font.
(set-background-color "black")
(set-frame-font "Inconsolata Bold 16" nil t)

;; set the font and background for all other frames.
(add-to-list 'default-frame-alist
             '(background-color . "black")
             '(font .  "Inconsolata Bold 16"))
