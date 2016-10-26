;;; init-cmake-ide.el --- Emacs Prelude: Personal bookmarks configuration
;;
;; Copyright © 2015 Marco Craveiro
;;
;; Author: Marco Craveiro <marco_craveiro@gmail.com>
;; URL: https://github.com/mcraveiro/prelude
;; Version: 0.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Personal bookmarks configuration

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

(global-unset-key "\C-cr")
(add-to-list 'exec-path (expand-file-name "~/Development/rtags/build/bin"))
(setq rtags-path "~/Development/rtags/build/bin/")
(prelude-require-package 'rtags)
(setq rtags-autostart-diagnostics t)
(rtags-diagnostics)
(setq rtags-completions-enabled t)
(setq rtags-use-helm t)
(rtags-enable-standard-keybindings)
(prelude-require-package 'cycbuf)
(prelude-require-package 'company)
(prelude-require-package 'company-quickhelp)
(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-rtags))
(global-company-mode)
(company-quickhelp-mode 1)
(define-key c-mode-base-map (kbd "<C-tab>") (function company-complete))
;; (prelude-require-package 'cmake-ide)

(prelude-require-package 'flycheck)
(require 'flycheck-rtags)
(setq flycheck-standard-error-navigation nil)

(prelude-require-package 'flycheck-pos-tip)
(with-eval-after-load 'flycheck (flycheck-pos-tip-mode))

(defun my-c-mode-common-hook ()
  (flycheck-select-checker 'rtags)
  (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
  (setq-local flycheck-check-syntax-automatically nil)
  (define-key c-mode-base-map (kbd "M-o") 'helm-projectile-find-other-file))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;;; init-cmake-ide.el ends here
