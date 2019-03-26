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

(prelude-require-package 'cycbuf)
(prelude-require-package 'company)
(prelude-require-package 'company-quickhelp)
(prelude-require-package 'cquery)

(prelude-require-package 'lsp-mode)
(prelude-require-package 'company-lsp)
(prelude-require-package 'lsp-ui)
(require 'lsp-ui-flycheck)

(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(setq cquery-executable "/home/marco/Development/cquery/output/cquery")

;; guess root from projectile
(setq lsp-auto-guess-root t)

(global-company-mode)
(company-quickhelp-mode 1)
(push 'company-lsp company-backends)
; (define-key c-mode-base-map (kbd "<C-tab>") (function company-complete))

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-after-open-hook (lambda () (lsp-ui-flycheck-enable 1))))

(setq lsp-ui-sideline-delay 5)
(custom-set-faces
 '(lsp-ui-sideline-current-symbol ((t (:foreground "royal blue" :box (:line-width -1 :color "royal blue") :weight ultra-bold :height 0.99))))
 '(lsp-ui-sideline-global ((t (:background "dark blue"))))
 '(lsp-ui-sideline-symbol ((t (:foreground "sea green" :box (:line-width -1 :color "sea green") :weight ultra-bold :height 0.99))))
 ; '(region ((t (:background "sky blue" :distant-foreground "gtk_selection_fg_color"))))
 )

(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(setq cquery-extra-init-params '(:index (:comments 2) :cacheFormat "msgpack"))

;(defun cquery//enable ()
;  (condition-case nil
;      (lsp-cquery-enable)
;    (user-error nil)))

(add-hook 'c++-mode-hook (lambda ()
                           (require 'cquery)
                           (lsp)
                           (flycheck-mode)))
;;; init-cquery.el ends here
