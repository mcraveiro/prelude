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
(prelude-require-package 'dap-mode)
(prelude-require-package 'company)
(prelude-require-package 'company-quickhelp)
(prelude-require-package 'lsp-mode)
(prelude-require-package 'company-lsp)
(prelude-require-package 'helm-lsp)

(require 'lsp-clients)
(setq lsp-clients-clangd-executable "/usr/bin/clangd-8")

(setq lsp-clients-clangd-args
      '("-j=2" "-log=verbose" "-background-index"
        ;; "--compile-commands-dir=/work/DomainDrivenConsulting/masd/dogen/integration/build/output/clang7/Release"
        )
      )

(setq lsp-prefer-flymake nil)
(prelude-require-package 'lsp-ui)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(require 'lsp-ui-flycheck)

flycheck-check-syntax-automatically

;; guess root from projectile
(setq lsp-auto-guess-root t)

(setq company-idle-delay 0.3)
(setq company-show-numbers t)
(setq company-minimum-prefix-length 2)
(setq company-dabbrev-downcase nil)
(setq company-dabbrev-other-buffers t)
(setq company-auto-complete nil)
(setq company-dabbrev-code-other-buffers 'all)
(setq company-dabbrev-code-everywhere t)
(setq company-dabbrev-code-ignore-case t)
(setq company-minimum-prefix-length 1)
(setq company-transformers nil
      company-lsp-async t
      company-lsp-cache-candidates nil)
(global-company-mode)
(company-quickhelp-mode 1)
(push 'company-lsp company-backends)

; (define-key c-mode-base-map (kbd "<C-tab>") (function company-complete))

(with-eval-after-load 'lsp-mode
  (add-hook
   'lsp-after-open-hook (lambda () (lsp-ui-flycheck-enable 1))))

(setq lsp-enable-indentation nil)

;; (setq lsp-ui-sideline-delay 5)
(setq lsp-ui-doc-delay 5)

(setq lsp-ui-sideline-enable nil
      ;; lsp-enable-completion-at-point t
      lsp-ui-doc-position 'at-point
      lsp-ui-doc-header nil
      lsp-ui-doc-include-signature t
      ;; lsp-ui-doc-background (doom-color 'base4)
      ;; lsp-ui-doc-border (doom-color 'fg)
      )

(setq lsp-ui-doc-enable t
      ;; lsp-ui-doc-use-childframe t
      ;; lsp-ui-doc-position 'top
      ;; lsp-ui-doc-include-signature t
      ;; lsp-ui-sideline-enable nil
      lsp-ui-flycheck-enable t
      lsp-ui-flycheck-list-position 'right
      lsp-ui-flycheck-live-reporting nil
      lsp-ui-peek-enable t
      lsp-ui-peek-list-width 60
      lsp-ui-peek-peek-height 25
      )

(custom-set-faces
 '(lsp-ui-sideline-current-symbol
   ((t (:foreground "royal blue" :box (:line-width -1 :color "royal blue")
                    :weight ultra-bold :height 0.99))))
 '(lsp-ui-sideline-global ((t (:background "dark blue"))))
 '(lsp-ui-sideline-symbol
   ((t (:foreground "sea green" :box (:line-width -1 :color "sea green")
                    :weight ultra-bold :height 0.99))))
 ; '(region ((t (:background "sky blue" :distant-foreground "gtk_selection_fg_color"))))
 )


(setq netrom--general-lsp-hydra-heads
        '(;; Xref
          ("d" xref-find-definitions "Definitions" :column "Xref")
          ("D" xref-find-definitions-other-window "-> other win")
          ("r" xref-find-references "References")
          ("s" netrom/helm-lsp-workspace-symbol-at-point "Helm search")
          ("S" netrom/helm-lsp-global-workspace-symbol-at-point "Helm global search")

          ;; Peek
          ("C-d" lsp-ui-peek-find-definitions "Definitions" :column "Peek")
          ("C-r" lsp-ui-peek-find-references "References")
          ("C-i" lsp-ui-peek-find-implementation "Implementation")

          ;; LSP
          ("p" lsp-describe-thing-at-point "Describe at point" :column "LSP")
          ("C-a" lsp-execute-code-action "Execute code action")
          ("R" lsp-rename "Rename")
          ("t" lsp-goto-type-definition "Type definition")
          ("i" lsp-goto-implementation "Implementation")
          ("f" helm-imenu "Filter funcs/classes (Helm)")
          ("C-c" lsp-describe-session "Describe session")

          ;; Flycheck
          ("l" lsp-ui-flycheck-list "List errs/warns/notes" :column "Flycheck"))

        netrom--misc-lsp-hydra-heads
        '(;; Misc
          ("q" nil "Cancel" :column "Misc")
          ("b" pop-tag-mark "Back")))

  ;; Create general hydra.
  (eval `(defhydra netrom/lsp-hydra (:color blue :hint nil)
           ,@(append
              netrom--general-lsp-hydra-heads
              netrom--misc-lsp-hydra-heads)))

(add-hook 'lsp-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-l") 'netrom/lsp-hydra/body)
            'lsp-ui-mode)
          )

(add-hook 'c++-mode-hook (lambda ()
                           (lsp)
                           (flycheck-mode)))
;;; init-cquery.el ends here
