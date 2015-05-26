;;; init-cedet.el --- Emacs Prelude: Personal cedet configuration
;;
;; Copyright © 2015 Marco Craveiro
;;
;; Author: Marco Craveiro <marco_craveiro@gmail.com>
;; URL: https://github.com/mcraveiro/prelude
;; Version: 0.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Personal cedet configuration

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

;; Load our snapshotted git version cedet.
(setq toplevel-dir (file-name-directory (or (buffer-file-name) load-file-name)))
(load-file (concat toplevel-dir "vendor/cedet/cedet-devel-load.el"))

;; Add further minor-modes to be enabled by semantic-mode.
(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode t)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode t)
(add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode t)

;; Enable semantic
(semantic-mode 1)

;; Enable ede
(global-ede-mode 1)

;; Add a few semantic modes
(add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)
(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)

(require 'semantic/bovine/c)
(require 'semantic/bovine/gcc)
(require 'semantic/bovine/clang)
(require 'semantic/ia)
(require 'semantic/decorate/include)
(require 'semantic/lex-spp)

;; (global-semantic-decoration-mode)
;; (global-semantic-highlight-func-mode)

;; Disable some annoying semantic decorations
;; (setq semantic-decoration-styles
;;       (append
;;        '(("semantic-decoration-on-includes" . t))
;;        '(("semantic-decoration-on-protected-members" . nil))
;;        '(("semantic-decoration-on-private-members" . nil))
;;        '(("semantic-tag-boundary" . nil))))

;; (Global-set-key (kbd "C-c C-g") 'semantic-symref)
;; (global-set-key (kbd "C-c C-f") 'semantic-symref-symbol)

;; Enable compilation database support
(prelude-require-package 'ede-compdb)
(require 'ede-compdb)

;; Setup flycheck with compilation database
(setq-default flycheck-c/c++-clang-executable "/usr/bin/clang++-3.5")
(require 'flycheck)

(defun flycheck-compdb-setup ()
  (when (and ede-object (oref ede-object compilation))
    (let* ((comp (oref ede-object compilation))
           (cmd (get-command-line comp)))

      ;; Configure flycheck clang checker.
      ;; TODO: configure gcc checker also
      (when (string-match " -std=\\([^ ]+\\)" cmd)
        (setq-local flycheck-clang-language-standard (match-string 1 cmd)))
      (when (string-match " -stdlib=\\([^ ]+\\)" cmd)
        (setq-local flycheck-clang-standard-library (match-string 1 cmd)))
      (when (string-match " -fms-extensions " cmd)
        (setq-local flycheck-clang-ms-extensions t))
      (when (string-match " -fno-exceptions " cmd)
        (setq-local flycheck-clang-no-exceptions t))
      (when (string-match " -fno-rtti " cmd)
        (setq-local flycheck-clang-no-rtti t))
      (when (string-match " -fblocks " cmd)
        (setq-local flycheck-clang-blocks t))
      (setq-local flycheck-clang-includes (get-includes comp))
      (setq-local flycheck-clang-definitions (get-defines comp))
      (setq-local flycheck-clang-include-path (get-include-path comp t))
      )))

(add-hook 'ede-compdb-project-rescan-hook #'flycheck-compdb-setup)
(add-hook 'ede-minor-mode-hook #'flycheck-compdb-setup)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Enable eassist.
(prelude-require-package 'eassist)

(setq eassist-header-switches
      '(("h" . ("cpp" "cxx" "c++" "CC" "cc" "C" "c" "mm" "m"))
        ("hh" . ("cc" "CC" "cpp" "cxx" "c++" "C"))
        ("hpp" . ("cpp" "cxx" "c++" "cc" "CC" "C"))
        ("hxx" . ("cxx" "cpp" "c++" "cc" "CC" "C"))
        ("h++" . ("c++" "cpp" "cxx" "cc" "CC" "C"))
        ("H" . ("C" "CC" "cc" "cpp" "cxx" "c++" "mm" "m"))
        ("HH" . ("CC" "cc" "C" "cpp" "cxx" "c++"))
        ("cpp" . ("hpp" "hxx" "h++" "HH" "hh" "H" "h"))
        ("cxx" . ("hxx" "hpp" "h++" "HH" "hh" "H" "h"))
        ("c++" . ("h++" "hpp" "hxx" "HH" "hh" "H" "h"))
        ("CC" . ("HH" "hh" "hpp" "hxx" "h++" "H" "h"))
        ("cc" . ("hh" "HH" "hpp" "hxx" "h++" "H" "h"))
        ("C" . ("hpp" "hxx" "h++" "HH" "hh" "H" "h"))
        ("c" . ("h"))
        ("m" . ("h"))
        ("mm" . ("h"))))

;;; init-cedet.el ends here
