;;; init-ibuffer.el --- Emacs Prelude: Personal ibuffer configuration.
;;
;; Copyright © 2015 Marco Craveiro
;;
;; Author: Marco Craveiro <marco_craveiro@gmail.com>
;; URL: https://github.com/mcraveiro/prelude
;; Version: 0.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Personal ibuffer configuration.

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
(setq toplevel-dir (file-name-directory (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path (concat toplevel-dir "/vendor"))
(require 'ede-ibuffer)

(define-key ibuffer-mode-map (kbd "/ e") 'ibuffer-filter-by-ede-project)
(define-key ibuffer-mode-map (kbd "% e") 'ibuffer-mark-by-ede-project-regexp)
(define-key ibuffer-mode-map (kbd "s e") 'ibuffer-do-sort-by-ede-project)

;; Group buffers
(setq ibuffer-saved-filter-groups
      (quote (("home"
               ("c++" (mode . c++-mode))
               ("python" (mode . python-mode))
               ("fsharp" (or
                          (mode . inferior-fsharp-mode)
                          (mode . fsharp-mode)))
               ("csharp" (mode . csharp-mode))
               ("java" (mode . java-mode))
               ("ruby" (mode . ruby-mode))
               ("perl" (mode . perl-mode))
               ("javascript" (or
                              (mode . javascript-mode)
                              (mode . js2-mode)
                              (mode . js-mode)))
               ("php" (mode . php-mode))
               ("xml" (mode . nxml-mode))
               ("sql" (or
                       (mode . sql-mode)
                       (name . "^\\*SQL")))
               ("make" (or
                        (mode . cmake-mode)
                        (mode . makefile-mode)
                        (mode . makefile-gmake-mode)))
               ("t4" (name . ".tt$"))
               ("Dogen - Stitch" (name . ".stitch$"))
               ("bash" (mode . sh-mode))
               ("awk" (mode . awk-mode))
               ("latex" (or
                         (name . ".tex$")
                         (mode . tex-mode)
                         (mode . latex-mode)))
               ("markdown" (or
                            (mode . markdown-mode)
                            (mode . gfm-mode)))
               ("emacs-lisp" (or
                              (mode . emacs-lisp-mode)
                              (name . "^\\*Compile-Log\\*$")))
               ("powershell" (or
                              (mode . powershell-mode)
                              (name . "^\\*PowerShell")))
               ("logs" (or
                        (mode . log4j-mode)
                        (mode . logview-mode)))
               ("grep" (or
                         (name . "^\\*Occur\\*$")
                         (name . "^\\*Moccur\\*$")
                         (mode . grep-mode)))
               ("irc" (mode . erc-mode))
               ("shell" (or
                         (name . "^\\*Shell Command Output\\*$")
                         (mode . shell-mode)
                         (mode . ssh-mode)
                         (name . "^\\*compilation\\*$")))
               ("file management" (or
                                   (mode . dired-mode)
                                   (mode . tar-mode)))
               ("org" (mode . org-mode))
               ("text files" (or
                              (mode . conf-unix-mode)
                              (mode . conf-space-mode)
                              (mode . text-mode)))
               ("yaml" (mode . yaml-mode))
               ("msdos" (mode . dos-mode))
               ("patches" (or
                           (name . "^\\*Assoc file dif")
                           (mode . diff-mode)))
               ("version control" (or
                                   (name . "^\\*svn-")
                                   (name . "^\\*vc")
                                   (name . "^\\*cvs")
                                   (name . "^\\*magit")))
               ("snippets" (mode . snippet-mode))
               ("semantic" (or
                            (mode . data-debug-mode)
                            (name . "^\\*Parser Output\\*$")
                            (name . "^\\*Lexer Output\\*$")))
               ("web browsing" (or
                                (mode . w3m-mode)
                                (mode . twittering-mode)
                                ))
               ("music" (or
                         (mode . bongo-playlist-mode)
                         (mode . bongo-library-mode)))
               ("mail" (or
                        (mode . gnus-group-mode)
                        (mode . gnus-summary-mode)
                        (mode . gnus-article-mode)
                        (name . "^\\*imap log\\*$")
                        (name . "^\\*gnus trace\\*$")
                        (name . "^\\*nnimap imap.")
                        ))
               ("web development" (or
                                   (mode . html-mode)
                                   (mode . css-mode)))
               ("documentation" (or
                                 (mode . Info-mode)
                                 (mode . apropos-mode)
                                 (mode . woman-mode)
                                 (mode . help-mode)
                                 (mode . Man-mode)))
               ("system" (or
                          (name . "^\\*Packages\\*$")
                          (name . "^\\*RTags Log\\*$")
                          (name . "^\\*tramp")
                          (name . "^\\*debug tramp")
                          (name . "^\\*Proced log\\*$")
                          (name . "^\\*Ediff Registry\\*$")
                          (name . "^\\*Bookmark List\\*$")
                          (name . "^\\*RE-Builder\\*$")
                                  (name . "^\\*Kill Ring\\*$")
                                  (name . "^\\*Calendar\\*$")
                                  (name . "^\\*icalendar-errors\\*$")
                                  (name . "^\\*Proced\\*$")
                                  (name . "^\\*WoMan-Log\\*$")
                                  (name . "^\\*Apropos\\*$")
                                  (name . "^\\*Completions\\*$")
                                  (name . "^\\*Help\\*$")
                                  (name . "^\\*Dired log\\*$")
                                  (name . "^\\*scratch\\*$")
                                  (name . "^\\*gnuplot\\*$")
                                  (name . "^\\*Flycheck errors\\*$")
                                  (name . "^\\*compdb:")
                                  (name . "^\\*Backtrace\\*$")
                                  (name . "^\\*Messages\\*$")))
               ))))

;; Shortcut for ibuffer
(global-set-key (kbd "<f5>") 'ibuffer)

;; Enable expert mode
(setq ibuffer-expert t)

;; Remove empty groups
(setq ibuffer-show-empty-filter-groups nil)

;; Setup filter groups
(add-hook 'ibuffer-mode-hook
          '(lambda ()
             (ibuffer-auto-mode 1)
             (ibuffer-switch-to-saved-filter-groups "home")
             (ibuffer-do-sort-by-filename/process)))

;;; init-buffer.el ends here
