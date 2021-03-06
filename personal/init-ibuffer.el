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
               ("python" (or
                          (mode . python-mode)
                          (name . "^\\*Python\\*$")))
               ("fsharp" (or
                          (mode . inferior-fsharp-mode)
                          (mode . fsharp-mode)))
               ("csharp" (mode . csharp-mode))
               ("java" (mode . java-mode))
               ("kotlin" (mode . kotlin-mode))
               ("ruby" (mode . ruby-mode))
               ("perl" (mode . perl-mode))
               ("json" (mode . json-mode))
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
               ("Dogen - Stitch" (or
                                  (mode . headtail-mode)
                                  (name . ".stitch$")))
               ("bash" (mode . sh-mode))
               ("awk" (mode . awk-mode))
               ("latex" (or
                         (name . ".tex$")
                         (name . ".texi$")
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
               ("irc" (or
                       (mode . erc-list-mode)
                       (mode . erc-mode)))
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
                                   (name . "^\\magit")))
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
                          (name . "^\\*helm M-x\\*$")
                          (name . "^\\*helm mini\\*$")
                          (name . "^\\*helm projectile\\*$")
                          (name . "^\\*RTags Log\\*$")
                          (name . "^\\**RTags Diagnostics\\*$")
                          (name . "^\\*tramp")
                          (name . "^\\**input/output of")
                          (name . "^\\**threads of")
                          (name . "^\\**breakpoints of")
                          (name . "^\\**Flycheck")
                          (name . "^\\**sx-search-result*")
                          (name . "^\\**gud-dogen.knit")
                          (name . "^\\**Warnings*")
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
               ("Treemacs" (or
                         (name . "^Treemacs Update")
                        (name . "^\\*nnimap imap.")
                        ))

               ))))

;; Shortcut for ibuffer
(global-set-key (kbd "<f5>") 'ibuffer)
(global-set-key (kbd "<f6>") 'bufler)

;; Enable expert mode
(setq ibuffer-expert t)

;; Remove empty groups
(setq ibuffer-show-empty-filter-groups nil)

(setq ibuffer-filter-group-name-face '(:inherit (font-lock-string-face bold)))

             ;; (
             ;;   ;; Display buffer icons on GUI
             ;;  (when (display-graphic-p)
             ;;    (define-ibuffer-column icon (:name " ")
             ;;      (let ((icon (if (and buffer-file-name
             ;;                         (all-the-icons-match-to-alist buffer-file-name
             ;;                                                       all-the-icons-icon-alist))
             ;;                      (all-the-icons-icon-for-file (file-name-nondirectory buffer-file-name)
             ;;                                                   :height 0.9 :v-adjust -0.05)
             ;;                    (all-the-icons-icon-for-mode major-mode :height 0.9 :v-adjust -0.05))))
             ;;        (if (symbolp icon)
             ;;            (setq icon (all-the-icons-faicon "file-o" :face 'all-the-icons-dsilver :height 0.9 :v-adjust -0.05))
             ;;          icon)))

             ;;    (setq ibuffer-formats '((mark modified read-only locked
             ;;                                  " " (icon 2 2 :left :elide) (name 18 18 :left :elide)
             ;;                                  " " (size 9 -1 :right)
             ;;                                  " " (mode 16 16 :left :elide) " " filename-and-process)
             ;;                            (mark " " (name 16 -1) " " filename))))
             ;;  )

;; Setup filter groups
(add-hook 'ibuffer-mode-hook
          '(lambda ()
             (ibuffer-auto-mode 1)
             (ibuffer-switch-to-saved-filter-groups "home")
             (ibuffer-do-sort-by-filename/process)))

;;; init-buffer.el ends here
