;;; init-org.el --- Emacs Prelude: Personal Org-mode configuration
;;
;; Copyright © 2015 Marco Craveiro
;;
;; Author: Marco Craveiro <marco_craveiro@gmail.com>
;; URL: https://github.com/mcraveiro/prelude
;; Version: 0.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Personal Org-mode configuration

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
(prelude-require-package 'org)
(prelude-require-package 'org-plus-contrib)
(require 'ox-taskjuggler)
(add-to-list 'org-export-backends 'taskjuggler)

;; Formatting of time stamps in clock table.
(setq org-time-clocksum-format
       (quote
        (:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)))

(setq org-duration-format (quote h:mm))

;; Preserve source code indentation on code blocks
(setq org-src-preserve-indentation t)

;; Unset disputed keys on other modes
(add-hook 'sh-mode-hook
          (lambda ()
            (local-unset-key (kbd "C-c C-o")) ; trigger for `sh-while-getopts'
            ))

;; Disable keys already taken by other modes
(add-hook 'org-mode-hook
          (lambda ()
            (setq org-replace-disputed-keys t)
            (setq org-CUA-compatible t)
            ;; (org-set-local 'yas/trigger-key [tab])
            ))

;; Provide org-mode link functionality for all buffers.
(global-set-key (kbd "C-c C-l") 'org-insert-link-global)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c C-o") 'org-open-at-point-global)

;; Avoid using keys already taken by other modes such as pc-select
(setq org-replace-disputed-keys t)

;; Log time when marking task as done
(setq org-log-done 'time)

;; Remove clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

;; Save the running clock and all clock history when exiting Emacs,
;; load it on startup
(setq org-clock-persist (quote history))

;; Enable auto clock resolution for finding open clocks
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))

;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)

;; File paths should always be relative
(setq org-link-file-path-type 'relative)

;; Let babel evaluate without asking
(setq org-confirm-babel-evaluate nil)

;; Enabled languages for org-babel code blocks
;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((emacs-lisp . t)
;;    (sh . t)
;;    (gnuplot . t)
;;    (R . t)))

;; States for stories on our sprints.
(setq org-todo-keywords
      '((sequence "STARTED" "|" "COMPLETED" "CANCELLED" "POSTPONED")))

(setq org-todo-keyword-faces
      '(
        ("STARTED" . (:foreground "yellow" :weight bold))
        ("POSTPONED" . (:foreground "blue" :weight bold))
        ("COMPLETED" . (:foreground "green" :weight bold))
        ("CANCELLED" . (:foreground "red" :weight bold))
        ))

; Hack for now as clocktable appears to be borked.
(defun org-clock-time%-mod (total &rest strings)
  "Compute a time fraction in percent.
TOTAL is a time string like 1d 10:21 or only time (10:21) specifying the total times.
STRINGS is a list of strings that should be checked for a time.
The first string that does have a time will be used.
This function is made for clock tables."
  (let ((re "\\(\\([0-9]+\\)d \\)?\\([0-9]+\\):\\([0-9]+\\)")
        tot s)
    (save-match-data
      (catch 'exit
    (if (not (string-match re total))
        (throw 'exit 0.)
      (setq tot (+ (string-to-number (match-string 4 total))
               (* 60 (string-to-number (match-string 3 total)))
               (* 1440 (string-to-number
                (if (null (match-string 2 total))
                    "0"
                  (match-string 2 total))))))
      (if (= tot 0.) (throw 'exit 0.)))
    (while (setq s (pop strings))
      (if (string-match re s)
          (throw 'exit
             (/ (* 100.0 (+ (string-to-number (match-string 4 s))
                    (* 60 (string-to-number (match-string 3 s)))
                    (* 1440 (string-to-number
                         (if (null (match-string 2 s))
                         "0"
                           (match-string 2 s))))))
            tot))))
    0))))

;; nadvice
; (advice-add 'org-clock-time% :override #'org-clock-time%-mod)

;;; init-org.el ends here
