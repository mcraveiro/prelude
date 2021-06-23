;;; init-bm.el --- Emacs Prelude: Personal bookmarks configuration
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

;; Visual bookmarks mode
(prelude-require-package 'bm)
(prelude-require-package 'helm-bm)

;; location of the persistent bookmarks
(setq bm-repository-file (concat temporary-file-directory "bm-repository"))

;; Put bookmarks on the fringe
(setq bm-highlight-style 'bm-highlight-only-fringe)

;; cycle across buffers
(setq bm-cycle-all-buffers t)

;; repository should be restored when loading `bm'
(setq bm-restore-repository-on-load t)

;; buffer should be recentered around the bookmark
(setq bm-recenter t)

;; make bookmarks persistent as default
(setq-default bm-buffer-persistence t)

;; FIXME: for some reason the hooks don't work with prelude.

;; loading the repository from file when on start up
;(add-hook' after-init-hook 'bm-repository-load)

;; restoring bookmarks when on file find
;(add-hook 'find-file-hooks 'bm-buffer-restore)

;; saving bookmark data on killing a buffer
;(add-hook 'kill-buffer-hook 'bm-buffer-save)

;; saving the repository to file when on exit
;; `kill-buffer-hook' is not called when emacs is killed, so we
;; must save all bookmarks first
;(add-hook 'kill-emacs-hook '(lambda nil
                              ;(bm-buffer-save-all)
                              ;(bm-repository-save)))

;; update bookmark repository when saving the file
;(add-hook 'after-save-hook 'bm-buffer-save)

;;
;; Key bindings
;;
(global-set-key (kbd "<f9>") 'bm-toggle)
(global-set-key (kbd "<C-f9>") 'bm-next)
(global-set-key (kbd "<S-f9>") 'bm-previous)
(global-set-key (kbd "<M-f9>") 'helm-bm)

(defvar bm-after-goto-hook nil
  "Hook run after jumping to a bookmark in `bm-goto'.")

(add-hook 'bm-after-goto-hook 'org-bookmark-jump-unhide)

(defun bm-goto (bookmark)
  "Goto specified BOOKMARK."
  (if (bm-bookmarkp bookmark)
      (progn
        (if bm-goto-position
            (goto-char (max
                        ;; sometimes marker-position is before start of overlay
                        ;; marker is not updated when overlay hooks are called.
                        (overlay-start bookmark)
                        (marker-position (overlay-get bookmark 'position))))
          (goto-char (overlay-start bookmark)))
        (run-hooks 'bm-after-goto-hook)
        (setq bm-wrapped nil)           ; turn off wrapped state
        (if bm-recenter
            (recenter))
        (let ((annotation (overlay-get bookmark 'annotation)))
          (if annotation
              (message annotation)))
        (when  (overlay-get bookmark 'temporary-bookmark)
          (bm-bookmark-remove  bookmark)))
    (when (> bm-verbosity-level 0)
      (message "Bookmark not found."))))

;;; init-bm.el ends here
