;;; helm-corefighter.el --- Helm interface to corefighter.el -*- lexical-binding: t -*-

;; Copyright (C) 2018 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 1.0-pre
;; Package-Requires: ((emacs "25.1") (helm "2.7.0") (corefighter "1.0"))
;; URL: https://github.com/akirak/corefighter.el

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a Helm interface to corefighter.
;; Instead of opening a sidebar window, this library lets you access
;; items via Helm.
;;
;; `helm-corefighter' is a Helm command.  It supports a persistent
;; action that allows you to preview an item without closing the
;; Helm session.  It also supports helm-follow-mode.

;;; Code:

(require 'helm)
(require 'corefighter)

(defvar helm-corefighter-action-buffer nil
  "Store the buffer selected by a (persistent) action.")

(defvar helm-corefighter-target-widow nil)

;;;###autoload
(defun helm-corefighter (&optional refresh)
  "Helm that displays items in the corefighter modules.

If REFRESH is non-nil, force refreshing items."
  (interactive "P")
  (setq helm-corefighter-action-buffer nil
        helm-corefighter-target-window (selected-window))
  (helm :prompt "corefighter: "
        :sources
        (let ((data (corefighter--get-data refresh)))
          (mapcar
           (lambda (section-data)
             (let-alist section-data
               (helm-build-sync-source .title
                 :candidates
                 (mapcar (lambda (item)
                           (cons (corefighter-item-title item)
                                 item))
                         .items)
                 :action #'helm-corefighter--action)))
           data)))
  ;; If a persistent action opens a buffer but helm is aborted,
  ;; switch to the buffer
  (when helm-corefighter-action-buffer
    (switch-to-buffer helm-corefighter-action-buffer)))

(defun helm-corefighter--action (item)
  "Execute ITEM in `helm-corefighter'."
  (let ((corefighter-target-window-setup nil))
    (with-selected-window helm-corefighter-target-window
      (corefighter-sidebar--run-action
       `(lambda ()
          ,(corefighter-item-action item)
          ;; Store the buffer in case the helm session is aborted
          (setq helm-corefighter-action-buffer (current-buffer)))
       (corefighter-item-action-window item)))))

(provide 'helm-corefighter)
;;; helm-corefighter.el ends here
