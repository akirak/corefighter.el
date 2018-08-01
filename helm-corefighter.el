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
;;;; Variables

(defvar helm-corefighter-action-buffer nil
  "Store the buffer selected by a (persistent) action.")

(defvar helm-corefighter-target-widow nil)

;;;; Faces
;; TODO: Define faces
(defface helm-corefighter-future-face
  nil
  "Face for future (scheduled after the next midnight) items.")
(defface helm-corefighter-scheduled-face
  nil
  "Face for items scheduled at a particular time on today.")
(defface helm-corefighter-unscheduled-face
  nil
  "Face for items scheduled at some time on today.")
(defface helm-corefighter-overdue-face
  '((t (:color "red")))
  "Face for items scheduled before the last midnight.")

;;;; Sources

(defvar helm-corefighter-agenda-source
  (helm-build-sync-source "Agenda"
    :candidates #'helm-corefighter--agenda-candidates
    :action #'helm-corefighter--run-cursor))

;;;; Main

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
                 (cl-loop for item being the elements of .items using (index index)
                          collect (cons (corefighter-item-title item)
                                        (make-corefighter-cursor :module-cursor .module
                                                                 :item item
                                                                 :index index)))
                 :action #'helm-corefighter--run-cursor)))
           data)))
  ;; If a persistent action opens a buffer but helm is aborted,
  ;; switch to the buffer
  (when helm-corefighter-action-buffer
    (switch-to-buffer helm-corefighter-action-buffer)))

(defun helm-corefighter--run-cursor (cursor)
  "Execute the item referenced by CURSOR."
  (let* ((item (corefighter-cursor-item cursor))
         (action `(lambda ()
                    ,(corefighter-item-action item)
                    ;; Store the buffer in case the helm session is aborted
                    (setq helm-corefighter-action-buffer (current-buffer))))
         (window (corefighter-item-action-window item))
         (corefighter-target-window-setup nil))
    (with-selected-window helm-corefighter-target-window
      (corefighter--run-action-1 action window cursor))))

;;;; Agenda

(defun helm-corefighter-agenda (&optional refresh)
  "Display an agenda."
  (interactive "P")
  (corefighter--get-data refresh)
  (helm :prompt "corefighter agenda: "
        :sources '(helm-corefighter-agenda-source)))

(defun helm-corefighter--agenda-candidates ()
  "Build candidates for the agenda source."
  (cl-loop for (type . cursors) in (corefighter--partition-agenda)
           for cursor in cursors
           append (cons (let* ((item (corefighter-cursor-item cursor))
                               (title (corefighter-item-title item))
                               (module-cursor (corefighter-cursor-module-cursor cursor))
                               (module-title (corefighter-module-cursor-title module-cursor)))
                          (format "%-8s  %s  from %s"
                                  (cl-case type
                                    ('overdue (propertize "OVERDUE"
                                                          'face 'helm-corefighter-overdue-face))
                                    ('unscheduled (propertize "Today"
                                                              'face 'helm-corefighter-unscheduled-face))
                                    ('scheduled (propertize (corefighter-format-time (corefighter-item-due item) "%R")
                                                            'face 'helm-corefighter-scheduled-face))
                                    ('future (propertize (pcase (corefighter-due-diff-days item)
                                                           (1 "Tomorrow")
                                                           (n (format "in %dd" n)))
                                                         'face 'helm-corefighter-future-face)))
                                  title
                                  module-title))
                        cursor)))

(provide 'helm-corefighter)
;;; helm-corefighter.el ends here
