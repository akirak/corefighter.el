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

(defgroup helm-corefighter nil
  "Helm for Core Fighter."
  :group 'corefighter
  :prefix "helm-corefighter")

(defvar helm-corefighter-action-buffer nil
  "Store the buffer selected by a (persistent) action.")

(defvar helm-corefighter-target-widow nil)

(defcustom helm-corefighter-prepend-agenda t
  "Prepend the agenda source to `helm-corefighter' command."
  :type 'boolean
  :group 'helm-corefighter)

;;;; Faces
;; TODO: Define faces
(defface helm-corefighter-agenda-future-face
  nil
  "Face for future (scheduled after the next midnight) items.")
(defface helm-corefighter-agenda-scheduled-face
  nil
  "Face for items scheduled at a particular time on today.")
(defface helm-corefighter-agenda-unscheduled-face
  nil
  "Face for items scheduled at some time on today.")
(defface helm-corefighter-agenda-overdue-face
  '((t (:inherit error)))
  "Face for items scheduled before the last midnight.")
(defface helm-corefighter-agenda-module-face
  '((t (:inherit font-lock-builtin-face)))
  "Face for module titles in Helm candidates.")
(defface helm-corefighter-agenda-item-face
  nil
  "Face for item titles in Helm candidates.")

;;;; Sources

(defvar helm-corefighter-agenda-source
  (helm-build-sync-source "Agenda"
    :candidates #'helm-corefighter--agenda-candidates
    :action #'helm-corefighter--run-cursor))

(defun helm-corefighter--make-module-source (module-cursor items)
  "Build a Helm source from MODULE-CURSOR and ITEMS."
  (helm-build-sync-source (corefighter-module-cursor-title module-cursor)
    :candidates
    (cl-loop for item being the elements of items using (index index)
             collect (cons (corefighter-item-title item)
                           (make-corefighter-cursor :module-cursor module-cursor
                                                    :item item
                                                    :index index)))
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
        (let* ((data (corefighter--get-data refresh))
               (sources (mapcar
                         (lambda (section-data)
                           (let-alist section-data
                             (helm-corefighter--make-module-source .module .items)))
                         data)))
          (if helm-corefighter-prepend-agenda
              (cons helm-corefighter-agenda-source sources)
            sources)))
  ;; If a persistent action opens a buffer but helm is aborted,
  ;; switch to the buffer
  (when helm-corefighter-action-buffer
    (switch-to-buffer helm-corefighter-action-buffer)))

(defun helm-corefighter--run-cursor (cursor)
  "Execute the item referenced by CURSOR."
  (let* ((item (corefighter-cursor-item cursor))
         (payload (corefighter-item-payload item))
         (action (corefighter-module-cursor-navigate-action
                  (corefighter-cursor-module-cursor cursor)))
         (corefighter-target-window-setup nil))
    (with-selected-window helm-corefighter-target-window
      (corefighter--run-action-2 action payload cursor))))

;;;###autoload
(defun helm-corefighter-run-module (module)
  "Run Helm with a single module.

This function is intended for prototyping.

MODULE is a module for Core Fighter, e.g. an instance of a class that
is a subclass of `corefighter-module'."
  (setq helm-corefighter-target-window (selected-window))
  (helm :prompt "corefighter: "
        :sources
        (let ((title (corefighter-module-title module))
              (items (corefighter-module-items module)))
          (list (helm-corefighter--make-module-source
                 (corefighter--module-cursor module)
                 items)))))

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
           append (mapcar (lambda (cursor)
                            (cons (helm-corefighter--format-agenda-item type cursor)
                                  cursor))
                          cursors)))

(defun helm-corefighter--format-agenda-item (type cursor)
  "Format a Helm candidate.

TYPE is a group, and CURSOR is a cursor to the item."
  (let* ((item (corefighter-cursor-item cursor))
         (title (corefighter-item-title item))
         (module-cursor (corefighter-cursor-module-cursor cursor))
         (module-title (corefighter-module-cursor-title module-cursor)))
    (format "%-8s  %-35s  %s"
            (cl-case type
              ('overdue (propertize "OVERDUE"
                                    'face 'helm-corefighter-agenda-overdue-face))
              ('unscheduled (propertize "Today"
                                        'face 'helm-corefighter-agenda-unscheduled-face))
              ('scheduled (propertize (corefighter-format-time (corefighter-item-due item) "%R")
                                      'face 'helm-corefighter-agenda-scheduled-face))
              ('future (propertize (pcase (corefighter-due-diff-days item)
                                     (1 "Tomorrow")
                                     (n (format "in %dd" n)))
                                   'face 'helm-corefighter-agenda-future-face)))
            (propertize title
                        'face 'helm-corefighter-agenda-item-face)
            (propertize module-title
                        'face 'helm-corefighter-agenda-module-face))))

(provide 'helm-corefighter)
;;; helm-corefighter.el ends here
