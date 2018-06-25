;;; corefighter.el --- Flexible dashboard -*- lexical-binding: t -*-

;; Copyright (C) 2018 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 1.0-pre
;; Package-Requires: ((emacs "25.1") (page-break-lines "0"))
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

;; Core Fighter is a dashboard for Emacs.

;;; Code:

(require 'eieio)
(require 'seq)
(require 'cl-lib)

(defgroup corefighter nil "Corefighter group"
  :prefix "corefighter")

;;;; Variables
(defconst corefighter-sidebar-buffer "*corefighter sidebar*")

(defvar corefighter-module-instances nil)

(defcustom corefighter-urgency-text "! "
  "Indicator for items that has the urgency status."
  :type 'string
  :group 'corefighter)

(defcustom corefighter-target-window-setup 'only
  "Where an action should be run and how the action should be organized."
  :type '(choice (const :tag "First other window" other-window)
                 (const :tag "Delete other windows" only))
  :group 'corefighter)

(defcustom corefighter-modules
  '((corefighter-org-agenda))
  "Alist of module configurations to enable.

Each item in this list should be a cons cell of a symbol and a plist.
The symbol represents a subclass of `corefighter-module', and the plist is
a list of options passed when the module is instantiated."
  :type '(repeat (cons symbol plist))
  :group 'corefighter
  :set (lambda (key value)
         (set key value)
         (corefighter-load-modules)))

;;;; Classes
;;;;; Items
(cl-defstruct corefighter-item title urgency description key
              action action-window)

;;;;; Modules
(defclass corefighter-module ()
  ((title :type string :allocation :class))
  "Abstract class of module.")

(cl-defgeneric corefighter-module-items ((obj corefighter-module)
                                         &optional refresh)
  "Retrieve items of a module.")

(cl-defgeneric corefighter-module-urgent-p ((obj corefighter-module))
  "t if the module is urgent."
  (seq-find #'corefighter-item-urgency (corefighter-module-items obj)))

;;;; Loading modules
(defun corefighter--find-instance (class)
  "Find a module instance of CLASS."
  (cl-loop for obj in corefighter-module-instances
           when (same-class-p obj class)
           return obj))

(defun corefighter-load-modules ()
  "Reload modules based on the configuration in `corefighter-modules'."
  (interactive)
  (let ((old corefighter-module-instances)
        new class)
    (condition-case err
        (cl-loop for (class-symbol . options) in corefighter-modules
                 do (progn
                      (setq class (find-class class-symbol t))
                      (if-let ((obj (corefighter--find-instance class)))
                          (push obj new)
                        (push (apply #'make-instance class-symbol options) new))))
      (error (message "Error while loading a module: " err)))
    (setq corefighter-module-instances (nreverse new))))

;;;; Helper functions
(defun corefighter--prepare-target-window ()
  (cl-case corefighter-target-window-setup
    ('only (progn (let ((windows (corefighter--other-windows)))
                    (select-window (pop windows))
                    (mapc #'delete-window windows))))
    (otherwise (select-window (car (corefighter--other-windows))))))

(defun corefighter--other-windows ()
  (cl-remove-if (lambda (window)
                  (equal (buffer-name (window-buffer window))
                         corefighter-sidebar-buffer))
                (window-list)))

;;;; Preconfigured modules
(defclass corefighter-org-agenda (corefighter-module)
  ((title :initform "Org Agenda")))

(defmethod corefighter-module-items ((obj corefighter-org-agenda)
                                     &optional _refresh)
  (cl-loop for (key description . options) in org-agenda-custom-commands
           collect (make-corefighter-item
                    :title description
                    :key key
                    :action-window `(org-agenda-window-setup
                                     . ,org-agenda-window-setup)
                    :description (if (listp (car options))
                                     "block agenda"
                                   (format "%s %s"
                                           (symbol-name (car options))
                                           (nth 1 options)))
                    :urgency nil
                    :action `(org-agenda nil ,key))))

(provide 'corefighter)
(require 'corefighter-sidebar)
;;; corefighter.el ends here
