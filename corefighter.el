;;; corefighter.el --- Flexible dashboard -*- lexical-binding: t -*-

;; Copyright (C) 2018 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 1.0-pre
;; Package-Requires: ((emacs "25.1") (page-break-lines "0") (dash "2.12") (dash-functional "1.2.0") (ov "1.0.6"))
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
;; (require 'seq)
(require 'cl-lib)
(require 'cl-seq)
(require 'dash)
(require 'dash-functional)
(require 'ov)

;;;; Modules and structs
(cl-defstruct corefighter-module-cursor
  ;; Symbol
  class
  ;; A list of slot names and values
  slots
  ;; Optional string
  title)
(cl-defstruct corefighter-cursor module-cursor item index)

(cl-defstruct corefighter-time seconds date-only)

;;;; Variables
;; Declare variables to prevent undefined variable errors
(eval-when-compile
  (defvar org-agenda-window-setup))

(defvar corefighter-module-instances nil)
(defvar corefighter-last-data nil)

(defvar corefighter-last-item nil
  "Cursor to the last visited item.")

;; `corefighter-item-history` can't replace `corefighter-last-item'.
;; That is, the first item of the former is not always the same as
;; the latter.
;; This is because the latter variable is nil if the last action
;; was not executed with a cursor.
(defvar corefighter-item-history nil
  "List of cursors to visited items.")

(defvar corefighter-sidebar-width nil
  "Expected width of the sidebar.")
(make-variable-buffer-local 'corefighter-sidebar-width)

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
         ;; This function can't be run during initialization
         (when (featurep 'corefighter)
           (corefighter-load-modules))))

(defcustom corefighter-agenda-scope 'day
  "Time range of items to display in an agenda."
  :type '(choice (symbol "Today" day)
                 (integer :tag "Within N days"))
  :group 'corefighter)

(defcustom corefighter-hide-empty-sections t
  "In the sidebar, hide sections that contain no items."
  :type 'boolean
  :group 'corefighter)

(defcustom corefighter-sidebar-item-prefix " "
  "String prepended to each item in the sidebar.

This can be useful both for visual presentation and for folding with
`origami-mode'."
  :type 'string
  :group 'corefighter)

(defcustom corefighter-cycle-modules t
  "Visit the first item of all modules if there is no module left.

This affects the behaviors of `corefighter-next-item' and
`corefighter-next-module'.  If there is no item in succeeding modules
and this value is non-nil, visit the first item in all modues."
  :type 'boolean
  :group 'corefighter)

;;;;; Variables for the sidebar
(defconst corefighter-sidebar-buffer "*corefighter sidebar*")

(defcustom corefighter-sidebar-min-width 0
  "Minimum width of the sidebar."
  :group 'corefighter
  :type 'integer)

(defcustom corefighter-sidebar-max-width 40
  "Maximum width of the sidebar."
  :group 'corefighter
  :type 'integer)

(defcustom corefighter-sidebar-side 'left
  "Side to display the sidebar."
  :group 'corefighter
  :type '(choice (const left)
                 (const right)))

(defcustom corefighter-sidebar-section-separator "\n\f\n\n"
  "String used as a separator between sections."
  :group 'corefighter
  :type 'string)

(defvar corefighter-sidebar-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" 'corefighter-sidebar-refresh)
    (define-key map "q" 'quit-window)
    (define-key map "n" 'corefighter-sidebar-next-link)
    (define-key map "p" 'corefighter-sidebar-previous-link)
    (define-key map (kbd "TAB") 'corefighter-sidebar-next-section)
    (define-key map (kbd "<backtab>") 'corefighter-sidebar-previous-section)
    (define-key map (kbd "SPC") 'corefighter-sidebar-preview)
    (define-key map (kbd "RET") 'corefighter-sidebar-follow-link)
    (define-key map [follow-link] 'corefighter-sidebar-follow-link)
    (define-key map [down-mouse-1] (lambda (event)
                                     (interactive "e")
                                     (mouse-set-point event)
                                     (corefighter-sidebar-follow-link)))
    map))

;;;;;; Faces
(defface corefighter-sidebar-section-title
  '((t (:weight bold)))
  "Face for section titles."
  :group 'corefighter)

;;;; Classes
;;;;; Items
(cl-defstruct corefighter-item
  ;; String (required)
  title
  ;; A short description string
  description
  ;; Key (string) to access the item
  key
  ;; S-expression (not lambda but a list)
  action
  ;; Symbol or nil. See `corefighter--run-action-1'
  action-window
  ;; Emacs encoded time
  due
  ;; Urgency (not implemented)
  urgency)

;;;;; Modules
(defclass corefighter-module ()
  ((title :initarg :title
          :type string
          :documentation "Title of the module."))
  "Abstract class of module.")

(cl-defgeneric corefighter-module-items ((obj corefighter-module)
                                         &optional refresh)
  "Retrieve items of a module.")

(cl-defgeneric corefighter-module-urgent-p ((obj corefighter-module))
  "t if the module is urgent."
  (-find #'corefighter-item-urgency (corefighter-module-items obj)))

;;;; Loading modules
(defun corefighter--find-instance (class)
  "Find a module instance of CLASS."
  (cl-loop for obj in corefighter-module-instances
           when (same-class-p obj class)
           return obj))

;;;###autoload
(defun corefighter-load-modules ()
  "Reload modules based on the configuration in `corefighter-modules'."
  (interactive)
  (let (new)
    (condition-case err
        (cl-loop for (class-symbol . options) in corefighter-modules
                 do (push (apply #'make-instance class-symbol options) new)
                 ;; TODO: Use cache
                 ;; (if-let ((obj (corefighter--find-instance class-symbol options)))
                 ;;     (push obj new)
                 ;;   (push (apply #'make-instance class-symbol options) new))
                 )
      (error (message "Error while loading a module: %s" err)))
    (setq corefighter-module-instances (nreverse new))
    (setq corefighter-last-data nil
          corefighter-last-item nil)))

;;;; Administration commands
;;;###autoload
(defun corefighter-describe-module (class)
  (interactive
   (list (completing-read "Modules: "
                          (eieio-class-children 'corefighter-module))))
  (describe-function (intern class)))

;;;; Navigation commands
;; These commands lets you to visit items without opening the sidebar.
;;;###autoload
(defun corefighter-next-item ()
  "Go to the next item of the last visited item.

If there is no item visited, visit the first item."
  (interactive)
  (if-let ((cursor (or (corefighter--next-item)
                       (and corefighter-cycle-modules
                            (progn
                              (message "No item remaining. Cycling to the first module...")
                              (corefighter--first-item))))))
      (corefighter--run-cursor cursor)
    (message "No remaining item")))

(defun corefighter--run-cursor (cursor)
  "Run the action of an item in CURSOR normally."
  (let* ((item (corefighter-cursor-item cursor))
         (window (corefighter-item-action-window item))
         (action `(lambda () ,(corefighter-item-action item))))
    (corefighter--run-action-1 action window cursor)))

(cl-defun corefighter--next-item (&key (allow-other-module t)
                                       (allow-first-item t))
  "Get a cursor to the next item based on the stored cursor."
  (cond
   (corefighter-last-item
    (or (corefighter--next-item-same-module)
        (and allow-other-module
             (corefighter--next-module-item))))
   (allow-first-item
    (corefighter--first-item))))

(defun corefighter--next-item-same-module ()
  "Get the next item in the same module."
  (unless corefighter-last-item
    (error "No last item"))
  (let* ((cursor corefighter-last-item)
         (last-item (corefighter-cursor-item cursor))
         (module-cursor (corefighter-cursor-module-cursor cursor))
         (module (corefighter--module-by-cursor module-cursor))
         (new-items (corefighter-module-items module))
         (key (corefighter-item-key last-item))
         (new-index (if-let ((index (or (and key
                                             (-find-index
                                              (lambda (item)
                                                (equal key
                                                       (corefighter-item-key item)))
                                              new-items))
                                        (-find-index
                                         (lambda (item)
                                           (corefighter--compare-items item last-item))
                                         new-items))))
                        (1+ index)
                      (corefighter-cursor-index cursor)))
         (new-item (nth new-index new-items)))
    (when new-item
      (make-corefighter-cursor :module-cursor module-cursor
                               :item new-item
                               :index new-index))))

(defun corefighter--module-by-cursor (module-cursor)
  "Get a module instance pointed by MODULE-CURSOR."
  (-find (lambda (module)
           (corefighter--test-module-cursor module-cursor module))
         corefighter-module-instances))

(defun corefighter--next-module-item ()
  "Get the first item in the next module."
  (let ((modules (corefighter--next-modules))
        module)
    (catch 'return
      (while (setq module (pop modules))
        (when-let ((item (corefighter--module-first-item module)))
          (throw 'return item))))))

(defun corefighter--module-first-item (module)
  "Return the first item in MODULE as a cursor."
  (when-let ((items (corefighter-module-items module)))
    (make-corefighter-cursor
     :module-cursor
     (corefighter--module-cursor module)
     :item (car items)
     :index 0)))

(defun corefighter--next-modules (&optional module-cursor)
  "Return module instances after the module by MODULE-CURSOR."
  (let ((module-cursor (or module-cursor
                           (corefighter-cursor-module-cursor corefighter-last-item))))
    (cdr (cl-member-if (lambda (module)
                         (corefighter--test-module-cursor module-cursor module))
                       corefighter-module-instances))))

(defun corefighter--first-item ()
  "Get the first item in all modules."
  ;; FIXME: Implement
  (let ((modules (cl-copy-list corefighter-module-instances))
        module)
    (catch 'return
      (while (setq module (pop modules))
        (when-let ((item (corefighter--module-first-item module)))
          (throw 'return item))))))

;;;###autoload
(defun corefighter-next-module ()
  "Visit the first item in the next module of the last visited item."
  (interactive)
  (if-let ((cursor (or (corefighter--next-module-item)
                       (if corefighter-cycle-modules
                           (progn
                             (message "No item remaining. Cycling to the first module...")
                             (corefighter--first-item))))))
      (corefighter--run-cursor cursor)
    (message "No item in the following modules")))

;;;###autoload
(defun corefighter-first-item ()
  "Visit the first item in all modules."
  (interactive)
  (if-let ((cursor (corefighter--first-item)))
      (corefighter--run-cursor cursor)
    (message "No item in the following modules")))

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

(defun corefighter--compare-items (item1 item2)
  "Compare two objects of `corefighter-item' type."
  ;; Use the action to compare two items
  (equal (corefighter-item-action item1)
         (corefighter-item-action item2)))

;;;;; Time
(cl-defgeneric corefighter-encode-time (time &optional date-only))

(cl-defmethod corefighter-encode-time ((time float)
                                       &optional date-only)
  (make-corefighter-time :seconds time :date-only date-only))

(cl-defmethod corefighter-encode-time ((time list)
                                       &optional  date-only)
  (corefighter-encode-time (float-time time) date-only))

(cl-defmethod corefighter-compare-times ((t1 corefighter-time)
                                         (t2 corefighter-time))
  (< (corefighter-time-seconds t1) (corefighter-time-seconds t2)))

(cl-defgeneric corefighter-format-time (time &optional fmt)
  "Format TIME with FMT.")

(cl-defmethod corefighter-format-time ((time corefighter-time)
                                       &optional fmt)
  (format-time-string (cond
                       (fmt fmt)
                       ((corefighter-time-date-only time) "%F")
                       (t "%F %R"))
                      (corefighter-time-seconds time)))

;;;;; Utilities for due time
(cl-defgeneric corefighter--due (item))

(cl-defmethod corefighter--due ((item corefighter-item))
  (corefighter-item-due item))

(cl-defmethod corefighter--due ((cursor corefighter-cursor))
  (corefighter-item-due (corefighter-cursor-item cursor)))

(cl-defgeneric corefighter-due-before (item threshold))

(cl-defmethod corefighter-due-before ((item corefighter-item)
                                      (threshold float))
  (< (corefighter-time-seconds (corefighter-item-due item)) threshold))

(cl-defmethod corefighter-due-before ((item corefighter-cursor)
                                      (threshold float))
  (corefighter-due-before (corefighter-cursor-item item) threshold))

(cl-defgeneric corefighter-due-diff-days (item))

(cl-defmethod corefighter-due-diff-days ((item corefighter-item))
  (/ (- (corefighter-time-seconds (corefighter-item-due item))
        (corefighter--midnight 0))
     86400))

;;;;; Cursors
(defun corefighter--module-cursor (module)
  "Return the module cursor to a MODULE instance.

MODULE must be an instance of `corefighter-module'.
 `corefighter-module-cursor' to the object."
  (make-corefighter-module-cursor
   :title (oref module title)
   :class (eieio-object-class-name module)
   :slots (cl-loop for slot in (eieio-class-slots (eieio-object-class module))
                   collect (cons slot (slot-value module
                                                  (eieio-slot-descriptor-name slot))))))

(defun corefighter--test-module-cursor (module-cursor module)
  "Test if MODULE-CURSOR points to MODULE."
  (corefighter--module-cursor-equal module-cursor
                                    (corefighter--module-cursor module)))

(cl-defmethod corefighter--module-cursor-equal ((m1 corefighter-module-cursor)
                                                (m2 corefighter-module-cursor))
  (and (eq (corefighter-module-cursor-class m1)
           (corefighter-module-cursor-class m2))
       (equal (corefighter-module-cursor-slots m1)
              (corefighter-module-cursor-slots m2))))

;;;;; Utilities for agenda
(defun corefighter--agenda-scope ()
  "Return the float time corresponding to `corefighter-agenda-scope."
  (pcase corefighter-agenda-scope
    (`day (corefighter--midnight 1))
    ((and n
          (guard (numberp n)))
     (corefighter--midnight n))))

(defun corefighter--agenda-from (data)
  "Build an agenda from DATA.

DATA must be a list that has the same structure as
`corefighter-last-data'.

The result is a list of cursors sorted by due."
  (->> data
       (--map (let-alist it
                (let ((end (corefighter--agenda-scope)))
                  (cl-loop for item being the elements of .items using (index index)
                           when (let ((due (corefighter-item-due item)))
                                  (and due
                                       (< (corefighter-time-seconds due) end)))
                           collect (make-corefighter-cursor
                                    :module-cursor .module
                                    :item item
                                    :index index)))))
       (-flatten-n 1)
       (-sort (-on #'corefighter-compare-times
                   (-compose #'corefighter-item-due #'corefighter-cursor-item)))))

(cl-defsubst corefighter--agenda ()
  "Build an agenda from the last data.

This function returns a list of cursors from `corefighter-last-data'."
  (corefighter--agenda-from corefighter-last-data))

(defun corefighter--midnight (day)
  "Return the float time of the midnight after DAY days."
  (let ((now (decode-time (current-time))))
    (setf (nth 0 now) 0
          (nth 1 now) 0
          (nth 2 now) 0)
    (+ (float-time (apply #'encode-time now))
       (* day 86400))))

(defun corefighter--partition-agenda ()
  "Return a grouped agenda."
  (-let* ((last-midnight (corefighter--midnight 0))
          (next-midnight (corefighter--midnight 1))
          ((overdue-items non-overdue) (--split-with
                                        (corefighter-due-before it last-midnight)
                                        (corefighter--agenda)))
          ((today future) (--split-with
                           (corefighter-due-before it next-midnight)
                           non-overdue))
          ((scheduled unscheduled) (--split-with
                                    (corefighter-time-date-only (corefighter--due it))
                                    today)))
    `((overdue . ,overdue-items)
      (unscheduled . ,unscheduled)
      (scheduled . ,scheduled)
      (future . ,future))))

;;;; Sidebar

;;;;; Entry point
;;;###autoload
(defun corefighter-sidebar ()
  "Open a sidebar window displaying a dashboard."
  (interactive)
  (let ((window (car (cl-remove-if-not
                      (lambda (w)
                        (equal corefighter-sidebar-buffer
                               (buffer-name (window-buffer w))))
                      (window-list)))))
    (if window
        (progn
          (select-window window)
          (corefighter-sidebar-refresh))
      (let ((buf (corefighter-sidebar--init)))
        (pop-to-buffer buf
                       `(display-buffer-in-side-window
                         . ((side . ,corefighter-sidebar-side)
                            (slot . -1))))
        (corefighter-sidebar-set-width (with-current-buffer buf
                                         corefighter-sidebar-width))
        (set-window-dedicated-p (corefighter-sidebar--window) t)))))

;;;;; Mode
(define-derived-mode corefighter-sidebar-mode special-mode "CoreFighter"
  "Major mode for a dashboard by Core Fighter.
\\<corefighter-sidebar-mode-map>
"
  :group 'corefighter
  :syntax-table nil
  :abbrev-table nil
  (setq buffer-read-only t
        truncate-lines t
        cursor-in-non-selected-windows nil)
  (page-break-lines-mode 1)
  (setq-local show-help-function #'corefighter-sidebar--show-help)
  (cursor-sensor-mode 1)
  (use-local-map corefighter-sidebar-map))

(defun corefighter-sidebar--init (&optional force)
  "Initialize the content of `corefighter-sidebar-buffer' and return the buffer.

When FORCE is non-nil, force reloading items."
  (with-current-buffer (get-buffer-create corefighter-sidebar-buffer)
    (let ((inhibit-read-only t))
      (kill-all-local-variables)
      (erase-buffer)
      (remove-overlays)
      (corefighter-sidebar-mode)
      (let ((data (corefighter--get-data force))
            (width (or corefighter-sidebar-min-width 0))
            section-begin)
        (dolist (section-data data)
          (let-alist section-data
            (unless (and corefighter-hide-empty-sections
                         (null .items))
              (insert corefighter-sidebar-section-separator)
              (setq section-begin (point))
              (insert (propertize .title
                                  'face 'corefighter-sidebar-section-title)
                      "\n\n")
              (setq width (max width (length .title)))
              (cl-loop for item being the elements of .items using (index index)
                       do (let ((text (concat
                                       (or corefighter-sidebar-item-prefix "")
                                       (if (corefighter-item-urgency item)
                                           corefighter-urgency-text
                                         "")
                                       (if-let ((key (corefighter-item-key item)))
                                           (format "[%s] " (propertize key 'face 'bold))
                                         "")
                                       (corefighter-item-title item)))
                                (cursor (make-corefighter-cursor :module-cursor .module
                                                                 :item item
                                                                 :index index)))
                            (setq width (max width (length text)))
                            (insert (propertize text
                                                'button t
                                                'follow-link t
                                                'mouse-face 'highlight
                                                'help-echo (corefighter-item-description item)
                                                'action #'corefighter-sidebar-follow-link
                                                'cursor-sensor-functions '(corefighter-sidebar--item-sensor)
                                                'corefighter-item-cursor cursor
                                                'corefighter-item item)
                                    "\n")))
              (ov-set (ov-make section-begin (point))
                      ;; TODO: Add a section-local keymap to access items
                      'corefighter-module .class))))
        (setq corefighter-sidebar-width
              (min width corefighter-sidebar-max-width))))
    (goto-char (point-min))
    (current-buffer)))

(defun corefighter--get-data (&optional refresh)
  "Retrieve data of the current modules.

If REFRESH is non-nil, force refreshing all modules."
  (setq corefighter-last-data
        (mapcar (lambda (module)
                  `((class . ,(eieio-object-class module))
                    (title . ,(oref module title))
                    (module . ,(corefighter--module-cursor module))
                    (items . ,(corefighter-module-items module refresh))))
                corefighter-module-instances)))

;;;;; Commands in the buffer
(defun corefighter-sidebar-refresh (&optional arg)
  "Refresh the content of the current dashboard buffer.

If ARG is non-nil, force reloading items of each module."
  (interactive "P")
  (unless (get-buffer corefighter-sidebar-buffer)
    (user-error "There is no sidebar buffer"))
  (message "Refreshing the sidebar...")
  (corefighter-sidebar--init arg)
  (message nil))

(defun corefighter--run-action-1 (action &optional action-window cursor)
  "Run ACTION in ACTION-WINDOW with CURSOR.

ACTION-WINDOW should denote how the action manages the window.

Because some commands (e.g. org-agenda) take care of windows by
themselves, this workaround related to window management is needed.

+ If the action displays a buffer in other-window, the value should be
  \"other-window\".

+ Otherwise, action should switch to a buffer in the current window.

CURSOR is an instance of `corefighter-cursor' type.  If it is
specified, it is stored in `corefighter-last-item' variable so that
`corefighter-next-item' command works.

ACTION is returned.
"
  ;; TODO: Support magit properly. If `corefighter-target-window-setup'
  ;; is 'only, the option doesn't take effect.
  (pcase action-window
    ('other-window (corefighter--prepare-other-window))
    (_ (corefighter--prepare-target-window)))
  (funcall action)
  (setq corefighter-last-item cursor)
  (when cursor
    (push cursor corefighter-item-history))
  ;; Return non-nil for use in commands like `corefighter-sidebar-preview'
  action)

(defun corefighter--prepare-other-window ()
  "Prepare the window for switching the buffer in other-window."
  (cl-case corefighter-target-window-setup
    (only (let ((orig-window (selected-window)))
            (mapc #'delete-window
                  (cdr (corefighter--other-windows)))
            ;; It seems that the window focus is changed
            ;; by delete-window, so re-select the original
            ;; window.
            (select-window orig-window)))))

(defun corefighter-sidebar-follow-link (&optional pos)
  "Follow a link at POS."
  (interactive)
  (when-let
      ((pos (or pos (point)))
       (cursor (get-char-property pos 'corefighter-item-cursor))
       (item (corefighter-cursor-item cursor))
       (action `(lambda () ,(corefighter-item-action item))))
    (corefighter--run-action-1 action
                               (corefighter-item-action-window item)
                               cursor)))

(defun corefighter-sidebar-preview ()
  "Preview a link at the position."
  (interactive)
  (let ((orig-window (selected-window)))
    ;; Check if an item under the cursor has an action
    (when (corefighter-sidebar-follow-link)
      (select-window orig-window))))

(defun corefighter-sidebar--goto-next-ov (prop)
  "Jump to the next non-nil occurrence of PROP."
  (when-let ((pos (thread-last (ov-in prop)
                    (mapcar #'ov-beg)
                    (-find (lambda (pos) (> pos (point)))))))
    (goto-char pos)))

(defun corefighter-sidebar--goto-previous-ov (prop)
  "Jump to the previous non-nil occurrence of PROP."
  (when-let ((ov (thread-last (ov-in prop)
                   (nreverse)
                   (-find (lambda (ov) (< (ov-end ov) (point)))))))
    (goto-char (ov-beg ov))))

(defun corefighter-sidebar--goto-next-prop (prop)
  "Jump to the next non-nil occurrence of PROP."
  (let ((start (point))
        pos)
    (catch 'loop
      (while (setq pos (next-single-property-change start prop))
        (when (get-char-property pos prop)
          (throw 'loop pos))
        (setq start pos)))
    (when pos (goto-char pos))))

(defun corefighter-sidebar--goto-previous-prop (prop)
  "Jump to the previous non-nil occurrence of PROP."
  (let ((start (if (get-char-property (point) prop)
                   (previous-single-property-change (point) prop)
                 (point)))
        pos)
    (when start
      (catch 'loop
        (while (setq pos (previous-single-property-change start prop))
          (when (get-char-property pos prop)
            (throw 'loop pos))
          (setq start pos))))
    (when pos (goto-char pos))))

(defun corefighter-sidebar-next-link ()
  "Jump to the next link in the buffer."
  (interactive)
  (corefighter-sidebar--goto-next-prop 'button))

(defun corefighter-sidebar-previous-link ()
  "Jump to the previous link in the buffer."
  (interactive)
  (corefighter-sidebar--goto-previous-prop 'button))

(defun corefighter-sidebar-next-section ()
  "Jump to the next section."
  (interactive)
  (corefighter-sidebar--goto-next-ov 'corefighter-module))

(defun corefighter-sidebar-previous-section ()
  "Jump to the previous section."
  (interactive)
  (corefighter-sidebar--goto-previous-ov 'corefighter-module))

(defun corefighter-sidebar--item-sensor (_window _oldpos dir)
  "Cursor sensor function items in the sidebar."
  (when (eq dir 'entered)
    (when-let ((item (get-char-property (point) 'corefighter-item))
               (description (corefighter-item-description item)))
      (corefighter-sidebar--show-help description))))

(defun corefighter-sidebar--show-help (msg)
  (let ((message-log-max))
    (message msg)))

;;;;; Sidebar window
(defun corefighter-sidebar--window ()
  "Get the sidebar window if any."
  (get-buffer-window corefighter-sidebar-buffer))

(defun corefighter-sidebar-set-width (width)
  "Set the width of the sidebar to WIDTH.

Copied from `dired-sidebar-set-width', which was originally copied from
`treemacs--set-width'."
  (with-selected-window (corefighter-sidebar--window)
    (unless (one-window-p)
      (let ((window-size-fixed)
            (w (max width window-min-width)))
        (cond
         ((> (window-width) w)
          (shrink-window-horizontally (- (window-width) w)))
         ((< (window-width) w)
          (enlarge-window-horizontally (- w (window-width)))))))))

;;;; Preconfigured modules
;;;;; org-agenda
(defclass corefighter-org-agenda (corefighter-module)
  ((title :initform "Org Agenda")))

(eval-when-compile
  (defvar org-agenda-custom-commands))

(cl-defmethod corefighter-module-items ((_obj corefighter-org-agenda)
                                        &optional _refresh)
  (require 'org-agenda)
  (cl-loop for (key description . options) in org-agenda-custom-commands
           collect (make-corefighter-item
                    :title description
                    :key key
                    :description (if (listp (car options))
                                     "block agenda"
                                   (format "%s %s"
                                           (symbol-name (car options))
                                           (nth 1 options)))
                    :urgency nil
                    :action `(let ((org-agenda-window-setup 'current-window))
                               (org-agenda nil ,key)))))

(provide 'corefighter)
;;; corefighter.el ends here
