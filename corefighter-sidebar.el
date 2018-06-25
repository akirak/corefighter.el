;;; corefighter-sidebar.el --- Sidebar-style dashboard for corefighter -*- lexical-binding: t -*-

;; Copyright (C) 2018 Akira Komamura

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

;; This is a sidebar user interface for corefighter.

;;; Code:

(require 'corefighter)
(require 'page-break-lines)

;;;; Variables
(defcustom corefighter-sidebar-max-width 40
  "Maximal width of the sidebar."
  :group 'corefighter
  :type 'integer)

(defcustom corefighter-sidebar-side 'left
  "Side to display the sidebar."
  :group 'corefighter
  :type '(choice (const left)
                 (const right)))

(defcustom corefighter-sidebar-section-separator "\n\f\n"
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

;;;; Faces
(defface corefighter-sidebar-section-title
  '((t (:weight bold)))
  "Face for section titles."
  :group 'corefighter
  :inherit 'variable-pitch)

;;;; Entry point
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
      (pop-to-buffer (corefighter-sidebar--init)
                     `(display-buffer-in-side-window
                       . ((side . ,corefighter-sidebar-side)
                          (slot . -1))))
      (corefighter-sidebar-set-width (corefighter-sidebar--width))
      (set-window-dedicated-p (corefighter-sidebar--window) t))))

;;;; Mode
(define-derived-mode corefighter-sidebar-mode special-mode "CoreFighter"
  "Major mode for a dashboard by Core Fighter.
\\<corefighter-sidebar-mode-map>
"
  :group 'corefighter
  :syntax-table nil
  :abbrev-table nil
  (setq buffer-read-only t
        truncate-lines t
        cursor-in-non-selected-window nil)
  (page-break-lines-mode 1)
  (use-local-map corefighter-sidebar-map))

(defun corefighter-sidebar--init ()
  "Initialize the content of `corefighter-sidebar-buffer' and return the buffer."
  (with-current-buffer (get-buffer-create corefighter-sidebar-buffer)
    (let ((inhibit-read-only t))
      (kill-all-local-variables)
      (erase-buffer)
      (remove-overlays)
      (corefighter-sidebar-mode)
      (let ((data (mapcar (lambda (module)
                            `((class . ,(eieio-object-class module))
                              (title . ,(oref module title))
                              (items . ,(corefighter-module-items module))))
                          corefighter-module-instances))
            section-begin)
        (dolist (section-data data)
          (insert corefighter-sidebar-section-separator)
          (let-alist section-data
            (setq section-begin (point))
            (insert (propertize .title
                                'face 'corefighter-sidebar-section-title)
                    "\n\n")
            (dolist (item .items)
              (insert (propertize (concat
                                   (if (corefighter-item-urgency item)
                                       corefighter-urgency-text
                                     "")
                                   (if-let ((key (corefighter-item-key item)))
                                       (format "[%s] " (propertize key 'face 'bold))
                                     "")
                                   (corefighter-item-title item))
                                  'button t
                                  'follow-link t
                                  'mouse-face 'highlight
                                  'help-echo (corefighter-item-description item)
                                  'action #'corefighter-sidebar-follow-link
                                  'action-window
                                  (corefighter-item-action-window item)
                                  'follow-action
                                  `(lambda ()
                                     ,(corefighter-item-action item)))
                      "\n"))
            (let ((section-end (point)))
              ;; TODO: Add a section-local keymap to access items
              (cl-loop for (prop . value) in `((corefighter-module . ,.class))
                       do (put-text-property section-begin section-end prop value)))
            (insert "\n")))))
    (goto-char (point-min))
    (current-buffer)))

;;;; Commands in the buffer
(defun corefighter-sidebar-refresh (&optional arg)
  "Refresh the content of the current dashboard buffer."
  (interactive "P")
  (unless (derived-mode-p 'corefighter-sidebar-mode)
    (user-error "Not in the mode"))
  (message "Refreshing the sidebar...")
  (corefighter-sidebar--init)
  (message nil))

(defun corefighter-sidebar--run-action (action &optional action-window)
  "Run ACTION with the window focus taken into an account."
  ;; Because some commands (e.g. org-agenda) take care of windows by themselves,
  ;; this workaround related to window management is needed.
  (pcase action-window
    ((and `(org-agenda-window-setup . ,window-setup)
          (guard (not (eq window-setup 'current-window))))
     (cl-case window-setup
       ;; ('current-window ) ; skipped by the parent condition
       ('other-window (cl-case corefighter-target-window-setup
                        ('only (let ((orig-window (selected-window)))
                                 (mapc #'delete-window
                                       (cdr (corefighter--other-windows)))
                                 ;; It seems that the window focus is changed
                                 ;; by delete-window, so re-select the original
                                 ;; window.
                                 (select-window orig-window)
                                 (funcall action)))
                        (otherwise (funcall action))))
       ;; TODO: Test if this works
       ('only-window (let ((org-agenda-window-setup 'reorganize-frame))
                       (funcall action)))
       ('reorganize-frame (funcall action))
       ('other-frame (funcall action))))
    (_
     (progn
       (corefighter--prepare-target-window)
       (funcall action)))))

(defun corefighter-sidebar-follow-link (&optional pos)
  "Follow a link at POS."
  (interactive)
  (when-let ((pos (or pos (point)))
             (action (get-char-property pos 'follow-action))
             (action-window (get-char-property pos 'action-window)))
    (corefighter-sidebar--run-action action action-window)
    action))

(defun corefighter-sidebar-preview ()
  "Preview a link at the position."
  (interactive)
  (let ((orig-window (selected-window)))
    (when (corefighter-sidebar-follow-link)
      (select-window orig-window))))

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
  (corefighter-sidebar--goto-next-prop 'corefighter-module))

(defun corefighter-sidebar-previous-section ()
  "Jump to the previous section."
  (interactive)
  (corefighter-sidebar--goto-previous-prop 'corefighter-module))

;;;; Sidebar window
(defun corefighter-sidebar--window ()
  "Get the sidebar window if any."
  (get-buffer-window corefighter-sidebar-buffer))

(defun corefighter-sidebar--width ()
  "Determine the width of the sidebar."
  ;; FIXME
  corefighter-sidebar-max-width)

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

(provide 'corefighter-sidebar)
;;; corefighter-sidebar.el ends here
