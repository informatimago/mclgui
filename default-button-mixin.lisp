;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               default-button-mixin.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Default Button Mixin.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-05-19 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2012 - 2014
;;;;
;;;;    Some code extracted from MCL (LGPL):
;;;;    Copyright 1985-1988 Coral Software Corp.
;;;;    Copyright 1989-1994 Apple Computer, Inc.
;;;;    Copyright 1995-2000 Digitool, Inc.
;;;;
;;;;    This program is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;
;;;;    This program is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU General Public License
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************


(in-package "MCLGUI")


(defclass default-button-mixin ()
  ())

(defmacro %get-default-button (window)
  `(view-get ,window '%default-button))

(defgeneric button-props-to-window (item window)
  (:method ((item default-button-mixin) window)
    (cond ((view-get item 'default-button-p)
           (setf (%get-default-button window) item))
          ((view-get item 'cancel-button-p)
           (setf (%get-cancel-button window) item)))))


(defmethod initialize-instance :after ((item default-button-mixin) &key default-button cancel-button view-container)
  (when default-button
    (setf (view-get item 'default-button-p) t))
  (when cancel-button
    (setf (view-get item 'cancel-button-p) t)
    (when (not (dialog-item-action-function item))
      (setf (dialog-item-action-function item)
            (lambda (item) (declare (ignore item)) (return-from-modal-dialog :cancel)))))
  (when view-container
    (let ((window (view-window view-container)))
      (when (and window (or default-button cancel-button))
        (button-props-to-window item window)))))



(defmethod install-view-in-window :after ((item default-button-mixin) view)
  (let ((window (view-window view)))
    (when window
      (button-props-to-window item window))))

(defmethod remove-view-from-window :before ((item default-button-mixin))
  (let ((dialog (view-window item)))
    (when dialog
      (cond ((eql item (%get-default-button dialog))
             (setf (%get-default-button dialog) nil))
            ((eql item (%get-cancel-button dialog))
             (setf (%get-cancel-button dialog) nil))))))


(defgeneric default-button (window)
  (:documentation "
The DEFAULT-BUTTON generic function returns the current default
button, or NIL if the window has no default button.  The default button
is the button whose action is run when the user presses Return or
Enter. It is outlined with a heavy black border.

If carriage returns are allowed in the current editable-text item,
they are sent to that item rather than to the default button.
")
  (:method ((window window))
    (%get-default-button window)))


(defgeneric set-default-button (window new-button)
  (:documentation "

The SET-DEFAULT-BUTTON generic function changes the default button
according to the value of new-button and returns new-button.  If
carriage returns are allowed in the current editable-text item, they
are sent to that item rather than to the default button.

WINDOW:         A window.

NEW-BUTTON:     The button that should be made the default button, or
                NIL, indicating that there should be no default button.
")
  (:method ((dialog window) new-button)
    (let ((default-button (%get-default-button dialog)))
      (unless (eql default-button new-button)
        (when default-button
          (invalidate-view-border default-button t))
        (setf (%get-default-button dialog) new-button)
        (when new-button
          (invalidate-view-border new-button))))
    new-button))


;;;; THE END ;;;;
