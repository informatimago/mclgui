;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               key.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Defines a window that handles key events by displaying in a
;;;;    static text dialog item the character and modifiers.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2014-10-07 <PJB> Created
;;;;BUGS
;;;;LEGAL
;;;;    GPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2014 - 2014
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

(defclass key-window (window)
  ((text-item :initform nil :initarg :text-item :accessor key-window-text-item)))

(defmethod initialize-instance :after ((window key-window) &key &allow-other-keys)
  (let ((text (make-instance 'static-text-dialog-item)))
    (setf (key-window-text-item window) text)
    (add-subviews window text)
    (window-size-parts window)))

(defmethod window-size-parts ((window key-window))
  (when (key-window-text-item window)
    (let ((bounds   (view-bounds window))
          (text     (key-window-text-item window)))
      (inset-rect bounds 4 4)
      (set-view-position text (rect-topleft bounds))
      (set-view-size text (rect-size bounds))
      (view-draw-contents window))))

(defun char-name* (char)
  (let ((entry (assoc (char-code char) ui::*ns-code-to-name*)))
    (if entry
        (string-downcase (cdr entry))
        (char-name char))))

(defmethod view-key-event-handler ((window key-window) char)
  (let ((text (key-window-text-item window)))
    (when text
      (setf (dialog-item-text text)
            (format nil "~A~%~A~%"
             (if (characterp char)
                 (format nil "Key: .~D. ~@[~S~] ~C" (char-code char) (char-name* char) char)
                 (format nil "Key: ~S" char))
             (when *current-event*
               (format nil "Modifiers: ~S"
                       (event-modifiers-label (event-modifiers *current-event*))))))
      (view-draw-contents text))))

(defun make-key-window ()
  (on-main-thread/sync (make-instance 'key-window
                                      :view-size #@(300 200)
                                      :window-title "Key Window")))

;;;; THE END ;;;;

