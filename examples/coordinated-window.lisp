;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               coordinated-window.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     MCLGUI
;;;;DESCRIPTION
;;;;
;;;;    This is a window that displays a box with the coordinates of the mouse.
;;;;    Useful while debugging
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2014-12-26 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2014 - 2015
;;;;
;;;;    This program is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU Affero General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;
;;;;    This program is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU Affero General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU Affero General Public License
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************
(defpackage "MCLGUI.EXAMPLE.COORDINATES-WINDOW"
  (:use "COMMON-LISP" "MCLGUI")
  (:export "COORDINATES-WINDOW"
           "RUN"))
(in-package "MCLGUI.EXAMPLE.COORDINATES-WINDOW")
(enable-sharp-at-reader-macro)

(defgeneric draw-coordinates (view))
(defgeneric erase-coordinates (view))
(defgeneric update-coordinates-size (view))
(defgeneric update-coordinates (view coordinates))

(defclass coordinates-view (simple-view)
  ((ascent :initform 0 :accessor coordinates-ascent)
   (text   :initform "" :reader coordinates-text)
   (coordinates :initform 0 :reader coordinates-view-coordinates)
   (filter :initform nil :accessor coordinates-filter))
  (:default-initargs
   :view-font '("Monaco" 12 :plain)
   :view-position #@(10 13)
   :view-size #@(100 14)))

(defmethod initialize-instance :after ((view coordinates-view) &key &allow-other-keys)
  (update-coordinates-size view))

(defmethod set-view-font-codes :after ((view coordinates-view) ff ms &optional ff-mask ms-mask)
  (declare (ignorable ff ms ff-mask ms-mask))
  (update-coordinates-size view))

(defmethod update-coordinates-size ((view coordinates-view))
  (with-font-focused-view view
    (let* ((w (string-width " x=0000 y=0000 "))
           (h (font-line-height)))
      (set-view-size view (make-point w h))
      (setf (coordinates-ascent view) (font-info)))))


(defmethod update-coordinates ((view coordinates-view) coordinates)
  (multiple-value-bind (x y) (if (coordinates-filter view)
                                 (funcall (coordinates-filter view)
                                          (point-h coordinates) (point-v coordinates))
                                 (values (point-h coordinates) (point-v coordinates)))
    (let ((text (format nil " x=~4D y=~4D " x y)))
      (setf (slot-value view 'text) text
            (slot-value view 'coordinates) coordinates))))

(defmethod erase-coordinates ((view coordinates-view))
  (with-font-focused-view view
    (let* ((bounds (view-bounds view))
           (x (rect-left   bounds))
           (y (rect-top    bounds))
           (w (rect-width  bounds))
           (h (rect-height bounds)))
      (erase-rect* (- x 2) (- y 2) (+ w 4) (+ h 4)))))

(defmethod draw-coordinates ((view coordinates-view))
  (with-focused-view view
    (let* ((text (coordinates-text view))
           (bounds (view-bounds view))
           (x (rect-left   bounds))
           (y (rect-top    bounds))
           (w (rect-width  bounds))
           (h (rect-height bounds)))
      (erase-rect* (- x 2) (- y 2) (+ w 4) (+ h 4))
      (draw-rect*  (- x 2) (- y 2) (+ w 4) (+ h 4))
      (draw-string x (+ y (coordinates-ascent view)) text))))

(defmethod view-draw-contents ((view coordinates-view))
  (draw-coordinates view))

(defmethod view-corners ((view coordinates-view))
  ;; return (values topleft bottomright)
  (multiple-value-call (function inset-corners) #@(-2 -2) (call-next-method)))


(defclass coordinated-window (window)
  ((coordinates-view :initarg :coordinates-position
                     :accessor coordinates-view))
  (:default-initargs :window-title "Coordinated Window"
                     :view-size #@(400 300)))

(defmethod initialize-instance :after ((window coordinated-window) &key &allow-other-keys)
  (setf (coordinates-view window) (make-instance 'coordinates-view
                                                 :view-container window))
  (update-coordinates-size (coordinates-view window)))

(defmethod set-view-font-codes :after ((window coordinated-window) ff ms &optional ff-mask ms-mask)
  (declare (ignorable ff ms ff-mask ms-mask))
  (when (slot-boundp window 'coordinates-view)
    (update-coordinates-size (coordinates-view window))))

(defmethod window-null-event-handler ((window coordinated-window))
  (call-next-method)
  (when (slot-boundp window 'coordinates-view)
    (let ((where (get-mouse))
          (cview (coordinates-view window)))
      (unless (= where (coordinates-view-coordinates cview))
        (update-coordinates cview where)
        (view-draw-contents cview)))))

(defmethod view-double-click-event-handler ((window coordinated-window) where)
  (when (slot-boundp window 'coordinates-view)
    (set-view-position  (coordinates-view window) where)
    (update-coordinates (coordinates-view window) where))
  (call-next-method)
  window)

(defmethod view-click-event-handler ((window coordinated-window) where)
  (when (slot-boundp window 'coordinates-view)
    (update-coordinates (coordinates-view window) where))
  (call-next-method)
  window)

(defmethod view-draw-contents ((window coordinated-window))
  (when (slot-boundp window 'coordinates-view)
    (update-coordinates (coordinates-view window) (get-mouse)))
  (call-next-method))


(defun run ()
  (initialize)
  (let ((win (make-instance 'coordinated-window)))
    win))


#-(and) (progn
          (class-of (front-window))
          (window-null-event-handler (front-window))
          (with-focused-view  (front-window)
            (view-draw-contents (coordinates-view (front-window))))
          (let ((v (coordinates-view (front-window))))
            (mapcar (lambda (f) (funcall f v))
                    '(coordinates-ascent
                      coordinates-text
                      coordinates-view-coordinates))))

;;;; THE END ;;;;
