;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               slowatch.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Implements a slow watch view.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-05-11 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2015 - 2015
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

(defpackage "COM.INFORMATIMAGO.SLOWATCH"
  (:use "CL" "UI")
  (:export
   "SLOWATCH" "SHOW-TIMEZONE" "CURRENT-HOUR" "TIMEZONE"
   "SLOWIN"
   "*SLOWIN*" "MAIN"))
(in-package "COM.INFORMATIMAGO.SLOWATCH")

(defclass slowatch (simple-view)
  ((show-timezone :initarg :show-timezone :initform t :accessor show-timezone)
   (update-period :initform nil :reader update-period)))

(defmethod timezone ((view slowatch))
  (uiop:getenv "TZ"))

(defmethod current-hour ((view slowatch))
  (multiple-value-bind (s m h) (decode-universal-time (get-universal-time))
    (+ h (/ (+ m (/ s 60)) 60))))

(defmethod hand-radius ((view slowatch))
  (let* ((bounds (view-bounds view))
         (size   (min (rect-height bounds) (rect-width bounds)))
         (r      (truncate size 2)))
    (- r 40)))

(defmethod update-period ((view slowatch))
  (or (slot-value view 'update-period)
      (setf (slot-value view 'update-period)
            (truncate (* 24 60 60) (* 2 pi (hand-radius view))))))

(defmethod set-view-size :after ((view slowatch) h &optional v)
  (declare (ignore h v))
  (setf (slot-value view 'update-period) nil))

(defmethod view-draw-contents ((view slowatch))
  (with-focused-view view
    (with-fore-and-back-color (get-fore-color (view-window view)) (get-back-color (view-window view))
      (let* ((bounds (view-bounds view))
             (size   (min (rect-height bounds) (rect-width bounds)))
             (x      (+ (rect-left bounds) (truncate (- (rect-width  bounds) size) 2)))
             (y      (+ (rect-top  bounds) (truncate (- (rect-height bounds) size) 2)))
             (r      (truncate size 2))
             (cx     (+ x r))
             (cy     (+ y r)))
        (erase-rect*  x y size size)
        (draw-ellipse x y size size)
        (flet ((x (r a) (+ cx (* r (cos a))))
               (y (r a) (+ cy (* r (sin a))))
               (a (h)   (+ (/ pi 2) (* 2/24 pi h))))
          (let ((ir (- r 30))
                (er (- r 20))
                (dr (- r 10)))
            (with-pen-state (:size #@(2 2))
              (loop
                :for h :from 0 :to 23
                :for a = (a h)
                :do (draw-line (x ir a) (y ir a) (x er a) (y er a))
                    (draw-string (- (x dr a) 5) (+ (y dr a) 5) (format nil "~D" h))))
            (loop
              :for h :from 0 :to 23
              :do (loop
                    :for q :from 1 :to 3 ; 0 1 2 3 4
                    :for a = (a (+ h (/ q 4)))
                    :do (draw-line (x ir a) (y ir a) (x er a) (y er a)))))
          (let ((h  (current-hour view)))
            (let* ((a  (a h))
                   (ir 4)
                   (er (hand-radius view)))
              (draw-ellipse cx cy ir ir)
              (draw-line (x ir a) (y ir a) (x er a) (y er a)))
            (when (show-timezone view)
              (let* ((tz (timezone view))
                     (w  (string-width tz)))
                (if (or (< h 6) (< 18 h))
                    ;; draw above
                    (draw-string (- cx (truncate w 2))
                                 (- cy 4)
                                 tz)
                    ;; draw below
                    (draw-string (- cx (truncate w 2))
                                 (+ cy 16)
                                 tz))))))))))

(defclass slowin (window)
  ((bit         :initform nil :accessor %slowin-bit)
   (update-time :initform nil :accessor %update-time)))

(defmethod set-view-size :after ((view slowin) h &optional v)
  (dovector (subview (view-subviews view))
    (set-view-size subview h v))
  (view-draw-contents view))

;; TODO: the redrawing updates occur to often with window-null-event-handler.
(defmethod window-null-event-handler ((view slowin))
  (let* ((now         (get-universal-time))
         (watch       (aref (view-subviews view) 0))
         (update-time (or (%update-time view)
                          (setf (%update-time view) now))))
    (when (<= update-time now)
      (setf (%update-time view) (+ now (update-period watch)))
      (view-draw-contents watch)
      (with-focused-view view
        (if (setf (%slowin-bit view) (not (%slowin-bit view)))
            (fill-rect*  0 0 2 2)
            (erase-rect* 0 0 2 2))))))

(defvar *slowin* nil)

(defun main ()
  (setf *slowin* (make-instance
                  'slowin
                  :window-title "SloWatch"
                  :view-size #@(200 200)
                  :view-subviews (vector (make-instance
                                          'slowatch
                                          :view-position #@(0 0)
                                          :view-size #@(200 200)))))
  (setf (slot-value *slowin* 'ui::back-color) *light-gray-color*
        (slot-value *slowin* 'ui::fore-color) *dark-gray-color*)
  *slowin*)

;;;; THE END ;;;;
