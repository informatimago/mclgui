;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               radio-button-dialog-item.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Radio Button Dialog Item.
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


(defclass radio-button-dialog-item (control-dialog-item)
  ((width-correction      :allocation :class
                          :initform 20)
   (procid                :allocation :class
                          :initform 0 ; #.(+ #$RadioButProc #$kControlUsesOwningWindowsFontVariant)
                          )
   (radio-button-cluster  :initarg :radio-button-cluster
                          :initform 0
                          :accessor radio-button-cluster
                          :documentation "
The cluster to which the radio button belongs. Only one
button from a given cluster can be pushed at a time.
Whenever the user clicks a button, the function radiobutton-
unpush is applied to all other buttons having
the same value for radio-button-cluster. To check
to see whether two buttons are in the same cluster, use
eq. The default cluster is 0.
")
   (radio-button-pushed-p :initarg  :radio-button-pushed-p
                          :initform nil
                          :accessor radio-button-pushed-p
                          :documentation "
The radio-button-pushed-p generic function returns T if the radio
button is pushed and NIL if it is not.  The default value
is NIL.
"))
  (:documentation "
Radio buttons are small circles that contain a black dot when they are
selected (“pushed”). Radio buttons occur in clusters, and only one
button in a cluster may be pushed at a time. Clicking a radio button
unpushes the previously pushed one. The following class and functions
govern the behavior of radio buttons.
"))


(defmethod view-default-font ((view radio-button-dialog-item))
  (sys-font-spec))


(defmethod view-default-size ((item radio-button-dialog-item))
  (let ((size (call-next-method)))
    (make-point (point-h size) (max 16 (point-v size)))))


(defmethod install-view-in-window ((item radio-button-dialog-item) dialog
                                   &aux (first t))
  (declare (ignore dialog))
  (without-interrupts
      (let ((cluster (radio-button-cluster item))
            (container (view-container item)))
        (do-dialog-items (other-item container 'radio-button-dialog-item)
          (when (and (not (eql item other-item))
                     (eql cluster (radio-button-cluster other-item)))
            (return (setq first nil)))))
    (call-next-method) ;this is failing to do it upon return
    (when (or first (radio-button-pushed-p item))
      (radio-button-push item))))

(defconstant +radio-button-diameter+ 11 "Diameter of the radio button circle.")
(defconstant +radio-button-interval+  3 "Space between the circle and the text.")
(defconstant +radio-button-inset+     2 "Inset for the dot inside the circle.")

(defmethod view-draw-contents ((item radio-button-dialog-item))
  (with-focused-dialog-item (item)
    (let* ((frame  (view-frame item))
           (x      (rect-left   frame))
           (y      (rect-top    frame))
           (w      (rect-width  frame))
           (h      (rect-height frame))
           (grayp  (not (dialog-item-enabled-p item)))
           (state  (control-hilite-state item))
           (pushed (radio-button-pushed-p item)))

      ;; circle on the left of the label
      ;; state /= 0 => bold circle
      ;; pushed => dot in the circle
      (multiple-value-bind (fa fd fw fl) (font-info)
        (declare (ignore fw fl))
        (let* ((th (round (+ fa fd)))
               (ta (round fa))
               (cx x)
               (cy (if (<= h +radio-button-diameter+)
                       y
                       (+ y (truncate (- h +radio-button-diameter+) 2))))
               (cw (if (<= h +radio-button-diameter+)
                       h
                       +radio-button-diameter+))
               (ch (if (<= h +radio-button-diameter+)
                       h
                       +radio-button-diameter+))
               (tx (+ cx cw +radio-button-interval+))
               (ty (+ y (truncate (- h th) 2) ta)))
          (with-slots (color-list) item
            (with-fore-color (or (getf color-list :frame nil) *black-color*)
              (with-back-color (or (getf color-list :body  nil) *white-color*)
                ;; radio-button circle:
                (with-pen-state (:size    (if (zerop state)
                                              #@(1 1)
                                              #@(2 2))
                                 :pattern (if grayp
                                              *gray-pattern*
                                              *black-pattern*))
                  (draw-ellipse cx cy cw ch))
                (with-pen-state (:size    #@(1 1)
                                 :pattern (if grayp
                                              *gray-pattern*
                                              *black-pattern*))
                  (when pushed
                    ;; radio-button dot:
                    (fill-ellipse (+ cx +radio-button-inset+)
                                  (+ cy +radio-button-inset+)
                                  (- cw (* 2 +radio-button-inset+))
                                  (- ch (* 2 +radio-button-inset+))))
                  (let ((text (dialog-item-text item)))
                    #+debug-view (format-trace '(view-draw-contents radio-button-dialog-item) (dialog-item-text item))
                    ;; always left-aligned
                    (draw-text tx (- ty ta) (- w (- tx x)) th text)))))))))))


;; (defmethod view-click-event-handler ((item radio-button-dialog-item) where)
;; ;; TODO: implement
;; (when (dialog-item-enabled-p item)
;;   (setf (control-hilite-state item) 1)
;;   track push/unpush
;;   (setf (control-hilite-state item) 0))
;;   )

(defmethod dialog-item-action ((item radio-button-dialog-item))
  (radio-button-push item)
  (call-next-method))                   ; dispatch to user's dialog-item-action code.



(defgeneric pushed-radio-button (view &optional cluster)
  (:documentation "
The pushed-radio-button generic function returns the pushed radio
button from the specified cluster.  The value NIL is returned if there
is no such cluster or if all the radio buttons in a cluster are
disabled.

WINDOW:         A window.

CLUSTER:        The cluster of radio buttons to search. Radio button
                clusters are numbered, starting with 0. The default is
                0.
")
  (:method ((view view) &optional (cluster 0))
    (dovector (item (view-subviews view))
              (when (and (typep item 'radio-button-dialog-item)
                         (= cluster (slot-value item 'radio-button-cluster))
                         (radio-button-pushed-p item))
                (return-from pushed-radio-button item)))))


(defgeneric radio-button-push (item)
  (:documentation "
The radio-button-push generic function pushes a radio button and
unpushes the previously pushed one.  The function merely toggles the
states of the two radio buttons; it does not run any action. The function
returns NIL.

ITEM:           A radio-button dialog item.
")
  (:method ((item radio-button-dialog-item))
    (let ((cluster      (radio-button-cluster item))
          (container    (view-container item)))
      (when container
        (do-dialog-items (other-item container 'radio-button-dialog-item)
          (when (and (not (eql other-item item))
                     (eql (radio-button-cluster other-item) cluster))
            (radio-button-unpush other-item)))
        (setf (radio-button-pushed-p item) t)
        (when (installed-item-p item)
          (with-focused-dialog-item (item container)
            (view-draw-contents item)))))))


(defgeneric radio-button-unpush (item)
  (:documentation "
The radio-button-unpush generic function unpushes the radio button
and returns NIL.

ITEM:           A radio-button dialog item.
")
  (:method ((item radio-button-dialog-item))
    (let ((container    (view-container item)))
      (setf (radio-button-pushed-p item) nil)
      (when (installed-item-p item)
        (with-focused-dialog-item (item container)
          (view-draw-contents item))))))


;;;; THE END ;;;;
