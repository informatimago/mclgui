;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               button-dialog-item.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Button Dialog Item.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2014-09-04 <PJB> Implemented.
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


(defclass button-dialog-item (default-button-mixin control-dialog-item)
  ((procid           :allocation :class
                     :initarg  :procid
                     :initform 0) ; #$PushButProc
   (width-correction :allocation :class
                     :initform 10))
  (:documentation "
This is the class used to make buttons.  Clicking a button usually has
an immediate result.  Buttons are generally given a function for
DIALOG-ITEM-ACTION-FUNCTION via the :dialog-item-action initialization
argument.
"))


(defmethod initialize-instance ((item button-dialog-item)
                                &key
                                  (border-p t)
                                  (view-font (view-default-font item)))
  (declare (ignorable view-font))
  (call-next-method)
  (unless border-p
    (setf (view-get item 'no-border) t)))



(defclass default-button-dialog-item (button-dialog-item)
  ()
  (:default-initargs
   :dialog-item-text "OK"
   :default-button t
   :cancel-button   nil)
  (:documentation "
Default buttons are a convenient subclass of button dialog items; they
serve as the default button. A dialog may have one default button. This
button has a bold border and usually may be selected by one of the
keystrokes Return or Enter.
"))




(defgeneric default-button-p (item)
  (:documentation "
The DEFAULT-BUTTON-P generic function returns true if item is the
default button in the view-window of ITEM.  Otherwise it returns NIL.
")
  (:method ((item default-button-mixin))
    (let ((window (view-window item)))
      (and window (eql item (default-button window))))))



(defmethod view-corners ((item button-dialog-item))
  (multiple-value-call (function inset-corners) #@(-1 -1) (call-next-method)))

 
(defmethod view-default-size ((button button-dialog-item))
  (let ((size (call-next-method)))
    (make-point (max 60 (+ 12 (point-h size)))
                (+ 4 (point-v size)))))


(defmethod view-default-font ((button button-dialog-item))
  (sys-font-spec))


(defmethod call-with-focused-dialog-item ((item button-dialog-item) fn &optional container)
  (call-next-method item fn (if (default-button-p item)
                                (view-window (or container item))
                                container)))


(defmethod dialog-item-disable ((item button-dialog-item))
  (view-put item 'was-enabled nil)
  (call-next-method))


(defgeneric press-button (button)
  (:documentation "
The press-button generic function highlights button, then calls the
dialog-item-action method for button.
")
  (:method ((button button-dialog-item))
    (hilite-control button 1)
    (sleep 0.05)
    (hilite-control button 0)
    (dialog-item-action button)))




(defmethod validate-control-dialog-item ((item button-dialog-item))
  ;; ROM redraw doesn't erase between the round-rect and the view borders
  (niy validate-control-dialog-item item)
  ;; (let ((wptr (wptr item))
  ;;       (container (view-container item))
  ;;       (handle (dialog-item-handle item)))
  ;;   (when (and wptr container)
  ;;     (with-focused-view (view-container item)
  ;;       (with-macptrs ((rgn (#_NewRgn))
  ;;                      (rgnSave (rref wptr :windowrecord.rgnSave)))
  ;;         (rset wptr :windowrecord.rgnSave (%null-ptr))
  ;;         (#_OpenRgn)
  ;;         (#_Draw1Control handle)
  ;;         (#_CloseRgn rgn)
  ;;         (rset wptr :windowrecord.rgnSave rgnSave)
  ;;         (validate-region container rgn)
  ;;         (#_DisposeRgn rgn)))))
  )



(defmethod invalidate-view ((item button-dialog-item) &optional erase-p)
  (if erase-p
    (call-next-method)
    (without-interrupts
      (call-next-method item t)
      (validate-control-dialog-item item)
      (call-next-method))))
            

(defmethod (setf dialog-item-enabled-p) (p (item button-dialog-item))
  (when (if (prog1 (dialog-item-enabled-p item)
              (call-next-method))
            (not p)
            p)
    (maybe-draw-default-button-outline item))
  p)


(defgeneric draw-default-button-outline (item)
  (:method ((item button-dialog-item))
    (when (installed-item-p item)
      (with-focused-dialog-item (item)
        (let* ((rect (view-frame item))
               (grayp (not (dialog-item-enabled-p item))))
          (inset-rect rect -4 -4)
          (with-slots (color-list) item
            (with-fore-color (or (getf color-list :frame nil) *light-blue-color*)
              (with-pen-state (:size #@(3 3)
                               :pattern (if grayp
                                            *gray-pattern*
                                            *black-pattern*))
                (draw-round-rect 16 16 rect)))))))))


(defun maybe-draw-default-button-outline (button)
  (let ((my-dialog (view-window button)))
    (when (and my-dialog
               (eql button (default-button my-dialog))
               (not (view-get button 'no-border)))
      (draw-default-button-outline button))))


(defmethod view-draw-contents ((item button-dialog-item))
  (let ((no-border (view-get item 'no-border)))
    (if no-border
        (let ((rect (view-frame item)))
          (erase-rect* (rect-left rect) (rect-top rect) (rect-width rect) (rect-height rect))
          (clip-inside-view item 2 2)
          (call-next-method))
        (with-focused-dialog-item (item)
          (let* ((frame (view-frame item))
                 (x (rect-left   frame))
                 (y (rect-top    frame))
                 (w (rect-width  frame))
                 (h (rect-height frame))
                 (grayp (not (dialog-item-enabled-p item)))
                 (state (control-hilite-state item)))
            (with-slots (color-list) item
              (with-fore-color (if (zerop state)
                                   (or (getf color-list :body  nil) *white-color*)
                                   (or (getf color-list :frame nil) *gray-color*))
                  (fill-round-rect* 12 12 x y w h))
              (with-fore-color (if (zerop state)
                                     (or (getf color-list :frame nil) *black-color*)
                                     (or (getf color-list :body  nil) *white-color*))
                (with-back-color (if (zerop state)
                                     (or (getf color-list :body  nil) *white-color*)
                                     (or (getf color-list :frame nil) *black-color*))
                  (with-pen-state (:size #@(1 1)
                                   :pattern (if grayp
                                                *gray-pattern*
                                                *black-pattern*))
                    (draw-round-rect*  12 12 x y w h)
                    (let ((text (dialog-item-text item)))
                      (draw-text (+ x (truncate  (- w (string-width text)) 2)) (+ y 2) w h text)))))))
          (maybe-draw-default-button-outline item)))))


;;;; THE END ;;;;

