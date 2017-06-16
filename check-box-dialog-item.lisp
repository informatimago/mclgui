;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               check-box-dialog-item.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Check box dialog item.
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


(defclass check-box-dialog-item (control-dialog-item)
  ((width-correction    :allocation :class
                        :initform    20)
   (procid              :allocation :class
                        :initform    0 ; #.(+ #$checkBoxProc #$kControlUsesOwningWindowsFontVariant)
                        )
   (check-box-checked-p :initform    nil
                        :initarg    :check-box-checked-p
                        :accessor   check-box-checked-p))
  (:documentation "
Checkboxes are small squares that toggle an X mark on and off when
clicked. The following class and functions govern the behavior of
checkboxes.
"))


(defmethod view-default-font ((view check-box-dialog-item))
  (sys-font-spec))



(defmethod view-default-size ((item check-box-dialog-item))
  (let ((size (call-next-method)))
    ;; required h fudge seems to depend on length of dialog-item-text - maybe font too - maybe fixed now
    (make-point (point-h size) (max 16 (point-v size)))))


(defmethod install-view-in-window :after ((item check-box-dialog-item) dialog)
  (declare (ignore dialog))
  (when (check-box-checked-p item)
    (check-box-check item)))

(defconstant +check-box-side+     11 "Side of the check-box square.")
(defconstant +check-box-interval+  3 "Space between the square and the text.")

(defmethod view-draw-contents ((item check-box-dialog-item))
  (with-focused-dialog-item (item)
    (let* ((frame   (view-frame item))
           (x       (rect-left   frame))
           (y       (rect-top    frame))
           (w       (rect-width  frame))
           (h       (rect-height frame))
           (grayp   (not (dialog-item-enabled-p item)))
           (state   (control-hilite-state item))
           (checked (check-box-checked-p item)))

      ;; box on the left of the label
      ;; state /= 0 => bold box
      ;; checked => cross in the box
      (multiple-value-bind (fa fd fw fl) (font-info)
        (declare (ignore fw fl))
        (let* ((th (round (+ fa fd)))
               (ta (round fa))
               (cx x)
               (cy (if (<= h 11)
                       y
                       (+ y (truncate (- h 11) 2))))
               (cw (if (<= h 11)
                       h
                       11))
               (ch (if (<= h 11)
                       h
                       11))
               (tx (+ cx cw 3))
               (ty (+ y (truncate (- h th) 2) ta)))
          (with-slots (color-list) item
            (with-fore-color (or (getf color-list :frame nil) *black-color*)
              (with-back-color (or (getf color-list :body  nil) *white-color*)
                ;; check-box square:
                (with-pen-state (:size    (if (zerop state)
                                              #@(1 1)
                                              #@(2 2))
                                 :pattern (if grayp
                                              *gray-pattern*
                                              *black-pattern*))
                  (draw-rect* cx cy cw ch))
                (with-pen-state (:size    #@(1 1)
                                 :pattern (if grayp
                                              *gray-pattern*
                                              *black-pattern*))
                  (when checked
                    ;; check-box cross:
                    (draw-line cx cy (+ cx cw -1) (+ cy ch -1))
                    (draw-line cx (+ cy ch -1) (+ cx cw -1) cy))
                  (let ((text (dialog-item-text item)))
                    #+debug-view (format-trace '(view-draw-contents check-box-dialog-item) (dialog-item-text item))
                    ;; always left-aligned
                    (draw-text tx (- ty ta) (- w (- tx x)) th text)))))))))))


(defgeneric check-box-check (item)
  (:documentation "
The check-box-check generic function places an X in the checkbox. The
function merely places an X in the box; it does not run the action of the
dialog item.
")
  (:method ((item check-box-dialog-item))
    (setf (check-box-checked-p item) t)
    (when (installed-item-p item)
      (with-focused-view (view-container item)
        (niy check-box-check item)
        ;; (#_SetControlValue (dialog-item-handle item) 1)
        ))))


(defgeneric check-box-uncheck (item)
  (:documentation "
The check-box-uncheck generic function removes the X from the
checkbox. The function merely removes the X from the box; it does not run
the action of the dialog item. The function returns nil.
")
  (:method ((item check-box-dialog-item))
    (setf (check-box-checked-p item) nil)
    (when (installed-item-p item)
      (with-focused-view (view-container item)
        (niy check-box-uncheck item)
        ;; (#_SetControlValue (dialog-item-handle item) 0)
        ))))


(defmethod dialog-item-action ((item check-box-dialog-item))
  "
The check-box-dialog-item primary method for dialog-itemaction
toggles the state of the box from unchecked to checked or vice
versa, then calls call-next-method.
"
  (if (check-box-checked-p item)
      (check-box-uncheck item)
      (check-box-check item))
  (call-next-method))                   ; dispatch to user's dialog-item-action code



;;;; THE END ;;;;
