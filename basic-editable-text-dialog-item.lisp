;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               basic-editable-text-dialog-item.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Basic Editable Text Dialog Item
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-05-23 <PJB> Created.
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

;;;---------------------------------------------------------------------
;;;

(defclass basic-editable-text-dialog-item (key-handler-mixin dialog-item)
  ((width-correction   :allocation :class      :initform 4)
   (line-height        :initform nil)
   (font-ascent        :initform nil)))

;;;---- key-handler-mixin

(defmethod selection-range ((item basic-editable-text-dialog-item))
  (with-slots (selection-start selection-end) item
    (values selection-start selection-end)))


(defmethod set-selection-range (item &optional start end)
  (with-slots (selection-start selection-end) item
    (unless (and (= selection-start start) (= selection-end end))
      (setf selection-start start
            selection-end end)
      (view-draw-contents item))))


;;;----

(defmethod view-default-size ((item basic-editable-text-dialog-item))
  (let* ((pt    (call-next-method))
         (width (point-h pt)))
    (make-point (- (ash width 1) (ash width -1)) (point-v pt))))


(defmethod dialog-item-disable :before ((item basic-editable-text-dialog-item))
  (let ((window (view-window item)))
    (when window
      (when (eql item (current-key-handler window))
        (change-key-handler window))
      (when (eql item (current-key-handler window)) ;still current, so only one
        (set-selection-range item 0 0)
        (setf (%get-current-key-handler window) nil)))))


(defmethod view-click-event-handler ((item basic-editable-text-dialog-item) where)
  (declare (ignorable where))
  #+debug-view (format-trace '(view-click-event-handler basic-editable-text-dialog-item) :where (point-to-list where) :item item)
  ;; (with-handle (texth item)
  ;;   [texth superMouseDown])
  (let ((*step-mode* :trace))
    (call-next-method))
  item)


(defmethod view-key-event-handler ((item basic-editable-text-dialog-item) key)
  (declare (ignorable key))
  #+debug-view (format-trace 'view-key-event-handler item key)
  (call-next-method)
  ;; (with-handle (texth item)
  ;;   [texth superKeyDown])
  item)


(defmethod view-draw-contents ((item basic-editable-text-dialog-item))
  (call-next-method)
  #-(and)
  (when (installed-item-p item)
    (with-focused-dialog-item (item)
      (let* ((frame (view-frame item))
             (x     (rect-left   frame))
             (y     (rect-top    frame))
             (w     (rect-width  frame))
             (h     (rect-height frame))
             (back  (or (part-color item :body) (get-back-color (view-window item))))
             (fore  (if (dialog-item-enabled-p item)
                        (or (part-color item :text) (get-fore-color (view-window item)))
                        *gray-color*)))
        (with-fore-and-back-color fore back
          (erase-rect* x y w h)
          (draw-text x y w h (dialog-item-text item)
                     (slot-value item 'text-truncation)
                     (slot-value item 'text-justification)
                     (compress-text item)))))))


(defmethod frame-key-handler ((item basic-editable-text-dialog-item))
  (let ((w (view-window item)))
    (when w
      (let* ((pos      (view-position item))
             (active-p (window-active-p w))
             (rect     (make-rect pos (add-points pos (view-size item)))))
        (if (and active-p (eql item (current-key-handler w)))                         
            (with-pen-state (:size #@(2 2))
              (inset-rect rect -3 -3)
              (erase-rect* (rect-left rect) (rect-top rect) (rect-width rect) (rect-height rect))
              (draw-rect*  (rect-left rect) (rect-top rect) (rect-width rect) (rect-height rect)))              
            (progn
              (inset-rect rect -1 -1)
              (erase-rect* (rect-left rect) (rect-top rect) (rect-width rect) (rect-height rect))
              (draw-rect*  (rect-left rect) (rect-top rect) (rect-width rect) (rect-height rect))))))))


(defmethod view-draw-contents :after ((item basic-editable-text-dialog-item))
  (let ((pos    (view-position item))
        (colorp (color-or-gray-p item)))
    (with-slots (dialog-item-enabled-p draw-outline) item      
      (when draw-outline
        (frame-key-handler item))
      (unless (or colorp dialog-item-enabled-p)
        (with-pen-state (:pattern *gray-pattern*
                         :mode 11)
          (fill-rect* (point-h pos) (point-v pos) (point-h (view-size item)) (point-v (view-size item))))))))


(defmethod view-corners ((item basic-editable-text-dialog-item))
  (let ((draw-outline (slot-value item 'draw-outline)))
    (if draw-outline
        (let* ((inset (if (fixnump draw-outline)
                          (- draw-outline 2)
                          -5)))
          (multiple-value-call (function inset-corners) (make-point inset inset) (call-next-method)))
        (call-next-method))))


(defmethod view-activate-event-handler :after  ((item basic-editable-text-dialog-item))
  (invalidate-view item)
  (when (slot-value item 'draw-outline)
    (frame-key-handler item)))


(defmethod view-deactivate-event-handler :after ((item basic-editable-text-dialog-item))
  (invalidate-view item)
  (when (slot-value item 'draw-outline)
    (frame-key-handler item)))


;;;; THE END ;;;;
