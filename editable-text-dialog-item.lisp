;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               editable-text-dialog-item.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Editable Text Dialog Item
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
  ((width-correction   :allocation :class :initform 4)
   (text-justification :allocation :class :initform 0)))

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
  (format-trace 'view-click-event-handler item (point-to-list where))
  ;; (with-handle (texth item)
  ;;   [texth superMouseDown])
  (call-next-method)
  item)

(defmethod view-key-event-handler ((item basic-editable-text-dialog-item) key)
  (declare (ignorable key))
  (format-trace 'view-key-event-handler item key)
  (call-next-method)
  ;; (with-handle (texth item)
  ;;   [texth superKeyDown])
  item)


(defmethod view-draw-contents ((item basic-editable-text-dialog-item))
  (with-focused-dialog-item (item)
   (let* ((frame (view-frame item))
          (x (rect-left   frame))
          (y (rect-top    frame))
          (w (rect-width  frame))
          (h (rect-height frame)))
     #+debug-view
     (progn (format t "~&view ~A~%" (or (view-nick-name item)  (class-name (class-of item))))
            (format t "~&  frame   = ~S~%" (rect-to-list (view-frame item)))
            (format t "~&  bounds  = ~S~%" (rect-to-list (view-bounds item)))
            (finish-output))
     ;; (with-fore-color *red-color*
     ;;   (fill-rect* x y w h))
     (erase-rect* x y w h)
     (draw-text x y w h (dialog-item-text item))
     (draw-rect* (1- x) (1- y) (+ 2 w) (+ 2 h))))
  
  ;; We shouldn't have to do anything really
  #-(and)
  (when (installed-item-p item)
    (without-interrupts
     (with-focused-view (view-container item)
       (let ((position           (view-position item))
             (size               (view-size item))
             (text-justification (slot-value item 'text-justification))
             (truncation         (slot-value item 'text-truncation))
             (enabled-p          (dialog-item-enabled-p item))
             (compress-p         (compress-text item))
             (old-state          nil))
         (declare (ignorable position size text-justification truncation enabled-p compress-p old-state))
         (let* ((rect (make-rect position (add-points position size)))
                (theme-back nil ;; (theme-background-p item)
                            )
                (back (or (part-color item :body)
                          (when (not theme-back)
                            (slot-value (view-window item) 'back-color))))                          
                (fore (if enabled-p
                        (part-color item :text)
                        *gray-color*)))
           ;; (when (and (not back) theme-back) ; (not (dialog-item-enabled-p item)))  ;; sometimes background goes white??
           ;; (rlet ((old-statep :ptr))
           ;;   (#_getthemedrawingstate old-statep)
           ;;   (setq old-state (%get-ptr old-statep)))
           ;; (let* ((wptr (wptr item))
           ;;        (depth (current-pixel-depth)))
           ;;   (#_setthemebackground  #$kThemeBrushModelessDialogBackgroundActive depth (wptr-color-p wptr)))
           ;; )
           (with-back-color back
             (multiple-value-bind (ff ms)(view-font-codes item)
               (when t ;; or when back?
                 (erase-rect* item
                             (point-h position) (point-v position)
                             (point-h size) (point-v size)))  
               (draw-string-in-rect (dialog-item-text item) rect 
                                    :justification text-justification
                                    :compress-p compress-p
                                    :truncation truncation
                                    :ff ff :ms ms :color fore)))
           ;; (if old-state (#_setthemedrawingstate old-state t))
           ))))))


;;;---------------------------------------------------------------------
;;;

(defclass editable-text-dialog-item (basic-editable-text-dialog-item)
  ())


(defmethod view-clip-region ((view editable-text-dialog-item))
  (niy view-clip-region view)
  ;; don't include frame
  #-(and)
  (let* ((pos           (view-position view))
         (size          (view-size view))
         (rgn           *simple-view-clip-region*)
         (container     (view-container view))
         (container-rgn (view-clip-region container)))
    (if (or (null pos) (null size))
        (#_EmptyRgn rgn)
        (let* ((pos-h  (point-h pos))
               (pos-v  (point-v pos))
               (size   (view-size view))
               (size-h (point-h size))
               (size-v (point-v size)))
          (#_setrectrgn rgn pos-h pos-v (+ pos-h size-h) (+ pos-v size-v))
          (#_SectRgn rgn container-rgn rgn)
          (#_OffsetRgn rgn (- pos-h) (- pos-v))))
    rgn))


(defmethod view-default-font ((view editable-text-dialog-item))
  (sys-font-spec))


;;;; THE END ;;;;
