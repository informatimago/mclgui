;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               control-dialog-item.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Control Dialog Item
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


(defclass control-dialog-item (dialog-item) 
  ((procid :allocation :class
           :reader control-dialog-item-procid)
   (hilite-state :initarg :hilite-state
                 :initform 0
                 :accessor control-hilite-state)))

(defmethod view-cursor ((item control-dialog-item) where)
  (declare (ignore where))
   *arrow-cursor*)
(defmethod installed-item-p ((item control-dialog-item))
  (let ((dialog (view-container item)))
    (and dialog
         ;; (wptr dialog)
         ;; (dialog-item-handle item)
         )))


(defmethod  install-view-in-window :after ((button control-dialog-item) w)
  (when (and (not (typep button 'scroll-bar-dialog-item)))
    (let ((text (dialog-item-text button)))
      (when text
        (niy install-view-in-window :after button w)
        #-(and)                       ;; done earlier
        (when (not (7bit-ascii-p text)) ;; fix it
          (set-dialog-item-text button text))))))



(defmethod set-view-font-codes :after ((button control-dialog-item) ff ms &optional m1 m2)
  ;; (declare (ignore ff ms m1 m2))
  (niy set-view-font-codes :after button ff ms m1 m2)
  #-(and)
  (when (and (not (typep button 'scroll-bar-dialog-item))
             (dialog-item-handle button))  ;; it's a reset
    ;;(set-dialog-item-text button (dialog-item-text button))
    ))
 


(defmethod set-dialog-item-text ((item control-dialog-item) text)
  (call-next-method)
  (when (installed-item-p item)
    (with-focused-dialog-item (item)
      (niy set-dialog-item-text item text)
      #-(and)
      (if (not (7bit-ascii-p text))
          (set-control-title-cfstring item text)
          (with-pstrs ((tp text))
            (#_SetControlTitle (dialog-item-handle item) tp)))))
  text)


#-(and)
(defmethod set-control-title-cfstring ((item control-dialog-item) text)
  (let* ((len (length text)))
    (declare (fixnum len))
    (%stack-block ((sb  (%i+ len len)))      
      (copy-string-to-ptr text 0 len sb)
      (with-macptrs ((cfstr (#_CFStringCreatewithCharacters (%null-ptr) sb len)))
        (#_SetControlTitleWithCFString (dialog-item-handle item) cfstr)
        (#_cfrelease cfstr)))))


(defmethod install-view-in-window :before ((dialog-item control-dialog-item) dialog)  
  (set-default-size-and-position dialog-item (view-container dialog-item))
  (niy install-view-in-window dialog-item dialog)
  #-(and)
  (when (not (typep dialog-item 'scroll-bar-dialog-item))  ;; done in after method
    (unless (dialog-item-handle dialog-item)  ;; some prior method may have already created it
      (with-pstrs ((sp (dialog-item-text dialog-item)))
        (with-item-rect (rect dialog-item)
          (setf (dialog-item-handle dialog-item)
                (#_NewControl (wptr dialog) 
                               rect 
                               sp
                               nil
                               0
                               0 
                               1
                               (control-dialog-item-procid dialog-item)             
                               0)))
        (let ((text (dialog-item-text dialog-item)))
          (if (not (7bit-ascii-p text))
              (set-control-title-cfstring dialog-item text)))
        (unless (slot-value dialog-item 'dialog-item-enabled-p)
                                        ;sync up what we believe with what the mac believes.
          (#_deactivatecontrol (dialog-item-handle dialog-item)))))))


(defgeneric validate-control-dialog-item (item)
  (:method ((item control-dialog-item))
    (validate-corners item #@(0 0) (view-size item))))


(defmethod set-view-position ((item control-dialog-item) h &optional v)
  (let ((new-pos (make-point h v)))
    (unless (eql new-pos (view-position item))
      (without-interrupts
          (invalidate-view item t)
        (setf (%view-position item) new-pos)
        (when (installed-item-p item)
          (with-focused-view (view-container item)
            (niy set-view-position item h v)
            #-(and)
            (let* ((handle (dialog-item-handle item)))           
              (#_MoveControl handle (point-h new-pos) (point-v new-pos))
              (validate-control-dialog-item item)
              (invalidate-view item))))))
    new-pos))


(defmethod set-view-size ((item control-dialog-item) h &optional v)
  (let ((new-size (make-point h v)))
    (unless (eql new-size (view-size item))
      (without-interrupts
          (let ((size (view-size item)))
            (when (and size (view-position item))
              (invalidate-corners item #@(0 0) size)))
        (call-next-method)
        (when (installed-item-p item)
          (with-focused-view (view-container item)
            (niy set-view-size item h v)
            #-(and)
            (let* ((handle (dialog-item-handle item)))
              (invalidate-view item t)           
              (#_SizeControl handle (point-h new-size)(point-v new-size))
              ;; (validate-control-dialog-item item)   ; remove erase region - no dont <<
              (invalidate-view item))))))
    new-size))


(defmethod remove-view-from-window :after ((item control-dialog-item))
  (niy remove-view-from-window item)
  #-(and)
  (let ((dialog (view-window item)))
    (when dialog
      (let ((handle (dialog-item-handle item)))
        (when handle
          (with-focused-view (view-container item)
            (#_DisposeControl handle)
            (setf (dialog-item-handle item) nil)
            (locally
             (declare (special handle->dialog-item)) ; defined elsewhere
             (when handle->dialog-item
               (remhash handle handle->dialog-item)))))))))


(defmethod dialog-item-disable ((item control-dialog-item))
  (when (and (installed-item-p item) (dialog-item-enabled-p item))
    (with-focused-dialog-item (item)
      (niy dialog-item-disable item)
      #-(and)
      (#_deactivatecontrol (dialog-item-handle item))))
  (setf (dialog-item-enabled-p item) nil))

        
(defmethod dialog-item-enable ((item control-dialog-item))
  (when (and (installed-item-p item) (not (dialog-item-enabled-p item)))
    (with-focused-dialog-item (item)
      (niy dialog-item-enable item)
      #-(and)
      (#_activatecontrol (dialog-item-handle item))))
  (setf (dialog-item-enabled-p item) t))


(defmethod view-activate-event-handler ((item control-dialog-item))
  (when (dialog-item-enabled-p item)
    (niy view-activate-event-handler item)
    #-(and)
    (#_activatecontrol (dialog-item-handle item))
    (unless *deferred-drawing*
     (when (part-color item :frame)
       (view-draw-contents item)))))


(defmethod view-deactivate-event-handler ((item control-dialog-item))
  (niy view-deactivate-event-handler item)
  #-(and)
  (#_deactivatecontrol (dialog-item-handle item)))


(defmethod view-draw-contents ((item control-dialog-item))
  (when (installed-item-p item)
    (call-next-method)
    ;; #-(and)
    ;; (if (#_iscontrolvisible handle)
    ;;     (#_Draw1Control handle)
    ;;     (#_ShowControl handle))
    ))



(defparameter *eol-chars*
  `(#\return #\linefeed ,(code-char #x2028) ,(code-char #x2029)))

(defparameter *eol-char-codes*
  (mapcar (function char-code) *eol-chars*))

(defparameter *eol-string*
  (coerce *eol-chars* 'string))

(defun char-eolp (char)
  (member char *eol-chars*))

(defun char-code-eolp (code)
  (member code *eol-char-codes*))

(defun string-eol-position (string &optional (start 0) (end (length string)))
  (position-if (function char-eolp) string :start start :end end))


(defun font-codes-string-width-for-control (string ff ms &optional (start 0) (end (length string)))
  (niy font-codes-string-width-for-control string ff ms start end)
  #-(and) (with-port  %temp-port%
            (with-font-codes ff ms
                             (with-cfstrs-hairy ((x string start end))    
                               (rlet ((bounds :point)
                                      (baseline :signed-integer))
                                     (#_GetThemeTextDimensions x #$kThemeCurrentPortFont #$kthemestateactive
                                                               nil bounds baseline)
                                     (point-h (%get-point bounds))))))
  100)


(defun font-codes-string-width-with-eol-for-control (string ff ms)  
  (let ((pos     0)
        (nextpos 0)
        (nlines  1)
        (max     0))
    (loop
      (if (setf nextpos (string-eol-position string pos))
          (setf max    (max max (font-codes-string-width-for-control string ff ms pos nextpos))
                nlines (1+ nlines)
                pos    (1+ nextpos))
          (return (values (max max (font-codes-string-width-for-control string ff ms pos))
                          nlines))))))

(defmethod view-default-size ((item control-dialog-item))
  (multiple-value-bind (ff ms) (view-font-codes item)
    (let* ((text (dialog-item-text item)))
      (multiple-value-bind (string-width nlines)  (font-codes-string-width-with-eol-for-control text ff ms)
        (make-point (+ (dialog-item-width-correction item) string-width)
                    (* nlines (font-codes-line-height ff ms)))))))

(defgeneric hilite-control (control-item state))

(defmethod hilite-control ((item control-dialog-item) state)
  (format-trace 'hilite-control :state state :item item)
  (setf (control-hilite-state item) state)
  (unless *deferred-drawing*
    (with-focused-dialog-item (item)
      (view-draw-contents item))
    (graphics-flush)))


(defmethod view-click-event-handler ((item control-dialog-item) where)
  (declare (ignore where))
  (format-trace '(view-click-event-handler control-dialog-item) "start")
  (loop
    :with frame = (view-frame item)
    :with state = 1
    :while (wait-mouse-up)
    :for mouse = (get-mouse)
    :initially (hilite-control item state)
    :do (if (zerop state)
            (when (point-in-rect-p frame mouse)
              (format-trace "entering" :frame (rect-to-list frame) :mouse (point-to-list mouse))
              (hilite-control item (setf state 1)))
            (unless (point-in-rect-p frame mouse)
              (format-trace "exiting"  :frame (rect-to-list frame) :mouse (point-to-list mouse))
              (hilite-control item (setf state 0))))
    :finally (unless (zerop state)
               (format-trace "acting")
               (unwind-protect
                    (dialog-item-action item)
                 (hilite-control item 0))))
  (format-trace '(view-click-event-handler control-dialog-item) "stop"))


;;;; THE END ;;;;
