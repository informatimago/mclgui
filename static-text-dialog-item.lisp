;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               static-text-dialog-item.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Static Text Dialog Item
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


(defclass static-text-dialog-item (dialog-item)
  ((width-correction   :allocation :class
                       :initform    4)
   (text-justification :initform   nil
                       :initarg    :text-justification)
   (text-truncation    :initform   nil
                       :initarg    :text-truncation)
   (compress-text      :initform   nil
                       :initarg    :compress-text
                       :reader     compress-text))
  (:default-initargs :dialog-item-text "Untitled"))


(defmethod view-default-font ((view static-text-dialog-item))
  (sys-font-spec))


(defmethod view-default-size ((dialog-item static-text-dialog-item))
  (let ((text (dialog-item-text dialog-item)))
    (multiple-value-bind (ff ms) (view-font-codes dialog-item)
      (multiple-value-bind (string-width nlines) (font-codes-string-width-with-eol text ff ms)
        (make-point (+ (dialog-item-width-correction dialog-item) string-width)
                    (* nlines (font-codes-line-height ff ms)))))))


(defmethod set-dialog-item-text :after ((item static-text-dialog-item) text)
  (invalidate-view item t))


(defmethod view-draw-contents ((item static-text-dialog-item))
  (with-focused-dialog-item (item)
   (let* ((frame (view-frame item))
          (x (rect-left   frame))
          (y (rect-top    frame))
          (w (rect-width  frame))
          (h (rect-height frame)))
     (progn (format t "~&view ~A~%" (view-nick-name item))
            (format t "~&  frame   = ~S~%" (rect-to-list (view-frame item)))
            (format t "~&  bounds  = ~S~%" (rect-to-list (view-bounds item)))
            (finish-output))
     (with-fore-color *red-color*
       (fill-rect* x y w h))
     ;; (erase-rect* x y w h)
     (draw-text x y w h (dialog-item-text item))))
  
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



;;;; THE END ;;;;
