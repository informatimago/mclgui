;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               graphics.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Implements a few graphic primitives.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2014-09-24 <PJB> Added draw-text.
;;;;    2012-07-04 <PJB> Extracted from pw-graphics.lisp for mclgui usage.
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
(mclgui.readtable:enable-objcl+ccl-reader-macros)
(enable-sharp-at-reader-macro)


(defun draw-char (x y cn)
  (draw-string x y (string cn)))


(defun maestro-string (string)
  (map 'string (lambda (ch) (code-char (+ #xf000 (char-code ch)))) string))

(defun draw-string (x y str)
  #+debug-graphics (format-trace "draw-string" x y str *current-view* (when *current-view* (view-window *current-view*)))
  (destructuring-bind (ff ms) *current-font-codes*
    (multiple-value-bind (descriptor mode) (font-descriptor-from-codes ff ms)
      (declare (ignore mode)) ; TODO: manage mode (:srcOr …)
      (let* ((attributes  [descriptor fontAttributes])
             (name        [attributes objectForKey:(objcl:objc-string NSFontNameAttribute)])
             (shift       (if [name isEqualToString:(objcl:objc-string "Maestro")]
                              (function maestro-string)
                              (function identity))))
        ;; (print descriptor)
        ;; [context setCompositingOperation:(mode-to-compositing-operation (pen-mode pen))]
        ;; (format-trace "draw-string" x y str [descriptor fontAttributes])
        (multiple-value-bind (a d w l) (font-codes-info ff ms)
          (declare (ignore w l))
          ;; (format-trace "draw-string" a d w l)
          ;; the origin of the bounding box.  topleft in flipped coordinates.
          [(objcl:objc-string (funcall shift str))
           drawAtPoint: (ns:make-ns-point x (- y a d))
           withAttributes: attributes]))))
  str)


(defun font-attributes (descriptor)
  [descriptor fontAttributes]
  #-(and) (let* ((attributes [[descriptor fontAttributes] mutableCopy])
                 (name [attributes objectForKey:#$NSFontNameAttribute])
                 (size [attributes objectForKey:#$NSFontSizeAttribute]))
            [attributes setObject:[NSFont fontWithName:name size:[size doubleValue]]
                        forKey:#$NSFontNameAttribute]
            attributes))


(defun draw-text (x y width height text
                  &optional (truncation :clipping) (justification :natural) (compress-p nil))
  #+debug-graphics (format-trace "draw-text" (list x y width height) text truncation justification compress-p)
  (destructuring-bind (ff ms) *current-font-codes*
    (multiple-value-bind (descriptor mode) (font-descriptor-from-codes ff ms)
      (declare (ignore mode)) ; TODO: manage mode (:srcOr …)
      (let ((attributes [(font-attributes descriptor) mutableCopy]))
        ;; [context setCompositingOperation:(mode-to-compositing-operation (pen-mode pen))]
        [attributes setObject:(unwrap (make-paragraph-style
                                       :alignment justification
                                       :line-break-mode truncation
                                       :tightening-factor (if compress-p 0.05 0.0)))
                    forKey:#$NSParagraphStyleAttributeName]
        [(objcl:objc-string text)
         drawInRect: (nsrect x y width height)
         withAttributes:attributes])))
  text)


(defun draw-point (x y)
  #+debug-graphics (format-trace "draw-point" x y *current-view* (when *current-view* (view-window *current-view*)))
  (when *current-view*
    (let ((window  (view-window *current-view*)))
      (when window
        (let ((size (pen-size (view-pen window))))
          (#_NSRectFill (ns:make-ns-rect x y (point-h size) (point-v size))))))))


(defun draw-line (x1 y1 x2 y2)
  #+debug-graphics (format-trace "draw-line" x1 y1 x2 y2 *current-view* (when *current-view* (view-window *current-view*)))
  (when *current-view*
    (let ((window  (view-window *current-view*)))
      (when window
        (let* ((pen  (view-pen window))
               (size (pen-size pen))
               (path [NSBezierPath bezierPath]))
          [path setLineCapStyle:#$NSSquareLineCapStyle]
          ;; stroke draws between the pixels, so we'll fill the line always.
          ;; (if (and (= #@(1 1) size)
          ;;          (eql *black-pattern* (pen-state-pattern pen)))
          ;;     (progn
          ;;       [path moveToPoint:(ns:make-ns-point x1 y1)]
          ;;       [path lineToPoint:(ns:make-ns-point x2 y2)]
          ;;       [path stroke])
          (let ((sx (point-h size))
                (sy (point-v size)))
            (unless (< x1 x2)
              (rotatef x1 x2)
              (rotatef y1 y2))
            (if (< y1 y2)
                (progn
                  [path moveToPoint:(ns:make-ns-point x1 y1)]
                  [path lineToPoint:(ns:make-ns-point (+ x1 sx) y1)]
                  [path lineToPoint:(ns:make-ns-point (+ x2 sx) y2)]
                  [path lineToPoint:(ns:make-ns-point (+ x2 sx) (+ y2 sy))]
                  [path lineToPoint:(ns:make-ns-point x2 (+ y2 sy))]
                  [path lineToPoint:(ns:make-ns-point x1 (+ y1 sy))])
                (progn
                  [path moveToPoint:(ns:make-ns-point x1 y1)]
                  [path lineToPoint:(ns:make-ns-point x2 y2)]
                  [path lineToPoint:(ns:make-ns-point (+ x2 sx) y2)]
                  [path lineToPoint:(ns:make-ns-point (+ x2 sx) (+ y2 sy))]
                  [path lineToPoint:(ns:make-ns-point (+ x1 sx) (+ y1 sy))]
                  [path lineToPoint:(ns:make-ns-point x1 (+ y1 sy))]))
            [path closePath]
            [path fill]))))))

;;;--------------------------------------------------------------------
;;; rect
;;;--------------------------------------------------------------------

(defun draw-rect* (x y w h)
  #+debug-graphics (format-trace "draw-rect*" x y w h *current-view* (when *current-view* (view-window *current-view*)))
  (unless *current-view*
    (with-error-file
      (format *error-output* "~&draw-rect* with null *current-view*!~%")))
  (when *current-view*
    (let ((window  (view-window *current-view*)))
      (when window
        (let* ((pen  (view-pen window))
               (size (pen-size pen)))
          ;; (print (list '(and (= #@(1 1) size)
          ;;                (eql *black-pattern* (pen-state-pattern pen)))
          ;;              (and (= #@(1 1) size)
          ;;                   (eql *black-pattern* (pen-state-pattern pen)))
          ;;              size (pen-state-pattern pen))
          ;;        *trace-output*)
          ;; (terpri *trace-output*)
          ;; (print (bt:current-thread) *trace-output*)
          ;; (terpri *trace-output*)
          (if (and (= #@(1 1) size)
                   (eql *black-pattern* (pen-state-pattern pen)))
              (#_NSFrameRect (ns:make-ns-rect x y w h))
              (let ((path [NSBezierPath bezierPath])
                    (sx (point-h size))
                    (sy (point-v size)))
                ;; (#_NSFrameRect (ns:make-ns-rect x y w h))
                [path setLineCapStyle:#$NSSquareLineCapStyle]
                ;; external border
                [path moveToPoint:(ns:make-ns-point x y)]
                [path lineToPoint:(ns:make-ns-point (+ x w) y)]
                [path lineToPoint:(ns:make-ns-point (+ x w) (+ y h))]
                [path lineToPoint:(ns:make-ns-point x (+ y h))]
                [path lineToPoint:(ns:make-ns-point x y)]
                ;; internal border
                [path moveToPoint:(ns:make-ns-point (+ x sx)       (+ y sy))]
                [path lineToPoint:(ns:make-ns-point (+ x sx)       (- (+ y h) sy))]
                [path lineToPoint:(ns:make-ns-point (- (+ x w) sx) (- (+ y h) sy))]
                [path lineToPoint:(ns:make-ns-point (- (+ x w) sx) (+ y sy))]
                [path lineToPoint:(ns:make-ns-point (+ x sx)       (+ y sy))]
                [path closePath]
                [path fill])))))))




#-(and) (loop
          :for mode :in '(:srcCopy :srcOr :srcXor :srcBic
                          :notSrcCopy :notSrcOr :notSrcXor :notSrcBic
                          :patCopy :patOr :patXor :patBic
                          :notPatCopy :notPatOr :notPatXor :notPatBic)
          :do (with-focused-view (front-window)
                (with-pen-state (:pattern *gray-pattern* :size (make-point 10 10)
                                 :mode :srcCopy)
                  (draw-rect* 10 20 100 200))
                (with-pen-state (:pattern *gray-pattern* :size (make-point 10 10)
                                 :mode mode)
                  (draw-rect* 10 20 100 200)))
              (sleep 3))

#-(and) (with-focused-view (front-window)
          (with-pen-state (:pattern *light-gray-pattern* :size (make-point 20 10)
                           :mode :srcCopy)
            (draw-rect* 10 20 200 100)))


(defun fill-rect* (x y w h)
  #+debug-graphics (format-trace "fill-rect*" x y w h *current-view* (when *current-view* (view-window *current-view*)))
  (when *current-view*
    (let ((window  (view-window *current-view*)))
      (when window
        (let* ((pen  (view-pen window)))
          (#_NSRectFillUsingOperation (ns:make-ns-rect x y w h)
                                      (mode-to-compositing-operation (pen-mode pen))))))))


(defun erase-rect* (x y w h)
  (let ((color (unwrap (or (and *current-view*
                                (view-window *current-view*)
                                (get-back-color (view-window *current-view*)))
                           *background-color*))))
    #+debug-graphics (format-trace "erase-rect*" x y w h :brightness [color brightnessComponent]
                                   :view *current-view* :window (when *current-view* (view-window *current-view*)))
    (with-saved-graphic-state ()
      [color setFill]
      (#_NSRectFill (ns:make-ns-rect x y w h)))))


(defun draw-rect (rect)
  (draw-rect* (rect-left rect) (rect-top rect) (rect-width rect) (rect-height rect)))

;; (defun fill-rect (rect)
;;   (fill-rect* (rect-left rect) (rect-top rect) (rect-width rect) (rect-height rect)))
;;
;; (defun erase-rect (rect)
;;   (erase-rect* (rect-left rect) (rect-top rect) (rect-width rect) (rect-height rect)))

;; (setf *color-available* t)
;; (with-focused-view (front-window)
;;   (with-back-color *orange-color*
;;    (erase-rect* 0 0 100 200)))
;; (with-focused-view (front-window)
;;   (with-pen-state (:pattern *light-gray-pattern* :size (make-point 20 10))
;;     (with-fore-color *blue-color*
;;       (draw-ellipse 20 20 200 100))))



;;;--------------------------------------------------------------------
;;; round-rect
;;;--------------------------------------------------------------------

(defun average (a b) (round (+ a b) 2))

(defun draw-round-rect* (oval-width oval-height x y w h)
  #+debug-graphics (format-trace "draw-round-rect*" oval-width oval-height x y w h *current-view* (when *current-view* (view-window *current-view*)))
  (when *current-view*
    (let ((window  (view-window *current-view*)))
      (when window
        (let* ((pen  (view-pen window))
               (size (pen-size pen)))
          ;; TODO: deal with rectangular pen sizes.
          [NSBezierPath setDefaultLineWidth:(cgfloat (average (point-h size) (point-v size)))]
          [[NSBezierPath bezierPathWithRoundedRect:(ns:make-ns-rect x y w h)
                         xRadius:(cgfloat (/ oval-width  2.0))
                         yRadius:(cgfloat (/ oval-height 2.0))] stroke])))))


(defun fill-round-rect* (oval-width oval-height x y w h)
  #+debug-graphics (format-trace "fill-round-rect*" oval-width oval-height x y w h *current-view* (when *current-view* (view-window *current-view*)))
  (when *current-view*
    (let ((window  (view-window *current-view*)))
      (when window
        (let* ((pen  (view-pen window))
               (size (pen-size pen)))
          ;; TODO: deal with rectangular pen sizes.
          [NSBezierPath setDefaultLineWidth:(cgfloat (average (point-h size) (point-v size)))]
          [[NSBezierPath bezierPathWithRoundedRect:(ns:make-ns-rect x y w h)
                         xRadius:(cgfloat (/ oval-width  2.0))
                         yRadius:(cgfloat (/ oval-height 2.0))] fill])))))


(defun erase-round-rect* (oval-width oval-height x y w h)
  #+debug-graphics (format-trace "erase-round-rect*" oval-width oval-height x y w h *current-view* (when *current-view* (view-window *current-view*)))
  (when *current-view*
    (let ((window  (view-window *current-view*)))
      (when window
        (let* ((pen  (view-pen window))
               (size (pen-size pen))
               (color (unwrap (or (and *current-view*
                                       (view-window *current-view*)
                                       (get-back-color (view-window *current-view*)))
                                  *background-color*))))
          ;; TODO: deal with rectangular pen sizes.
          [NSBezierPath setDefaultLineWidth:(cgfloat (average (point-h size) (point-v size)))]
          (with-saved-graphic-state ()
            [color setFill]
            [[NSBezierPath bezierPathWithRoundedRect:(ns:make-ns-rect x y w h)
                           xRadius:(cgfloat (/ oval-width  2.0))
                           yRadius:(cgfloat (/ oval-height 2.0))] fill]))))))


(defun draw-round-rect (oval-width oval-height rect)
  (draw-round-rect* oval-width oval-height (rect-left rect) (rect-top rect) (rect-width rect) (rect-height rect)))

;;;--------------------------------------------------------------------
;;; ellipse
;;;--------------------------------------------------------------------

(defun draw-ellipse (x y w h)
  #+debug-graphics (format-trace "draw-ellipse-rect" x y w h *current-view* (when *current-view* (view-window *current-view*)))
  (when *current-view*
    (let ((window  (view-window *current-view*)))
      (when window
        (let* ((pen  (view-pen window))
               (size (pen-size pen)))
          (if (and (= #@(1 1) size)
                   (eql *black-pattern* (pen-state-pattern pen)))
              [[NSBezierPath bezierPathWithOvalInRect: (ns:make-ns-rect x y w h)] stroke]
              (let ((path [NSBezierPath bezierPath])
                    (sx (point-h size))
                    (sy (point-v size)))
                ;; TODO: use the pen-pattern
                [path setWindingRule:#$NSEvenOddWindingRule]
                ;; external border
                [path appendBezierPathWithOvalInRect:(ns:make-ns-rect x y w h)]
                ;; internal border
                [path appendBezierPathWithOvalInRect:(ns:make-ns-rect (+ x sx) (+ y sy)
                                                                      (- w sx sx) (- h sy sy))]
                [path fill])))))))


(defun fill-ellipse (x y w h)
  #+debug-graphics (format-trace "fill-ellipse-rect" x y w h *current-view* (when *current-view* (view-window *current-view*)))
  (when *current-view*
    (let ((window  (view-window *current-view*)))
      (when window
        ;; TODO: use pen-pattern
        [[NSBezierPath bezierPathWithOvalInRect: (ns:make-ns-rect x y w h)] fill]))))


#-(and)
(progn

  (trace draw-char
         draw-string
         font-attributes
         draw-text
         draw-point
         draw-line
         draw-rect*
         fill-rect*
         erase-rect*
         draw-rect
         average
         draw-round-rect*
         fill-round-rect*
         erase-round-rect*
         draw-round-rect
         draw-ellipse
         fill-ellipse)

  (pushnew :debug-graphics *features*)
  )


;;;; THE END ;;;;
