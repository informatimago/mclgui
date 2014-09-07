;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               layout.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Layout of subviews in a view.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2014-09-05 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2014 - 2014
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

(defmacro with-readers ((&rest slots-and-readers) object &body body)
  (let ((vobject (gensym)))
    `(let ((,vobject ,object))
       (let ,(mapcar (lambda (slot-and-reader)
                       (list (first slot-and-reader)
                             (list (second slot-and-reader) vobject)))
              slots-and-readers)
         ,@body))))


;;;--------------------------------------------------------------------

(defclass layout (view)
  ((left-margin   :initarg :left-margin   :initform 0 :accessor layout-left-margin)
   (right-margin  :initarg :right-margin  :initform 0 :accessor layout-right-margin)
   (top-margin    :initarg :top-margin    :initform 0 :accessor layout-top-margin)
   (bottom-margin :initarg :bottom-margin :initform 0 :accessor layout-bottom-margin)
   (minimum-size  :initarg :minimum-size  :initform #@(0 0) :accessor layout-minimum-size))
  (:documentation "Abstract superclass for layouts"))

(defgeneric adjust-layout-to-parent (layout)
  (:documentation "Set size and position of layout to match the parent view.
Recompute the layout if the size changes."))

(defgeneric adjust-layout-to-match-parent (layout dimension)
  (:documentation "Set size and position of layout to match the parent view.
Recompute the layout if the size changes.
DIMENSION: (member :width :height :match"))

(defgeneric adjust-layout (layout)
  (:documentation "Recompute the layout of all subviews."))

(defgeneric layout-box (layout)
  (:documentation "Returns the bounds of the layout, adjusted by the margins."))




(defun size-minimum (a b)
  (make-point (min (point-h a) (point-h b))
              (min (point-v a) (point-v b))))

(defmethod set-view-size :after ((layout layout) h &optional v)
  (declare (ignorable h v))
  (loop
    :for subview :across (view-subviews layout)
    :do (adjust-layout-to-match-parent subview :match)))

(defmethod adjust-layout-to-match-parent ((view simple-view) match)
  (declare (ignorable view match)))

(defmethod adjust-layout-to-parent ((layout layout))
  (adjust-layout-to-match-parent layout :match))

(defmethod adjust-layout-to-match-parent ((layout layout) match)
  (let ((parent (view-container layout)))
    (when parent
      (let ((bounds (view-bounds parent))
            (frame  (view-frame layout)))
        (with-deferred-drawing layout
          (case match
            ((:width)
             (format-trace '(set-view-position layout) :left (rect-left bounds) :top (rect-top frame))
             (set-view-position layout (rect-left bounds) (rect-top frame))
             (when (/= (rect-width frame) (rect-width bounds))
               (format-trace '(set-view-size layout) :width  (rect-width bounds) :height (rect-height frame))
               (set-view-size layout (rect-width bounds) (rect-height frame))
               (adjust-layout layout)))
            ((:height)
             (format-trace '(set-view-position layout) :left (rect-left frame) :top (rect-top bounds))
             (set-view-position layout (rect-left frame) (rect-top bounds))
             (when (/= (rect-height frame) (rect-height bounds))
               (format-trace '(set-view-size layout) :width (rect-width frame) :height (rect-height bounds))
               (set-view-size layout (rect-width frame) (rect-height bounds))
               (adjust-layout layout)))
            ((:match)
             (format-trace '(set-view-position layout) :rect (rect-to-list (rect-topleft bounds)))
             (set-view-position layout (rect-topleft bounds))
             (when (/= (view-size layout) (rect-size bounds))
               (format-trace '(set-view-size layout) :size (point-to-list (rect-size bounds)))
               (set-view-size layout (rect-size bounds))
               (adjust-layout layout)))))))))


(defmethod layout-box ((layout layout))
  (with-accessors ((left-margin layout-left-margin)
                   (right-margin layout-right-margin)
                   (top-margin layout-top-margin)
                   (bottom-margin layout-bottom-margin)
                   (minimum-size layout-minimum-size)
                   (orientation layout-orientation)
                   (horizontal-alignment layout-horizontal-alignment)
                   (vertical-alignment layout-vertical-alignment)
                   (spacing layout-spacing)) layout
    (let ((bounds (view-bounds layout)))
      (incf (rect-left   bounds) left-margin)
      (incf (rect-top    bounds) top-margin)
      (decf (rect-right  bounds) right-margin)
      (decf (rect-bottom bounds) bottom-margin)
      bounds)))



;;;--------------------------------------------------------------------

(defgeneric rect-border (orientation direction rect)
  (:method ((orientation (eql :horizontal)) (direction (eql :left-to-right)) rect) (rect-left   rect))
  (:method ((orientation (eql :horizontal)) (direction (eql :right-to-left)) rect) (rect-right  rect))
  (:method ((orientation (eql :vertical))   (direction (eql :top-down))      rect) (rect-top    rect))
  (:method ((orientation (eql :vertical))   (direction (eql :bottom-up))     rect) (rect-bottom rect)))
(defgeneric rect-length (orientation rect)
  (:documentation "Length of the rect in the given orientation.")
  (:method ((orientation (eql :horizontal)) rect) (rect-width rect))
  (:method ((orientation (eql :vertical))   rect) (rect-height rect)))
(defgeneric rect-breadth (orientation rect)
  (:documentation "Length of the rect in the given orientation.")
  (:method ((orientation (eql :horizontal)) rect) (rect-height rect))
  (:method ((orientation (eql :vertical))   rect) (rect-width rect)))

(defgeneric view-layout-length (orientation view)
  (:documentation "Length of the view in the given orientation.")
  (:method ((orientation (eql :horizontal)) view) (point-h (view-size view)))
  (:method ((orientation (eql :vertical))   view) (point-v (view-size view))))
(defgeneric view-layout-breadth (orientation view)
  (:documentation "Length of the view in the other orientation.")
  (:method ((orientation (eql :horizontal)) view) (point-v (view-size view)))
  (:method ((orientation (eql :vertical))   view) (point-h (view-size view))))

(defgeneric set-view-border (orientation direction view ordinate)
  (:method ((orientation (eql :horizontal)) (direction (eql :left-to-right)) (view simple-view) ordinate)
    (let ((pos (view-position view)))
      (set-view-position view ordinate (point-v pos))))
  (:method ((orientation (eql :horizontal)) (direction (eql :right-to-left)) (view simple-view) ordinate)
    (let ((pos (view-position view))
          (siz (view-size view)))
      (set-view-position view (- ordinate (point-h siz)) (point-v pos))))
  (:method ((orientation (eql :vertical))   (direction (eql :top-down))      (view simple-view) ordinate)
    (let ((pos (view-position view)))
      (set-view-position view (point-h pos) ordinate)))
  (:method ((orientation (eql :vertical))   (direction (eql :bottom-up))     (view simple-view) ordinate)
    (let ((pos (view-position view))
          (siz (view-size view)))
      (set-view-position view (point-h pos) (- ordinate (point-v siz))))))


(defun other-orientation (orientation)
  (ecase orientation
     (:horizontal :vertical)
     (:vertical :horizontal)))

(defgeneric view-layout-set-position (view orientation direction distance box alignment)
  (:method ((view simple-view) orientation direction distance box alignment)
    (let ((ordinate (funcall (ecase direction
                               ((:left-to-right :top-down)  (function +))
                               ((:right-to-left :bottom-up) (function -)))
                             (rect-border orientation direction box) distance)))
      (set-view-border orientation direction view ordinate))
    (ecase alignment
      ((:left)   (set-view-border :horizontal :left-to-right view (rect-left   box)))
      ((:right)  (set-view-border :horizontal :right-to-left view (rect-right  box)))
      ((:top)    (set-view-border :vertical   :top-down      view (rect-top    box)))
      ((:bottom) (set-view-border :vertical   :bottom-up     view (rect-bottom box)))
      ((:center) (let ((axis (ecase orientation
                               (:horizontal (point-v (rect-center box)))
                               (:vertical   (point-h (rect-center box))))))
                   (set-view-border (other-orientation orientation)
                                    (ecase orientation
                                      (:horizontal :top-down)
                                      (:vertical   :left-to-right))
                                    view
                                    (- axis (truncate (view-layout-breadth orientation view) 2))))))))

;;;--------------------------------------------------------------------

(defclass linear-layout (layout)                           
  ((orientation          :initarg :orientation :initform :horizontal :accessor layout-orientation
                         :type (member :horizontal :vertical))
   (direction            :initarg :direction :initform :left-to-right
                         :accessor layout-direction
                         :type (member :left-to-right :right-to-left :top-down :bottom-up))
   (horizontal-alignment :initarg :horizontal-alignment :initform :center :accessor layout-horizontal-alignment
                         :type (member :left :center :right :full))
   (vertical-alignment   :initarg :vertical-alignment :initform :center :accessor layout-vertical-alignment
                         :type (member :top :center :bottom :full))
   (spacing              :initarg :spacing :initform 0 :accessor layout-spacing
                         :type (or integer (member :elastic))))
  (:documentation "Places subviews linearly.
                                                             
      +---(linear-layout)-----------------------------------+  :orientation :vertical       
      |                            ^                        |  :vertical-alignment :top
      |                            | top-margin             |  :horizontal-alignment :center
      |                            v                        |
      |         +-----------+------------+-----------+      |
      |         |           |            |           |      |
      |         |           +------------+           |      |
      |         |                 ^                  |      |
      |         |                 |                  | right|
      |  left   |                 v                  | mar  |
      | margin  |              +------+              | gin  |
      |<------->|              |      |              |<---->|
      |         |              |      |              |      |
      |         |              +------+              |      |
      |         |                 ^                  |      |
      |         |                 | spacing          |      |
      |         |                 v                  |      |
      |         |       +--------------------+       |      |
      |         |       |                    |       |      |
      |         |       +--------------------+       |      |
      |         |                                    |      |
      |         |             unused space           |      |
      |         +------------------------------------+      |
      |                             ^                       |
      |                             v bottom-margin         |
      +-----------------------------------------------------+

"))



;;                   
;;  justification    :left-to-right                :right-to-left
;;  :left            0                             (- box.width total-length)
;;  :right           (- box.width total-length)    0
;;  :center                   (/ (- box.width total-length) 2)
;;  :full            0 + new-spacing               0 + new-spacing


(defun compute-start-and-spacing (direction justification box spacing n total-length)
  (let ((start))
    (ecase direction
      (:left-to-right
       (ecase justification
         ((:left  :anterior)    (setf start 0))
         ((:right :posterior)   (setf start (- (rect-width box) total-length)))
         (:center  (setf start (truncate (- (rect-width box) total-length) 2)))
         (:full    (setf start 0
                         spacing (if (< 1 n)
                                     (/ (- (rect-width box) (- total-length (* spacing (1- n))))
                                        (1- n))
                                     spacing)))))
      (:right-to-left
       (ecase justification
         ((:left  :posterior)  (setf start (- (rect-width box) total-length)))
         ((:right :anterior)   (setf start 0))
         (:center  (setf start (truncate (- (rect-width box) total-length) 2)))
         (:full    (setf start 0
                         spacing (if (< 1 n)
                                     (/ (- (rect-width box) (- total-length (* spacing (1- n))))
                                        (1- n))
                                     spacing)))))
      (:top-down
       (ecase justification
         ((:top    :anterior)   (setf start 0))
         ((:bottom :posterior)  (setf start (- (rect-height box) total-length)))
         (:center  (setf start (truncate (- (rect-height box) total-length) 2)))
         (:full    (setf start 0
                         spacing (if (< 1 n)
                                     (/ (- (rect-height box) (- total-length (* spacing (1- n))))
                                        (1- n))
                                     spacing)))))
      (:bottom-up
       (ecase justification
         ((:top    :posterior)  (setf start (- (rect-height box) total-length)))
         ((:bottom :anterior)   (setf start 0))
         (:center  (setf start (truncate (- (rect-height box) total-length) 2)))
         (:full    (setf start 0
                         spacing (if (< 1 n)
                                     (/ (- (rect-height box) (- total-length (* spacing (1- n))))
                                        (1- n))
                                     spacing))))))
    (values start spacing)))


(defmethod set-view-size :after ((layout linear-layout) h &optional v)
  (declare (ignorable h v))
  (format-trace '(set-view size :after linear-layout))
  (loop
    :for subview :across (view-subviews layout)
    :do (adjust-layout-to-match-parent subview (case (layout-orientation layout)
                                                 (:horizontal :height)
                                                 (:vertical   :width)))))


(defmethod adjust-layout ((layout linear-layout))
  (with-readers ((orientation layout-orientation)
                 (direction layout-direction)
                 (horizontal-alignment layout-horizontal-alignment)
                 (vertical-alignment layout-vertical-alignment)
                 (spacing layout-spacing)) layout
    (let* ((subviews      (view-subviews layout))
           (n             (length subviews))
           (total-length  (+ (reduce (function +) subviews :key (lambda (view) (view-layout-length orientation view)))
                             (* spacing (1- n))))
           (justification (if (eql orientation :horizontal)
                              horizontal-alignment
                              vertical-alignment))
           (alignment     (if (eql orientation :horizontal)
                              vertical-alignment
                              horizontal-alignment))
           (box           (layout-box layout))
           start)
      (multiple-value-setq (start spacing)
        (compute-start-and-spacing direction justification box spacing n total-length))
      (loop
        :for distance = start :then (+ distance spacing (view-layout-length orientation subview))
        :for subview :across (view-subviews layout)
        :do (view-layout-set-position subview orientation direction distance box alignment)))))



;;;--------------------------------------------------------------------

(defclass flow-layout (layout)
  ((orientation          :initarg :orientation :initform :horizontal :accessor layout-orientation
                         :type (member :horizontal :vertical)
                         :documentation "Orientation of the lines.")
   (justification        :initarg :justification :initform :anterior :accessor layout-justification
                         :type (member :anterior :posterior :left :right :top :bottom :full :center)
                         :documentation "
Justification of elements on a line can be specified relative to the
direction of the flow with :anterior and :posterior, or with an
absolute direction: :left or :right, or :top or :bottom, depending on
the orientation, or as :center or :full.

    :justification :left
                       +-----------+
      +------------+   |           |   +---------+
      |            |   |           |   |         |
    --+------------+---+-----------+---+---------+--------------


    :justification :right
                                   +-----------+
                  +------------+   |           |   +---------+
                  |            |   |           |   |         |
    --------------+------------+---+-----------+---+---------+--


    :justification :center
                             +-----------+
            +------------+   |           |   +---------+
            |            |   |           |   |         |
    --------+------------+---+-----------+---+---------+--------


    :justification :full
                             +-----------+
      +------------+         |           |         +---------+
      |            |         |           |         |         |
    --+------------+---------+-----------+---------+---------+--
")
   (alignment            :initarg :alignment :initform :center :accessor layout-alignment
                         :type (member :previous :next :left :top :center :bottom :right)
                         :documentation "
Alignment of elements on a line can be specified relative to the order
of the lines as :previous or :next, or with an absolute direction:
:left or :right or :top or :bottom, depending on the orientation, or
as :center.

    :alignment :top
    --+------------+---+-----------+---+---------+--------------
      |            |   |           |   |         |
      +------------+   |           |   +---------+
                       +-----------+

    :alignment :center                
                       +------------+
      +-------------+  |            |  +---------+
    --|             |--|            |--|         |--------------
      +-------------+  |            |  +---------+
                       +------------+  

    :alignment :bottom
                       +-----------+
      +------------+   |           |   +---------+
      |            |   |           |   |         |
    --+------------+---+-----------+---+---------+--------------
")
   (horizontal-direction :initarg :horizontal-direction :initform :left-to-right
                         :accessor layout-horizontal-direction
                         :type (member :left-to-right :right-to-left))
   (vertical-direction   :initarg :vertical-direction :initform :top-down
                         :accessor layout-vertical-direction
                         :type (member :top-down :bottom-up))

   (line-justification   :initarg :line-justification :initform :anterior :accessor layout-line-justification
                         :type (member :anterior :posterior :left :right :top :bottom :full :center)
                         :documentation "
Justification of lines can be specified relative to the direction of
the flow with :anterior and :posterior, or with an absolute direction:
:left or :right, or :top or :bottom, depending on the orientation, or
as :center or :full.
")
   (spacing              :initarg :spacing     :initform 0 :accessor layout-spacing
                         :type integer
                         :documentation "Spacing between subviews in the flow (minimum
when :justification :full, exact otherwise).")
   (line-spacing         :initarg :line-spacing :initform 0 :accessor layout-line-spacing
                         :type integer
                         :documentation "Spacing between lines of subviews (minimum
when :line-justification :full, exact otherwise).")))


(defmethod adjust-layout ((layout flow-layout))
  (with-readers ((orientation layout-orientation)
                 (justification layout-justification)
                 (alignment layout-alignment)
                 (horizontal-direction layout-horizontal-direction)
                 (vertical-direction layout-vertical-direction)
                 (line-justification layout-line-justification)
                 (spacing layout-spacing)
                 (line-spacing layout-line-spacing)) layout
    (let* ((subviews         (view-subviews layout))
           (direction        (case orientation
                               (:horizontal horizontal-direction)
                               (:vertical   vertical-direction)))
           (line-direction   (case orientation
                               (:horizontal vertical-direction)
                               (:vertical   horizontal-direction)))
           (box              (layout-box layout))
           (line-length      (rect-length orientation box))
           (lines            (loop
                               :with line = '()
                               :with lines = '()
                               :with length = (- spacing)
                               :for subview :across subviews
                               :for subview-length = (view-layout-length orientation subview)
                               :do (incf length (+ spacing subview-length))
                                   (cond
                                     ((<= length line-length)
                                      (push subview line))
                                     ((< line-length subview-length)
                                      (when line
                                        (push (cons (- length (+ spacing subview-length)) (nreverse line)) lines))
                                      (push (list subview) lines)
                                      (setf line '()
                                            length (- spacing)))
                                     (t
                                      (when line
                                        (push (cons (- length (+ spacing subview-length)) (nreverse line)) lines))
                                      (setf line (list subview)
                                            length subview-length)))
                               :finally (when line
                                          (push (cons length (nreverse line)) lines))
                                        (return (nreverse lines))))
           (lines            (mapcar (lambda (line)
                                       (cons (reduce (function max) (cdr line)
                                                     :key (lambda (view) (view-layout-breadth orientation view))
                                                     :initial-value 0)
                                             line))
                                     lines))
           (nlines           (length lines))
           (total-breadth    (+ (reduce (function +) lines :key (function car))
                                (* (1- nlines) line-spacing)))
           (line-start))
      (multiple-value-setq (line-start line-spacing)
               (compute-start-and-spacing line-direction line-justification box line-spacing nlines total-breadth))
      (loop
        :for line-distance = line-start :then (+ line-distance line-breadth line-spacing)
        :for line :in lines
        :for line-breadth = (car line)
        :for line-length  = (cadr line)
        :for subviews     = (cddr line)
        :for line-box     = (case (print line-direction)
                              (:left-to-right (make-rect (+ (rect-left box) line-distance)
                                                         (rect-top box)
                                                         (+ (rect-left box) line-distance line-breadth)
                                                         (rect-bottom box)))
                              (:right-to-left (make-rect (- (rect-right box) line-distance line-breadth)
                                                         (rect-top box)
                                                         (- (rect-right box) line-distance)
                                                         (rect-bottom box)))
                              (:top-down      (make-rect (rect-left box)
                                                         (+ (rect-top box) line-distance)
                                                         (rect-right box)
                                                         (+ (rect-top box) line-distance line-breadth)))
                              (:bottom-up     (make-rect (rect-left box)
                                                         (- (rect-bottom box) line-distance line-breadth)
                                                         (rect-right box)
                                                         (- (rect-bottom box) line-distance))))
        :do (print (rect-to-list line-box))
        :do (let ((spacing spacing)
                  (start))
              (multiple-value-setq (start spacing)
                (compute-start-and-spacing direction justification line-box spacing (length subviews) line-length))
              (loop
                :for distance = start :then (+ distance spacing (view-layout-length orientation subview))
                :for subview :in subviews
                :do (view-layout-set-position subview orientation direction distance line-box alignment)))))))

