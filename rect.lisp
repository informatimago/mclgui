;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               rect.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    QuickDraw rect functions.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-07-07 <PJB> Created.
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

(eval-when (:compile-toplevel :load-toplevel :execute) ; to be able to use #S in the same file.

  (defstruct (rect
               (:constructor %make-rect))
    (topLeft     (make-point 0 0) :type point)
    (bottomRight (make-point 0 0) :type point))

  (defmethod make-load-form ((object rect) &optional environment)
    (declare (ignore environment))
    ;; => creation-form[, initialization-form]
    `(%make-rect :topleft ,(rect-topleft object)
                 :bottomright ,(rect-bottomright object)))

  );;eval-when

(defun rect-to-list (rect)
  "
RETURN:         A list of two lists containing the coordinates of the
                topLeft and bottomRight points of the RECT rectangle.
"
  (list :topleft (point-to-list (rect-topleft rect))
        :size (point-to-list (rect-size rect))))

(defmethod print-object ((rect rect) stream)
  (declare (stepper disable))
  (if *print-readably*
      (call-next-method)
      #-(and)
      (format stream "(rect :left ~D :top ~D :right ~D :bottom ~D)"
              (rect-left rect) (rect-top rect)
              (rect-right rect) (rect-bottom rect))
      (format stream "#.(rect ~{~S #@~S~^ ~})" (rect-to-list rect)))
  rect)


(defun make-rect (left &optional top right bottom)
  "
The function MAKE-RECT can be called with either:
- a single RECT argument, a copy is then made;
- two points, topLeft and bottomRight;
- four coordinates: left, top, right, bottom.
"
  (declare (stepper disable))
   (cond
    ((and (null top) (null right) (null bottom))
     (copy-rect left))
    ((and (null right) (null bottom))
     (%make-rect :topleft left :bottomright top))
    (t
     (%make-rect :topleft (make-point left top) :bottomright (make-point right bottom)))))

(defun rect (&key left top right bottom width height topleft bottomright size)
  (declare (stepper disable))
  (assert (xor width  size) (width  size) ":WIDTH and :SIZE are exclusive.")
  (assert (xor height size) (height size) ":HEIGHT and :SIZE are exclusive.")
  (flet ((solve (direction
                 label-min1   min1   label-min2   min2
                 label-max1   max1   label-max2   max2
                 label-delta1 delta1 label-delta2 delta2)
           (assert (not (and min1 min2)) (min1 min2) "~S and ~S are exclusive" label-min1 label-min2)
           (assert (not (and max1 max2)) (max1 max2) "~S and ~S are exclusive" label-max1 label-max2)
           (assert (not (and delta1 delta2)) (delta1 delta2) "~S and ~S are exclusive" label-delta1 label-delta2)
           (let ((min   (or min1 min2))
                 (max   (or max1 max2))
                 (delta (or delta1 delta2)))
             (assert (= 2 (+ (if min 1 0) (if max 1 0) (if delta 1 0)))
                     (min max delta)
                     "Exactly 2 ~A specifiers must be given." direction)
             (values (or min (- max delta))
                     (or max (+ min delta))))))
    (multiple-value-bind (left right) (solve :horizontal
                                             :left  left  :topleft     (when topleft     (point-h topleft))
                                             :right right :bottomright (when bottomright (point-h bottomright))
                                             :width width :size        (when size        (point-h size)))
      (multiple-value-bind (top bottom) (solve :vertical
                                               :top    top    :topleft     (when topleft     (point-v topleft))
                                               :bottom bottom :bottomright (when bottomright (point-v bottomright))
                                               :height height :size        (when size        (point-v size)))
        (make-rect left top right bottom)))))

(defun pt2rect (p1 p2)
  (make-rect (min (point-h p1) (point-h p2))
             (min (point-v p1) (point-v p2))
             (max (point-h p1) (point-h p2))
             (max (point-v p1) (point-v p2))))

;; (make-rect 1 2 3 4)
;; #S(rect :topleft 131073 :bottomright 262147)
;; (make-rect (make-point 1 2) (make-point 3 4))
;; #S(rect :topleft 131073 :bottomright 262147)
;; (make-rect (make-rect (make-point 1 2) (make-point 3 4)))
;; #S(rect :topleft 131073 :bottomright 262147)

(defun rect-left   (rect) (point-h (rect-topleft     rect)))
(defun rect-top    (rect) (point-v (rect-topleft     rect)))
(defun rect-right  (rect) (point-h (rect-bottomright rect)))
(defun rect-bottom (rect) (point-v (rect-bottomright rect)))

(defun rect-width  (rect) (- (point-h (rect-bottomright rect))
                             (point-h (rect-topleft rect))))
(defun rect-height (rect) (- (point-v (rect-bottomright rect))
                             (point-v (rect-topleft rect))))

(defun (setf rect-left)   (value rect)
  (setf  (rect-topleft     rect) (make-point value  (point-v (rect-topleft     rect))))
  value)

(defun (setf rect-top)    (value rect)
  (setf  (rect-topleft     rect) (make-point (point-h (rect-topleft     rect))  value))
  value)

(defun (setf rect-right)   (value rect)
  (setf (rect-bottomright rect) (make-point value  (point-v (rect-bottomright rect))))
  value)

(defun (setf rect-bottom)    (value rect)
  (setf (rect-bottomright rect) (make-point (point-h (rect-bottomright rect))  value))
  value)

(defun (setf rect-width)  (new-width rect)
  "Moves the botright point to accomodate the new width"
  (setf (rect-right rect) (+ (rect-left rect) new-width))
  new-width)

(defun (setf rect-height)  (new-height rect)
  "Moves the botright point to accomodate the new height"
  (setf (rect-bottom rect) (+ (rect-top rect) new-height))
  new-height)

(defun assign-rect (dst-rect src-rect)
  (setf (rect-topleft     dst-rect) (rect-topleft     src-rect)
        (rect-bottomright dst-rect) (rect-bottomright src-rect))
  dst-rect)

(defun equal-rect (rect1 rect2)
  "
The EQUAL-RECT function returns T if RECT1 and RECT2 are equal and NIL
otherwise.

RECT1           A rectangle.
RECT2:          A rectangle.
"
  (and (= (rect-topleft     rect1) (rect-topleft     rect2))
       (= (rect-bottomright rect1) (rect-bottomright rect2))
       t))


(defun empty-rect-p (left &optional top right bot)
  "
The EMPTY-RECT-P function returns T if the rectangle specified by arg
is empty (contains no points) and NIL otherwise.  A rectangle is empty
if its bottom coordinate is less than or equal to the top or if the
right coordinate is less than or equal to the left.

LEFT, TOP, RIGHT, BOTTOM:
                These four arguments are used together to specify the
                rectangle. If only left is given, it should be a
                pointer to a rectangle record. If only two arguments
                are given, they should be points specifying the
                upper-left and lowerright coordinates of the
                rectangle. If all four arguments are given, they
                should be coordinates representing the left, top,
                right, and bottom of the rectangle.

"
  (if (and (null top) (null right) (null bot))
    (let ((l (rect-left   left))
          (r (rect-right  left))
          (a (rect-top    left))
          (b (rect-bottom left)))
      (not (and (< l r) (< a b))))
    (empty-rect-p (make-rect left top right bot))))


(defun rect-size   (rect)
  "
RETURN:        The size of the RECT, as a POINT.
"
  (if (empty-rect-p rect)
    (make-point 0 0)
    (make-point (- (point-h (rect-bottomright rect)) (point-h (rect-topleft rect)))
                (- (point-v (rect-bottomright rect)) (point-v (rect-topleft rect))))))


(defun rect-center (rect)
  "
RETURN:         The center point of the RECT.
"
  (make-point (round (+ (rect-left rect) (rect-right  rect)) 2)
              (round (+ (rect-top  rect) (rect-bottom rect)) 2)))



(defun offset-rect (rect h &optional v)
  "
The OFFSET-RECT function moves rectangle H to the right and V down.
It returns the destructively modified rectangle.

RECT:           A rectangle.

H:              Horizontal position.

V:              Vertical position.  If V is NIL (the default), H is
                assumed to represent a point.

RETURN:         RECT
"
  (let ((d  (make-point h v)))
    (setf (rect-topleft     rect) (add-points (rect-topleft     rect) d)
          (rect-bottomright rect) (add-points (rect-bottomright rect) d))
    rect))


(defun inset-rect (rect h &optional v)
  "
The INSET-RECT function shrinks or expands rectangle by H and V.  It
returns the destructively modified rectangle.  If H and V are
positive, the left and right sides and the top and bottom move toward
the center.  If H and V are negative, the sides move outward.

RECT:           A rectangle.

H:              Horizontal position.

V:              Vertical position.  If V is NIL (the default), H is
                assumed to represent a point.

RETURN:         RECT
"
  (let ((d (make-point h v)))
    (setf (rect-topleft     rect) (add-points      (rect-topleft     rect) d)
          (rect-bottomright rect) (subtract-points (rect-bottomright rect) d))
    rect))


(defun intersect-rect (rect1 rect2 &optional (dest-rect (make-rect 0 0)))
  "
The INTERSECT-RECT function stores in DEST-RECT the rectangle created
by the intersection of RECT1 and RECT2 and returns DEST-RECT.  A
single rectangle may be passed as DEST-RECT and as RECT1 or RECT2,
making it unnecessary to allocate one extra rectangle.

RECT1:          A rectangle.

RECT2:          A rectangle.

DEST-RECT:      A rectangle structure used to hold the intersection of
                RECT1 and RECT2.

RETURN:         DEST-RECT
"
  (let ((l1 (rect-left   rect1))
        (t1 (rect-top    rect1))
        (r1 (rect-right  rect1))
        (b1 (rect-bottom rect1))
        (l2 (rect-left   rect2))
        (t2 (rect-top    rect2))
        (r2 (rect-right  rect2))
        (b2 (rect-bottom rect2)))
    (if (or (<= r1 l2) (<= r2 l1)
            (<= b1 t2) (<= b2 t1))
      (setf (rect-left   dest-rect) 0
            (rect-right  dest-rect) 0
            (rect-top    dest-rect) 0
            (rect-bottom dest-rect) 0)
      ;; (and (< l1 r2) (< l2 r1)
      ;;      (< t1 b2) (< t2 b1))
      (setf (rect-left   dest-rect) (max l1 l2)
            (rect-right  dest-rect) (min r1 r2)
            (rect-top    dest-rect) (max t1 t2)
            (rect-bottom dest-rect) (min b1 b2)))
    dest-rect))


(defun union-rect (rect1 rect2 &optional (dest-rect (make-rect 0 0)))
  "
The UNION-RECT function stores in DEST-RECT the rectangle created by
the union of RECT1 and RECT2 and returns DEST-RECT.  A single
rectangle may be passed as DEST-RECT and as RECT1 or RECT2, making it
unnecessary to allocate one extra rectangle.

RECT1:          A rectangle.

RECT2:          A rectangle.

DEST-RECT:      A rectangle structure used to hold the intersection of
                RECT1 and RECT2.

RETURN:         DEST-RECT
"
  (let ((l1 (rect-left   rect1))
        (t1 (rect-top    rect1))
        (r1 (rect-right  rect1))
        (b1 (rect-bottom rect1))
        (l2 (rect-left   rect2))
        (t2 (rect-top    rect2))
        (r2 (rect-right  rect2))
        (b2 (rect-bottom rect2)))
    (setf (rect-left   dest-rect) (min l1 l2)
          (rect-right  dest-rect) (max r1 r2)
          (rect-top    dest-rect) (min t1 t2)
          (rect-bottom dest-rect) (max b1 b2))
    dest-rect))


(defun rect-difference (rect1 rect2)
  "
RETURN:         A (possibly empty when (equal-rect rect1 rect2)) list
                of rectangles whose union is the area in RECT1 that is
                not in RECT2.

RECT1:          A rectangle.

RECT2:          A rectangle.
"
  (let ((inter (intersect-rect rect1 rect2)))
    (cond
      ((equal-rect inter rect1) '())
      ((empty-rect-p inter)     (list rect1))
      (t (let* ((l1 (rect-left   rect1))
                (t1 (rect-top    rect1))
                (r1 (rect-right  rect1))
                (b1 (rect-bottom rect1))
                (li (rect-left   inter))
                (ti (rect-top    inter))
                (ri (rect-right  inter))
                (bi (rect-bottom inter))
                (rtop (make-rect l1 t1 r1 ti))
                (rlef (make-rect l1 ti li bi))
                (rrig (make-rect ri ti r1 bi))
                (rbot (make-rect l1 bi r1 b1))
                (result '()))
           (unless (empty-rect-p rbot) (push rbot result))
           (unless (empty-rect-p rrig) (push rrig result))
           (unless (empty-rect-p rlef) (push rlef result))
           (unless (empty-rect-p rtop) (push rtop result))
           result)))))



(defun point-in-rect-p (rect h &optional v)
  "

The POINT-IN-RECT-P function returns T if the point specified by H and
V is inside rectangle; otherwise, it returns NIL.

RECTANGLE:      A rectangle.

H:              Horizontal position.

V:              Vertical position.  If V is NIL (the default), H is
                assumed to represent a point.
"
  (let ((p (make-point h v))
        (l (rect-left   rect))
        (a (rect-top    rect))
        (r (rect-right  rect))
        (b (rect-bottom rect)))
    (and (<= l (point-h p) r)
         (<= a (point-v p) b)
         t)))


(defun points-to-rect (point1 point2 dest-rect)
  (let ((x1 (point-h point1))
        (y1 (point-v point1))
        (x2 (point-h point2))
        (y2 (point-v point2)))
    (setf (rect-left   dest-rect) (min x1 x2)
          (rect-right  dest-rect) (min y1 y2)
          (rect-top    dest-rect) (max x1 x2)
          (rect-bottom dest-rect) (max y1 y2))
    dest-rect))


(defun point-to-angle (rect h &optional v)
  (let ((p      (make-point h v))
        (size   (rect-size rect))
        (center (rect-center rect)))
    (if (or (zerop (point-h size)) (zerop (point-v size))
            (= p center))
      0
      (let* ((pv  (subtract-points p center))
             (d   (sqrt (coerce (+ (* (point-h pv) (point-h pv))
                                   (* (point-v pv) (point-v pv)))
                                'double-float)))
             (cos-theta (/ (- (point-v pv)) d))
             (angle     (acos cos-theta)))
        (when (minusp (point-h pv))
          (setf angle (- (* 2 pi) angle)))
        (values (round angle (/ pi 180)))))))



(defmacro with-rectangle-arg ((var left &optional top right bottom) &body body)
  "takes a rectangle, two points, or four coordinates and makes a rectangle.
body is evaluated with VAR bound to that rectangle."
  `(let ((,var (make-rect ,left ,top ,right ,bottom)))
     (declare (ignorable ,var)) ; for now
    ,@body))


;;;; THE END ;;;;
