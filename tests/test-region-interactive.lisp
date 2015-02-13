;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               test-region-interactive.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Implements an interactive window and view to test regions.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-02-12 <PJB> Extracted from region-new-stuff.lisp
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2015 - 2015
;;;;    
;;;;    This program is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU Affero General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU Affero General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU Affero General Public License
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************
(in-package :ui)

(defparameter *hole*
  (offset-region
   (union-region
    (union-region
     ;; center box
     (rect-region 40 40 60 60)
     ;; side bar
     (rect-region 140 0 160 100))
    (union-region
     ;; four spikes:
     (union-region (union-region (rect-region -20 40 0 60)
                                 (rect-region 40 -20 60 0))
                   (union-region (rect-region 100 40 120 60)
                                 (rect-region 40 100 60 120)))
     ;; four walls:
     (union-region  (union-region (rect-region 0 0 100 20)
                                  (rect-region 0 80 100 100))
                    (union-region (rect-region 0 0 20 100)
                                  (rect-region 80 0 100 100)))))
   100 50))


(defparameter *Z*
  (offset-region
   (union-region (rect-region 40 0 100 50)
                 (rect-region 0 50 60 100))
   50 30))



(defparameter *F*
  (offset-region
   (union-region (rect-region 0 0 20 100)
                 (union-region (rect-region 0 0 100 20)
                               (rect-region 0 40 60 60)))
   50 30))

(defparameter *c*
  (offset-region
   (difference-region (rect-region 0 0 100 100)
                      (rect-region 20 20 100 80))
   50 30))

(defparameter *i*
  (offset-region
   (rect-region 70 -10 90 110)
   50 30))

(defparameter *test/regions*
  (list
   
   ;;        +---+
   ;;        |   |
   ;;        +---+
   ;; +---+
   ;; |   |
   ;; +---+
   (union-region (rect-region 200 100 250 130)
                 (rect-region 100 130 150 160))
   (union-region (rect-region 200 100 250 130)
                 (rect-region 150 130 200 160))
   

   ;; +---+
   ;; |   |
   ;; +---+
   ;;        +---+
   ;;        |   |
   ;;        +---+
   (union-region (rect-region 100 100 150 130)
                 (rect-region 150 130 200 160))
   (union-region (rect-region 250 100 300 130)
                 (rect-region 150 130 200 160))
   
   ;;        +---+
   ;;        |   |
   ;; +---+  |   |
   ;; |   |  +---+
   ;; |   |
   ;; +---+
   (union-region (rect-region 200 100 250 160)
                 (rect-region 100 130 150 190))
   
   ;;     +---+
   ;;     |   |
   ;; +---+   |
   ;; |   +---+
   ;; |   |
   ;; +---+
   (union-region (rect-region 200 100 250 160)
                 (rect-region 150 130 200 190))


   
   ;;   +---+       
   ;;   |   |       
   ;;   |   |  +---+
   ;;   +---+  |   |
   ;;          |   |
   ;;          +---+
   (union-region (rect-region 100 100 150 160)
                 (rect-region 200 130 250 190))
   
   ;; +---+       
   ;; |   |       
   ;; |   +---+   
   ;; +---+   |   
   ;;     |   |   
   ;;     +---+   
   (union-region (rect-region 150 100 200 160)
                 (rect-region 200 130 250 190))

   ;; +----+
   ;; |    |
   ;; +--+-+----+
   ;;    |      |
   ;;    +------+
   (union-region (rect-region 100 100 200 130)
                 (rect-region 150 130 250 160))
   
   ;; +----------+
   ;; |          |
   ;; +-----+----+
   ;;       |    |
   ;;       +----+
   (union-region (rect-region 100 100 200 130)
                 (rect-region 150 130 200 160))
   
   ;; +----------+
   ;; |          |
   ;; +--+-----+-+
   ;;    |     |
   ;;    +-----+
   (union-region (rect-region 100 100 250 130)
                 (rect-region 150 130 200 160))
   
   ;; +-----+
   ;; |     |
   ;; +-----+----+
   ;; |          |
   ;; +----------+
   (union-region (rect-region 100 100 150 130)
                 (rect-region 100 130 200 160))

   ;; +----+
   ;; |    |
   ;; +----+
   ;; |    |
   ;; +----+
   (union-region (rect-region 100 100 150 130)
                 (rect-region 100 130 150 160))
   
   ;; +----------+
   ;; |          |
   ;; +-----+----+
   ;; |     |
   ;; +-----+
   (union-region (rect-region 100 100 200 130)
                 (rect-region 100 130 150 160))
   
   ;;    +-----+
   ;;    |     |
   ;; +--+-----+-+
   ;; |          |
   ;; +----------+
   (union-region (rect-region 150 100 200 130)
                 (rect-region 100 130 250 160))
   
   ;;       +----+
   ;;       |    |
   ;; +-----+----+
   ;; |          |
   ;; +----------+
   (union-region (rect-region 150 100 200 130)
                 (rect-region 100 130 200 160))
   
   ;;    +------+
   ;;    |      |
   ;; +--+-+----+
   ;; |    |
   ;; +----+
   (union-region (rect-region 150 100 250 130)
                 (rect-region 100 130 200 160))

   
   ;; +------------+  0
   ;; |            |
   ;; |   +----+   | 20
   ;; |   |    |   |
   ;; +---+    +---+ 40
   ;; +------------+
   ;; |            |
   ;; +------------+ 60
   (xor-region (rect-region 100 100 200 200)
               (rect-region 130 130 170 170))
   
   ;; +------------+    0
   ;; |            |
   ;; |   +----+   |   20
   ;; |   |    |   |
   ;; +---+    +---+   40
   ;;   +------------+
   ;;   |            |
   ;;   +------------+ 60
   (union-region (xor-region (rect-region 100 100 200 170)
                             (rect-region 130 130 170 170))
                 (rect-region 110 170 210 200))
   
   ;;   +------------+  0
   ;;   |            |
   ;;   |   +----+   | 20
   ;;   |   |    |   |
   ;;   +---+    +---+ 40
   ;; +------------+
   ;; |            |
   ;; +------------+   60
   (union-region (xor-region (rect-region 100 100 200 170)
                             (rect-region 130 130 170 170))
                 (rect-region 90 170 190 200))
   
   ;; (assert (tline-goes (first  (tpath-bottom-lines merged)) 100 40 050 40))
   ;; +------------+
   ;; |            |
   ;; |   +----+   |
   ;; |   |    |   |
   ;; +---+    +---+
   ;;   +--------+
   ;;   |        |
   ;;   +--------+
   (union-region (xor-region (rect-region 100 100 200 170)
                             (rect-region 130 130 170 170))
                 (rect-region 110 170 190 200))
   
   ;;   100 200 300 400
   ;;   +------------+    0 
   ;;   |            |
   ;;   |   +----+   |   20
   ;;   |   |    |   |
   ;;   +---+    +---+   40
   ;; +----------------+
   ;; |                |
   ;; +----------------+ 60
   (union-region (xor-region (rect-region 100 100 200 170)
                             (rect-region 130 130 170 170))
                 (rect-region 90 170 210 200))

   
   ;; +------------+  0
   ;; |            |
   ;; |   +----+   | 20
   ;; |   |    |   |
   ;; +---+    +---+ 40
   ;; +------------+
   ;; |            |
   ;; +------------+ 60
   (union-region (xor-region (rect-region 100 100 300 170)
                             (union-region (rect-region 130 130 170 170)
                                           (rect-region 230 130 270 170)))
                 (rect-region 90 170 310 200))

   *z*
   *hole*
   *f*

   (union-region      *hole* *f*)
   (intersect-region  *hole* *f*)
   (difference-region *hole* *f*)
   (difference-region *hole* *f*)
   (xor-region        *hole* *f*)
   
   (disc-region 100 100 70)
   
   (let ((circle (disc-region 100 100 70)))     
     (inset-region (copy-region circle) 8 8))
   
   (let* ((circle (disc-region 100 100 70))
          (i      (inset-region (copy-region circle) 8 8)))
     (difference-region circle i))
   
   (difference-region (disc-region 150 100 60)
                      (disc-region 150 100 30))

   (xor-region (rect-region 50 140 250 160)
               (rect-region 140 50 160 250))

   (xor-region
    (difference-region (disc-region 150 100 60)
                       (disc-region 150 100 30))
    (xor-region (rect-region 50 140 250 160)
                (rect-region 140 50 160 250)))

   *c* *i*
   (xor-region *c* *i*)
   ))


#-(and) (#S(region :bounds #S(rect :topleft 5242980 :bottomright 14418230)
           :segments
           #((80 . #(100 190 200 210))
             (100 . #(100 120 150 250 290 310))
             (120 . #(100 150 160 170 230 250 290 310))
             (140 . #(100 120 130 170 190 210 230 270 290 310))
             (160 . #(100 120 150 170 230 250 290 310))
             (180 . #(150 250 290 310))
             (200 . #(190 210))
             (220 . #())))
  (stroke-region (offset-region (xor-region        *hole* *f*) 50 50))
  nil)
;; (setf (test-regions *w*) *test/regions*)

(defun debug-region (r)
  (with-focused-view *w*
    (erase-rect* 0 0 1000 1000))
  (bezier-path-from-region r))

(defun stroke-region (r)
  (with-focused-view *w*
    (erase-rect* 0 0 1000 1000))
  (let ((path (bezier-path-from-region r)))
    (with-focused-view *w*
      (erase-rect* 0 0 1000 1000)
      [path stroke])))

(defun fill-region (r)
  (with-focused-view *w*
    (erase-rect* 0 0 1000 1000))
  (let ((path (bezier-path-from-region r)))
    (with-focused-view *w*
      (erase-rect* 0 0 1000 1000)
      [path fill])))


(defgeneric current-region (window))
(defgeneric update-test (window))
(defgeneric (setf region-view-region) (region view))

(defclass region-view (simple-view)
  ((region :initarg :region :accessor region-view-region)))

(defmethod view-draw-contents ((self region-view))
  (with-focused-view self
    (let ((path (bezier-path-from-region (region-view-region self))))
      (erase-rect* 0 0 1000 1000)
      (with-fore-color *yellow-color*
        [path fill])
      [path stroke])
    (let ((bounds (view-bounds self)))
      (draw-rect* (rect-left bounds) (rect-top bounds)
                  (rect-width bounds) (rect-height bounds)))))

(defmethod (setf region-view-region) :after (new-region (self region-view))
  (declare (ignorable new-region))
  (view-draw-contents self))


(defclass region-test-window (coordinated-window)
  ((tests :initform *test/regions* :initarg :test-regions :accessor test-regions)
   (current :initform 0 :accessor test-region-index)
   (region :initarg :region :accessor region-view)
   (current-item :reader test-current-item))
  (:default-initargs :window-title "Region Test"))

(defmethod initialize-instance :after ((self region-test-window) &key &allow-other-keys)
  (let ((size (view-size self))
        (subviews (concatenate 'list (view-subviews self))))
    (dolist (subview subviews) (remove-subviews self subview))
    (setf (region-view self)
          (make-instance 'region-view
                         :view-container self
                         :view-position #@(0 40)
                         :view-size (make-point (point-h size) (- (point-v size) 40))
                         :region (elt (test-regions self) (test-region-index self))))
    (dolist (subview subviews) (add-subviews self subview)))
  (make-instance 'button-dialog-item
                 :view-container self
                 :view-size #@(90 20)
                 :view-position #@(130 10)
                 :dialog-item-text "Previous"
                 :dialog-item-action (lambda (item)
                                       (declare (ignore item))
                                       (setf (test-region-index self)
                                             (mod (1- (test-region-index self))
                                                  (length (test-regions self))))
                                       (update-test self)))
  (make-instance 'button-dialog-item
                 :view-container self
                 :view-size #@(90 20)
                 :view-position #@(240 10)
                 :dialog-item-text "Next"
                 :dialog-item-action (lambda (item)
                                       (declare (ignore item))
                                       (setf (test-region-index self)
                                             (mod (1+ (test-region-index self))
                                                  (length (test-regions self))))
                                       (update-test self)))
  (setf (slot-value self 'current-item)
        (make-instance 'dialog-item
                       :view-container self
                       :view-size #@(50 20)
                       :view-position #@(350 10)
                       :dialog-item-text "0"))
  (update-test self))

(defmethod view-draw-contents ((self region-test-window))
  (call-next-method)
  (unwind-protect
       (call-next-method)
    (with-focused-view self
      (dovector (view (view-subviews self))
        (reporting-errors (view-draw-contents view))))))

(defmethod set-view-size :after ((self region-test-window) h &optional v)
  (declare (ignore h v))
  (let ((size (view-size self)))
    (set-view-size (region-view self) (point-h size) (- (point-v size) 40))))

(defmethod current-region ((self region-test-window))
  (elt (test-regions self) (test-region-index self)))
(defmethod update-test ((self region-test-window))
  (set-dialog-item-text (test-current-item self)
                        (format nil "~D/~D"
                                (1+ (test-region-index self))
                                (length (test-regions self))))
  (setf (region-view-region (region-view self)) (current-region self)))

;;;; THE END ;;;;
