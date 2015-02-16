;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               test-region-path.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     MCLGUI
;;;;DESCRIPTION
;;;;    
;;;;    Test region paths.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-02-12 <PJB> Extracted from region-new-stuff.
;;;;BUGS
;;;;LEGAL
;;;;    GPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2015 - 2015
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
(in-package :ui)

;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (pushnew :region/run-time-checks *features*))
;; (setf *features* (remove :region/run-time-checks *features*))

(defmacro tpath-dolines ((line tpath &optional result) &body body)
  `(tline-dolines (,line (tpath-lines ,tpath) ,result)
     ,@body))

(defun list-tpath-lines (tpath)
  (let ((lines '()))
    (tpath-dolines (line tpath (nreverse lines))
      (push line lines))))

(defun tline-coordinates (tline)
  "RETURN: a list of coordinates (fx fy tx ty) of the points of the tline."
  (list (tline-from-x tline)
        (tpoint-y (tline-from-point tline))
        (tline-to-x tline)
        (tpoint-y (tline-to-point tline))))

(defun tpath-coordinates (tpath)
  "RETURN: a list of coordinates of the points on tpath (the first is the last)."
  (let ((coordinates '()))
    (push (tpoint-x (tline-from-point (tpath-lines tpath))) coordinates)
    (push (tpoint-y (tline-from-point (tpath-lines tpath))) coordinates)
    (tpath-dolines (line tpath (nreverse coordinates))
      (push (tpoint-x (tline-to-point line)) coordinates)
      (push (tpoint-y (tline-to-point line)) coordinates))))

(defun tpath-from-coordinates (&rest coordinates)
  (let ((clen (length coordinates)))
    (assert (evenp clen))
    (assert (<= 6 clen)))
  (let* ((lines (loop
                  :while (cdr coordinates)
                  :for from-pt = (make-tpoint :x (pop coordinates) :y (pop coordinates)) :then to-pt
                  :for to-pt = (make-tpoint :x (pop coordinates) :y (pop coordinates))
                  :collect (make-tline :from-point from-pt :to-point to-pt)))
         (first (first lines))
         (last  (first (last lines))))
    (assert (= (tpoint-x (tline-from-point first)) (tpoint-x (tline-to-point last))))
    (assert (= (tpoint-y (tline-from-point first)) (tpoint-y (tline-to-point last))))
    (setf (tline-from-line first) last
          (tline-to-line last) first
          (tline-to-point last) (tline-from-point first))
    (loop :for current :on lines
          :for prev = first :then (first current)
          :for curr = (second current)
          :while curr
          :do (setf (tline-from-line curr) prev
                    (tline-to-line   prev) curr)
              (tline-update-links prev))
    (make-tpath :lines (first lines))))


(defun tline-goes (tline from-x from-y to-x to-y)
  (and (= (tpoint-x (tline-from-point tline)) from-x)
       (= (tpoint-y (tline-from-point tline)) from-y)
       (= (tpoint-x (tline-to-point tline)) to-x)
       (= (tpoint-y (tline-to-point tline)) to-y)))


(defun tpath-goes (tpath &rest coordinates)
  (let ((from-x (pop coordinates))
        (from-y (pop coordinates))
        (to-x   (pop coordinates))
        (to-y   (pop coordinates)))
    (tpath-dolines (line tpath (null coordinates))
      (unless (and to-y (tline-goes line from-x from-y to-x to-y))
        (return-from tpath-goes nil))
      (shiftf from-x to-x (pop coordinates))
      (shiftf from-y to-y (pop coordinates)))))


(defun nth-tline (n tline)
  (loop
    :while (plusp n)
    :do (decf n)
        (setf tline (tline-to-line tline))
    :finally (return tline)))


(defun lines-from-tpath (tpath)
  (let ((lines '()))
    (flet ((point (tpoint)
             (list (tpoint-x tpoint)
                   (tpoint-y tpoint))))
      (push (point (tline-from-point (tpath-lines tpath))) lines)
      (tpath-dolines (line tpath (nreverse lines))
        (push (point (tline-to-point line)) lines)))))


#+region/run-time-checks
(defvar *checked-objects* nil)

(defmacro with-checked-objects (&body body)
  #+region/run-time-checks
  (let ((vbody (gensym)))
    `(flet ((,vbody () ,@body))
       (if *checked-objects*
           (,vbody)
           (let ((*checked-objects* (make-hash-table)))
             (,vbody)))))
  #-region/run-time-checks
  `(progn ,@body))


#+region/run-time-checks
(defun check-tline-links (tline &key name)
  (check-type tline tline (format nil "a tline~@[ ~A~]" name))
  ;; check that the tline circular list contains at least three elements:
  (let ((to   (tline-to-line tline))
        (from (tline-from-line tline)))
    (check-type to   tline (format nil "a tline~@[ ~A~]" `(tline-to-line ,name)))
    (check-type from tline (format nil "a tline~@[ ~A~]" `(tline-from-line ,name)))
    (assert (not (eq tline to))   () "tline~@[ ~A~] shouldn't have itself as to-line" name)
    (assert (not (eq tline from)) () "tline~@[ ~A~] shouldn't have itself as from-line" name)
    (assert (not (eq to    from)) () "tline~@[ ~A~] shouldn't have same from-line as to-line" name)
    (let ((to-to     (tline-to-line to))
          (from-from (tline-from-line from)))
      (check-type to-to     tline (format nil "a tline~@[ ~A~]" `(tline-to-line (tline-to-line ,name))))
      (check-type from-from tline (format nil "a tline~@[ ~A~]" `(tline-from-line (tline-from-line ,name))))
      (assert (not (eq tline to-to))     () "tline~@[ ~A~] shouldn't have itself as to-line of to-line" name)
      (assert (not (eq tline from-from)) () "tline~@[ ~A~] shouldn't have itself as from-line of from-line" name))
    ;; check double-links:
    (let ((to-from  (tline-to-line from))
          (from-to  (tline-from-line to)))
      (assert (eq tline to-from) () "tline~@[ ~A~] should have itself as to-line of from-line" name)
      (assert (eq tline from-to) () "tline~@[ ~A~] should have itself as from-line of to-line" name))
    ;; check points:
    (let ((to-point   (tline-to-point tline))
          (from-point (tline-from-point tline))
          (to-point-of-from (tline-to-point from))
          (from-point-of-to (tline-from-point to)))
      (assert (equalp from-point-of-to to-point)   () "inconsitent to-point vs. from-point of to-tline of ~@[ ~A~]" name)
      (assert (equalp to-point-of-from from-point) () "inconsitent from-point vs. to-point of from-tline of ~@[ ~A~]" name)))
  tline)


(defun tpoint-to-list (tpoint)
  (list (tpoint-x tpoint) (tpoint-y tpoint)))

(defun tline-to-list (tline)
  (list (tpoint-to-list (tline-from-point tline))
        (tpoint-to-list (tline-to-point tline))))

(defun fmt-line (stream argument colon at &rest parameters)
  (declare (ignore colon at parameters))
  (check-type argument tline)
  (format stream "~S" (tline-to-list argument))
  (values))

(defmethod print-object ((line tline) stream)
  (if *print-readably*
      (call-next-method)
      (print-unreadable-object (line stream :identity t :type t)
        (format stream "~S" (tline-to-list line))
        #-(and) (format stream "~{~S~^ ~}" (list :from-point (tline-from-point line)
                                                 :to-point (tline-to-point line)))
        #+region/run-time-checks
        (let ((message nil))
          (unless (ignore-errors (setf message (with-output-to-string (*error-output*)
                                                 (check-tline-links line))) )
            (when message (format stream "~A" message))))))
  line)

(defmethod print-object ((path tpath) stream)
  (if *print-readably*
      (call-next-method)
      (print-unreadable-object (path stream :identity t :type t)
        (format stream "~S " (group-by (tpath-coordinates path) 2))
        (format stream ":bottom-lines ~S :new-bottom-lines ~S"
                (mapcar (function tline-to-list) (tpath-bottom-lines path))
                (mapcar (function tline-to-list) (tpath-new-bottom-lines path)))))
  path)


(defun check-tpath/tline-links (tpath tline &key name)
  #-region/run-time-checks
  (declare (ignore tpath name))
  #+region/run-time-checks
  (progn
    (check-type tpath tpath (format nil "a tpath~@[ ~A~]" `(tpath of ,name)))
    (let ((tline.tpath (gethash tline *checked-objects*)))
      (if tline.tpath
          (assert (eq tpath tline.tpath) () "tline~@[ ~A~] has two tpaths: ~A (passed) and ~A (recorded)" tpath tline.tpath)
          (setf (gethash tline *checked-objects*) tpath)))
    (check-tline-links tline :name name))
  tline)


#+region/run-time-checks
(defun check-tline-double-chain (tpath tline &key name)
  (with-checked-objects
    (loop
      :for i :from 0
      :for slow = tline :then (tline-to-line slow)
      :for fast = (tline-to-line tline) :then (tline-to-line (tline-to-line fast))
      :until (eq slow fast)
      :do (check-tpath/tline-links tpath slow :name `(nth ,i ,name))
          (check-tpath/tline-links tpath fast :name `(nth ,(1+ (* 2 i)) ,name))
          (check-tpath/tline-links tpath (tline-to-line fast) :name `(nth ,(* 2 (1+ i)) ,name))))
  tline)


(defun tline-member (tline tlines)
  (tline-dolines (line tlines nil)
    (when (eq tline line) (return t))))


#+region/run-time-checks
(defun check-tpath-bottom-lines (tpath bottom-lines &key name)
  (with-checked-objects
    (loop
      :with prev-tline-to-x = most-negative-long-float
      :for i :from 0
      :for tline :in bottom-lines
      :do (assert (<= prev-tline-to-x (tline-to-x tline)))
          (setf prev-tline-to-x (tline-to-x tline))
          (assert (tline-member tline (tpath-lines tpath))
                  ()  "~A should be member of ~A"
                  `(nth ,i ,(or name 'bottom-lines))
                  `(tpath-lines ,(or name 'tpath))))))


(defun check-tpath (tpath &key name)
  #-region/run-time-checks
  (declare (ignore name))
  #+region/run-time-checks
  (with-checked-objects
    (check-tline-double-chain tpath (tpath-lines tpath) :name `(tpath-lines ,name))
    (check-tpath-bottom-lines tpath (tpath-bottom-lines tpath) :name `(tpath-bottom-lines ,name))
    (check-tpath-bottom-lines tpath (tpath-new-bottom-lines tpath) :name `(tpath-new-bottom-lines ,name)))
  tpath)


#+region/run-time-checks
(defun check-disjoint-tpaths (tpaths &key name)
  (with-checked-objects
    (loop :for i :from 0
          :for tpath :in tpaths
          :do (check-tpath tpath :name `(nth ,i ,name))))
  tpaths)



(defun test/tpath-contains-line-p ()
  (let* ((path (make-rect-tpath 0 0 100 100))
         (line))
    (assert (not (tpath-contains-line-p path (make-tline))))
    (assert (tpath-contains-line-p path (setf line (tpath-lines path))))
    (assert (tpath-contains-line-p path (setf line (tline-to-line line))))
    (assert (tpath-contains-line-p path (setf line (tline-to-line line))))
    (assert (tpath-contains-line-p path (setf line (tline-to-line line))))
    (assert (tpath-contains-line-p path (setf line (tline-to-line line))))
    :success))




(defun test/merge-tpaths ()
  ;;        +---+
  ;;        |   |
  ;;        +---+
  ;; +---+
  ;; |   |
  ;; +---+
  (let ((above (make-rect-tpath 100  0 150 20))
        (below (make-rect-tpath   0 20  50 40))
        top-line)
    (check-tpath above :name 'above)
    (check-tpath below :name 'below)
    (multiple-value-bind (merged internal new-above-lines new-top-lines)
        (merge-tpaths above (first (tpath-bottom-lines above))
                      below (setf top-line (tpath-lines below)))
      (declare (ignorable new-above-lines))
      (assert (eq :left merged))
      (assert (null internal))
      (assert (equalp (list top-line) new-top-lines))))
  ;; +---+
  ;; |   |
  ;; +---+
  ;;        +---+
  ;;        |   |
  ;;        +---+
  (let ((above (make-rect-tpath   0  0 50 20))
        (below (make-rect-tpath 100 20 150 40))
        top-line)
    (check-tpath above :name 'above)
    (check-tpath below :name 'below)
    (multiple-value-bind (merged internal new-above-lines new-top-lines)
        (merge-tpaths above (first (tpath-bottom-lines above))
                      below (setf top-line (tpath-lines below)))
      (declare (ignorable new-above-lines))
      (assert (eq :right merged))
      (assert (null internal))
      (assert (equalp (list top-line) new-top-lines))))
  
  ;; +----+
  ;; |    |
  ;; +--+-+----+
  ;;    |      |
  ;;    +------+
  (let ((above (make-rect-tpath   0  0 200 20))
        (below (make-rect-tpath 100 20 300 40))
        top-line)
    (declare (ignorable top-line))
    (check-tpath above :name 'above)
    (check-tpath below :name 'below)
    (multiple-value-bind (merged internal new-above-lines new-top-lines)
        (merge-tpaths above (first (tpath-bottom-lines above))
                      below (setf top-line (tpath-lines below)))
      (declare (ignorable new-above-lines))
      (check-tpath merged :name 'merged)
      (assert (eq above merged))
      (assert (tpath-goes merged 0 0 200 0 200 20 300 20 300 40 100 40 100 20 0 20 0 0))
      (assert (null internal))
      (assert (= 1 (length new-top-lines)))
      (assert (tline-goes (first new-top-lines) 200 20 300 20))))
  ;; +----------+
  ;; |          |
  ;; +-----+----+
  ;;       |    |
  ;;       +----+
  (let ((above (make-rect-tpath   0  0 200 20))
        (below (make-rect-tpath 100 20 200 40))
        top-line)
    (declare (ignorable top-line))
    (check-tpath above :name 'above)
    (check-tpath below :name 'below)
    (multiple-value-bind (merged internal new-above-lines new-top-lines)
        (merge-tpaths above (first (tpath-bottom-lines above))
                      below (setf top-line (tpath-lines below)))
      (declare (ignorable new-above-lines))
      (check-tpath merged :name 'merged)
      (assert (eq above merged))
      (assert (tpath-goes merged 0 0 200 0 200 40 100 40 100 20 0 20 0 0))
      (assert (null internal))
      (assert (null new-top-lines))))
  ;; +----------+
  ;; |          |
  ;; +--+-----+-+
  ;;    |     |
  ;;    +-----+
  (let ((above (make-rect-tpath   0  0 300 20))
        (below (make-rect-tpath 100 20 200 40))
        top-line)
    (declare (ignorable top-line))
    (check-tpath above :name 'above)
    (check-tpath below :name 'below)
    (multiple-value-bind (merged internal new-above-lines new-top-lines)
        (merge-tpaths above (first (tpath-bottom-lines above))
                      below (setf top-line (tpath-lines below)))
      (declare (ignorable new-above-lines))
      (check-tpath merged :name 'merged)
      (assert (eq above merged))
      (assert (tpath-goes merged 0 0 300 0 300 20 200 20 200 40 100 40 100 20 0 20 0 0))
      (assert (null internal))
      (assert (null new-top-lines))))
  ;; +-----+
  ;; |     |
  ;; +-----+----+
  ;; |          |
  ;; +----------+
  (let ((above (make-rect-tpath   0  0 100 20))
        (below (make-rect-tpath   0 20 200 40))
        top-line)
    (declare (ignorable top-line))
    (check-tpath above :name 'above)
    (check-tpath below :name 'below)
    (multiple-value-bind (merged internal new-above-lines new-top-lines)
        (merge-tpaths above (first (tpath-bottom-lines above))
                      below (setf top-line (tpath-lines below)))
      (declare (ignorable new-above-lines))
      (check-tpath merged :name 'merged)
      (assert (eq above merged))
      (assert (tpath-goes merged 0 0 100 0 100 20 200 20 200 40 0 40 0 0))
      (assert (null internal))
      (assert (= 1 (length new-top-lines)))
      (assert (tline-goes (first new-top-lines) 100 20 200 20))))
  ;; +----+
  ;; |    |
  ;; +----+
  ;; |    |
  ;; +----+
  (let ((above (make-rect-tpath   0  0 200 20))
        (below (make-rect-tpath   0 20 200 40))
        top-line)
    (declare (ignorable top-line))
    (check-tpath above :name 'above)
    (check-tpath below :name 'below)
    (multiple-value-bind (merged internal new-above-lines new-top-lines)
        (merge-tpaths above (first (tpath-bottom-lines above))
                      below (setf top-line (tpath-lines below)))
      (declare (ignorable new-above-lines))
      (check-tpath merged :name 'merged)
      (assert (eq above merged))
      (assert (tpath-goes merged 0 0 200 0 200 40 0 40 0 0))
      (assert (null internal))
      (assert (null new-top-lines))))
  ;; +----------+
  ;; |          |
  ;; +-----+----+
  ;; |     |
  ;; +-----+
  (let ((above (make-rect-tpath   0  0 200 20))
        (below (make-rect-tpath   0 20 100 40))
        top-line)
    (declare (ignorable top-line))
    (check-tpath above :name 'above)
    (check-tpath below :name 'below)
    (multiple-value-bind (merged internal new-above-lines new-top-lines)
        (merge-tpaths above (first (tpath-bottom-lines above))
                      below (setf top-line (tpath-lines below)))
      (declare (ignorable new-above-lines))
      (check-tpath merged :name 'merged)
      (assert (eq above merged))
      (assert (tpath-goes merged 0 0 200 0 200 20 100 20 100 40 0 40 0 0))
      (assert (null internal))
      (assert (null new-top-lines))))
  ;;    +-----+
  ;;    |     |
  ;; +--+-----+-+
  ;; |          |
  ;; +----------+
  (let* ((above (make-rect-tpath   100  0 200 20))
         (below (make-rect-tpath   0   20 300 40))
         (below-bottom-line (first (tpath-bottom-lines below)))
         top-line)
    (declare (ignorable top-line))
    (check-tpath above :name 'above)
    (check-tpath below :name 'below)
    (multiple-value-bind (merged internal new-above-lines new-top-lines)
        (merge-tpaths above (first (tpath-bottom-lines above))
                      below (setf top-line (tpath-lines below)))
      (declare (ignorable new-above-lines))
      (check-tpath merged :name 'merged)
      (assert (eq below-bottom-line (first (tpath-new-bottom-lines merged)))
              () "bottom-line of below = ~S~%new bottom-line of merged = ~S~%"
              below-bottom-line (first (tpath-bottom-lines merged)))
      (assert (eq above merged))
      (assert (tpath-goes merged 100 0 200 0 200 20 300 20 300 40 0 40 0 20 100 20 100 0))
      (assert (null internal))
      (assert (= 2 (length new-top-lines)))
      (assert (tline-goes (first  new-top-lines) 0 20 100 20))
      (assert (tline-goes (second new-top-lines) 200 20 300 20))))
  ;;       +----+
  ;;       |    |
  ;; +-----+----+
  ;; |          |
  ;; +----------+
  (let ((above (make-rect-tpath   100  0 200 20))
        (below (make-rect-tpath     0 20 200 40))
        top-line)
    (declare (ignorable top-line))
    (check-tpath above :name 'above)
    (check-tpath below :name 'below)
    (multiple-value-bind (merged internal new-above-lines new-top-lines)
        (merge-tpaths above (first (tpath-bottom-lines above))
                      below (setf top-line (tpath-lines below)))
      (declare (ignorable new-above-lines))
      (check-tpath merged :name 'merged)
      (assert (eq above merged))
      (assert (tpath-goes merged 100 0   200 0   200 40   0 40   0 20   100 20  100 0))
      (assert (null internal))
      (assert (= 1 (length new-top-lines)))
      (assert (tline-goes (first  new-top-lines) 0 20 100 20))))
  ;;    +------+
  ;;    |      |
  ;; +--+-+----+
  ;; |    |
  ;; +----+
  (let ((above (make-rect-tpath   100  0 300 20))
        (below (make-rect-tpath     0 20 200 40))
        top-line)
    (declare (ignorable top-line))
    (check-tpath above :name 'above)
    (check-tpath below :name 'below)
    (multiple-value-bind (merged internal new-above-lines new-top-lines)
        (merge-tpaths above (first (tpath-bottom-lines above))
                      below (setf top-line (tpath-lines below)))
      (declare (ignorable new-above-lines))
      (check-tpath merged :name 'merged)
      (assert (eq above merged))
      (assert (tpath-goes merged 100 0  300 0 300 20 200 20 200 40 0 40 0 20 100 20 100 0))
      (assert (null internal))
      (assert (= 1 (length new-top-lines)))
      (assert (tline-goes (first  new-top-lines) 0 20 100 20))))
  ;; 100 200 300 400
  ;; +------------+  0
  ;; |            |
  ;; |   +----+   | 20
  ;; |   |    |   |
  ;; +---+    +---+ 40
  ;; +------------+
  ;; |            |
  ;; +------------+ 60
  (let ((above (tpath-from-coordinates   100 0  400 0  400 40 300 40 300 20 200 20 200 40 100 40 100 0))
        (below (make-rect-tpath          100 40 400 60)))
    (setf (tpath-bottom-lines above) (list (nth-tline 6 (tpath-lines above))
                                           (nth-tline 2 (tpath-lines above))))
    (check-tpath above :name 'above)
    (check-tpath below :name 'below)
    (multiple-value-bind (merged internal new-above-lines new-top-lines)
        (merge-tpaths above (first (tpath-bottom-lines above))
                      below (tpath-lines below))
      (declare (ignorable new-above-lines))
      (check-tpath merged :name 'merged)
      (assert (eq above merged))
      (assert (tpath-goes merged 100 0 400 0 400 40 300 40 300 20 200 20 200 40 400 40 400 60 100 60 100 0))
      (assert (null internal))
      (assert (= 1 (length (tpath-bottom-lines merged))))
      (assert (tline-goes (first (tpath-bottom-lines merged)) 400 40 300 40))
      (assert (= 1 (length new-top-lines)))
      (assert (tline-goes (first new-top-lines) 200 40 400 40))
      (multiple-value-bind (merged internal new-above-lines new-top-lines)
          (merge-tpaths above (first (tpath-bottom-lines above))
                        below (first new-top-lines))
        (declare (ignorable new-above-lines))
        (assert (eq above merged))
        (assert (tpath-goes merged 100 0 400 0 400 60 100 60 100 0))
        (assert internal)
        (assert (tpath-goes internal 200 40 300 40 300 20 200 20 200 40))
        (assert (= 1 (length new-top-lines)))
        (assert (tline-goes (first new-top-lines) 200 40 300 40)))))
  ;; 100 200 300 400
  ;; +------------+    0
  ;; |            |
  ;; |   +----+   |   20
  ;; |   |    |   |
  ;; +---+    +---+   40
  ;;   +------------+
  ;;   |            |
  ;;   +------------+ 60
  (let ((above (tpath-from-coordinates   100 0  400 0  400 40 300 40 300 20 200 20 200 40 100 40 100 0))
        (below (make-rect-tpath          150 40 450 60)))
    (setf (tpath-bottom-lines above) (list (nth-tline 6 (tpath-lines above))
                                           (nth-tline 2 (tpath-lines above))))
    (check-tpath above :name 'above)
    (check-tpath below :name 'below)
    (print `((tpath-bottom-lines above) ,(tpath-bottom-lines above)))
    (print `((tpath-bottom-lines below) ,(tpath-bottom-lines below)))
    (multiple-value-bind (merged internal new-above-lines new-top-lines)        
        (merge-tpaths above (first (tpath-bottom-lines above))
                      below (tpath-lines below))
      (declare (ignorable new-above-lines))
      (check-tpath merged :name 'merged)
      (assert (eq above merged))
      (assert (tpath-goes merged 100 0 400 0 400 40 300 40 300 20 200 20 200 40 450 40 450 60 150 60 150 40 100 40 100 0))
      (assert (null internal))
      (print `((tpath-bottom-lines merged) ,(tpath-bottom-lines merged)))
      (assert (= 2 (length (tpath-bottom-lines merged))))
      (assert (tline-goes (first  (tpath-bottom-lines merged)) 150 40 100 40))
      (assert (tline-goes (second (tpath-bottom-lines merged)) 400 40 300 40))
      (assert (= 1 (length new-top-lines)))
      (assert (tline-goes (first new-top-lines) 200 40 450 40))
      (multiple-value-bind (merged internal new-above-lines new-top-lines)
          (merge-tpaths above (second (tpath-bottom-lines above))
                        below (first new-top-lines))
        (declare (ignorable new-above-lines))
        (check-tpath merged :name 'merged)
        (assert (eq above merged))
        (assert (tpath-goes merged 100 0 400 0 400 40 450 40 450 60 150 60 150 40 100 40 100 0))
        (assert internal)
        (assert (tpath-goes internal 200 40 300 40 300 20 200 20 200 40))
        (assert (= 2 (length new-top-lines)))
        (assert (tline-goes (first new-top-lines) 200 40 300 40))
        (assert (tline-goes (second new-top-lines) 400 40 450 40)))))
  ;;   100 200 300 400
  ;;   +------------+  0
  ;;   |            |
  ;;   |   +----+   | 20
  ;;   |   |    |   |
  ;;   +---+    +---+ 40
  ;; +------------+
  ;; |            |
  ;; +------------+   60
  (let ((above (tpath-from-coordinates   100 0  400 0  400 40 300 40 300 20 200 20 200 40 100 40 100 0))
        (below (make-rect-tpath           50 40 350 60)))
    (setf (tpath-bottom-lines above) (list (nth-tline 6 (tpath-lines above))
                                           (nth-tline 2 (tpath-lines above))))
    (check-tpath above :name 'above)
    (check-tpath below :name 'below)
    (multiple-value-bind (merged internal new-above-lines new-top-lines)
        (merge-tpaths above (first (tpath-bottom-lines above))
                      below (tpath-lines below))
      (declare (ignorable new-above-lines))
      (check-tpath merged :name 'merged)
      (assert (eq above merged))
      (assert (tpath-goes merged 100 0 400 0 400 40 300 40 300 20 200 20 200 40 350 40 350 60 050 60 050 40 100 40 100 0))
      (assert (null internal))
      (assert (= 1 (length (tpath-bottom-lines merged))))
      ;; (assert (tline-goes (first  (tpath-bottom-lines merged)) 100 40 050 40))
      (assert (tline-goes (first (tpath-bottom-lines merged)) 400 40 300 40))
      (assert (= 2 (length new-top-lines)))
      (assert (tline-goes (first new-top-lines) 050 40 100 40))
      (assert (tline-goes (second new-top-lines) 200 40 350 40))
      (multiple-value-bind (merged internal new-above-lines new-top-lines)
          (merge-tpaths above (first (tpath-bottom-lines above))
                        below (second new-top-lines))
        (declare (ignorable new-above-lines))
        (check-tpath merged :name 'merged)
        (assert (eq above merged))
        (assert (tpath-goes merged 100 0 400 0 400 40 350 40 350 60 50 60 50 40 100 40 100 0))
        (assert internal)
        (assert (tpath-goes internal 200 40 300 40 300 20 200 20 200 40))
        (assert (= 1 (length new-top-lines)))
        (assert (tline-goes (first new-top-lines) 200 40 300 40)))))
  ;; 100 200 300 400
  ;; +------------+
  ;; |            |
  ;; |   +----+   |
  ;; |   |    |   |
  ;; +---+    +---+
  ;;   +--------+
  ;;   |        |
  ;;   +--------+
  (let ((above (tpath-from-coordinates   100 0  400 0  400 40 300 40 300 20 200 20 200 40 100 40 100 0))
        (below (make-rect-tpath          150 40 350 60)))
    (setf (tpath-bottom-lines above) (list (nth-tline 6 (tpath-lines above))
                                           (nth-tline 2 (tpath-lines above))))
    (check-tpath above :name 'above)
    (check-tpath below :name 'below)
    (multiple-value-bind (merged internal new-above-lines new-top-lines)
        (merge-tpaths above (first (tpath-bottom-lines above))
                      below (tpath-lines below))
      (declare (ignorable new-above-lines))
      (check-tpath merged :name 'merged)
      (assert (eq above merged))
      (assert (tpath-goes merged 100 0 400 0 400 40 300 40 300 20 200 20 200 40 350 40 350 60 150 60 150 40 100 40 100 0))
      (assert (null internal))
      (assert (= 2 (length (tpath-bottom-lines merged))))
      (assert (tline-goes (first  (tpath-bottom-lines merged)) 150 40 100 40))
      (assert (tline-goes (second (tpath-bottom-lines merged)) 400 40 300 40))
      (assert (= 1 (length new-top-lines)))
      (assert (tline-goes (first new-top-lines) 200 40 350 40))
      (multiple-value-bind (merged internal new-above-lines new-top-lines)
          (merge-tpaths above (second (tpath-bottom-lines above))
                        below (first new-top-lines))
        (declare (ignorable new-above-lines))
        (check-tpath merged :name 'merged)
        (assert (eq above merged))
        (assert (tpath-goes merged 100 0 400 0 400 40 350 40 350 60 150 60 150 40 100 40 100 0))
        (assert internal)
        (assert (tpath-goes internal 200 40 300 40 300 20 200 20 200 40))
        (assert (= 1 (length new-top-lines)))
        (assert (tline-goes (first new-top-lines) 200 40 300 40)))))
  ;;   100 200 300 400
  ;;   +------------+    0 
  ;;   |            |
  ;;   |   +----+   |   20
  ;;   |   |    |   |
  ;;   +---+    +---+   40
  ;; +----------------+
  ;; |                |
  ;; +----------------+ 60
  (let ((above (tpath-from-coordinates   100 0  400 0  400 40 300 40 300 20 200 20 200 40 100 40 100 0))
        (below (make-rect-tpath           50 40 450 60)))
    (setf (tpath-bottom-lines above) (list (nth-tline 6 (tpath-lines above))
                                           (nth-tline 2 (tpath-lines above))))
    (check-tpath above :name 'above)
    (check-tpath below :name 'below)
    (multiple-value-bind (merged internal new-above-lines new-top-lines)
        (merge-tpaths above (first (tpath-bottom-lines above))
                      below (tpath-lines below))
      (declare (ignorable new-above-lines))
      (check-tpath merged :name 'merged)
      (assert (eq above merged))
      (assert (tpath-goes merged 100 0 400 0 400 40 300 40 300 20 200 20 200 40 450 40 450 60 050 60 050 40 100 40 100 0))
      (assert (null internal))
      (assert (= 1 (length (tpath-bottom-lines merged))))
      (assert (tline-goes (first (tpath-bottom-lines merged)) 400 40 300 40))
      (assert (= 2 (length new-top-lines)))
      (assert (tline-goes (first new-top-lines) 050 40 100 40))
      (assert (tline-goes (second new-top-lines) 200 40 450 40))
      (multiple-value-bind (merged internal new-above-lines new-top-lines)
          (merge-tpaths above (first (tpath-bottom-lines above))
                        below (second new-top-lines))
        (declare (ignorable new-above-lines))
        (check-tpath merged :name 'merged)
        (assert (eq above merged))
        (assert (tpath-goes merged 100 0 400 0 400 40 450 40 450 60 050 60 050 40 100 40 100 0))
        (assert internal)
        (assert (tpath-goes internal 200 40 300 40 300 20 200 20 200 40))
        (assert (= 2 (length new-top-lines)))
        (assert (tline-goes (first new-top-lines) 200 40 300 40))
        (assert (tline-goes (second new-top-lines) 400 40 450 40)))))
  :success)




(defun bezier-path-from-tpath (tpath)
  (flet ((tpoint-to-point (tpoint)
           (ns:make-ns-point (tpoint-x tpoint) (tpoint-y tpoint))))
    (let ((path [NSBezierPath bezierPath]))
      [path setLineCapStyle:#$NSSquareLineCapStyle]
      ;; [path setLineJoinStyle:#$NSRoundLineJoinStyle]
      [path setLineJoinStyle:#$NSBevelLineJoinStyle]
      [path moveToPoint:(tpoint-to-point (tline-from-point (tpath-lines tpath)))]
      (tpath-dolines (line tpath)
        [path lineToPoint:(tpoint-to-point (tline-to-point line))])
      [path closePath]
      path)))



(defvar *w* nil)
(defvar *speed* 0.0)
;; (setf *speed* :infinite)
;; (setf *speed* 0.0)
;; (setf *speed* 1.0)
(defun trace-path-pause (how path)
  (case *speed*
    (:infinite
     (format *query-io*  "~&pause ~S ~S" how path)
     (finish-output *query-io*)
     (read-line *query-io*))
    (otherwise
     (sleep *speed*))))

(defun trace-path (how path)
  (with-focused-view (or *current-view* *w*)
    (with-fore-color (case how
                       (:disjoint       *red-color*)
                       (:open           *yellow-color*)
                       (:above          *green-color*)
                       (:below          *light-blue-color*)
                       (:merged         *magenta-color*)
                       (otherwise       *black-color*))
      (case how
        ;; ((:above :below :bottom)  [(bezier-path-from-tpath path) fill])
        (otherwise                [(bezier-path-from-tpath path) stroke]))
      (with-fore-color *purple-color*
        (loop
          :for bottom :in (tpath-bottom-lines path)
          :for from = (tline-from-point bottom)
          :for to   = (tline-to-point bottom)
          :do (draw-line (tpoint-x from) (tpoint-y from) (tpoint-x to) (tpoint-y to))))
      (with-fore-color *green-color*
        (loop
          :for bottom :in (tpath-new-bottom-lines path)
          :for from = (tline-from-point bottom)
          :for to   = (tline-to-point bottom)
          :do (draw-line (tpoint-x from) (tpoint-y from) (tpoint-x to) (tpoint-y to))))))
      (trace-path-pause how path))


(defun segments-from-region (region)
  (loop
    :for tpath :in (paths-from-region region)
    :for start = (tpath-lines tpath)
    :collect (loop
               :with points = '()
               :with current = (tline-to-line start)
               :until (eq current start)
                 :initially (push (tpoint-to-list (tline-from-point current)) points)
               :do (push (tpoint-to-list (tline-to-point current)) points)
                   (setf current (tline-to-line current))
               :finally (return (nreverse points)))))

(defun lines-from-region (region)
  (loop
    :with segments = (region-segments region)
    :for i :below (1- (length segments))
    :for (top . current) = (aref segments i)
    :for bottom = (car (aref segments (1+ i)))
    :append (loop :for j :below (length current) :by 2
                  :for left = (aref current j)
                  :for right = (aref current (1+ j))
                  :collect (list (list left top) (list right top))
                  :collect (list (list right top) (list right bottom))
                  :collect (list (list right bottom) (list left bottom))
                  :collect (list (list left bottom) (list left top)))))


(defun changes (old new)
  (loop :with result = '()
        :with i = 0 :with mi = (length old)
        :with j = 0 :with mj = (length new)
        :while (and (< i mi) (< j mj))
        :do (let ((o (aref old i))
                  (n (aref new j)))
              (cond ((< o n) (incf i) (push o result))
                    ((< n o) (incf j) (push n result))
                    (t       (incf i) (incf j))))
        :finally (loop
                   :while (< i mi)
                   :do (push (aref old i) result) (incf i))
                 (loop
                   :while (< j mj)
                   :do (push (aref new j) result) (incf j))
                 (return (nreverse result))))




#-(and)
(progn
  (cd #P"~/works/patchwork/src/mclgui/")
  (pushnew #P"./" asdf:*central-registry* :test (function equal))
  (ql:quickload :mclgui-test)
  (in-package :ui)

  (initialize)

  (when (front-window) (window-close (front-window)))

  (defparameter *w* (make-instance 'region-test-window))
  (setf (coordinates-filter (find-subview-of-type *w* 'coordinates-view))
        (lambda (x y) (values (* 10 (round x 10)) (* 10 (round y 10)))))
  
  (debug-region (elt *test/regions* 4))

  (window-close *w*)
  (window-close (front-window))
  (update-coordinates (elt (view-subviews *w*) 1) (get-mouse))
  (view-draw-contents *w*)
  (map nil 'print (view-subviews *w*))
  
  (setf (test-regions *w*) *test/regions*)

  (with-focused-view *w*
    (draw-line 190 200 90 200)
    (draw-line 170 170 200 170)
    (draw-line 200 170 190 170))
  
  (debug-region *hole*)
  (stroke-region *hole*)
  (setf *speed* 1.0)
  (debug-region (current-region *w*))
  (current-region *w*)

  (paths-from-region *hole*)
  (map nil 'print (lines-from-region *hole*))

  (rect-to-list (region-bounds *hole*))
  (offset-region *hole* 70 50)
  (stroke-region *hole*)
  (stroke-region *z*)

  (debug-region *hole*)

  
  (defmethod view-draw-contents ((self (eql *w*)))
    (debug-region *hole*))
  (remove-method (function view-draw-contents)
                 (find-method (function view-draw-contents) '() (list (list 'eql *w*))))
  
  (stroke-region  )

  
  (let ((p (let ((circle (set-disc-region (new-region) 100 100 70)))
             (xor-region circle (inset-region (copy-region circle) 4 4) (new-region)))))
    (with-focused-view *w*
      (erase-rect* 0 0 1000 1000)
      (let ((c *specified-colors*))
        (dolist (path (paths-from-region p))
          (with-fore-color (pop c)
            [(bezier-path-from-tpath path) fill])
          [(bezier-path-from-tpath path) stroke]))))

  
  (let ((region *f*))
    (loop :with previous = #()
          :for (y . current) :across (region-segments region)
          :collect (changes current previous)
          :do (setf previous current)))

  (map nil 'print (region-segments *hole*))
  (map nil 'print (region-segments *z*))

  (let ((region *z*)) ;  *hole*
    (loop :with previous = #()
          :for (y . current) :across (region-segments region)
          :collect (print (list y (changes current previous)))
          :do (setf previous current)))

  );;progn


              ;; (progn
              ;;   (setf open-paths (mapcar (lambda (path)
              ;;                              ;; there's only one bottom to those just generated paths.
              ;;                              (cons (first (tpath-bottom-lines path)) path))
              ;;                            (generate-paths-for-row)))
              ;;   (let ((*speed* 0.0)) (mapc (lambda (pp) (trace-path :open (cdr pp))) open-paths))
              ;;   (sleep *speed*))


                ;; (progn (format t "~&** below-paths = ~{~/ui:fmt-line/~^ ~}~%" (mapcar (function car) below-paths))
                ;;        (let ((*speed* 0.0)) (mapc (lambda (pp) (trace-path :below (cdr pp))) below-paths))
                ;;        (sleep *speed*))
                
                        ;; (format t "~&** top-line    = ~/ui:fmt-line/~%" top-line)
                              ;; (progn (format t "~&below has not path above, push it to next, and shift new-bottom-lines~%")
                              ;;        (trace-path :below below))

;; (progn (format t "~&** above-line  = ~/ui:fmt-line/~%** above       = ~S~%" above-line above)
                              ;;        (trace-path :above above))
                                  ;; (trace-path :disjoint internal)

                                ;; (progn (format t "~&** merged      = ~S~{~%**             = ~S~%~}"
                                ;;                merged (tpath-new-bottom-lines merged))
                                ;;        (format t "~&~{** new above-lines = ~S~%~}" new-above-lines)
                                ;;        (format t "~&~{** new top-lines   = ~S~%~}" new-top-lines)
                                ;;        (format t "~&** open-paths = ~{~/ui:fmt-line/~^ ~}~%"
                                ;;                (mapcar (function car) open-paths))
                                ;;        (print `((tpath-new-bottom-lines merged) ,(mapcar (function tline-coordinates) (tpath-new-bottom-lines merged))))
                                ;;        (trace-path :open merged))
                
                ;; (loop :for (nil . path) :in open-paths :do (trace-path :open path))
                ;; (loop :for path :in next-paths :do (trace-path :bottom path))
                ;; (dolist (path next-paths)
                ;;   (format t "~&** next-path  = ~S~{~%  new bottom  = ~S~}~%"
                ;;           path (copy-list (tpath-new-bottom-lines path))))
                ;; (dolist (path (mapcar (function cdr) open-paths))
                ;;   (format t "~&** open-path  = ~S~{~%  new bottom  = ~S~}~%"
                ;;           path (copy-list (tpath-new-bottom-lines path))))

                           ;; (progn
                           ;;   (format t "~&** current open-paths = ~{~/ui:fmt-line/~^ ~}~%"
                           ;;           (mapcar (function car) open-paths))
                           ;;   (dolist (path disjoint-paths) (trace-path :disjoint path))
                           ;;   (dolist (path open-paths)     (trace-path :open (cdr path)))
                           ;;   (sleep *speed*))


(defun test/region-paths ()
  (test/tpath-contains-line-p)
  (test/merge-tpaths))

;; (test/region-paths)


;;;; THE END ;;;;


