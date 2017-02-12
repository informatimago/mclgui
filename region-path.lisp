;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               region-path.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Implements conversion of regions to paths.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-02-12 <PJB> Added this header.
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
(in-package "MCLGUI")


;;; Temporary Paths
;;; ---------------
;;;
;;; While building paths from regions, we need to keep a doubly-linked
;;; chain of lines, and to keep the list of bottom (horizontal) lines
;;; (which may be modified, along with their from and to lines).

(defstruct tpoint
  x y)
(defstruct tline
  from-point to-point
  from-line  to-line)
(defstruct tpath
  lines
  above-lines       ; sorted by < tline-to-x
  top-lines         ; sorted by < tline-to-x
  bottom-lines      ; sorted by < tline-to-x
  );;tpath


(defun make-rect-tpath (left top right bottom)
  (let* ((topleft     (make-tpoint :x left  :y top))
         (topright    (make-tpoint :x right :y top))
         (botright    (make-tpoint :x right :y bottom))
         (botleft     (make-tpoint :x left  :y bottom))
         (top-line    (make-tline :from-point topleft  :to-point topright))
         (right-line  (make-tline :from-point topright :to-point botright))
         (bottom-line (make-tline :from-point botright :to-point botleft))
         (left-line   (make-tline :from-point botleft  :to-point topleft)))
    (setf (tline-from-line top-line) left-line
          (tline-to-line   top-line) right-line
          (tline-from-line right-line) top-line
          (tline-to-line   right-line) bottom-line
          (tline-from-line bottom-line) right-line
          (tline-to-line   bottom-line) left-line
          (tline-from-line left-line) bottom-line
          (tline-to-line   left-line) top-line)
    (make-tpath :lines top-line
                :top-lines    (list top-line)
                :bottom-lines (list bottom-line))))

(defmacro tline-dolines ((line tlines &optional result) &body body)
  (let ((vstart   (gensym))
        (vnext (gensym)))
    `(loop
       :with ,vstart = ,tlines
       :for ,line = ,vstart :then ,vnext
       :for ,vnext = (tline-to-line ,vstart) :then (tline-to-line ,vnext)
       :do (progn ,@body)
       :until (eq ,vnext ,vstart)
       :finally (return ,result))))




(defun tpath-contains-line-p (path line)
  (not (loop
         :with start = (tpath-lines path)
         :for current = (tline-to-line start) :then (tline-to-line current)
         :never (eql line current)
         :until (eql start current))))


(declaim (inline tline-update-links tline-to-x tline-from-x merge-tlines))

(defun tline-update-links (tline)
  #+region/run-time-checks
  (progn (assert (eq (tline-from-point tline)
                     (tline-to-point (tline-from-line tline))))
         (assert (eq (tline-to-point tline)
                     (tline-from-point (tline-to-line tline)))))
  (setf (tline-from-line (tline-to-line tline)) tline
        (tline-to-line (tline-from-line tline)) tline)
  tline)

(defun tline-to-x (line)
  (tpoint-x (tline-to-point line)))

(defun tline-from-x (line)
  (tpoint-x (tline-from-point line)))

(defun merge-tlines (new-lines lines)
  (merge 'list new-lines lines (function <) :key (function tline-to-x)))

(defun merge-tpaths (above above-line below top-line)
  "

ABOVE:          a tpath.

ABOVE-LINE:     the tline in (tpath-above-lines above) that should be
                merged.

BELOW:          a tpath.

TOP-LINE:       a line of BELOW that might intersect with ABOVE-LINE.

RETURN:         :left or :right or above;
                internal or NIL;
                a list of split above-lines;
                a list of split top-lines.

                If above-line and top-line don't 'intersect' then
                either :left or :right depending on the position of
                top-line with respect to above-line, otherwise the
                below path is merged with above, and above is
                returned; If merging paths detaches an internal path,
                then it is returned as second value, or else NIL.
                Lists of split above-lines and top-lines are returned
                as third and fourth values.

"
  (flet ((merge-tpath (above below internal
                       removed-above-line new-above-lines
                       removed-top-line new-top-lines)
           (setf (tpath-above-lines above) (merge-tlines (delete removed-above-line (tpath-above-lines above))
                                                         (copy-list new-above-lines))
                 (tpath-top-lines   below) (merge-tlines (delete removed-top-line (tpath-top-lines below))
                                                         (copy-list new-top-lines)))
           (unless (eql above below)
             (setf (tpath-above-lines  above) (merge-tlines (tpath-above-lines above)
                                                            (tpath-above-lines below))
                   (tpath-top-lines    above) (merge-tlines (tpath-top-lines above)
                                                            (tpath-top-lines below))
                   (tpath-bottom-lines above) (merge-tlines (tpath-bottom-lines above)
                                                            (tpath-bottom-lines below))))
           (values above internal new-above-lines new-top-lines))
         (tline (from-line to-line)
           (tline-update-links (make-tline :from-point (tline-from-point from-line)
                                           :from-line  (tline-from-line  from-line)
                                           :to-point   (tline-to-point   to-line)
                                           :to-line    (tline-to-line    to-line)))))
    (let ((bleft  (tpoint-x (tline-from-point top-line)))
          (bright (tpoint-x (tline-to-point   top-line)))
          (aleft  (tpoint-x (tline-to-point   above-line)))
          (aright (tpoint-x (tline-from-point above-line))))
      (cond
        ((<= bright aleft)  ; ___---
         ;;        +---+
         ;;        |   |
         ;;        +---+
         ;; +---+
         ;; |   |
         ;; +---+
         (values :left  nil (list above-line) (list top-line)))
        ((<= aright bleft)  ; ---___
         ;; +---+
         ;; |   |
         ;; +---+
         ;;        +---+
         ;;        |   |
         ;;        +---+
         (values :right nil (list above-line) (list top-line)))
        ;; aleft<bright  bleft<aright
        (t
         (cond
           ((< aleft bleft)
            (cond
              ((< aright bright) ; ---===___
               ;; | aleft bleft | bleft aright | aright bright |
               ;; +----+
               ;; |    |
               ;; +--+-+----+
               ;;    |      |
               ;;    +------+
               (let ((left-line   (tline top-line above-line))
                     (right-line  (tline above-line top-line)))
                 (merge-tpath above below nil above-line (list left-line) top-line (list right-line))))
              ((= aright bright) ; ---===
               ;; +----------+
               ;; |          |
               ;; +-----+----+
               ;;       |    |
               ;;       +----+
               (let* ((left-line    (tline top-line above-line))
                      (right-above  (tline-from-line above-line))
                      (right-below  (tline-to-line top-line))
                      (right-vert   (tline right-above right-below)))
                 (declare (ignorable right-vert))
                 (merge-tpath above below nil above-line (list left-line) top-line '())))
              (t ; (> aright bright) ; ---===---
               ;; +----------+
               ;; |          |
               ;; +--+-----+-+
               ;;    |     |
               ;;    +-----+
               (let ((left-line  (tline top-line above-line))
                     (right-line (tline above-line top-line)))
                 (merge-tpath above below nil above-line (list left-line right-line) top-line '())))))
           ((= aleft bleft)
            (cond
              ((< aright bright) ; ===___
               ;; +-----+
               ;; |     |
               ;; +-----+----+
               ;; |          |
               ;; +----------+
               (let* ((left-below  (tline-from-line top-line))
                      (left-above  (tline-to-line above-line))
                      (left-vert   (tline left-below left-above))
                      (right-line  (tline above-line top-line)))
                 (declare (ignorable left-vert))
                 (merge-tpath above below nil above-line '() top-line (list right-line))))
              ((= aright bright) ; ======
               ;; +----+
               ;; |    |
               ;; +----+
               ;; |    |
               ;; +----+
               (let* ((left-below  (tline-from-line top-line))
                      (left-above  (tline-to-line above-line))
                      (left-vert   (tline left-below left-above))
                      (right-above (tline-from-line above-line))
                      (right-below (tline-to-line top-line))
                      (right-vert  (tline right-above right-below)))
                 (declare (ignorable left-vert right-vert))
                 (merge-tpath above below nil above-line '() top-line '())))
              (t ; (> aright bright) ; ===---
               ;; +----------+
               ;; |          |
               ;; +-----+----+
               ;; |     |
               ;; +-----+
               (let* ((left-below  (tline-from-line top-line))
                      (left-above  (tline-to-line above-line))
                      (left-vert   (tline left-below left-above))
                      (right-line  (tline above-line top-line)))
                 (declare (ignorable left-vert))
                 (merge-tpath above below nil above-line (list right-line) top-line '())))))
           (t ; (> aleft bleft) ;; *** may split path
            (cond
              ((< aright bright) ; ___===___
               ;;    +-----+
               ;;    |     |
               ;; +--+-----+-+
               ;; |          |
               ;; +----------+
               (let* ((left-line   (tline top-line above-line))
                      (right-line  (tline above-line top-line))
                      (internal    (unless (tpath-contains-line-p above left-line)
                                     (make-tpath :lines left-line))))
                 (merge-tpath above below internal above-line '() top-line (list left-line right-line))))
              ((= aright bright) ; ___===
               ;;       +----+
               ;;       |    |
               ;; +-----+----+
               ;; |          |
               ;; +----------+
               (let* ((left-line   (tline top-line above-line))
                      (right-above (tline-from-line above-line))
                      (right-below (tline-to-line top-line))
                      (right-vert  (tline right-above right-below))
                      (internal    (unless (tpath-contains-line-p above left-line)
                                     (make-tpath :lines left-line))))
                 (declare (ignorable right-vert))
                 (merge-tpath above below internal above-line '() top-line (list left-line))))
              (t ; (> aright bright) ; ___===---
               ;;    +------+
               ;;    |      |
               ;; +--+-+----+
               ;; |    |
               ;; +----+
               (let* ((left-line   (tline top-line above-line))
                      (right-line  (tline above-line top-line))
                      (internal    (unless (tpath-contains-line-p above left-line)
                                     (make-tpath :lines left-line))))
                 (merge-tpath above below internal above-line
                              (list right-line) top-line (list left-line))))))))))))


(defun paths-from-region (region)
  "

RETURN: A list of tpaths surrounding each connected part of the given
        REGION.  Those paths circulate clock-wise when outside, and
        counter-clock-wise when inside.

"
  (loop
    :with disjoint-paths = '() ; list of tpath whose bottom-lines is empty.
    :with open-paths     = '() ; list of (above-line . tpath), in order of above-lines x-coordinates.
    :with next-paths     = '()
    :with rows = (expanded-segments region)
    :for i :below (1- (length rows))
    :for (top . row) = (aref rows i)
    :for bottom = (car (aref rows (1+ i)))
    :do (flet ((generate-paths-for-row ()
                 (loop :for j :below (length row) :by 2
                       :for left  = (aref row j)
                       :for right = (aref row (1+ j))
                       :collect (make-rect-tpath left top right bottom)))
               (tpath-sorted-above-cells (path)
                 (mapcar (lambda (above-line) (cons above-line path)) (tpath-above-lines path))))
          (declaim (inline tpath-sorted-above-cells generate-paths-for-row))
          #+debug-region-path(dolist (p open-paths) (trace-path :open (cdr p)))
          (if (null open-paths)

              ;; When no path is currently "open", then we generate a
              ;; new row of open path from the new row of segments:

              (setf open-paths (mapcar (lambda (path)
                                         (setf (tpath-above-lines  path) (tpath-bottom-lines path)
                                               (tpath-bottom-lines path) '())
                                         ;; there's only one bottom to those just generated paths.
                                         (cons (first (tpath-above-lines path)) path))
                                       (generate-paths-for-row)))

              ;; Otherwise, the new row of segments is used to build
              ;; below paths, and they're merged with the currently open
              ;; paths:

              (let ((below-paths (mapcar (lambda (path)
                                           ;; there's only one top-line to those just generated paths.
                                           ;; The top-line is the first line in those just generated paths.
                                           (cons (tpath-lines path) path))
                                         (generate-paths-for-row))))
                ;; merge the below-paths:
                (loop
                  :while below-paths
                  :do (let* ((entry           (pop below-paths))
                             (top-line        (car entry))
                             (below           (cdr entry))
                             (top-line-from-x (tline-from-x top-line))
                             (top-line-to-x   (tline-to-x top-line))
                             (path            (find-if-not (lambda (path)
                                                             (let ((above-line (car path)))
                                                               (or (<= top-line-to-x (tline-to-x above-line))
                                                                   (<= (tline-from-x above-line) top-line-from-x))))
                                                           open-paths)))
                        #+debug-region-path(trace-path :below below)
                        (if (null path)
                            (progn
                              ;; Below doesn't merge, but will become a new open path.
                              ;; (setf (tpath-bottom-lines below) (merge-tlines (tpath-bottom-lines below)
                              ;;                                                (tpath-above-lines below)
                              ;;                                                ;; (sort (tpath-above-lines below)
                              ;;                                                ;;       (function <) :key (function tline-to-x))
                              ;;                                                )
                              ;;       (tpath-above-lines below) nil)
                              (pushnew below next-paths))
                            (let ((above-line (car path))
                                  (above      (cdr path)))
                              #+debug-region-path(trace-path :above above)
                              (multiple-value-bind (merged internal new-above-lines new-top-lines)
                                  (merge-tpaths above above-line below top-line)
                                (when internal
                                  #+region/run-time-checks (check-tpath internal :name 'internal)
                                  #+debug-region-path(trace-path :disjoint internal)
                                  (push internal disjoint-paths))

                                #+debug-region-path(trace-path :merged merged)
                                #+region/run-time-checks (check-tpath merged :name 'merged)
                                #+region/run-time-checks (assert (tpath-bottom-lines merged))
                                (pushnew merged next-paths)

                                (setf open-paths (mapcan (lambda (cell)
                                                           (if (eql above-line (car cell))
                                                               (mapcar (lambda (line) (cons line merged)) new-above-lines)
                                                               (list (if (or (eql above (cdr cell))
                                                                             (eql below (cdr cell)))
                                                                         (cons (car cell) merged)
                                                                         cell))))
                                                         open-paths)
                                      below-paths (nconc (mapcar (lambda (top-line) (cons top-line merged))
                                                                 new-top-lines)
                                                         (mapcar (lambda (cell)
                                                                   (if (or (eql above (cdr cell))
                                                                           (eql below (cdr cell)))
                                                                       (cons (car cell) merged)
                                                                       cell))
                                                                 below-paths))))))))
                ;; then sort out the new paths into disjoint paths and the new open-paths list:
                (loop
                  :for path :in (delete-duplicates (append next-paths (mapcar (function cdr) open-paths)))
                  :do (setf (tpath-above-lines path) (tpath-bottom-lines path)
                            (tpath-bottom-lines path) '())
                  :if (tpath-above-lines path)
                    :collect path :into open
                  :else
                    :collect path :into disjoint
                  :finally (setf disjoint-paths (nconc disjoint disjoint-paths)
                                 open-paths     (sort (mapcan (function tpath-sorted-above-cells) open)
                                                      (function <) :key (lambda (entry) (tline-from-x (car entry))))
                                 next-paths '())))))
    :finally (return (append disjoint-paths (delete-duplicates (mapcar (function cdr) open-paths))))))



;;;; THE END ;;;;
