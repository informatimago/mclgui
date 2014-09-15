;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               text-edit.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Implement a Text Edit Manager inspired from MacOS.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2014-09-07 <PJB> Created.
;;;;BUGS
;;;;    TODO: see the white space paragraph prefix and suffix while
;;;;          editing in various justifications.
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


(deftype word-break-function    ()
  `(function (#|line|#string #|charpos|#integer) boolean))
(deftype click-loop-function    ()
  `(function () boolean))
(deftype te-caret-function      ()
  `(function (#|caret-rect|#rect      #|lino|#integer terec) (values)))
(deftype te-selection-function  ()
  `(function (#|selection-rects|#list #|start-lino|#integer #|end-lino|#integer terec) (values)))

;; Drawing the text must be left to TE.
;; TE-CARET-FUNCTION and TE-SELECTION-FUNCTION should only draw the
;; caret and selection rectangle (background) and then call
;; TE-DRAW-LINE draw the text over it.

(defconstant te-just-left   0)
(defconstant te-just-right -1)
(defconstant te-just-center 1)

(defconstant te-no-wrap    -1)
(defconstant te-word-wrap   1)


(defvar *caret-half-period* 32                 "Number of ticks between each caret state change.")
(defvar *selection-color-active*     *light-blue-color* "Selection color when active.")
(defvar *selection-color-inactive*   *light-gray-color* "Selection color when inactive.")


(defstruct (terec (:conc-name te-))
  (dest-rect   nil :type (or null rect))
  (view-rect   nil :type (or null rect))
  (sel-rect    nil :type (or null rect))
  (line-height 0   :type integer)
  (font-ascent 0   :type integer)
  (sel-point   #@(0 0) :type point)
  (sel-start   0   :type integer)
  (sel-end     0   :type integer)
  (active      0   :type integer)
  (word-break  nil :type (or null word-break-function))
  (click-loop  nil :type (or null click-loop-function))
  (click-time  0   :type integer)
  (click-loc   0   :type integer)
  (caret-time  0   :type integer)
  (caret-state 0   :type integer)
  (just        0   :type integer)
  (length      0   :type integer)
  (text        nil)
  (recal-back  0   :type integer)
  (recal-lines 0   :type integer)
  (click-stuff nil)
  (cr-only    -1   :type integer) ; minusp => cr-only; plusp => word-wrap
  (tx-font     0   :type integer)
  (tx-face     0   :type integer)
  (tx-mode     0   :type integer)
  (tx-size     0   :type integer)
  (in-port     nil :type (or null window))
  (high-hook   nil :type (or null te-selection-function))
  (caret-hook  nil :type (or null te-caret-function))
  (sel-current 0   :type integer) ; current charpos (used when extending selection with shift-).
  ;; Note: lines are justified lines; there may be several lines for a given paragraph.
  (nlines      0   :type integer)
  (line-starts nil :type (or null (vector integer)))
  
  ;; We use the a gap buffer of paragraphs, and a string gap buffer for the current paragraph.
  paragraphs ; an adjustable array of (cons position line); newlines count one position but are not stored.
  (nparagraphs             0 :type integer) ; number of paragraphs 
  (current-paragraph-index 0 :type integer) 
  (next-paragraph-index    0 :type integer)
  ;; current-paragraph-{before,after}-point are adjustable strings with fill-pointers,
  ;; containing the current paragraph split into before and after parts.
  ;; the characters in current-paragraph-after-point are stored in reverse order.
  current-paragraph-before-point
  current-paragraph-after-point
  (current-paragraph-dirty    nil :type boolean) ; whether (aref (te-paragraphs te) (te-current-paragraph-index te)) needs to be recomputed from current-paragraph-before-point and current-paragraph-after-point
  bindings ; the key bindings hash-table
  (updates                    '() :type list)
  (line-coordinates-cache     nil)
  (selection-rectangles-cache nil)
  (display-state              nil))


;;(remove-method (function print-object) (find-method (function print-object) '() '(terec t)))
#-(and)
(defmethod print-object ((te terec) stream)
  (if *print-readably*
      (call-next-method)
      (let* ((line1  (te-line 0 te))
             (len1   (length line1)))
        (format stream "#<~S :length ~D :text ~S :selection ~S >"
                'terec (te-length te)
                (concatenate 'string
                             (subseq line1 0 (min 32 len1))
                             (if (< (te-length te) 32) "" "…"))
                (if (= (te-sel-start te) (te-sel-end te))
                    (te-sel-start te)
                    (list (te-sel-start te) (te-sel-end te))))
        te)))


(defun te-clear-caches (te)
  (setf (te-line-coordinates-cache te) nil
        (te-selection-rectangles-cache te) nil))


(defun te-invariant (te)
  (assert (or (= (te-sel-current te) (te-sel-start te))
              (= (te-sel-current te) (te-sel-end   te))))
  ;; there's at least one empty paragraph:
  (assert (<= 1 (te-nparagraphs te) (length (te-paragraphs te)))) 
  (assert (<= 0 (te-current-paragraph-index te) (te-nparagraphs te)))
  (assert (< (te-current-paragraph-index te) (te-next-paragraph-index te)))
  (assert (<= (te-next-paragraph-index te) (length (te-paragraphs te))))
  (assert (loop :for i :from 0 :to (te-current-paragraph-index te)
                :always (not (null (aref (te-paragraphs te) i)))))
  (assert (loop :for i :from (1+ (te-current-paragraph-index te))
                  :below (te-next-paragraph-index te)
                :always (null (aref (te-paragraphs te) i))))
  (assert (loop :for i :from (te-next-paragraph-index te) :below (length (te-paragraphs te))
                :always (not (null (aref (te-paragraphs te) i))))))


(declaim (inline te-is-active))
(defun te-is-active (te)
  "Whether the TeRec is active (ie. displays a blinking caret or full selection boxes)."
  (plusp (te-active te)))


(declaim (inline te-has-caret))
(defun te-has-caret (te)
  "Whether the TeRec shall display a caret (instead of a selection)."
  (= (te-sel-start te) (te-sel-end te)))



(define-condition out-of-bound-error (error)
  ((start :initform nil :initarg :start :reader out-of-bound-start)
   (end   :initform nil :initarg :end   :reader out-of-bound-end)
   (index :initform nil :initarg :index :reader out-of-bound-index)
   (datum :initform nil :initarg :datum :reader out-of-bound-datum)
   (label :initform nil :initarg :label :reader out-of-bound-label))
  (:report (lambda (condition stream)
             (format stream "Out of bounds: index ~A should have been between ~
                             ~A (inclusive) and ~A (exclusive) when accessing ~A ~S"
                     (out-of-bound-index condition)
                     (out-of-bound-start condition)
                     (out-of-bound-end condition)
                     (out-of-bound-label condition)
                     (out-of-bound-datum condition)))))


(defun real-compare (a b)
  "Compares A and B, returning -1, 0 or 1 depending on their order."
  (cond
    ((< a b) -1)
    ((> a b) +1)
    (t        0)))


(declaim (inline adjustable-vector))
(defun adjustable-vector (size &optional (elements '()) (element-type t))
  (let ((avector (make-array size :element-type element-type
                                  :fill-pointer (length elements)
                                  :adjustable t)))
    (replace avector elements)
    avector))


(declaim (inline adjustable-string))
(defun adjustable-string (size &optional (string ""))
  "
PRE:    (<= (length string) size)
RETURN: An adjustable string with fill-pointer containing the
        characters of the STRING, and being of allocated SIZE.
"
  (adjustable-vector size string 'character))






;;;---------------------------------------------------------------------
;;;
;;;---------------------------------------------------------------------

;;   (tx-font tx-face tx-mode tx-size) --> (line-height font-ascent)

(defun te-update-font-info (te)
  (flet ((set-font-info (ff ms)
           (multiple-value-bind (ascent d w l) (font-codes-info ff ms)
             (declare (ignore w))
             (setf (te-tx-font     te) (ldb (byte 16 16) ff)
                   (te-tx-face     te) (ldb (byte 16  0) ff)
                   (te-tx-mode     te) (ldb (byte 16 16) ms)
                   (te-tx-size     te) (ldb (byte 16  0) ms)
                   (te-line-height te) (ceiling (+ ascent d l))
                   (te-font-ascent te) (round ascent)))))
    (if (te-in-port te)
        (multiple-value-bind (ff ms) (view-font-codes (te-in-port te))
          (multiple-value-bind (ff ms) (font-codes (view-font (te-in-port te)) ff ms)
            (set-font-info ff ms)))
        (set-font-info 0 0))))

;;;---------------------------------------------------------------------
;;;
;;;---------------------------------------------------------------------

;; edition (by character)
;; --->
;; (current-paragraph-before-point current-paragraph-after-point)
;; <-->
;; ---(te-current-paragraph)--->
;;                                 paragraphs
;;                                           <--- edition (by paragraph)
;;                                           ---> line-starts
;;                                           ---> display



;;;---------------------------------------------------------------------
;;;
;;;---------------------------------------------------------------------

;; (charpos offset) ==> all characters position from charpos to te-length must be incremented by offset

(defun %te-adjust-starts (charpos increment te)
  "
DO:   Adjust by INCREMENT the total length, paragraph and line start
      positions starting from the paragraph and line following
      CHARPOS.
"
  (loop :for parno :from (1+ (te-paragraph-at charpos te))
          :below (te-nparagraphs te)
        :do (incf (car (te-paragraph parno te)) increment))
  (loop :for lino :from (1+ (te-line-at charpos te))
          :below (te-nlines te)
        :do (incf (aref (te-line-starts te) lino) increment))
  (incf (te-length te) increment))

;;;---------------------------------------------------------------------
;;; current-paragraph editing
;;;---------------------------------------------------------------------


;;; current-paragraph-{before,after}-point ---> paragraphs

(defun te-current-paragraph (te)
  "
RETURN: The cons cell containing the start and the string of the current paragraph.
NOTE:   This string is computed from current-paragraph-before-point
        and current-paragraph-after-point and cached.
"
  (if (te-current-paragraph-dirty te)
      (setf (te-current-paragraph-dirty te) nil
            (aref (te-paragraphs te) (te-current-paragraph-index te))
            (cons (car (te-current-paragraph-before-point te))
                  #-(and) (concatenate 'string (cdr (te-current-paragraph-before-point te))
                                       (reverse (cdr (te-current-paragraph-after-point te))))
                  ;; Let's assume this will be faster; at least, it should cons less.
                  (let* ((before     (cdr (te-current-paragraph-before-point te)))
                         (after      (cdr (te-current-paragraph-after-point  te)))
                         (len-before (length before))
                         (len-after  (length after))
                         (para       (make-string (+ len-before len-after))))
                    (replace para before)
                    (loop :for src :from (1- len-after) :downto 0
                          :for dst :from len-before
                          :do (setf (aref para dst) (aref after src)))
                    para)))
      (aref (te-paragraphs te) (te-current-paragraph-index te))))



;;;---------------------------------------------------------------------
;;; paragraph
;;;---------------------------------------------------------------------

(defun te-paragraph (parno te)
  "
PARNO:  An integer between 0 and (te-nparagraphs te) exclusive.
TE:     A TeRec
RETURN: A cons cell containing the index of the position an the text of the paragraph number PARNO;
        the index  of the paragraph in the te-paragraphs gap buffer.
"
  (when (or (minusp parno) (<= (te-nparagraphs te) parno))
    (error 'out-of-bound-error :start 0 :end (te-nparagraphs te) :index parno
                               :datum (te-paragraphs te)
                               :label "paragraphs"))
  (cond
    ((< parno (te-current-paragraph-index te))
     (values (aref (te-paragraphs te) parno) parno))
    ((= parno (te-current-paragraph-index te))
     (values (te-current-paragraph te) parno))
    (t
     (let ((index (+ (- parno (te-current-paragraph-index te) 1)
                     (te-next-paragraph-index te))))
       (values (aref (te-paragraphs te) index) index)))))


(defun te-paragraph-at (charpos te)
  "
CHARPOS: the character position, 0-based.
TE :     TeRec
RETURN:  The paragraph number of the paragraph containing charpos.
"
  (when (or (minusp charpos) (< (te-length te) charpos))
    (error 'out-of-bound-error :start 0 :end (te-length te) :index charpos
                               :datum te
                               :label "characters"))
  (multiple-value-bind (found parno order)
      (dichotomy (lambda (parno) (real-compare charpos (car (te-paragraph parno te))))
                 0 (te-nparagraphs te))
    (declare (ignore found order))
    parno))


(defmacro doparagraphs ((index var te &optional result) &body body)
  "
DO:     Evaluates the BODY with the variables given in INDEX bound to
        the paragraph index, in (te-paragraphs te), and VAR bound to
        the paragraph cons (paragraph-start . paragraph-text).
RETURN: The RESULT.
"
  (let ((vte    (gensym))
        (vparas (gensym))
        (vcuri  (gensym))
        (vnexi  (gensym))
        (vend   (gensym))
        (fbody  (gensym)))
    `(flet ((,fbody (,index ,var) ,@body))
       (let* ((,vte ,te)
              (,vparas (te-paragraphs ,vte))
              (,vcuri  (te-current-paragraph-index ,vte))
              (,vnexi  (te-next-paragraph-index ,vte))
              (,vend   (length ,vparas)))
         (do ((,index 0 (1+ ,index)))
             ((>= ,index ,vcuri))
           (,fbody ,index (aref ,vparas ,index)))
         (,fbody ,vcuri (te-current-paragraph te))
         (do ((,index ,vnexi (1+ ,index)))
             ((>= ,index ,vend) ,result)
           (,fbody ,index (aref ,vparas ,index)))))))

;;;---------------------------------------------------------------------
;;; paragraph editing
;;;---------------------------------------------------------------------

(defun %te-move-gap-to (parno te)
  "
POST:  (= parno (te-current-paragraph-index te))
NOTE:  This doesn't change current-paragraph-{before,after}-point, hence the %.
"
  (when (or (minusp parno) (<= (te-nparagraphs te) parno))
    (error 'out-of-bound-error :start 0 :end (te-nparagraphs te) :index parno
                               :datum te
                               :label "paragraphs"))
  (let ((current (te-current-paragraph-index te))
        (next    (te-next-paragraph-index    te))
        (paras   (te-paragraphs              te)))
    (cond
      ((< parno current)
       (loop
         :repeat (- current parno)
         :do (setf (aref paras (decf next)) (aref paras current)
                   (aref paras current)     nil)
             (decf current)))
      ((> parno current)
       (loop
         :repeat (- parno current)
         :do (setf (aref paras (incf current)) (aref paras next)
                   (aref paras next)           nil)
             (incf next)))
      #| otherwise nothing to do. |#)
    (assert (= parno current))
    (setf (te-current-paragraph-index te) current
          (te-next-paragraph-index    te) next)))


(declaim (inline te-gap-size))
(defun te-gap-size (te)
  (- (te-next-paragraph-index te) (te-current-paragraph-index te)))



(defun te-adjust-gap (increment te)
  "
DO:     Increases or decreases the gap between current-paragraph-index
        and next-paragraph-index by INCREMENT.
"
  (unless (zerop increment)
    (let* ((current (te-current-paragraph-index te))
           (next    (te-next-paragraph-index    te))
           (paras   (te-paragraphs              te))
           (gap     (- next current 1))
           (size    (array-dimension paras 0)))
      (when (minusp (+ gap increment))
        (error "Cannot reduce the gap (~D) by ~D" gap increment))
      (if (minusp increment)
          (progn
            (replace paras paras :start1 (+ next increment) :start2 increment)
            (setf paras (adjust-array paras (+ size increment))))
          (progn
            (setf paras (adjust-array paras (+ size increment)))
            (replace paras paras :start1 (+ next increment) :start2 increment)))
      (incf next increment)
      (setf (te-next-paragraph-index    te) next
            (te-paragraphs              te) paras))))


;;;---------------------------------------------------------------------
;;; Split the current paragraph
;;;---------------------------------------------------------------------

;;; (current-paragraph) --> (current-paragraph-{before,after}-point)
(defun %te-split-paragraph (index te)
  (let* ((paragraphs (te-paragraphs te))
         (para-text  (cdr (aref paragraphs index)))
         (length     (ceiling (* 1.5 (length para-text))))
         (line-start (car (aref paragraphs index))))
    (setf (te-current-paragraph-before-point te) (cons line-start
                                                       (adjustable-string length para-text))
          (te-current-paragraph-after-point  te) (cons (+ line-start (length para-text) 1)
                                                       (adjustable-string length ""))
          (te-current-paragraph-dirty        te) nil)))


;;; move the gap to the paragraph containing charpos and split it to charpos.
(defun te-split-paragraph-at (charpos te)
  "
POST:   (= (te-current-paragraph-index te) (te-paragraph-at te))
RETURN: parno of the current paragraph.
"
  (let ((parno (te-paragraph-at charpos te)))
    (if (= parno (te-current-paragraph-index te))
        ;; resplit current paragraph
        (let* ((line-start (car (te-current-paragraph-before-point te)))
               (before     (cdr (te-current-paragraph-before-point te)))
               (after      (cdr (te-current-paragraph-after-point  te)))
               (new-split  (- charpos line-start)))
          (unless (= new-split (length before))
            (cond
              ((< new-split (length before))
               ;; move to before
               (let ((change (- (length before) new-split)))
                 (when (< (- (array-dimension before 0) (fill-pointer before)) change)
                   (setf before (adjust-array before (+ (array-dimension before 0) (* 4 change)))))
                 (loop
                   :repeat change
                   :do (vector-push-extend (vector-pop before) after))))
              ((> new-split (length before))
               ;; move to after
               (let ((change (- new-split (length before))))
                 (when (< (- (array-dimension after 0) (fill-pointer after)) change)
                   (setf after (adjust-array after (+ (array-dimension after 0) (* 4 change)))))
                 (loop
                   :repeat change
                   :do (vector-push-extend (vector-pop after) before)))))
            (setf (te-current-paragraph-dirty te) t
                  (te-current-paragraph-before-point te) (cons line-start before)
                  (te-current-paragraph-after-point  te) (cons (+ line-start (length before)) after))))
        (progn          
          (te-current-paragraph te) ; unsplit current paragraph.
          ;; split another paragraph:
          (%te-move-gap-to parno te)
          (%te-split-paragraph parno te)))))


;;;---------------------------------------------------------------------
;;; paragraph --> line-starts
;;;---------------------------------------------------------------------


(declaim (inline te-whitespacep))
(defun te-whitespacep (char)
  "
RETURN:  Whether CHAR is a white space character (or non-printing control code).
"
  (let ((code (char-code char)))
    (or (<= code #x0020)
        (= code #x007F)
        ;; (= code #x00A0) ; NO-BREAK SPACE
        (and (<= #x1680 code)
             (find code #(#x1680 #x180E #x2000 #x2001 #x2002 #x2003
                          #x2004 #x2005 #x2006 #x2007 #x2008 #x2009
                          #x200A #x200B #x202F #x205F #x3000
                          ;; #xFEFF ; ZERO WIDTH NO-BREAK SPACE
                          ))))))


(defun te-default-word-break (text charpos)
  "The default workd break function."
  (te-whitespacep (aref text charpos)))

(declaim (inline te-ff))
(defun te-ff (te) (dpb (te-tx-font te) (byte 16 16) (te-tx-face te)))

(declaim (inline te-ms))
(defun te-ms (te) (dpb (te-tx-mode te) (byte 16 16) (te-tx-size te)))

(declaim (inline te-string-width))
(defun te-string-width (string te &optional (start 0) (end nil))
  "
RETURN: The width in pixels to draw the STRING in the font configured in the TE TeRec.
"
  (let* ((ff          (te-ff te))
         (ms          (te-ms te)))
    (font-codes-string-width string ff ms start end)))


(defun te-wrap-paragraph (para te)
  "
DO:     Perform word wrapping on the given paragraph.
PARA:   A cons (paragraph-start . paragraph-text).
RETURN: A sorted list of line-starts for the paragraph.
"
  (let* ((starts     (list (car para)))
         (text       (cdr para))
         (width      (rect-width (te-dest-rect te)))
         (word-break (or (te-word-break te) (function te-default-word-break)))
         (ff         (te-ff te))
         (ms         (te-ms te))
         (start      0)
         (end        (length text)))
    (loop
      (let ((text-width (font-codes-string-width text ff ms start end)))
        (when (<= text-width width)
          (return-from te-wrap-paragraph (nreverse starts)))
        (multiple-value-bind (found index order)
            (dichotomy (lambda (mid)
                         (let* ((text-width (font-codes-string-width text ff ms start mid))
                                (result (real-compare width text-width)))
                           ;; (format-trace 'dichotomy "mid = ~3D, text-width = ~6E, width = ~6E -> ~2D~%" mid text-width width result)
                           result))
                       start end)
          (declare (ignore found order))
          (let ((next-start (loop
                              :with wrap = index
                              :until (or (funcall word-break text wrap) (zerop wrap))
                              :do (decf wrap)
                              :finally (return (if (zerop wrap)
                                                   index
                                                   (1+ wrap))))))
            (push (+ (car para) next-start) starts)
            (setf start next-start)))))))


;;; global update:
;;; ( ((tx-font tx-face tx-mode tx-size) --> (line-height font-ascent)) cr-only dest-rect te-paragraph ) --> (nlines line-starts length)

(defun te-calculate-text (te)
  "
DO:     Compute nlines and line-starts, as the number and positions of
        displayed lines.  When (plusp (te-cr-only te)) paragraphs are
        wrapped, so several lines may be computed for a paragraph.
NOTE:   Call TE-CAL-TEXT, which is the one with mutex.
"
  (te-clear-caches te)
  (te-update-font-info te)
  (setf (te-nparagraphs te) (count nil (te-paragraphs te) :test-not (function eql)))
  (if (minusp (te-cr-only te))
      ;; no wrapping:
      (let ((starts (make-array (* 2 (te-nparagraphs te)) :adjustable t :initial-element 0 :fill-pointer 0)))
        (doparagraphs (i para te)
          (declare (ignore i))
          (vector-push (car para) starts))
        (setf (te-nlines te) (te-nparagraphs te)
              (te-line-starts te) starts))
      ;; with wrapping:
      (let ((starts (make-array (* 2 (te-nparagraphs te)) :adjustable t :initial-element 0 :fill-pointer 0))
            (nlines 0))
        (doparagraphs (i para te)
          (declare (ignore i))
          (dolist (start (te-wrap-paragraph para te))
            (vector-push-extend start starts (round (* 1.5 (array-dimension starts 0))))
            (incf nlines)))
        (setf (te-nlines te) nlines
              (te-line-starts te) starts)))
  (setf (te-length te) (let ((last-paragraph (te-paragraph (1- (te-nparagraphs te)) te)))
                         (+ (car last-paragraph) (length (cdr last-paragraph))))))


;;;---------------------------------------------------------------------
;;; Line
;;;---------------------------------------------------------------------

(defun te-line (lino te)
  "
LINO:   Line Number, 0-based.
TE:     TeRec
RETURN: A string containing the characters on that line.
NOTE:   The newline on the last line of paragraphs is removed.
"
  (when (or (minusp lino) (<= (te-nlines te) lino))
    (error 'out-of-bound-error :start 0 :end (te-nlines te) :index lino
                               :datum (te-line-starts te)
                               :label "line starts"))
  (let ((start (aref (te-line-starts te) lino))
        (end   (if (< (1+ lino) (te-nlines te))
                   (aref (te-line-starts te) (1+ lino)) 
                   (te-length te))))
    (let* ((para (te-paragraph (te-paragraph-at start te) te))
           (end  (min (- end (car para)) (length (cdr para)))))
      (subseq (cdr para) (min end (- start (car para))) end))))


(defun te-line-at (charpos te)
  "
CHARPOS: the character position, 0-based.
TE :     TeRec
RETURN:  The lino of the line containing CHARPOS.
"
  (when (or (minusp charpos) (< (te-length te) charpos))
    (error 'out-of-bound-error :start 0 :end (te-length te) :index charpos
                               :datum te
                               :label "characters"))
  (multiple-value-bind (found lino order)
      (dichotomy-search (te-line-starts te) charpos (function real-compare))
    (declare (ignore found order))
    lino))


(defun te-line-coordinates (lino te)
  "
LINO:   Line Number, 0-based.
TE:     TeRec
RETURN: justified-line-left, base-line, line-rect, line-text.
"
  (values-list
   (cdr (if (eql lino (car (te-line-coordinates-cache te)))
            (te-line-coordinates-cache te)
            (let* ((line-height (te-line-height te))
                   (font-ascent (te-font-ascent te))
                   (dest-rect   (te-dest-rect   te))
                   (just        (te-just        te))
                   (top         (+ (rect-top dest-rect) (* lino line-height)))
                   (bottom      (+ top line-height))
                   (base        (+ top font-ascent))
                   (line        (te-line lino te))
                   (x           (if (or (= just te-just-right) (= just te-just-center))
                                    (let* ((end         (loop
                                                          :with end = (length line)
                                                          :while (and (plusp end)
                                                                      (te-whitespacep (aref line (1- end))))
                                                          :do (decf end)
                                                          :finally (return end)))
                                           (line-width (te-string-width line te 0 end)))
                                      (if (= just te-just-right)
                                          (- (rect-right dest-rect) line-width)
                                          (+ (rect-left dest-rect)
                                             (round (- (rect-width dest-rect) line-width) 2))))
                                    0)))
              (setf (te-line-coordinates-cache te)
                    (list lino x base
                          (make-rect (rect-left dest-rect) top (rect-right dest-rect) bottom)
                          line)))))))


(defun te-column (charpos te)
  "
RETURN: The number of characters between the beginning of the line at
        CHARPOS on the line at CHARPOS.
"
  (multiple-value-bind (found lino order)
      (dichotomy-search (te-line-starts te) charpos (function real-compare))
    (declare (ignore found order))
    (values (- charpos (aref (te-line-starts te) lino))
            lino)))


;;;---------------------------------------------------------------------
;;; Drawing & Redrawing.
;;;---------------------------------------------------------------------


#|

display state:

(global redraw: dest-rect view-rect font (tx-font tx-face tx-mode tx-size line-height font-ascent)
 caret (rect lino)  or  selection (rects start-lino end-lino)
 { lino line-rect base-line x line-text })

(te-display-state-changes current-state new-state) --> display-changes

editing:     current-state --> new-state
displaying:  (te-display-state-changes current-state new-state) --> display-changes

|#

(defstruct (te-display-line-state (:conc-name tedls-))
  (lino                 0                       :type integer)
  (rect                 (make-rect 0 0)         :type rect)
  (base                 0                       :type integer)
  (left                 0                       :type integer)
  (text                 ""                      :type string))

(defstruct (te-display-state (:conc-name teds-))
  (dest-rect             (make-rect 0 0)        :type rect)
  (view-rect             (make-rect 0 0)        :type rect)
  (ff                    0                      :type integer)
  (ms                    0                      :type integer)
  (line-height           0                      :type integer)
  (font-ascent           0                      :type integer)
  (caret                 t                      :type boolean)
  (caret-rect            (make-rect 0 0)        :type rect)
  (caret-lino            0                      :type integer)
  (selection-rects      '()                     :type list)
  (selection-start-lino  0                      :type integer)
  (selection-end-lino    0                      :type integer)
  (lines                 (adjustable-vector 0)  :type vector #|(vector te-display-line-state *)|#))


;; (justified-line-left base-line line-rect line-text) (te-line-coordinates lino te)

(defun te-snapshot-display-state (te)
  (let ((dest-rect (te-dest-rect te)))
    (make-te-display-state
     :dest-rect            dest-rect
     :view-rect            (te-view-rect te)
     :ff                   (te-ff te)
     :ms                   (te-ms te)
     :line-height          (te-line-height te)
     :font-ascent          (te-font-ascent te)
     :caret                (te-has-caret te)
     :caret-rect           (if (te-has-caret te)   (make-rect 0 0))
     :caret-lino           (if (te-has-caret te)   0)
     :selection-rects      (if (te-has-caret te) '()  )
     :selection-start-lino (if (te-has-caret te) 0    )
     :selection-end-lino   (if (te-has-caret te) 0    )
     :lines (adjustable-vector 10 (vector (make-te-display-line-state
                                           :lino 0
                                           :rect (make-rect (rect-left dest-rect)
                                                            (rect-top dest-rect)
                                                            (rect-right dest-rect)
                                                            (+ (rect-top dest-rect) (te-line-height te)))
                                           :base (+ (rect-top dest-rect) (te-font-ascent te))
                                           :left (case (te-just te)
                                                   (-1 (rect-right dest-rect))
                                                   (1  (round (- (rect-right dest-rect) (rect-left dest-rect)) 2))
                                                   (t  0))
                                           :text ""))))))


;; (te-snapshot-display-state (test-window-te (front-window)))
;; #S(te-display-state :dest-rect #1=#S(rect :topleft 0 :bottomright 27394495)
;;                     :view-rect #1#
;;                     :ff 786432 :ms 65554
;;                     :line-height 18 :font-ascent 14
;;                     :caret t
;;                     :caret-rect #S(rect :topleft 0 :bottomright 0) :caret-lino 0
;;                     :selection-rects nil :selection-start-lino 0 :selection-end-lino 0
;;                     :lines #(#S(te-display-line-state :lino 0 :rect #S(rect :topleft 0 :bottomright 1180095) :base 14 :left 0 :text "")))


(defun te-display-state-changes (base new)
"
Compare the base and new display states, and generates a list of display changes:
- scrollrects
- lines needing redisplay


"
  )



(defun te-redraw-line (lino te)
  "
DO:    Draws the line number LINO, according to the current TeRec configuration.
NOTE:  The focus should be already established.
"
  (format-trace 'te-redraw-line :lino lino)
  (multiple-value-bind (x base line-rect line) (te-line-coordinates lino te)
    (erase-rect* (rect-left line-rect) (rect-top line-rect) (rect-width line-rect) (rect-height line-rect))
    (draw-string x base line))
  te)


(defun te-draw-line (lino te)
  "
DO:    Draws the line number LINO, according to the current TeRec configuration.
NOTE:  The focus should be already established.
"
  (format-trace 'te-draw-line :lino lino)
  (multiple-value-bind (x base line-rect line) (te-line-coordinates lino te)
    (declare (ignore line-rect))
    (draw-string x base line))
  te)






(defun te-update-view (clip-rect te)
  "
DO:     Draws the text, clipped by CLIP-RECT.
RETURN: TE
"
  (let ((updates (nreverse (te-updates te))))
    (when updates
      (setf (te-updates te) '())
      (when (te-in-port te)
        (with-font-focused-view (te-in-port te)
          (with-clip-rect-intersect clip-rect
            (loop
              :for update = (pop updates)
              :do (format-trace 'te-update-view :-> update)
              :do (if (atom update)
                      (ecase update
                        (:display
                         (loop :for lino :below (te-nlines te)
                               :do (te-draw-line lino te)))
                        (:selection
                         (if (te-has-caret te)
                             (te-caret-transition (tick-count) te)
                             (multiple-value-bind (rects start-lino end-lino)
                                 (te-compute-selection-rectangles (te-sel-start te) (te-sel-end te) te)
                               (funcall (or (te-high-hook te) (function te-default-high-hook))
                                        rects start-lino end-lino te)))))
                      (ecase (first update)
                        (:old-selection
                         (destructuring-bind (op start end) update
                           (declare (ignore op))
                           (when (/= start end)
                             (multiple-value-bind (rects start-lino end-lino)
                                 (te-compute-selection-rectangles start end te)
                               (declare (ignore rects))
                               (loop :for lino :from start-lino :to end-lino
                                     :do (te-redraw-line lino te))))))))
              :while updates))))))
  te)


(defmacro with-te-update-display (te &body body)
  ;; TODO: make it more sophisticated: We want to detect automatically
  ;; what has changed in the TE so that we may redisplay only the
  ;; lines and the selection that have changed.
  (let ((vte (gensym)))
    `(let ((,vte ,te))
       (multiple-value-prog1 (progn ,@body)
         (te-update-view (te-view-rect ,vte) ,vte)))))



;;;---------------------------------------------------------------------
;;; Caret
;;;---------------------------------------------------------------------

(defun te-compute-caret-rect (te)
  "
RETURN:  the rectangle where the caret must be drawn; the lino where the caret is.
"
  (assert (te-has-caret te))
  (let* ((caret-position (te-sel-start te))
         (lino (te-line-at caret-position te)))
    (multiple-value-bind (x base line-rect line) (te-line-coordinates lino te)
      (declare (ignore base))
      ;; (format-trace 'te-compute-caret-rect (list (rect-left line-rect) (rect-top line-rect) (rect-right line-rect) (rect-bottom line-rect)))
      (let ((caret-x (+ x (te-string-width line te 0 (- caret-position (aref (te-line-starts te) lino))))))
        (values (make-rect (1- caret-x) (rect-top line-rect) (1+ caret-x) (rect-bottom line-rect)) lino)))))


(defun te-default-caret-hook (rect lino te)
  "The default caret hook function."
  ;; TODO: add a visibility test.
  (if (plusp (te-caret-state te))
      (fill-rect*  (rect-left rect) (rect-top rect) (rect-width rect) (rect-height rect))
      (te-redraw-line lino te)
      #-(and)
      (with-clip-rect-intersect rect
        (erase-rect* (rect-left rect) (rect-top rect) (rect-width rect) (rect-height rect))
        (te-draw-line lino te))))


(defun te-call-caret-hook (te)
  (when (te-in-port te)
    (multiple-value-bind (caret-rect lino) (te-compute-caret-rect te)
      (with-font-focused-view (te-in-port te)
        (with-clip-rect-intersect (te-view-rect te)
          (funcall (or (te-caret-hook te)  (function te-default-caret-hook)) caret-rect lino te))))))


(defun te-caret-transition (tick te)
  "
DO:     Performs a caret transition when times has come.
"
  (when (<= (te-caret-time te) tick)
    ;; (format-trace 'te-caret-transition :tick tick :time (te-caret-time te) :has-caret  (te-has-caret te))
    (when (te-has-caret te)
      (let ((period (round *caret-time* 2)))
        (setf (te-caret-time  te) (* (1+ (truncate tick period)) period)
              (te-caret-state te) (- 1 (te-caret-state te))))
      (te-call-caret-hook te))))


(defun te-erase-caret (te)
  "
DO:     Erase the caret.
"
  (setf (te-caret-state te) 0)
  (te-call-caret-hook te))


;;;---------------------------------------------------------------------
;;; Selection highlighting
;;;---------------------------------------------------------------------


(declaim (inline te-clean-selection))
(defun te-clean-selection (start end te)
  "
DO:     Sort the start and end points, and clip them to text limits.
RETURN: new-start, new-end
"
  (when (< end start) (rotatef start end))
  (setf start (max start 0))
  (setf end   (min end   (te-length te)))
  (values start end))


(defun te-default-high-hook (rects start-lino end-lino te)
  "The default selection highlight hook function."
  ;; (format-trace 'te-default-high-hook :start-lino start-lino :end-lino end-lino :rects (mapcar (function rect-to-list) rects))
  ;; TODO: add a visibility test.
  (flet ((highlight (rect start-lino end-lino)
           (format-trace 'highlight :rect (rect-to-list rect) :start start-lino :end end-lino)
           (with-clip-rect-intersect rect
             (with-fore-color (if (te-is-active te)
                                  *selection-color-active*
                                  *selection-color-inactive*)
               (if (te-has-caret te)
                   (erase-rect* (rect-left rect) (rect-top rect) (rect-width rect) (rect-height rect))
                   (fill-rect* (rect-left rect) (rect-top rect) (rect-width rect) (rect-height rect))))
             (loop
               :for lino :from start-lino :to end-lino
               :do (te-draw-line lino te)))))
    (case (length rects)
      (1
       (let ((rect (first rects)))
         (highlight rect start-lino start-lino)))
      (2
       (loop
         :for rect :in rects
         :for lino :in (list start-lino end-lino)
         :do (highlight rect lino lino)))
      (3
       (destructuring-bind (start-rect middle-rect end-rect) rects
         (highlight start-rect  start-lino      start-lino)
         (highlight middle-rect (1+ start-lino) (1- end-lino))
         (highlight end-rect    end-lino        end-lino)))))
  (graphics-flush))



(defun te-compute-selection-rectangles (start end te)
  "
START:  The start point of the selection.

END:    The end point of the selection.

DO:     Compute the rectangles covering the selection.  There may be
        one, two or three rectangles depending on whether the
        selection is on a single line, on two lines or more.

RECTANGLESP:
        When true, returns a list of rectangles, when NIL, returns the
        number of rectangles it would compute.

RETURN: rects-or-count, start-lino, end-lino, start-column, end-column.
"
  (values-list
   (cdr (if (equal (car (te-selection-rectangles-cache te)) (cons start end))
            (te-selection-rectangles-cache te)
            (setf (te-selection-rectangles-cache te)
                  (cons (cons start end)
                        (let ((start-lino (te-line-at start te))
                              (end-lino   (te-line-at end   te)))
                          (cond
                            ((= start-lino end-lino)
                             ;; 1 rect
                             (multiple-value-bind (x base line-rect line) (te-line-coordinates start-lino te)
                               (declare (ignore base))
                               (let* ((line-start  (aref (te-line-starts te) start-lino))
                                      (start-column (- start line-start))
                                      (end-column   (- end   line-start))
                                      (left        (+ x (te-string-width line te 0 start-column)))
                                      (right       (+ x (te-string-width line te 0 end-column))))
                                 (list (list (make-rect left  (rect-top    line-rect)
                                                        right (rect-bottom line-rect)))
                                       start-lino end-lino start-column end-column))))
                            ((= (1+ start-lino) end-lino)
                             ;; 2 rects
                             (multiple-value-bind (start-x start-base start-line-rect start-line) (te-line-coordinates start-lino te)
                               ;; TODO: perhaps we should forward those data for the highlight function…
                               (declare (ignore start-base))
                               (multiple-value-bind (end-x end-base end-line-rect end-line) (te-line-coordinates end-lino te)
                                 (declare (ignore end-base))
                                 (let* ((start-line-start  (aref (te-line-starts te) start-lino))
                                        (end-line-start    (aref (te-line-starts te) end-lino))
                                        (start-column      (- start start-line-start))
                                        (end-column        (- end   end-line-start))
                                        (start-left  (+ start-x (te-string-width start-line te 0 start-column)))
                                        (end-right   (+ end-x   (te-string-width end-line   te 0 end-column))))
                                   (list (list (make-rect start-left                   (rect-top    start-line-rect)
                                                          (rect-right start-line-rect) (rect-bottom start-line-rect))
                                               (make-rect (rect-left  end-line-rect)   (rect-top    end-line-rect)
                                                          end-right                    (rect-bottom end-line-rect)))
                                         start-lino end-lino start-column end-column)))))
                            (t
                             ;; 3 rects
                             (multiple-value-bind (start-x start-base start-line-rect start-line) (te-line-coordinates start-lino te)
                               (declare (ignore start-base))
                               (multiple-value-bind (end-x end-base end-line-rect end-line) (te-line-coordinates end-lino te)
                                 (declare (ignore end-base))
                                 (let* ((start-line-start  (aref (te-line-starts te) start-lino))
                                        (end-line-start    (aref (te-line-starts te) end-lino))
                                        (start-column      (- start start-line-start))
                                        (end-column        (- end   end-line-start))
                                        (start-left  (+ start-x (te-string-width start-line te 0 start-column)))
                                        (end-right   (+ end-x   (te-string-width end-line   te 0 end-column))))
                                   (list (list (make-rect start-left                   (rect-top    start-line-rect)
                                                          (rect-right start-line-rect) (rect-bottom start-line-rect))
                                               (make-rect (rect-left  start-line-rect) (rect-bottom start-line-rect)
                                                          (rect-right end-line-rect)   (rect-top    end-line-rect))
                                               (make-rect (rect-left  end-line-rect)   (rect-top    end-line-rect)
                                                          end-right                    (rect-bottom end-line-rect)))
                                         start-lino end-lino start-column end-column)))))))))))))





;;;---------------------------------------------------------------------
;;; Public API
;;;---------------------------------------------------------------------


;; We need a mutex to avoid re-entry of te-idle (or other API
;; functions, from different threads).
;;
;; We could have one mutex per TextEdit Record, but it shouldn't be
;; too much of a restriction to use a single global mutex.

(defvar *te-mutex* nil)


(defun te-init ()
  "Initialize the Text Edit Manager."
  (setf *te-mutex* (make-mutex "Text Edit Mutex")
        *selection-color-active*     *light-blue-color*
        *selection-color-inactive*   *light-gray-color*)
  (values))


(defun te-cal-text (te)
  "
DO:     Compute nlines and line-starts, as the number and positions of
        displayed lines.  When (plusp (te-cr-only te)) paragraphs are
        wrapped, so several lines may be computed for a paragraph.
RETURN: TE
"
  (with-mutex *te-mutex*
    (te-calculate-text te))
  te)


(defun te-set-rects (dest-rect view-rect te)
  "
DO:         Updates the dest-rect and the view-rect of the TeRec TE,
            recomputing the line starts.
DEST-RECT:  A non-empty rectangle.
VIEW-RECT:  A non-empty rectangle.
RETURN:     TE
"
  (assert (not (empty-rect-p dest-rect)) (dest-rect) "Destination rectangle must not be empty.")
  (assert (not (empty-rect-p view-rect)) (view-rect) "View rectangle must not be empty.")
  (with-mutex *te-mutex*
    (setf (te-dest-rect te) dest-rect
          (te-view-rect te) view-rect)
    (te-calculate-text te))
  te)


;;; Globally replace the text in the TextEdit Record:
(defun te-set-text (text te)
  "
DO:     Incorporates a copy of the specified text into the edit record
        specified by TE.  The TEXT is a string containing the text.
        The selection range is set to an insertion point at the end of
        the text.  TE-SET-TEXT doesn't affect the text drawn in the
        destination rectangle, so call INVAL-RECT afterward if
        necessary. 
TEXT:   A string containing the text to be copied into the Text Edit record.
RETURN: TE
"
  (let* ((paras      (split-sequence #\newline text))
         (pcount     (length paras))
         (paragraphs (make-array (+ 64 pcount) :adjustable t :initial-element nil)))
    (loop
      :for pos = 0 :then (+ pos (length para) 1)
      :for i :from 0
      :for para :in paras
      :do (setf (aref paragraphs i) (cons pos para)))
    (with-mutex *te-mutex*
      (setf (te-paragraphs                     te) paragraphs
            (te-nparagraphs                    te) pcount
            (te-current-paragraph-index        te) (1- pcount)
            (te-next-paragraph-index           te) (length paragraphs))
      (%te-split-paragraph (1- pcount) te)
      (setf (te-sel-start   te) (length text)
            (te-sel-end     te) (length text)
            (te-sel-current te) (length text))
      (te-calculate-text te))
    te))


(defun te-get-text (te)
  "
RETURN: A copy of the text in the Text Edit record, as a STRING.
"
  (with-mutex *te-mutex*
    (with-output-to-string (*standard-output*)
      (let ((newline nil))
        (doparagraphs (i para te)
          (declare (ignore i))
          (if newline
              (terpri)
              (setf newline t))
          (write-string (cdr para)))))))


(defun te-new (dest-rect view-rect window)
  "
DO:     Creates and initializes an edit record, and returns a handle
        to the new edit record.  DEST-RECT and VIEW-RECT are the
        destination and view rectangles, respectively.  Both
        rectangles are specified in the WINDOW coordinates.  The
        destination rectangle must always be at least as wide as the
        first character drawn (about 20 pixels is a good minimum
        width). The view rectangle must not be empty (for example,
        don't make its right edge less than its left edge if you don't
        want any text visible—specify a rectangle off the screen
        instead).  The edit record incorporates the drawing
        environment of the WINDOW, and is initialized for
        left-justified, single-spaced text with an insertion point at
        character position 0.  Note: The caret won't appear until you
        call TE-ACTIVATE.
RETURN: The new TeRec.
"
  (assert (not (empty-rect-p dest-rect)) (dest-rect) "Destination rectangle must not be empty.")
  (assert (not (empty-rect-p view-rect)) (view-rect) "View rectangle must not be empty.")
  (let ((te (make-terec :dest-rect dest-rect :view-rect view-rect :in-port window)))
    (te-set-default-bindings te)
    (setf (te-word-break te) (function te-default-word-break)
          (te-caret-hook te) (function te-default-caret-hook)
          (te-high-hook  te) (function te-default-high-hook))
    (te-set-text "" te)
    te))


(defun te-dispose (te)
  "Call this procedure when you're completely through with an edit record."
  (declare (ignore te))
  (values))


(defun te-idle (te)
  "
Call TE-IDLE repeatedly to make a blinking caret appear at the
insertion point (if any) in the text specified by TE.  (Title caret
appears only when the window containing that text is active, of
course.)  TextEdit observes a minimum blink interval specified by
*CARET-HALF-PERIOD*: no matter how often you call TE-IDLE, the time
between blinks will never be less than the minimum interval.

Note: The initial minimum blink interval setting is 32 ticks. The
user can adjust this by setting *CARET-HALF-PERIOD*.

To provide a constant, frequency of blinking, you should call TE-IDLE
as often as possible-at least once each time through your main event
loop.  Call it more than once if your application does an unusually
large amount of processing each time through the loop.

Note: You actually need to call TE-IDLE only when the window
containing the text is active.
"
  (with-mutex *te-mutex*
    (when (te-is-active te)
      (te-caret-transition (tick-count) te))))


(defun %te-coordinates-at-point (pt te)
  (let* ((h     (point-h pt))
         (v     (point-v pt))
         (lino  (truncate (- v (rect-top (te-dest-rect te)))
                          (te-line-height te)))
         (start (if (<= (length (te-line-starts te)) lino)
                    (te-length te)
                    (aref (te-line-starts te) lino)))
         (lino  (if (<= (length (te-line-starts te)) lino)
                    (1- (length (te-line-starts te)))
                    lino)))
    (values (multiple-value-bind (x base line-rect line) (te-line-coordinates lino te)
              (declare (ignore base line-rect))
              (if (<= h x)
                  start
                  (let ((ff         (te-ff te))
                        (ms         (te-ms te))
                        (offset     (- h x)))
                    (multiple-value-bind (found index order)
                        (dichotomy (lambda (charpos)
                                     (let ((width (font-codes-string-width line ff ms 0 charpos)))
                                       (real-compare offset width)))
                                   0 (length line))
                      (cond
                        (found
                         (+ start index))
                        ((minusp order)
                         ;; should not occur for above (<= h x) test.
                         start) 
                        ((<= (1- (length line)) index)
                         ;; TODO: We're not handling the spaces suffix.
                         (+ start (length line)))
                        (t (let ((width      (font-codes-string-width line ff ms 0 index))
                                 (next-width (font-codes-string-width line ff ms 0 (1+ index))))
                             (+ start index (if (< h (round (+ width next-width) 2)) 0 1)))))))))
            lino)))


(defun te-word-at (charpos lino te)
  "
PRE:    LINO is the line number containing CHARPOS.
RETURN: The start and end positions of the word that contains CHARPOS.
"
  (let* ((word-break (or (te-word-break te) (function te-default-word-break)))
         (line-start (aref (te-line-starts te) lino))
         (start      (- charpos line-start))
         (end        start)
         (line       (te-line lino te)))
    (if (zerop (length line))
        (values charpos charpos)
        (progn
          (format-trace 'te-word-at :charpos charpos :lino :lino
                        :start start :line line)
          (loop
            :until (or (minusp start) (funcall word-break line start))
            :do (decf start)
            :finally (incf start))
          (loop
            :until (or (<= (length line) end) (funcall word-break line end))
            :do (incf end))
          (format-trace 'te-word-at :word (subseq line start end))
          (values (+ start line-start) (+ end line-start))))))


(defun te-default-click-loop ()
  (not (wait-mouse-up)))


(defun %te-click-loop (word extend initial-charpos initial-lino current-charpos current-lino te)
  (let ((click-loop (or (te-click-loop te) (function te-default-click-loop))))
    (if word
        (multiple-value-bind (istart iend) (te-word-at initial-charpos initial-lino te)
          (multiple-value-bind (cstart cend) (te-word-at current-charpos current-lino te)
            (loop
              ;; TODO: This is not good.
              :do (if extend
                      (te-set-select (min istart cstart) (max iend cend) te)
                      (te-set-select cstart cend te))
              :until (funcall click-loop)
              :do (let ((pt (get-mouse)))
                    (multiple-value-bind (new-charpos new-lino) (%te-coordinates-at-point pt te)
                    (format-trace 'click-loop :pt (point-to-list pt) :new-charpos new-charpos)
                      (when (/= current-charpos new-charpos)
                        (setf current-charpos new-charpos
                              current-lino new-lino)
                        (multiple-value-setq (cstart cend) (te-word-at new-charpos new-lino te))))))))
        ;; 
        (loop
          :do (if extend
                  (te-set-select current-charpos (if (< current-charpos (round (+ (te-sel-start te)
                                                                                  (te-sel-end te)) 2))
                                                     (te-sel-end   te)
                                                     (te-sel-start te)) te) 
                  (te-set-select current-charpos initial-charpos te))
          :until (funcall click-loop)
          :do (setf current-charpos (%te-coordinates-at-point (get-mouse) te))))))


(defun te-click (pt extend te)
  "

TEClick controls the placement and highlighting of the selection range
as determined by mouse events. Call TEClick whenever a mouse-down
event occurs in the view rectangle of the edit record specified by
hTE, and the window associated with that edit record is
active. TEClick keeps control until the mouse button is released. Pt
is the mouse location (in local coordinates) at the time the button
was pressed, obtainable from the event record.

Note: Use the QuickDraw procedure GlobalToLocal to convert the global
coordinates of the mouse location given in the event record to the
local coordinate system for pt.

Pass TRUE for the extend parameter if the Event Manager indicates that
the Shift key was held down at the time of the click (to extend the
selection).

TEClick unhighlights the old selection range unless the selection
range is being extended. If the mouse moves, meaning that a drag is
occurring, TEClick expands or shortens the selection range
accordingly. In the case of a double-click, the word under the cursor
becomes the selection range; dragging expands or shortens the
selection a word at a time.

"
  (with-mutex *te-mutex*
    (let* ((now    (tick-count))
           (double (and (< (- now (te-click-time te)) *double-click-time*)
                        (< (point-distance pt (te-click-loc te)) *double-click-jitter*))))
      (multiple-value-bind (charpos lino) (%te-coordinates-at-point pt te)
        (if double
            (%te-click-loop t extend
                            (car (te-click-stuff te)) (cdr (te-click-stuff te))
                            charpos lino te)
            (progn
              (setf (te-click-time  te) now
                    (te-click-loc   te) pt
                    (te-click-stuff te) (cons charpos lino))
              (%te-click-loop nil extend
                              charpos lino
                              charpos lino te)))))))




#|

word & extend:
      click-stuff.charpos -> a.start-word a.end-word
      pt.charpos          -> b.start-word b.end-word
      sel.start   <- (min a.start-word b.start-word)
      sel.end     <- (min a.end-word b.end-word)
      sel.current <- closer to pt.charpos of b.start-word b.end-word

      NOPE: current shall be determined by the next clic
      NOTE: it seems reducing shift-click selections are done with
            respect to the middle of the selection text anyways.

word & ¬extend:
      pt.charpos          -> start-word end-word
      sel.start   <- start-word 
      sel.end     <- end-word
      sel.current <- closer to pt.charpos of b.start-word b.end-word


¬word & extend
      sel.start   <- (min click-stuff.charpos pt.charpos)
      sel.end     <- (max click-stuff.charpos pt.charpos)
      sel.current <- pt.charpos

¬word & ¬extend
      sel.start   <- pt.charpos
      sel.end     <- pt.charpos
      sel.current <- pt.charpos

|#



(defun te-set-select (start end te)
  "

TESetSelect sets the selection range to the text between selStart and
selEnd in the text specified by hTE. The old selection range is
unhighlighted, and the new one is highlighted. If selStart equals
selEnd, the selection range is an insertion point, and a caret is
displayed.

SelEnd and selStart can range from 0 to 32767. If selEnd is anywhere
beyond the last character of the text, the position just past the last
character is used.

"
  (with-mutex *te-mutex*
    (with-te-update-display te
     (multiple-value-bind (start end) (te-clean-selection start end te)
       (when (and (te-in-port te) (te-has-caret te))
         (te-erase-caret te))
       (push (list :old-selection (te-sel-start te) (te-sel-end te)) (te-updates te))
       (setf (te-sel-start   te) start
             (te-sel-end     te) end
             (te-sel-current te) end)
       (push :selection (te-updates te))))))


(defun te-activate (te)
  "

TEActivate highlights the selection range in the view rectangle of the
edit record specified by hTE. If the selection range is an insertion
point, it displays a caret there. This procedure should be called
every time the Toolbox Event Manager function GetNextEvent reports
that the window containing the edit record has become active.

"
  (with-mutex *te-mutex*
    (with-te-update-display te
     (setf (te-active te) 1
           (te-caret-time te) (tick-count))
     (push :selection (te-updates te)))))


(defun te-deactivate (te)
  "

TEDeactivate unhighlights the selection range in the view rectangle of
the edit record specified by hTE. If the selection range is an
insertion point, it removes the caret. This procedure should be called
every time the Toolbox Event Manager function GetNextEvent reports
that the window containing the edit record has become inactive.

"
  (with-mutex *te-mutex*
    (with-te-update-display te
     (setf (te-active te) 0)
     (push :selection (te-updates te)))))


(defun te-key (char te)
  "
TE-KEY processes the character CHAR, possibly modified by the
*CURRENT-EVENT* modifiers.  It actually goes thru a binding table
mapping key chords (for now, using only the character code of CHAR),
to editing commands.

The default command is TE-SELF-INSERT-COMMAND, which performs the
following default TE-KEY algorithm:

TE-KEY replaces the selection range in the text specified by TE with
the character given by the key parameter, and leaves an insertion
point just past the inserted character. If the selection range is an
insertion point, TE-KEY just inserts the character there. If the key
parameter contains a Backspace character, the selection range or the
character immediately to the left of the insertion point is
deleted. TE-KEY redraws the text as necessary. Call TE-KEY every time
the Toolbox Event Manager function GetNextEvent reports a keyboard
event that your application decides should be handled by TextEdit.

Note: TE-KEY inserts every character passed in the key parameter, so
it's up to your application to filter out all characters that aren't
actual text (such as keys typed in conjunction with the Command key).

The other commands implemented are mostly emacs simple Control key:

    default       te-self-insert-command
    C-@           te-set-mark-command
    C-A           te-beginning-of-line-command     (*)
    C-B           te-backward-char-command         (*)
    C-C           te-enter-command
    C-D           te-delete-char-command
    C-E           te-end-of-line-command           (*)
    C-F           te-forward-char-command          (*)
    C-J           te-newline-command
    C-K           te-kill-line-command
    C-L           te-recenter-top-bottom-command
    C-M           te-newline-command
    C-N           te-next-line-command             (*)
    C-O           te-open-line-command
    C-P           te-previous-line-command         (*)
    C-T           te-transpose-chars-command
    C-V           te-scroll-up-command
    C-W           te-kill-region-command
    C-Y           te-yank-command
    Rubout        te-delete-backward-char-command
    up-arrow      te-previous-line-command         (*)
    down-arrow    te-next-line-command             (*)
    left-arrow    te-backward-char-command         (*)
    right-arrow   te-forward-char-command          (*)

The commands marked with (*) also extend the selection when used with
the Shift key.
"
  (let ((code (char-code char))
        (mods (if *current-event*
                  (event-modifiers *current-event*)
                  0)))
    (format-trace 'te-key :modifiers mods :code code :char char)
    (with-mutex *te-mutex*
      (with-te-update-display te
        (when (te-has-caret te)
          (te-erase-caret te))
        (funcall (te-get-binding code te) mods code char te)))))


(defun te-cut (te)
  "
DO:     Removes the selection range from the text specified by TE and
        places it in the TextEdit scrap.  The text is redrawn as
        necessary.  Anything previously in the scrap is deleted.  If
        the selection range is an insertion point, the scrap is
        emptied.
"
  (with-mutex *te-mutex*
    (with-te-update-display te
     (%te-copy   te)
     (%te-delete te))))


(defun te-copy (te)
  "
DO:     Copies the selection range from the text specified by TE into
        the TextEdit scrap.  Anything previously in the scrap is
        deleted.  The selection range is not deleted.  If the
        selection range is an insertion point, the scrap is emptied.
"
  (with-mutex *te-mutex*
    (%te-copy te)))


(defun te-paste (te)
  "
DO:     Replaces the selection range in the text specified by TE with
        the contents of the TextEdit scrap, and leaves an insertion
        point just past the inserted text.  The text is redrawn as
        necessary.  If the scrap is empty, the selection range is
        deleted. If the selection range is an insertion point,
        TE-PASTE just inserts the scrap there.
"
  (with-mutex *te-mutex*
    (with-te-update-display te
     (%te-delete te)
     (%te-insert (get-scrap :text) te))))


(defun te-delete (te)
  "
DO:     removes the selection range from the text specified by TE,
        and redraws the text as necessary. TE-DELETE is the same as
        TE-CUT (above) except that it doesn't transfer the selection
        range to the scrap.  If the selection range is an insertion
        point, nothing happens.
"
  (with-mutex *te-mutex*
    (with-te-update-display te
      (%te-delete te))))


(defun te-insert (text te)
  "

DO:     TE-INSERT takes the specified text and inserts it just before
        the selection range into the text indicated by TE, redrawing
        the text as necessary. The text parameter points to the text
        to be inserted, and the length parameter indicates the number
        of characters to be inserted. TE-INSERT doesn't affect either
        the current selection range or the scrap.

"
  (with-mutex *te-mutex*
    (with-te-update-display te
      (let ((start   (te-sel-start   te))
            (end     (te-sel-end     te))
            (current (te-sel-current te)))
        (setf (te-sel-current te) start)
        (%te-insert text te)
        (setf (te-sel-start   te) (+ start   (length text))
              (te-sel-end     te) (+ end     (length text))
              (te-sel-current te) (+ current (length text)))))))


(defun te-set-just (just te)
  "
DO:     Sets the justification of the text specified by TE to
        just. TextEdit provides three predefined constants for setting
        justification:

            TE-JUST-LEFT
            TE-JUST-CENTER
            TE-JUST-RIGHT

        By default, text is left-justified. If you change the
        justification, call InvalRect after TE-SET-JUST, so the text
        will be redrawn with the new justification.
"
  (check-type just (member -1 0 1))
  (with-mutex *te-mutex*    
    (setf (te-just te) just)
    (te-cal-text te)))


(defun te-update (update-rect te)
  "
DO:     Draws the text specified by TE within the rectangle specified
        by UPDATE-RECT (given in the coordinates of the current
        window). Call TE-UPDATE every time the Toolbox Event Manager
        function GetNextEvent reports an update event for a text
        editing window, in WINDOW-UPDATE-EVENT-HANDLER or in the
        VIEW-DRAW-CONTENTS of the window, since it's called from
        WINDOW-UPDATE-EVENT-HANDLER.  TE-UPDATE should be called in
        the dynamic context established by WITH-FOCUSED-VIEW on the
        window.
"
  (let ((clip-rect (make-rect 0 0)))
    (with-mutex *te-mutex*
      (intersect-rect update-rect (te-view-rect te) clip-rect)
      (push :display   (te-updates te))
      (push :selection (te-updates te))
      (te-update-view clip-rect te))))


(defun text-box (text box just)
  "
DO:     TextBox draws the specified text in the rectangle indicated by
        the box parameter, with justification just. (See
        \"Justification\" under \"Edit Records\").  The text parameter
        points to the text, and the length parameter indicates the
        number of characters to draw.  The rectangle is specified in
        local coordinates, and must be at least as wide as the first
        character drawn (a good rule of thumb is to make it at least
        20 pixels wide).  TextBox creates its own edit record, which
        it deletes when it's finished with it, so the text it draws
        cannot be edited.
"
  ;; TODO
  )


(defun te-scroll (dh dv te)
  "
DO:     scrolls the text within the view rectangle of the specified
        edit record by the number of pixels specified in the dh and dv
        parameters.  The edit record is specified by the hTE
        parameter.  Positive dh and dv values move the text right and
        down, respectively, and negative values move the text left and
        up.  For example,

            TEScroll(Of—hTEA A.lineHeight,hTE)

        scrolls the text up one line.  Remember that you scroll text
        up when the user clicks in the scroll arrow pointing down.
        The destination rectangle is offset by the amount you scroll.

NOTE:   To implement automatic scrolling, you store the address of a
        routine in the clikLoop field of the edit record, as described
        above under \"The TERec Data Type\".
"
  ;; TODO
  )


(defun te-from-scrap ()
  "
DO:     Copies the desk scrap to the TextEdit scrap. If no error
        occurs, it returns the result code noErr; otherwise, it
        returns an appropriate Operating System result code.

NOTE:   We do nothing since our GET-SCRAP already gets directly from
        the Desk scrap.
"
  0)


(defun te-to-scrap ()
  "
DO:     Copies the TextEdit scrap to the desk scrap. If no error
        occurs, it returns the result code noErr; otherwise, it
        returns an appropriate Operating System result code.

WARNING:
        You must call the Scrap Manager function ZeroScrap to
        initialize the desk scrap or clear its previous contents
        before calling TEToScrap.

NOTE:   We do nothing since our PUT-SCRAP already writes directly the
        Desk scrap.
"
  0)


(defun te-scrap-handle ()
  "
RETURN: a handle to the TextEdit scrap.
"
  *scrap-handler*)


(defun te-get-scrap-len ()
  "
RETURN: the size of the TextEdit scrap in bytes.
"
  (length (get-scrap :text)))


(defun te-set-scrap-len (length)
  "
DO:     sets the size of the TextEdit scrap to the given number of bytes.
"
  (declare (ignore length))
  (values))


(defun set-word-break (word-proc te)
  "
DO:     Installs in the wordBreak field of the specified edit record a
        special routine that calls the word break routine pointed to
        by wBrkProc. The specified word break routine will be called
        instead of TextEdit's default routine, as described under
        \"The WordBreak Field\" in the \"Edit Records\" section.
"
  (with-mutex *te-mutex*
    (setf (te-word-break te) word-proc)))


(defun set-click-loop (click-proc te)
  "
DO:     Installs in the clikLoop field of the specified edit record a
        special routine that calls the click loop routine pointed to
        by clikProc. The specified click loop routine will be called
        repeatedly as long as the user holds down the mouse button
        within the text, as described above under \"The ClikLoop
        Field\" in the \"Edit Records\" section.
"
  (with-mutex *te-mutex*
    (setf (te-click-loop te) click-proc)))


;;;--------------------------------------------------------------------
;;; Commands (key bindings):
;;;--------------------------------------------------------------------


(defun te-selected-text (te)
  "
RETURN: a string containing the selected text from the TEREC.
"
  (if (= (te-sel-start te) (te-sel-end te))
      ""
      (multiple-value-bind (rects start-lino end-lino start-column end-column)
          (te-compute-selection-rectangles (te-sel-start te) (te-sel-end te) te)
        (ecase (length rects)
          (1
           (subseq (te-line start-lino te) start-column end-column))
          (2
           (with-output-to-string (*standard-output*)
             (write-line   (subseq (te-line start-lino te) start-column))
             (write-string (subseq (te-line end-lino te) 0 end-column))))
          (3
           (with-output-to-string (*standard-output*)
             (write-line   (subseq (te-line start-lino te) start-column))
             (loop
               :for lino :from (1+ start-lino) :to (1- end-lino)
               :do (write-line (te-line lino te)))
             (write-string (subseq (te-line end-lino te) 0 end-column))))))))


(defun %te-copy (te)
  (if (= (te-sel-start te) (te-sel-end te))
      (zero-scrap)
      (put-scrap :text (te-selected-text te))))

(defun %te-split-paragraph (charpos te)
  
  )


(defun %te-delete (te)
  (let ((start (te-sel-start te))
        (end   (te-sel-end te)))
    (unless (= start end)
      (multiple-value-bind (rects start-lino end-lino start-column end-column)
          (te-compute-selection-rectangles start end te)
        (declare (ignore start-lino end-lino end-column))
        (if (= 1 (length rects))
            (progn
              (te-split-paragraph-at end te)
              (decf (fill-pointer (cdr (te-current-paragraph-before-point te))) (- end start)))
            (let ((start-parno (te-paragraph-at start te))
                  (end-parno   (te-paragraph-at end   te)))
              (te-split-paragraph-at end te)
              (let ((index (te-current-paragraph-index te))
                    (deleted-para-count (- end-parno start-parno)))
                (loop
                  :repeat deleted-para-count
                  :do (setf (aref (te-paragraphs te) index) nil)
                      (decf index))
                (assert (= start-parno index))
                (let* ((para   (te-paragraph index te))
                       (before (adjustable-string (* 2 start-column) (subseq (cdr para) 0 start-column))))
                  (setf (te-current-paragraph-before-point te) (cons (car (te-current-paragraph-before-point te))
                                                                     before)
                        (te-current-paragraph-dirty te) t)
                  (decf (te-nparagraphs te) deleted-para-count))))))
      (%te-adjust-starts start (- end start) te)
      (setf (te-sel-end     te) end
            (te-sel-current te) end))))


(defun %te-insert (text te)
  (assert (= (te-sel-start te) (te-sel-end te)))
  (let* ((paragraphs (split-sequence #\Newline text))
         (nparas     (length paragraphs))
         (plen       (length (first paragraphs)))
         (charpos    (te-sel-current te)))
    (format-trace '%te-insert :text text :charpos charpos)
    (te-split-paragraph-at charpos te)
    (when (<= (te-gap-size te) nparas)
      (te-adjust-gap nparas te))
    (let* ((after   (cdr (te-current-paragraph-after-point   te))) ; keep after the insertion point
           (before  (cdr (te-current-paragraph-before-point  te)))
           (current (length before))
           (nlen    (+ current plen)))
      ;; insert into the current paragraph:
      (setf before (adjust-array before nlen :fill-pointer nlen))
      (replace before (pop paragraphs) :start1 current)
      (setf (car (te-current-paragraph-after-point   te)) (+ nlen (car (te-current-paragraph-before-point  te)))
            (cdr (te-current-paragraph-after-point   te)) "")
      (te-current-paragraph te) ; unsplit current paragraph.
      (let* ((index   (te-current-paragraph-index te))
             (parapos (loop
                        :for parapos = (1+ (car (te-current-paragraph-after-point   te)))
                          :then (+ parapos plen 1)
                        :for para :in paragraphs
                        :for plen = (length para)
                        :do (setf (aref (te-paragraphs te) (incf index))
                                  (cons parapos (adjustable-string plen para)))
                        :finally (return parapos))))
        (decf index)
        (setf (te-current-paragraph-index te) index)
        (%te-split-paragraph index te)
        (setf (cdr (te-current-paragraph-after-point te)) after)
        ;; compute new lines and adjust following.
        (setf (te-sel-start   te) parapos
              (te-sel-end     te) parapos
              (te-sel-current te) parapos)))))


(defun te-self-insert-command (mods code char te)
  (declare (ignore mods code))
  (when (/= (te-sel-start te) (te-sel-end te))
    ;; TODO: save the selection for undo
    (%te-delete te))
  (%te-insert (string char) te)
  (values))


(defun te-transpose-chars-command (mods code char te)
  (declare (ignore mods code char))
  ;; Note: transpose-chars at the end of a line doesn't move forward,
  ;; but in the middle of a line, it moves forward.

  ;; if after is empty then just swap the two previous characters (if
  ;; there's only one then the previous char moves up the end of the
  ;; previous line, but at the beginning of buffer, it just beeps).
  ;; if after is not empty then swap the previous and next and advance one char.
  
  ;; TODO
  (values))


(defun te-newline-command (mods code char te)
  (declare (ignore mods code char))
  ;; use after to create a new paragraph and move to it (beginning of line).
  ;; TODO
  (values))


(defun te-open-line-command (mods code char te)
  (declare (ignore mods code char))
  ;; use after to create a new paragraph and stay on the old paragraph.
  ;; TODO
  (values))


(defun te-kill-region-command (mods code char te)
  (declare (ignore mods code char))
  (%te-copy te)
  (%te-delete te)
  (values))

;; deleting:

(defun te-delete-backward-char-command (mods code char te)
  (te-split-paragraph-at (te-sel-end te) te)
  ;; if beginning of line, then join with the previous line
  ;; else delete the previous character (in before).
  ;; TODO
  (values))


(defun te-delete-char-command (mods code char te)
  (te-split-paragraph-at (te-sel-end te) te)
  ;; if end of line, then join with the next line
  ;; else delete the following character (in after).
  ;; TODO
  (values))


(defun te-join-next-line (lino te)
  ;; TODO
  )


(defun te-kill-line-command (mods code char te)
  (declare (ignore mods code char))
  (let* ((lino (te-line-at (te-sel-current te) te))
         (end  (if (< (1+ lino) (length (te-line-starts te)))
                   (1- (aref (te-line-starts te) (1+ lino)))
                   (te-length te))))
    (if (and (< end (te-length te))
             (= (te-sel-current te) end))
        (te-join-next-line lino te)
        (progn
          (te-set-select (te-sel-current te) end te)
          (%te-copy te)
          (%te-delete te))))
  (values))


(defun te-yank-command (mods code char te)
  (declare (ignore mods code char))
  (%te-copy te)
  (values))


(defun te-undo-command (mods code char te)
  (declare (ignore mods code char te))
  ;; We don't implement it yet.
  ;; Usually, it's implemented by the application anyways.
  (values))


;; movement:

#-(and) "

+-+-+-|-+-+-+-+-+-+-+
: : : | : : : : : : :
+-+-+-|-+-+-+-+-+-+-+
      ->
+-+-+-+-|-+-+-+-+-+-+
: : : : | : : : : : :
+-+-+-+-|-+-+-+-+-+-+


+-+-+-|-+-+-+-+-+-+-+
: : : | : : : : : : :
+-+-+-|-+-+-+-+-+-+-+
  2 shift ->
+-+-+-|-+-|-+-+-+-+-+
: : : | : | : : : : :
+-+-+-|-+-|-+-+-+-+-+
      s   e
          c
    shift <-    
+-+-+-|-|-+-+-+-+-+-+
: : : | | : : : : : :
+-+-+-|-|-+-+-+-+-+-+
      s e
        c
    shift <-    
+-+-+-|-+-+-+-+-+-+-+
: : : | : : : : : : :
+-+-+-|-+-+-+-+-+-+-+
      s
      e
      c
    shift <-    
+-+-|-|-+-+-+-+-+-+-+
: : | | : : : : : : :
+-+-|-|-+-+-+-+-+-+-+
    s e
    c  

"

(defun te-change-selection (extend new-point te)
  "
DO:     Compute the new selection points from the current ones and the
        extend and new-point parameters.
"
  (let ((start   (te-sel-start te))
        (end     (te-sel-end   te))
        (current (te-sel-current te)))
    (multiple-value-bind (new-start new-end new-current)
        (if extend
            (cond
              ((< new-point current)
               (if (= current start)
                   (values new-point end       new-point)
                   (values start     new-point new-point)))
              ((> new-point current)
               (if (= current end)
                   (values start     new-point new-point)
                   (values new-point end       new-point)))
              (t
               (values start end current)))
            (values new-point new-point new-point))
      (format-trace 'te-change-selection :extend extend :new-point new-point
                                         :start new-start :end new-end :current new-current)
      (te-set-select new-start new-end te)
      (setf (te-sel-current te) new-current)))
  te)


(defun te-beginning-of-line-command (mods code char te)
  (declare (ignore code char))
  (multiple-value-bind (colno lino) (te-column (te-sel-current te) te)
    (declare (ignore lino))
    (let ((extend (plusp (logand mods shift-key)))
          (new-point (- (te-sel-current te) colno)))
      (te-change-selection extend new-point te)))
  (values))


(defun te-end-of-line-command (mods code char te)
  (declare (ignore code char))
  (multiple-value-bind (colno lino) (te-column (te-sel-current te) te)
    (declare (ignore colno))
    (let ((extend (plusp (logand mods shift-key)))
          (new-point (if (= lino (1- (te-nlines te)))
                         (te-length te)
                         (1- (aref (te-line-starts te) (1+ lino))))))
      (te-change-selection extend new-point te)))
  (values))


(defun te-backward-char-command (mods code char te)
  (declare (ignore code char))
  (let ((extend (plusp (logand mods shift-key)))
        (new-point (max 0 (1- (te-sel-current te)))))
    (te-change-selection extend new-point te))
  (values))


(defun te-forward-char-command (mods code char te)
  (declare (ignore code char))
  (let ((extend (plusp (logand mods shift-key)))
        (new-point (min (1+ (te-sel-current te)) (te-length te))))
    (te-change-selection extend new-point te))
  (values))


(defun te-previous-line-command (mods code char te)
  (declare (ignore code char))
  (multiple-value-bind (colno lino) (te-column (te-sel-current te) te)
    (let ((extend (plusp (logand mods shift-key))))
      (if (zerop lino)
          (te-change-selection extend 0 te)
          (let* ((new-line-start  (aref (te-line-starts te) (1- lino)))
                 (new-line-length (- (aref (te-line-starts te) lino)
                                     new-line-start))
                 (new-point       (+ (min colno (1- new-line-length))
                                     new-line-start)))
            (te-change-selection extend new-point te)))))
  (values))


(defun te-next-line-command (mods code char te)
  (declare (ignore code char))
  (multiple-value-bind (colno lino) (te-column (te-sel-current te) te)
    (let ((extend (plusp (logand mods shift-key))))
      (if (= (1+ lino) (te-nlines te))
          (te-change-selection extend (te-length te) te)
          (let* ((new-line-start  (aref (te-line-starts te) (1+ lino)))
                 (new-line-length (- (if (< (+ 2 lino) (te-nlines te))
                                         (aref (te-line-starts te) (+ 2 lino))
                                         (te-length te))
                                     new-line-start))
                 (new-point       (+ (min colno (1- new-line-length))
                                     new-line-start)))
            (te-change-selection extend new-point te)))))
  (values))


;; special:

(defun te-set-mark-command (mods code char te)
  (declare (ignore mods code char))
  (te-set-select (te-sel-current te) (te-sel-current te) te)
  (values))


(defun te-enter-command (mods code char te)
  (declare (ignore mods code char te))
  (values))


(defun te-beep-command (mods code char te)
  (declare (ignore mods code char te))
  (ed-beep 1)
  (values))


(defun te-recenter-top-bottom-command (mods code char te)
  (declare (ignore mods code char))
  (when (te-in-port te)
    (with-font-focused-view (te-in-port te)
      (let ((rect (te-view-rect te)))
        (erase-rect* (rect-left rect) (rect-top rect) (rect-width rect) (rect-height rect))
        (te-update rect te))))
  (values))


(defun te-scroll-up-command (mods code char te)
  (declare (ignore mods code char te))
  ;; TODO
  (values))


;;;--------------------------------------------------------------------
;;; Bindings:
;;;--------------------------------------------------------------------

(defun te-bind (chord command te)
  (unless (te-bindings te)
    (setf (te-bindings te) (make-hash-table :test (function equal))))
  (let ((bindings  (te-bindings te)))
    (setf (gethash chord bindings) command)))


(defun te-get-binding (chord te)
  (when (te-bindings te)
    (or (gethash chord    (te-bindings te))
        (gethash :default (te-bindings te)))))


(defun te-set-default-bindings (te)
  (te-bind :default 'te-self-insert-command te)
  (te-bind     0 'te-set-mark-command te)
  (te-bind     1 'te-beginning-of-line-command te)
  (te-bind     2 'te-backward-char-command te)
  (te-bind     3 'te-enter-command te)
  (te-bind     4 'te-delete-char-command te)
  (te-bind     5 'te-end-of-line-command te)
  (te-bind     6 'te-forward-char-command te)
  (te-bind     7 'te-beep-command te)
  (te-bind     8 'te-beep-command te)
  (te-bind     9 'te-beep-command te)
  (te-bind    10 'te-newline-command te)
  (te-bind    11 'te-kill-line-command te)
  (te-bind    12 'te-recenter-top-bottom-command te)
  (te-bind    13 'te-newline-command te)
  (te-bind    14 'te-next-line-command te)
  (te-bind    15 'te-open-line-command te)
  (te-bind    16 'te-previous-line-command te)
  (te-bind    17 'te-beep-command te)
  (te-bind    18 'te-beep-command te)
  (te-bind    19 'te-beep-command te)
  (te-bind    20 'te-transpose-chars-command te)
  (te-bind    21 'te-beep-command te)
  (te-bind    22 'te-scroll-up-command te)
  (te-bind    23 'te-kill-region-command te)
  (te-bind    24 'te-beep-command te)
  (te-bind    25 'te-yank-command te)
  (te-bind    26 'te-beep-command te)
  (te-bind    27 'te-beep-command te)
  (te-bind    28 'te-beep-command te)
  (te-bind    29 'te-beep-command te)
  (te-bind    30 'te-beep-command te)
  (te-bind    31 'te-undo-command te)
  (te-bind   127 'te-delete-backward-char-command te)
  (te-bind 63232 'te-previous-line-command te)
  (te-bind 63233 'te-next-line-command te)
  (te-bind 63234 'te-backward-char-command te)
  (te-bind 63235 'te-forward-char-command te))

#-(and)
(loop with bindings = (te-bindings (test-window-te (front-window)))
      for k being the hash-key of bindings
      for v = (gethash k bindings)
      do (format t "~10A -> ~A~%"
                 (typecase k
                   (integer (if (< k 32)
                                (format nil "C-~A" (code-char (+ k (char-code #\@))))
                                (char-name (code-char k))))
                   (t k))
                 v))




;;;---------------------------------------------------------------------
;;; Tests
;;;---------------------------------------------------------------------

(defclass te-test-window (window)
  ((te :initform nil :accessor test-window-te)))

(defmethod window-size-parts ((window te-test-window))
  (let ((bounds (view-bounds window))
        (te (test-window-te window)))
    (when te
      (te-set-rects bounds bounds te)
      (view-draw-contents window))))

(defmethod window-null-event-handler ((window te-test-window))
  (let ((te (test-window-te window)))
    (when te
      (te-idle te))))

(defmethod view-activate-event-handler ((window te-test-window))
  (let ((te (test-window-te window)))
    (when te
      (te-activate te))))

(defmethod view-deactivate-event-handler ((window te-test-window))
  (let ((te (test-window-te window)))
    (when te
      (te-deactivate te))))


(defmethod view-draw-contents ((window te-test-window))
  (let ((te (test-window-te window)))
    (when te
      (te-update (te-view-rect te) te))))


(defmethod view-key-event-handler ((window te-test-window) char)
  (reporting-errors
   (let ((te (test-window-te window)))
     (when te
       (te-key char te)))))

(defmethod view-click-event-handler ((window te-test-window) where)
  (reporting-errors
    (let ((te (test-window-te window)))
      (when te
        (te-click where
                  (when *current-event*
                    (event-modifierp *current-event* shift-key))
                  te)))))


;; TODO: call test/paragraph on terec in different states.
(defun test/paragraph (te)
  (doparagraphs (i para te)
    (assert (equal para (te-paragraph i te))))
  :success)

(defun test/te-set-text ()
  (let ((window (make-instance 'te-test-window
                               :window-title "test/te-set-text"
                               :view-size #@(200 400)
                               :view-font '("Times" 18 :plain :srcor))))
    (unwind-protect
         (let ((te (te-new (ui:make-rect 0 0 200 100) (ui:make-rect 0 0 200 100) window))
               (text "Hao Wang, logicien americain.

L'algorithme en  question  a  été  publié  en  1960  dans l'IBM Journal,
article intitule \"Toward  Mechanical Mathematics\", avec des variantes et
une  extension au calcul  des  prédicats.  Il  s'agit  ici  du  \"premier
programme\" de Wang, systeme \"P\".

L'article a été écrit en 1958, et les expériences effectuées sur IBM 704
-- machine à lampes, 32 k  mots  de 36 bits, celle-là même qui vit naître
LISP à la même époque. Le programme  a  été écrit en assembleur (Fortran
existait, mais il ne s'était pas encore imposé)  et  l'auteur estime que
\"there is very little in the program that is not straightforward\".

Il observe que les preuves engendrées sont \"essentiellement des arbres\",
et  annonce  que  la  machine  a  démontre 220 théorèmes du  calcul  des
propositions  (tautologies)  en  3  minutes. Il en tire argument pour la
supériorité  d'une  approche  algorithmique  par  rapport à une approche
heuristique comme celle du \"Logic Theorist\" de Newell, Shaw et  Simon (à
partir de 1956 sur la machine JOHNNIAC de la Rand Corporation): un débat
qui dure encore...

Cet  algorithme  a  été popularisé par J. McCarthy, comme exemple-fanion
d'application  de LISP. Il figure dans le manuel de la première  version
de  LISP  (LISP  1,  sur IBM 704 justement, le manuel est daté  de  Mars
1960), et il a été repris dans le celebre \"LISP 1.5 Programmer's Manual\"
publié en 1962 par MIT Press, un des maîtres-livres de l'Informatique.


"))
           (setf (test-window-te window) te)
           ;; ---
           (te-set-text text te)
           (assert (string= text (te-get-text te)))
           (test/paragraph te)
           :success)
      ;; (window-close window)
      )))


(Defun test/te-wrap-paragraph ()
  (let* ((window (make-instance 'te-test-window
                                :window-title "test/te-wrap-paragraph"
                                :view-size #@(200 400)
                                :view-font '("Times" 18 :plain :srcor)))
         (text   (format nil "~
Hao Wang, logicien americain. 

L'algorithme en question a été publié en 1960 dans l'IBM Journal, ~
article intitule \"Toward Mechanical Mathematics\", avec des variantes et ~
une extension au calcul des prédicats. Il s'agit ici du \"premier ~
programme\" de Wang, systeme \"P\". 

L'article a été écrit en 1958, et les expériences effectuées sur IBM 704 ~
-- machine à lampes, 32 k mots de 36 bits, celle-là même qui vit naître ~
LISP à la même époque. Le programme a été écrit en assembleur (Fortran ~
existait, mais il ne s'était pas encore imposé) et l'auteur estime que ~
\"there is very little in the program that is not straightforward\". 

Il observe que les preuves engendrées sont \"essentiellement des arbres\", ~
et annonce que la machine a démontré 220 théorèmes du calcul des ~
propositions (tautologies) en 3 minutes. Il en tire argument pour la ~
supériorité d'une approche algorithmique par rapport à une approche ~
heuristique comme celle du \"Logic Theorist\" de Newell, Shaw et Simon (à ~
partir de 1956 sur la machine JOHNNIAC de la Rand Corporation): un débat ~
qui dure encore... 

Cet algorithme a été popularisé par J. McCarthy, comme exemple-fanion ~
d'application de LISP. Il figure dans le manuel de la première version ~
de LISP (LISP 1, sur IBM 704 justement, le manuel est daté de Mars ~
1960), et il a été repris dans le celebre \"LISP 1.5 Programmer's Manual\" ~
publié en 1962 par MIT Press, un des maîtres-livres de l'Informatique. 

"))
         (te     (te-new (make-rect 0 0 200 400) (make-rect 0 0 200 400) window)))
    (setf (test-window-te window) te)
    ;; ---
    (setf (te-cr-only te) te-word-wrap)
    (te-set-text text te)
    (te-set-just te-just-left te)
    (with-focused-view (te-in-port te)
      (let ((rect (te-view-rect te)))
        (erase-rect* (rect-left rect) (rect-top rect) (rect-width rect) (rect-height rect))))
    (view-draw-contents window)
    (test/paragraph te)))


(defun test/te-selection ()
  (let* ((text "first1 first2 first3 first4 first5
secon1 secon2 secon3 secon4 secon5
third1 third2 third3 third4 third5
fourt1 fourt2 foutr3 fourt4 fourt5
fifth1 fifth2 fifth3 fifth4 fifth5
")
         (rect   (make-rect 0 0 200 100))
         (te     (te-new rect rect nil))
         (third3 (search "third3" text)))

    (te-set-text text te)
    
    (macrolet ((check (expression s e c)
                 (let ((vs (gensym))
                       (ve (gensym))
                       (vc (gensym)))
                   `(let ((,vs ,s)
                          (,ve ,e)
                          (,vc ,c))
                      ,expression
                      (assert (= ,vs (te-sel-start   te)) () "after ~S, ~A should be ~A instead of ~A" ',expression 'start   ,vs (te-sel-start te))
                      (assert (= ,ve (te-sel-end     te)) () "after ~S, ~A should be ~A instead of ~A" ',expression 'end     ,ve (te-sel-end te)) 
                      (assert (= ,vc (te-sel-current te)) () "after ~S, ~A should be ~A instead of ~A" ',expression 'current ,vc (te-sel-current te))))))

      
      (check (te-set-select 0 6 te)
             0 6 6)

      ;; no extend:
      (check (te-set-select third3 third3 te)
             third3 third3 third3)

      (check (te-change-selection nil (+ third3 6) te)
             (+ third3 6) (+ third3 6) (+ third3 6))

      (check (te-change-selection nil (+ third3 36) te)
             (+ third3 36) (+ third3 36) (+ third3 36))

      (check (te-change-selection nil third3 te)
             third3 third3 third3)

      (check (te-change-selection nil (- third3 6) te)
             (- third3 6) (- third3 6) (- third3 6))

      ;; extend after:
      (check (te-set-select third3 third3 te)
             third3  third3  third3)

      (check (te-change-selection t (+ third3 6) te)
             third3 (+ third3 6) (+ third3 6))

      (check (te-change-selection t (+ third3 9) te)
             third3 (+ third3 9) (+ third3 9))

      (check (te-change-selection t (+ third3 3) te)
             third3 (+ third3 3) (+ third3 3))

      (check (te-change-selection t (+ third3 6) te)
             third3 (+ third3 6) (+ third3 6))

      ;; extend before:
      (check (te-change-selection t (- third3 6) te)
             (- third3 6) third3 (- third3 6))

      (check (te-change-selection t (- third3 9) te)
             (- third3 9) third3 (- third3 9))

      (check (te-change-selection t (- third3 3) te)
             (- third3 3) third3 (- third3 3))

      ;; expand after:

      (check (te-change-selection t (+ third3 6) te)
             third3 (+ third3 6) (+ third3 6))

      ;; expand back:
      
      (check (te-change-selection t third3 te)
             third3 third3 third3))
    :success))




(defun test/te-all ()
  (te-init)
  (test/te-set-text)
  (test/te-wrap-paragraph)
  (test/te-selection))




(defun test/te-split ()
  (let* ((te (test-window-te (front-window)))
         (charpos 10)
         ;; (charpos (te-sel-current te))
         )
    (te-invariant te)
    (te-split-paragraph-at charpos te)
    (te-invariant te)
    (assert (aref (te-paragraphs te) (te-current-paragraph-index te)))
    (assert (or (= (te-current-paragraph-index te) (te-next-paragraph-index te))
                (null (aref (te-paragraphs te) (1+ (te-current-paragraph-index te))))))
    (assert (or (= (length (te-paragraphs te)) (te-next-paragraph-index te))
                (aref (te-paragraphs te) (te-next-paragraph-index te))))
    (assert (or (= (te-current-paragraph-index te) (te-next-paragraph-index te))
                (null (aref (te-paragraphs te) (1- (te-next-paragraph-index te))))))
    (values
     (te-paragraph-at charpos te)
     (te-current-paragraph-index te)
     (te-next-paragraph-index    te)
     (te-paragraphs te))))



#-(and) (progn
          (defparameter *text* (format nil "~
Hao Wang, logicien americain. 

L'algorithme en question a été publié en 1960 dans l'IBM Journal, ~
article intitule \"Toward Mechanical Mathematics\", avec des variantes et ~
une extension au calcul des prédicats. Il s'agit ici du \"premier ~
programme\" de Wang, systeme \"P\". 

L'article a été écrit en 1958, et les expériences effectuées sur IBM 704 ~
-- machine à lampes, 32 k mots de 36 bits, celle-là même qui vit naître ~
LISP à la même époque. Le programme a été écrit en assembleur (Fortran ~
existait, mais il ne s'était pas encore imposé) et l'auteur estime que ~
\"there is very little in the program that is not straightforward\". 

Il observe que les preuves engendrées sont \"essentiellement des arbres\", ~
et annonce que la machine a démontré 220 théorèmes du calcul des ~
propositions (tautologies) en 3 minutes. Il en tire argument pour la ~
supériorité d'une approche algorithmique par rapport à une approche ~
heuristique comme celle du \"Logic Theorist\" de Newell, Shaw et Simon (à ~
partir de 1956 sur la machine JOHNNIAC de la Rand Corporation): un débat ~
qui dure encore... 

Cet algorithme a été popularisé par J. McCarthy, comme exemple-fanion ~
d'application de LISP. Il figure dans le manuel de la première version ~
de LISP (LISP 1, sur IBM 704 justement, le manuel est daté de Mars ~
1960), et il a été repris dans le celebre \"LISP 1.5 Programmer's Manual\" ~
publié en 1962 par MIT Press, un des maîtres-livres de l'Informatique. 

"))

          (te-set-text *text* (test-window-te (front-window)))
          (setf (te-click-stuff (test-window-te (front-window))) nil)
          
          (te-init)
          (test/te-wrap-paragraph)

          (test/te-all)
          (test/te-set-text)

          (test/te-split)
          (test/te-selection)
          
          (te-set-select start end te)
          (te-change-selection extend new-point te)

          (inspect (test-window-te (front-window)))

          (te-cal-text (test-window-te (front-window)))
          
          (dotimes (i (te-nlines (test-window-te (front-window))))
            (write-line (te-line i (test-window-te (front-window)))))

          (rect-to-list (te-compute-caret-rect (test-window-te (front-window))))
          (:topleft (0 -1) :size (200 2))

          (map nil (lambda (w) (ignore-errors (te-cal-text (test-window-te w))))
            (remove 'te-test-window (windows) :key (lambda (x) (class-name (class-of x))) :test-not (function eql)))

          (let ((te (test-window-te (front-window))))
            (list (te-paragraph-at 0 te)
                  (te-paragraph-at 300 te)
                  (te-paragraph-at (te-length te) te)))

          (let ((te (test-window-te (front-window))))
            (setf (te-length te) 1475))

          (let ((te (test-window-te (front-window))))
            (values (te-length te)
                    (TE-line-starts te)))



          
          (let ((te (test-window-te (front-window))))
            (te-line 25 te))
          
          (let ((te (test-window-te (front-window))))
            (te-invariant te))
          
          (let ((te (test-window-te (front-window))))
            (te-paragraphs te))
          
          (let ((te (test-window-te (front-window))))
            (test/paragraph te))

          (let ((te (test-window-te (front-window))))
            (doparagraphs (i para te)
              (write-line (cdr para))))

          (let ((te (test-window-te (front-window))))
            (dotimes (i (te-nparagraphs te))
              (write-line (cdr (te-paragraph i te)))))

          (let ((te (test-window-te (front-window))))
            (dotimes (i (te-nparagraphs te))
              (write-line (cdr (te-paragraph i te)))))
          
          (let ((te (test-window-te (front-window))))
            (setf (te-high-hook te) (function te-default-high-hook)))

          (let ((te (test-window-te (front-window))))
            (te-set-default-bindings te))
          
          (let ((te (test-window-te (front-window))))
            (setf (te-just te) te-just-center)
            (te-cal-text te)
            (view-draw-contents  (front-window)))

          (let ((te (test-window-te (front-window))))
            (setf (te-just te) te-just-right)
            (te-cal-text te)
            (view-draw-contents  (front-window)))
          
          (te-set-select 0 10 (test-window-te (front-window)))
          (te-updates (test-window-te (front-window)))
          (view-draw-contents (test-window-te (front-window)))
          (let ((te    (test-window-te (front-window))))
            (te-cal-text te)
            (let ((rect  (te-compute-caret-rect te)))
              (list (rect-left rect)
                    (rect-top rect)
                    (rect-right rect)
                    (rect-bottom rect))))
          

          (let* ((te  (test-window-te (front-window)))
                 (caret-position (te-sel-start te))
                 (lino (te-line-at caret-position te)))
            ;; (te-line-coordinates lino te)
            (rect-to-list (te-compute-caret-rect te)))
          (:topleft (-1 0) :size (2 200))
          0
          608
          (rect-to-list #S(rect :topleft 594 :bottomright 13107812)) (:topleft (594 0) :size (18 200))
          ""
          
          (te-line-coordinates lino te)

           (defstruct e (v (make-array 2000 :element-type 'integer :initial-element 0)))
          (time (loop with v = (e-v (make-e)) 
                      for i below 2000
                      do (incf (aref v i))))

          (time (loop
                  for para in (let ((text (split-sequence #\Newline *text*)))
                             (loop repeat 2000
                                   with i = text
                                   collect (pop i)
                                   unless i
                                     do (setf i text)))
                  do (multiple-value-bind (ff ms) (font-codes '("Times" 18))
                         (font-codes-string-width para ff ms))))


          )


