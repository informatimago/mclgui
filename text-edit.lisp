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

(defconstant +te-just-left+   0)
(defconstant +te-just-right+ -1)
(defconstant +te-just-center+ 1)

(defconstant +te-no-wrap+    -1)
(defconstant +te-word-wrap+   1)

(defvar *selection-color-active*     *light-blue-color* "Selection color when active.")
(defvar *selection-color-inactive*   *light-gray-color* "Selection color when inactive.")


(defstruct (te-paragraph (:conc-name tep-))
  startpos ; the current charpos of the first character of the paragraph.
  text ; the text of the paragraph, without the newline.
  line-offsets) ; a vector of offsets (aref line-offsets 0) = 0

(defun tep-length (tep)
  (length (tep-text tep)))


(defstruct (terec (:conc-name te-))
  (%dest-rect  (make-rect 0 0 20 20) :type rect)
  (%view-rect  (make-rect 0 0 20 20) :type rect)
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
  ;; (text        nil)
  (recal-back  0   :type integer)
  (recal-lines 0   :type integer)
  (click-stuff nil)
  (cr-only     -1  :type integer) ; minusp => cr-only; plusp => word-wrap
  (rewrap      nil :type boolean) ; whether lines should be rewrapped.
  (tx-font     0   :type integer)
  (tx-face     0   :type integer)
  (tx-mode     0   :type integer)
  (tx-size     0   :type integer)
  (in-port     nil :type (or null window))
  (high-hook   nil :type (or null te-selection-function))
  (caret-hook  nil :type (or null te-caret-function))
  (sel-current 0   :type integer) ; current charpos (used when extending selection with shift-).
  ;; Note: lines are justified lines; there may be several lines for a given paragraph.
  (%nlines      0   :type integer)
  %line-starts ; adjustable vector.  
  ;; We use the a gap buffer of paragraphs, and a string gap buffer for the current paragraph.
  paragraphs ; an adjustable array of te-paragraph; newlines count one position but are not stored.
  (nparagraphs             0 :type integer) ; number of paragraphs 
  (current-paragraph-index 0 :type integer) 
  (next-paragraph-index    0 :type integer)
  ;; current-paragraph-{before,after}-point are adjustable strings with fill-pointers,
  ;; containing the current paragraph split into before and after parts.
  ;; the characters in current-paragraph-after-point are stored in reverse order.
  current-paragraph-before-point
  current-paragraph-after-point
  (current-paragraph-dirty    nil :type boolean) ; whether (aref (te-paragraphs te) (te-current-paragraph-index te)) needs to be recomputed from current-paragraph-before-point and current-paragraph-after-point
  (line-starts-dirty          nil :type boolean) ; whether line-starts need to be recomputed.
  bindings ; the key bindings hash-table
  (updates                    '() :type list)
  (line-coordinates-cache     nil)
  (selection-rectangles-cache nil)
  (display-state              nil))

(defun te-dest-rect (te) (make-rect (te-%dest-rect te)))
(defun te-view-rect (te) (make-rect (te-%view-rect te)))
(defun (setf te-dest-rect) (new-rect te) (assign-rect (te-%dest-rect te) new-rect))
(defun (setf te-view-rect) (new-rect te) (assign-rect (te-%view-rect te) new-rect))

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
                             (nsubseq line1 0 (min 32 len1))
                             (if (< (te-length te) 32) "" "…"))
                (if (= (te-sel-start te) (te-sel-end te))
                    (te-sel-start te)
                    (list (te-sel-start te) (te-sel-end te))))
        te)))


(defun te-clear-caches (te)
  (setf (te-line-coordinates-cache te) nil
        (te-selection-rectangles-cache te) nil))


(defun te-invariant (te)
  (assert (or (te-line-starts-dirty te)
              (= (te-%nlines te) (length (te-%line-starts te))))
          () "line-starts must be dirty (~S) or nlines (~D) must cache its length (~D)."
          (te-line-starts-dirty te) (te-%nlines te) (length (te-%line-starts te)))
  (assert (or (= (te-sel-current te) (te-sel-start te))
              (= (te-sel-current te) (te-sel-end   te)))
          () "sel-current (~D) must be either sel-start (~D) or sel-end (~D)."
          (te-sel-current te) (te-sel-start te) (te-sel-end   te))
  (assert (<= 1 (te-nparagraphs te) (length (te-paragraphs te)))
          () "There must be at least one paragraph and nparagraphs = ~D must be less or equal to (length paragraphs) = ~D)."
          (te-nparagraphs te) (length (te-paragraphs te))) 
  (assert (< -1 (te-current-paragraph-index te) (te-nparagraphs te))
          () "Current paragraph (~D) must be within nparagraphs (~D)."
          (te-current-paragraph-index te) (te-nparagraphs te))
  (assert (< (te-current-paragraph-index te) (te-next-paragraph-index te))
          () "Next paragraph (~D) must be greater than current paragraph (~D)."
          (te-next-paragraph-index te) (te-current-paragraph-index te))
  (assert (<= (te-next-paragraph-index te) (length (te-paragraphs te)))
          () "Next paragraph (~D) must be less or equal to (length paragraphs) = ~D."
          (te-next-paragraph-index te) (length (te-paragraphs te)))
  (loop :for i :from 0 :to (te-current-paragraph-index te)
        :do (assert (not (null (aref (te-paragraphs te) i)))
                    () "Paragraphs up to current (~D) must be packed first; i = ~D -> (aref paragraph i) must not be NIL."
                    (te-current-paragraph-index te) i))
  (loop :for i :from (1+ (te-current-paragraph-index te))
          :below (te-next-paragraph-index te)
        :do (assert (null (aref (te-paragraphs te) i))
                    () "(aref paragraphs ~D) between current (~D<) and next (<~D) must be NIL, not ~S"
                    i (te-current-paragraph-index te)  (te-next-paragraph-index te)  (aref (te-paragraphs te) i)))
  (loop :for i :from (te-next-paragraph-index te) :below (length (te-paragraphs te))
        :do (assert (not (null (aref (te-paragraphs te) i)))
                    () "Paragraphs from next (~D<=) to end of paragraphs (<~D) must be packed; i = ~D -> (aref paragraph i) must not be NIL."
                    (te-next-paragraph-index te)  (length (te-paragraphs te)) i)))


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



(defmacro doparagraphs ((index var te &optional result) &body body)
  "
DO:     Evaluates the BODY with the variables given in INDEX bound to
        the paragraph index, in (te-paragraphs te), and VAR bound to
        the paragraph structure..
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
        :do (incf (aref (te-%line-starts te) lino) increment))
  (incf (te-length te) increment))


(defun te-rewrap-paragraphs (te)
  "Recompute the line-offsets of each paragraph."
  (doparagraphs (i para te)
    (declare (ignore i))
    (setf (tep-line-offsets para) (if (te-word-wrap-p te)
                                      (te-wrap-paragraph-text (tep-text para) te)
                                      #(0)))))


(defun te-update-line-starts (te)
  "Recompute line-starts from paragraphs."
  (let ((starts (make-array (ceiling (* 1.5 (reduce (function +)
                                                    (te-paragraphs te)
                                                    :key (lambda (para)
                                                           (if para
                                                               (length (tep-line-offsets para))
                                                               0)))))
                            :adjustable t
                            :fill-pointer 0
                            :initial-element 0))
        (start 0))
    (doparagraphs (i para te)
      (declare (ignore i))
      (setf (tep-startpos para) start)
      (dovector (offset (tep-line-offsets para))
        (vector-push (+ start offset) starts))
      (incf start (1+ (tep-length para))))
    (setf (te-%line-starts te) starts
          (te-%nlines te) (length starts)
          (te-line-starts-dirty te) nil
          (te-length te) (decf start))
    (setf (te-sel-start   te) (min start (te-sel-start   te))
          (te-sel-end     te) (min start (te-sel-end     te))
          (te-sel-current te) (min start (te-sel-current te)))))


(defun te-nlines (te)
  (when (te-line-starts-dirty te)
    (te-update-line-starts te))
  (te-%nlines te))


(defun te-line-starts (te)
  (when (te-line-starts-dirty te)
    (te-update-line-starts te))
  (te-%line-starts te))

(defun te-line-start (lino te)
  (aref (te-line-starts te) lino))

(defun te-line-end (lino te)
  (if (< (1+ lino) (te-nlines te))
      (1- (aref (te-line-starts te) (1+ lino))) 
      (te-length te)))

(defun te-line-length (lino te)
  (-  (if (< (1+ lino) (te-nlines te))
          (te-line-start (1+ lino) te)
          (te-length te))
      (te-line-start lino te)))

;;;---------------------------------------------------------------------
;;; current-paragraph editing
;;;---------------------------------------------------------------------


;;; current-paragraph-{before,after}-point ---> paragraphs

(defun te-word-wrap-p (te)
  (plusp (te-cr-only te)))

(defun te-make-paragraph (startpos text te)
  (make-te-paragraph :startpos startpos
                     :text text
                     :line-offsets (if (te-word-wrap-p te)
                                       (te-wrap-paragraph-text text te)
                                       #(0))))

(defun te-startpos-of-paragraph (parno te)
  (tep-startpos (aref (te-paragraphs te) parno)))

(defun te-current-paragraph (te)
  "
RETURN: The cons cell containing the start and the string of the current paragraph.
NOTE:   This string is computed from current-paragraph-before-point
        and current-paragraph-after-point and cached.
"
  (let ((parno (te-current-paragraph-index te)))
    (if (te-current-paragraph-dirty te)
        (let ((text #-(and) (concatenate 'string (te-current-paragraph-before-point te)
                                         (reverse (te-current-paragraph-after-point te)))
                    ;; Let's assume this will be faster; at least, it should cons less.
                    (let* ((before     (te-current-paragraph-before-point te))
                           (after      (te-current-paragraph-after-point  te))
                           (len-before (length before))
                           (len-after  (length after))
                           (para       (make-string (+ len-before len-after))))
                      (replace para before)
                      (loop :for src :from (1- len-after) :downto 0
                            :for dst :from len-before
                            :do (setf (aref para dst) (aref after src)))
                      para)))
          (setf (te-current-paragraph-dirty te) nil
                (aref (te-paragraphs te) parno) (te-make-paragraph (te-startpos-of-paragraph parno te) text te)))
        (aref (te-paragraphs te) parno))))




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
      (dichotomy (lambda (parno) (real-compare charpos (tep-startpos (te-paragraph parno te))))
                 0 (te-nparagraphs te))
    (declare (ignore found order))
    parno))



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
          (te-next-paragraph-index    te) next)
    (te-invariant te)))


(declaim (inline te-gap-size))
(defun te-gap-size (te)
  (- (te-next-paragraph-index te) (te-current-paragraph-index te)))


(defun te-adjust-gap (increment te)
  "
DO:     Increases or decreases the gap between current-paragraph-index
        and next-paragraph-index by INCREMENT.
"
  (te-invariant te)
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
            (te-paragraphs              te) paras)
      (te-invariant te))))


;;;---------------------------------------------------------------------
;;; Split the current paragraph
;;;---------------------------------------------------------------------

;;; (current-paragraph) --> (current-paragraph-{before,after}-point)
(defun %te-split-paragraph (parno te)
  (let* ((paragraphs (te-paragraphs te))
         (para       (aref paragraphs parno))
         (para-text  (tep-text para))
         (buf-length (ceiling (* 1.5 (length para-text)))))
    (setf (te-current-paragraph-before-point te) (adjustable-string buf-length para-text)
          (te-current-paragraph-after-point  te) (adjustable-string buf-length "")
          (te-current-paragraph-dirty        te) nil)))


;;; move the gap to the paragraph containing charpos and split it to charpos.
(defun te-split-paragraph-at (charpos te)
  "
POST:   (= (te-current-paragraph-index te) (te-paragraph-at charpos te))
RETURN: parno of the current paragraph.
"
  (let ((parno (te-paragraph-at charpos te)))
    (if (= parno (te-current-paragraph-index te))
        ;; resplit current paragraph
        (let* ((line-start (te-startpos-of-paragraph parno te))
               (before     (te-current-paragraph-before-point te))
               (after      (te-current-paragraph-after-point  te))
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
                  (te-current-paragraph-before-point te) before
                  (te-current-paragraph-after-point  te) after)))
        (progn
          (te-current-paragraph te) ; unsplit current paragraph.
          ;; split another paragraph:
          (%te-move-gap-to parno te)
          (%te-split-paragraph parno te)
          (te-split-paragraph-at charpos te))))
  (assert (= (te-current-paragraph-index te) (te-paragraph-at charpos te))
          () "~S should set the current paragraph (~D) to the paragraph at ~D -> ~D"
          (te-current-paragraph-index te) charpos  (te-paragraph-at charpos te))
  (te-invariant te))


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


(defun te-wrap-paragraph-text (text te)
  "
DO:     Perform word wrapping on the given paragraph text.
TEXT:   A string containing the paragraph text.
TE:     A TeRec structure.
RETURN: A sorted vector of line-offsets for the paragraph.
"
  (let* ((starts     (list 0))
         (width      (rect-width (te-%dest-rect te)))
         (word-break (or (te-word-break te) (function te-default-word-break)))
         (ff         (te-ff te))
         (ms         (te-ms te))
         (start      0)
         (end        (length text)))
    (loop
      (let ((text-width (font-codes-string-width text ff ms start end)))
        (when (<= text-width width)
          (return-from te-wrap-paragraph-text (coerce (nreverse starts) 'vector)))
        (multiple-value-bind (found index order)
            (dichotomy (lambda (mid)
                         (let ((text-width (font-codes-string-width text ff ms start mid)))
                           (real-compare width text-width)))
                       start end)
          (declare (ignore found order))
          (let ((next-start (loop
                              :with wrap = index
                              :until (or (funcall word-break text wrap) (zerop wrap))
                              :do (decf wrap)
                              :finally (return (if (zerop wrap)
                                                   index
                                                   (1+ wrap))))))
            (push next-start starts)
            (setf start next-start)))))))


;;; global update:
;;; ( ((tx-font tx-face tx-mode tx-size) --> (line-height font-ascent)) cr-only dest-rect te-paragraph ) --> (nlines line-starts length)

(defun te-calculate-text (te)
  "
DO:     Compute nlines and line-starts, as the number and positions of
        displayed lines.  When (te-word-wrap-p te) paragraphs are
        wrapped, so several lines may be computed for a paragraph.
NOTE:   Call TE-CAL-TEXT, which is the one with mutex.
"
  (te-clear-caches te)
  (setf (te-nparagraphs te) (count nil (te-paragraphs te) :test-not (function eql)))
  (when (te-rewrap te)
    (te-rewrap-paragraphs te)
    (setf (te-rewrap te) nil))
  (te-update-line-starts te)
  (te-invariant te))


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
  (let ((start (te-line-start lino te))
        (end   (te-line-end lino te)))
    (let* ((para (te-paragraph (te-paragraph-at start te) te))
           (end  (min (- end (tep-startpos para)) (tep-length para))))
      (nsubseq (tep-text para) (min end (- start (tep-startpos para))) end))))


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
                   (dest-rect   (te-%dest-rect   te))
                   (just        (te-just        te))
                   (top         (+ (rect-top dest-rect) (* lino line-height)))
                   (bottom      (+ top line-height))
                   (base        (+ top font-ascent))
                   (line        (te-line lino te))
                   (x           (if (or (= just +te-just-right+) (= just +te-just-center+))
                                    (let* ((end (loop
                                                  :with end = (length line)
                                                  :while (and (plusp end)
                                                              (te-whitespacep (aref line (1- end))))
                                                  :do (decf end)
                                                  :finally (return end)))
                                           (line-width (te-string-width line te 0 end)))
                                      (if (= just +te-just-right+)
                                          (- (rect-right dest-rect) line-width)
                                          (+ (rect-left dest-rect)
                                             (round (- (rect-width dest-rect) line-width) 2))))
                                    (rect-left dest-rect))))
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
    (values (- charpos (te-line-start lino te))
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
  (let ((dest-rect (te-%dest-rect te)))
    (make-te-display-state
     :dest-rect            dest-rect
     :view-rect            (te-%view-rect te)
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
  ;; (format-trace 'te-redraw-line :lino lino)
  (multiple-value-bind (x base line-rect line) (te-line-coordinates lino te)
    (erase-rect* (rect-left line-rect) (rect-top line-rect) (rect-width line-rect) (rect-height line-rect))
    (draw-string x base line))
  te)


(defun te-draw-line (lino te)
  "
DO:    Draws the line number LINO, according to the current TeRec configuration.
NOTE:  The focus should be already established.
"
  ;; (format-trace 'te-draw-line :lino lino)
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
              ;; :do (format-trace 'te-update-view :-> update)
              :do (if (atom update)
                      (ecase update
                        (:display
                         (loop :for lino :below (te-nlines te)
                               :do (te-draw-line lino te)))
                        (:selection
                         (assert (<= 0 (te-sel-start te) (te-sel-end te) (te-length te)))
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
                           (when (and (/= start end) (<= (max start end) (te-length te)))
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
  (let ((vte         (gensym "te"))
        (vselection  (gensym "sel")))
    `(let* ((,vte ,te)
            (,vselection (cons (te-sel-start ,vte) (te-sel-end ,vte))))
       (with-font-focused-view (te-in-port ,vte)
         (with-clip-rect-intersect (te-%view-rect ,vte)
           (when (and (te-in-port ,vte) (te-has-caret ,vte))
             (te-erase-caret ,vte))
           (multiple-value-prog1 (progn ,@body)
             (unless (and (= (car ,vselection) (te-sel-start ,vte))
                          (= (cdr ,vselection) (te-sel-end   ,vte)))
               (te-show-charpos (te-sel-current ,vte) ,vte))
             (when (te-in-port ,vte)
               (invalidate-corners (te-in-port ,vte)
                                   (rect-topleft (te-%view-rect ,vte))
                                   (rect-bottomright (te-%view-rect ,vte))
                                   #|already erased:|#nil))
             (te-update-view (te-%view-rect ,vte) ,vte)))))))



;;;---------------------------------------------------------------------
;;; Caret
;;;---------------------------------------------------------------------

(defun te-compute-caret-rect (charpos te)
  "
RETURN:  the rectangle where a caret at CHARPOS would be drawn; the lino where the caret is.
"
  (let* ((lino (te-line-at charpos te)))
    (multiple-value-bind (x base line-rect line) (te-line-coordinates lino te)
      (declare (ignore base))
      ;; (format-trace 'te-compute-caret-rect (list (rect-left line-rect) (rect-top line-rect) (rect-right line-rect) (rect-bottom line-rect)))
      (let ((caret-x (+ x (te-string-width line te 0 (- charpos (te-line-start lino te))))))
        (values (make-rect (1- caret-x) (rect-top line-rect) (1+ caret-x) (rect-bottom line-rect))
                lino)))))


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
    (assert (te-has-caret te))
    (multiple-value-bind (caret-rect lino) (te-compute-caret-rect (te-sel-start te) te)
      (with-font-focused-view (te-in-port te)
        (with-clip-rect-intersect (te-%view-rect te)
          (funcall (or (te-caret-hook te) (function te-default-caret-hook)) caret-rect lino te))))))


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
  (values (max (min start end (te-length te)) 0)
          (min (max start end) (te-length te))))


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
                               (let* ((line-start   (te-line-start start-lino te))
                                      (start-column (- start line-start))
                                      (end-column   (- end   line-start))
                                      (left         (+ x (te-string-width line te 0 start-column)))
                                      (right        (+ x (te-string-width line te 0 end-column))))
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
                                 (let* ((start-line-start  (te-line-start start-lino te))
                                        (end-line-start    (te-line-start end-lino   te))
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
                                 (let* ((start-line-start  (te-line-start start-lino te))
                                        (end-line-start    (te-line-start end-lino   te))
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
        displayed lines.  When (te-word-wrap-p te) paragraphs are
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
NOTE:       The rectangles are given in the *current-view* coordinates, and are converted to the window coordinates.
"
  (assert (not (empty-rect-p dest-rect)) (dest-rect) "Destination rectangle must not be empty.")
  (assert (not (empty-rect-p view-rect)) (view-rect) "View rectangle must not be empty.")
  (with-mutex *te-mutex*
    (setf (te-rewrap te) (/= (rect-width dest-rect) (rect-width (te-%dest-rect te))))
    (assign-rect (te-%dest-rect te) (if (and *current-view* (view-window *current-view*))
                                        (convert-rectangle dest-rect *current-view* (view-window *current-view*))
                                        dest-rect))
    (assign-rect (te-%view-rect te) (if (and *current-view* (view-window *current-view*))
                                        (convert-rectangle view-rect *current-view* (view-window *current-view*))
                                        view-rect))
    (when (< (rect-width (te-%dest-rect te)) 20)
      (setf (rect-width (te-%dest-rect te)) 20))
    (setf (rect-bottom (te-%dest-rect te)) 16000)
    (format-trace 'te-set-rects :rewrap  (te-rewrap te)
                                :new-dest-rect (rect-to-list dest-rect)
                                :dest-rect (rect-to-list (te-%dest-rect te))
                                :new-view-rect (rect-to-list view-rect)
                                :view-rect (rect-to-list (te-%view-rect te)))
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
      :do (setf (aref paragraphs i) (te-make-paragraph pos para te)))
    (with-mutex *te-mutex*
      (setf (te-sel-start               te) 0
            (te-sel-end                 te) 0
            (te-sel-current             te) 0
            (te-paragraphs              te) paragraphs
            (te-nparagraphs             te) pcount
            (te-current-paragraph-index te) (1- pcount)
            (te-next-paragraph-index    te) (length paragraphs))
      (%te-move-gap-to (1- pcount) te)
      (%te-split-paragraph (1- pcount) te)
      (te-calculate-text te)
      (%te-set-caret (length text) te))
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
          (write-string (tep-text para)))))))

(defun te-text (te)
  (te-get-text te))


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
  (let ((te (make-terec :%dest-rect (make-rect dest-rect) :%view-rect (make-rect view-rect) :in-port window)))
    (te-set-default-bindings te)
    (setf (te-word-break te) (function te-default-word-break)
          (te-caret-hook te) (function te-default-caret-hook)
          (te-high-hook  te) (function te-default-high-hook))
    (te-set-text "" te)
    (if (te-in-port te)
        (multiple-value-bind (ff ms) (view-font-codes (te-in-port te))
          (multiple-value-bind (ff ms) (font-codes (view-font (te-in-port te)) ff ms)
            (te-set-font-info ff ms te)))
        (te-set-font-info 0 0 te))
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
*CARET-TIME*: no matter how often you call TE-IDLE, the time
between blinks will never be less than the minimum interval.

Note: The initial minimum blink interval setting is 32 ticks. The
user can adjust this by setting *CARET-TIME*.

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
         (lino  (truncate (- v (rect-top (te-%dest-rect te)))
                          (te-line-height te)))
         (start (if (<= (te-nlines te) lino)
                    (te-length te)
                    (te-line-start lino te)))
         (lino  (if (<= (te-nlines te) lino)
                    (1- (te-nlines te))
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
         (line-start (te-line-start lino te))
         (start      (- charpos line-start))
         (end        start)
         (line       (te-line lino te)))
    (cond
      ((zerop (length line))
       (values charpos charpos))
      ((<= (length line) start) ; charpos is on the newline.
       (values charpos charpos))
      (t
       (format-trace 'te-word-at :charpos charpos :lino :lino
                                 :start start :line line)
       (loop
         :until (or (minusp start) (funcall word-break line start))
         :do (decf start)
         :finally (incf start))
       (loop
         :until (or (<= (length line) end) (funcall word-break line end))
         :do (incf end))
       (format-trace 'te-word-at :word (nsubseq line start end))
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
    (te-invariant te)
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


(defun %te-set-select (start end te)
  (assert (<= start end))
  (push (list :old-selection (te-sel-start te) (te-sel-end te)) (te-updates te))
  (setf (te-sel-start   te) start
        (te-sel-end     te) end
        (te-sel-current te) end)
  (push :selection (te-updates te)))


(declaim (inline %te-set-caret))
(defun %te-set-caret (charpos te)
  (%te-set-select charpos charpos te))


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
    (te-invariant te)
    (with-te-update-display te
      (multiple-value-bind (start end) (te-clean-selection start end te)
        (%te-set-select start end te)))
    (te-invariant te)))


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
    C-D           te-delete-forward-char-command
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
      (te-invariant te)
      (with-te-update-display te
        (when (te-has-caret te)
          (te-erase-caret te))
        (funcall (te-get-binding code te) mods code char te))
      (te-invariant te))))


(defun te-cut (te)
  "
DO:     Removes the selection range from the text specified by TE and
        places it in the TextEdit scrap.  The text is redrawn as
        necessary.  Anything previously in the scrap is deleted.  If
        the selection range is an insertion point, the scrap is
        emptied.
"
  (format-trace 'te-cut)
  (with-mutex *te-mutex*
    (te-invariant te)
    (with-te-update-display te
      (%te-copy   (te-sel-start te) (te-sel-end te) te)
      (%te-delete (te-sel-start te) (te-sel-end te) te)
      (%te-set-caret (te-sel-start te) te))
    (te-invariant te)))


(defun te-copy (te)
  "
DO:     Copies the selection range from the text specified by TE into
        the TextEdit scrap.  Anything previously in the scrap is
        deleted.  The selection range is not deleted.  If the
        selection range is an insertion point, the scrap is emptied.
"
  (format-trace 'te-copy)
  (with-mutex *te-mutex*
    (te-invariant te)
    (%te-copy (te-sel-start te) (te-sel-end te) te)))


(defun te-paste (te)
  "
DO:     Replaces the selection range in the text specified by TE with
        the contents of the TextEdit scrap, and leaves an insertion
        point just past the inserted text.  The text is redrawn as
        necessary.  If the scrap is empty, the selection range is
        deleted. If the selection range is an insertion point,
        TE-PASTE just inserts the scrap there.
"
  (format-trace 'te-paste)
  (with-mutex *te-mutex*
    (te-invariant te)
    (with-te-update-display te
      (%te-delete (te-sel-start te) (te-sel-end te) te)
      (%te-set-caret (%te-insert (te-sel-start te) (get-scrap :text) te) te))
    (te-invariant te)))


(defun te-delete (te)
  "
DO:     removes the selection range from the text specified by TE,
        and redraws the text as necessary. TE-DELETE is the same as
        TE-CUT (above) except that it doesn't transfer the selection
        range to the scrap.  If the selection range is an insertion
        point, nothing happens.
"
  (format-trace 'te-delete)
  (with-mutex *te-mutex*
    (te-invariant te)
    (with-te-update-display te
      (%te-delete (te-sel-start te) (te-sel-end te) te))
    (te-invariant te)))


(defun te-insert (text te)
  "
DO:     Takes the specified text and inserts it just before the
        selection range into the text indicated by TE, redrawing the
        text as necessary. The text parameter points to the text to be
        inserted, and the length parameter indicates the number of
        characters to be inserted. TE-INSERT doesn't affect either the
        current selection range or the scrap.
"
  (with-mutex *te-mutex*
    (te-invariant te)
    (with-te-update-display te
      (let ((start   (te-sel-start   te))
            (end     (te-sel-end     te))
            (current (te-sel-current te))
            (tlen    (length text)))
        (setf (te-sel-current te) start)
        (%te-insert start text te)
        (%te-set-select (+ start tlen) (+ end tlen) te)
        (setf (te-sel-current te) (+ current tlen))))
    (te-invariant te)))


(defun TE-Auto-View (auto te)
  ;; TODO: TE-Auto-View
  )


(defun te-set-font (font face mode size te)
  (with-mutex *te-mutex*
    (te-invariant te)
    (setf (te-rewrap te) (or (/= (te-tx-font te) font)
                             (/= (te-tx-face te) face)
                             (/= (te-tx-mode te) mode)
                             (/= (te-tx-size te) size))
          (te-tx-font te) font
          (te-tx-face te) face
          (te-tx-mode te) mode
          (te-tx-size te) size)
    (multiple-value-bind (ascent d w l) (font-codes-info (te-ff te) (te-ms te))
      (declare (ignore w))
      (setf (te-line-height te) (ceiling (+ ascent d l))
            (te-font-ascent te) (round ascent)))
    (te-calculate-text te)))


(defun te-set-font-info (ff ms te)
  (let ((ff (or ff 0))
        (ms (or ms 0)))
    (te-set-font (ldb (byte 16 16) ff)  (ldb (byte 16 0) ff)
                 (ldb (byte 16 16) ms)  (ldb (byte 16 0) ms) te)))


(defun te-wrapping (te)
  (te-cr-only te))


(defun te-set-wrapping (wrapping te)
  "
WRAPPING:  Either TE-NO-WRAP or TE-WORD-WRAP
DO:        Set the wrapping mode
"
  (check-type wrapping (member -1 1))
  (with-mutex *te-mutex*
    (te-invariant te)
    (setf (te-rewrap te) (/= (te-cr-only te) wrapping)
          (te-cr-only te) wrapping)
    (te-calculate-text te)))


(defun te-set-just (just te)
  "
DO:     Sets the justification of the text specified by TE to
        just. TextEdit provides three predefined constants for setting
        justification:

            +TE-JUST-LEFT+
            +TE-JUST-CENTER+
            +TE-JUST-RIGHT+

        By default, text is left-justified. If you change the
        justification, call InvalRect after TE-SET-JUST, so the text
        will be redrawn with the new justification.
"
  (check-type just (member -1 0 1))
  (with-mutex *te-mutex*
    (te-invariant te)
    (setf (te-just te) just)
    (te-calculate-text te)))


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
      (te-invariant te)
      (intersect-rect update-rect (te-%view-rect te) clip-rect)
      (push :display   (te-updates te))
      (push :selection (te-updates te))
      (te-update-view clip-rect te))
    (te-invariant te)))


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
  (draw-string-in-rect text (if (and *current-view* (view-window *current-view*))
                                (convert-rectangle box *current-view* (view-window *current-view*))
                                box)
                       :justification (cond
                                        ((= just +te-just-right+)  :right)
                                        ((= just +te-just-center+) :center)
                                        (t                         :left))
                       :truncation :word-wrapping))


(defun %te-scroll (dh dv te)
  (te-invariant te)
  (te-set-rects (offset-rect (te-dest-rect te) dh dv) (te-%view-rect te) te)
  (let ((view (te-in-port te)))
    (when view
      (dolist (rect (scroll-rect view (te-%view-rect te) dh dv))
        (invalidate-corners view (rect-topleft rect) (rect-bottomright rect) #|already erased:|#nil)))))


(defun te-scroll (dh dv te)
  "
DO:     scrolls the text within the view rectangle of the specified
        edit record by the number of pixels specified in the dh and dv
        parameters.  The edit record is specified by the hTE
        parameter.  Positive dh and dv values move the text right and
        down, respectively, and negative values move the text left and
        up.  For example,

            (te-scroll 0 (- (te-line-height te)) te)

        scrolls the text up one line.  Remember that you scroll text
        up when the user clicks in the scroll arrow pointing down.
        The destination rectangle is offset by the amount you scroll.

NOTE:   To implement automatic scrolling, you store the address of a
        routine in the clikLoop field of the edit record, as described
        above under \"The TERec Data Type\".
"
  (with-mutex *te-mutex*
    (%te-scroll dh dv te)))


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

(defun te-text-in-range (start end te)
  "
RETURN: a string containing the text between start and end charpos from the TEREC.
"
  (assert (<= start end (te-length te)))
  (if (= start end)
      ""
      (multiple-value-bind (rects start-lino end-lino start-column end-column)
          (te-compute-selection-rectangles start end te)
        (ecase (length rects)
          (1
           (nsubseq (te-line start-lino te) start-column end-column))
          (2
           (with-output-to-string (*standard-output*)
             (write-line   (nsubseq (te-line start-lino te) start-column))
             (write-string (nsubseq (te-line end-lino te) 0 end-column))))
          (3
           (with-output-to-string (*standard-output*)
             (write-line   (nsubseq (te-line start-lino te) start-column))
             (loop
               :for lino :from (1+ start-lino) :to (1- end-lino)
               :do (write-line (te-line lino te)))
             (write-string (nsubseq (te-line end-lino te) 0 end-column))))))))


(defun te-selected-text (te)
  "
RETURN: a string containing the selected text from the TEREC.
"
  (te-text-in-range  (te-sel-start te) (te-sel-end te) te))


(defun %te-copy (start end te)
  (if (= start end)
      (zero-scrap)
      (put-scrap :text (te-text-in-range start end te))))


(defun %te-delete (start end te)
  "
DO:     Delete the characters between start and end and set the caret to start.
POST:   (= start (te-sel-start te) (te-sel-end te) (te-sel-current te))
NOTE:   We set the selection because deleting can reduce the size
        below the current selection.
"
  (te-invariant te)
  ;; TODO: add display updating stuff.
  (unless (= start end)
    (let ((start-lino (te-line-at start te))
          (end-lino   (te-line-at end   te)))
      (te-split-paragraph-at end te)
      (if (= start-lino end-lino)
          (decf (fill-pointer (te-current-paragraph-before-point te)) (- end start))
          (let* ((start-parno        (te-paragraph-at start te))
                 (index              (te-current-paragraph-index te))
                 (deleted-para-count (- index start-parno)))
            (loop
              :repeat deleted-para-count
              :do (setf (aref (te-paragraphs te) index) nil)
                  (decf index))
            (assert (= start-parno index))
            (let* ((para         (te-paragraph index te))
                   (start-column (- start (tep-startpos para)))
                   (before       (adjustable-string (* 2 start-column) (nsubseq (tep-text para) 0 start-column))))
              (setf (te-current-paragraph-before-point te) before
                    (te-current-paragraph-index te)        index)
              (decf (te-nparagraphs te) deleted-para-count)))))
    (setf (te-current-paragraph-dirty te) t
          (te-line-starts-dirty       te) t)
    (te-clear-caches te))
  (%te-set-caret start te)
  (te-invariant te))


(defun %te-insert (charpos text te)
  "
DO:     Inserts the TEXT at position CHARPOS. 
NOTE:   Doesn't change the selection range.
RETURN: The position after the last character inserted.
"
  ;; TODO: add display updating stuff.
  (assert (<= 0 charpos (te-length te)))
  (te-invariant te)
  (te-split-paragraph-at charpos te)
  (format-trace '%te-insert 1) (te-invariant te)
  (let* ((paragraphs (split-sequence #\Newline text))
         (nparas     (length paragraphs))
         (plen       (length (first paragraphs))))
    (format-trace '%te-insert :text text :charpos charpos)
    (when (<= (te-gap-size te) nparas)
      (te-adjust-gap nparas te))
    (let* ((after   (te-current-paragraph-after-point   te)) ; keep after the insertion point
           (before  (te-current-paragraph-before-point  te))
           (current (length before))
           (nlen    (+ current plen))
           (npos    (+ nlen (te-startpos-of-paragraph (te-current-paragraph-index te) te))))
      ;; insert first new paragraph into the current paragraph:
      (setf before (adjust-array before (max nlen (array-dimension before 0)) :fill-pointer nlen))
      (replace before (pop paragraphs) :start1 current)
      (setf (te-current-paragraph-dirty te) t)
      (when (< 1 nparas)
        (setf (te-current-paragraph-after-point te) (adjustable-string 10 ""))
        (te-current-paragraph te) ; unsplit current paragraph.
        ;; insert the other new paragraphs into the gap buffer:
        (let* ((index   (te-current-paragraph-index te))
               (parapos (loop
                          :for parapos = (1+ npos) :then (+ parapos plen 1)
                          :for para :in paragraphs
                          :for plen = (length para)
                          :do (setf (aref (te-paragraphs te) (incf index))
                                    (te-make-paragraph parapos (adjustable-string plen para) te))
                              (incf (te-nparagraphs te))
                          :finally (return parapos))))
          (setf (te-current-paragraph-index te) index)
          ;; append the old after part onto the last new paragraph:
          (%te-split-paragraph index te)
          (setf (te-current-paragraph-after-point te) after
                (te-current-paragraph-dirty te) t)
          (setf npos parapos)))
      ;; compute new lines and adjust following.
      (loop :for parno :from (te-current-paragraph-index te) :below (te-nparagraphs te)
            :for para = (te-paragraph parno te)
            :for plen = (tep-length para)
            :for parapos = (te-startpos-of-paragraph parno te) :then (+ parapos plen 1)
            :do (setf (tep-startpos para) parapos)
            :finally (setf (te-length te) (1- parapos)))
      (format-trace '%te-insert 2) (te-invariant te)
      (setf (te-line-starts-dirty te) t)
      (te-calculate-text te) ; TODO: do it more efficiently!
      (format-trace '%te-insert 3) (te-invariant te)
      npos)))


(defun te-self-insert-command (mods code char te)
  (declare (ignore mods code))
  (te-invariant te)
  (when (/= (te-sel-start te) (te-sel-end te))
    ;; TODO: save the selection for undo
    (%te-delete (te-sel-start te) (te-sel-end te) te))
  (let ((npos (%te-insert (te-sel-start te) (string char) te)))
    (setf (te-sel-start   te) npos
          (te-sel-end     te) npos
          (te-sel-current te) npos))
  (te-invariant te)
  (values))

(defun te-end-of-line-p (charpos te)
  (let* ((lino   (te-line-at charpos te))
         (endpos (te-line-end lino te)))
    (= charpos endpos)))

(defun te-transpose-chars-command (mods code char te)
  (declare (ignore mods code char))
  ;; Note: transpose-chars at the end of a line doesn't move forward,
  ;; but in the middle of a line, it moves forward.
  ;;
  ;; If after is empty then just swap the two previous characters (if
  ;; there's only one then the previous char moves up the end of the
  ;; previous line, but at the beginning of buffer, it just beeps).
  ;; if after is not empty then swap the previous and next and advance one char.
  (let* ((pos (te-sel-current te))
         (eolp (te-end-of-line-p pos te)))
    (if (zerop pos)
        (ed-beep 1)
        (multiple-value-bind (start end) (if eolp
                                             (values (- pos 2) pos)
                                             (values (1- pos) (1+ pos)))
          (let ((chars (te-text-in-range start end te)))
            (%te-delete start end te)
            (%te-set-caret (%te-insert start (concatenate 'string (subseq chars 1 2) (subseq chars 0 1)) te) te)))))
  (values))


(defun te-newline-command (mods code char te)
  (declare (ignore mods code char))
  ;; use after to create a new paragraph and move to it (beginning of line).
  (%te-delete (te-sel-start te) (te-sel-end te) te)
  (%te-set-caret (1- (%te-insert (te-sel-start te) (string #\Newline) te)) te)
  (values))


(defun te-open-line-command (mods code char te)
  (declare (ignore mods code char))
  ;; use after to create a new paragraph and stay on the old paragraph.
  (%te-delete (te-sel-start te) (te-sel-end te) te)
  (%te-insert (te-sel-start te) (string #\Newline) te)
  (values))


(defun te-kill-region-command (mods code char te)
  (declare (ignore mods code char))
  (%te-copy (te-sel-start te) (te-sel-end te) te)
  (%te-delete (te-sel-start te) (te-sel-end te) te)
  (values))

;; deleting:

(defun te-delete-backward-char-command (mods code char te)
  (declare (ignore mods code char))
  ;; if beginning of line, then join with the previous line
  ;; else delete the previous character (in before).
  (cond
    ((/= (te-sel-start te) (te-sel-end te))
     (%te-delete (te-sel-start te) (te-sel-end te) te))
    ((plusp (te-sel-start te))
     (%te-delete (1- (te-sel-start te)) (te-sel-start te) te))
    (t
     (ed-beep 1)))
  (values))


(defun te-delete-forward-char-command (mods code char te)
  (declare (ignore mods code char))
  ;; if end of line, then join with the next line
  ;; else delete the following character (in after).
  (cond
    ((/= (te-sel-start te) (te-sel-end te))
     (%te-delete (te-sel-start te) (te-sel-end te) te))
    ((< (te-sel-start te) (te-length te))
     (%te-delete (te-sel-start te) (1+ (te-sel-start te)) te))
    (t
     (ed-beep 1)))
  (values))


(defun te-join-next-line (lino te)
  (if (< (1+ lino) (te-nlines te))
      (let ((next-start (te-line-start (1+ lino) te)))
        (%te-delete (1- next-start) next-start te))
      (ed-beep 1)))


(defun te-kill-line-command (mods code char te)
  (declare (ignore mods code char))
  (let* ((start (te-sel-current te))
         (lino  (te-line-at start te))
         (end   (te-line-end lino te)))
    (format-trace 'te-kill-line-command
                  :start start :end end :lino lino
                  :length (te-length te)
                  :start (te-sel-start te) :end (te-sel-end te) :current (te-sel-current te)
                  :test (list (/= (te-sel-start te) (te-sel-end te))
                              (= (te-sel-current te) end)
                              (< end (te-length te))))
    (if (and (= (te-sel-start te) (te-sel-end te))
             (= (te-sel-current te) end)
             (< end (te-length te)))
        (te-join-next-line lino te)
        (progn
          (%te-set-select start end te)
          (%te-copy start end te)
          (%te-delete start end te))))
  (values))


(defun te-yank-command (mods code char te)
  (declare (ignore mods code char))
  (%te-delete (te-sel-start te) (te-sel-end te) te)
  (%te-set-caret (%te-insert (te-sel-start te) (get-scrap :text) te) te)
  (values))


(defun te-undo-command (mods code char te)
  (declare (ignore mods code char te))
  ;; We don't implement it yet.
  ;; Usually, it's implemented by the application anyways.
  (ed-beep 1)
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


(defun te-beginning-of-buffer-command (mods code char te)
  (declare (ignore code char))
  (let ((extend (plusp (logand mods shift-key))))
    (te-change-selection extend 0 te)
    (te-set-top-line 0 te))
  (values))


(defun te-end-of-buffer-command (mods code char te)
  (declare (ignore code char))
  (let ((extend (plusp (logand mods shift-key))))
    (te-change-selection extend (te-length te) te)
    (te-set-top-line (- (te-nlines te) (te-lines-per-page te)) te))
  (values))


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
          (new-point (te-line-end lino te)))
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
          (let* ((new-line-start  (te-line-start (1- lino) te))
                 (new-line-length (te-line-length (1- lino) te))
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
          (let* ((new-line-start  (te-line-start (1+ lino) te))
                 (new-line-length (te-line-length (1+ lino) te))
                 (new-point       (+ (min colno (1- new-line-length))
                                     new-line-start)))
            (te-change-selection extend new-point te)))))
  (values))


;; special:

(defun te-set-mark-command (mods code char te)
  (declare (ignore mods code char))
  (%te-set-caret (te-sel-current te) te)
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
      (let ((rect (te-%view-rect te)))
        (erase-rect* (rect-left rect) (rect-top rect) (rect-width rect) (rect-height rect))
        (te-update rect te))))
  (values))



(defun te-lines-per-page (te)
  (truncate (rect-height (te-%view-rect te)) (te-line-height te)))

(defun te-max-top-line (te)
  (1- (te-nlines te)))

(defun te-top-line (te)
  (values (truncate (- (rect-top (te-%view-rect te)) (rect-top (te-%dest-rect te))) (te-line-height te))))

(defun te-bottom-line (te)
  (1- (truncate
       (- (rect-bottom (te-%view-rect te))
          (rect-top    (te-%dest-rect te)))
       (te-line-height te))))


(defun te-set-top-line (lino te)
  (let ((new-top-offset (* lino (te-line-height te)))
        (view-rect      (te-%view-rect te))
        (dest-rect      (te-dest-rect te)))
    (setf (rect-top dest-rect) (- (rect-top view-rect) new-top-offset))
    (te-set-rects dest-rect view-rect te)))

(defun te-set-bottom-line (lino te)
  (te-set-top-line (max 0 (- lino (te-lines-per-page te) -1)) te))


(defun te-offset-for-top-line (lino te)
  (- (- (rect-top (te-%view-rect te))
        (* lino (te-line-height te)))
     (rect-top (te-%dest-rect te))))

(defun te-offset-for-bottom-line (lino te)
  (- (- (rect-top (te-%view-rect te))
        (* (max 0 (- lino (te-lines-per-page te) -1)) (te-line-height te)))
     (rect-top (te-%dest-rect te))))


(defun te-show-charpos (charpos te)
  (let ((view-rect  (te-%view-rect te))
        (lino       (te-line-at charpos te))
        (dh 0)
        (dv 0))
    ;; scroll to lino
    (cond
      ((< lino (te-top-line te))
       (setf dv (te-offset-for-top-line lino te)))
      ((< (te-bottom-line te) lino)
       (setf dv (te-offset-for-bottom-line lino te))))
    (let* ((carect  (te-compute-caret-rect charpos te))
           (left    (rect-left  carect))
           (right   (rect-right carect)))
      (cond
        ((< left (1+ (rect-left view-rect)))
         ;; |left   [r.left  r.right]
         ;; scroll right
         (setf dh (- (1+ (rect-left view-rect)) left)))
        ((< (1- (rect-right view-rect)) right)
         ;; [r.left  r.right]    right|
         ;; scroll left
         (setf dh (- (1- (rect-right view-rect)) right)))))
    (unless (and (zerop dh) (zerop dv))
      (%te-scroll dh dv te))))


(defun te-scroll-up-command (mods code char te)
  (declare (ignore mods code char))
  (te-set-top-line (min (te-max-top-line te)
                        (+ (te-top-line te) (te-lines-per-page te) -1)) te)
  (values))


(defun te-scroll-down-command (mods code char te)
  (declare (ignore mods code char))
  (te-set-top-line (max 0 (- (te-top-line te) (te-lines-per-page te) -1)) te)
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

(defconstant +up-arrow+    #xF700)
(defconstant +down-arrow+  #xF701)
(defconstant +left-arrow+  #xF702)
(defconstant +right-arrow+ #xF703)
(defconstant +delete+      #xF728)
(defconstant +home+        #xF729)
(defconstant +end+         #xF72B)
(defconstant +page-up+     #xF72C)
(defconstant +page-down+   #xF72D)

(defun te-set-default-bindings (te)
  (te-bind :default      'te-self-insert-command          te)
  (te-bind        0      'te-set-mark-command             te)
  (te-bind        1      'te-beginning-of-line-command    te)
  (te-bind        2      'te-backward-char-command        te)
  (te-bind        3      'te-enter-command                te)
  (te-bind        4      'te-delete-forward-char-command  te)
  (te-bind        5      'te-end-of-line-command          te)
  (te-bind        6      'te-forward-char-command         te)
  (te-bind        7      'te-beep-command                 te)
  (te-bind        8      'te-beep-command                 te)
  (te-bind        9      'te-beep-command                 te)
  (te-bind       10      'te-newline-command              te)
  (te-bind       11      'te-kill-line-command            te)
  (te-bind       12      'te-recenter-top-bottom-command  te)
  (te-bind       13      'te-newline-command              te)
  (te-bind       14      'te-next-line-command            te)
  (te-bind       15      'te-open-line-command            te)
  (te-bind       16      'te-previous-line-command        te)
  (te-bind       17      'te-beep-command                 te)            
  (te-bind       18      'te-beep-command                 te)
  (te-bind       19      'te-beep-command                 te)
  (te-bind       20      'te-transpose-chars-command      te)
  (te-bind       21      'te-beep-command                 te)
  (te-bind       22      'te-scroll-up-command            te)
  (te-bind       23      'te-kill-region-command          te)
  (te-bind       24      'te-beep-command                 te)            
  (te-bind       25      'te-yank-command                 te)
  (te-bind       26      'te-beep-command                 te)
  (te-bind       27      'te-beep-command                 te)
  (te-bind       28      'te-beep-command                 te)
  (te-bind       29      'te-beep-command                 te)
  (te-bind       30      'te-beep-command                 te)
  (te-bind       31      'te-undo-command                 te)
  (te-bind      127      'te-delete-backward-char-command te)
  (te-bind +up-arrow+    'te-previous-line-command        te)
  (te-bind +down-arrow+  'te-next-line-command            te)
  (te-bind +left-arrow+  'te-backward-char-command        te)
  (te-bind +right-arrow+ 'te-forward-char-command         te)
  (te-bind +page-up+     'te-scroll-down-command          te)
  (te-bind +page-down+   'te-scroll-up-command            te)
  (te-bind +delete+      'te-delete-forward-char-command  te)
  (te-bind +home+        'te-beginning-of-buffer-command  te)
  (te-bind +end+         'te-end-of-buffer-command        te))


;; (te-set-default-bindings (test-window-te (front-window)))

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
  (let ((te (test-window-te window)))
    (when te
      (let ((bounds    (view-bounds window))
            (dest-rect (te-dest-rect te))
            (top-char  (te-line-start  (te-top-line te) te)))
        (format-trace 'window-size-part :bounds (rect-to-list bounds))
        (setf (rect-width dest-rect) (rect-width bounds))
        (te-set-rects dest-rect bounds te)
        (te-set-top-line (te-line-at top-char te) te)
        (view-draw-contents window)))))

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
      (te-update (te-%view-rect te) te))
    (call-next-method)))


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


(defclass te-test-item-window (te-test-window)
  ())

(defmethod window-size-parts ((window te-test-item-window))
  (values))

(defmethod view-draw-contents ((window te-test-item-window))
  (let ((te (test-window-te window)))
    (when te
      (with-focused-view window
        (let ((border (inset-rect (te-view-rect te) -2 -2)))
          (draw-rect* (rect-left border) (rect-top border) (rect-width border) (rect-height border)))
        (te-update (te-%view-rect te) te)))))


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


(defun test/te-field ()
  (let* ((window (make-instance 'te-test-item-window
                                :window-title "test/field"
                                :view-size #@(200 100)
                                :view-font '("Times" 18 :plain :srcor)))
         (text   "hello world")
         (te     (te-new (make-rect 20 10 180 30) (make-rect 20 10 180 30) window)))
    (setf (test-window-te window) te)
    ;; ---
    (te-set-wrapping +te-no-wrap+ te)
    (te-set-just +te-just-left+ te)
    (te-set-text text te)
    (with-focused-view (te-in-port te)
      (let ((rect (te-%view-rect te)))
        (erase-rect* (rect-left rect) (rect-top rect) (rect-width rect) (rect-height rect))))
    (view-draw-contents window)
    te))


(defun test/te-wrap-paragraph ()
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
    (te-set-wrapping +te-word-wrap+ te)
    (te-set-just +te-just-left+ te)
    (te-set-text text te)
    (with-focused-view (te-in-port te)
      (let ((rect (te-%view-rect te)))
        (erase-rect* (rect-left rect) (rect-top rect) (rect-width rect) (rect-height rect))))
    (view-draw-contents window)
    (test/paragraph te)
    te))


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


#-(and) (defparameter *text* (format nil "~
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

#-(and) (progn

          (defparameter *te* (test-window-te (front-window)))
          (te-set-text *text* *te*)
          (setf (te-click-stuff (test-window-te (front-window))) nil)
          
          (te-init)
          (test/te-wrap-paragraph)
          (te-set-wrapping +te-no-wrap+ *te*)
          (te-set-wrapping +te-word-wrap+ *te*)
          (te-update (view-bounds (te-in-port *te*)) *te*)
          
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

          (let ((te  (test-window-te (front-window))))
            (rect-to-list (te-compute-caret-rect (te-sel-start te) te)))
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
            (let ((rect  (te-compute-caret-rect (te-sel-start te) te)))
              (list (rect-left rect)
                    (rect-top rect)
                    (rect-right rect)
                    (rect-bottom rect))))
          

          (let* ((te  (test-window-te (front-window)))
                 (caret-position (te-sel-start te))
                 (lino (te-line-at caret-position te)))
            ;; (te-line-coordinates lino te)
            (rect-to-list (te-compute-caret-rect (te-sel-start te) te)))
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


          (te-set-text "bonjour le
monde!

" *te*)

          (defparameter *te* (test/te-field))
          (defparameter *te* (test/te-wrap-paragraph))
          )


;;;; THE END ;;;;
