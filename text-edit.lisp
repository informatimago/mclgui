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

#-(and)
(setf *selection-color-active*     *light-blue-color*
      *selection-color-inactive*   *light-gray-color*)


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
  (click-stuff 0   :type integer)
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
  (current-paragraph-dirty nil :type boolean) ; whether (aref (te-paragraphs te) (te-current-paragraph-index te)) needs to be recomputed from current-paragraph-before-point and current-paragraph-after-point  
  bindings
  (updates '() :type list)
  (line-coordinates-cache nil))


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


(defvar *te-mutex* nil)

(defun te-init ()
  "Initialize the Text Edit Manager."
  (setf *te-mutex* (make-mutex "Text Edit Mutex"))
  (values))


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


(defun te-default-word-break (text charpos)
  "The default workd break function."
  (char<= (aref text charpos) #\space))


(defun te-string-width (string te &optional (start 0) (end nil))
  "
RETURN: The width in pixels to draw the STRING in the font configured in the TE TeRec.
"
  (let* ((ff          (dpb (te-tx-font te) (byte 16 16) (te-tx-face te)))
         (ms          (dpb (te-tx-mode te) (byte 16 16) (te-tx-size te))))
    (font-codes-string-width string ff ms start end)))
(declaim (inline te-string-width))


(defun te-wrap-paragraph (para te)
  "
DO:     Perform word wrapping on the given paragraph.
RETURN: A list of line-starts for the paragraph.
PARA:   A cons (paragraph-start . paragraph-text).
"
  (let* ((starts     (list (car para)))
         (text       (cdr para))
         (width      (rect-width (te-dest-rect te)))
         (word-break (te-word-break te))
         (ff         (dpb (te-tx-font te) (byte 16 16) (te-tx-face te)))
         (ms         (dpb (te-tx-mode te) (byte 16 16) (te-tx-size te)))
         (start      0)
         (end        (length text)))
    (loop
      (let ((text-width (font-codes-string-width text ff ms start end)))
        (when (<= text-width width)
          (return-from te-wrap-paragraph (nreverse starts)))
        (multiple-value-bind (found index order)
            (dichotomy (lambda (mid)
                         (let* ((text-width (font-codes-string-width text ff ms start mid))
                                (result (integer-compare width text-width)))
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


(defun te-calculate-text (te)
  "
DO:     Compute nlines and line-starts, as the number and positions of
        displayed lines.  When (plusp (te-cr-only te)) paragraphs are
        wrapped, so several lines may be computed for a paragraph.
NOTE:   Call TE-CAL-TEXT, which is the one with mutex.
"
  (progn (print 'te-calculate-text *terminal-io*) (finish-output *terminal-io*))
  (setf (te-line-coordinates-cache te) nil)
  (te-update-font-info te)
  (if (minusp (te-cr-only te))
      ;; no wrapping:
      (let ((starts (make-array (te-nparagraphs te) :adjustable t :initial-element 0 :fill-pointer 0)))
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
    (progn (print 'te-set-text *terminal-io*) (finish-output *terminal-io*))
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


(defun te-is-active (te)
  "Whether the TeRec is active."
  (plusp (te-active te)))


(defun te-has-caret (te)
  "Whether the TeRec shall display a caret (insteadf of a selection)."
  (= (te-sel-start te) (te-sel-end te)))


(declaim (inline te-is-active te-has-caret))


(defun integer-compare (a b)
  "Compares A and B, returning -1, 0 or 1 depending on their order."
  (cond
    ((< a b) -1)
    ((> a b) +1)
    (t        0)))


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


(defun te-paragraph (parno te)
  "
PARNO:  An integer between 0 and (te-nparagraphs te) exclusive.
TE:     A TeRec
RETURN: A cons cell containing the index of the position an the text of the paragraph number PARNO.
"
  (when (or (minusp parno) (<= (te-nparagraphs te) parno))
    (error 'out-of-bound-error :start 0 :end (te-nparagraphs te) :index parno
                               :datum (te-paragraphs te)
                               :label "paragraphs"))
  (cond
    ((< parno (te-current-paragraph-index te))
     (aref (te-paragraphs te) parno))
    ((= parno (te-current-paragraph-index te))
     (te-current-paragraph te))
    (t
     (aref (te-paragraphs te) (+ (- parno (te-current-paragraph-index te) 1)
                                 (te-next-paragraph-index te))))))


(defun te-index-of-paragraph-with-position (charpos te)
  "
CHARPOS: the character position, 0-based.
TE :     TeRec
RETURN:  The index of the paragraph containing charpos.
"
  (when (or (minusp charpos) (< (te-length te) charpos))
    (error 'out-of-bound-error :start 0 :end (te-length te) :index charpos
                               :datum te
                               :label "characters"))
  (multiple-value-bind (found parno order)
      (dichotomy (lambda (index) (integer-compare charpos (car (te-paragraph index te))))
                 0 (te-nparagraphs te))
    (declare (ignore found order))
    parno))


(defun %te-move-gap-to (parno te)
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
      ;; otherwise nothing to do.
      )
    (setf (te-current-paragraph-index te) current
          (te-next-paragraph-index    te) next)))


(defun te-adjust-gap (increment te)
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


(defun adjustable-string (string size)
  (let ((astring (make-array size :element-type 'character
                                  :fill-pointer (length string)
                                  :adjustable t)))
    (replace astring string)
    astring))


(defun %te-split-paragraph (parno te)
  (let* ((paragraphs (te-paragraphs te))
         (para-text  (cdr (aref paragraphs parno)))
         (length     (ceiling (* 1.5 (length para-text)))))
    (setf (te-current-paragraph-before-point te) (cons (car (aref paragraphs parno))
                                                       (adjustable-string para-text length))
          (te-current-paragraph-after-point  te) (cons (+ (car (aref paragraphs parno))
                                                          (length para-text)
                                                          1)
                                                       (adjustable-string "" length))
          (te-current-paragraph-dirty        te) nil)))


(defun te-split-paragraph-at (charpos te)
  "
POST:   (= (te-current-paragraph-index te) (te-paragraph-with-position te))
"
  (let ((parno (te-index-of-paragraph-with-position charpos te)))
    (if (= parno (te-current-paragraph-index te))
        ;; resplit current paragraph
        (let* ((before    (cdr (te-current-paragraph-before-point te)))
               (after     (cdr (te-current-paragraph-after-point  te)))
               (start     (car (aref (te-paragraphs te) (te-current-paragraph-index te))))
               (new-split (- charpos start)))
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
                  (te-current-paragraph-before-point te)
                  (cons (car (te-current-paragraph-before-point te))
                        before)
                  (te-current-paragraph-after-point  te)
                  (cons (+ (car (te-current-paragraph-before-point te))
                           (length before))
                        after))))
        (progn
          ;; unsplit current paragraph:
          (te-current-paragraph te) 
          ;; split another paragraph:
          (%te-move-gap-to parno te)
          (%te-split-paragraph parno te)))))


(defun te-whitespacep (char)
  "
RETURN:  Whether CHAR is a white space character (or non-printing control code).
"
  (let ((code (char-code char)))
    (or (<= code #x0020)
        (= code #x007F)
        (= code #x00A0)
        (and (<= #x1680 code)
             (find code #(#x1680 #x180E #x2000 #x2001 #x2002 #x2003
                          #x2004 #x2005 #x2006 #x2007 #x2008 #x2009
                          #x200A #x200B #x202F #x205F #x3000 #xFEFF))))))
(declaim (inline te-whitespacep))


(defun te-text-line (lino te)
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
    (multiple-value-bind (found index order)
        (dichotomy (lambda (parno)
                     (let ((para (te-paragraph parno te)))
                       (integer-compare start (car para))))
                   0 (te-nparagraphs te))
      (declare (ignore found order))
      (let* ((para (te-paragraph index te))
             (end  (min (- end (car para)) (length (cdr para)))))
        ;;(format-trace 'te-text-line :found found :index index :order order :start start :end end :para (car para))
        (subseq (cdr para) (- start (car para)) end)))))


(defun te-line-coordinates (lino te)
  "
LINO:   Line Number, 0-based.
TE:     TeRec
RETURN: justified-line-left base-line line-rect line-text
"
  (values-list
   (cdr 
    (if (eql lino (car (te-line-coordinates-cache te)))
        (te-line-coordinates-cache te)
        (let* ((line-height (te-line-height te))
               (font-ascent (te-font-ascent te))
               (dest-rect   (te-dest-rect   te))
               (just        (te-just        te))
               (top         (+ (rect-top dest-rect) (* lino line-height)))
               (bottom      (+ top line-height))
               (base        (+ top font-ascent))
               (line        (te-text-line lino te))
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


(defun te-line-with-position (charpos te)
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
      (dichotomy-search (te-line-starts te) charpos (function integer-compare))
    (declare (ignore found order))
    lino))


(defun te-column (charpos te)
  "
RETURN: The number of characters between the beginning of the line and te-sel-start.
"
  (multiple-value-bind (found lino order)
      (dichotomy-search (te-line-starts te) charpos (function integer-compare))
    (declare (ignore found order))
    (values (- charpos (aref (te-line-starts te) lino))
            lino)))


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
  (multiple-value-bind (x base line-rect line) (te-line-coordinates lino te)
    (declare (ignore line-rect))
    (format-trace 'te-draw-line :lino lino)
    ;; (erase-rect* (rect-left line-rect) (rect-top line-rect) (rect-width line-rect) (rect-height line-rect))
    (draw-string x base line))
  te)


(defun te-compute-caret-rect (te)
  "
RETURN:  the rectangle where the caret must be drawn; the lino where the caret is.
"
  (assert (te-has-caret te))
  (let* ((caret-position (te-sel-start te))
         (lino (te-line-with-position caret-position te)))
    (multiple-value-bind (x base line-rect line) (te-line-coordinates lino te)
      (declare (ignore base))
      ;; (format-trace 'te-compute-caret-rect (list (rect-left line-rect) (rect-top line-rect) (rect-right line-rect) (rect-bottom line-rect)))
      (let ((caret-x (+ x (te-string-width line te 0 (- caret-position (aref (te-line-starts te) lino))))))
        (values (make-rect (1- caret-x) (rect-top line-rect) (1+ caret-x) (rect-bottom line-rect)) lino)))))


(defun te-call-caret-hook (te)
  (when (te-in-port te)
    (multiple-value-bind (caret-rect lino) (te-compute-caret-rect te)
      (with-font-focused-view (te-in-port te)
        (with-clip-rect-intersect (te-view-rect te)
          (funcall (te-caret-hook te) caret-rect lino te))))))


(defun te-caret-transition (tick te)
  "
DO:     Performs a caret transition when times has come.
"
  (when (<= (te-caret-time te) tick)
    ;; (format-trace 'te-caret-transition :tick tick :time (te-caret-time te) :has-caret  (te-has-caret te))
    (when (te-has-caret te)
      (setf (te-caret-time  te) (* (1+ (truncate tick *caret-half-period*)) *caret-half-period*)
            (te-caret-state te) (- 1 (te-caret-state te)))
      (te-call-caret-hook te))))


(defun te-erase-caret (te)
  "
DO:     Erase the caret.
"
  (setf (te-caret-state te) 0)
  (te-call-caret-hook te))


(defun te-default-caret-hook (rect lino te)
  "The default caret hook function."
  ;; TODO: add a visibility test.
  (if (plusp (te-caret-state te))
      (fill-rect*  (rect-left rect) (rect-top rect) (rect-width rect) (rect-height rect))
      (with-clip-rect-intersect rect
        (erase-rect* (rect-left rect) (rect-top rect) (rect-width rect) (rect-height rect))
        (te-draw-line lino te))))


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
                   (fill-rect* (rect-left rect) (rect-top rect) (rect-width rect) (rect-height rect)))
               #-(and)
               (cond
                 ((te-has-caret te)
                  (erase-rect* (rect-left rect) (rect-top rect) (rect-width rect) (rect-height rect)))
                 ((te-is-active te)
                  (fill-rect* (rect-left rect) (rect-top rect) (rect-width rect) (rect-height rect)))
                 (t
                  (erase-rect* (rect-left rect) (rect-top rect) (rect-width rect) (rect-height rect))
                  (draw-rect* (rect-left rect) (rect-top rect) (rect-width rect) (rect-height rect)))))
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
DO:     Compute the rectangles covering the selection.  There may be
        one, two or three rectangles depending on whether the
        selection is on a single line, on two lines or more.
RETURN: rects, start-lino, end-lino.
"
  (let ((start-lino (te-line-with-position start te))
        (end-lino   (te-line-with-position end   te)))
    (cond
      ((= start-lino end-lino)
       ;; 1 rect
       (multiple-value-bind (x base line-rect line) (te-line-coordinates start-lino te)
         (declare (ignore base))
         (let* ((line-start  (aref (te-line-starts te) start-lino))
                (left        (+ x (te-string-width line te 0 (- start line-start))))
                (right       (+ x (te-string-width line te 0 (- end   line-start)))))
           (values (list (make-rect left  (rect-top    line-rect)
                                    right (rect-bottom line-rect)))
                   start-lino end-lino))))
      ((= (1+ start-lino) end-lino)
       ;; 2 rects
       (multiple-value-bind (start-x start-base start-line-rect start-line) (te-line-coordinates start-lino te)
         ;; TODO: perhaps we should forward those data for the highlight function…
         (declare (ignore start-base))
         (multiple-value-bind (end-x end-base end-line-rect end-line) (te-line-coordinates end-lino te)
           (declare (ignore end-base))
           (let* ((start-line-start  (aref (te-line-starts te) start-lino))
                  (end-line-start    (aref (te-line-starts te) end-lino))
                  (start-left  (+ start-x (te-string-width start-line te 0 (- start start-line-start))))
                  (end-right   (+ end-x   (te-string-width end-line   te 0 (- end   end-line-start)))))
             (values (list (make-rect start-left                   (rect-top    start-line-rect)
                                      (rect-right start-line-rect) (rect-bottom start-line-rect))
                           (make-rect (rect-left  end-line-rect)   (rect-top    end-line-rect)
                                      end-right                    (rect-bottom end-line-rect)))
                     start-lino end-lino)))))
      (t
       ;; 3 rects
       (multiple-value-bind (start-x start-base start-line-rect start-line) (te-line-coordinates start-lino te)
         (declare (ignore start-base))
         (multiple-value-bind (end-x end-base end-line-rect end-line) (te-line-coordinates end-lino te)
           (declare (ignore end-base))
           (let* ((start-line-start  (aref (te-line-starts te) start-lino))
                  (end-line-start    (aref (te-line-starts te) end-lino))
                  (start-left  (+ start-x (te-string-width start-line te 0 (- start start-line-start))))
                  (end-right   (+ end-x   (te-string-width end-line   te 0 (- end   end-line-start)))))
             (values (list (make-rect start-left                   (rect-top    start-line-rect)
                                      (rect-right start-line-rect) (rect-bottom start-line-rect))
                           (make-rect (rect-left  start-line-rect) (rect-bottom start-line-rect)
                                      (rect-right end-line-rect)   (rect-top    end-line-rect))
                           (make-rect (rect-left  end-line-rect)   (rect-top    end-line-rect)
                                      end-right                    (rect-bottom end-line-rect)))
                     start-lino end-lino))))))))


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
                             (multiple-value-call (te-high-hook te)
                               (te-compute-selection-rectangles (te-sel-start te) (te-sel-end te) te)
                               te))))
                      (ecase (first update)
                        (:old-selection
                         (destructuring-bind (op start end) update
                           (declare (ignore op))
                           (when (/= start end)
                             (multiple-value-bind (rects start-lino end-lino)
                                 (te-compute-selection-rectangles start end te)
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
    (progn (print 'te-new *terminal-io*) (finish-output *terminal-io*))
    (te-set-text "" te)
    te))


(defun te-dispose (te)
  "Call this procedure when you're completely through with an edit record."
  (declare (ignore te))
  (values))


(defun te-idle (te)
  ;; (format-trace 'te-idle)
  (with-mutex *te-mutex*
    (when (te-is-active te)
      (te-caret-transition (tick-count) te))))


(defun te-click (pt extend te)
  (with-mutex *te-mutex*
    ;; TODO
    ))



(defun te-clean-selection (start end te)
  (when (< end start) (rotatef start end))
  (setf start (max start 0))
  (setf end   (min end   (te-length te)))
  (values start end))
(declaim (inline te-clean-selection))


(defun te-set-select (start end te)
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
  (with-mutex *te-mutex*
    (with-te-update-display te
     (setf (te-active te) 1
           (te-caret-time te) (tick-count))
     (push :selection (te-updates te)))))


(defun te-deactivate (te)
  (with-mutex *te-mutex*
    (with-te-update-display te
     (setf (te-active te) 0)
     (push :selection (te-updates te)))))


(defun te-key (char te)
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


(defun %te-copy (te)
  (put-scrap :text (progn "TODO")))


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
  (with-mutex *te-mutex*
    (with-te-update-display te
      (%te-delete te))))


(defun te-insert (text te)
  (with-mutex *te-mutex*
    (with-te-update-display te
     (%te-insert text te))))


(defun te-set-just (just te)
  (check-type just (member -1 0 1))
  (with-mutex *te-mutex*    
    (setf (te-just te) just)
    (te-cal-text te)))


(defun te-update (update-rect te)
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

NOTE:   We do nothing since our PUT-SCRAP already puts directly into
        the Desk scrap.
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


(defun te-set-crap-len (length)
  "
DO:     sets the size of the TextEdit scrap to the given number of bytes.
"
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

(defun %te-adjust-starts (charpos increment te)
  (loop :for parno :from (1+ (te-index-of-paragraph-with-position charpos te))
          :below (te-nparagraphs te)
        :do (incf (car (te-paragraph parno te)) increment))
  (loop :for lino :from (1+ (te-line-with-position charpos te))
          :below (te-nlines te)
        :do (incf (aref (te-line-starts te) lino) increment))
  (incf (te-length te) increment))


(defun %te-delete (te)
  )


(defun %te-insert (text te)
  ;; TODO: Deal with newlines!
  (assert (= (te-sel-start te) (te-sel-end te)))
  (let ((charpos (te-sel-current te))
        (count   (length text)))
    (format-trace '%te-insert :text text :charpos charpos)
    (te-split-paragraph-at charpos te)
    (loop
      :for char :across text
      :do (vector-push-extend char (cdr (te-current-paragraph-before-point te))))
    (setf (te-current-paragraph-dirty te) t)
    (format-trace 'te-self-insert (te-current-paragraph-before-point te))
    (incf (car (te-current-paragraph-after-point te)) count)
    (%te-adjust-starts charpos count te)
    (push :display  (te-updates te))
    (te-set-select (+ charpos count) (+ charpos count) te)))


(defun te-self-insert (mods code char te)
  (declare (ignore modes code))
  (when (/= (te-sel-start te) (te-sel-end te))
    ;; TODO: save the selection for undo
    (%te-delete te))
  (%te-insert (string char) te)
  (values))


(defun te-newline (mods code char te)
  (declare (ignore modes code char))
  ;; TODO
  (values))


(defun te-open-line (mods code char te)
  (declare (ignore modes code char))
  ;; TODO
  (values))





(defun te-transpose-chars (mods code char te)
  (declare (ignore modes code char))
  ;; TODO
  (values))

(defun te-kill-region (mods code char te)
  (declare (ignore modes code char))
  (%te-delete te)
  (values))


(defun te-yank (mods code char te)
  (declare (ignore modes code char))
  (%te-copy te)
  (values))


(defun te-undo (mods code char te)
  (declare (ignore modes code char))
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


(defun te-beginning-of-line (mods code char te)
  (declare (ignore code char))
  (multiple-value-bind (colno lino) (te-column (te-sel-current te) te)
    (let ((extend (plusp (logand mods shift-key)))
          (new-point (- (te-sel-current te) colno)))
      (te-change-selection extend new-point te)))
  (values))


(defun te-end-of-line (mods code char te)
  (declare (ignore code char))
  (multiple-value-bind (colno lino) (te-column (te-sel-current te) te)
    (let ((extend (plusp (logand mods shift-key)))
          (new-point (if (= lino (1- (te-nlines te)))
                         (te-length te)
                         (1- (aref (te-line-starts te) (1+ lino))))))
      (te-change-selection extend new-point te)))
  (values))


(defun te-backward-char (mods code char te)
  (declare (ignore code char))
  (let ((extend (plusp (logand mods shift-key)))
        (new-point (max 0 (1- (te-sel-current te)))))
    (te-change-selection extend new-point te))
  (values))


(defun te-forward-char (mods code char te)
  (declare (ignore code char))
  (let ((extend (plusp (logand mods shift-key)))
        (new-point (min (1+ (te-sel-current te)) (te-length te))))
    (te-change-selection extend new-point te))
  (values))


(defun te-previous-line (mods code char te)
  (declare (ignore mods code char))
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


(defun te-next-line (mods code char te)
  (declare (ignore mods code char))
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


;; deleting:

(defun te-delete-backward-char (mods code char te)
  (te-split-paragraph-at (te-sel-end te) te)
  ;; TODO
  (values))


(defun te-delete-char (mods code char te)
  (te-split-paragraph-at (te-sel-end te) te)
  ;; TODO
  (values))


(defun te-kill-line (mods code char te)
  (te-split-paragraph-at (te-sel-end te) te)
  ;; TODO
  (values))


;; special:

(defun te-set-mark (mods code char te)
  (te-set-select (te-sel-end te) (te-sel-end te) te)
  (values))


(defun te-enter (mods code char te)
  (values))


(defun te-beep (mods code char te)
  (ed-beep 1)
  (values))


(defun te-recenter-top-bottom (mods code char te)
  (declare (ignore mods code char))
  (when (te-in-port te)
    (with-font-focused-view (te-in-port te)
      (let ((rect (te-view-rect te)))
        (erase-rect* (rect-left rect) (rect-top rect) (rect-width rect) (rect-height rect))
        (te-update rect te))))
  (values))


(defun te-scroll-up (mods code char te)
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
  (te-bind :default 'te-self-insert te)
  (te-bind     0 'te-set-mark te)
  (te-bind     1 'te-beginning-of-line te)
  (te-bind     2 'te-backward-char te)
  (te-bind     3 'te-enter te)
  (te-bind     4 'te-delete-char te)
  (te-bind     5 'te-end-of-line te)
  (te-bind     6 'te-forward-char te)
  (te-bind     7 'te-beep te)
  (te-bind     8 'te-beep te)
  (te-bind     9 'te-beep te)
  (te-bind    10 'te-newline te)
  (te-bind    11 'te-kill-line te)
  (te-bind    12 'te-recenter-top-bottom te)
  (te-bind    13 'te-newline te)
  (te-bind    14 'te-next-line te)
  (te-bind    15 'te-open-line te)
  (te-bind    16 'te-previous-line te)
  (te-bind    17 'te-beep te)
  (te-bind    18 'te-beep te)
  (te-bind    19 'te-beep te)
  (te-bind    20 'te-transpose-chars te)
  (te-bind    21 'te-beep te)
  (te-bind    22 'te-scroll-up te)
  (te-bind    23 'te-kill-region te)
  (te-bind    24 'te-beep te)
  (te-bind    25 'te-yank te)
  (te-bind    26 'te-beep te)
  (te-bind    27 'te-beep te)
  (te-bind    28 'te-beep te)
  (te-bind    29 'te-beep te)
  (te-bind    30 'te-beep te)
  (te-bind    31 'te-undo te)
  (te-bind   127 'te-delete-backward-char te)
  (te-bind 63232 'te-previous-line te)
  (te-bind 63233 'te-next-line te)
  (te-bind 63234 'te-backward-char te)
  (te-bind 63235 'te-forward-char te))


;;(remove-method (function print-object) (find-method (function print-object) '() '(terec t)))
#-(and)
(defmethod print-object ((te terec) stream)
  (if *print-readably*
      (call-next-method)
      (let* ((line1  (te-text-line 0 te))
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
     (te-index-of-paragraph-with-position charpos te)
     (te-current-paragraph-index te)
     (te-next-paragraph-index    te)
     (te-paragraphs te))))


#-(and) (progn

          (test/te-split)
          (test/te-selection)
          
          (te-set-select start end te)
          (te-change-selection extend new-point te)

          
          (test/te-all)
          (test/te-set-text)
          (test/te-wrap-paragraph)
          
          (dotimes (i (te-nlines (test-window-te (front-window))))
            (write-line (te-text-line i (test-window-te (front-window)))))

          (rect-to-list (te-compute-caret-rect (test-window-te (front-window))))
          (:topleft (0 -1) :size (200 2))
          
          (map nil (lambda (w) (ignore-errors (te-cal-text (test-window-te w))))
            (remove 'te-test-window (windows) :key (lambda (x) (class-name (class-of x))) :test-not (function eql)))

          (let ((te (test-window-te (front-window))))
            (list (te-index-of-paragraph-with-position 0 te)
                  (te-index-of-paragraph-with-position 300 te)
                  (te-index-of-paragraph-with-position (te-length te) te)))

          (let ((te (test-window-te (front-window))))
            (setf (te-length te) 1475))
          (let ((te (test-window-te (front-window))))
            (values (te-length te)
                    (TE-line-starts te)))


          
          (let ((te (test-window-te (front-window))))
            (TE-TEXT-LINE 25 te))
          
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
          
          (0 324 376 342)
          (-1 324 1 342)
          
          (0 324 376 342)
          (-1 324 1 342)

          (324 0 342 376)
          (-1 0 1 376)
          (324 0 342 376)
          (-1 0 1 376)         

          (594 0 612 200) 
          (-1 0 1 200)


          (let* ((te  (test-window-te (front-window)))
                 (caret-position (te-sel-start te))
                 (lino (te-line-with-position caret-position te)))
            ;; (te-line-coordinates lino te)
            (rect-to-list (te-compute-caret-rect te)))
          (:topleft (-1 0) :size (2 200))
          0
          608
          (rect-to-list #S(rect :topleft 594 :bottomright 13107812)) (:topleft (594 0) :size (18 200))
          ""
          
          (te-line-coordinates lino te)
          )

;;;; THE END ;;;
;
