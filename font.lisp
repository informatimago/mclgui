;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               font.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Defines the Font API.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-05-09 <PJB> Created.
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
(mclgui.readtable:enable-objcl+ccl-reader-macros)
(in-package "MCLGUI")



(defun string-start-end (string &optional start end)
  (let* ((string (string string))
         (len    (length string))
         (start  (or start 0))
         (end    (or end len)))
    (check-type start fixnum "a start index in the string")
    (check-type end   fixnum "an end position in the string")
    (flet ((are (a i) (error "Array index ~S out of bounds for ~S." i a)))    
      (unless (<= 0 end len)   (are string end))
      (unless (<= 0 start len) (are string start))
      (unless (<= start end)
        (error "Start ~S exceeds end ~S for a string operation." start end))
      (multiple-value-bind (str off) (array-displacement string)
        (values (or str string) (+ off start) (+ off end))))))


(declaim (inline nsubstring-start-end))
(defun nsubstring-start-end (string start end)
  (multiple-value-call (function nsubseq) (string-start-end string start end)))


;; On MacOSX 10.9, system fonts are not found in the normal font
;; families.  So we need to process them specially.

(defun additional-fonts ()
  "
RETURN: A list of additional NSFont that may be in families not
        obtained by -[NSFontManager availableFontFamilies].
"
  (list [NSFont boldSystemFontOfSize:(cgfloat 12.0d0)]
        [NSFont controlContentFontOfSize:(cgfloat 12.0d0)]
        [NSFont labelFontOfSize:(cgfloat 12.0d0)]
        [NSFont menuFontOfSize:(cgfloat 12.0d0)]
        [NSFont menuBarFontOfSize:(cgfloat 12.0d0)]
        [NSFont messageFontOfSize:(cgfloat 12.0d0)]
        [NSFont paletteFontOfSize:(cgfloat 12.0d0)]
        [NSFont systemFontOfSize:(cgfloat 12.0d0)]
        [NSFont titleBarFontOfSize:(cgfloat 12.0d0)]
        [NSFont toolTipsFontOfSize:(cgfloat 12.0d0)]))


(defun additional-font-families ()
  "
RETURN:         A list of lisp strings naming the additional font families.
"
  (delete-duplicates
   (mapcar (lambda (font) (objcl:lisp-string [font familyName]))
           (additional-fonts))
   :test (function string=)))


;; ---

(defun available-fonts ()
  "
RETURN:         A list of font names (STRING)
"
  (nsarray-to-list [[NSFontManager sharedFontManager] availableFonts]))


(defun available-font-families ()
  "
RETURN:         A list of font family names (STRING).
"
  (delete-duplicates
   (append (additional-font-families)
           (nsarray-to-list [[NSFontManager sharedFontManager] availableFontFamilies]))
   :test (function string=)))


(defun available-members-of-font-family (family)
  "
RETURN:         A list of FONT-DESCRIPTION lists for members of the font FAMILY:
                0. The PostScript font name, as a STRING.
                1. The part of the font name used in the font panel
                   that’s not the font name, as an STRING
                   object. This value is not localized—for example,
                   \"Roman\", \"Italic\", or \"Bold\".
                2. The font’s weight (integer).
                3. The font’s traits (integer).
"
  (mapcar (lambda (element) (coerce element 'list))
          (nsarray-to-list [[NSFontManager sharedFontManager]
                            availableMembersOfFontFamily:(objcl:objc-string family)])))


(defstruct (font-description
             (:type list)
             (:conc-name fd-))
  name
  variant
  weight
  traits)


(defvar *font-families* nil
  "An A-List mapping font family names (STRING) to a list of FONT-DESCRIPTION.")

(defvar *font-traits* nil
  "An A-List mapping NS font traits masks to NS font-trait keywords.")

;; *font-traits*
;; ((1 . :italic) (2 . :bold) (4 . :unbold) (8 . :non-standard-character-set)
;;  (16 . :narrow) (32 . :expanded) (64 . :condensed) (128 . :small-caps)
;;  (256 . :poster) (512 . :compressed) (1024 . :fixed-pitch)
;;  (16777216 . :unitalic))
;; 
;; *style-alist*
;; ((:plain . 0) (:bold . 1) (:italic . 2) (:underline . 4) (:outline . 8)
;;  (:shadow . 16) (:condense . 32) (:extend . 64))

;; (font-traits-to-style '(:italic :bold :unbold
;;                         :non-standard-character-set :narrow :expanded
;;                         :condensed :small-caps :poster :compressed
;;                         :fixed-pitch :unitalic))
;; -->
;; (:italic :bold :condense :extend :condense :condense)
;; (:unbold :non-standard-character-set :small-caps :poster :fixed-pitch :unitalic)


;; (style-to-font-traits '(:plain :bold :italic :underline :outline :shadow :condense :extend))
;; -->
;; (:expanded :compressed :italic :bold)
;; (:shadow :outline :underline :plain)


(defvar *font-traits-to-style*
  '((:italic     . :italic)
    (:bold       . :bold)
    (:condensed  . :condense) ; must come before the other :condense.
    (:compressed . :condense)
    (:narrow     . :condense)
    (:expanded   . :extend))
  "An A-List mapping NS font-trait keywords to Mac style keywords")

;; Note: :underline, :outline and :shadow must be processed separately
;; on OpenStep, they're not encoded into the font, but are implemented
;; as transformations on the font.


(defun mask-to-font-traits (mask)
  "
MASK:           A NS font traits mask integer.
RETURN:         A list of NS font traits keywords.
EXAMPLE:        (mask-to-font-traits #x13) --> (:italic :bold :narrow)
"
  (loop
    :for (bit . key) :in *font-traits*
    :when (plusp (logand bit mask))
    :collect key))


(defun font-traits-to-style (traits)
  "
TRAITS:         A list of NS font trait keywords.
RETURN:         A list of style keywords ; a list of remaining NS font trait keywords.
EXAMPLE:        (font-traits-to-style '(:italic :bold :narrow)) --> (:italic :bold :condense) ; nil
"

  (loop
    :for trait :in traits
    :for entry = (assoc trait *font-traits-to-style*)
    :if entry :collect (cdr entry) :into style
    :else     :collect trait :into remaining
    :finally (return (values (or style '(:plain))
                             remaining))))


(defun style-to-mask (style)
  "
STYLE:          A list of Mac style keywords.
RETURN:         A Mac style mask.
EXAMPLE:        (style-to-mask '(:bold :italic :underline :outline :shadow :condense :extend)) --> 127
"
  (loop
    :with mask = 0
    :for style :in (ensure-list style)
    :for entry = (assoc style *style-alist*)
    :when entry
    :do (setf mask (logior mask (cdr entry)))
    :finally (return mask)))


(defun mask-to-style (mask)
  "
MASK:          A Mac style mask.
RETURN:        A list of Mac style keywords.
EXAMPLE:       (mask-to-style #x7f) --> (:bold :italic :underline :outline :shadow :condense :extend)
"
  (or (loop
        :for (style . bit) :in *style-alist*
        :when (plusp (logand bit mask))
        :collect style)
      '(:plain)))


(defun style-to-font-traits (style)
  "
STYLE:          A Mac style list.
RETURN:         A list of font-traits keywords ; a list of remaining Mac style keywords.
EXAMPLE:        (style-to-font-traits '(:italic :bold :underline :outline :extend))
                --> (:expanded :italic :bold)
                    (:outline :underline)
"
  (loop
    :with font-traits = '()
    :with remaining   = '()
    :with style = (ensure-list style)
    :for (key . nil) :in *style-alist*
    :when (member key style)
    :do (let ((entry (rassoc key *font-traits-to-style*)))
          (if entry
              (push (car entry) font-traits)
              (push key remaining)))
    :finally (return (values font-traits remaining))))


(defun font-traits-to-mask (font-traits)
  (loop
    :with mask = 0
    :for trait :in font-traits
    :for entry = (rassoc trait *font-traits*)
    :when entry
    :do (setf mask (logior mask (car entry)))
    :finally (return mask)))


;; (font-traits-to-style (mask-to-font-traits #x13))
;; --> (:italic :bold :condense)
;;
;; (font-traits-to-mask
;;  (style-to-font-traits
;;   (mask-to-style
;;    (style-to-mask
;;     (font-traits-to-style
;;      (mask-to-font-traits
;;       #x13))))))
;; --> 515


;; (mapcar (lambda (family)
;;           (mapcar (lambda (fd)
;;                     (multiple-value-list (font-traits-mask-to-style (fd-traits fd))))
;;                   (cdr family)))
;;         *font-families*)




;;;---------------------------------------------------------------------
;;; Macintosh font numbers (cf. Inside Macintosh I, page  I-219):
;;;---------------------------------------------------------------------
;;;
;;; 0          system font
;;; 1          application font
;;; 2 .. 24    pre-defined font numbers 
;;; 25 .. 127  apple fonts
;;; 128 .. 255 third-party fonts
;;;

(defun font-family (font-name)
  (find font-name *font-families*
        :key (function cdr)
        :test (lambda (o e) (find o e
                                  :key (function car)
                                  :test (function string-equal)))))

(defun system-font-name ()
  (objcl:lisp-string [[NSFont systemFontOfSize:(cgfloat 12.0d0)] familyName]))

(defun application-font-name ()
  (objcl:lisp-string [[NSFont userFontOfSize:(cgfloat 12.0d0)] familyName]))





(defvar *mac-font-names*
  '((:newyork    . "Times")
    (:geneva     . "Geneva")
    (:monaco     . "Monaco")
    (:venice     . "Geneva")
    (:london     . "Times")
    (:athens     . "Geneva")
    (:sanfran    . "Geneva")
    (:toronto    . "Times")
    (:cairo      . "Geneva")
    (:losangeles . "Geneva")
    (:times      . "Times")
    (:helvetica  . "Helvetica")
    (:courier    . "Courier")
    (:symbol     . "Symbol")
    (:taliesin   . "Geneva")))


(defconstant +font-number-base+ 32)

(defun font-name-from-number (font-number)
  (case font-number
    ((0) (system-font-name))
    ((1) (application-font-name))
    (otherwise
     (if (< font-number +font-number-base+)
         (let ((name (cdr (nth (- font-number 2) *mac-font-names*))))
           (if (member name *font-list* :test (function string-equal))
               name
               (application-font-name)))
         (or (nth (- font-number +font-number-base+) *font-list*)
             (application-font-name))))))


(defun font-number-from-name (font-name)
  (let ((num (position font-name *mac-font-names*
                       :test (function string-equal)
                       :key (function car))))
    (if num
        (+ 2 num)
        (let ((num (position font-name *font-families*
                             :test (lambda (o ff)
                                     (or (string-equal o (car ff))
                                         (find o (cdr ff)
                                               :key (function car)
                                               :test (function string-equal)))))))
          (if num
              (+ num +font-number-base+)
              0)))))






(defvar *current-font-codes* (list 0 0)
  "Current font codes.")

(defun current-font-codes ()
  "RETURN: The font codes of the current font."
  (values-list *current-font-codes*))

(defun set-current-font-codes (ff ms)
  "DO: Set the current font codes."
  (setf (first  *current-font-codes*) ff
        (second *current-font-codes*) ms)
  (values ff ms))

(defmacro with-font-codes (ff-code ms-code &body body)
  `(let ((*current-font-codes* (list ,ff-code ,ms-code)))
     ,@body))


;; Font-face layout:
;; +------------------------------+---------------+---------------+
;; |      txFont                  |   txFace      |   color       |
;; +------------------------------+---------------+---------------+
;;  31                          16 15            8 7             0
;; 
;; Mode-size layout:
;; +------------------------------+-------------------------------+
;; |      txMode                  |         txSize                |
;; +------------------------------+-------------------------------+
;;  31                          16 15                            0

(defun font-values (ff-code ms-code)
  "
RETURN: the five font values: font, size, mode, face and color.
"
  (let* ((ff-code   (or ff-code 0))
         (ms-code   (or ms-code 0))
         (font-code (point-v ff-code))
         (font      (font-name-from-number font-code))
         (mode      (xfer-mode-to-name (point-v ms-code)))
         (size      (point-h ms-code))
         (color     (ldb (byte 8 0) ff-code))
         (ff-code   (ldb (byte 8 8) ff-code))
         (face      (if (= ff-code 0)
                        :plain
                        (loop 
                          :for (style . code) :in *style-alist*
                          :when (plusp (logand code ff-code))
                          :collect style))))
    (values font size mode face color)))


(defun font-spec (ff-code ms-code)
  "
A font specification (font spec) is an atom or list of atoms
specifying one or more of the following: the font name, font size,
font styles, font color and transfer mode. They are more humanly
readable than font codes.  They can be translated into font codes
through the function FONTCODES.  Font codes represent font information
in a way that accesses the Macintosh Font Manager directly.  Since they
don’t need to be interpreted, they are significantly faster than font
specifications.  They can be translated into font specifications
explicitly through the function FONT-SPEC.

RETURN:         A font specification created from the font codes.

FF-CODE:        The font-face code, a 32-bit integer combining the
                encoded name of the font and its face (plain, bold,
                italic, etc).

MS-CODE:        The mode-size code, a 32-bit integer indicating the
                font mode (inclusive-or, exclusive-or, complemented,
                etc), and the font size.
"
  (multiple-value-bind (name size mode style color) (font-values ff-code ms-code)
    (list* name size mode
           (append (if (atom style)
                       (list style)
                       style)
                   (list (list :color-index color))))))


(defun sys-font-codes ()
  ;; (values 0 0)
  (multiple-value-bind (ff ms #|and more|#) (font-codes '("Helvetica" 14))
    (values ff ms)))


(defun sys-font-spec ()
  (multiple-value-call (function font-spec) (sys-font-codes)))


(define-condition invalid-font-spec-error (error)
  ((font-spec :initarg :font-spec
              :reader invalid-font-spec)
   (reason    :initarg :reason
              :reader invalid-font-spec-reason)
   (option    :initarg :option
              :reader invalid-font-spec-option))
  (:report  (lambda (err stream)
              (format stream "Invalid font spec ~S, option ~S: ~A."
                      (invalid-font-spec err)
                      (invalid-font-spec-option err)
                      (case (invalid-font-spec-reason err)
                        (:duplicate-size      "duplicate sizes")
                        (:duplicate-name      "duplicate names")
                        (:duplicate-color     "duplicate colors")
                        (:invalid-color       "invalid color")
                        (:duplicate-text-mode "duplicate text-modes")
                        (:invalid-option      "invalid option" )
                        (otherwise (invalid-font-spec-reason err)))))))

(defun font-codes (font-spec &optional old-ff old-ms)
  "
Creates font codes from a font specification.

RETURN:         Four values: the font-face code, the mode-size code,
                the ff-mask, and the ms-mask. The two latter values
                are masks that tell which bits were specified in the
                font-face and mode-size codes, respectively.

FONT-SPEC:      A font specification.

OLD-FF:         The old font/face code. A font/face code is a 32-bit
                integer that combines the encoded name of the font and
                its face (plain, bold, italic, and so on). If there is
                an old-ff, its values are used if the new font
                specification specifies no value for either the font
                name or its face. If old-ff is nil or unspecified, it
                defaults to 0.

OLD-MS:         The old mode-size code. A mode-size code is a 32-bit
                integer that indicates the font mode (inclusive-or,
                exclusive-or, complemented, and so on) and the font
                size.  If there is an old-ms, its values are used if
                the new font specification specifies no value for
                either the font mode or its size. If old-ms is nil or
                unspecified, it defaults to 65536 (the code for a mode
                of :SRCOR and a size of 0).
"
  (unless font-spec
    (return-from font-codes (values old-ff old-ms 0 0)))
  (let* ((items      (ensure-list font-spec))
         (font       nil)
         (face       nil)
         (color      nil)
         (mode       nil)
         (size       nil)
         (font-mask  0)
         (face-mask  0)
         (color-mask 0)
         (mode-mask  0)
         (size-mask  0)
         (reset-style-p nil)
         (old-ff     (or old-ff 0))
         (old-ms     (or old-ms (make-point 0 (xfer-mode-arg :srcOr)))))
    (dolist (item items)
      (cond
        ((realp item)
         (when size 
           (error 'invalid-font-spec-error :font-spec font-spec 
                  :reason :duplicate-size  :option item))
         (setf size item
               size-mask -1))
        ((or (stringp item) (and (symbolp item) (not (keywordp item))))
         (when font
           (error 'invalid-font-spec-error :font-spec font-spec 
                  :reason :duplicate-name  :option item))
         (setf font-mask -1)
         (setf font (font-number-from-name item)))
        ((consp item)
         (ecase (first item)
           (:color-index
            (when color
              (error 'invalid-font-spec-error :font-spec font-spec 
                     :reason :duplicate-color  :option item))
            (setf color (second item)
                  color-mask 255)
            (unless (and (fixnump color)
                         (<= 0 color 255))
              (error 'invalid-font-spec-error :font-spec font-spec 
                     :reason :invalid-color  :option item)))
           (:color
            (when color
              (error 'invalid-font-spec-error :font-spec font-spec 
                     :reason :duplicate-color  :option item))
            (setf color (color->ff-index (second item))
                  color-mask 255))))
        ((let ((temp (xfer-mode-arg item :if-does-not-exist nil)))  
           (when temp
             (if mode
                 (unless (eql item :plain)
                   (error 'invalid-font-spec-error :font-spec font-spec 
                          :reason :duplicate-text-mode  :option item))
                 (setf mode temp
                       mode-mask -1)))))
        ((let ((entry (assoc item *style-alist*)))
           (when entry
             (when (eql (car entry) :plain)
               (setf reset-style-p t
                     face-mask -1))
             (let ((code (cdr entry)))
               (setf face      (logior code (or face 0))
                     face-mask (logior code face-mask))))))
        (t
         (error 'invalid-font-spec-error :font-spec font-spec 
                :reason :invalid-option :option item))))
    (let ((font  (or font  (point-v old-ff)))
          (face  (if (and reset-style-p face)
                     face
                     (logior (or face 0) (ldb (byte 8 8) (point-h old-ff)))))
          (color (or color (ldb (byte 8 0) (point-h old-ff))))
          (mode  (or mode  (point-v old-ms)))
          (size  (or size  (point-h old-ms))))
      ;; (print (list :font font :face face :color color :mode mode :size size))
      (values (make-point (dpb face (byte 8 8) color) font)
              (make-point size mode)
              (make-point (dpb face-mask (byte 8 8) color-mask) font-mask)
              (make-point size-mask mode-mask)))))


(defun real-font (&optional font-spec)
  "
RETURN:         Whether the FONT-SPEC corresponds to a font or
                font-size that actually exists in the system, and is
                not a calculated font.

FONT-SPEC:      A font specification.  The default is the current
                font.
"
  (if font-spec
      (let ((font-name (find-if (function stringp) font-spec)))
        (and font-name (first (font-family font-name))))
      (values (multiple-value-call (function font-values) (current-font-codes)))))



(defun nsfont-from-codes (ff ms)
  (multiple-value-bind (name size mode face color) (font-values ff ms)
    (multiple-value-bind (traits others) (style-to-font-traits face)
      (values
       [[NSFontManager sharedFontManager]
        convertFont:[NSFont fontWithName:(objcl:objc-string name)
                            size: (cgfloat size)]
        toHaveTrait:(font-traits-to-mask traits)]
       mode color others))))


(defun font-codes-info (ff ms)
  "
RETURN:         Four values that represent (in pixels) the ascent,
                descent, maximum width, and leading of the font
                specified by FF-CODE and MS-CODE.

FF:             Font/Face code.

MS:             Mode/Size code.

The ascent is the distance from the baseline to the highest ascender
of the font, the descent is the distance from the baseline to the
lowest descender of the font, the maximum width is that of the widest
character in the font, and the leading is the suggested spacing
between lines. Only the font and font-size aspects of font-spec are
used in the calculation. The font styles and transfer mode are not
significant.
"
  (let ((font (nsfont-from-codes ff ms)))
    (values [font ascender]
            (- [font descender])
            (nsrect-width (get-nsrect [font boundingRectForFont]))
            [font leading])))


(defun font-codes-line-height (ff ms)
  "
RETURN:         The line height for the font specified by FF and MS.

FF:             Font/Face code.

MS:             Mode/Size code.
"
  (multiple-value-bind (a d w l) (font-codes-info ff ms)
    (declare (ignore w))
    (values (ceiling (+ a d l)))))


(defun font-info (&optional font-spec)
  "
RETURN:         four values that represent (in pixels) the ascent,
                descent, maximum width, and leading of font-spec.

FONT-SPEC:      If not supplied, the current font is used.

The ascent is the distance from the baseline to the highest ascender
of the font, the descent is the distance from the baseline to the
lowest descender of the font, the maximum width is that of the widest
character in the font, and the leading is the suggested spacing
between lines.  Only the font and font-size aspects of font-spec are
used in the calculation.  The font styles and transfer mode are not
significant.
"
  (multiple-value-bind (ff ms) (if font-spec
                                   (font-codes font-spec)
                                   (current-font-codes))
    (font-codes-info ff ms)))


(defun font-line-height (&optional font-spec)
  (multiple-value-bind (a d w l) (font-info font-spec)
    (declare (ignore w))
    (values (round (+ a d l)))))



;; (multiple-value-bind (ff ms)  (font-codes '("Times" 12 :italic :bold (:color "blue")))
;;   (values (multiple-value-list (nsfont-from-codes ff ms))
;;           (multiple-value-list (font-codes-info ff ms))))
;; (#<ns-font "Times-BoldItalic 12.00 pt. P [] (0x6129bf0) fobj=0x1cb4f0, spc=3.00" (#x1CC0B0)> :srcor 0 nil)
;; (9.0D0 -3.0D0 23.173828 0.0D0)
;; (#<ns-font "Times-BoldItalic 12.00 pt. P [] (0x6129bf0) fobj=0x1cb4f0, spc=3.00" (#x1CC0B0)> :srcor 0 nil)
;; (9.0D0 -3.0D0 23.173828 0.0D0)



;; NSFontDescriptor:

(defvar NSFontFamilyAttribute          ""
  "An optional NSString object that specifies the font family.")
(defvar NSFontNameAttribute            ""
  "An optional NSString object that specifies the font name.")
(defvar NSFontFaceAttribute            ""
  "An optional NSString object that specifies the font face.")
(defvar NSFontSizeAttribute            ""
  "An optional NSString object, containing a float value, that specifies the font size.")
(defvar NSFontVisibleNameAttribute     ""
  "An optional NSString object that specifies the font’s visible name.")
(defvar NSForegroundColorAttributeName "")
;; (defvar NSFontColorAttribute           ""
;;   "An optional NSData object that specifies the font color.
;;  (Deprecated. Use NSForegroundColorAttributeName instead.)")
(defvar NSFontMatrixAttribute          ""
  "An NSAffineTransform instance that specifies the font’s transformation matrix.
The default value is the identity matrix.")
(defvar NSFontVariationAttribute       ""
    "An NSDictionary instance that describes the font’s variation axis.
The default value is supplied by the font. See “Font variation axis dictionary keys” for dictionary keys.")
(defvar NSFontCharacterSetAttribute    ""
  "An NSCharacterSet instance that represents the set of Unicode characters covered by the font.
The default value is supplied by the font.")
(defvar NSFontCascadeListAttribute     ""
  "An NSArray instance—each member of the array is a sub-descriptor.
The default value is the system default cascading list for user's locale.")
(defvar NSFontTraitsAttribute          ""
  "An NSDictionary instance instance fully describing font traits.
The default value is supplied by the font. See “Font traits dictionary keys” for dictionary keys.")
(defvar NSFontFixedAdvanceAttribute    ""
  "An NSNumber instance containing a float value that overrides the glyph advancement specified by the font.
The default value is 0.0.")
(defvar NSFontFeatureSettingsAttribute ""
  "An array of dictionaries representing non-default font feature settings.
Each dictionary contains NSFontFeatureTypeIdentifierKey and NSFontFeatureSelectorIdentifierKey.")

;; Font Traits Attributes:
(defvar NSFontSymbolicTrait ""
  "The symbolic traits value as an NSNumber object.")
(defvar NSFontWeightTrait   ""
  "The normalized weight value as an NSNumber object.
The valid value range is from -1.0 to 1.0. The value of 0.0 corresponds to the regular or medium font weight.")
(defvar NSFontWidthTrait    ""
  "The relative inter-glyph spacing value as an NSNumber object.
The valid value range is from -1.0 to 1.0. The value of 0.0 corresponds to the regular glyph spacing.")
(defvar NSFontSlantTrait    ""
  "The relative slant angle value as an NSNumber object.
The valid value range is from -1.0 to 1.0. The value of 0.0 corresponds to 0 degree clockwise rotation from the vertical and 1.0 corresponds to 30 degrees clockwise rotation.")

;; NSAttributedString
(defvar NSUnderlineStyleAttributeName  "") ; 0/1
(defvar NSShadowAttributeName          "") ; nil or NSShadow
(defvar NSStrokeWidthAttributeName     "") ; 0.0d0 or 3.0d0
(defvar NSFontAttributeName            "") ; a NSFont


(defstruct shadow
  (blur-radius 1.0f0)
  (color       (make-color 0 0 0 1/3))
  (offset      (make-nssize :width 1.0f0 :height 1.0f0)))

(defmethod unwrap ((self shadow))
  (unwrapping self
              (let ((shadow [[NSShadow alloc] init]))
                [shadow setShadowOffset:(unwrap (shadow-offset self))]
                [shadow setShadowBlurRadius:(shadow-blur-radius self)]
                [shadow setShadowColor:(unwrap (shadow-color self))]
                shadow)))


(defvar *default-shadow* nil)

(defstruct descriptor-cache ff ms descriptor)
(defvar *descriptor-cache* (make-descriptor-cache))
;; (setf *descriptor-cache* (make-descriptor-cache))

(defvar *descriptor-cache-usage* (list 0))

(defun font-descriptor-from-codes (ff ms)
  (if (and (eql ff (descriptor-cache-ff *descriptor-cache*))
           (eql ms (descriptor-cache-ms *descriptor-cache*)))
      (incf (car *descriptor-cache-usage*))
      (progn
        (push 0  *descriptor-cache-usage*)
        (setf  (descriptor-cache-ff *descriptor-cache*) ff
               (descriptor-cache-ms *descriptor-cache*) ms
               (descriptor-cache-descriptor *descriptor-cache*)
               (multiple-value-bind (name size mode face color) (font-values ff ms)
                 (multiple-value-bind (traits others) (style-to-font-traits face)
                   (declare (ignore others)) ; for now…
                   ;; (print (list name size mode traits others color))
                   (list (awrap [NSFontDescriptor
                                 fontDescriptorWithFontAttributes:
                                 (unwrap-plist
                                  (list
                                   NSFontAttributeName            [NSFont fontWithName: (objcl:objc-string name)
                                                                          size:(cgfloat size)]
                                   NSFontNameAttribute            name
                                   NSFontSizeAttribute            (cgfloat size)
                                   NSForegroundColorAttributeName (if (zerop color)
                                                                      (make-color 0 0 0)
                                                                      (error "Color for font not implemented yet."))
                                   NSFontTraitsAttribute          (unwrap-plist
                                                                   (list NSFontSymbolicTrait  (font-traits-to-mask traits)))
                                   ;; NSUnderlineStyleAttributeName  (if (member :underline others) 1 0)
                                   ;; NSShadowAttributeName          (if (member :shadow    others) *default-shadow* nil)
                                   ;; NSStrokeWidthAttributeName     (if (member :outline   others) 3.0f0 0.0f0)
                                   ))])
                         mode
                         size))))))
  (values-list (list* (handle (first (descriptor-cache-descriptor *descriptor-cache*)))
                      (rest (descriptor-cache-descriptor *descriptor-cache*)))))


(defun font-from-codes (ff ms)
  (multiple-value-bind (descriptor mode size) (font-descriptor-from-codes ff ms)
    (values [NSFont fontWithDescriptor:descriptor size:(if size (cgfloat size) (cgfloat 0.0))]
            mode)))



(defun font-codes-string-width (string ff ms &optional (start 0) (end (length string)))
  "
RETURN:         The width in pixels of the substring of STRING from
                START to END using the font specified by FF and MS.

FF:             Font/Face code.

MS:             Mode/Size code.
"
  (check-type start fixnum "a start index in the string")
  (check-type end   fixnum "an end position in the string")
  ;; (format-trace 'font-codes-string-width [(font-descriptor-from-codes ff ms) fontAttributes])
  (let ((string (nsubstring-start-end string start end)))
    (round (nssize-width
            (get-nssize [(objcl:objc-string string)
                         sizeWithAttributes: [(font-descriptor-from-codes ff ms)
                                              fontAttributes]])))))


(defun font-code-draw-string (string ff ms &optional
                              (start 0)
                              (end (length string))
                              color)
  "
DO:             Draw the substring of STRING from START to END using
                the font specified by FF and MS.

FF:             Font/Face code.

MS:             Mode/Size code.
"
  (let* ((string               (nsubstring-start-end string start end))
         (width                (font-codes-string-width string ff ms))
         (pos                  (pen-position (view-pen *current-view*)))
         (*current-font-codes* (list ff ms)))
    (with-fore-color color
      (draw-string (point-h pos) (point-v pos) string))
    (move *current-view* width 0)
    (values string width)))


(defun color->ff-index (color)
  #-(and)
  (if (and color (not (eql color *black-color*)))
      (fred-palette-closest-entry color)
      0)
  (niy color->ff-index color)
  0)


(defun grafport-font-codes-with-color ()
  #-(and)
  (multiple-value-bind (ff ms)(grafport-font-codes)
    (let* ((foo (grafport-fore-color))) ;; 0 is black is 0    
      (if (not (zerop foo))
          (setf ff (logior (logand ff (lognot #xff)) (fred-palette-closest-entry foo))))
      (values ff ms)))
  (current-font-codes))


(defmacro grafport-write-string (string start end &optional ff ms color)
  (let ((vstart (gensym))
        (vend   (gensym))
        (vff    (gensym))
        (vms    (gensym)))
    `(let ((,vstart ,start)
           (,vend   ,end)
           (,vff    ,ff)
           (,vms    ,ms))
       (font-code-draw-string ,string ,vff ,vms ,vstart ,vend ,color))))


(defun draw-string-in-rect (string rect
                            &key
                              (truncation :clipping) (justification :natural) (compress-p nil)
                              (start 0) (end (length string))
                              ff ms color)
  (when (not (and ff ms))
    (multiple-value-setq (ff ms) (grafport-font-codes-with-color)))
  (when color
    (setf ff (logior (logand ff (lognot #xff)) (color->ff-index color))))
  (let ((*current-font-codes* (list ff ms)))
    (draw-text (rect-left rect) (rect-top rect) (rect-width rect) (rect-height rect)
               (nsubstring-start-end string start end)
               truncation justification compress-p)))


(defun string-width (string &optional font-spec)
  "
RETURN:         The width in pixel of the STRING, as if it was
                displayed in the font, size and style of the
                FONT-SPEC.

FONT-SPEC:      If not supplied, the current font is used.
"
  (multiple-value-bind (ff ms) (if font-spec
                                   (multiple-value-call (function font-codes)
                                     font-spec (current-font-codes))
                                   (current-font-codes))
    (font-codes-string-width string ff ms)))



(defun merge-font-codes (old-ff-code old-ms-code ff-code ms-code &optional ff-mask ms-mask)
  "
DO:             The merge-font-codes function merges two font codes.

OLD-FF:         The old font/face code, expressed as a fixnum. A font/
                face code stores the encoded name of the font and its
                face (plain, bold, italic, and so on). If there is no
                old-ff, the value of old-ff is set to 0.

OLD-MS:         The old mode/size code, expressed as a fixnum. A mode/
                size code indicates the font mode (inclusive-or,
                exclusiveor, complemented, and so on) and the font
                size. If there is no old-ms, the value of old-ms is
                set to 0.

FF:             The new font/face code, expressed as a fixnum

MS:             The new mode/size code, expressed as a fixnum.

FF-MASK:        A mask that allows merge-font-codes to look only at
                certain bits of the font/face integer.

MS-MASK:        A mask that allows merge-font-codes to look only at
                certain bits of the mode/size integer.
"
  (values (if ff-mask
              (logior (logand ff-code ff-mask) (logandc2 old-ff-code ff-mask))
              ff-code)
          (if ms-mask
              (logior (logand ms-code ms-mask) (logandc2 old-ms-code  ms-mask))
              ms-code)))



(defgeneric view-font (view)
  (:documentation "
RETURN:         The font specification used for drawing text in the window.

You should not write methods for this function; use VIEW-FONT-CODES
instead.
"))


(defgeneric set-view-font (view font-spec)
  (:documentation "
DO:             Sets the font of view to fontspec.

You should not write methods for this function; use SET-VIEWFONT-CODES
instead.
"))


(defgeneric view-font-codes (view)
  (:documentation "
RETURN:         Two values: the font/face code and mode/size code for
                view’s font.
"))


(defgeneric set-view-font-codes (view ff ms &optional ff-mask ms-mask)
  (:documentation "
DO:             Change the view font codes of view.  The font/face
                code is changed only in the bits that are set in
                FF-MASK.  The mode/size code is changed only in the
                bits that are set in MS-MASK.  These masks default to
                passing all bits of FF and MS.
"))



(defclass font (wrapper)
  ((specification :initarg :specification :accessor font-specification)))

(defmethod wrap ((nsfont ns:ns-font))
  (make-instance 'font :handle nsfont))


(defun initialize/font ()
  ;; Font Attributes
  (setf NSFontFamilyAttribute            (objcl:lisp-string #$NSFontFamilyAttribute)
        NSFontNameAttribute              (objcl:lisp-string #$NSFontNameAttribute)
        NSFontFaceAttribute              (objcl:lisp-string #$NSFontFaceAttribute)
        NSFontSizeAttribute              (objcl:lisp-string #$NSFontSizeAttribute)
        NSFontVisibleNameAttribute       (objcl:lisp-string #$NSFontVisibleNameAttribute)
        NSForegroundColorAttributeName   (objcl:lisp-string #$NSForegroundColorAttributeName)
        NSFontMatrixAttribute            (objcl:lisp-string #$NSFontMatrixAttribute)
        NSFontVariationAttribute         (objcl:lisp-string #$NSFontVariationAttribute)
        NSFontCharacterSetAttribute      (objcl:lisp-string #$NSFontCharacterSetAttribute)
        NSFontCascadeListAttribute       (objcl:lisp-string #$NSFontCascadeListAttribute)
        NSFontTraitsAttribute            (objcl:lisp-string #$NSFontTraitsAttribute)
        NSFontFixedAdvanceAttribute      (objcl:lisp-string #$NSFontFixedAdvanceAttribute)
        NSFontFeatureSettingsAttribute   (objcl:lisp-string #$NSFontFeatureSettingsAttribute))
  ;; Font Traits Attributes
  (setf NSFontSymbolicTrait              (objcl:lisp-string #$NSFontSymbolicTrait)
        NSFontWeightTrait                (objcl:lisp-string #$NSFontWeightTrait)
        NSFontWidthTrait                 (objcl:lisp-string #$NSFontWidthTrait)
        NSFontSlantTrait                 (objcl:lisp-string #$NSFontSlantTrait))
  ;; NSAttributedString Attributes:
  (setf NSUnderlineStyleAttributeName  (objcl:lisp-string #$NSUnderlineStyleAttributeName)
        NSShadowAttributeName          (objcl:lisp-string #$NSShadowAttributeName)
        NSStrokeWidthAttributeName     (objcl:lisp-string #$NSStrokeWidthAttributeName)
        NSFontAttributeName            (objcl:lisp-string #$NSFontAttributeName))
  ;; -- 
  (setf *default-shadow* (make-shadow :blur-radius 2.0d0 :offset (make-nssize :width 2.0f0 :height 2.0f0)))
  (setf *font-traits*
        `((,#$NSItalicFontMask                  . :italic)
          (,#$NSBoldFontMask                    . :bold)
          (,#$NSUnboldFontMask                  . :unbold)
          (,#$NSNonStandardCharacterSetFontMask . :non-standard-character-set)
          (,#$NSNarrowFontMask                  . :narrow)
          (,#$NSExpandedFontMask                . :expanded)
          (,#$NSCondensedFontMask               . :condensed)
          (,#$NSSmallCapsFontMask               . :small-caps)
          (,#$NSPosterFontMask                  . :poster)
          (,#$NSCompressedFontMask              . :compressed)
          (,#$NSFixedPitchFontMask              . :fixed-pitch)
          (,#$NSUnitalicFontMask                . :unitalic)))
  (setf *font-list*     '()
        *font-families* '()
        *font-list*     (sort (available-font-families) (function string<))
        *font-families* (mapcar (lambda (family) (cons family (available-members-of-font-family family)))
                                *font-list*))
  (setf *current-font-codes* (list 0 0))
  (values))

;; (initialize/font)
;; (multiple-value-bind (ff ms) (font-codes  '("Times" 32)
;;                                           #-(and)
;;                                           '("Times" 12  :bold :italic :underline
;;                                            :outline :shadow :condense :extend))
;;   [(font-descriptor-from-codes ff ms) fontAttributes])
;; (string-width "Hello World! Hello World!" '("American Typewriter" 12))



;;;; THE END ;;;;
