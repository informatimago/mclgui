;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               color.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Colors.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-05-15 <PJB> Extracted from menu.
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
(objcl:enable-objcl-reader-macros)


(defstruct (color
            (:constructor %make-color)
            (:conc-name %color-))
  red green blue (alpha 1.0f0))


(defun make-color (red green blue &optional (alpha 1.0f0))
  "
RETURN:         An encoded color, with components red, green, and
                blue.  The components should be in the range 0–65535.
                Each component is stored with an accuracy of ±255.

RED:            The red component of the color. This should be an
                integer in the range 0–65535.

GREEN:          The green component of the color. This should be an
                integer in the range 0–65535.

BLUE:           The blue component of the color. This should be an
                integer in the range 0–65535.
"
  (%make-color :red   (coerce (/ red   65535.0f0) 'single-float)
               :green (coerce (/ green 65535.0f0) 'single-float)
               :blue  (coerce (/ blue  65535.0f0) 'single-float)
               :alpha (coerce alpha 'single-float)))


(defun color-red   (color)
  "
RETURN:         the red component of color as an integer in the range
                0–65535.
"
  (truncate (%color-red   color) 1/65535))


(defun color-green (color)
  "
RETURN:         the green component of color as an integer in the range
                0–65535.
"
  (truncate (%color-green color) 1/65535))


(defun color-blue  (color)
  "
RETURN:         the blue component of color as an integer in the range
                0–65535.
"
  (truncate (%color-blue  color) 1/65535))


(defun color-alpha   (color)
  "
RETURN:         the alpha component of color as an integer in the range
                0–65535.
"
  (truncate (%color-alpha   color) 1/65535))


(defun color-values (color)
  "
RETURN:         three values corresponding to the red, green, and blue
                components of color.
"
  (values (color-red   color)
          (color-green color)
          (color-blue  color)))


(defun real-color-equal (color1 color2)
  "
The REAL-COLOR-EQUAL function returns true if color1 and color2 are
displayed as the same color on the current display device.  Otherwise
it returns NIL.

This function may return different results for the same arguments,
depending on the current configuration of the computer running
Macintosh Common Lisp. For information on the algorithm used to map
RGB colors into Macintosh color-table entries, see Inside Macintosh.
"
  (and (= (color-red   color1) (color-red   color2))
       (= (color-green color1) (color-green color2))
       (= (color-blue  color1) (color-blue  color2))))


(defmethod wrap ((nscolor ns:ns-color))
  ;; (format-trace 'wrap nscolor)
  (make-color [nscolor redComponent]
              [nscolor greenComponent]
              [nscolor blueComponent]
              [nscolor alphaComponent]))

(defmethod unwrap ((self color))
  (unwrapping self
    [NSColor colorWithCalibratedRed: (cgfloat (%color-red self))
             green: (cgfloat (%color-green self))
             blue:  (cgfloat (%color-blue self))
             alpha: (cgfloat (%color-alpha self))]))



(defgeneric set-fore-color (window color)
  (:documentation "
The SET-FORE-COLOR generic function sets the foreground color of the
window to color and returns NIL.  Future drawing in the window appears
in this color; when the window is redrawn, all drawing appears in this
color.

WINDOW:         A window.

COLOR:          A color.
"))


(defgeneric set-back-color (window color &optional redisplay-p)
  (:documentation "
The SET-BACK-COLOR generic function sets the background color of the
window to color and returns nil.

WINDOW:         A window.

COLOR:          A color.

REDISPLAY-P:    If the value of this is true (the default), this
                function invalidates the window, forcing a redrawing.
                The displayed background color does not change unless
                the window is redrawn.
"))


(defgeneric get-fore-color (window)
  (:documentation "

"))

(defgeneric get-back-color (window)
  (:documentation ""))


(defun %set-fore-color (color)
  (let ((fore-color (unwrap color)
                    #-(and) [NSColor colorWithCalibratedRed: (color-red color)
                                     green: (color-green color)
                                     blue: (color-blue color)
                                     alpha: (color-alpha color)]))
    ;; PJB-DEBUG (format-trace '%set-fore-color color fore-color)
    [fore-color set]
    [fore-color setFill]
    [fore-color setStroke]))

(defun %set-back-color (window color)
  (let ((back-color (unwrap color)))
    ;; PJB-DEBUG (format-trace '%set-back-color color back-color)
    (with-handle (winh window)
      [winh setBackgroundColor:back-color])))


(defun call-with-fore-color (color thunk)
  (let ((window (and *current-view* (view-window *current-view*))))
    (if (or (null color)
            (not *color-available*)
            (null window))
        (funcall thunk)
        (let ((*foreground-color* color)
              (old-fore-color (slot-value window 'fore-color)))
          (with-saved-graphic-state
            (%set-fore-color color)
            (unwind-protect
                 (progn
                   (setf (slot-value window 'fore-color) *foreground-color*)
                   (funcall thunk))
              (setf (slot-value window 'fore-color) old-fore-color)))))))


(defun call-with-back-color (color thunk)
  (let ((window (and *current-view* (view-window *current-view*))))
    (if (or (null color)
            (not *color-available*)
            (null window))
        (funcall thunk)
        (let ((*background-color* color)
              (old-back-color (slot-value window 'back-color)))
          (%set-back-color window color)
          (unwind-protect
               (progn
                 (setf (slot-value (view-window *current-view*) 'back-color) *background-color*)
                 (funcall thunk))
            (setf (slot-value (view-window *current-view*) 'back-color) old-back-color))))))


(defun call-with-fore-and-back-color (fore back thunk)
  (let ((window (and *current-view* (view-window *current-view*))))
    (cond
      ((or (and (null fore) (null back))
           (not *color-available*)
           (null window))
       (funcall thunk))
      ((null fore) (call-with-back-color back thunk))
      ((null back) (call-with-fore-color fore thunk))
      (t (let ((*foreground-color* fore)
               (*background-color* back)
               (old-fore-color (slot-value window 'fore-color))
               (old-back-color (slot-value window 'back-color)))
           (with-saved-graphic-state
             (%set-fore-color fore)
             (unwind-protect
                  (progn
                    (%set-back-color window back)
                    (setf (slot-value (view-window *current-view*) 'fore-color) *foreground-color*
                          (slot-value (view-window *current-view*) 'back-color) *background-color*)
                    (funcall thunk))
               (%set-back-color window (or old-back-color *background-color*))
               (setf (slot-value window 'fore-color) old-fore-color
                     (slot-value window 'back-color) old-back-color))))))))


(defmacro with-fore-color (color &body body)
  `(call-with-fore-color ,color (lambda () ,@body)))

(defmacro with-back-color (color &body body)
  `(call-with-back-color ,color (lambda () ,@body)))

(defmacro with-fore-and-back-color (fore back &body body)
  `(call-with-fore-and-back-color ,fore ,back (lambda () ,@body)))


;;;---------------------------------------------------------------------
;;;
;;; colored mixin class
;;;

(defclass colored ()
  ((color-list :initform '()
               :documentation "The property-list of key and colors for all the parts of the thing."
               :reader part-color-list))
  (:documentation "A mix-in for colored things."))



(defgeneric part-color (thing key)
  (:documentation "
RETURN:         The color of the part KEY of the THING.
")
  (:method ((thing t) key)
    (declare (ignore key))
    nil)
  (:method ((thing colored) key)
    (getf (slot-value thing 'color-list) key nil)))


(defgeneric set-part-color (thing key new-color)
  (:documentation "
DO:             Sets the color of the part KEY of the THING to NEW-COLOR,
                or resets it if NEW-COLOR is NIL.
")
  (:method ((thing t) key new-color)
    (declare (ignore key))
    new-color)
  (:method ((thing colored) key new-color)
    (if new-color
        (setf (getf (slot-value thing 'color-list) key) new-color)
        (remf (slot-value thing 'color-list) key))
    new-color))


(defgeneric color-parts (thing)
  (:documentation "
RETURN:         A list of key parts that can be colored in the THING.
")
  (:method ((thing t))
    '()))


(defgeneric set-part-color-loop (part colors)
  (:method (part colors)
    (loop
      :for (key color) :on colors :by (function cddr)
      :do (set-part-color part key color))))




(defparameter *menubar-color-part-alist*
  '((:default-menu-title      . 4)
    (:default-menu-background . 10)
    (:default-item-title      . 16)
    (:menubar                 . 22)))

(defparameter *menu-color-part-alist*
  '((:menu-title         . 4)
    (:default-item-title . 16)
    (:menu-background    . 22)))

(defparameter *menu-item-color-part-alist*
  '((:item-mark  . 4)
    (:item-title . 10)
    (:item-key   . 16)))

(defvar *window-color-part-alist*
  ;; '((:content   . #.#$wContentColor)
  ;;   (:frame     . #.#$wFrameColor)
  ;;   (:text      . #.#$wTextColor)
  ;;   (:hilite    . #.#$wHiliteColor)
  ;;   (:title-bar . #.#$wTitleBarColor))
  nil)

(defvar *control-color-part-alist*
  ;; '((:frame . #.#$cFrameColor)
  ;;   (:body  . #.#$cBodyColor)
  ;;   (:text  . #.#$cTextColor)
  ;;   (:thumb . #.#$cThumbColor))
  nil)




(defun initialize/color ()
  (setf *black-color*        (make-color 0 0 0)
        *white-color*        (make-color 65280 65280 65280) 
        *pink-color*         (make-color 61952 2048 33792) 
        *red-color*          (make-color 56576 2048 1536) 
        *orange-color*       (make-color 65280 25600 512) 
        *yellow-color*       (make-color 64512 62208 1280) 
        *green-color*        (make-color 7936 46848 5120) 
        *dark-green-color*   (make-color 0 25600 4352) 
        *light-blue-color*   (make-color 512 43776 59904) 
        *blue-color*         (make-color 0 0 54272) 
        *purple-color*       (make-color 17920 0 42240) 
        *brown-color*        (make-color 22016 11264 1280) 
        *tan-color*          (make-color 36864 28928 14848) 
        *gray-color*         (make-color 32768 32768 32768) 
        *light-gray-color*   (make-color 49152 49152 49152) 
        *lighter-gray-color* (make-color 56576 56576 56576) 
        *dark-gray-color*    (make-color 16384 16384 16384)
        *tool-back-color*    *yellow-color* ; what is this?
        *background-color*   *white-color*
        *foreground-color*   *black-color*)) 

(defgeneric color-or-gray-p (item)
  (:method (item)
    (declare (ignore item))
    nil))

(defgeneric window-color-p (item)
  (:method (item)
    (declare (ignore item))
    nil))

;;;; THE END ;;;;
