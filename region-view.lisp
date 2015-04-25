;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               region-view.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    View specific Region operations.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2015-04-25 <PJB> Extracted from region.lisp
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


(defgeneric window-open-region (window)
  (:documentation "RETURN: NIL or the open region of the window"))
(defgeneric (setf window-open-region) (new-region window)
  (:documentation "DO: Sets the open-region of the window.
NEW-REGION: a region or NIL."))



(defmacro with-hilite-mode (&body body)
  (niy with-hilite-mode body)
  `(progn
     (niy with-hilite-mode ',body)
     ,@body)
  #-(and)
  `(progn
     (let ((byte (require-trap #_lmgethilitemode)))
       (require-trap #_lmsethilitemode (%ilogand2 #x7f byte)))
     ,@body))




(defmacro with-clip-region (region &body body)
  (let ((saved-clip (gensym))
        (clip       (gensym)))
    `(let ((,saved-clip (get-clip (new-region))))
       (unwind-protect
            (let ((,clip (intersect-region ,saved-clip ,region)))
              (set-clip ,clip)
              (print ,clip)
              ,@body)
         (set-clip ,saved-clip)))))

(defmacro with-clip-rect-intersect (rect &body body)
  `(with-clip-region (rect-region ,rect) ,@body))



#-(and)
(defmacro with-clip-rect-intersect (rect &rest body)
  (let ((old (gensym))
        (new (gensym)))
    `(with-temp-rgns (,old ,new)
       (get-clip ,old)
       (set-rect-region ,new ,rect)
       (intersect-region ,old ,new ,new)
       (set-clip ,new)
       (unwind-protect
            (progn ,@body)
         (set-clip ,old)))))




(defgeneric open-region (view)
  (:documentation "
The OPEN-REGION generic function hides the pen and begins recording a
region.  Subsequent drawing commands to the window add to the region.
Recording ends when CLOSE-REGION is called. The function returns NIL.
It is an error to call OPEN-REGION a second time without first calling
CLOSE-REGION.

VIEW:           A window or a view contained in a window.
")
  #-(and) "

OpenRgn tells QuickDraw to allocate temporary space and start saving
lines and framed shapes for later processing as a region
definition. While a region is open, all calls to Line, LineTo, and the
procedures that draw framed shapes (except arcs) affect the outline of
the region. Only the line endpoints and shape boundaries affect the
region definition; the pen mode, pattern, and size do not affect
it. In fact, OpenRgn calls HidePen, so no drawing occurs on the screen
while the region is open (unless you called ShowPen just after
OpenRgn, or you called ShowPen previously without balancing it by a
call to HidePen). Since the pen hangs below and to the right of the
pen location, drawing lines with even the smallest pen will change
bits that lie outside the region you define.

The outline of a region is mathematically defined and infinitely thin,
and separates the bit image into two groups of bits: Those within the
region and those outside it. A region should consist of one or more
closed loops. Each framed shape itself constitutes a loop. Any lines
drawn with Line or LineTo should connect with each other or with a
framed shape. Even though the on screen presentation of a region is
clipped, the definition of a region is not; you can define a region
anywhere on the coordinate plane with complete disregard for the
location of various grafPort entities on that plane.

When a region is open, the current grafPort's rgnSave field contains a
handle to information related to the region definition. If you want to
temporarily disable the collection of lines and shapes, you can save
the current value of this field, set the field to NIL, and later
restore the saved value to resume the region definition.  Also,
calling SetPort while a regionis being formed will discontinue
formation of the region until another call to SetPort resets the
region's original grafPort.

Warning: Do not call OpenRgn while another region or polygon is
already open. All open regions but the most recent will behave
strangely.

Note: Regions are limited to 32K bytes. You can determine the current
size of a region by calling the Memory Manager function GetHandleSize.

"
  (:method ((view simple-view))
    (when (window-open-region (view-window view))
      (error "Cannot call ~S twice in a row before calling ~S."
             'open-region 'close-region))
    (niy open-region view)
    (pen-hide view)
    (setf (window-open-region (view-window view)) (new-region))))


(defgeneric close-region (view &optional dest-region)
  (:documentation "

The CLOSE-REGION generic function shows the pen and returns a region
that is the accumulation of drawing commands in the window since the
last open-region for the window.  It returns the result in
DEST-REGION, if supplied, or else in a newly created region.  It is an
error to call CLOSE-REGION before OPEN-REGION has been called.  Note
that if a new region is created, you must dispose of it explicitly to
reclaim its storage space.

VIEW:           A window or a view contained in a window.

DEST-REGION:    A region.
")
  #-(and) "

CloseRgn stops the collection of lines and framed shapes, organizes
them into a region definition, and saves the resulting region in the
region indicated by dstRgn. CloseRgn does not create the destination
region; space must already have been allocated for it. You should
perform one and only one CloseRgn for every OpenRgn. CloseRgn calls
ShowPen, balancing the HidePen call made by OpenRgn.

Here's an example of how to create and open a region, define a barbell
shape, close the region, draw it, and dispose of it:

    barbell := NewRgn;                   {create a new region}
    OpenRgn;                             {begin collecting stuff}
        SetRect(tempRect,20,20,30,50);   {form the left weight}
        FrameOval(tempRect);
        SetRect(tempRect,25,30,85,40);   {form the bar}
        FrameRect(tempRect);
        SetRect(tempRect,80,20,90,50);   {form the right weight}
        FrameOval(tempRect);
    CloseRgn(barbell);                   {we're done; save in barbell}
    FillRgn(barbell,black);              {draw it on the screen}
    DisposeRgn(barbell);                 {dispose of the region}

"
  (:method ((view simple-view) &optional dest-region)
    (unless (window-open-region (view-window view))
      (error "Cannot call ~S without calling ~S before."
             'close-region 'open-region)) 
    (niy close-region view dest-region)
    (prog1 (if dest-region
             (copy-region (window-open-region (view-window view)) dest-region)
             (window-open-region (view-window view)))
      (setf (window-open-region (view-window view)) nil)
      (pen-show view))))

;;;; THE END ;;;;
