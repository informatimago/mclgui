;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               window.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Implement the window class.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-05-13 <PJB> Created.
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
(in-package "MCLGUI")
(mclgui.readtable:enable-objcl+ccl-reader-macros)
(enable-sharp-at-reader-macro)


(defvar *window-list* '()
  "A list of window instances.")


;; (initialize/window)

(defgeneric window-frame-from-nswindow-frame (window))
(defgeneric nswindow-frame-from-window-frame (window))

(defgeneric window-event (window)
  (:documentation "
The WINDOW-EVENT generic function is called by EVENT-DISPATCH to get a
window to handle an event.  This function is called only when the
event system determines the appropriate window.  The method of
WINDOW-EVENT for WINDOW checks the type of the event and calls the
appropriate event handler.  The WINDOW-EVENT function should be
specialized in windows that need to do something in addition to or
different from the default behavior for many types of events.

WINDOW:         A window.
"))


(defmethod window-frame-from-nswindow-frame ((frame nsrect))
  "
FRAME:  A NSRECT (in screen coordinates).
RETURN: a RECT (in flipped \"screen coordinates\").
"
  (multiple-value-bind (sx sy sw sh) (main-screen-frame)
    (declare (ignore sw))
    (make-rect (- (nsrect-x frame)                           sx) ; left
               (- (+ sy sh)   (+ (nsrect-y frame) (nsrect-height frame))) ; top
               (- (+ (nsrect-x frame) (nsrect-width frame))  sx) ; right
               (- (+ sy sh)   (nsrect-y frame)))))

(defmethod window-frame-from-nswindow-frame ((window window))
  "
RETURN: a RECT containing the position and size of the window,
        computed from the NSWindow frame.
"
  (with-handle (winh window)
    (let ((frame (get-nsrect [winh contentRectForFrameRect:[winh frame]])))
      (window-frame-from-nswindow-frame frame))))



(defmethod nswindow-frame-from-window-frame ((frame rect))
  "
RETURN: A NSRect containing the frame of the window.
"
  (multiple-value-bind (sx sy sw sh) (main-screen-frame)
    (declare (ignore sw))
    (let ((position (rect-topleft frame))
          (size     (rect-size frame)))
      (ns:make-ns-rect (+ sx (point-h position))
                       (- (+ sy sh) (+ (point-v position) (point-v size)))
                       (point-h size)
                       (point-v size)))))

(defmethod nswindow-frame-from-window-frame ((window window))
  "
RETURN: A NSRect containing the frame of the window, compute from the position and size.
"
  (with-handle (winh window)
    (get-nsrect [winh frameRectForContentRect:(nswindow-frame-from-window-frame (view-frame window))])))




(defmethod update-handle ((window window))
  (setf (%view-position window) (center-window (view-size window) (view-position window)))
  (setf (view-valid window) (list nil))
  (view-allocate-clip-region window)
  (when (and (slot-value window 'erase-anonymous-invalidations)
             (not (slot-value window 'theme-background)))
    ;; only needed for non-theme color background
    (setf (window-invalid-region window) (new-rgn)))
  (add-new-to-list *window-list* window)
  ;;#+debug-views
  (format-trace '(update-handle window)
                :pos (point-to-list (view-position window))
                :siz (point-to-list (view-size window))
                :frame (rect-to-list (view-frame window))
                :content-rect (nswindow-frame-from-window-frame (view-frame window)))
  (let ((winh [[MclguiWindow alloc]
               initWithContentRect:(nswindow-frame-from-window-frame (view-frame window))
               styleMask:(ecase (window-type window)
                           ((:document)
                            (logior #$NSTitledWindowMask
                                    #$NSMiniaturizableWindowMask
                                    (if (window-close-box-p window)
                                        #$NSClosableWindowMask
                                        0)))
                           ((:document-with-zoom
                             :document-with-grow)
                            (logior #$NSTitledWindowMask
                                    #$NSMiniaturizableWindowMask
                                    (if (window-close-box-p window)
                                        #$NSClosableWindowMask
                                        0)
                                    #$NSResizableWindowMask))
                           ((:double-edge-box
                             :single-edge-box
                             :shadow-edge-box)
                            #$NSBorderlessWindowMask)
                           ((:tool)
                            (logior #$NSTitledWindowMask
                                    (if (window-close-box-p window)
                                        #$NSClosableWindowMask
                                        0))))
               backing:#$NSBackingStoreBuffered
               defer:NO]))
    (setf (slot-value winh 'window) window)
    (setf (handle window) winh) ; must be done before setDelegate.
    #+debug-views
    (format-trace '(update-handle window)
                  :cview-frame  (nswindow-frame-from-window-frame window))
    (let ((cviewh [[MclguiView alloc]
                   initWithFrame:(unwrap (nswindow-frame-from-window-frame window))]))
      (setf (slot-value cviewh 'view) window)
      [cviewh setAutoresizingMask:(logior #$NSViewWidthSizable #$NSViewHeightSizable)]
      [winh setContentView:cviewh] window)
    [winh setReleasedWhenClosed:YES]
    [winh setHasShadow:yes]
    [winh invalidateShadow]
    [winh setAcceptsMouseMovedEvents:YES]
    ;; [winh setDelegate:(make-instance 'mclgui-window-delegate :window window)]
    [winh setDelegate:winh]
    [winh setTitle:(objcl:objc-string (window-title window))]
    ;; (setf (window-affine-transform window) (current-affine-transform window))
    #+debug-views
    (format-trace '(update-handle window)
                  :wintitle (window-title window)
                  :pos (point-to-list (view-position window))
                  :size (point-to-list (view-size window))
                  :nsframe (nswindow-frame-from-window-frame window))
    winh))



(defmethod initialize-instance ((window window)
                                &key (view-font (view-default-font window))
                                &allow-other-keys)
  ;; to set the view-font.
  (declare (ignorable view-font))
  (call-next-method))


(defmethod initialize-instance :after ((window window) &key &allow-other-keys)
  (with-handle (winh window)
    (let* ((bounds (get-nsrect [[winh contentView] bounds]))
           (ori    (subtract-points #@(0 0) (rect-topleft (nsrect-to-rect bounds))))
           (posiz  (window-frame-from-nswindow-frame window)))
      (setf (slot-value window 'window-title)         (objcl:lisp-string [winh title])
            (slot-value window 'view-position)        (rect-topleft posiz)
            (slot-value window 'view-size)            (rect-size posiz)
            (slot-value window 'view-scroll-position) ori)))
  (window-size-parts window)
  (when (window-visiblep window)
    (setf (slot-value window 'visiblep) nil)
    (window-show window)
    [(handle window) display]))


(defmethod view-allocate-clip-region ((window window))
  ;; Note: It's important here that a view's WPTR gets set before its subview's
  ;; WPTR's and that it gets cleared (set to NIL) after its subview's WPTR's.
  ;; Otherwise (:method (setf wptr) (t dialog-item)) won't work.
  (let ((rgn (view-clip-region-slot window)))
    (or rgn (setf (view-clip-region-slot window) (new-region)))))

(defmethod get-window-visrgn ((window window) (region region))
  (set-rect-region region (view-bounds window)))

(defmethod window-visrgn ((window window))
  (get-window-visrgn window (new-region)))


;;; get-clip, set-clip (and clip-rect) are quickdraw functions.

(defun get-clip (region)
  (let ((win (and *current-view* (view-window *current-view*))))
    (when win
      (unless (view-clip-region-slot win)
        (setf (view-clip-region-slot win)
              (rect-region -32767 -32767 32767 32767)))
      (copy-region (view-clip-region-slot win) region))))

(defun %set-clip (win region)
  "
WIN:        A WINDOW.
REGION:     A REGION in the WIN coordinate systems.
POST:       (eql (view-clip-region-slot win) region)
            and the window clip region is set to REGION.
RETURN:     REGION.
"
  #+debug-views (format-trace "%set-clip" region)
  ;; #+debug-views (let ((*allow-print-backtrace* t)) (print-backtrace))
  (setf (view-clip-region-slot win) region)
  #-(and)
  (let ((path (bezier-path-from-region region))
        (saved-view *current-view*))
    (unwind-protect
         (progn
           (focus-view win)
           [path setClip]
           #-(and) (progn #| DEBUG-PJB |#
                     [path setLineWidth:(cgfloat 4.0)]
                     [(unwrap *gray-color*) set]
                     [path stroke]))
      (refocus-view saved-view)))
  region)

(defun set-clip (region)
  (let ((win (and *current-view* (view-window *current-view*))))
    (when win
      (%set-clip win
                 (if region
                     (copy-region region (or (view-clip-region-slot win) (new-region)))
                     (set-rect-region    (or (view-clip-region-slot win) (new-region))
                                         -32767 -32767 32767 32767))))))



(defmethod clip-region ((view simple-view) &optional (save-region (new-region)))
  ;; returns the WINDOW’s current clip region.
  (let ((*current-view* view))
    (get-clip save-region)))

(defmethod set-clip-region ((view simple-view) new-region)
  ;; sets the WINDOW’s clip region to NEW-REGION and returns
  ;; NEW-REGION.
  (let ((*current-view* view))
    (set-clip new-region)))

(defmethod clip-rect ((view simple-view) left &optional top right bottom)
  ;; makes the window’s clip region a rectangular region equivalent to
  ;; the rectangle determined by the arguments.  It returns NIL.
  (with-rectangle-arg (r left top right bottom)
    (set-clip-region view (rect-region left top right bottom)))
  nil)






(defgeneric view-window (view)
  (:documentation "
RETURN:         The window containing VIEW, or NIL if the view is not
                contained in a window.  If VIEW is a window,
                VIEW-WINDOW returns the window.

VIEW:           A simple view or subclass of simple-view.
")
  (:method ((view simple-view))
    (loop
      :for w = view :then (view-container w)
      :until (or (null w) (typep w 'window))
      :finally (return w))))


(defun windows (&key (class 'window) include-invisibles include-windoids)
  "
RETURN:         A list of existing windows that are instances
                of CLASS.  The list is ordered from front to back.

CLASS:          A class used to filter output.  Only windows that match
                the value of CLASS are included in the returned list.  The
                default is WINDOW, which includes all windows.

INCLUDE-INVISIBLES
                If the value of this variable is true, invisible
                windows are included in the list. If NIL (the
                default), invisible windows are not included.

INCLUDE-WINDOIDS
                If the value of this variable is true, floating
                windows (the class WINDOID) are included in the
                list.  If NIL (the default), floating windows are not
                included.  Floating windows are also included if the
                value of the CLASS argument is WINDOID.
"
  ;; We don't care about the array, just that the window instances be updated:
  (wrap [[NSApplication sharedApplication] orderedWindows])
  (delete-if (lambda (window)
               (not (and (if include-windoids
                             (or (typep window class)
                                 (typep window 'windoid))
                             (typep window class))
                         (or include-invisibles
                             (window-visiblep window)))))
             (copy-list *window-list*)))



(defun front-window (&key (class 'window) include-invisibles include-windoids)
  "
RETURN:         The frontmost window satisfying the arguments. If no
                windows satisfy the tests, NIL is returned.

CLASS:          A class used to filter output.  Only windows that match
                the value of CLASS are included in the returned list.  The
                default is WINDOW, which includes all windows.

INCLUDE-INVISIBLES
                If the value of this variable is true, invisible
                windows are included in the list. If NIL (the
                default), invisible windows are not included.

INCLUDE-WINDOIDS
                If the value of this variable is true, floating
                windows (the class WINDOID) are included in the
                list.  If NIL (the default), floating windows are not
                included.  Floating windows are also included if the
                value of the CLASS argument is WINDOID.
"
  (first (windows :class class
                  :include-invisibles include-invisibles
                  :include-windoids   include-windoids)))


(defun target ()
  "
RETURN:         the second window on the list of windows; it is
                equivalent to (second (windows)).
"
  (second (windows)))


(defun map-windows (function &key (class 'window) include-invisibles include-windoids)
  "
DO:             Call FUNCTION, a function of one argument, on each
                window that satisfies the keywords.

FUNCTION:       A function of one argument.

CLASS:          A class used to filter output.  Only windows that match
                the value of CLASS are included in the returned list.  The
                default is WINDOW, which includes all windows.

INCLUDE-INVISIBLES
                If the value of this variable is true, invisible
                windows are included in the list. If NIL (the
                default), invisible windows are not included.

INCLUDE-WINDOIDS
                If the value of this variable is true, floating
                windows (the class WINDOID) are included in the
                list.  If NIL (the default), floating windows are not
                included.  Floating windows are also included if the
                value of the CLASS argument is WINDOID.
"
  (dolist (w (windows :class class
                      :include-invisibles include-invisibles
                      :include-windoids include-windoids)
             nil)
    (funcall function w)))


(defun find-window (title &optional (class 'window))
  "
RETURN:         The frontmost window of the class CLASS for which a
                prefix of the window’s title is string-equal to
                TITLE.  If no window has TITLE as its title, NIL is
                returned.  (The cross that appears in the title bar of
                modified Fred windows is ignored when comparing the
                title.)

TITLE:          A string specifying the title of the window to search
                for.

CLASS:          A class used to filter the result. The frontmost
                window that inherits from class is returned. The
                default is WINDOW.
"
  (map-windows (lambda (w)
                 (if (string-equal (window-title w) title)
                     (return-from find-window w)))
               :class class
               :include-windoids t
               :include-invisibles t)
  nil)


(defgeneric window-close (window)
  (:documentation "
The WINDOW-CLOSE generic function closes the window.  The associated
Macintosh data structures will be deallocated the next time the garbage
collector runs.  This operation is the inverse of INITIALIZE-INSTANCE.
When a window is closed, its state is lost and cannot be recovered.
The MCL event system calls WINDOW-CLOSE when the user clicks a window’s
close box or chooses Close from the File menu.
")
  (:method ((window window))
    (window-hide window)
    (with-handle (winh window)
      (close-nswindow winh))
    (delete-from-list *window-list* window)
    (release window)
    (values)))



;; (defmethod view-position ((w window) &aux (wptr (wptr w)))
;;   (rlet ((rect :rect))
;;     (#_getwindowportbounds wptr rect)
;;     (local-to-global wptr (pref rect :rect.topleft))))


(defun center-window (size position)
  (if (numberp position)
      position
      (let ((pos-h (truncate (- *screen-width*  (point-h size)) 2))
            (pos-v (truncate (- *screen-height* (point-v size)) 2)))
        (cond ((eql position :centered)
               (make-point pos-h pos-v))
              ((atom position)
               (error 'simple-type-error
                      :datum position
                      :expected-type '(or (member :centered) cons)
                      :format-control "The position should be either :CENTERED or a list (CONSTRAINT [AMOUNT]) instead of ~S"
                      :format-arguments (list position)))
              (t (let ((constraint (pop position))
                       (amount (or (pop position) 0)))
                   (case constraint
                     (:top
                      (make-point pos-h amount))
                     (:bottom
                      (make-point pos-h (- *screen-height* amount (point-v size))))
                     (:left
                      (make-point amount pos-v))
                     (:right
                      (make-point (- *screen-width* amount (point-h size)) pos-v))
                     (otherwise
                      (error 'simple-type-error
                             :datum constraint
                             :expected-type '(member :top :bottom :left :right)
                             :format-control "The constraint should be one of ~{~S~^, ~} instead of ~S"
                             :format-arguments (list '(:top :bottom :left :right) constraint))))))))))


(defvar *window-moving* nil
  "Disable calling the handle in SET-VIEW-POSITION.")

(defmethod set-view-position ((window window) h &optional v)
  "
DO:             Move the window.

RETURN:         The new position of the upper-left corner, expressed
                as a point.  For windows with title bars, such as
                document windows and tool windows, the position is not
                the upper-left corner of the title bar but the
                upper-left corner of the content area of the window.

WINDOW          A window.

H               The horizontal coordinate of the new position, or the
                complete position.

                This may also be a keyword or list specifying how to
                center the window.

                To center a window, specify the new position as the
                keyword :CENTERED. If the position is :CENTERED, the
                window will be centered vertically and horizontally.

                The position may also be a list of the form (reference
                offset), where reference is one of the keywords :top,
                :left, :bottom, or :right, and offset is a number.

                * If reference is :top, the top of the window is offset
                  offset number of pixels from the top of the screen,
                  and the window is centered horizontally.

                * If reference is :bottom, the bottom of the window is
                  offset offset number of pixels from the bottom of the
                  screen, and the window is centered horizontally.

                * If reference is :left, the left side of the window is
                  offset offset number of pixels from the left of the
                  screen, and the window is centered vertically.

                * If reference is :right, the right side of the window
                  is offset offset number of pixels from the right of the
                  screen, and the window is centered vertically.

V:              The vertical coordinate of the new position, or NIL if
                the complete position is given by H.
"
  (if (numberp h)
      (let ((pos      (make-point h v))
            (mswindow (handle window)))
        (setf (%view-position window) pos)
        (when (and (not *window-moving*) mswindow)
          (if [mswindow isVisible]
              (application-eval-enqueue
               *application*
               (lambda ()
                 [mswindow setFrameOrigin:(unwrap (nswindow-frame-from-window-frame window))]
                 [mswindow invalidateShadow]
                 (setf (%view-position window) (rect-topleft (window-frame-from-nswindow-frame window)))))
              (progn
                [mswindow setFrameOrigin:(unwrap (nswindow-frame-from-window-frame window))]
                (setf (%view-position window) (rect-topleft (window-frame-from-nswindow-frame window))))))
        (%view-position window))
      (set-view-position window (center-window (view-size window) h))))


(defvar *window-growing* nil
  "Disable calling the handle in SET-VIEW-SIZE.")

(defmethod set-view-size ((window window) h &optional v)
  (let ((siz      (make-point h v))
        (frame    (window-frame-from-nswindow-frame window))
        (nswindow (handle window)))
    ;; (format-trace '(set-view-size window) (point-to-list siz))
    (setf (%view-size window) siz)
    (if (and nswindow (/= siz (rect-size frame)))
        (flet ((resize (redisplay)
                 (let ((*window-growing* t))
                   [nswindow setFrame:(unwrap (nswindow-frame-from-window-frame window)) display:redisplay]
                   [nswindow invalidateShadow])
                 (let ((frame  (window-frame-from-nswindow-frame window)))
                   (setf (%view-position window) (rect-topleft frame)
                         (%view-size     window) (rect-size frame)))
                 (window-size-parts window)))
          (if [nswindow isVisible]
              (application-eval-enqueue *application* (resize YES))
              (resize NO)))
        (window-size-parts window))
    (refocus-view window)
    (%view-size window)))

(defgeneric window-minimum-size (window)
  (:method ((window window))
    (view-get window 'window-minimum-size #@(0 0))))
(defgeneric window-maximum-size (window)
  (:method ((window window))
    (view-get window 'window-maximum-size (rect-size (main-screen-frame)))))
(defgeneric (setf window-minimum-size) (new-min-size window)
  (:method (new-min-size (window window))
    (view-put window 'window-minimum-size new-min-size)
    (with-handle (winh window)
      [winh setContentMinSize:(nssize new-min-size)])))
(defgeneric (setf window-maximum-size) (new-max-size window)
  (:method (new-max-size (window window))
    (view-put window 'window-maximum-size new-max-size)
    (with-handle (winh window)
      [winh setContentMaxSize:(nssize new-max-size)])))

(defgeneric window-size-parts (w)
  (:documentation "
The WINDOW-SIZE-PARTS generic function can be specialized to resize
the subviews of a window whenever the size of the window is changed.

This function is called directly or indirectly by the methods
specialized on window for the generic functions INITIALIZE-INSTANCE,
SET-VIEW-SIZE, WINDOW-ZOOM-EVENT-HANDLER, and
WINDOW-GROW-EVENT-HANDLER.

The primary method for window does nothing.  The :before method for
WINDOW ensures that the VIEW-CLIP-REGION and VIEW-ORIGIN of each of
the window’s subviews are recomputed the next time they are needed.
The method for FRED-WINDOW resizes the horizontal and vertical scroll
bars as well as the main text area of the window.

WINDOW:    A window or Fred window.
")
  (:method         ((w window))
    (values))
  (:method :before ((w window))
    (make-view-invalid w)
    (invalidate-view w)))



(defmethod view-default-position ((w window))
  *window-default-position*)


(defmethod view-default-size ((w window))
  *window-default-size*)


(defgeneric set-window-title (window new-title)
  (:documentation "
DO:             Set the window title to NEWTITLE.  It ignores the
                crosses in the title bars of modified Fred windows.

WINDOW:         A window.

NEW-TITLE:      A string to be used as the new title.
")
  (:method ((window window) new-title)
    (let ((len (length new-title)))
      (when (< 255 len)
        (error 'view-error
               :view window
               :format-control "Title ~S too long"
               :format-arguments (list new-title)))
      (setf (slot-value window 'window-title) new-title)
      (let ((nswindow (handle window)))
        (when nswindow
          (on-main-thread [nswindow setTitle:(objcl:objc-string new-title)])))
      new-title)))


(defgeneric set-window-filename (window filename)
  (:method ((window window) filename)
    (set-window-title window (file-namestring filename))))



(defmethod view-default-font ((window window))
  *default-font-spec*)


(defgeneric window-show (window)
  (:method ((window window))
    #+debug-views (format-trace 'window-show window)
    (unless (window-visiblep window)
      (setf (slot-value window 'visiblep) t)
      (window-bring-to-front window))
    window))


(defgeneric window-hide (window)
  (:method ((window window))
    (when (window-visiblep window)
      (let ((nswindow (handle window)))
        (when nswindow
          (on-main-thread [nswindow orderOut:nswindow])))
      (setf (slot-value window 'visiblep) nil)
      (when (eql window *selected-window*)
        (window-select (front-window))))
    window))


(defgeneric window-on-screen-position (window)
  (:method ((window window))
    #@(6 44)))

(defgeneric window-on-screen-size (window)
  (:method ((window window))
    #@(502 147)))


(defgeneric window-screen-frame (window)
  (:documentation "RETURN: a list with position and size of the screen of the window.")
  (:method ((window window))
    (with-handle (winh window)
      (multiple-value-bind (x y w h) (frame [[winh screen] frame])
        (list (make-point (round x) (round y))
              (make-point (round w) (round h)))))))


(defgeneric window-on-screen-p (window)
  (:documentation "
RETURN:         Whether all of window is on the screen.
NOTE:           This may return true even if the window is invisible,
                if the window has a screen.
")
  (:method ((window window))
    ;; Note: on MacOS, regions are used for screens.
    (let ((screen-frame (window-screen-frame window)))
      (when screen-frame
        (let ((pos (view-position window))
              (siz (view-size window)))
          (and (point<= (first screen-frame) pos)
               (point<= (add-points pos siz)
                        (add-points (first screen-frame) (second screen-frame)))))))))


(defgeneric window-ensure-on-screen (window &optional default-position default-size)
  (:documentation "

DO:             Ensure that the window is entirely visible on one or
                more of the Macintosh screens. It may overlap two
                screens, but if it is not entirely visible, as
                determined by WINDOW-ON-SCREEN-P, it is moved to the
                position DEFAULT-POSITION. If it is still not entirely
                visible, its size is changed to DEFAULT-SIZE.  This
                function is useful when window positions are saved and
                restored on Macintosh computers with different screen
                configurations.  If you hold down the shift key while
                selecting a window from the Windows menu,
                WINDOW-ENSURE-ON-SCREEN is called on it.

WINDOW:         A window.

DEFAULT-POSITION:
                The position to which the window is moved if it needs
                to be.  The default default-position is the value of
                *WINDOW-DEFAULT-POSITION*.

DEFAULT-SIZE:   The default size of the window. The default default-size is
                the value of *WINDOW-DEFAULT-SIZE*.
")
  (:method ((window window) &optional default-position default-size)
    (unless (window-on-screen-p window)
      (set-view-position window (or default-position (view-default-position window)))
      (unless (window-on-screen-p window)
        (set-view-size window (or default-size (view-default-size window)))
        (unless (window-on-screen-p window)
          (set-view-position window (window-on-screen-position window))
          (unless (window-on-screen-p window)
            (set-view-size window (or default-size (window-on-screen-size window)))))))))


(defun screen-to-window-point (window screen-point)
  "
SCREEN-POINT:  A screen coordinate.
RETURN:        The coordinate of SCREEN-POINT in the WINDOW coordinate system.
"
  (add-points screen-point (subtract-points (view-scroll-position window)
                                            (view-position window))))


(defgeneric window-layer (w &optional include-invisibles)
  (:documentation "
RETURN:         The number of windows in front of window. Floating
                windows are counted.

WINDOW:         A window.

INCLUDE-INVISIBLES:
                A Boolean value specifying whether or not to include
                invisible windows in the count. The default value is NIL,
                indicating that window-layer counts only visible
                windows.
")
  (:method ((window window) &optional include-invisibles)
    (position window (windows :include-invisibles include-invisibles))))


(defgeneric set-window-layer (window new-layer &optional include-invisibles)
  (:documentation "

DO:             Change the layer of the WINDOW to new-layer.  Floating
                windows are counted.  To make a window the frontmost
                window that is not a floating window, set its layer to
                *WINDOID-COUNT*.  You can use SET-WINDOW-LAYER to move
                a regular window in front of a floating window.  Once
                other events occur, however, the floating window moves
                back to the front.

WINDOW:         A window.

NEW-LAYER:      A non-negative integer indicating how many windows
                should be in front of window. If new-layer is equal to
                or greater than the number of windows on screen,
                window is moved all the way to the back. If the value
                of new-layer is 0, window is moved to the front.

INCLUDE-INVISIBLES:
                A variable specifying whether the layering should take
                invisible windows into account. If the value of
                include-invisibles is NIL (the default), invisible
                windows are ignored. If it is true, invisible windows
                are counted.
")
  (:method ((window window) new-layer &optional include-invisibles)
    (let ((handle (handle window)))
      (when handle
        (if (plusp new-layer)
            (let ((other-window
                    (or (elt (windows :include-invisibles include-invisibles) new-layer)
                        (first (last *window-list*)))))
              (window-send-behind window other-window))
            (window-bring-to-front window)
            ;; (if (eql window (first *window-list*))
            ;;     [handle makeKeyAndOrderFront:handle]
            ;;     (let ((other (handle (first *window-list*))))
            ;;       (when other
            ;;         [handle orderWindow:#$NSWindowAbove
            ;;                 relativeTo:[other windowNumber]])))
            )))
    ;; (without-interrupts
    ;;     (let* ((wptr (wptr w)))
    ;;       (when wptr
    ;;         (let* ((visible? #-carbon-compat (rref wptr windowrecord.visible) #+carbon-compat (#_iswindowvisible wptr)))
    ;;           (if (<= new-layer 0)
    ;;               (with-macptrs ((fw #-carbon-compat (#_FrontWindow)
    ;;                                  #+carbon-compat (#_FrontNonFloatingWindow))) ; this is the modal dialog case  - was FrontWindow - fix from Brendan Burns
    ;;                 (unless (%ptr-eql wptr fw)
    ;;                   (window-bring-to-front w wptr)
    ;;                   (when visible?
    ;;                     (unselect-windows t)
    ;;                     (setq *selected-window* w)
    ;;                     (view-activate-event-handler w))))
    ;;               (let ((selected *selected-window*))
    ;;                 (if (set-window-layer-internal
    ;;                      w (max *windoid-count* new-layer) include-invisibles)
    ;;                     (when (eql w selected)
    ;;                       (let ((new-selected (front-window)))
    ;;                         (unless (eql w new-selected)
    ;;                           (view-deactivate-event-handler w)
    ;;                           (setq *selected-window* new-selected)
    ;;                           (view-activate-event-handler new-selected))))
    ;;                     (unless (or (not visible?) (eql w selected))
    ;;                       (view-deactivate-event-handler selected)
    ;;                       (setq *selected-window* w)
    ;;                       (view-activate-event-handler w)))))))))
    ))



(defvar *last-windoid* nil)

(defun window-bring-to-front (window)
  "
DO:             Order the WINDOW above every other.
"
  (delete-from-list *window-list* window)
  (insert-into-list *window-list* 0 window)
  (setf (slot-value window 'visiblep) t)
  (let ((handle (handle window)))
    (when handle
      (on-main-thread/sync [handle makeKeyAndOrderFront:handle])))
  (view-draw-contents window))


(defun window-send-behind (window other-window)
  "
DO:             Order the WINDOW behind the OTHER-WINDOW.
"
  (let ((winh (handle window))
        (wino (handle other-window)))
    (delete-from-list *window-list* window)
    (insert-into-list *window-list*
                      (or (position other-window *window-list*)
                          (length *window-list*))
                      window)
    (when (and winh wino)
      (on-main-thread [winh orderBelow:wino]))))


(defun reselect-windows ()
  "Hilite & activate the windows.
Move windoids to the front.
If *selected-window* is a DA, pick a non-DA to select.
If *selected-window* is nil, leave it that way.
Update *last-windoid* and *windoid-count*
"
  (let ((selected *selected-window*)
        (last-windoid        nil)
        (found-non-windoid?  nil)
        (windoid-count 0))
    (dolist (window *window-list*)
      (when (window-visiblep window)
        (cond ((windoid-p window)
               (when found-non-windoid?
                 (if last-windoid
                     (window-send-behind window last-windoid)
                     (window-bring-to-front window)))
               (setf last-windoid window)
               (unless (window-active-p window)
                 (view-activate-event-handler window))
               (incf windoid-count))
              (t
               (setf found-non-windoid? t)))))
    (setf *windoid-count* windoid-count
          *last-windoid* last-windoid)
    (when *selected-window*           ; maybe nobody is selected
      (setf *selected-window* selected)
      (if last-windoid
          (window-send-behind selected last-windoid )
          (window-bring-to-front selected))
      (unless (window-active-p selected)
        (view-activate-event-handler selected)))))


(defgeneric window-select (window)
  (:documentation "
DO:             Bring WINDOW to the front, activate it, and show
                it if it is hidden.  The previously active window is
                deactivated.
")
  (:method ((window null))
    ;; Sometimes (front-window) is nil
    (let ((window (front-window)))
      (cond
        (window
         (window-select window))
        (*selected-window*
         (view-deactivate-event-handler *selected-window*)
         (setq *selected-window* nil)))))

  (:method ((window window))
    (setf *last-mouse-click-window* window)
    (if (eql window *selected-window*)
        (unless (window-active-p window)
          (view-activate-event-handler window))
        (progn
          (when *selected-window*
            (view-deactivate-event-handler *selected-window*))
          (window-bring-to-front window)
          (setf *selected-window* nil)
          (reselect-windows)
          (window-bring-to-front window)
          (setf *selected-window* window)
          (view-activate-event-handler window)
          (menu-update (edit-menu))))))



(defgeneric window-zoom-position (window)
  (:documentation "
RETURN:         The zoom position of WINDOW, that is, its position
                after the user clicks the zoom box.  This value is
                either the last value given to SET-WINDOW-ZOOM-POSITION
                for window or the value returned by calling
                WINDOW-DEFAULT-ZOOM-POSITION on WINDOW.
")
  (:method ((window window))
    (or (view-get window 'window-zoom-position)
        (window-default-zoom-position window))))


(defgeneric set-window-zoom-position (window h &optional v)
  (:documentation "

DO:             Set the zoom position of WINDOW, that is, its new
                position after the user clicks the zoom box.

RETURN:         The new position, encoded as an integer.

WINDOW:         A window.

H:              The horizontal coordinate of the new position, or the
                complete position (encoded as an integer) if V is NIL
                or not supplied.

V:              The vertical coordinate of the new position, or NIL if
                the complete position is given by H.
")
  (:method ((window window) h &optional v)
    (if h
        (setf (view-get window 'window-zoom-position) (make-point h v))
        (view-remprop window 'window-zoom-position))))


(defgeneric window-default-zoom-position (window)
  (:documentation "
DO:             Determine the default zoom position of WINDOW, that
                is, its new position after the user clicks the zoom
                box.
")
  (:method ((window window))
    (multiple-value-bind (sl st sr sb) (window-preferred-screen-bounds window)
      (let* ((pos         (view-position window))
             (current-h   (point-h pos))
             (current-v   (point-v pos))
             (size        (window-default-zoom-size window))
             (new-width   (point-h size))
             (new-height  (point-v size))
             (left-border (window-border-width window))
             (moved-h     (+ sl left-border 1))
             (moved-v     (+ st 2 (window-title-height window))))
        ;; If origin of the window is still on the same screen...
        (if (and (<= sl current-h (1- sr))
                 (<= st current-v (1- sb)))
            ;; ...then keep the same coordinates where they allow the window to remain
            ;; wholly on the screen, and use the new ones where the old ones don't...
            (make-point (if (< (+ current-h new-width left-border 1) sr) current-h moved-h)
                        (if (< (+ current-v new-height 2) sb) current-v moved-v))
            ;; otherwise go ahead and move the window.
            (make-point moved-h moved-v))))))



(defgeneric window-zoom-size (window)
  (:documentation "

RETURN:         The zoom size of WINDOW, that is, its size after the
                user clicks the zoom box. This value is either the
                last value given to SET-WINDOW-ZOOM-SIZE for window or
                the value returned by calling WINDOW-DEFAULT-ZOOM-SIZE
                on window.
")
  (:method ((window window))
    (or (view-get window 'window-zoom-size)
        (window-default-zoom-size window))))


(defgeneric set-window-zoom-size (window h &optional v)
  (:documentation "
DO:             Set the zoom size of WINDOW, that is, its new size
                after the user clicks the zoom box.

RETURN:         the new size, encoded as an integer.
")
  (:method ((window window) h &optional v)
    (if h
        (setf (view-get window 'window-zoom-size) (make-point h v))
        (view-remprop window 'window-zoom-size))))


(defgeneric window-default-zoom-size (window)
  (:documentation "
DO:             Determine the default zoom size of WINDOW, that is,
                its new size after the user clicks the zoom box.  The
                provided method returns the value of
                *WINDOW-DEFAULT-ZOOM-SIZE*.
")
  (:method ((window window))
    (multiple-value-bind (sl st sr sb) (window-preferred-screen-bounds window)
      (let* ((left-border   (window-border-width window))
             (right-border  (window-right-border-width window))
             (bottom-border (window-bottom-border-width window))
             (psize         (view-preferred-size window))
             (ph            (min (point-h psize) (- sr sl
                                                    left-border right-border 1)))
             (pv            (min (point-v psize) (- sb st (window-title-height window)
                                                    bottom-border 2)))) ;; huh?
        (make-point ph pv)))))


(defmethod view-cursor ((w window) point)
  (declare (ignore point))
  (window-cursor w))


;;;---------------------------------------------------------------------
;;; menu commands

(defgeneric display-in-windows-menu (w)
  (:method ((w window))
    (or (window-shown-p w) (view-get w :display-in-menu-when-hidden))))


(defgeneric window-needs-saving-p (window)
  (:documentation "
DESCRIPTION:    The WINDOW-NEEDS-SAVING-P generic function determines
                whether the Save menu item in the File menu should be
                enabled for windows that have a definition of
                WINDOW-SAVE.

                The Save menu item is enabled if the class of the
                active window has a method definition for window-save,
                unless the window has a method definition for
                WINDOW-NEEDS-SAVING-P and a call to
                WINDOW-NEEDS-SAVING-P returns NIL.  If the window has
                a method definition for WINDOW-NEEDS-SAVING-P, then
                Save is enabled only if a call to
                WINDOW-NEEDS-SAVING-P returns true.
")
  (:method ((window window))
    nil))




(defgeneric window-can-undo-p (window)
  (:documentation "Obsolete? Used in window-can-do-operation"))



(defun non-window-method-exists-p (op w)
  (let* ((gf           (and (symbolp op) (fboundp op)))
         (methods      (and (standard-generic-function-p gf)
                            (generic-function-methods gf)))
         (class        (class-of w))
         (window-class (find-class 'window))
         (cpl          (class-precedence-list class)))
    (and methods
         (dolist (method methods)
           (when (and (null (method-qualifiers method))
                      (let ((spec (car (method-specializers method))))
                        (and (not (eql spec window-class))
                             (if (typep spec 'eql-specializer)
                                 (eql (eql-specializer-object spec) w)
                                 (member spec cpl)))))
             (return t))))))


(defgeneric window-can-do-operation (window op &optional item)
  (:documentation "
RETURN:         A BOOLEAN value indicating whether view can perform
                operation. (This is a more general replacement for the
                older MCL function window-can-undo-p, which could
                check only for Undo).  If the value returned is true,
                the menu item for operation is enabled; otherwise, it
                is disabled.  The WINDOW-CAN-DO-OPERATION method for
                window returns T if there is a method for operation
                defined for the class of window that is more specific
                than the built-in method defined for the class
                window.  Otherwise WINDOW-CAN-DO-OPERATION returns the
                result of calling WINDOW-CAN-DO-OPERATION on the
                current key handler of window, if there is one.  If
                not, it returns NIL.
")
  (:method ((view window) op &optional item)
    ;; window-can-do-operation needs to check for an applicable primary
    ;; method other than the one that is specialized on the class
    ;; named window rather than just calling method-exists-p
    (cond
      ((and (eql op 'undo)
            (method-exists-p 'window-can-undo-p view))
       (funcall 'window-can-undo-p view))
      ((non-window-method-exists-p op view))
      (t (let ((handler (current-key-handler view)))
           (when handler
             (if (method-exists-p 'window-can-do-operation handler)
                 (window-can-do-operation handler op item)
                 (method-exists-p op handler))))))))


(defun window-do-operation (window op &optional (consider-window-method t))
  (when window
    (cond
      ((and consider-window-method (method-exists-p op window))
       (funcall op window))
      (t
       (let ((handler (current-key-handler window)))
         (when handler
           (cond
             ((method-exists-p op handler)
              (funcall op handler)))))))))




(defmethod window-close :after ((window window))
  ;; window is a subclass* of STREAM.
  (window-do-operation window 'close nil)
  ;; --
  (setf (slot-value window 'my-item) nil)
  (let ((wm *windows-menu*))
    (when (and (typep wm 'menu) (menu-enabled-p wm))
      (update-windows-menu wm))
    (let ((em (edit-menu)))
      (when em (menu-update em)))))

(defgeneric window-save (window)
  (:method ((window window))
    (window-do-operation window 'save nil)))

(defgeneric window-save-as (window)
  (:method ((window window))
    (window-do-operation window 'save-as nil)))

(defgeneric window-save-copy-as (window)
  (:method ((window window))
    (window-do-operation window 'save-copy-as nil)))

(defgeneric window-revert (window)
  (:method ((window window))
    (window-do-operation window 'revert nil)))

(defgeneric window-hardcopy (window &optional show-fl)
  (:method ((window window) &optional show-fl)
    (declare (ignore show-fl))
    (window-do-operation window 'hardcopy nil)))


(defgeneric undo (window)
  (:method ((window window))
    (window-do-operation window 'undo nil)))

(defgeneric undo-more (window)
  (:method ((window window))
    (window-do-operation window 'undo-more nil)))

(defgeneric cut (window)
  (:method ((window window))
    (window-do-operation window 'cut nil)))

(defgeneric copy (window)
  (:method ((window window))
    (window-do-operation window 'copy nil)))

(defgeneric paste (window)
  (:method ((window window))
    (window-do-operation window 'paste nil)))

(defgeneric clear (window)
  (:method ((window window))
    (window-do-operation window 'clear nil)))

(defgeneric select-all (window)
  (:method ((window window))
    (window-do-operation window 'select-all nil)))



;;;---------------------------------------------------------------------
;;; Events

(defmethod window-event ((window window))
  (when *current-event*
    (with-accessors ((what      event-what)
                     (where     event-where)
                     (modifiers event-modifiers)
                     (message   event-message)) *current-event*
      (with-handle (winh window)
        (let ((gc [NSGraphicsContext graphicsContextWithWindow:winh])
              (saved-context [NSGraphicsContext currentContext]))
          (unwind-protect
               (progn
                 [gc setShouldAntialias:NO]
                 [NSGraphicsContext setCurrentContext:gc]
                 (case what
                   ((#.null-event)
                    ;; (view-mouse-moved-event-handler window)
                    (window-null-event-handler window))
                   ((#.mouse-down)
                    #+debug
                    (format *terminal-io* "~&screen point ~S~%" (point-to-list where))
                    (let ((where (screen-to-window-point window where)))
                      #+debug
                      (progn (format *terminal-io* "~&window point ~S~%" (point-to-list where))
                             (finish-output *terminal-io*))
                      (view-click-event-handler window where)
                      (when (double-click-p)
                        (view-double-click-event-handler window where))))
                   ((#.mouse-up)
                    (window-mouse-up-event-handler window))
                   ((#.key-down #.auto-key)
                    (view-key-event-handler window (nth-value 1 (decode-key-message message))))
                   ((#.activate-evt)
                    (if (logtest active-flag modifiers)
                        (view-activate-event-handler window)
                        (view-deactivate-event-handler window)))
                   ((#.update-evt)
                    (window-update-event-handler window))))
            [NSGraphicsContext setCurrentContext:saved-context]))))))

;;;---------------------------------------------------------------------
;;; Fonts

(defmethod set-view-font-codes ((w window) ff ms &optional ff-mask ms-mask)
  (declare (ignorable ff ms ff-mask ms-mask))
  (format-trace '(set-view-font-codes window) :ff ff :ms ms :ff-mask ff-mask :ms-mask ms-mask)
  (call-next-method)
  (values-list (set-font w)))

;;;---------------------------------------------------------------------
;;; Colors

;; TODO: use get-fore-color when drawing in the window…
(defmethod set-fore-color ((window window) color)
  (setf (slot-value window 'fore-color) color)
  (when *color-available*
    (%set-fore-color (or color *foreground-color*))))


(defmethod set-back-color ((window window) color &optional (redisplay-p t))
  (setf (slot-value window 'back-color) color)
  (when *color-available*
    (%set-back-color window (or color *background-color*))
    (when redisplay-p
      (invalidate-view window t))))


(defmethod get-fore-color ((window window))
  (or (slot-value window 'fore-color) *foreground-color*))


(defmethod get-back-color ((window window))
  (or (slot-value window 'back-color) *background-color*))


(defmethod view-draw-contents ((window window))
  (unless  *deferred-drawing*
    (when (window-visiblep window)
      ;; time/stdout
      #+debug-views (format-trace '(view-draw-contents window))
      ;; #+debug-views #|DEBUG-PJB|# (print-backtrace *standard-output*)
      (with-focused-view window
        (with-fore-and-back-color (get-fore-color window) (get-back-color window)
          (call-next-method))))))


;;;---------------------------------------------------------------------

(defclass unknown-window (window)
  ())

(defclass hemlock-frame (window)
  ())

(defclass hemlock-listener-frame (hemlock-frame)
  ())

(defmethod view-draw-contents ((window hemlock-frame))
  (values))

(defmethod wrap ((nswindow ns:ns-window))
  ;; (format-trace 'wrap nswindow)
  (or (find nswindow *window-list* :key (function handle))
      (make-instance 'unknown-window :handle nswindow)))

(defmethod wrap ((nswindow gui::hemlock-listener-frame))
  ;; (format-trace 'wrap nswindow)
  (or (find nswindow *window-list* :key (function handle))
      (make-instance 'hemlock-listener-frame :handle nswindow)))

(defmethod wrap ((nswindow gui::hemlock-frame))
  ;; (format-trace 'wrap nswindow)
  (or (find nswindow *window-list* :key (function handle))
      (make-instance 'hemlock-frame :handle nswindow)))


(defun initialize/window ()
  (setf *window-list* '()) ; wrapping windows modify *window-list*, so let's start from a blank state.
  (setf *window-list* (nsarray-to-list [[NSApplication sharedApplication] windows]))
  (values))

;; (initialize/window)
;; (map nil 'print *window-list*)
;; (mapcar 'nswindow-window (cddr *window-list*))
;; (type-of (first *window-list*))



;;;; THE END ;;;;
