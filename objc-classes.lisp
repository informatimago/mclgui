;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               objc-classes.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Defines a few Objective-C/CLOS classes.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-06-20 <PJB> Created.
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



;;; MCLGUI use the coordinates system of the Macintosh OS,
;;; that is, origin is at the top-left corner, and Y axis goes downward.
;;;
;;; OpenStep uses the normal mathematical coordinates system, with the
;;; origin on the bottom-left corner, and Y axis going upward.
;;; Furthermore, the main screen may not be positionned at the origin
;;; of the coordinates system.

;;;------------------------------------------------------------
;;;
;;; Representation of NSPoint, NSSize and NSRect in lisp,
;;; with conversion between MCLGUI:POINT.

(defun xor (a b)
  "Return A ⊻ B"
  (or (and a (not b)) (and (not a) b)))

(defun nstimeinterval (value) (coerce value 'double-float))
(defun cgfloat        (value) (coerce value 'ns:cgfloat))
(defun sfloat         (value) (coerce value 'single-float))
(defun fontsize       (value) (values (round  value)))
(defun coord          (value) (values (round  value)))
(declaim (inline nstimeinterval cgfloat sfloat fontsize coord))


(defstruct (nspoint
            (:constructor %make-nspoint))
  (x      0.0d0 :type ns:cgfloat)
  (y      0.0d0 :type ns:cgfloat))

(defun make-nspoint (&key (x 0.0d0) (y 0.0d0))
  (%make-nspoint :x (cgfloat x) :y (cgfloat y)))


(defstruct (nssize
            (:constructor %make-nssize))
  (width  0.0d0 :type ns:cgfloat)
  (height 0.0d0 :type ns:cgfloat))

(defun make-nssize (&key (width 0.0d0) (height 0.0d0))
  (%make-nssize :width (cgfloat width) :height (cgfloat height)))


(defstruct (nsrect
            (:constructor %make-nsrect))
  (x      0.0d0 :type ns:cgfloat)
  (y      0.0d0 :type ns:cgfloat)
  (width  0.0d0 :type ns:cgfloat)
  (height 0.0d0 :type ns:cgfloat))


(defun make-nsrect (&key (x 0.0d0 xp) (y 0.0d0 yp) (width 0.0d0 widthp) (height 0.0d0 heightp)
                      origin size)
  (assert (xor (or xp yp) origin))
  (assert (xor (or widthp heightp) size))
  (let ((origin (typecase origin
                  (null    nil)
                  (integer (make-nspoint :x (point-h origin) :y (point-v origin)))
                  (t       origin)))
        (size (typecase size
                (null    nil)
                (integer (make-nssize :width (point-h size) :height (point-v size)))
                (t       size))))
    (if origin
        (if size
            (%make-nsrect :x     (nspoint-x origin)  :y      (nspoint-y origin)
                          :width (nssize-width size) :height (nssize-height size))
            (%make-nsrect :x     (nspoint-x origin)  :y      (nspoint-y origin)
                          :width (cgfloat width)     :height (cgfloat height)))
        (if size
            (%make-nsrect :x     (cgfloat x)         :y      (cgfloat y)
                          :width (nssize-width size) :height (nssize-height size))
            (%make-nsrect :x     (cgfloat x)         :y      (cgfloat y)
                          :width (cgfloat width)     :height (cgfloat height))))))


(defun point-to-nspoint (point)
  (make-nspoint :x (cgfloat (point-h point)) :y (cgfloat (point-v point))))
(defun nspoint-to-point (nspoint)
  (make-point (coord (nspoint-x nspoint)) (coord (nspoint-y nspoint))))

(defun size-to-nssize (size)
  (make-nssize :width (cgfloat (point-h size)) :height (cgfloat (point-v size))))
(defun nssize-to-size (nssize)
  (make-point (coord (nssize-width nssize)) (coord (nssize-height nssize))))


(defun nsrect-origin (nsrect)
  (make-nspoint :x     (nsrect-x nsrect)     :y      (nsrect-y nsrect)))
(defun nsrect-size   (nsrect)
  (make-nssize  :width (nsrect-width nsrect) :height (nsrect-height nsrect)))

(defun (setf nsrect-origin) (nspoint nsrect)
  (setf (nsrect-x nsrect) (nspoint-x nspoint)
        (nsrect-y nsrect) (nspoint-y nspoint)))
(defun (setf nsrect-size)   (nssize  nsrect)
  (setf (nsrect-width  nsrect) (nssize-width  nssize)
        (nsrect-height nsrect) (nssize-height nssize)))

;; Note: we consider the NSRects in flipped coordinate systems.

(defun rect-to-nsrect (rect)
  (make-nsrect :x (cgfloat (rect-left rect))
               :y (cgfloat (rect-top rect))
               :width  (cgfloat (rect-width rect))
               :height (cgfloat (rect-height rect))))

(defun nsrect-to-rect (nsrect)
  "RETURN: A RECT."
  (make-rect (coord (nsrect-x nsrect))
             (coord (nsrect-y nsrect))
             (coord (+ (nsrect-x nsrect) (nsrect-width nsrect)))
             (coord (+ (nsrect-y nsrect) (nsrect-height nsrect)))))



;;;------------------------------------------------------------
;;; Conversions between ns:ns-point, ns:ns-size, ns:ns-rect and
;;; nspoint nssize and nsrect.

(defmethod wrap ((nspoint ns:ns-point))
  ;; (format-trace 'wrap nspoint)
  (make-nspoint :x     (ns:ns-point-x nspoint)
                :y     (ns:ns-point-y nspoint)))

(defmethod wrap ((nssize ns:ns-size))
  ;; (format-trace 'wrap nssize)
  (make-nssize :width  (ns:ns-size-width nssize)
               :height (ns:ns-size-height nssize)))

(defmethod wrap ((nsrect ns:ns-rect))
  ;; (format-trace 'wrap nsrect)
  (make-nsrect :x      (ns:ns-rect-x nsrect)
               :y      (ns:ns-rect-y nsrect)
               :width  (ns:ns-rect-width nsrect)
               :height (ns:ns-rect-height nsrect)))


(defmethod unwrap ((nspoint nspoint))
  (ns:make-ns-point (nspoint-x nspoint) (nspoint-y nspoint)))

(defmethod unwrap ((nssize nssize))
  (ns:make-ns-size (nssize-width nssize) (nssize-height nssize)))

(defmethod unwrap ((nsrect nsrect))
  (ns:make-ns-rect (nsrect-x nsrect) (nsrect-y nsrect)
                   (nsrect-width nsrect) (nsrect-height nsrect)))

;; Shortcuts:

(defun nsrect (a &optional b c d)
  (cond
    (d (ns:make-ns-rect (cgfloat a) (cgfloat b) (cgfloat c) (cgfloat d)))
    (b (ns:make-ns-rect (cgfloat (point-h a)) (cgfloat (point-v a)) (cgfloat (point-h b)) (cgfloat (point-v b))))
    (t (ns:make-ns-rect (cgfloat (rect-left a)) (cgfloat (rect-top a)) (cgfloat (rect-width a)) (cgfloat (rect-height a))))))

(defun nspoint (pos)
  (ns:make-ns-point (point-h pos) (point-v pos)))

(defun nssize (siz)
  (ns:make-ns-size (point-h siz) (point-v siz)))

(declaim (inline nsrect-to-list nsrect nspoint nssize))

(defmacro sret (call)
  (let ((vresult (gensym)))
    `(oclo:slet ((,vresult ,call)) ,vresult)))

(defmacro get-nspoint (call)
  (let ((vpoint (gensym)))
    `(oclo:slet ((,vpoint ,call)) (wrap ,vpoint))))

(defmacro get-nssize (call)
  (let ((vsize (gensym)))
    `(oclo:slet ((,vsize ,call)) (wrap ,vsize))))

(defmacro get-nsrect (call)
  (let ((vframe (gensym)))
    `(oclo:slet ((,vframe ,call)) (wrap ,vframe))))


(defun <nsr>ect-to-nsrect (rect)
  #-ccl (error "~S is not implemented on ~A" '<nsr>ect-to-nsrect (lisp-implementation-type))
  #+ccl (make-nsrect :x (ccl::pref rect #>NSRect.origin.x)
                     :y (ccl::pref rect #>NSRect.origin.y)
                     :width  (ccl::pref rect #>NSRect.size.width)
                     :height (ccl::pref rect #>NSRect.size.height)))

;;;------------------------------------------------------------

(defparameter *event-map*
  `((,#$NSLeftMouseDown          . ,mouse-down)
    (,#$NSLeftMouseUp            . ,mouse-up)
    (,#$NSRightMouseDown         . ,mouse-down)
    (,#$NSRightMouseUp           . ,mouse-up)
    (,#$NSMouseMoved             . ,null-event)
    (,#$NSLeftMouseDragged       . ,null-event)
    (,#$NSRightMouseDragged      . ,null-event)
    (,#$NSMouseEntered           . ,null-event)
    (,#$NSMouseExited            . ,null-event)
    (,#$NSKeyDown                . ,key-down)
    (,#$NSKeyDown                . ,auto-key)
    (,#$NSKeyUp                  . ,key-up)
    (,#$NSFlagsChanged           . ,null-event)
    (,#$NSAppKitDefined          . ,null-event)
    (,#$NSSystemDefined          . ,null-event)
    (,#$NSApplicationDefined     . ,null-event)
    (,#$NSPeriodic               . ,null-event)
    (,#$NSCursorUpdate           . ,null-event)
    (,#$NSScrollWheel            . ,null-event)
    (,#$NSTabletPoint            . ,null-event)
    (,#$NSTabletProximity        . ,null-event)
    (,#$NSOtherMouseDown         . ,mouse-down)
    (,#$NSOtherMouseUp           . ,mouse-up)
    (,#$NSOtherMouseDragged      . ,null-event)
    ;; (,#$NSEventTypeGesture       . ,null-event)
    ;; (,#$NSEventTypeMagnify       . ,null-event)
    ;; (,#$NSEventTypeSwipe         . ,null-event)
    ;; (,#$NSEventTypeRotate        . ,null-event)
    ;; (,#$NSEventTypeBeginGesture  . ,null-event)
    ;; (,#$NSEventTypeEndGesture    . ,null-event)
    ))

(defun mac-event-mask-to-ns-event-mask (mac-mask)
  (loop
    :with ns-mask = 0
    :for (ns-event . mac-event) :in *event-map*
    :do (when (and (/= null-event mac-event)
                   (plusp (logand (ash 1 mac-event) mac-mask)))
          (setf ns-mask (logior ns-mask (ash 1 ns-event))))
    :finally (return ns-mask)))




(defparameter *modifier-map*
  `((,#$NSAlphaShiftKeyMask . ,alpha-lock)
    (,#$NSShiftKeyMask      . ,shift-key)
    (,#$NSControlKeyMask    . 0)
    (,#$NSAlternateKeyMask  . ,option-key)
    (,#$NSCommandKeyMask    . ,cmd-key)
    (,#$NSNumericPadKeyMask . 0)
    (,#$NSHelpKeyMask       . 0)
    (,#$NSFunctionKeyMask   . 0)))

(defun nsmodifier-to-macmodifier (nsmodifier)
  (loop
    :for (nsmod . macmod) :in *modifier-map*
    :sum (if (zerop (logand nsmod nsmodifier))
             0
             macmod)))

(defun macmodifier-to-nsmodifier (macmodifier)
  (loop
    :for (nsmod . macmod) :in *modifier-map*
    :sum (if (zerop (logand macmod macmodifier))
             0
             nsmod)))

(defun modifiers ()
  "The current mac modifier flags."
  (nsmodifier-to-macmodifier (modifier-flags)))



(defun encode-key-message (kcode characters)
  (let ((kcode  (ldb (byte 8 0) kcode)))
    (case (length characters)
      ((0) (dpb kcode (byte 8 8) 0))
      ((1) (let ((ccode (char-code (aref characters 0))))
             (if (<= 0 ccode 255)
                 (dpb kcode (byte 8 8) ccode)
                 (dpb 1 (byte 1 31)
                      (dpb kcode (byte 8 22)
                           ccode)))))
      (otherwise
       (list kcode characters)))))

(defun decode-key-message (message)
  (if (listp message)
      (values-list message)
      (let ((extended (ldb (byte 1 31) message)))
        (if (zerop extended)
            (values (ldb (byte 8 8) message)
                    (code-char (ldb (byte 8 0) message)))
            (values (ldb (byte 8 22) message)
                    (code-char (ldb (byte 21 0) message)))))))


(defun nsevent-to-event (nsevent)
  (check-type nsevent ns:ns-event)
  ;; (format-trace 'wrap nsevent)
  (let ((what (case [nsevent type]
                ((#.#$NSLeftMouseDown)         mouse-down)
                ((#.#$NSLeftMouseUp)           mouse-up)
                ((#.#$NSRightMouseDown)        mouse-down)
                ((#.#$NSRightMouseUp)          mouse-up)
                ((#.#$NSMouseMoved)            null-event)
                ((#.#$NSLeftMouseDragged)      null-event)
                ((#.#$NSRightMouseDragged)     null-event)
                ((#.#$NSMouseEntered)          null-event)
                ((#.#$NSMouseExited)           null-event)
                ((#.#$NSKeyDown)               (if [nsevent isARepeat]
                                                   auto-key
                                                   key-down))
                ((#.#$NSKeyUp)                 key-up)
                ((#.#$NSFlagsChanged)          null-event)
                ((#.#$NSAppKitDefined)         null-event)
                ((#.#$NSSystemDefined)         null-event)
                ((#.#$NSApplicationDefined)    null-event)
                ((#.#$NSPeriodic)              null-event)
                ((#.#$NSCursorUpdate)          null-event)
                ((#.#$NSScrollWheel)           null-event)
                ((#.#$NSTabletPoint)           null-event)
                ((#.#$NSTabletProximity)       null-event)
                ((#.#$NSOtherMouseDown)        mouse-down)
                ((#.#$NSOtherMouseUp)          mouse-up)
                ((#.#$NSOtherMouseDragged)     null-event)
                ;; ((#.#$NSEventTypeGesture)      null-event)
                ;; ((#.#$NSEventTypeMagnify)      null-event)
                ;; ((#.#$NSEventTypeSwipe)        null-event)
                ;; ((#.#$NSEventTypeRotate)       null-event)
                ;; ((#.#$NSEventTypeBeginGesture) null-event)
                ;; ((#.#$NSEventTypeEndGesture)   null-event)
                (otherwise                     null-event))))
    (make-event
     :what      what
     :message   (case what
                  ((#.key-down #.key-up #.auto-key)
                   (encode-key-message [nsevent keyCode] (objcl:lisp-string [nsevent characters])))
                  ((#.mouse-down #.mouse-up)
                   (nswindow-window [nsevent window]))
                  (otherwise 0))
     :when      (truncate [nsevent timestamp] (/ +tick-per-second+))
     :where     (nsscreen-to-screen-point
                 (if (or (= what mouse-down) (= what mouse-up))
                     (let ((winh [nsevent window])
                           (pt (get-nspoint [nsevent locationInWindow])))
                       (if (nullp winh)
                           pt
                           (nswindow-to-nsscreen-point winh pt)))
                     (get-nspoint [NSEvent mouseLocation])))
     :modifiers (nsmodifier-to-macmodifier [nsevent modifierFlags])
     :nsevent [nsevent retain])))



(defmethod wrap ((nsevent ns:ns-event))
  (nsevent-to-event nsevent))


(defun make-key-nsevent (key)
  [NSEvent keyEventWithType:#$NSKeyDown
           location:(ns:make-ns-point 100.0 50.0)
           modifierFlags:0
           timestamp:(cgfloat 0.0)
           windowNumber:0
           context:[NSGraphicsContext currentContext]
           characters:(objcl:objcl-string key)
           charactersIgnoringModifiers:(objcl:objcl-string (string-downcase key))
           isARepeat:nil
           keyCode:(char-code (aref key 0))])

;; (nsevent-to-event (make-key-nsevent "a"))



;;;------------------------------------------------------------
;;; coordinates

(defmacro frame (call)
  (let ((vframe (gensym)))
    `(oclo:slet ((,vframe ,call))
       (values
        (ns:ns-rect-x ,vframe)
        (ns:ns-rect-y ,vframe)
        (ns:ns-rect-width  ,vframe)
        (ns:ns-rect-height ,vframe)))))


;; wx = sx + vh
;; wy = sy - vv - sv
;;
;; vh = wx - sx
;; vv = sy - wy - sv


(defun screen-frames ()
  "RETURN: A list of frame for each of the screen. The mainScreen frame is first.
The coordinates are in Cocoa coordinates."
  (let ((results '())
        (main     [NSScreen mainScreen]))
    (cons (get-nsrect [main frame])
          (do-nsarray (screen [NSScreen screens] (nreverse results))
            (unless (eql screen main)
              (push (get-nsrect [screen frame]) results))))))

;; (screen-frames)
;; (#S(nsrect :x 0.0D0 :y 0.0D0 :width 1920.0D0 :height 1080.0D0))


(defun main-screen-frame ()
  "
RETURN:         x y w h of the main screen (in Cocoa rounded coordinates).
"
  (multiple-value-bind (x y w h) (frame [[NSScreen mainScreen] frame])
    (values (round x) (round y)
            (round w) (round h))))

(defun nsscreen-to-screen-point (nspoint)
  (multiple-value-bind (sx sy sw sh) (main-screen-frame)
    (declare (ignore sx sw))
    (make-point (round (nspoint-x nspoint))
                (round (- (+ sy sh) (nspoint-y nspoint))))))

(defun screen-to-nsscreen-point (point)
  (multiple-value-bind (sx sy sw sh) (main-screen-frame)
    (declare (ignore sx sw))
    (make-nspoint (point-h point)
                  (- (+ sy sh) (point-v point)))))


(defun nswindow-to-nsscreen-point (nswindow nspoint)
  #-cocoa-10.7 (get-nspoint [nswindow convertBaseToScreen:(unwrap nspoint)])
  #+cocoa-10.7 (let* ((r (ns:make-ns-rect (nspoint-x nspoint) (nspoint-y nspoint) 1 1))
                      (c (get-nsrect [nswindow convertRectToScreen:r])))
                 (make-nspoint :x (nsrect-x c) :y (nsrect-y c))))


;; (nswindow-to-nsscreen-point (handle (first (windows))) (ns:make-ns-point 10.0 20.0))


;;;------------------------------------------------------------
;;; mouse coordinates

(defmacro with-view-handle ((handle view-or-window) &body body)
  "
VIEW-OR-WINDOW: An instance of VIEW, that can be a WINDOW.

HANDLE:         A variable.

DO:             Evaluates the BODY in a lexical environment where
                HANDLE is bound to the handle of the contentView of
                the window of the view.
"
  (let ((vov (gensym))
        (winh (gensym)))
    `(let* ((,vov  ,view-or-window)
            (,winh (handle (if (typep ,vov 'window)
                               ,vov
                               (view-window ,vov)))))
       (when ,winh
         (let ((,handle [,winh contentView]))
           ,@body)))))

(defun window-mouse (window)
  "Current position of the mouse in the coordinates of the given window."
  ;; see also view-mouse-position
  (or (with-handle (winh window)
        (with-view-handle (viewh window)
          (nspoint-to-point
           ;; only for 10.6+
           #-(and) (unwrap (nsrect-origin (get-nsrect [winh convertRectFromScreen:[NSEvent mouseLocation]])))
           ;; deprecated, but 10.6+ doesn't work on ccl-1.8.
           (get-nspoint [viewh convertPoint:[winh mouseLocationOutsideOfEventStream]
                               fromView:*null*]))))
      (screen-mouse)))

(defun screen-mouse ()
  "Current position of the mouse in screen coordinates."
  (nsscreen-to-screen-point (get-nspoint [NSEvent mouseLocation])))





;;;------------------------------------------------------------
;;; mouse buttons

(defun any-button-down ()
  "Whether any mouse button is pressed."
  #-cocoa-10.6 (loop
                 :for button :below 2
                   :thereis (not (zerop (#_CGEventSourceButtonState
                                         #$kCGEventSourceStateCombinedSessionState
                                         button))))
  #+cocoa-10.6 (not (zerop [NSEvent pressedMouseButtons])))


;;;------------------------------------------------------------
;;; Types.

#+ccl (ccl:def-foreign-type ns-rect-ptr (:* :<NSR>ect))

;;;------------------------------------------------------------
;;; Application Delegate



@[LispApplicationDelegate
  method:(applicationShouldTerminate:(id)sender)
  resultType:(:int)
  body:
  (declare (ignore sender))
  (with-event-environment
    (block nil
      (catch :cancel
        (mapc (function funcall) *application-should-terminate-functions*)
        (return #$NSTerminateNow))
      #$NSTerminateCancel))]


;;;------------------------------------------------------------
;;; NSWindow

@[NSWindow
  method:(setFrame:(:<NSR>ect)rect)
  resultType:(:void)
  body:
  #+debug-objc (format-trace "-[NSWindow setFrame:]")
  [self setFrame:rect display:YES]]



@[NSWindow
  method:(orderBelow:(:id)otherWindow)
  resultType:(:void)
  body:
  #+debug-objc (format-trace "-[NSWindow orderBelow:]")
  [self orderWindow:#$NSWindowBelow relativeTo:[otherWindow windowNumber]]]


;;;------------------------------------------------------------
;;; MclguiWindow

@[NSWindow subClass:MclguiWindow
           slots:((window :initform nil
                          :initarg :view
                          :reader nswindow-window))]


(defmethod wrap ((nswindow mclgui-window))
  ;; (format-trace 'wrap nswindow)
  (or (nswindow-window nswindow)
      (progn (cerror "Wrap ~S into an UNKNOWN-WINDOW instance."
                     "The window ~S doesn't have a WINDOW instance."
                     nswindow)
             (make-instance 'unknown-window :handle nswindow))))

@[MclguiWindow
  method:(windowDidMove:(:id)nsnotification)
  resultType:(:void)
  body:
  (declare (ignore nsnotification))
  (with-event-environment
    (let ((window (nswindow-window self)))
      ;; (format-trace "-[MclguiWindow windowDidMove:]" window)
      (window-move-event-handler window (rect-topleft (window-frame-from-nswindow-frame window)))))]


@[MclguiWindow
  method:(windowDidResize:(:id)nsnotification)
  resultType:(:void)
  body:
  (declare (ignore nsnotification))
  (with-event-environment
    (let ((window (nswindow-window self)))
      ;; (unfrequently 1/3 (format-trace "-[MclguiWindow windowDidResize:]" window))
      (window-size-event-handler window (rect-size (window-frame-from-nswindow-frame window)))))]



@[MclguiWindow
  method:(windowShouldClose:(:id)nsnotification)
  resultType:(:<bool>)
  body:
  (declare (ignore nsnotification))
  (with-event-environment
    (let* ((window (nswindow-window self)))
      ;; (format-trace "-[MclguiWindow windowShouldClose:]" window)
      (window-close-event-handler window)))]


@[MclguiWindow
  method:(doClose)
  resultType:(:void)
  body:
  ;; (format-trace "-[MclguiWindow doClose]")
  [super close]]

(defun close-nswindow (winh)
  (with-event-environment
    [winh doClose]))

;; (objc:define-objc-method ((:void do-close) mclgui-window)
;;   ;; (format-trace "-[MclguiWindow doClose]")
;;   (objc:send-super 'close))


@[MclguiWindow
  method:(close)
  resultType:(:void)
  body:
  (with-event-environment
    (let ((window  (nswindow-window self)))
      ;; (format-trace "-[MclguiWindow close]" window)
      (catch :cancel (window-close window))))]




(defconstant inZoomIn  0)
(defconstant inZoomOut 1)

@[MclguiWindow
  method:(windowShouldZoom:(:id)nswindow toFrame:(:<NSR>ect)newFrame)
  resultType:(:<BOOL>)
  body:
  (declare (ignore nswindow))
  ;; #|DEBUG-PJB|#(break)
  (with-event-environment
    (let ((window (nswindow-window self)))
      #+debug-objc (format-trace "-[MclguiWindow windowShouldZoom:toFrame:]" window)
      (when (eql (window-type window) :document-with-zoom)
        (window-zoom-event-handler
         window
         (if (< (format-trace '(self frame) (multiple-value-bind (x y w h) (frame [self frame])
                                              (declare (ignore x y))
                                              (* w h)))
                (format-trace 'newframe
                              (let ((frame (<nsr>ect-to-nsrect newFrame)))
                                (* (nsrect-width frame)
                                   (nsrect-height frame)))))
             inZoomOut
             inZoomIn))
        t)))]

;; @[MclguiWindow
;;   method:(windowWillUseStandardFrame:(:id)window defaultFrame:(:<NSR>ect)newFrame)
;;   resultType:(:<NSR>ect)
;;   body:
;;   (let* ((window (nswindow-window self)))
;;     (format-trace "window should close" window)
;;     (window-zoom-event-handler window :inZoomIn))]

@[MclguiWindow
  method:(zoom:(:id)sender)
  resultType:(:void)
  body:
  (with-event-environment
    [super zoom:sender]
    (let ((window (nswindow-window self)))
      #+debug-objc (format-trace "-[MclguiWindow zoom:]" window)
      (when window
        (window-do-zoom window))))]


@[MclguiWindow
  method:(becomeMainWindow)
  resultType:(:void)
  body:
  (with-event-environment
    [super becomeMainWindow]
    (let* ((window (nswindow-window self)))
      ;; (format-trace "-[MclguiWindow becomeMainWindow]" window)
      ;; TODO: move after windoids.
      (when window
        (delete-from-list *window-list* window)
        (insert-into-list *window-list* 0 window)
        (let ((event (get-null-event))
              (*multi-click-count* 0))
          (setf (event-what event) activate-evt
                (event-modifiers event) (logior (event-modifiers event)
                                                active-flag)
                (event-message event) window)
          ;; (format-trace '|becomeMainWindow| event)
          ;; (view-activate-event-handler window)
          (post-event event)))))]


@[MclguiWindow
  method:(resignMainWindow)
  resultType:(:void)
  body:
  (with-event-environment
    [super resignMainWindow]
    (let ((window (nswindow-window self)))
      ;; (format-trace "-[MclguiWindow resignMainWindow]" window)
      (when window
        (let ((event (get-null-event))
              (*multi-click-count* 0))
          (setf (event-what event) activate-evt
                (event-modifiers event) (logandc2 (event-modifiers event)
                                                  active-flag)
                (event-message event) window)
          ;; (format-trace '|resignMainWindow| event)
          ;; (view-deactivate-event-handler window)
          (post-event event)))))]

(defvar *current-ns-event* nil)

@[MclguiWindow
  method:(keyDown:(:id)event)
  resultType:(:void)
  body:
  (with-event-environment
    (let ((*current-ns-event* event))
      #+debug-objc (format-trace '|-[MclguiWindow keyDown:]| self event)
      (post-event (nsevent-to-event event))))]

@[MclguiWindow
  method:(keyUp:(:id)event)
  resultType:(:void)
  body:
  (with-event-environment
    (let ((*current-ns-event* event))
      #+debug-objc (format-trace '|-[MclguiWindow keyUp:]| self event)
      (post-event (nsevent-to-event event))))]

@[MclguiWindow
  method:(mouseDown:(:id)event)
  resultType:(:void)
  body:
  (with-event-environment
    (let ((*current-ns-event* event))
      #+debug-objc (format-trace '|-[MclguiWindow mouseDown:]| self event)
      (post-event (nsevent-to-event event))))]

@[MclguiWindow
  method:(mouseUp:(:id)event)
  resultType:(:void)
  body:
  (with-event-environment
    (let ((*current-ns-event* event))
      #+debug-objc (format-trace '|-[MclguiWindow mouseUp:]| self event)
      (post-event (nsevent-to-event event))))]

(defun needs-to-draw-rect (window rect)
  (with-event-environment
    #+(and debug-objc debug-view) (format-trace 'needs-to-draw-rect :posi (point-to-list (rect-topleft rect)) :size (point-to-list (rect-size rect)) :win window)
    (with-handle (winh window)
      [[winh contentView] setNeedsDisplayInRect:(unwrap (rect-to-nsrect rect))]
      ;; [winh setViewsNeedDisplay:yes]
      #+debug-views (format-trace 'needs-to-draw-rect [winh viewsNeedDisplay]))))

(defun does-not-need-to-display (window)
  (with-event-environment
    #+(and debug-objc debug-view) (format-trace 'does-not-need-to-display :win window)
    (with-handle (winh window)
      [[winh contentView] setNeedsDisplay:no]
      [winh setViewsNeedDisplay:no]
      #+debug-view (format-trace 'does-not-need-to-display [winh viewsNeedDisplay]))))

(defun needs-to-display (window)
  (with-event-environment
    #+(and debug-objc debug-view) (format-trace 'needs-to-display :win window)
    (with-handle (winh window)
      [[winh contentView] setNeedsDisplay:yes]
      ;; [winh setViewsNeedDisplay:yes]
      #+debug-views (format-trace 'needs-to-display [winh viewsNeedDisplay]))))

(defun needs-to-display-p (window)
  (with-event-environment
    (with-handle (winh window)
      [winh viewsNeedDisplay])))

;;;------------------------------------------------------------
;;; MclguiView

@[NSView subClass:MclguiView
         slots:((view :initform nil
                      :initarg :view
                      :reader nsview-view))]


@[MclguiView
  method:(isFlipped)
  resultType:(:<bool>)
  body:YES]

(defun *nsrect-to-nsrect (prect)
  #+ccl (make-nsrect
         :x (ccl:pref prect :<nsr>ect.origin.x)
         :y (ccl:pref prect :<nsr>ect.origin.y)
         :width (ccl:pref prect :<nsr>ect.size.width)
         :height (ccl:pref prect :<nsr>ect.size.height))
  #-ccl prect)

@[MclguiView
  method:(drawRect:(:<NSR>ect)rect)
  resultType:(:void)
  body:
  (with-event-environment
    (let* ((window (nsview-view self))
           (nsrect (*nsrect-to-nsrect rect))
           (visrgn (rect-region (nsrect-to-rect nsrect))))
      (when window
        #+debug-objc (with-handle (winh window)
                       (format-trace "progn (-[MclguiView drawRect:]"
                                     [winh viewsNeedDisplay] [[winh contentView] needsDisplay]
                                     nsrect self))
        #+debug-objc (progn
                       (format-trace ":vis-region"     (rect-to-list (region-bounds visrgn)))
                       (format-trace ":invalid-region" (rect-to-list (region-bounds (window-invalid-region window))))
                       (format-trace ":erase-region"   (rect-to-list (region-bounds (window-erase-region   window)))))
        (let ((erase-region  (window-erase-region window)))
          (unless (empty-region-p erase-region)
            (erase-region window erase-region)
            (set-rect-region (window-erase-region window) 0 0 0 0)))
        (let ((invalid-region  (window-invalid-region window)))
          (declare (ignorable invalid-region))
          (view-focus-and-draw-contents window visrgn visrgn)
          #-(and)
          (unless (empty-region-p invalid-region)
            (view-focus-and-draw-contents window visrgn invalid-region #|(view-clip-region window)|#)))
        #+debug-objc (with-handle (winh window)
                       (format-trace "-[MclguiView drawRect:])"
                                     [winh viewsNeedDisplay]
                                     [[winh contentView] needsDisplay])))))]

@[MclguiView
  method:(mouseDown:(:id)event)
  resultType:(:void)
  body:
  (with-event-environment
    (let ((*current-ns-event* event))
      #+debug-objc (format-trace "-[MclguiView mouseDown:]" self event)
      (post-event (nsevent-to-event event))))]

@[MclguiView
  method:(mouseUp:(:id)event)
  resultType:(:void)
  body:
  (with-event-environment
    (let ((*current-ns-event* event))
      #+debug-objc (format-trace "-[MclguiView mouseUp:]" self event)
      (post-event (nsevent-to-event event))))]


@[MclguiView
  method:(mouseMoved:(:id)event)
  resultType:(:void)
  body:
  (with-event-environment
    #+(and debug-objc debug-view) (format-trace "-[MclguiView mouseMoved:]" self (nsview-view self) event)
    (when (nsview-view self)
      (let ((*current-ns-event* event)
            (*current-event* (nsevent-to-event event))
            (*multi-click-count* [event clickCount]))
        ;; (unfrequently 1/10 (format-trace '|mouseMoved:| *current-event*))
        (window-null-event-handler (view-window (nsview-view self))))))]


@[MclguiView
  method:(mouseDragged:(:id)event)
  resultType:(:void)
  body:
  (with-event-environment
    #+(and debug-objc debug-view) (format-trace "-[MclguiView mouseDragged]" self (nsview-view self) event)
    (when (nsview-view self)
      (let ((*current-ns-event* event)
            (*current-event* (nsevent-to-event event))
            (*multi-click-count* [event clickCount]))
        #+debug-objc (unfrequently 1/10 (format-trace "mouseDragged:" *current-event*))
        (window-null-event-handler (view-window (nsview-view self))))))]



;;;------------------------------------------------------------
;;; MclguiTextField -- static-text-dialog-item

@[NSTextField subClass:MclguiTextField
              slots:((item :initform nil
                       :initarg :item
                       :accessor nsview-view
                       :accessor nscontroller-dialog-item
                       :documentation "An instance of DIALOG-ITEM or subclasses.")
                     (event :initform nil
                            :accessor nscontroller-current-event))]

@[MclguiTextField
  method:(mclguiAction:(:id)sender)
  resultType:(:void)
  body:
  (declare (ignore sender))
  (with-event-environment
    (when (nscontroller-dialog-item self)
      (dialog-item-action (nscontroller-dialog-item self))))]


;;;------------------------------------------------------------
;;; MclguiTextField -- static-text-dialog-item

;; @[MclguiTextField
;;   method:(acceptsFirstResponder)
;;   resultType:(:<bool>)
;;   body:
;;   (format-trace  "-[MclguiTextField acceptsFirstResponder]" self)
;;   YES]
;;
;; @[MclguiTextField
;;   method:(becomeFirstResponder)
;;   resultType:(:<bool>)
;;   body:
;;   (format-trace  "-[MclguiTextField becomeFirstResponder]" self
;;                  [super becomeFirstResponder])]
;;
;; @[MclguiTextField
;;   method:(resignFirstResponder)
;;   resultType:(:<bool>)
;;   body:
;;   (format-trace  "-[MclguiTextField resignFirstResponder]" self
;;                  [super resignFirstResponder])]
;;
;; @[MclguiTextField
;;   method:(keyDown:(:id)event)
;;   resultType:(:void)
;;   body:
;;   (format-trace "-[MclguiTextField keyDown:]" self event)
;;   [super keyDown:event]]


;;;------------------------------------------------------------
;;; MclguiEvaluator


@[NSObject subClass:MclguiEvaluator
           slots:((thunk :initform nil
                         :initarg :think
                         :accessor evaluator-thunk))]

@[MclguiEvaluator
  method:(evaluate)
  resultType:(:void)
  body:
  (with-event-environment
    (if (evaluator-thunk self)
        (funcall (evaluator-thunk self))
        (warn "Evaluator got a NIL thunk")))]


;;;------------------------------------------------------------
;;; Objective-C

(defun class-get-subclasses (class)
  (if (symbolp class)
      (class-get-subclasses (find-class class))
      (let ((num-classes (#_objc_getClassList *null* 0)))
        (cffi:with-foreign-object (classes :pointer num-classes)
          (#_objc_getClassList classes num-classes)
          (loop
            :for i :below num-classes
            :for subclass = (cffi:mem-aref classes :pointer i)
            :when (loop
                    :for superclass = (#_class_getSuperclass subclass)
                      :then (#_class_getSuperclass superclass)
                    :while (and (not (nullp superclass))
                                (eql superclass class))
                    :finally (return (if (nullp superclass)
                                         nil
                                         superclass)))
              :collect subclass)))))

;; (class-get-subclasses 'ns:ns-object)

;; (pushnew :debug-objc *features*)
;; (setf *features* (remove :debug-objc *features*))
;;;; THE END ;;;;
