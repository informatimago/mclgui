;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               event.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Event processing
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-05-16 <PJB> Created.
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


;;;---------------------------------------------------------------------
;;; application event handlers
;;;---------------------------------------------------------------------

(defgeneric application-event-handler (application event)
  (:documentation "
MCLGUI extension.
This generic function is called when an app?-evt is received.
APPLICATION: *APPLICATION*
EVENT:       The EVENT received.")
  (:method (application event)
    (declare (ignore (application event)))
    nil))


;;;---------------------------------------------------------------------
;;; view event handlers
;;;---------------------------------------------------------------------

(defgeneric view-activate-event-handler (view)
  (:documentation "
The generic function view-activate-event-handler is called by the
event system when the window containing the view is made active.
The definition for simple-view does nothing. The definition for view calls
view-activate-event-handler on each subview. Specialize this generic
function if your view needs to indicate visually that it is active.

VIEW:           A simple view or view.
")
  (:method (view)
    (declare (ignore view))
    nil))


(defgeneric view-deactivate-event-handler (view)
  (:documentation "
The generic function view-deactivate-event-handler is called by
the event system to deactivate a view. It is called when the window
containing the view is active and a different window is made active.
The definition for simple-view does nothing. The definition for view calls
view-deactivate-event-handler on each subview. Specialize this
generic function if your view needs to indicate visually that it has been
deactivated.

VIEW:           A simple view or view.
")
  (:method (view)
    (declare (ignore view))
    nil))


(defgeneric view-click-event-handler (view where)
  (:documentation "
The generic function VIEW-CLICK-EVENT-HANDLER is called by the
event system when a mouse click occurs. The SIMPLE-VIEW method does
nothing.  The view method calls VIEW-CONVERT-COORDINATES-AND-CLICK
on the first subview for which POINT-IN-CLICK-REGION-P returns T.

The VIEW-CLICK-EVENT-HANDLER function is not called when the user
clicks the title bar, close box, zoom box, or size box of a
window. The method for SIMPLE-VIEW does nothing.  Specialized windows
provided by the system, such as FRED-WINDOW, have special behavior.

The function VIEW-CLICK-EVENT-HANDLER scans subviews in the opposite
order as does VIEW-DRAW-CONTENTS.  The first view added is the first one
drawn but the last one to be queried during clicking.
If you define any VIEW-CLICK-EVENT-HANDLER methods for window, they
must call CALL-NEXT-METHOD.

VIEW:           A simple view or view.

WHERE:          For a view, the mouse click position (the position when
                the mouse is clicked) of the view in the local coordinate
                system. For a simple view, the mouse click position of the
                simple view in the local coordinate system of the view’s
                container.
")
  (:method (view where)
    (declare (ignore view where))
    ;; (format-trace 'view-click-event-handler 't view (point-to-list where))
    nil))


(defgeneric view-key-event-handler (view key)
  (:documentation "
The methods of the generic function VIEW-KEY-EVENT-HANDLER examine the
current keystroke and determine what is to be done with it.  The
method for SIMPLE-VIEW calls ED-BEEP.  The method for WINDOW
determines whether the key indicates the selection of a default button
or indicates a change of the current key handler, then selects the
button or passes the keystroke to the appropriate key handler.  The
method for FRED-MIXIN binds the *CURRENT-KEYSTROKE* variable to the
keystroke of the current event and runs the Fred command associated
with the keystroke.  The method for FRED-DIALOG-ITEM calls
call-next-method inside WITH-FOCUSED-VIEW and WITH-FORE-COLOR.

The generic function VIEW-KEY-EVENT-HANDLER is called with
*APPLICATION* as the first argument when there are no active windows
and the user presses a key on the keyboard.  The method for
application sounds a beeps.

VIEW:           A simple view.

KEY:            The current keystroke character.
")
  (:method (view key)
    (declare (ignore view key))
    nil))



(defgeneric view-mouse-enter-event-handler (view)
  (:documentation "
The methods of these generic functions for SIMPLE-VIEW do nothing.
You specialize them to create mouse-sensitive items.

VIEW:           A simple view.
")
  (:method ((view simple-view))
    view))


(defgeneric view-mouse-leave-event-handler (view)
  (:documentation "
The methods of these generic functions for SIMPLE-VIEW do nothing.
You specialize them to create mouse-sensitive items.

VIEW:           A simple view.
")
  (:method ((view simple-view))
    view))


;;;---------------------------------------------------------------------
;;; window event handlers
;;;---------------------------------------------------------------------

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


(defgeneric window-null-event-handler (window)
  (:documentation "
The generic function WINDOW-NULL-EVENT-HANDLER is called on the top
window (if there is one) whenever the system is idle.  It updates the
cursor, runs system tasks, and forces output from *TERMINAL-IO*.  If
there is no top window, the unspecialized method simply updates the
cursor.

WINDOW:         A window.
")
  (:method (window)
    (declare (ignore window))
    nil))


(defgeneric window-select-event-handler (window)
  (:documentation "
The generic function WINDOW-SELECT-EVENT-HANDLER is called whenever
the user clicks an inactive window.  The WINDOW-SELECT-EVENT-HANDLER
function may be specialized, for example, to make a window
unselectable.

WINDOW:         A window.
")
  (:method (window)
    (declare (ignore window))
    nil))


(defgeneric window-key-up-event-handler (window)
  (:documentation "
The generic function WINDOW-KEY-UP-EVENT-HANDLER is called whenever a
key is released after being pressed.  The method for WINDOW does
nothing.

Every key pressed by the user actually generates two events: one when
the key is pressed and another when the key is released.

The default Macintosh event mask filters out key-up events.  To allow
key-up events, call #_SetEventMask with an appropriate mask.  Note
that you must reset the event mask before exiting Lisp.  For details
on event masks, see Macintosh Technical Note 202 and Inside Macintosh.

WINDOW:         A window.
")
  (:method (window)
    (declare (ignore window))
    nil))


(defgeneric window-mouse-up-event-handler (window)
  (:documentation "
The WINDOW-MOUSE-UP-EVENT-HANDLER generic function is called whenever
the user releases the mouse button.  The method for WINDOW does
nothing.

WINDOW:         A window.
")
  (:method (window)
    (declare (ignore window))
    nil))


(defgeneric window-grow-event-handler (window where)
  (:documentation "
The generic function WINDOW-GROW-EVENT-HANDLER is called by the event
system whenever the user clicks a window’s grow box.  The method for
window calls #_GrowWindow, then calls SET-VIEW-SIZE on the window and
the new size.

WINDOW:         A window.

WHERE:          The position in screen coordinates of the cursor when
                the mouse button was pressed down.
")
  (:method (window where)
    (declare (ignore window where))
    nil))


(defgeneric window-drag-event-handler (window where)
  (:documentation "
The generic function WINDOW-DRAG-EVENT-HANDLER is called by the event
system whenever a window needs to be dragged.  It calls #_SetClip and
#_ClipAbove on the region of the window, copies the contents of the
region to the new location of window, and calls SET-VIEW-POSITION on
the window and the new position of the upper-left corner of the
window.

WINDOW:         A window.

WHERE:          The position in screen coordinates of the cursor when
                the mouse button was pressed down.
")
  (:method (window where)
    (declare (ignore window where))
    nil))


(defgeneric window-zoom-event-handler (window message)
  (:documentation "
The generic function WINDOW-ZOOM-EVENT-HANDLER is called by the event
system when the user clicks the window’s zoom box.  It executes the
Toolbox calls to zoom the window, then calls WINDOW-SIZE-PARTS.  The
function WINDOW-SIZE-PARTS should be specialized if you want to change
the contents of a window whenever the window changes size.

WINDOW:         A window.

MESSAGE:        An keyword, :inZoomOut if the window should move to
                the window’s zoom position and size, or :inZoomIn if
                the window should move to the position and size it had
                before zooming out.
")
  (:method (window message)
    (declare (ignore window message))
    nil))


(defgeneric window-close-event-handler (window)
  (:documentation "
The generic function WINDOW-CLOSE-EVENT-HANDLER is called by the event
system whenever a window needs to be closed.  In the method for
WINDOW, if the Meta key was pressed when the command was given, the
command closes all windows in the class of window.  If the Control key
was pressed, window is hidden.  Otherwise, WINDOW-CLOSE is called on
WINDOW.

WINDOW:         A window.
")
  (:method (window)
    (declare (ignore window))
    nil))


(defgeneric window-do-first-click (window)
  (:documentation "
The generic function WINDOW-DO-FIRST-CLICK determines whether the
click that selects a window is also passed to VIEW-CLICK-EVENT-HANDLER.

The default value is NIL, meaning that the click that selects a window
generates no further action.  You can give a window instance or
subclass of window its own value for WINDOW-DO-FIRST-CLICK.

WINDOW:         A window.
")
  (:method (window)
    (declare (ignore window))
    nil))


(defgeneric window-update-event-handler (window)
  (:documentation "
The generic function WINDOW-UPDATE-EVENT-HANDLER is called by the
event system whenever any portion of the window needs to be redrawn.
The window version calls #_BeginUpdate to make the VisRgn field of the
GrafPort the portion that needs to be redrawn, calls
VIEW-DRAW-CONTENTS, and then calls #_EndUpdate to restore the GrafPort
VisRgn field.

Because event processing occurs asynchronously,
WINDOW-UPDATE-EVENT-HANDLER may not be called until a moment after a
window is created or uncovered.  (In the default environment, this may
take up to one-third of a second; see event-ticks in “The event
management system” on page 375.)  This means that anything drawn in
the window immediately after it is created or uncovered may be erased
when WINDOW-UPDATE-EVENT-HANDLER is first called.

To fix this problem, simply call EVENT-DISPATCH before drawing in the
window.  The function EVENT-DISPATCH forces the processing of any
pending events. Note that it is necessary to call EVENT-DISPATCH only
when drawing occurs soon after a window is created or uncovered.

You should not specialize this function except to note that the window
has been updated. To get special drawing behavior, you should instead
specialize VIEW-DRAW-CONTENTS.

WINDOW:         A window.
")
  (:method (window)
    (declare (ignore window))
    nil))


(defgeneric window-update-cursor (window point)
  (:documentation "
DESCRIPTION:    The generic function WINDOW-UPDATE-CURSOR is called by UPDATE-CURSOR
                whenever the cursor is over the window.

                When the mouse is over the front window or any floating window, the
                WINDOW-UPDATE-CURSOR method for the window class sets the variable
                *MOUSE-VIEW* to the view containing the mouse, using FIND-CLICKED-SUBVIEW.
                The WINDOW-NULL-EVENT-HANDLER method for the window class
                calls UPDATE-CURSOR, which calls *CURSORHOOK*. The function that is the
                initial value of *CURSORHOOK* calls WINDOW-UPDATE-CURSOR, which sets
                the cursor using the value returned by view-cursor.
                The method for window simply sets the cursor to the result of calling the
                generic function view-cursor on the clicked subview of window if there is
                one; otherwise it sets the cursor to the result of calling window-cursor on the
                window.

                The null method sets the cursor to the value of *ARROW-CURSOR*.

                The WINDOW-UPDATE-CURSOR function should be shadowed if the cursor
                must change according to what part of the window it is over.

WINDOW:         A window or Fred window.

POINT:          The position of the cursor, given in the window’s
                local coordinates.
"))


(defgeneric window-draw-grow-icon (window)
  (:documentation "
The generic function WINDOW-DRAW-GROW-ICON is called when the size box
in the lower-right corner of a window must be redrawn. You may need to
call this function explicitly if you draw over the size box.

When a window is inactive (that is, not the frontmost window),
WINDOW-DRAW-GROW-ICON erases the inside of the size box.

WINDOW:         A window.
")
  (:method (window)
    (declare (ignore window))
    nil))


;;;---------------------------------------------------------------------
;;; event functions
;;;---------------------------------------------------------------------

(defgeneric view-draw-contents (view)
  (:documentation "
The generic function VIEW-DRAW-CONTENTS is called whenever a view
needs to redraw any portion of its contents.  The VIEW method for
VIEW-DRAW-CONTENTS erases the area in the window’s erase region (for
new windows, this is the entire content area) and then calls
VIEW-DRAW-CONTENTS on each subview.  You can specialize this function
so that a user-defined view can be redrawn when portions of it are
covered and uncovered.

When VIEW-DRAW-CONTENTS is called by the event system, the view’s clip
region is set so that drawing occurs only in the portions that need to
be updated.  This normally includes areas that have been covered by
other windows and then uncovered.

VIEW:           A simple view.
")
  (:method (view)
    (declare (ignore view))
    nil))


(defgeneric view-mouse-position (view)
  (:documentation "
The generic function VIEW-MOUSE-POSITION returns the cursor position
as a point expressed in the view’s local coordinates.  The point is
returned as an integer (for a description of points, see “Chapter 2:
Points and Fonts”).  This function may be called at any time, not just
during event processing. The coordinates may be negative, or outside
of the view’s PortRect, depending on the position of the cursor.

The function (VIEW-MOUSE-POSITION NIL) returns the cursor position
expressed in screen coordinates.

VIEW:           A simple view.
")
  (:method (view)
    (declare (ignore view))
    nil)
  (:method ((screen null))
    (nsscreen-to-screen-point (get-nspoint [NSEvent mouseLocation])))
  (:method ((view simple-view))
    (if (handle view)
        ;; We don't use the *current-event* since it may not be an event
        ;; relative to (view-window view).
        (let ((window (view-window view)))
         (nspoint-to-point
          (with-handle (winh window)
            #-(and)                     ; only for 10.6+
            (get-nsrect [winh convertRectFromScreen:(ns:make-ns-rect (nspoint-x pt) (nspoint-y pt) 1 1)])
            ;; deprecated, but 10.6+ doesn't work on ccl-1.8.
            (with-view-handle (viewh window) ;; TODO: viewh = contentView of window. therefore we convert to window coordinates, not view coordinates??? ; see also window-mouse
              (get-nspoint
               [viewh convertPoint:[winh convertScreenToBase:[NSEvent mouseLocation]]
                      fromView:*null*])))))
        ;; The following is not very meaningful, but then why a view
        ;; without a handle would need a view-mouse-position?
        (call-next-method))))

;; (point-to-list (view-mouse-position nil))
;; (view-convert-coordinates-and-click subview where view)


(defun modifier-flags ()
  #-cocoa-10.6
  (#_CGEventSourceFlagsState  #$kCGEventSourceStateCombinedSessionState)
  #+cocoa-10.6
  [NSEvent modifierFlags])


(defun mouse-down-p ()
  "
RETURN:         T if the mouse button is pressed and NIL
                otherwise. This function may be called at any time,
                not only during event processing.
"
  #-cocoa-10.6
  (loop
    :for button :below 2
    :thereis (not (zerop (#_CGEventSourceButtonState #$kCGEventSourceStateCombinedSessionState button))))
  #+cocoa-10.6
  (not (zerop [NSEvent pressedMouseButtons])))


(defun multi-click-count ()
  #-(and) (let ((nsevent [[NSApplication sharedApplication] currentEvent]))
            (if (and nsevent
                     (find [nsevent type] '#.(list #$NSLeftMouseDown #$NSRightMouseDown #$NSOtherMouseDown)))
                [nsevent clickCount]
                (if (mouse-down-p)
                    1
                    0)))
  *multi-click-count*)


(defun double-click-p ()
  "

RETURN:         T if the click currently being processed was the
                second half of a double-click.  Double-clicks take into
                account the timing as well as the spacing of
                consecutive clicks.

                The DOUBLE-CLICK-P function always returns NIL if
                called from outside event processing. It also returns
                false if the first click activated the window and
                WINDOW-DO-FIRST-CLICK is false.

"
  (let ((nsevent [[NSApplication sharedApplication] currentEvent]))
    (and nsevent
         (find [nsevent type] '#.(list #$NSLeftMouseDown #$NSRightMouseDown #$NSOtherMouseDown))
         (= 2 [nsevent clickCount])
         t)))

;; TODO: perhaps it would be preferable to use *current-event* to
;; track double-click-p etc, to keep them synchronized with the
;; mac-event state.
;;
;; (defun double-click-p ()  
;;   (and (and (boundp '*current-event*) *current-event*)
;;        (eq $MButDwnEvt (rref *current-event* eventrecord.what))
;;        (> *multi-click-count* 1)))



(defun double-click-spacing-p (point1 point2)
  "
DESCRIPTION:    The function DOUBLE-CLICK-SPACING-P is called by
                DOUBLE-CLICK-P to see whether two clicks should count
                as a DOUBLE-CLICK.  It is also used to determine
                whether to increment *MULTI-CLICK-COUNT*.  Macintosh
                guidelines specify that if the cursor is moved
                excessively between clicks, the clicks do not count as
                a DOUBLE-CLICK.

RETURN:         NIL if POINT1 and POINT2 are separated by more than 4
                pixels, horizontally or vertically.  If they are within
                4 pixels of each other, both horizontally and
                vertically, the function returns true.

POINT1:         The cursor position during the first click.

POINT2:         The cursor position during the second click.
"
  (and (< (abs (- (point-h point1) (point-h point2))) 4)
       (< (abs (- (point-v point1) (point-v point2))) 4)))


(defun command-key-p ()
  "
RETURN:         If called during event processing, return true if the
                command key was pressed during the event;
                otherwise, return NIL.

                If called outside of event processing, return true if
                the command key is currently pressed; otherwise,
                return NIL.
"
  (not (zerop (logand (modifier-flags) #$NSCommandKeyMask))))


(defun control-key-p ()
  "
RETURN:         If called during event processing, return true if the
                control key was pressed during the event;
                otherwise, return NIL.

                If called outside of event processing, return true if
                the control key is currently pressed; otherwise,
                return NIL.
"
  (not (zerop (logand (modifier-flags) #$NSControlKeyMask))))


(defun option-key-p ()
  "
RETURN:         If called during event processing, return true if the
                option  key was pressed during the event;
                otherwise, return NIL.

                If called outside of event processing, return true if
                the option key is currently pressed; otherwise,
                return NIL.
"
  (not (zerop (logand (modifier-flags) #$NSAlternateKeyMask))))


(defun shift-key-p ()
  "
RETURN:         If called during event processing, return true if the
                shift key was pressed during the event;
                otherwise, return NIL.

                If called outside of event processing, return true if
                the shift key is currently pressed; otherwise,
                return NIL.
"
  (not (zerop (logand (modifier-flags) #$NSShiftKeyMask))))


(defun caps-lock-key-p ()
  "
RETURN:         If called during event processing, return true if the
                caps-lock key was pressed during the event;
                otherwise, return NIL.

                If called outside of event processing, return true if
                the caps-lock key is currently pressed; otherwise,
                return NIL.
"
  (not (zerop (logand (modifier-flags) #$NSAlphaShiftKeyMask))))


(defun any-modifier-keys-p ()
    "
RETURN:         If called during event processing, return true if
                any modifier key was pressed during the event;
                otherwise, return NIL.

                If called outside of event processing, return true if
                any modifier key is currently pressed; otherwise,
                return NIL.
"
  (not (zerop (logand (modifier-flags)
                      (logior #$NSCommandKeyMask
                              #$NSControlKeyMask
                              #$NSAlternateKeyMask
                              #$NSShiftKeyMask
                              #$NSAlphaShiftKeyMask)))))



(defun test/event/1 ()
  (loop
    (sleep 1)
    (format t "~12A ~:[     ~;mouse~] ~2D ~:[      ~;dblclk~] ~:[   ~;cmd~] ~:[    ~;ctrl~] ~:[   ~;opt~] ~:[     ~;shift~] ~:[    ~;caps~]~%"
            (point-to-list (view-mouse-position nil))
            (mouse-down-p)
            (multi-click-count)
            (double-click-p)
            (command-key-p)
            (control-key-p)
            (option-key-p)
            (shift-key-p)
            (caps-lock-key-p))))


;; (defvar *in-wait-mouse-up-or-moved* nil)
;; (defun wait-mouse-up-or-moved ()
;;   (if  (and *the-timer* (%i> *timer-count* 0))
;;        (progn 
;;          (when *in-wait-mouse-up-or-moved*
;;            (error "Recursive call to wait-mouse-up-or-moved"))
;;          (let ((*in-wait-mouse-up-or-moved* t))
;;            (with-periodic-task-mask ($ptask_event-dispatch-flag t)  ;; don't let another process eat the mouse up event?
;;              (when (still-down)
;;                (rlet ((outpt :point)
;;                       (out-res :unsigned-integer))
;;                  (errchk (#_trackmouselocation (%null-ptr) outpt out-res))
;;                  (let ((what (%get-unsigned-word out-res)))
;;                    ;; T if mouse moved, key modifiers changed or ... , NIL iff mouse up
;;                    (not (eq what #$kmousetrackingmouseup))))))))
;;        (new-mouse-down-p)))


(defmacro with-timer (&body body)
  (niy with-timer)
  `(progn ,@body))

(defmacro with-event-processing-enabled  (&body body)
  (niy with-event-processing-enabled)
  `(progn ,@body))



(defparameter *last-mouse-down-time*     0)
(defparameter *last-mouse-down-position* 0)
(defparameter *last-null-event-time*     0)


(defun process-multi-clicks (event)
  (let ((when  (event-when  event))
        (where (event-where event)))
    (if (and (< (- when *last-mouse-down-time*) (get-dbl-time))
             (double-click-spacing-p where *last-mouse-down-position*))
        (incf *multi-click-count*)
        (setf *last-mouse-down-position* where
              *multi-click-count* 1))
    (setq *last-mouse-down-time* when)))


(defun process-event (event)
  ;; Note: we don't dispatch on menu events (menu key or menu select),
  ;; since those events are handled by Cocoa. 
  (let ((window  (front-window))
        (what    (event-what    event))
        (message (event-message event))
        (where   (event-where   event)))
    (when (= what mouse-down) 
      (process-multi-clicks event))
    (case what
      ((#.null-event)
       (let ((time *last-null-event-time*))
         (unless (= time (setq *last-null-event-time* (tick-count)))
           ;; (view-mouse-moved-event-handler window)
           (window-null-event-handler window))))
      ((#.mouse-down #.mouse-up)
       ;; NOTE: menu mouse events are processed by Cocoa.
       (when (typep message 'window)
         (window-event message)))
      ((#.key-down #.auto-key #.key-up)
       (if window
           (window-event window)
           (view-key-event-handler *application* (nth-value 1 (decode-key-message message)))))
      ((#.activate-evt #.update-evt)
       (window-event message))
      ((#.disk-evt)     #|nop|#)
      ((#.network-evt)  #|nop|#)
      ((#.driver-evt)   #|nop|#)
      ((#.app1-evt #.app2-evt #.app3-evt #.app4-evt)
       (application-event-handler *application* event))
      #-(and)
      ((#.os-evt)
       (when (= 1 (ldb (byte 8 24) (event-message event))) ; suspend or resume event
         (if (setq *foreground* (logbitp 0 (event-message event)))
             (application-resume-event-handler  *application*)
             (application-suspend-event-handler *application*))))
      (otherwise
       ;; TODO: handle AppleEvents
       #|nop|#))
    what))

(defun receive-appleevent ()
  (values))


(defvar *eventhooks-in-progress* nil)

(defun dispatch-eventhook ()
  (let ((eventhook (or (and *modal-dialog-on-top*
                            (caar *modal-dialog-on-top*)
                            (modal-dialog-eventhook (car *modal-dialog-on-top*)))
                       *eventhook*)))
    (flet ((process-eventhook (hook)
             (unless (member hook *eventhooks-in-progress*)
               (let ((*eventhooks-in-progress* (cons hook *eventhooks-in-progress*)))
                 (funcall hook)))))
      (if (listp eventhook)
          (dolist (item eventhook)
            (when (process-eventhook item)
              (return t)))
          (process-eventhook eventhook)))))


(defvar *event-mask* every-event)
(defun event-dispatch (&optional (idle *idle*) (event-mask *event-mask*))
  "
The EVENT-DISPATCH function is called periodically as a background
process.  The EVENT-DISPATCH function calls #_WaitNextEvent and binds
the value of *CURRENT-EVENT* for the duration of the event processing.
It then calls *EVENTHOOK* if *EVENTHOOK* is not NIL.  If *EVENTHOOK*
returns true, the processing of the event stops.  If *EVENTHOOK*
returns NIL, the event is passed to the system event handlers.
Finally, EVENT-DISPATCH checks for deferred Apple events.

If you create a program with a loop that checks for events, you should
probably include a call to EVENT-DISPATCH inside the loop.  This
improves the response time when events occur.


IDLE:           An argument representing whether the main Lisp process
                is idle. The default is the value of *IDLE*, which is
                true when the main Lisp process is idle and NIL
                otherwise.  The function EVENT-DISPATCH calls
                GET-NEXT-EVENT with an event and the value of IDLE.
"
  (let ((*current-event* (get-next-event idle event-mask)))
    (when *current-event*
      (unless (= null-event (event-what *current-event*))
        (format-trace 'event-dispatch *current-event*))
      (unless (dispatch-eventhook)
        (when (= null-event (process-event *current-event*))
          (receive-appleevent))))))



(defun application-is-active ()
  [[NSApplication sharedApplication] isActive])

(define-symbol-macro *foreground*
    (application-is-active))

(defvar *event-ticks* *foreground-event-ticks*)

(defun event-ticks ()
  "
RETURN:         The number of ticks (sixtieths of a second) between
                calls to EVENT-DISPATCH. This number is applicable
                when code is running. When Lisp is idling in the main
                read-eval-print loop, EVENT-DISPATCH is called as
                close to continuously as possible.  This value is
                reset on every suspend and resume event, according to
                the values in *FOREGROUND-EVENT-TICKS* and
                *BACKGROUND-EVENT-TICKS*.
"
  *event-ticks*)


(defun set-event-ticks (n)
  "
DO:             Set the number of ticks between calls to
                EVENT-DISPATCH to N.

                If N is too low, EVENT-DISPATCH is called too often,
                and the system may get bogged down by event
                processing.  If it is too high, the system may not
                respond smoothly to events. To keep the insertion bar
                blinking smoothly, for example, a sleep time of 12 to
                20 ticks is recommended.  This will yield 3 to 5 idle
                events per second.

                This function is called on every suspend and resume
                event, with the argument *FOREGROUND-EVENT-TICKS* or
                *BACKGROUND-EVENT-TICKS*.

N:              An integer determining the number of ticks.
"
  (setf *event-ticks* n))





(defun initialize/event ()
  #| nothing |#
  (values))


;;;; THE END ;;;;
