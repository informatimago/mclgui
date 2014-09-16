;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               mac-event.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    MacOS Toolbox-like Events.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2014-04-27 <PJB> Extracted from objc-classes.lisp.
;;;;BUGS
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
(objcl:enable-objcl-reader-macros)


;; event what:
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant null-event    0)
  (defconstant mouse-down    1)
  (defconstant mouse-up      2)
  (defconstant key-down      3)
  (defconstant key-up        4)
  (defconstant auto-key      5)
  (defconstant update-evt    6)
  (defconstant disk-evt      7)
  (defconstant activate-evt  8)
  (defconstant network-evt  10)
  (defconstant driver-evt   11)
  (defconstant app1-evt     12)
  (defconstant app2-evt     13)
  (defconstant app3-evt     14)
  (defconstant app4-evt     15)

  (defconstant every-event     #xffff)
  (defconstant mouse-down-mask (ash 1  1))
  (defconstant mouse-up-mask   (ash 1  2))
  (defconstant key-down-mask   (ash 1  3))
  (defconstant key-up-mask     (ash 1  4))
  (defconstant auto-key-mask   (ash 1  5))
  (defconstant update-mask     (ash 1  6))
  (defconstant disk-mask       (ash 1  7))
  (defconstant activate-mask   (ash 1  8))
  (defconstant network-mask    (ash 1 10))
  (defconstant driver-mask     (ash 1 11))
  (defconstant app1-mask       (ash 1 12))
  (defconstant app2-mask       (ash 1 13))
  (defconstant app3-mask       (ash 1 14))
  (defconstant app4-mask       (ash 1 15))

  (defconstant active-flag   1)
  (defconstant btn-state     128)
  (defconstant cmd-key       256)
  (defconstant shift-key     512)
  (defconstant alpha-lock    1024)
  (defconstant option-key    2048)
  );; eval-when


(defvar *event-what-labels* #(null-event mouse-down mouse-up key-down
                              key-up auto-key update-evt disk-evt
                              activate-evt network-evt driver-evt app1-evt
                              app2-evt app3-evt app4-evt))

(defun event-what-label (what)
  (if (<= null-event what app4-evt)
      (aref *event-what-labels* what)
      what))


(defvar *event-modifier-labels `((,active-flag :active)
                                 (,btn-state   :mouse-down)
                                 (,cmd-key     :command)
                                 (,shift-key   :shift)
                                 (,alpha-lock  :alpha-lock)
                                 (,option-key  :option)))

(defun event-modifiers-label (modifiers)
  (loop :for i from 0 to 15
        :when (logbitp i modifiers)
          :collect (or (second (assoc (expt 2 i) *event-modifier-labels)) i)))


(defstruct (event (:copier %copy-event))
  (what      0 :type integer)
  (message   0)
  (when      0 :type integer)
  (where     0 :type point)
  (modifiers 0 :type integer)
  (nsevent   nil))

(defun copy-event (event)
  (let ((copy (%copy-event event)))
    (when (event-nsevent copy)
      [(event-nsevent copy) retain])
    copy))

#|
Event type         event-message
keyboard           four bytes: not-used, not-used, key-code, character-code
activate, update   window
disk inserted      two half-words: file manager result code, drive number
mouse              undefined -- for the event forwared from a -[NSResponder mouseDown:], the window.
null               undefined 
network            parameter block
driver             cf. driver chapter
application        used defined.
|#


(defun event-key-code (event)
  (nth-value 0 (decode-key-message (event-message event))))

(defun event-character (event)
  (nth-value 1 (decode-key-message (event-message event))))

(defun event-modifierp (event modifier-key)
  (plusp (logand (event-modifiers event) modifier-key)))

(defmethod print-object ((event event) stream)
  (format stream "#S(~S" 'event)
  (format stream " :what ~A" (event-what-label (event-what event)))
  (format stream " :message ~S" (event-message event))
  (format stream " :when ~S" (event-when event))
  (format stream " :where #@(~A ~A)" (point-h (event-where event)) (point-v (event-where event)))
  (format stream " :modifiers (~D ~S)"  (event-modifiers event) (event-modifiers-label (event-modifiers event)))
  (format stream " :nsevent ~S"  (event-nsevent event))
  (format stream ")")
  event)


(defun assign-event (dst src)
  "
DO:             Copies the fields of SRC event to DST event.
RETURN:         DST.
"
  (setf (event-what      dst) (event-what      src)
        (event-message   dst) (event-message   src)
        (event-when      dst) (event-when      src)
        (event-where     dst) (event-where     src)
        (event-modifiers dst) (event-modifiers src))
  dst)


(defstruct queue (mutex (make-mutex)) head tail)
(defvar *event-queue* (make-queue :mutex (make-mutex "event-queue")))
(defun event-queue-length ()
  (length (queue-head *event-queue*)))

(defun post-event (event)
  (check-type event event)
  (let ((entry (list event)))
    (with-mutex (queue-mutex *event-queue*)
      (if (queue-tail *event-queue*)
          (setf (cdr (queue-tail *event-queue*)) entry
                (queue-tail *event-queue*) entry)
          (setf (queue-head *event-queue*) entry
                (queue-tail *event-queue*) entry))))
  (values))


(defun %dequeue-event ()
  "Remove the first event from the *EVENT-QUEUE*, and return it.
NOTE: should be called insinde (with-mutex (queue-mutex *event-queue*) …)"
  (let ((entry (queue-head *event-queue*)))
    (when entry
      (if (eql entry (queue-tail *event-queue*))
          (setf (queue-head *event-queue*) nil
                (queue-tail *event-queue*) nil)
          (setf (queue-head *event-queue*) (cdr (queue-head *event-queue*))))
      (car entry))))

(defun dequeue-event ()
  (with-mutex (queue-mutex *event-queue*)
    (%dequeue-event)))



(defun event-priority (event)
  (let ((what      (event-what event))
        (modifiers (event-modifiers event)))
    (case what
      ((#.activate-evt)
       (if (zerop (logand modifiers active-flag))
           1
           2))

      ((#.mouse-down #.mouse-up #.key-down #.key-up #.disk-evt
                     #.network-evt #.driver-evt #.app1-evt #.app2-evt #.app3-evt
                     #.app4-evt)
       3)
      ((#.auto-key)
       4)
      ((#.update-evt)
       (let ((pos (position (event-message event) (windows))))
         (if pos
             (+ 5 pos)
             (1- most-positive-fixnum))))
      (otherwise
       most-positive-fixnum))))

(defun %find-event (event-mask)
  "Return the first event in the event queue that matches the
EVENT-MASK, of the highest priority.
NOTE: should be called insinde (with-mutex (queue-mutex *event-queue*) …)."
  (first (sort (remove-if-not (lambda (event) (logbitp (event-what event) event-mask))
                              (queue-head *event-queue*))
               (function <) :key (function event-priority))))

(defun %extract-event (event)
  "Remove the event from the *EVENT-QUEUE*.
Return event when removed, NIL if not found in the queue.
NOTE: should be called inside (with-mutex (queue-mutex *event-queue*) …)"
  (let ((events (queue-head *event-queue*)))
    (if (eql event (car events))
        (%dequeue-event)
        (loop
          :until (or (null (cdr events))
                     (eql event (cadr events)))
          :do (pop events)
          :finally (return (when (cdr events)
                             (when (eql (cdr events) (queue-tail *event-queue*))
                               (setf (queue-tail *event-queue*) events))
                             (pop (cdr events))))))))



(defun get-null-event ()
  "RETURN: A new null event."
  (make-event :what null-event
              :when (tick-count)
              :where (get-mouse)
              :modifiers (modifiers)))

(defun get-next-event (&optional (idle *idle*) (mask every-event) sleep-ticks)
  "
DESCRIPTION:    The GET-NEXT-EVENT function calls #_WaitNextEvent to
                get an event.  It disables and reenables the clock
                sampled by GET-INTERNAL-RUNTIME.  (MultiFinder may do
                a context switch.)  After #_WaitNextEvent returns, the
                function reschedules the EVENT-DISPATCH task, which is
                the usual caller of GET-NEXT-EVENT.

EVENT:          An event record allocated on the stack or the heap.

IDLE:           Used to determine the default value of SLEEP-TICKS.
                The default value is *IDLE*, which is true if
                GET-NEXT-EVENT is called via EVENT-DISPATCH from the
                top-level loop when the Listener is waiting for input.

MASK:           This is the EventMask argument for #_WaitNextEvent, a
                fixnum.  The default is EVERY-EVENT.

SLEEP-TICKS:    This is the Sleep argument to #_WaitNextEvent.  It
                determines how many ticks are given to other
                applications under MultiFinder if no event is pending.
                The default is determined by the values of the idle
                argument and the global variables *IDLE-SLEEP-TICKS*,
                *FOREGROUND-SLEEP-TICKS*, and
                *BACKGROUND-SLEEP-TICKS*.  If Macintosh Common Lisp is
                running in the foreground, then the default is
                *IDLE-SLEEP-TICKS* if the value of idle is true;
                otherwise, the default is *FOREGROUND-SLEEP-TICKS*.  If
                Macintosh Common Lisp is running in the background,
                then the default is *BACKGROUND-SLEEP-TICKS* unless
                that value is NIL, in which case the default is the
                same as when Macintosh Common Lisp is running in the
                foreground.
"
  (declare (ignore idle sleep-ticks))
  (with-mutex (queue-mutex *event-queue*)
    (let ((event (%find-event mask)))
      (if event
          (%extract-event event)
          (get-null-event)))))


(defun event-avail (event-mask)
  "Same as GET-NEXT-EVENT, but doesn't dequeue the event.
NOTE: This returns a copy of the event in the queue."
  (with-mutex (queue-mutex *event-queue*)
    (let ((event (%find-event event-mask)))
      (when event
        (copy-event event)))))


(defvar *current-window* nil) ; current GrafPort
;; (setf  *current-window* nil )






(defun get-mouse ()
  "Current mouse location, given in the local coordinate system of the
current window, or of the screen if there is no window."
  (let ((win (or *current-window* (front-window))))
    (if (and win (handle win))
        (window-mouse win)
        (screen-mouse))))


(defun button ()
  "Whether the mouse button is currently down.  (When multiple mouse
buttons are available, whether any of them is down)."
  (any-button-down))


(defun still-down ()
  "Tests whether the mouse-button is still down.  Returns true if the
button is currently down and there are no more mouse events pending in
the event queue."
  (with-mutex (queue-mutex *event-queue*)
    (and (button) (not (%find-event mouse-up-mask)))))

(defun wait-mouse-up ()
  "Same as STILL-DOWN, but if the button is not still down from the
original press, WAIT-MOUSE-UP removes the preceding mouse-up event
before returning NIL."
  (with-mutex (queue-mutex *event-queue*)
    (and (button)
         (let ((event (%find-event mouse-up-mask)))
           (if event
               (progn (%extract-event event)
                      nil)
               t)))))


(defun get-keys ()
  "Reads the current state of the keyboard and returns it in the form of a keymap."
  ;; TODO: get-keys
  #128*0)


(defconstant +tick-per-second+ 60 "Number of ticks per second.")

(defun timestamp ()
  "RETURN: The time in second since startup."
  (/ (get-internal-real-time) internal-time-units-per-second))

(defun tick-count ()
  "Current number of ticks (sixtieths of a second) since the system
last started up."
  (truncate (timestamp) (/ +tick-per-second+)))

(defvar *double-click-jitter* 4
  "Maximum mouse movement distance during a double-click.")
(defvar *double-click-time* 50
  "Suggested maximum difference (in ticks) that should exist between
the times of a mouse-up event and a mouse-down event for those two
mouse clicks to be considered a double-click.")

(defun get-dbl-time ()
  "Suggested maximum difference (in ticks) that should exist between
the times of a mouse-up event and a mouse-down event for those two
mouse clicks to be considered a double-click."
  *double-click-time*)

(defvar *caret-time* 32
  "The time (in ticks) between blinks of the 'caret'.")

(defun get-caret-time ()
  "The time (in ticks) between blinks of the 'caret'."
  *caret-time*)






(defun test/post-event/dequeue-event ()
  (let ((e1 (make-event :what mouse-down :when (tick-count) :where #@(100 100)))
        (e2 (make-event :what key-down :when (+ 10 (tick-count)) :message (char-code #\a)))
        (e3 (make-event :what mouse-up :when (+ 20 (tick-count))  :where #@(200 150))))
    (let ((*event-queue* (make-queue)))
      (post-event e1)
      (post-event e2)
      (post-event e3)
      (let ((e (dequeue-event))) (assert (eql e1 e) (e e1)))
      (let ((e (dequeue-event))) (assert (eql e2 e) (e e2)))
      (let ((e (dequeue-event))) (assert (eql e3 e) (e e3)))
      (let ((e (dequeue-event))) (assert (null  e) (e))))
    :success))

(defun test/%find-event ()
  (let ((e1 (make-event :what mouse-down :when (tick-count) :where #@(100 100)))
        (e2 (make-event :what key-down :when (+ 10 (tick-count)) :message (char-code #\a)))
        (e3 (make-event :what mouse-up :when (+ 20 (tick-count))  :where #@(200 150))))
    (let ((*event-queue* (make-queue)))
      (post-event (make-event))
      (post-event e1)
      (post-event e2)
      (post-event e3)
      (post-event (make-event))
      (with-mutex (queue-mutex *event-queue*)
        (let ((e (%find-event mouse-down-mask))) (assert (eql e e1) (e e1)))
        (let ((e (%find-event key-down-mask)))   (assert (eql e e2) (e e2)))
        (let ((e (%find-event mouse-up-mask)))   (assert (eql e e3) (e e3)))
        (let ((e (%find-event activate-mask)))   (assert (null e)  (e)))
        (let ((e (%find-event (+ mouse-down-mask
                                 mouse-up-mask)))) (assert (eql e e1) (e e1)))))
    :success))

(defun test/%extract-event ()
  (let ((e1 (make-event :what mouse-down :when (tick-count) :where #@(100 100)))
        (e2 (make-event :what key-down :when (+ 10 (tick-count)) :message (char-code #\a)))
        (e3 (make-event :what mouse-up :when (+ 20 (tick-count))  :where #@(200 150))))
    (let ((*event-queue* (make-queue)))
      (post-event e1)
      (post-event e2)
      (post-event e3)
      (with-mutex (queue-mutex *event-queue*)
        (let* ((x (%find-event mouse-down-mask))
               (e (%extract-event x)))
          (assert (eql e e1) (e e1)))
        (let ((e (%find-event mouse-down-mask))) (assert (null e)  (e)))
        (let ((e (%find-event key-down-mask)))   (assert (eql e e2) (e e2)))
        (let ((e (%find-event mouse-up-mask)))   (assert (eql e e3) (e e3))))
      (let ((e (dequeue-event))) (assert (eql e2 e) (e e2)))
      (let ((e (dequeue-event))) (assert (eql e3 e) (e e3)))
      (let ((e (dequeue-event))) (assert (null e) (e))))
    (let ((*event-queue* (make-queue)))
      (post-event e1)
      (post-event e2)
      (post-event e3)
      (with-mutex (queue-mutex *event-queue*)
        (let* ((x (%find-event key-down-mask))
               (e (%extract-event x)))
          (assert (eql e e2) (e e2)))
        (let ((e (%find-event mouse-down-mask))) (assert (eql e e1) (e e1)))
        (let ((e (%find-event key-down-mask)))   (assert (null e) (e)))
        (let ((e (%find-event mouse-up-mask)))   (assert (eql e e3) (e e3))))
      (let ((e (dequeue-event))) (assert (eql e1 e) (e e1)))
      (let ((e (dequeue-event))) (assert (eql e3 e) (e e3)))
      (let ((e (dequeue-event))) (assert (null e) (e))))
    (let ((*event-queue* (make-queue)))
      (post-event e1)
      (post-event e2)
      (post-event e3)
      (with-mutex (queue-mutex *event-queue*)
        (let* ((x (%find-event mouse-up-mask))
               (e (%extract-event x)))
          (assert (eql e e3) (e e3)))
        (let ((e (%find-event mouse-down-mask))) (assert (eql e e1) (e e1)))
        (let ((e (%find-event key-down-mask)))   (assert (eql e e2) (e e2)))
        (let ((e (%find-event mouse-up-mask)))   (assert (null e)  (e))))
      (let ((e (dequeue-event))) (assert (eql e1 e) (e e1)))
      (let ((e (dequeue-event))) (assert (eql e2 e) (e e2)))
      (let ((e (dequeue-event))) (assert (null e) (e))))
    (let ((*event-queue* (make-queue)))
      (post-event e1)
      (post-event e2)
      (post-event e3)
      (with-mutex (queue-mutex *event-queue*)
        (let ((e (%extract-event (make-event))))
          (assert (null e) (e))))
      (let ((e (dequeue-event))) (assert (eql e1 e) (e e1)))
      (let ((e (dequeue-event))) (assert (eql e2 e) (e e2)))
      (let ((e (dequeue-event))) (assert (eql e3 e) (e e3)))
      (let ((e (dequeue-event))) (assert (null e) (e))))
    :success))


(defun test/mac-event ()
  (test/post-event/dequeue-event)
  (test/%find-event)
  (test/%extract-event))


(test/mac-event)

'(event event-p copy-event make-event
  even-what event-message event-when event-where event-modifiers
  assign-event post-event
  get-next-event event-avail get-mouse button still-down wait-mouse-up
  get-keys tick-count get-dbl-time get-caret-time
  null-event mouse-down mouse-up key-down key-up
  auto-key update-evt disk-evt activate-evt network-evt
  driver-evt app1-evt app2-evt app3-evt app4-evt
  every-event mouse-down-mask mouse-up-mask key-down-mask
  key-up-mask auto-key-mask update-mask disk-mask
  activate-mask network-mask driver-mask app1-mask
  app2-mask app3-mask app4-mask
  active-flag btn-state cmd-key shift-key alpha-lock
  option-key)

;;;; THE END ;;;;
