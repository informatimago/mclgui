;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               mclgui.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Defines general MCLGUI functions.
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Initialize the streams.
;;;

(defun hemlock-listener-window-process (window)
  (find (ui::handle window)
        (remove-if-not (lambda (process)
                         (typep process 'gui::cocoa-listener-process))
                       (bt:all-threads))
        :key (function gui::cocoa-listener-process-window)))

(defun hemlock-windows ()  (windows :class 'hemlock-listener-frame))

#-(and)
(let ((windows (hemlock-windows)))
  (format t "~&hemlock-windows ~S~%"  windows)
  (if windows
      (let ((process (hemlock-listener-window-process (first (hemlock-windows)))))
        (setf *patchwork-io* (make-two-way-stream (gui::cocoa-listener-process-input-stream process)
                                                  (gui::cocoa-listener-process-output-stream process))))
      ))


;; (defun ccl::display-cocoa-listener-output-buffer (stream)
;;   (with-slots (ccl::hemlock-view gui::buffer) stream
;;     (unwind-protect
;;          (gui::with-dob-output-data (gui::data ccl::buffer)
;;            (when (and gui::data (> (fill-pointer gui::data) 0))
;;              (gui::append-output ccl::hemlock-view gui::data)
;;              (setf (fill-pointer gui::data) 0)))
;;       (gui::dob-return-output-data gui::buffer))))

;; (com.informatimago.common-lisp.cesarum.stream:stream-input-stream  *terminal-io*)
;; (com.informatimago.common-lisp.cesarum.stream:stream-output-stream *terminal-io*)


(defvar *listener-io-queue* nil "For debugging.")

(defun make-listener-io ()
  #-cocoa
  *terminal-io*

  #|

  We should return a stream that reads and writes from the hemlock
  listener window.  However, such a listener window is not always
  available (and when the window is present, the stream in it may not
  be ready (dob-output-data may be nil).  Furthermore, it looks like
  the GUI thread just cannot write to the listener windows.

  In this case, we enqueue instead
  the data that must be output, and we will write it when the listener
  window stream becomes available.

  This is implemented by using two redirecting-streams, and a
  filter-stream is used to enqueue data written to it.


  Note: functions enqueued with ccl:process-interrupt are called in
  "random" order (eg. LIFO).  Therefore we cannot use them
  directly to display ordered data: we need to go thru a buffer.

 (eq *current-process* *cocoa-event-process*)
  |#

  #+cocoa
  (flet ((input-stream  ()
           (hemlock-ext:top-listener-input-stream))
         (output-stream ()
           (let ((hemlock-stream (hemlock-ext:top-listener-output-stream)))
             (when (and hemlock-stream
                        #+ccl (slot-value hemlock-stream 'gui::buffer))
               hemlock-stream))))

      (make-two-way-stream
       (make-instance 'redirecting-character-input-stream
                      :input-stream-function
                      (let ((default-stream (make-string-input-stream "")))
                        (lambda () (or (input-stream) default-stream))))
       (make-instance 'redirecting-character-output-stream
                      :output-stream-function

                      (let ((queue      (setf *listener-io-queue* (make-queue "Output")))
                            (flush-lock (bt:make-lock "Output Flush Lock")))
                        (labels ((flush-lower ()
                                   (bt:with-lock-held (flush-lock)
                                    (loop
                                      :until (queue-emptyp queue)
                                      :do (write-string (dequeue queue)))))
                                 (flush ()
                                   (let ((process (gui::top-listener-process)))
                                     (when process
                                       (ccl:process-interrupt process (function flush-lower))))))
                          (let ((output-stream
                                   (make-output-filter-stream
                                    queue
                                    (lambda (operation queue &rest arguments)
                                      (ecase operation
                                        ;; character
                                        (write-char
                                         (if (eql (bt:current-thread) (gui::top-listener-process))
                                             (write-char (first arguments))
                                             (progn
                                               (enqueue queue (string (first arguments)))
                                               (flush))))
                                        (write-string
                                         (let ((string (subseq (first arguments) (second arguments) (third arguments))))
                                          (if (eql (bt:current-thread) (gui::top-listener-process))
                                              (write-string string)
                                              (progn
                                                (enqueue queue string)
                                                (flush)))))
                                        ;; both:
                                        (write-sequence
                                         (let ((string (coerce (subseq (first arguments) (second arguments) (third arguments))
                                                                'string)))
                                          (if (eql (bt:current-thread) (gui::top-listener-process))
                                              (write-string string)
                                              (progn
                                                (enqueue queue string)
                                                (flush)))))
                                        (close          #|ignore|#)))
                                    :element-type 'character)))
                            (lambda ()
                              output-stream))))))))


(defvar *old-terminal-io* (make-synonym-stream '*terminal-io*))
(defvar *application-io*  *old-terminal-io*)

#+swank (defvar swank::*current-terminal-io* *old-terminal-io*)


(defun initialize-streams ()
  (setf *old-terminal-io* *terminal-io*)
  (setf *application-io* (make-listener-io))
  #+swank (setf swank::*current-terminal-io* *application-io*)
  (let ((stream (make-synonym-stream '*terminal-io*)))
    (setf *terminal-io*       *application-io*
          *standard-input*    stream
          *standard-output*   stream
          *error-output*      stream
          ;; *trace-output*      stream ;; TODO: redirect to stderr (NSLog) or a trace file in production.
          *query-io*          stream
          *debug-io*          stream)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Initialize the screen.
;;;

(defun initialize/screen ()
  (multiple-value-bind (sx sy sw sh) (main-screen-frame)
    (declare (ignore sx sy))
    (setf *screen-width*   sw
          *screen-height*  sh))
  (values))


(defvar *initialized* nil)

(defun initialize ()
  "Initialize the MCL GUI."
  (unless *initialized*
    (progn
      (ui::reporting-errors (mclgui.wrapper::initialize/wrapper))
      (ui::reporting-errors (initialize-streams))
      (ui::reporting-errors (initialize-event-environment-bindings))
      (ui::reporting-errors (initialize/process))
      (ui::reporting-errors (initialize/application))
      (ui::reporting-errors (initialize/paragraph-style))
      (ui::reporting-errors (initialize/screen))
      (ui::reporting-errors (initialize/region))
      (ui::reporting-errors (initialize/color))
      (ui::reporting-errors (initialize/pattern))
      (ui::reporting-errors (initialize/pen))
      (ui::reporting-errors (initialize/cursor))
      (ui::reporting-errors (initialize/scrap))
      (ui::reporting-errors (initialize/font))
      (ui::reporting-errors (initialize/menu))
      (ui::reporting-errors (initialize/view))
      (ui::reporting-errors (initialize/window))
      (ui::reporting-errors (te-init))
      (ui::reporting-errors (initialize/table-dialog-item))
      (ui::reporting-errors (initialize/file))
      (ui::reporting-errors (initialize/event))
      (ui::reporting-errors (initialize/eval))
      (ui::reporting-errors (initialize/pop-up-menu-dialog-item))
      #+has-appleevent (ui::reporting-errors
                         (when (fboundp 'initialize/apple-event)
                           (initialize/apple-event))))
    (setf *initialized* t))
  (values))


;;; --- ccl repl

(defun safe-repl (&rest arguments &key &allow-other-keys)
  (loop
    (handler-bind ((error (function invoke-debugger)))
      (apply (function ccl::read-loop) arguments))))

(on-restore ccl-repl
  (setf ccl::*read-loop-function* 'safe-repl
        ccl::*inhibit-greeting*         t
        ccl::*did-show-marketing-blurb* t))

;;; --- trace output saved to file.

(defvar *patchwork-trace-output* *trace-output*)

(defun redirect-trace-output-to-file (pathname)
  (setf *patchwork-trace-output* (open pathname
                                       :direction :output
                                       :if-does-not-exist :create
                                       :if-exists :append
                                       #+ccl :sharing #+ccl :lock))
  (setf *trace-output* *patchwork-trace-output*)
  (setf (ui::aget ui::*event-environment-bindings* '*trace-output*)
        *patchwork-trace-output*)
  (format *trace-output* "~%~A~2%" (date)))

(on-restore patchwork-trace
  (ui::reporting-errors
    (redirect-trace-output-to-file (merge-pathnames #P"Desktop/Patchwork-trace.txt"
                                                    (user-homedir-pathname)))
    (format-trace "Welcome to the Machine!")))

;;; --------------------------------------------------------------------
;;; Initialization of patchwork

;; (on-startup patchwork-initialization
;;   (ui::reporting-errors
;;     ;; in ccl-1.11, ccl::*application* is still nil here.
;;     (let ((ccl::*application* (or ccl::*application* t)))
;;       (eval-enqueue '(initialize-patchwork)))))
;;
;; (setf (symbol-function 'patchwork-initialization)
;;       (lambda nil (block patchwork-initialization
;;                     (let ((ccl::*application*
;;                             (or ccl::*application* t)))
;;                       (eval-enqueue '(initialize-patchwork))))))

;;;; THE END ;;;;
