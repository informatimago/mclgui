(in-package :ui)

(unless (find-package "MIDI")
  (let ((midi (make-package "MIDI" :use '())))
    (export (mapcar (lambda (name) (intern name midi))
                    '("CLOCK-TIME" "MIDI-READ" "MIDI-WRITE" "MIDI-WRITE-TIME"))
            midi)))

(load #P"PATCHWORK:SRC;PW-KERNEL;ENVIRONMENT;LELISP-MACROS.LISP")
(load #P"PATCHWORK:SRC;PACKAGES.LISP")
(load #P"PATCHWORK:SRC;STREAM;REDIRECTING-STREAM.LISP")

(defun identify-streams (&key label verbose)
  (dolist (stream-var '(*terminal-io*
                        *query-io*
                        *debug-io*
                        ;; *standard-input*
                        *standard-output*
                        *error-output*
                        *trace-output*))
    (let ((stream (symbol-value stream-var)))
      (format stream "~&~20S~@[~20<(~A)~;~>~]~@[~S~]~%" stream-var label (and verbose stream))
      (finish-output stream))))



(defun make-patchwork-io ()
  #-cocoa
  *terminal-io*
  #+cocoa
  (make-two-way-stream
   (make-instance 'redirecting-stream:redirecting-character-input-stream
                  :input-stream-function
                  (let ((default-stream
                          (com.informatimago.common-lisp.cesarum.stream:stream-input-stream
                           *terminal-io*)))
                    (lambda ()
                      (or (hemlock-ext:top-listener-input-stream)
                          default-stream))))
   (make-instance 'redirecting-stream:redirecting-character-output-stream
                  :output-stream-function
                  (let ((default-stream
                          (com.informatimago.common-lisp.cesarum.stream:stream-output-stream
                           *terminal-io*)))
                    (lambda ()
                      (or (hemlock-ext:top-listener-output-stream)
                          default-stream))))))


(defvar *patchwork-io* (make-synonym-stream '*terminal-io*))
(defvar *patchwork-io-initialized* nil)
#+swank (defvar swank::*current-terminal-io*)

(defun initialize-streams ()
  (unless *patchwork-io-initialized*    
    (setf *patchwork-io* (make-patchwork-io)
          *patchwork-io-initialized* t))
  #+swank (setf swank::*current-terminal-io* *patchwork-io*)
  (let ((stream (make-synonym-stream '*terminal-io*)))
    (setf *terminal-io*       *patchwork-io*
          *standard-input*    stream
          *standard-output*   stream
          *error-output*      stream
          ;; *trace-output*      stream ;; TODO: redirect to stderr (NSLog) or a trace file in production.
          *query-io*          stream
          *debug-io*          stream
          *package*           (find-package "MCLGUI"))))

;; (setf *patchwork-io-initialized* nil)

(initialize-streams)
(identify-streams :label :app :verbose t)

(application-eval-enqueue *application* (lambda ()
                                          (initialize-streams)
                                          (identify-streams :label :app :verbose t)))
(application-eval-enqueue *application* (lambda ()
                                          (identify-streams :label :app :verbose t)))
