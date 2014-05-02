(in-package :ui)

(load #P"~/works/patchwork/src/patchwork/src/pw-kernel/environment/lelisp-macros")
(load #P"~/works/patchwork/src/patchwork/src/packages.lisp")
(load #P"~/works/patchwork/src/patchwork/src/stream/redirecting-stream.lisp")

(defun identify-streams (&optional label)
  (dolist (stream-var '(*terminal-io*
                        *query-io*
                        *debug-io*
                        ;; *standard-input*
                        *standard-output*
                        *error-output*
                        *trace-output*))
    (let ((stream (symbol-value stream-var)))
      (format stream "~&~20S~@[(~A)~]~%" stream-var label)
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
#+swank (defvar swank::*current-terminal-io*)

(defun initialize-streams ()
  (setf *patchwork-io* (make-patchwork-io))
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

(initialize-streams)
