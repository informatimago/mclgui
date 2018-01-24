;;;-*-Mode: LISP; Package: GUI -*-
;;;
;;; Patches to the cocoa-listener to support output from the gui thread.


(in-package "GUI")



;; Printing from the GUI thread uses a different mechanism:
;; all the output is buffered, and it's written directly to the hemlock view upon finish-output.
;; The dob mechanism shall ignore attempts from the GUI thread.


(defclass cocoa-listener-output-stream (fundamental-character-output-stream)
  ((buffer :initform (make-double-output-buffer $listener-flush-limit))
   (gui-buffer :initform nil)  ; lazy initialization
   (hemlock-view :initarg :hemlock-view)))

(defmethod stream-element-type ((stream cocoa-listener-output-stream))
  (with-slots (buffer) stream
    (array-element-type (dob-data buffer))))




(defun display-cocoa-listener-output-buffer (stream)
  (with-slots (hemlock-view buffer gui-buffer) stream
    (when (eq *current-process* *cocoa-event-process*)
      (when (and gui-buffer (plusp (fill-pointer gui-buffer)))
        (let ((data nil))
          (rotatef data gui-buffer)
          (append-output hemlock-view data)))
      (unwind-protect
           (with-dob-output-data (data buffer)
             (when (and data (plusp (fill-pointer data)))
               (append-output hemlock-view data)
               (setf (fill-pointer data) 0)))
        (dob-return-output-data buffer)))))



(defun gui-ensure-buffer (stream)
  (with-slots (gui-buffer) stream
    (or gui-buffer
        (setf gui-buffer (make-array (1+ $listener-flush-limit)
                                     :adjustable t
                                     :fill-pointer 0
                                     :element-type 'character)))))

(defun gui-write-char (stream char)
  (let ((buffer (gui-ensure-buffer stream)))
    (vector-push-extend char buffer (array-dimension buffer 0))))

(defun gui-write-string (stream string start end)
  (let* ((buffer   (gui-ensure-buffer stream))
         (start    (or start 0))
         (end      (or end (length string)))
         (size     (- end start))
         (start1   (fill-pointer buffer))
         (capacity (array-dimension buffer 0)))
    (if (< size (- capacity (length buffer)))
        (incf (fill-pointer buffer) size)
        (with-slots (gui-buffer) stream
          (setf buffer
                (setf gui-buffer (adjust-array buffer (max (+ capacity size) (* 2 capacity))
                                               :fill-pointer (+ (fill-pointer buffer) size)
                                               :element-type 'character)))))
    (replace buffer string :start1 start1 :start2 start :end2 end)))

(defun gui-stream-line-column (stream)
  (let ((buffer (gui-ensure-buffer stream)))
    (- (length buffer) (or (position #\newline buffer :from-end t) 0))))

(defun gui-stream-force-output (stream)
  (display-cocoa-listener-output-buffer stream))

(defun gui-stream-clear-output (stream)
  (let ((buffer (gui-ensure-buffer stream)))
    (setf (fill-pointer buffer) 0)0))




(defun dob-push-string (stream string start end)
  (loop
    (with-slots (buffer) stream
      (let ((dob buffer))
        (with-dob-data (data dob)
          (let* ((start1     (length data))
                 (new-length (+ start1 (- end start))))
            (if (< new-length (array-dimension data 0))
                (progn (setf (fill-pointer data) new-length)
                       (replace data string :start1 start1 :start2 start :end2 end)
                       (return-from dob-push-string
                         (when (>= (length data) (dob-flush-limit dob))
                           (dob-queue-output-data dob t)
                           t)))
                (let ((new-length (1- (array-dimension data 0)))
                      (new-end    (- (+ new-length start) start1)))
                  (setf (fill-pointer data) new-length)
                  (replace data string :start1 start1 :start2 start :end2 new-end)
                  (setf start new-end)
                  (dob-queue-output-data dob t)))))))))

(defmethod ccl:stream-write-char ((stream cocoa-listener-output-stream) char)
  (if (eq *current-process* *cocoa-event-process*)
      (gui-write-char stream char)
      (with-slots (buffer) stream
        (when (dob-push-char buffer char)
          (queue-for-gui
           (lambda () (display-cocoa-listener-output-buffer stream)))))))

(defmethod ccl:stream-write-string ((stream cocoa-listener-output-stream) string &OPTIONAL start end)
  (format *trace-output* "~&ccl:stream-write-string ((stream cocoa-listener-output-stream) ~%
                                    ~S &OPTIONAL start=~S end=~S)~%" string start end)
  (format *trace-output* "~&(eq *current-process* *cocoa-event-process*) = ~S~%" (eq *current-process* *cocoa-event-process*))
  (if (eq *current-process* *cocoa-event-process*)
      (gui-write-string stream string start end)
      (when (dob-push-string stream string (or start 0) (or end (length string)))
        (queue-for-gui (lambda () (display-cocoa-listener-output-buffer stream))))))

(defmethod ccl:stream-write-string ((s deferred-cocoa-listener-output-stream)
                                    string &OPTIONAL start end)
  (format *trace-output* "~&ccl:stream-write-string ((s deferred-cocoa-listener-output-stream) ~%
                                    ~S &OPTIONAL start=~S end=~S)~%" string start end)
  (with-autorelease-pool
    (stream-write-string (underlying-output-stream s) string start end)))




;; This isn't really thread safe, but it's not too bad...  I'll take a chance - trying
;; to get it to execute in the gui thread is too deadlock-prone.
(defmethod hemlock-listener-output-mark-column ((view hi::hemlock-view))
  (let* ((output-region (hi::variable-value 'hemlock::current-output-font-region
                                            :buffer (hi::hemlock-view-buffer view))))
    (hi::mark-charpos (hi::region-end output-region))))


;; TODO: doesn't do the right thing for embedded tabs (in buffer or data)
(defmethod ccl:stream-line-column ((stream cocoa-listener-output-stream))
  (if (eq *current-process* *cocoa-event-process*)
      (gui-stream-line-column stream)
      (with-slots (hemlock-view buffer) stream
        (with-dob-data (data buffer)
          (let* ((n (length data))
                 (pos (position #\Newline data :from-end t)))
            (if pos
                (- n pos 1)
                (with-dob-output-data (output-data buffer)
                  (let* ((output-n (if output-data (length output-data) 0))
                         (output-pos (and (> output-n 0)
                                          (position #\Newline output-data :from-end t))))
                    (if output-pos
                        (+ n (- output-n output-pos 1))
                        (+ (hemlock-listener-output-mark-column hemlock-view)
                           n output-n))))))))))

(defmethod ccl:stream-fresh-line ((stream cocoa-listener-output-stream))
  (unless (eql 0 (ccl:stream-line-column stream))
    (ccl:stream-write-char stream #\Newline)))

(defmethod ccl::stream-finish-output ((stream cocoa-listener-output-stream))
  (ccl:stream-force-output stream))

(defmethod ccl:stream-force-output ((stream cocoa-listener-output-stream))
  (if (eq *current-process* *cocoa-event-process*)
      (gui-stream-force-output stream)
      (with-slots (buffer) stream
        (with-dob-data (data buffer)
          data
          (when (dob-queue-output-data buffer)
            (queue-for-gui #'(lambda () (stream-force-output stream))))))))

(defmethod ccl:stream-clear-output ((stream cocoa-listener-output-stream))
  (if (eq *current-process* *cocoa-event-process*)
      (gui-stream-clear-output stream)
      (with-slots (buffer) stream
        (with-dob-data (data buffer)
          (setf (fill-pointer data) 0)))))

(defmethod ccl:stream-line-length ((stream cocoa-listener-output-stream))
  (with-slots (hemlock-view) stream
    (values (hemlock-view-size hemlock-view))))


;;;; THE END ;;;;
