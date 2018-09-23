(defmacro with-mutex (mutex &body body)
  `(progn
     (unless (member (ccl:lock-name ,mutex) '("event-queue") :test (function string=))
       (ui::with-error-file
         (format *trace-output* "with-mutex ~S" (ccl:lock-name ,mutex))))
     (bt:with-lock-held (,mutex) ,@body)))
 
