
(in-package "CCL")
(let ((*warn-if-redefine-kernel* nil))
  
 (defun y-or-n-p (&optional format-string &rest arguments &aux response)
   "Y-OR-N-P prints the message, if any, and reads characters from
   *QUERY-IO* until the user enters y or Y as an affirmative, or either
   n or N as a negative answer. It asks again if you enter any other
   characters."
   (declare (dynamic-extent arguments))
   (if (eq *current-process* *cocoa-event-process*)
      
       (ui:y-or-n-dialog (format nil "~?" format-string arguments)
                         :yes-text "Yes"
                         :no-text "No"
                         :cancel-text nil
                         :window-title "Yes or No?")
      
       (with-terminal-input
         (clear-input *query-io*)
         (loop
           (when format-string
             (fresh-line *query-io*)
             (apply 'format *query-io* format-string arguments))
           (princ " (y or n)  " *query-io*)
           (setq response (read-char *query-io*))
           ;; Consume input up to trailing newline
           (when (peek-char #\NewLine *query-io* nil)
             ;; And consume the #\newline
             (read-char *query-io*))
           (clear-input *query-io*)
           (if (char-equal response #\y) (return t))
           (if (char-equal response #\n) (return nil))
           (format *query-io* "Please answer y or n.")))))


  (defun yes-or-no-p (&optional format-string &rest arguments &aux response)
    "YES-OR-NO-P is similar to Y-OR-N-P, except that it clears the
   input buffer, beeps, and uses READ-LINE to get the strings
   YES or NO."
    (declare (dynamic-extent arguments))
    (if (eq *current-process* *cocoa-event-process*)
        
        (ui:y-or-n-dialog (format nil "~?" format-string arguments)
                          :yes-text "Yes"
                          :no-text "No"
                          :cancel-text nil
                          :window-title "Yes or No?")
        
        (with-terminal-input
          (loop
            (when format-string
              (fresh-line *query-io*)
              (apply 'format *query-io* format-string arguments))
            (princ " (yes or no)  " *query-io*)
            (format *query-io* "~A" #\Bell)
            (setq response (read-line *query-io*))
            (clear-input *query-io*)
            (when response
              (setq response (string-trim wsp response))
              (if (string-equal response "yes") (return t))
              (if (string-equal response "no") (return nil))
              (format *query-io* "Please answer yes or no.")))))))


;; (in-package "GUI")
;; 
;; (defun call-with-dob-data (thunk dob)
;;   (ui::with-error-file
;;     (ui::print-backtrace)
;;     (format *error-output* "call-with-dob-data thread=~S~%" *current-process*))
;;   (unless (eq *current-process* *cocoa-event-process*)
;;     (with-lock-grabbed ((dob-data-lock dob))
;;       (funcall thunk (dob-data dob)))))
;; 
;; (defun call-with-dob-output-data (thunk dob)
;;   (ui::with-error-file
;;     (ui::print-backtrace)
;;     (format *error-output* "call-with-dob-output-data thread=~S~%" *current-process*))
;;   (with-lock-grabbed ((dob-output-data-lock dob))
;;     (funcall thunk (dob-output-data dob))))



;;;; THE END ;;;;
