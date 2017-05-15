(in-package :ui)

(defun m (seq &key (key (function identity)))
  (map nil (lambda (element)
             (print (funcall key element)))
    seq)
  (values))


(defun hemlock-listener-window-process (window)
  (find (handle window) (remove-if-not (lambda (process)
                                         (typep process 'gui::cocoa-listener-process)) 
                                       (bt:all-threads))
        :key (function gui::cocoa-listener-process-window)))

(defun hemlock-windows ()  (windows :class 'hemlock-listener-frame))


(defun make-listener-io ()
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
                      (let ((hemlock-stream (hemlock-ext:top-listener-output-stream)))
                        (if (and hemlock-stream
                                 #+ccl (gui::dob-output-data (slot-value hemlock-stream 'gui::buffer)))
                            hemlock-stream
                            default-stream)))))))

#-(and)
(progn
  
  (mapcar (lambda (process) (list (gui::cocoa-listener-process-input-stream process)
                                  (gui::cocoa-listener-process-output-stream process)
                                  (gui::cocoa-listener-process-window process)))
          (remove-if-not (lambda (process) (typep process 'gui::cocoa-listener-process)) 
                         (bt:all-threads)))


  (defun hemlock-listener-window-process (window)
    (find (ui::handle window)
          (remove-if-not (lambda (process)
                           (typep process 'gui::cocoa-listener-process)) 
                         (bt:all-threads))
          :key (function gui::cocoa-listener-process-window)))

  (defun hemlock-windows ()  (windows :class 'hemlock-listener-frame))

  (let ((windows (hemlock-windows)))
    (format t "~&hemlock-windows ~S~%"  windows)
    (if windows
        (let ((process (hemlock-listener-window-process (first (hemlock-windows)))))
          (setf *patchwork-io* (make-two-way-stream (gui::cocoa-listener-process-input-stream process)
                                                    (gui::cocoa-listener-process-output-stream process))))
        ))


  
  );;#-(and)



(defun gui-command-open-eval-expression-window ()
  (let ((width 1000))
    (let (w e r b)
      (flet ((eval-expression (b)
               (handler-case
                   (progn
                     (format t "~&Hello frmo eval-expression!~%")
                     (set-dialog-item-text r (format nil "~{~S~^; ~}"
                                                     (multiple-value-list
                                                      (eval
                                                       (read-from-string
                                                        (dialog-item-text e)))))))
                 (error (err)
                   (set-dialog-item-text r (substitute #\space #\newline
                                                       (princ-to-string err)))))))
        (setf w (make-instance 'dialog
                               :window-title "Expression"
                               :view-size (make-point width 200)))
        (setf e (make-instance 'editable-text-dialog-item
                               :dialog-item-action (function eval-expression)
                               :view-position #@(10 10)
                               :view-size (make-point (- width 20) 20)))
        (setf r (make-instance 'static-text-dialog-item
                               :view-position #@(10 40)
                               :view-size (make-point (- width 20) 20)))
        (setf b (make-instance 'button-dialog-item
                               :dialog-item-enabledp t 
                               :dialog-item-action (function eval-expression)
                               :default-button t
                               :dialog-item-text "Eval"
                               :view-position #@(10 80)
                               :view-size (make-point (- width 20) 20)))
        (add-subviews w e r b)
        w))))
