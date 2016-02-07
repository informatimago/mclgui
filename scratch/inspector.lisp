(defpackage "COM.INFORMATIMAGO.COMMON-LISP.MCLGUI.INSPECTOR"
  (:use "COMMON-LISP" "MCLGUI")
  (:export "MAKE-INSPECTOR-WINDOW"))
(in-package "COM.INFORMATIMAGO.COMMON-LISP.MCLGUI.INSPECTOR")



(defgeneric view-class-for-object (object)
  (:method ((object t))       'atom-view)
  (:method ((object package)) 'package-view))

(defgeneric adjust-size (view))
(defgeneric displayed-text (view))


(defclass inspector-window (window)
  ((displayed-objects :initform '()
                      :initarg :displayed-objects
                      :accessor displayed-objects)))


(defclass atom-view (simple-view)
  ((object :initarg :object :reader displayed-object)))

(defmethod displayed-text ((view atom-view))
  (let ((*print-circle*   t)
        (*print-length*   4)
        (*print-level*    4)
        (*print-readably* nil)
        (object           (displayed-object view)))
    (if (and (symbolp object) (not (keywordp object))
             (symbol-package object))
        (let ((*package* (symbol-package object)))
          (prin1-to-string object))
        (prin1-to-string object))))





(defmethod adjust-size ((view atom-view))
  (multiple-value-bind (ff ms) (view-font-codes view)
    (set-view-size view
                   (+ 6 (font-codes-string-width (displayed-text view) ff ms))
                   (+ 2 (font-codes-line-height ff ms)))))

(defmethod view-draw-contents ((view atom-view))
  (with-focused-view view
    (let ((bounds (view-bounds view)))
      (offset-rect bounds 2 0)
      (draw-string-in-rect (displayed-text view) bounds)
      (offset-rect bounds -2 0)
      (draw-rect bounds))))

(defmethod view-click-event-handler ((view atom-view) where)
  (let ((offset (subtract-points (view-position view) where)))
    (loop :while (still-down)
          :for last-pos = where :then pos
          :for pos = (get-mouse)
          :when (/= last-pos pos)
          :do (set-view-position view (add-points pos offset)))))


(defclass package-view (atom-view)
  ())

(defun make-inspector-window (&optional (root-objects (list-all-packages)))
  (let ((win (on-main-thread/sync (make-instance 'inspector-window
                                                 :window-title "Inspector"
                                                 :view-size #@(400 300)
                                                 :displayed-objects root-objects)))
        (p (make-point 10 10)))
    (dolist (object root-objects)
      (adjust-size (make-instance (view-class-for-object object)
                                  :object object
                                  :view-container win
                                  :view-position p))
      (setf p (add-points p #@(20 10))))
    win))



#-(and) (

         (let ((window (front-window)))
           (map nil (function adjust-size)
             (view-subviews window)))
         
         (make-inspector-window)
         (make-inspector-window '(a b c))
         (length (list-all-packages))
         (map nil 'print (view-subviews (front-window)))
         
         (view-draw-contents (front-window))
         )


;;;; THE END ;;;;
