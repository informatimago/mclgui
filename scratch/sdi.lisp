(in-package :ui)
(initialize)
(defparameter *w* (make-instance 'window :window-title "Test"))

(defclass boxed-static-text-dialog-item (static-text-dialog-item)
  ())

(defclass boxed-editable-text-dialog-item (editable-text-dialog-item)
  ())

(defmethod view-draw-contents ((item boxed-editable-text-dialog-item))
  (call-next-method)
  (with-focused-view item
    (format t  "~&~S~% "(list 'draw-rect* 0 0
                (point-h (view-size item))
                (point-v (view-size item))))
    (draw-rect* 0 0
                (point-h (view-size item))
                (point-v (view-size item)))))



(progn
  (apply (function remove-subviews) *w* (coerce (view-subviews *w*) 'list))
  (add-subviews *w*
                
                (make-instance
                 'boxed-static-text-dialog-item
                 :dialog-item-text "STATIC TEXT"
                 :dialog-item-action (lambda (item)
                                       (format t "~&~S ~S~%" item (dialog-item-text item)))
                 :view-position (make-point 20 10)
                 :view-size     (make-point 100 20))

                (make-instance
                 'boxed-editable-text-dialog-item
                 :dialog-item-text "EDIT IT"
                 :dialog-item-action (lambda (item)
                                       (format t "~&~S ~S~%" item (dialog-item-text item)))
                 :view-position (make-point 20 30)
                 :view-size     (make-point 100 20))))


(map 'list (lambda (x) (list (class-name (class-of x)) (dialog-item-enabled-p x)))
  (view-subviews *w*))
((boxed-static-text-dialog-item nil) (boxed-editable-text-dialog-item t))

(view-draw-contents (aref (view-subviews *w*) 0))
(invalidate-view (aref (view-subviews *w*) 0))
(remove-subviews *w* (aref (view-subviews *w*) 0))


(window-close *w*)
