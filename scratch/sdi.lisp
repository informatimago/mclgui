(in-package :ui)
(initialize)

(defclass color-box (view)
  ((color :initarg :color :initform *black-color*  :accessor color)))

(defmethod view-draw-contents ((view color-box))
  (with-focused-view view
    (with-fore-color (color view)
      (fill-rect* 0 0
                  (point-h (view-size view))
                  (point-v (view-size view))))
    (call-next-method)))

(progn
 (apply (function remove-subviews) *w* (coerce (view-subviews *w*) 'list))

 (let ((red (make-instance
             'color-box 
             :color *red-color*
             :view-position (make-point 20 10)
             :view-size     (make-point 100 20)))
       (blue (make-instance
              'color-box 
              :color *blue-color*
              :view-position (make-point 2 2)
              :view-size     (make-point 12 12))))
   (add-subviews red blue)
   (add-subviews *w* red)))

(set-view-position  (aref (view-subviews *w*) 0) 30 20)
(view-draw-contents (aref (view-subviews *w*) 0))






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



(defparameter *w* (make-instance 'window :window-title "Test"))

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
                 :view-size     (make-point 100 20))

                (make-instance
                 'boxed-editable-text-dialog-item
                 :dialog-item-text "Another edit"
                 :dialog-item-action (lambda (item)
                                       (format t "~&~S ~S~%" item (dialog-item-text item)))
                 :view-position (make-point 20 50)
                 :view-size     (make-point 100 20))))


(defmethod first-responder ((w window))
  (with-handle (winh w)
    [winh firstResponder]))

#-(and)(progn
         
         (values (first-responder  (first (windows)))
                 (handle (aref (view-subviews (first (windows))) 2)))


         (values (map 'list 'dialog-item-text
                   (view-subviews (first (windows))))
                 (map 'list (lambda (x) (with-handle (h x) (objcl:lisp-string [h stringValue])))
                   (view-subviews (first (windows)))))


         (map 'list (lambda (x) (list (class-name (class-of x)) (dialog-item-enabled-p x)))
           (view-subviews *w*))
         ((boxed-static-text-dialog-item nil) (boxed-editable-text-dialog-item t))

         (view-draw-contents (aref (view-subviews *w*) 0))
         (invalidate-view (aref (view-subviews *w*) 0))
         (remove-subviews *w* (aref (view-subviews *w*) 0))


         (window-close *w*)
         )
