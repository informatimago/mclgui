
(defpackage "MCLGUI.TEST.GRAPHICS"
  (:use  "COMMON-LISP" "MCLGUI"))
(in-package "MCLGUI.TEST.GRAPHICS")


(defclass graphic-view (view)
  ())


(defmethod draw-contents ((view graphic-view))
  (with-focused-view view
    (view-draw-contents view)))

(defmethod view-draw-contents ((view graphic-view))
  (draw-rect* 10 10 290 190)
  (fill-rect* 13 13 287 187))


(defun create-window (&optional (title "Graphic"))
  (let ((wind (make-instance 'window
                  :view-position (make-point 30 30)
                  :view-size     (make-point 300 200)
                  :window-title  title))
        (view (make-instance 'graphic-view
                  :view-position (make-point 0 0)
                  :view-size     (make-point 300 200))))
    (set-view-container view wind)
    wind))


;;;; THE END ;;;;
