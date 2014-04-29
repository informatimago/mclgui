(defpackage "MCLGUI.TEST.VIEW"
  (:use  "COMMON-LISP" "MCLGUI"))
(in-package "MCLGUI.TEST.VIEW")



(defclass box (view)
  ())

(defmethod ui::update-handle ((view view)) nil)

(defmethod view-draw-contents ((view box))
  (with-focused-view view
    (let ((size (view-size view)))
      (fill-rect* 0 0 (point-h size) (point-v size)))))

(defparameter *w* (make-instance 'window :window-title "Test"))

(trace set-view-container)
(progn
  (apply (function remove-subviews) *w* (coerce (view-subviews *w*) 'list))

  (add-subviews *w*
                
                (make-instance
                 'box
                 :view-position (make-point 20 10)
                 :view-size     (make-point 100 20))

                (make-instance
                 'box
                 :view-position (make-point 20 30)
                 :view-size     (make-point 20 20))
                ))

(view-subviews *w*)

#((box :view-position (20 10) :position/window nil :view-size (100 20) :view-scroll-position (0 0) "#x302004B42F0D") (box :view-position (20 30) :position/window nil :view-size (20 20) :view-scroll-position (0 0) "#x302004B42D6D") (box :view-position (20 10) :position/window (40 20) :view-size (100 20) :view-scroll-position (0 0) "#x302004B4AF4D") (box :view-position (20 30) :position/window (40 60) :view-size (20 20) :view-scroll-position (0 0) "#x302004B4AE3D"))

#((box :view-position (20 10) :position/window nil :view-size (100 20) :view-scroll-position (0 0) "#x302004B42F0D")
  (box :view-position (20 30) :position/window nil :view-size (20 20) :view-scroll-position (0 0) "#x302004B42D6D")
  (box :view-position (20 10) :position/window (40 20) :view-size (100 20) :view-scroll-position (0 0) "#x302004B4AF4D")
  (box :view-position (20 30) :position/window (40 60) :view-size (20 20) :view-scroll-position (0 0) "#x302004B4AE3D"))
