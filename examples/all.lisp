(in-package :cl-user)

(defun run-all ()
  (ui:on-main-thread
    (mclgui.example.computed-view:run)
    (mclgui.example.coordinates-window:run)
    (mclgui.example.slowatch:run)))
