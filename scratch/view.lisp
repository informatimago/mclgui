(defmethod (setf %view-position) :around (new-position view)
  ;; (when (= 0 new-position) (break "set-view-position (0 0) !"))
  (format-trace '(setf %view-position) (point-to-list new-position) view)
  (prog1 (call-next-method)
    (format-trace '(setf %view-position) (point-to-list (view-position view)) view)))


(defmethod find-view-containing-point :around (view h &optional v direct-subviews-only)
  (declare (ignorable view h v direct-subviews-only))
  (let ((result (call-next-method)))
    (format-trace 'find-view-containing-point result)
    (with-focused-view result
      (with-pen-state (:pattern *black-pattern* :mode :srcCopy)
        (fill-rect* 0 0 (point-h (view-size result)) (point-v (view-size result)))))
    result))
