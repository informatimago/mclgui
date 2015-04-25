(in-package "MCLGUI")
(objcl:enable-objcl-reader-macros)

(defun bezier-path-from-region (region)
  (flet ((tpoint-to-point (tpoint)
           (ns:make-ns-point (tpoint-x tpoint) (tpoint-y tpoint))))
    (declare (inline tpoint-to-point))
    (let ((tpaths (paths-from-region region))
          (path [NSBezierPath bezierPath]))
      [path setLineCapStyle:#$NSSquareLineCapStyle]
      ;; [path setLineJoinStyle:#$NSRoundLineJoinStyle]
      [path setLineJoinStyle:#$NSBevelLineJoinStyle]
      (loop
        :for tpath :in tpaths
        :for start = (tpath-lines tpath)
        :do (let ((first t))
              (tline-dolines (line start)
                (when first
                  [path moveToPoint:(tpoint-to-point (tline-from-point line))]
                  (setf first nil))
                [path lineToPoint:(tpoint-to-point (tline-to-point line))])))
      [path closePath]
      path)))


;;;; THE END ;;;;
