(defmethod update-buffer-display ((self C-patch-buffer) obj)
  (let ((text (prin1-to-string obj))
        (box  (find 'c-numbox (view-subviews self) :key (lambda (obj) (class-name (class-of obj))))))
    (pw::set-numbox-item-text box text)))

(defmethod patch-value ((self C-patch-buffer) obj)
  (let ((in (car (input-objects self))))
    (if (value self)
      (the-buffer self)
      (update-buffer-display self (setf (the-buffer self) (patch-value in obj))))))