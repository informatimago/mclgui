

(defun get-at (ctm)
  #-ccl (error "~S is not implemented on ~A" 'get-at (lisp-implementation-type))
  #+ccl (list (list (ccl::pref ctm #>CGAffineTransform.a)
                    (ccl::pref ctm #>CGAffineTransform.b))
              (list (ccl::pref ctm #>CGAffineTransform.c)
                    (ccl::pref ctm #>CGAffineTransform.d))
              (list (ccl::pref ctm #>CGAffineTransform.tx)
                    (ccl::pref ctm #>CGAffineTransform.ty))))

(defun get-at-at (at)
  #-ccl  (error "~S is not implemented on ~A" 'get-at-at (lisp-implementation-type))
  #+ccl (ccl::rlet ((r :<NSA>ffine<T>ransform<S>truct))
          (ccl::send/stret r at "transformStruct")
          (get-at r)))

(defun get-at* (ctm)
  (mapcar (lambda (xs) (mapcar (lambda (x) (/ (round x 0.1) 10.0)) xs)) (get-at ctm)))


(declaim (inline get-ctm))
(defun get-ctm (window)
  "RETURN: The Current Transform Matrix."
  (cg:context-get-ctm  [[(handle window) graphicsContext] graphicsPort]))


(declaim (inline make-affine-transform))
(defun make-affine-transform ()
  ;; A bug in ccl prevents this to work: [NSAffineTransform transform], so:
  [[[NSAffineTransform class] performSelector:(objc:@selector |transform|)] retain])


(defun current-affine-transform (window)
  "RETURN: The current NSAffineTransform in the window."
  (let ((trans (make-affine-transform)))
    [trans setTransformStruct:(get-ctm window)]
    trans))


