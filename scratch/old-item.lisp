    ;; #|PJB-DEBUG|# #-(and) (format-trace 'call-with-focused-view
    ;;                                     :view view
    ;;                                     :backtrace (mapcar (lambda (x)
    ;;                                                          (if (member (first x) '(ccl::%call-next-method-with-args
    ;;                                                                                  ccl::%%cnm-with-args-combined-method-dcode
    ;;                                                                                  ccl::%call-next-method
    ;;                                                                                  ccl::%%before-and-after-combined-method-dcode
    ;;                                                                                  ccl::%%standard-combined-method-dcode
    ;;                                                                                  ccl::%pascal-functions%
    ;;                                                                                  funcall))
    ;;                                                              (second x)
    ;;                                                              (first x)))
    ;;                                                        (ccl::backtrace-as-list)))


(defgeneric make-text-item (item &key selectable editable bordered bezeled))
(defmethod make-text-item ((item dialog-item) &key selectable editable bordered bezeled)
  (let* ((pos (or (slot-value item 'view-position) #@(0   0)))
         (siz (or (slot-value item 'view-size)     #@(10 10)))
         (texth [[MclguiTextField alloc] initWithFrame:(ns:make-ns-rect (point-h pos) (point-v pos)
                                                                        (point-h siz) (point-v siz))]))
    ;; -- NSControl attributes:
    [texth setTarget:texth]
    [texth setAction:(objc:@selector "mclguiAction:")]
    [texth setStringValue:(objcl:objcl-string (dialog-item-text item))]
    [texth setEnabled:(if (dialog-item-enabled-p item)
                          YES
                          NO)]
    (multiple-value-bind (ff ms) (view-font-codes item)
      (multiple-value-bind (font mode color other) (nsfont-from-codes ff ms)
        (declare (ignore mode other))
        (declare (ignore color))
        ;;[texth setTextColor:color]
        [texth setFont:font]))
    [texth setAlignment:(case (slot-value item 'text-justification)
                          (:left      #$NSLeftTextAlignment)
                          (:right     #$NSRightTextAlignment)
                          (:center    #$NSCenterTextAlignment)
                          (:justified #$NSJustifiedTextAlignment)
                          (:natural   #$NSNaturalTextAlignment)
                          (otherwise  #$NSNaturalTextAlignment))]
    ;; -- NSTextField attributes:
    [texth setEditable:editable]
    [texth setBordered:bordered]
    [texth setSelectable:selectable]
    ;; [texth setTextColor:] ;; set above
    ;; [texth setBackgroundColor:]
    ;; [texth setDrawBackground:]
    [texth setBezeled:bezeled]
    [texth setBezelStyle:#$NSTextFieldSquareBezel]
    ;; #$NSTextFieldSquareBezel  = 0
    ;; #$NSTextFieldRoundedBezel = 1
    ;; --
    (setf (nsview-view texth) item)
    texth))
