(defpackage "ArchiverTest"
  (:use "COMMON-LISP"))
(in-package "ArchiverTest")
(objcl:enable-objcl-reader-macros)

(defun vector-from-nsdata (data)
  (let* ((length  [data length])
         (bytes   [data bytes])
         (result (make-array length :element-type '(unsigned-byte 8) :initial-element 0)))
    (loop
      :for i :below length
      :do (setf (aref result i) (cffi:mem-ref bytes :unsigned-char i)))
    result))

(defun nsdata-from-vector (vector)
  (assert (every (lambda (x) (typep x '(unsigned-byte 8))) vector))
  (let* ((length  (length vector))
         (data    [NSMutableData dataWithLength:length])
         (bytes   [data mutableBytes]))
    (loop
      :for i :below length
      :do (setf (cffi:mem-ref bytes :unsigned-char i)  (aref vector i)))
    data))

(defun as-string (nsobject)
  (format nil "#<~A #x~(~X~)>"
          (objcl:lisp-string [nsobject className])
          (cffi:pointer-address nsobject)))



@[NSKeyedArchiver subClass:TestKeyedArchiver
                  slots:()]

@[TestKeyedArchiver
  method:(encodeObject:(:id)object forKey:(:id)key)
  resultType:(:void)
  body:
  (format-trace "-[TestKeyedArchiver encodeObject:forKey:]" (as-string self) (as-string object) (objcl:lisp-string key))
  [super encodeObject:object forKey:key]]

;; (defclass mclgui-keyed-archiver (ns:ns-keyed-archiver)
;;   ()
;;   (:metaclass ns:+ns-object))
;;
;; (objc:define-objc-method ((:void :encode-object (:id object) :for-key (:id key)) mclgui-keyed-archiver)
;;   (format-trace "-[MclguiKeyedArchiver encodeObject:forKey:]" (as-string self) (as-string object) (com.informatimago.objective-cl:lisp-string key))
;;   (objc:send-super :encode-object object :for-key key))

@[TestKeyedArchiver
  method:(encodeConditionalObject:(:id)object forKey:(:id)key)
  resultType:(:void)
  body:
  (format-trace "-[TestKeyedArchiver encodeConditionalObject:forKey:]" (as-string self) (as-string object) (objcl:lisp-string key))
  [super encodeConditionalObject:object forKey:key]]


(defun test-object ()
  (let ((d1 [NSMutableDictionary dictionary])
        (d2 [NSMutableDictionary dictionary]))
    [d1 setObject:(objcl:objc-string "Hello") forKey:(objcl:objc-string "one")]
    [d1 setObject:(objcl:objc-string "World") forKey:(objcl:objc-string "two")]
    [d1 setObject:d2                          forKey:(objcl:objc-string "d2")]
    [d2 setObject:(objcl:objc-string "un")    forKey:(objcl:objc-string "one")]
    [d2 setObject:(objcl:objc-string "deux")  forKey:(objcl:objc-string "two")]
    [d2 setObject:d1                          forKey:(objcl:objc-string "d1")]
    d1))

(defun test-encode ()
  (let* ((data      [NSMutableData data])
         (archiver  [[TestKeyedArchiver alloc] initForWritingWithMutableData:data]))
    [archiver encodeObject:(test-object) forKey:(objcl:objc-string "ROOT")]
    [archiver finishEncoding]
    (vector-from-nsdata data)))

#+test (print (test-encode))

