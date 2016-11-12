;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               objc-persistent.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    This module saves NSObject in the image that are referenced by
;;;;    the live wrappers into a byte vector, so the image can be
;;;;    saved, and when relaunched, restores those NSObjects frmo that
;;;;    byte vector, re-establishing the links with their respective
;;;;    wrappers.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2013-03-22 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2013 - 2014
;;;;
;;;;    This program is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;
;;;;    This program is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU General Public License
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************
(in-package "MCLGUI")
(objcl:enable-objcl-reader-macros)



(defmacro with-temporary-retained-nsobject ((variable retained-nsobject-expression) &body body)
  (let ((object (gensym)))
    `(let* ((,object ,retained-nsobject-expression)
            (,variable ,object))
       (unwind-protect
            (progn ,@body)
         [,object release]))))

(defmacro with-temporary-retained-nsobjects (bindings &body body)
  (if (null bindings)
      `(progn ,@body)
      `(with-temporary-retained-nsobject ,(first bindings)
         (with-temporary-retained-nsobjects ,(rest bindings)
           ,@body))))


(defun description (nsobject)
  (format nil "#<~A #x~(~X~)>"
          (objcl:lisp-string [nsobject className])
          (cffi:pointer-address nsobject)))

(defmethod print-object ((object ns:ns-object) stream)
  (print-unreadable-object (object stream :type t :identity t)
      (format stream "#<~A #x~(~X~)>"
          (objcl:lisp-string [object className])
          (cffi:pointer-address object)))
  object)

;; ignore-errors only ignores CL errors;
;; we need a macro to ignore also NSExceptions.




#||


;; print circular structure.

start from the root as the current object and with 0 as sequence number.

to generate the current object do
    if the current object is in the table
    then it is processed object,
         generate its sequence number reference
    else it is unprocessed object,
         put unprocessed object into a table with a sequence number,
         generate the unprocessed object.



||#








;;;---------------------------------------------------------------------
;;;

@[NSObject subClass:MclguiKeyedArchiverDelegate
           slots:((objects :initform '()
                           :accessor objects-being-encoded))]

@[MclguiKeyedArchiverDelegate
  method:(archiver:(:id)archiver willEncodeObject:(:id)object)
  resultType:(:id)
  body:
  (format-trace "-[MclguiKeyedArchiverDelegate archiver:willEncodeObject:]" (description self) (description archiver) (description object))
  (push object (objects-being-encoded self))
  object]

@[MclguiKeyedArchiverDelegate
  method:(archiver:(:id)archiver willReplaceObject:(:id)object withObject:(:id)replacement)
  resultType:(:void)
  body:
  (format-trace "-[MclguiKeyedArchiverDelegate archiver:willReplaceObject:]" (description self) (description archiver) (description object))]

@[MclguiKeyedArchiverDelegate
  method:(encodeWithCoder:(:id)coder)
  resultType:(:void)
  body:
  (format-trace "-[MclguiKeyedArchiverDelegate encodeWithCoder:]" (description self) (description coder))
  [(first (objects-being-encoded self)) encodeWithCoder:coder]]

@[MclguiKeyedArchiverDelegate
  method:(archiver:(:id)archiver didEncodeObject:(:id)object)
  resultType:(:void)
  body:
  (format-trace "-[MclguiKeyedArchiverDelegate archiver:didEncodeObject:]" (description self) (description archiver) (description object))
  (pop (objects-being-encoded self))]

@[MclguiKeyedArchiverDelegate
  method:(archiverWillFinish:(:id)archiver)
  resultType:(:void)
  body:(format-trace "-[MclguiKeyedArchiverDelegate archiverWillFinish:]" (description self) (description archiver))]

@[MclguiKeyedArchiverDelegate
  method:(archiverDidFinish:(:id)archiver)
  resultType:(:void)
  body:(format-trace "-MclguiKeyedArchiverDelegate archiverDidFinish:]" (description self) (description archiver))]


;;;---------------------------------------------------------------------
;;;

@[NSKeyedArchiver subClass:MclguiKeyedArchiver
                  slots:((objects :initform '()
                                  :accessor objects-being-encoded))]

@[MclguiKeyedArchiver
  method:(encodeConditionalObject:(:id)object forKey:(:id)key)
  resultType:(:void)
  body:
  (format-trace "-[MclguiKeyedArchiver encodeConditionalObject:forKey:]" (description self) (description object) (description key))
  (push object (objects-being-encoded self))
  [super encodeConditionalObject:object forKey:key]]


@[MclguiKeyedArchiver
  method:(encodeObject:(:id)object forKey:(:id)key)
  resultType:(:void)
  body:
  (format-trace "-[MclguiKeyedArchiver encodeObject:forKey:]" (description self) (description object) (description key))
  (push object (objects-being-encoded self))
  [super encodeObject:object forKey:key]]


;;;---------------------------------------------------------------------
;;;

@[NSObject subClass:MclguiKeyedUnarchiverDelegate
           slots:(;; (objects :initform '() :accessor objects-being-encoded)
                  )]


@[MclguiKeyedUnarchiverDelegate
  method:(unarchiver:(:id)unarchiver cannotDecodeObjectOfClassName:(:id)name originalClasses:(:id)classNames)
  resultType:(:id)
  body:
  (format-trace "-[MclguiKeyedUnarchiverDelegate unarchiver:cannotDecodeObjectOfClassName:originalClasses:]"
                (description self) (description unarchiver)
                (description name) (description classNames)
                ;; (objcl:lisp-string name) (wrap classNames)
                )
  nil]

@[MclguiKeyedUnarchiverDelegate
  method:(unarchiver:(:id)unarchiver didDecodeObject:(:id)object)
  resultType: (:id)
  body:
  (format-trace "-[MclguiKeyedUnarchiverDelegate unarchiver:didDecodeObject:]" (description self) (description unarchiver)  (description object))
  object]

@[MclguiKeyedUnarchiverDelegate
  method:(unarchiver:(:id)unarchiver willReplaceObject:(:id)object withObject:(:id)newObject)
  resultType: (:void)
  body:
  (format-trace "-[MclguiKeyedUnarchiverDelegate unarchiver:willReplaceObject:withObject:]" (description self) (description unarchiver) (description object) (description newObject))]

@[MclguiKeyedUnarchiverDelegate
  method:(unarchiverWillFinish:(:id)unarchiver)
  resultType:(:void)
  body:
  (format-trace "-[MclguiKeyedUnarchiverDelegate unarchiverWillFinish:]" (description self) (description unarchiver))]

@[MclguiKeyedUnarchiverDelegate
  method:(unarchiverDidFinish:(:id)unarchiver)
  resultType:(:void)
  body:
  (format-trace "-[MclguiKeyedUnarchiverDelegate unarchiverDidFinish:]" (description self) (description unarchiver))]

;;;---------------------------------------------------------------------
;;;

@[NSKeyedUnarchiver subClass:MclguiKeyedUnarchiver
                  slots:((objects :initform '()
                                  :accessor objects-being-encoded))]

@[MclguiKeyedUnarchiver
  method:(decodeObjectForKey:(:id)key)
  resultType:(:id)
  body:
  (let ((object [super decodeObjectForKey:key]))
    (format-trace "-[MclguiKeyedUnarchiverDelegate decodeObjectForKey:]" (description self) (description key) "-->" (description object))
    object)]


;;;---------------------------------------------------------------------
;;;

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

(defun test/nsdata-vector ()
  (assert (equalp (vector-from-nsdata (nsdata-from-vector #(1 2 3 4)))
                  #(1 2 3 4)))
  :success)


(defstruct archive
  wrappers
  data
  unarchiver-class-name
  root-key)

(defun archive-objects/nsarchive (&optional (wrapper-instances *wrapper-instances*))
  (let* ((wrappers  (remove nil (weak-list-list wrapper-instances) :key (function handle)))
         (length    (length wrappers))
         (nsobjects [NSMutableArray arrayWithCapacity:length]))
    (loop
      :for i :from 0
      :for wrapper :in wrappers
      :do
      [nsobjects addObject:(handle wrapper)]
      (setf (handle wrapper) nil))
    (make-archive :wrappers wrappers
                  :data (vector-from-nsdata [NSArchiver archivedDataWithRootObject:nsobjects])
                  :unarchiver-class-name "NSUnarchiver")))

(defvar *root-key* "MCLGUI::KEYED-ARCHIVE-ROOTS")

(defun archive-objects (&optional (wrapper-instances *wrapper-instances*))
  (let* ((wrappers  (remove nil (weak-list-list wrapper-instances) :key (function handle)))
         (length    (length wrappers))
         (nsobjects [NSMutableArray arrayWithCapacity:length])
         (data      [NSMutableData data]))
    (break "initialized")
    (with-temporary-retained-nsobjects
        ((delegate  [MclguiKeyedArchiverDelegate new])
         (archiver  [[MclguiKeyedArchiver alloc] initForWritingWithMutableData:data]))
      (break "will set delegate")
      [archiver setDelegate:[delegate autorelease]]
      (break "archiver set up")
      (loop
        :for i :from 0
        :for wrapper :in wrappers
        :do (progn
              [nsobjects addObject:(handle wrapper)]
              (setf (handle wrapper) nil)))
      (break "nsobjects filled up, will encode it")
      [archiver encodeObject:nsobjects forKey:(objcl:objc-string *root-key*)]
      (break "archiver encoded, will finish it")
      [archiver finishEncoding]
      (break "wrapping the archive")
      (make-archive :wrappers wrappers
                    :data (vector-from-nsdata data)
                    :root-key *root-key*
                    :unarchiver-class-name "MclguiKeyedUnarchiver"))))



(defgeneric unarchive-objects (archive))

;; (defmethod unarchive-objects (archive)
;;   (let* ((wrappers  (archive-wrappers archive))
;;          (data      (nsdata-from-vector (archive-data archive)))
;;          (nsobjects [(#_NSClassFromString (objcl:objc-string (archive-unarchiver-class-name archive)))
;;                      unarchiveObjectWithData:data]))
;;     (loop
;;       :for i :from 0
;;       :for wrapper :in wrappers
;;       :do (setf (handle wrapper) [nsobjects objectAtIndex:i]))
;;     wrappers))

(defmethod unarchive-objects (archive)
  (let* ((wrappers   (archive-wrappers archive))
         (data       (nsdata-from-vector (archive-data archive))))
    (with-temporary-retained-nsobjects
        ((unarchiver [[(#_NSClassFromString (objcl:objc-string (archive-unarchiver-class-name archive))) alloc] initForReadingWithData:data])
         (delegate   [MclguiKeyedUnarchiverDelegate new]))
      [unarchiver setDelegate:delegate]
      (let ((nsobjects [unarchiver decodeObjectForKey:(objcl:objc-string (archive-root-key archive))]))
        (loop
          :for i :from 0
          :for wrapper :in wrappers
          :do (setf (handle wrapper) [nsobjects objectAtIndex:i]))))
    wrappers))


(defmethod update-handle ((wrapper wrapper))
  (handle wrapper))






(defparameter *wi*  (list (make-instance 'wrapper :handle [NSNull null])
                          (make-instance 'wrapper :handle [NSNull null])))
(defparameter *wi*  (list (make-instance 'wrapper :handle nil)
                          (make-instance 'wrapper :handle nil)))


#-(and)
(defparameter *a* (archive-objects
                   (let ((d1 [NSMutableDictionary dictionary])
                         (d2 [NSMutableDictionary dictionary]))
                     [d1 setObject:(objcl:objc-string "Hello") forKey:(objcl:objc-string "one")]
                     [d1 setObject:(objcl:objc-string "World") forKey:(objcl:objc-string "two")]
                     [d1 setObject:d2                           forKey:(objcl:objc-string "d2")]
                     [d2 setObject:(objcl:objc-string "un")    forKey:(objcl:objc-string "one")]
                     [d2 setObject:(objcl:objc-string "deux")  forKey:(objcl:objc-string "two")]
                     [d2 setObject:d1                           forKey:(objcl:objc-string "d1")]
                     (setf (handle (elt *wi* 0)) d1
                           (handle (elt *wi* 1)) d2)
                     (make-weak-list *wi*))))

(defparameter *test-object*
  (let ((d1 [NSMutableDictionary dictionary])
        (d2 [NSMutableDictionary dictionary]))
    [d1 setObject:(objcl:objc-string "Hello") forKey:(objcl:objc-string "one")]
    [d1 setObject:(objcl:objc-string "World") forKey:(objcl:objc-string "two")]
    [d1 setObject:d2                           forKey:(objcl:objc-string "d2")]
    [d2 setObject:(objcl:objc-string "un")    forKey:(objcl:objc-string "one")]
    [d2 setObject:(objcl:objc-string "deux")  forKey:(objcl:objc-string "two")]
    [d2 setObject:d1                           forKey:(objcl:objc-string "d1")]
    ;; (wrap d1)
    d1))

#-(and) (defparameter *a* (archive-objects (make-weak-list (list *test-object*))))


;; (defparameter *wo* (unarchive-objects *a*))
;; [(handle (elt *wo* 0)) description]
;; [(handle (elt *wo* 1)) description]




;; (defparameter *a* (archive-objects
;;                    (let ((d1 [NSMutableDictionary dictionary])
;;                          (d2 [NSMutableDictionary dictionary]))
;;                      [d1 setObject:(objcl:objc-string "Hello") forKey:(objcl:objc-string "one")]
;;                      [d1 setObject:(objcl:objc-string "World") forKey:(objcl:objc-string "two")]
;;                      [d1 setObject:d2                           forKey:(objcl:objc-string "d2")]
;;                      [d2 setObject:(objcl:objc-string "un")    forKey:(objcl:objc-string "one")]
;;                      [d2 setObject:(objcl:objc-string "deux")  forKey:(objcl:objc-string "two")]
;;                      [d2 setObject:d1                           forKey:(objcl:objc-string "d1")]
;;                      (setf (handle (elt *wi* 0)) d1
;;                            (handle (elt *wi* 1)) d2)
;;                      (make-weak-list *wi*))))
;;
;; (defparameter *wo* (unarchive-objects *a*))

;; [(handle (elt *wo* 0)) description]

(defmacro do-nsarray ((element-variable nsarray-expression &optional result-form) &body body)
  (let ((varray (gensym))
        (vindex (gensym)))
    `(let* ((,varray ,nsarray-expression)
            (,element-variable nil))
       (dotimes (,vindex [,varray count] ,result-form)
         (let ((,element-variable [,varray objectAtIndex:,vindex]))
           ,@body)))))

(defmacro do-nsdictionary ((key-variable value-variable nsdictionary-expression &optional result-form) &body body)
  (let ((vdictionary (gensym))
        (vkeys (gensym))
        (vindex (gensym)))
    `(let* ((,vdictionary ,nsdictionary-expression)
            (,vkeys [,vdictionary allKeys])
            (,key-variable   nil)
            (,value-variable nil))
       (dotimes (,vindex [,vkeys count] ,result-form)
         (let* ((,key-variable   [,vkeys objectAtIndex:,vindex])
                (,value-variable [,vdictionary objectForKey:,key-variable]))
           ,@body)))))


#-(and) (progn
          (let ((*max-level* 2))
            (declare (special *max-level*))
            (labels ((dump-nsdictionary (nsdictionary)
                       (princ "(")
                       (when (plusp *max-level*)
                         (let ((*max-level* (1- *max-level*)))
                           (declare (special *max-level*))
                           (do-nsdictionary (key value nsdictionary)
                             (terpri) (prin1 key) (princ " ")
                             (if [value isKindOfClass:[NSDictionary class]]
                                 (dump-nsdictionary value)
                                 (prin1 value)))))
                       (princ ")")))
              (dump-nsdictionary (handle (elt *wo* 1)))))


          (do-nsarray (str [(handle (elt *wo* 1)) allKeys])
            (print str))

          [(handle (elt *wo* 0)) objectForKey:(objcl:objc-string "d2")]
          [(handle (elt *wo* 0)) objectForKey: (objcl:objc-string "d2")]
          [(handle (elt *wo* 1)) objectForKey: (objcl:objc-string "d1")]



          ;; #<ns-mutable-string "{
          ;;     d2 =     {
          ;;         one = un;
          ;;         two = deux;
          ;;     };
          ;;     one = Hello;
          ;;     two = World;
          ;; }" (#x5CDA20)>
          [(handle (elt *wo* 1)) description]

          ;; #<ns-mutable-string "{
          ;;     one = un;
          ;;     two = deux;
          ;; }" (#x55AAF0)>
          (map 'string 'code-char (archive-data *a*))

          ;; #S(archive :wrappers (#<wrapper #x302004ED356D> #<wrapper #x302004ED335D>) :data #(4 11 115 116 114 101 97 109 116 121 112 101 100 129 232 3 132 1 64 132 132 132 14 78 83 77 117 116 97 98 108 101 65 114 114 97 121 0 132 132 7 78 83 65 114 114 97 121 0 132 132 8 78 83 79 98 106 101 99 116 0 133 132 1 105 2 146 132 132 132 19 78 83 77 117 116 97 98 108 101 68 105 99 116 105 111 110 97 114 121 0 132 132 12 78 83 68 105 99 116 105 111 110 97 114 121 0 149 150 3 146 132 132 132 8 78 83 83 116 114 105 110 103 1 149 132 1 43 3 111 110 101 134 146 132 154 154 5 72 101 108 108 111 134 146 132 154 154 3 116 119 111 134 146 132 154 154 5 87 111 114 108 100 134 146 132 154 154 2 100 50 134 146 132 151 150 3 146 132 154 154 3 111 110 101 134 146 132 154 154 2 117 110 134 146 132 154 154 3 116 119 111 134 146 132 154 154 4 100 101 117 120 134 146 132 154 154 2 100 49 134 146 150 134 134 146 159 134))
          ;; (apropos "make-weak-list")
          ;; (describe 'make-weak-list)
          ;; make-weak-list
          ;; Type: symbol
          ;; Class: #<built-in-class symbol>
          ;; Function
          ;; external in package: #<Package "COM.INFORMATIMAGO.CLEXT.CLOSER-WEAK">
          ;; Print name: "MAKE-WEAK-LIST"
          ;; Value: #<Unbound>
          ;; Function: #<Compiled-function make-weak-list #x3020027FA8EF>
          ;; Arglist: (list)
          ;; Plist: nil





          (cl-user::lspack :objcl t)

          ;; COM.INFORMATIMAGO.OBJECTIVE-CL
          ;;    Symbols:         14 exported, 1130 total.
          ;;    Nicknames:     COM.INFORMATIMAGO.OBJCL OBJCL
          ;;    Uses:          COM.INFORMATIMAGO.SIMPLE-TEST COMMON-LISP
          ;;    Exported:      *OBJECTIVE-CL-READTABLE* @ DISABLE-OBJCL-READER-MACROS ENABLE-OBJCL-READER-MACROS
          ;;                   LISP-STRING NO OBJC-DEFINITION-READER-MACRO OBJC-EXPRESSION-READER-MACRO OBJC-STRING
          ;;                   READ-ERROR READ-ERROR-ARGUMENTS READ-ERROR-CONTROL-STRING SET-OBJECTIVE-CL-SYNTAX YES
          ;; nil


          )


;; #<MclguiKeyedArchiverDelegate #x369b660> archiver:#<NSKeyedArchiver #x369b130> willEncodeObject:#<__NSArrayM #x3698ff0>
;; #<MclguiKeyedArchiverDelegate #x369b660> archiver:#<NSKeyedArchiver #x369b130>  willEncodeObject:#<__NSCFDictionary #x3692d30>
;; #<MclguiKeyedArchiverDelegate #x369b660> archiver:#<NSKeyedArchiver #x369b130>   willEncodeObject:#<__NSCFString #x369a8d0>
;; #<MclguiKeyedArchiverDelegate #x369b660> archiver:#<NSKeyedArchiver #x369b130>   didEncodeObject:#<__NSCFString #x369a8d0>
;; #<MclguiKeyedArchiverDelegate #x369b660> archiver:#<NSKeyedArchiver #x369b130>   willEncodeObject:#<__NSCFString #x369a940>
;; #<MclguiKeyedArchiverDelegate #x369b660> archiver:#<NSKeyedArchiver #x369b130>   didEncodeObject:#<__NSCFString #x369a940>
;; #<MclguiKeyedArchiverDelegate #x369b660> archiver:#<NSKeyedArchiver #x369b130>   willEncodeObject:#<__NSCFString #x369b610>
;; #<MclguiKeyedArchiverDelegate #x369b660> archiver:#<NSKeyedArchiver #x369b130>   didEncodeObject:#<__NSCFString #x369b610>
;; #<MclguiKeyedArchiverDelegate #x369b660> archiver:#<NSKeyedArchiver #x369b130>   willEncodeObject:#<__NSCFString #x369aa80>
;; #<MclguiKeyedArchiverDelegate #x369b660> archiver:#<NSKeyedArchiver #x369b130>   didEncodeObject:#<__NSCFString #x369aa80>
;; #<MclguiKeyedArchiverDelegate #x369b660> archiver:#<NSKeyedArchiver #x369b130>   willEncodeObject:#<__NSCFString #x3636ed0>
;; #<MclguiKeyedArchiverDelegate #x369b660> archiver:#<NSKeyedArchiver #x369b130>   didEncodeObject:#<__NSCFString #x3636ed0>
;; #<MclguiKeyedArchiverDelegate #x369b660> archiver:#<NSKeyedArchiver #x369b130>   willEncodeObject:#<__NSCFDictionary #x5ccd10>
;; #<MclguiKeyedArchiverDelegate #x369b660> archiver:#<NSKeyedArchiver #x369b130>    willEncodeObject:#<__NSCFString #x36995c0>
;; #<MclguiKeyedArchiverDelegate #x369b660> archiver:#<NSKeyedArchiver #x369b130>    didEncodeObject:#<__NSCFString #x36995c0>
;; #<MclguiKeyedArchiverDelegate #x369b660> archiver:#<NSKeyedArchiver #x369b130>    willEncodeObject:#<__NSCFString #x5f8410>
;; #<MclguiKeyedArchiverDelegate #x369b660> archiver:#<NSKeyedArchiver #x369b130>    didEncodeObject:#<__NSCFString #x5f8410>
;; #<MclguiKeyedArchiverDelegate #x369b660> archiver:#<NSKeyedArchiver #x369b130>    willEncodeObject:#<__NSCFString #x5f8430>
;; #<MclguiKeyedArchiverDelegate #x369b660> archiver:#<NSKeyedArchiver #x369b130>    didEncodeObject:#<__NSCFString #x5f8430>
;; #<MclguiKeyedArchiverDelegate #x369b660> archiver:#<NSKeyedArchiver #x369b130>    willEncodeObject:#<__NSCFString #x5fb4b0>
;; #<MclguiKeyedArchiverDelegate #x369b660> archiver:#<NSKeyedArchiver #x369b130>    didEncodeObject:#<__NSCFString #x5fb4b0>
;; #<MclguiKeyedArchiverDelegate #x369b660> archiver:#<NSKeyedArchiver #x369b130>    willEncodeObject:#<__NSCFString #x5f83f0>
;; #<MclguiKeyedArchiverDelegate #x369b660> archiver:#<NSKeyedArchiver #x369b130>    didEncodeObject:#<__NSCFString #x5f83f0>
;; #<MclguiKeyedArchiverDelegate #x369b660> archiver:#<NSKeyedArchiver #x369b130>   didEncodeObject:#<__NSCFDictionary #x5ccd10>
;; #<MclguiKeyedArchiverDelegate #x369b660> archiver:#<NSKeyedArchiver #x369b130>  didEncodeObject:#<__NSCFDictionary #x3692d30>
;; #<MclguiKeyedArchiverDelegate #x369b660> archiver:#<NSKeyedArchiver #x369b130> didEncodeObject:#<__NSArrayM #x3698ff0>
;; #<MclguiKeyedArchiverDelegate #x369b660> archiverWillFinish:#<NSKeyedArchiver #x369b130>
;; #<MclguiKeyedArchiverDelegate #x369b660> archiverDidFinish:#<NSKeyedArchiver #x369b130>



;; #<MclguiKeyedUnarchiverDelegate #x369a0e0> unarchiver:#<NSKeyedUnarchiver #x36994f0> didDecodeObject:#<__NSCFDictionary #x369bca0>
;; #<MclguiKeyedUnarchiverDelegate #x369a0e0> unarchiver:#<NSKeyedUnarchiver #x36994f0> didDecodeObject:#<__NSCFDictionary #x3699490>
;; #<MclguiKeyedUnarchiverDelegate #x369a0e0> unarchiver:#<NSKeyedUnarchiver #x36994f0> didDecodeObject:#<__NSArrayM #x369a170>

#+test (test/nsdata-vector)

;;;; THE END ;;;;
