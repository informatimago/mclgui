
(defun ostype (ostype-string)
  (loop :for ch :across ostype-string
        :for byte := (char-code ch)
        :for e :from 24 :downto 0 :by 8
        :sum (* byte (expt 2 e))))
