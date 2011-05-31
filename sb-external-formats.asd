(defsystem :sb-external-formats
  :serial t
  :components
  ((:file "package")
   (:file "utils" :depends-on ("package"))
   (:file "encoding" :depends-on ("utils"))
   (:file "enc-ascii" :depends-on ("encoding"))
   (:file "enc-utf8" :depends-on ("encoding"))
   (:file "external-format" :depends-on ("encoding"))
   (:file "api" :depends-on ("external-format"))))
