;;;; By Nikodemus Siivola <nikodemus@random-state.net>, 2012.
;;;;
;;;; Permission is hereby granted, free of charge, to any person
;;;; obtaining a copy of this software and associated documentation files
;;;; (the "Software"), to deal in the Software without restriction,
;;;; including without limitation the rights to use, copy, modify, merge,
;;;; publish, distribute, sublicense, and/or sell copies of the Software,
;;;; and to permit persons to whom the Software is furnished to do so,
;;;; subject to the following conditions:
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(defsystem :sb-external-formats
  :serial t
  :components
  ((:file "package")
   (:file "utils" :depends-on ("package"))
   (:file "encoding" :depends-on ("utils"))
   (:file "enc-ascii" :depends-on ("encoding"))
   (:file "enc-iso" :depends-on ("encoding"))
   (:file "enc-dos" :depends-on ("encoding"))
   (:file "enc-win" :depends-on ("encoding"))
   (:file "enc-utf8" :depends-on ("encoding"))
   (:file "enc-utf16" :depends-on ("encoding"))
   (:file "external-format" :depends-on ("encoding"))
   (:file "api" :depends-on ("external-format"))))
