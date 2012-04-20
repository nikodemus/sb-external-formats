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

(in-package :sb-external-format)

(defmacro pop-assoc (name alist &environment env)
  (with-unique-names (n-name old new cell)
    (multiple-value-bind (temps args values set-form get-form)
        (get-setf-expansion alist env)
      `(let* ((,n-name ,name)
              ,@(mapcar #'list temps args)
              (,old ,get-form)
              (,cell (assoc ,n-name ,old)))
         (when ,cell
           (let* ((,new (remove ,cell ,old :test #'eq))
                  (,@values (if (assoc ,n-name ,new)
                                (error "Multiple occurrances of ~S in options." ,n-name)
                                ,new)))
             ,set-form)
           (cdr ,cell))))))

(defmacro pop-assoc-atom (name alist)
  `(destructuring-bind (&optional atom) (pop-assoc ,name ,alist)
     atom))

(defun name-list-p (object)
  (or (null object)
      (and (consp object)
           (let ((name (car object)))
             (and (not (null name))
                  (symbolp name)
                  (name-list-p (cdr object)))))))

(deftype name-list ()
  '(satisfies name-list-p))

;; (declaim (inline sap-match-16))
;; (defun sap-match-16 (integer sap offset length)
;;   ;; NIL if out of bounds
;;   (when (>= length (+ offset 2))
;;     (= integer (sap-ref-16 sap offset))
;;     (dotimes (i l-part t)
;;       (unless (= (aref part i) (sb-sys:sap-ref-8 sap (+ offset i)))
;;         (return nil)))))

;; (defun sap-match (part sap offset length)
;;   (declare (type (simple-array (unsigned-byte 8) (*)) part))
;;   (declare (sb-sys:system-area-pointer sap))
;;   (declare (sb-int:index offset length))
;;   (let* ((l-part (length l-part))
;; 	 (end (+ offset l-part)))
;;     (when (>= length end)
;;       ;; FIXME: For parts up to 1 word in size we can manage with a single
;;       ;; comparison -- in practise only UTF-32 needs multiple comparisons.
;;       (dotimes (i l-part t)
;; 	(unless (= (aref part i) (sb-sys:sap-ref-8 sap (+ offset i)))
;; 	  (return nil))))))
