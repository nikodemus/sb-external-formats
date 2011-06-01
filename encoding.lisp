(in-package :sb-external-format)

(defun encoding-stub (slot)
  (lambda (&rest args)
    (declare (ignore args))
    (error "~S not available for this character encoding." slot)))

(defstruct character-encoding
  ;; Canonical name.
  (name nil :type symbol)
  ;; Nicknames.
  (nicknames nil :type name-list)
  ;; Documentation
  (documentation nil :type (or null string))
  ;; These implement the guts of ENCODE-STRING and DECODE-OCTETS for this
  ;; encoding.
  (encoder (encoding-stub 'encoder) :type function)
  (decoder (encoding-stub 'decoder) :type function)
  ;; These implement the guts of DECODED-LENGTH and ENCODED-LENGTH for this
  ;; encoding.
  (decoded-length (encoding-stub 'decoded-length) :type function)
  (encoded-length (encoding-stub 'encoded-length) :type function)
  ;; For guesstimate of how large a string or a vector to allocate.
  #+nil
  (average-character-bytes 1 :type index)
  ;; Information on how different EOL styles are encoded.
  (eol-info nil :type list)
  ;; Source location.
  (source-location nil))

(defun %encoded-length (encoding string start length limit code1 code2)
  (funcall (character-encoding-encoded-length encoding)
           string start length limit code1 code2))

(defun %decoded-length (encoding string start length limit code1 code2)
  (funcall (character-encoding-decoded-length encoding)
           string start length limit code1 code2))

(declaim (inline %encode))
(defun %encode (encoding string string-offset buf buf-offset count x eol)
  (funcall (character-encoding-encoder encoding)
           string string-offset buf buf-offset count x eol))

(defun %decode (encoding string string-offset sap sap-offset count x code1 code2)
  (funcall (character-encoding-decoder encoding)
           string string-offset sap sap-offset count x code1 code2))

(defparameter *character-encodings* (make-hash-table :test 'eq :synchronized t))

;;; Removes a CHARACTER-ENCODING from *CHARACTER-ENCODINGS*, and clears its NAME and NICKNAMES.
(defun delete-character-encoding (encoding)
  (let ((table *character-encodings*))
    (with-locked-hash-table (table)
      (dolist (name (cons (character-encoding-name encoding)
                          (character-encoding-nicknames encoding)))
        (let ((old (gethash name table)))
          ;; Allow nonexistent olds for recovery purposes.
          (aver (or (not old) (eq old encoding))))
        (remhash name table))
      (setf (character-encoding-name encoding) nil
            (character-encoding-nicknames encoding) nil)))
  encoding)

;;; Adds CHARACTER-ENCODING to *CHARACTER-ENCODINGS* under NAME and NICKNAMES, replacing any old
;;; name and nicknames it has.
(defun rename-character-encoding (encoding name &optional nicknames)
  (let ((table *character-encodings*)
        (names (cons name nicknames)))
    (with-locked-hash-table (table)
      ;; Check that the names we want are not taken by others.
      (dolist (nick names)
        (awhen (gethash nick table)
          (unless (eq encoding it)
            (error "~S already names a character encoding: ~S."
                   name it))))
      ;; Detach from old names.
      (flet ((drop (nick)
               (aver (eq encoding (gethash nick table)))
               (remhash nick table)))
        (awhen (character-encoding-name encoding)
          (unless (eq name it)
            (drop it)))
        (dolist (nick (character-encoding-nicknames encoding))
          (unless (member nick nicknames :test #'eq)
            (drop nick))))
      ;; Attach to new names.
      (setf (character-encoding-name encoding) name
            (character-encoding-nicknames encoding) nicknames)
      (dolist (nick names)
        (setf (gethash name table) encoding)))
    encoding))

(define-condition unknown-character-encoding-error (cell-error) ()
  (:report (lambda (condition stream)
             (format stream "There is no character encoding called ~S."
                     (cell-error-name condition)))))

(defun find-character-encoding (name &optional (errorp t))
  (if (typep name 'character-encoding)
      name
      (or (gethash name *character-encodings*)
          (when errorp
            (restart-case
                (error 'unknown-character-encoding-error :name name)
              (use-value (value)
                (find-character-encoding value errorp))
              (continue ()
                nil))))))

(defmacro define-character-encoding (name &body options)
  (let ((nicknames (pop-assoc :nicknames options)))
    `(let ((old (find-character-encoding ',name nil))
           (new (make-character-encoding
                 ,@(mapcan #'identity options)
                 :source-location (sb-c:source-location))))
       (when old
         (style-warn "Redefining character encoding ~S." ',name)
         (delete-character-encoding old))
       (rename-character-encoding new ',name ',nicknames))))

(defmacro do-code ((code char) &body body)
  (let (styles)
    (loop repeat 2
          do (let ((style (pop body)))
               (assert (member (car style) '(:cr :crlf)))
               (assert (not (assoc (car style) styles)))
               (push style styles)))
    `(let ((,code (char-code ,char)))
       ;; This is use being cleaver: for :LF style we don't need to
       ;; check if we have a newline, but can use the regular encode.
       (if (eq :lf (eol-style))
           (locally ,@body)
           (if (eql ,code (char-code #\newline))
               (ecase (eol-style)
                 (:cr ,@(cdr (assoc :cr styles)))
                 (:crlf ,@(cdr (assoc :crlf styles))))
               (locally ,@body))))))

(defmacro eol-style ()
  (error "bad"))

(defmacro defencoder (encoding (src src-offset dst dst-offset length limit)
                      &body body)
  (let ((name (symbolicate encoding "-ENCODER"))
        (eol (gensym "EOL")))
    `(progn
       (defun ,name (,src ,src-offset ,dst ,dst-offset ,length ,limit ,eol)
           (declare (string ,src)
            (type (or (simple-array (unsigned-byte 8) (*)) system-area-pointer) ,dst)
            (index ,src-offset ,dst-offset ,length ,limit))
         (labels ((encode-to-sap (,dst)
                    (etypecase ,src
                      ((simple-array character (*))
                       (encode-from-string ,src ,dst))
                      ((simple-array base-char (*))
                       (encode-from-string ,src ,dst))))
                  (encode-from-string (,src ,dst)
                    (declare (system-area-pointer ,dst))
                    (declare (muffle-conditions code-deletion-note))
                    (macrolet ((using-eol (eol-style)
                                 `(macrolet ((eol-style () ,eol-style))
                                    ,@',body)))
                      (ecase ,eol
                       (:lf (using-eol :lf))
                       (:crlf (using-eol :crlf))
                       (:cr (using-eol :cr))))))
           (declare (inline encode-from-string))
           (etypecase ,dst
             (system-area-pointer
              (encode-to-sap ,dst))
             ((simple-array (unsigned-byte 8) (*))
              (with-pinned-objects (,dst)
                (encode-to-sap (vector-sap ,dst)))))))
             (let ((encoding (find-character-encoding ',encoding t)))
               (setf (character-encoding-encoder encoding) #',name)))))

(defmacro defdecoder (encoding (src src-offset dst dst-offset length limit eol-0 eol-1) &body body)
  `(let ((encoding (find-character-encoding ',encoding)))
     (setf (character-encoding-decoder encoding)
           (named-lambda ,(symbolicate encoding "-DECODER")
               (,src ,src-offset ,dst ,dst-offset ,length ,limit ,eol-0 ,eol-1)
             (declare (system-area-pointer ,src)
                      (string ,dst)
                      (index ,src-offset ,dst-offset ,length ,eol-0)
                      (type (or null index) ,eol-1))
             ,@body))))

(defmacro define-encoded-length (encoding (src src-offset length limit eol-0 eol-1) &body body)
  `(let ((encoding (find-character-encoding ',encoding)))
     (setf (character-encoding-encoded-length encoding)
           (named-lambda ,(symbolicate encoding "-ENCODED-LENGTH")
               (,src ,src-offset ,length ,limit ,eol-0 ,eol-1)
             (declare (string ,src)
                      (index ,src-offset ,length ,eol-0)
                      (type (or index null) ,limit ,eol-1))
             ,@body))))

(defmacro define-decoded-length (encoding (src src-offset length limit eol-0 eol-1) &body body)
  `(let ((encoding (find-character-encoding ',encoding)))
     (setf (character-encoding-decoded-length encoding)
           (named-lambda ,(symbolicate encoding "-DECODED-LENGTH")
               (,src ,src-offset ,length ,limit ,eol-0 ,eol-1)
             (declare (system-area-pointer ,src)
                      (index ,src-offset ,length ,eol-0)
                      (type (or index null) ,limit ,eol-1))
             ,@body))))

;;; Similar tool for decoding.
(defmacro with-eol-decoding ((eol-code-0 eol-code-1) &body body)
  (with-unique-names (eol-0 eol-1 last)
    `(let ((,eol-0 ,eol-code-0)
           (,eol-1 ,eol-code-1))
       (if ,eol-1
           ;; Two character EOL
           (let ((,last 1))
             (flet ((set-code (code string offset)
                      ;; Using bit-operations allows us to do the comparison
                      ;; of both last character and current one with a single
                      ;; branch.
                      (if (zerop (logior ,last (logxor ,eol-1 code)))
                          ;; EOL sequence: rewrite previous character as
                          ;; newline instead.
                          (setf code ,(char-code #\newline)
                                index (1- index)
                                ,last 1)
                          ;; Not EOL
                          (setf ,last (logxor ,eol-0 code)))
                      (setf (char string index) (code-char code))
                      ;; Return the next index
                      (values (1+ index))))
               (declare (inline set-code))
               ,@body))
           ;; Single character EOL
           (flet ((set-code (code string index)
                    (when (= code ,eol-0)
                      (setf code ,(char-code #\newline)))
                    (setf (char string index) (code-char code))
                    (1+ index)))
             (declare (inline set-code))
             ,@body)))))

(defun unibyte-encoded-length (src src-offset length limit eol-0 eol-1)
  (cond ((not eol-1)
         ;; Single byte EOL
         (let ((octets (if limit
                           (min limit length)
                           length)))
           (values octets (+ src-offset octets))))
        ((not limit)
         ;; Two byte EOL, no limit
         (let ((octets (+ length (count #\newline src
                                        :start src-offset
                                        :end (+ src-offset length)))))
           (values octets (+ src-offset length))))
        (t
         ;; Two byte EOL and limit. Take care here! We don't want to split a
         ;; two-char newline across LIMIT.
         (let ((octets 0)
               (new 0)
               (i 0))
           (tagbody
            :more
              (if (eq #\newline (char src (+ src-offset i)))
                  (setf new (+ octets 2))
                  (setf new (+ octets 1)))
              (when (> new limit)
                (go :end))
              (setf octets new
                    i (1+ i))
              (when (< i length)
                (go :more))
            :end)
           (values octets (+ src-offset i))))))

(defmacro define-unibyte-encoder (encoding (char-code) &body body)
  (with-unique-names (src src-offset dst dst-offset length limit i j
                      limit-1)
    `(progn
       (defencoder ,encoding
           (,src ,src-offset ,dst ,dst-offset ,length ,limit)
         (let ((,limit-1 (- ,limit 1)))
           (do ((,i 0 (1+ ,i))
                (,j 0))
               ((or (= ,i ,length) (>= ,j ,limit))
                (values ,i ,j))
             (declare (index ,i ,j) (optimize (safety 0) (speed 3)))
             (do-code (,char-code (char ,src (+ ,src-offset ,i)))
               (:cr
                (setf (sap-ref-8 ,dst (+ ,dst-offset ,j)) 13)
                (incf ,j))
               (:crlf
                (when (= ,j ,limit-1)
                  (return (values ,i ,j)))
                (setf (sap-ref-8 ,dst (+ ,dst-offset ,j)) 13)
                (setf (sap-ref-8 ,dst (+ ,dst-offset ,j 1)) 10)
                (incf ,j 2))
               (setf (sap-ref-8 ,dst (+ ,dst-offset ,j))
                     (macrolet
                         ((handle-error ()
                            `(unibyte-encoding-error ',,encoding ,',char-code)))
                       ,@body))
               (incf ,j)))))
       (let ((encoding (find-character-encoding ',encoding t)))
         (setf (character-encoding-encoded-length encoding)
               #'unibyte-encoded-length)))))

(defun unibyte-decoded-length (src src-offset length limit code0 code1)
  (declare (system-area-pointer src)
           (index src-offset length)
           (type (or null index) limit))
  (cond ((not code1)
         ;; Single byte EOL
         (let ((chars (if limit
                          (min limit length)
                          length)))
           (values chars (+ src-offset chars 1))))
        ((not limit)
         ;; Two byte EOL, no limit
         (let ((chars 0)
               ;; First index which cannot be followed by an EOL (because
               ;; there isn't space for one.)
               (stop (- length 1))
               (eol (logior (ash code0 8) code1)))
           (declare (index chars stop))
           (tagbody
            :more
              (if (and (< src-offset stop)
                       (= eol (sap-ref-16 src src-offset)))
                  (setf src-offset (+ src-offset 2))
                  (setf src-offset (+ src-offset 1)))
              (when (< src-offset length)
                (incf chars)
                (go :more))
            :end)
           (values chars src-offset)))
        (t
         ;; Two byte EOL and limit.
         (let ((chars 0)
               (new 0)
               ;; First index which cannot be followed by an EOL (because
               ;; there isn't space for one.)
               (stop (- length 1))
               (eol (logior (ash code0 8) code1)))
           (declare (index chars new stop))
           (tagbody
            :more
              (setf new (+ chars 1))
              (if (and (< src-offset stop)
                       (= eol (sap-ref-16 src src-offset)))
                  (setf src-offset (+ src-offset 2))
                  (setf src-offset (+ src-offset 1)))
              (when (and (< src-offset length)
                         (<= new limit))
                (setf chars new)
                (go :more))
            :end)
           (values chars src-offset)))))

(defmacro define-unibyte-decoder (encoding (byte) &body body)
  (with-unique-names (src src-offset dst dst-offset length limit i j)
    `(progn
       (defdecoder ,encoding (,src ,src-offset ,dst ,dst-offset ,length ,limit)
         (do ((,i 0 (1+ i))
              (,j 0))
             ((or (= ,i ,length) (>= ,j ,limit))
              (values ,i ,j))
           (let* ((,byte (sap-ref-8 ,src (+ ,src-offset ,i)))
                  (char-code
                    (macrolet ((handle-error ()
                                 `(unibyte-decoding-error ',,encoding ,',byte)))
                      ,@body)))
             (setf ,j (set-code char-code ,dst (+ ,dst-offset ,j))))))
       (let ((encoding (find-character-encoding ',encoding t)))
         (setf (character-encoding-decoded-length encoding)
               #'unibyte-decoded-length)))))
