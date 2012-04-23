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

(defun encoding-stub (slot)
  (lambda (&rest args)
    (declare (ignore args))
    (error "~S not available for this character encoding." slot)))

;;;; A CHARACTER-ENCODING object manages all encoding and decoding
;;;; related activities.
;;;;
;;;; It contains functions that perform those duties for the encoding
;;;; in question.
;;;;
;;;; An EXTERNAL-FORMAT object is a combination of a CHARACTER-ENCODING
;;;; and an EOL policy, and an error handling policy.

(defstruct character-encoding
  ;; Canonical name.
  (name nil :type symbol)
  ;; Nicknames.
  (nicknames nil :type name-list)
  ;; Documentation
  (documentation nil :type (or null string))
  ;; These implement the guts of ENCODE-STRING and DECODE-OCTETS.
  (encoder (encoding-stub 'encoder) :type function)
  (decoder (encoding-stub 'decoder) :type function)
  ;; These implement the guts of ENCODED-LENGTH and DECODED-LENGTH.
  (encoded-length (encoding-stub 'encoded-length) :type function)
  (decoded-length (encoding-stub 'decoded-length) :type function)
  ;; These implement the guts of GUESS-ENCODED-LENGTH and GUESS-DECODED-LENGTH.
  (guess-encoded-length (encoding-stub 'guess-encoded-length) :type function)
  (guess-decoded-length (encoding-stub 'guess-decoded-length) :type function)
  ;; Source location for the DEFINE-CHARACTER-ENCODING.
  (source-location nil)
  ;; Character repertoire
  (repertoire nil :type list)
  ;; True if this encoding can be used to round-trip arbitrary octets
  (binary nil :type boolean))

(defun %encode (encoding string string-offset buf buf-offset count x eol)
  (funcall (character-encoding-encoder encoding)
           string string-offset buf buf-offset count x eol))

(defun %decode (encoding octets octets-offset sap sap-offset count x eol)
  (funcall (character-encoding-decoder encoding)
           octets octets-offset sap sap-offset count x eol))

#+nil ; FIXME
(defun %encoded-length (encoding string start length limit code1 code2)
  (funcall (character-encoding-encoded-length encoding)
           string start length limit code1 code2))

#+nil ; FIXME
(defun %decoded-length (encoding string start length limit code1 code2)
  (funcall (character-encoding-decoded-length encoding)
           string start length limit code1 code2))

(defparameter *character-encodings* (make-hash-table :test 'eq :synchronized t))

;;; Removes a CHARACTER-ENCODING from *CHARACTER-ENCODINGS*, and
;;; clears its NAME and NICKNAMES.
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

;;; Adds CHARACTER-ENCODING to *CHARACTER-ENCODINGS* under NAME and
;;; NICKNAMES, replacing any old name and nicknames it has.
(defun rename-character-encoding (encoding name &optional nicknames)
  (let ((table *character-encodings*)
        (names (cons name nicknames))
        (problem nil))
    (tagbody
       (with-locked-hash-table (table)
         ;; Check that the names we want are not taken by others.
         (dolist (nick names)
           (awhen (gethash nick table)
             (unless (eq encoding it)
               (setf problem (cons name it))
               (go :error))))
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
           (setf (gethash nick table) encoding)))
     :error
       (when problem
         (error "~S already names a character encoding: ~S."
                (car problem) (cdr problem))))
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
  (let ((nicknames (pop-assoc :nicknames options))
        (repertoire (pop-assoc :repertoire options)))
    `(let ((old (find-character-encoding ',name nil))
           (new (make-character-encoding
                 ,@(apply #'append options)
                 :repertoire ',repertoire
                 :source-location (sb-c:source-location))))
       (when old
         (style-warn "Redefining character encoding ~S." ',name)
         (delete-character-encoding old))
       (rename-character-encoding new ',name ',nicknames))))

(defmacro do-encode ((code char) &body body)
  (let (styles)
    (loop repeat 2
          do (let ((style (pop body)))
               (assert (member (car style) '(:cr :crlf)))
               (assert (not (assoc (car style) styles)))
               (push style styles)))
    `(let ((,code (char-code ,char)))
       ;; This is us being cleaver: for :LF style we don't need to check if we
       ;; have a newline, but can use the regular encode.
       (tagbody
        encode
          (if (eq :lf (eol-style))
              (locally ,@body)
              (if (eql ,code (char-code #\newline))
                  (ecase (eol-style)
                    (:cr ,@(cdr (assoc :cr styles)))
                    (:crlf ,@(cdr (assoc :crlf styles))))
                  (locally ,@body)))))))

(deftype eol-style ()
  `(member :cr :lf :crlf))

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

(define-symbol-macro set-char-code-eol-style nil)
(define-symbol-macro set-char-code-eol-mark nil)
(define-symbol-macro set-char-code-dst nil)
(define-symbol-macro set-char-code-dst-offset nil)

(defmacro set-char-code (index char-code &environment env)
  (let ((dst (or (macroexpand 'set-char-code-dst env)
                 (make-symbol "?-DST")))
        (dst-offset (or (macroexpand 'set-char-code-dst-offset env)
                        (make-symbol "?-DST-OFFSET")))
        (eol-mark (or (macroexpand 'set-char-code-eol-mark env)
                      (make-symbol "?-EOL-MARK"))))
    (with-unique-names (code)
      `(let ((,code ,char-code))
         (declare (type (integer 0 (,char-code-limit)) ,code))
         ,@(ecase (or (macroexpand 'set-char-code-eol-style env)
                      (restart-case
                          (error "Not in an environment with EOL-STYLE.")
                        (select-cr () :cr)
                        (select-lf () :lf)
                        (select-crlf () :crlf)))
             (:crlf
              ;; Using bit-operations allows us to do the comparison
              ;; of both last character and current one with a single
              ;; branch. If we hit a CRLF, we rewrite the last CR.
              `((when (zerop (logior ,eol-mark (logxor ,code ,(char-code #\newline))))
                  (setf ,index (1- ,index)))
                (setf ,eol-mark (logxor ,code ,(char-code #\return)))
                (setf (char ,dst (+ ,dst-offset ,index)) (code-char ,code))))
             (:cr
              `((when (= ,code ,(char-code #\return))
                  (setf ,code ,(char-code #\newline)))
                (setf (char ,dst (+ ,dst-offset ,index)) (code-char ,code))))
             (:lf
              `((setf (char ,dst (+ ,dst-offset ,index)) (code-char ,code)))))))))

(defmacro defdecoder (encoding (src src-offset dst dst-offset length limit) &body body)
  "Define a function to decode octets into strings. SRC receives the source, either
a SAP or a (SIMPLE-ARRAY (UNSIGNED-BYTE 8). SRC-OFFSET is the offset from the start
of the source in bytes. DST is the string to decode into, starting from DST-OFFSET.
LENGTH is the number of bytes to decode, and LIMIT is the maximum number of characters
to decode into.

The defined function should return as multiple values the number of octets decoded,
the number of characters they decoded into, and a tertiary value that is true iff
any remaining bytes in the given input are too short to consitute a valid code sequence.

Use macro SET-CHAR-CODE in the body to write to DST."
  (with-unique-names (eol-mark eol typed-dst)
    (let ((name (symbolicate encoding "-DECODER")))
      `(progn
         (defun ,name (,src ,src-offset ,dst ,dst-offset ,length ,limit ,eol)
           (declare (type (and simple-string (not (simple-array nil (*)))) ,dst)
                    (index ,src-offset ,dst-offset ,length ,limit)
                    (eol-style ,eol)
                    (optimize speed))
           (labels ((decode-from-sap (,src)
                      (declare (system-area-pointer ,src))
                      (etypecase ,dst
                        ((simple-array character (*))
                         (locally (declare (type (simple-array character (*)) ,dst))
                           (decode-to-string ,src ,dst)))
                        ((simple-array base-char (*))
                         (locally (declare (type (simple-array base-char (*)) ,dst))
                           (decode-to-string ,src ,dst)))))
                    (decode-to-string (,src ,typed-dst)
                      (declare (muffle-conditions code-deletion-note))
                      (macrolet ((using-eol (eol-style)
                                   `(symbol-macrolet ((set-char-code-eol-style ,eol-style)
                                                      (set-char-code-eol-mark ,',eol-mark)
                                                      (set-char-code-dst ,',typed-dst)
                                                      (set-char-code-dst-offset ,',dst-offset))
                                      ,@',body)))
                        (let ((,eol-mark 1))
                          (declare (sb-ext:word ,eol-mark))
                          (ecase ,eol
                            (:lf (using-eol :lf))
                            (:crlf (using-eol :crlf))
                            (:cr (using-eol :cr)))))))
             (declare (inline decode-to-string))
             (etypecase ,src
               (system-area-pointer
                (decode-from-sap ,src))
               ((simple-array (unsigned-byte 8) (*))
                (with-pinned-objects (,src)
                  (decode-from-sap (vector-sap ,src)))))))
         (let ((encoding (find-character-encoding ',encoding)))
           (setf (character-encoding-decoder encoding) #',name))))))

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

(define-condition ecoding-error (error)
  ((character :initarg :character :reader encoding-error-character)
   (encoding :initarg :encoding :reader encoding-error-encoding))
  (:report (lambda (condition stream)
             (let ((character (encoding-error-character condition)))
               (format stream "Character ~S (~S) cannot be encoded as ~A."
                       character
                       (char-code character)
                       (encoding-error-encoding condition))))))

(define-condition decoding-error (error)
  ((octets :initarg :octets :reader decoding-error-octets)
   (encoding :initarg :encoding :reader decoding-error-encoding))
  (:report (lambda (condition stream)
             (format stream "Octet sequence ~S cannot be decoded as ~A."
                     (decoding-error-octets condition)
                     (decoding-error-encoding condition)))))

(declaim (ftype (function (t char-code) (values (or null char-code) &optional))
                encoding-error))
(defun encoding-error (encoding char-code)
  (restart-case
      (error 'encoding-error
             :character (code-char char-code)
             :encoding encoding)
    (use-value (character)
      :report "Provide an alternative character encode instead."
      (check-type character character)
      (char-code character))
    (continue ()
      :report "Continue encoding, dropping this character."
      nil)))

(defmacro define-unibyte-encoder (encoding (char-code) &body body)
  (with-unique-names (src src-offset dst dst-offset length limit i j
                      limit-1 replacement done)
    `(progn
       (defencoder ,encoding
           (,src ,src-offset ,dst ,dst-offset ,length ,limit)
         (let ((,limit-1 (- ,limit 1)))
           (do ((,i 0 (1+ ,i))
                (,j 0))
               ((or (= ,i ,length)  (>= ,j ,limit))
                (values ,i ,j))
             (declare (index ,i ,j) (optimize (safety 0) (speed 3)))
             (do-encode (,char-code (char ,src (+ ,src-offset ,i)))
               (:cr
                (setf (sap-ref-8 ,dst (+ ,dst-offset ,j)) 13)
                (incf ,j))
               (:crlf
                (when (= ,j ,limit-1)
                  (return (values ,i ,j)))
                (setf (sap-ref-8 ,dst (+ ,dst-offset ,j)) 13)
                (setf (sap-ref-8 ,dst (+ ,dst-offset ,j 1)) 10)
                (incf ,j 2))
               (tagbody
                  (setf (sap-ref-8 ,dst (+ ,dst-offset ,j))
                        (macrolet
                            ((handle-error ()
                               `(let ((,',replacement
                                        (encoding-error ',,encoding ,',char-code)))
                                  (cond (,',replacement
                                         (setf ,',char-code ,',replacement)
                                         (go encode))
                                        (t
                                         (go ,',done))))))
                          ,@body))
                  (incf ,j)
                  ,done)))))
       (let ((encoding (find-character-encoding ',encoding t)))
         (setf (character-encoding-encoded-length encoding)
               #'unibyte-encoded-length)))))

(declaim (ftype (function (t list) (values (or null char-code) &optional))
                decoding-error))
(defun decoding-error (encoding octets)
  (restart-case
      (error 'decoding-error
             :octets octets
             :encoding encoding)
    (use-value (character)
      :report "Provide a character to decode as."
      (check-type character character)
      (char-code character))
    (continue ()
      :report "Skip this octet sequence."
      nil)))

(defmacro define-unibyte-decoder (encoding (byte) &body body)
  (with-unique-names (src src-offset dst dst-offset length limit i j replacement skip)
    `(progn
       (defdecoder ,encoding (,src ,src-offset ,dst ,dst-offset ,length ,limit)
         (do ((,i 0 (1+ ,i))
              (,j 0 (1+ ,j)))
             ((or (= ,i ,length) (>= ,j ,limit))
              (values ,i ,j))
           (declare (index ,i ,j) (optimize (safety 0) (speed 3)))
           (set-char-code ,j
                          (macrolet ((handle-error ()
                                       `(let ((,',replacement
                                                (decoding-error ',,encoding (list ,',byte))))
                                          (cond (,',replacement
                                                 ,',replacement)
                                                (t
                                                 (decf ,',j)
                                                 (go ,',skip))))))
                            (let ((,byte (sap-ref-8 ,src (+ ,src-offset ,i))))
                              ,@body)))
           ,skip))
       (let ((encoding (find-character-encoding ',encoding t)))
         (setf (character-encoding-decoded-length encoding)
               #'unibyte-decoded-length)))))

;;; Converting bytes to character codes is easy: just use a 256-element
;;; lookup table that maps each possible byte to its corresponding
;;; character code.
;;;
;;; Converting character codes to bytes is a little harder, since the
;;; codes may be spare (e.g. we use codes 0-127, 3490, and 4598).  The
;;; previous version of this macro utilized a gigantic CASE expression
;;; to do the hard work, with the result that the code was huge (since
;;; SBCL's then-current compilation strategy for CASE expressions was
;;; (and still is) converting CASE into COND into if-the-elses--which is
;;; also inefficient unless your code happens to occur very early in the
;;; chain.
;;;
;;; The current strategy is to build a table:
;;;
;;; [ ... code_1 byte_1 code_2 byte_2 ... code_n byte_n ... ]
;;;
;;; such that the codes are sorted in order from lowest to highest.  We
;;; can then binary search the table to discover the appropriate byte
;;; for a character code.  We also implement an optimization: all unibyte
;;; mappings do not remap ASCII (0-127) and some do not remap part of
;;; the range beyond character code 127.  So we check to see if the
;;; character code falls into that range first (a quick check, since
;;; character codes are guaranteed to be positive) and then do the binary
;;; search if not.  This optimization also enables us to cut down on the
;;; size of our lookup table.
(defmacro define-unibyte-character-encoding (name nicknames docstring pathname)
  (let (;; List of (char-code octet) pairs when they differ.
        (exceptions nil)
        ;; Complete list of defined (char-code octet) pairs.
        (pairs nil)
        ;; T if there are holes in the encoding -- ie. if it cannot round-trip
        ;; arbitrary octets.
        (holes nil)
        (last -1))
    (flet ((skip (line)
             (or (zerop (length line))
                 (eql #\# (char line 0))))
           (parse (line)
             (let* ((p (or (position #\tab line)
                           (return-from parse nil)))
                    (s1 (subseq line 0 p))
                    (s2 (string-trim
                         " " (subseq line (1+ p) (position #\tab line :start (1+ p))))))
               (setf (char s1 0) #\#)
               (if (plusp (length s2))
                   (setf (char s2 0) #\#)
                   (setf s2 nil))
               (let ((n1 (read-from-string s1))
                     (n2 (when s2 (read-from-string s2))))
                 (loop for hole from (1+ last) below n1
                       do (push (list hole nil) exceptions)
                          (setf holes t))
                 (setf last n1)
                 (unless (eql n1 n2)
                   (push (list n1 n2) exceptions))
                 (if n2
                     (push (list n2 n1) pairs)
                     (setf holes t))))))
      (with-open-file (f (merge-pathnames pathname
                                         (asdf:component-pathname
                                          (asdf:find-system :sb-external-formats)))
                        :external-format :ascii)
       (loop for line = (read-line f nil nil)
             while line
             do (unless (skip line)
                  (handler-case
                      (parse line)
                    (error (e)
                      (error "Error parsing line ~S:~%  ~A" line e))))))
      (loop for hole from (1+ last) below 256
            do (push (list hole nil) exceptions)
               (setf holes t))
      (setf pairs (nreverse pairs)
            exceptions (nreverse exceptions))
      ;;
      ;; Done parsing the file
      ;;
      (let* (;; Find the smallest character code such that the corresponding
             ;; byte is != to the code.
             (lowest-non-equivalent-code
               (caar (sort (copy-seq exceptions) #'< :key #'first)))
             ;; Sort them for our lookup table.
             (sorted-pairs (sort (if lowest-non-equivalent-code
                                     (subseq pairs lowest-non-equivalent-code)
                                     (copy-seq pairs))
                                 #'< :key #'car))
             ;; Create the lookup table.
             (sorted-lookup-table
               (reduce #'append sorted-pairs :from-end t :initial-value nil))
             (encoder-table (symbolicate "**" name "-ENCODER-TABLE**"))
             (decoder-table (symbolicate "**" name "-DECODER-TABLE**"))
             (repertoire nil))
        ;; Figure out the repertoire.
        (let ((low (caar pairs))
              (hi (caar pairs))
              (chars nil))
          (dolist (pair (cdr pairs))
            (cond ((eql hi (1- (car pair)))
                   (setf hi (car pair)))
                  (t
                   (if (< low hi)
                       (push (cons low hi) repertoire)
                       (push (code-char low) chars))
                   (setf low (car pair)
                         hi (car pair)))))
          (if (< low hi)
              (push (cons low hi) repertoire)
              (push (code-char low) chars))
          (setf repertoire (reverse (append chars repertoire))))
        `(progn
           (define-character-encoding ,name
             (:nicknames ,@nicknames)
             (:documentation ,docstring)
             (:repertoire ,@repertoire)
             (:binary ,(not holes)))
           ,@(when exceptions
               `((declaim (type (simple-array fixnum (*)) ,decoder-table))
                 (defglobal ,decoder-table
                     ,(make-array 256 :element-type 'fixnum
                                      :initial-contents
                                      (loop for octet below 256
                                            collect
                                               (let ((pair (assoc octet exceptions)))
                                                 (cond (pair
                                                        (or (second pair) -1))
                                                       (t
                                                        octet))))))
                 (declaim (type (simple-array char-code (*)) ,encoder-table))
                 (defglobal ,encoder-table
                     ,(make-array (length sorted-lookup-table)
                                  :element-type 'char-code
                                  :initial-contents sorted-lookup-table))))
           (define-unibyte-decoder ,name (octet)
             ,(cond (holes
                     `(let ((code (aref ,decoder-table octet)))
                        (if (minusp code)
                            (handle-error)
                            code)))
                    (exceptions
                     `(aref ,decoder-table octet))
                    (t
                     'octet)))
           (define-unibyte-encoder ,name (char-code)
             ,(if exceptions
                  `(if (< char-code ,lowest-non-equivalent-code)
                       char-code
                       ;; FIXME: Check performance
                       (loop with low = 0
                             with high = ,(- (length sorted-lookup-table) 2)
                             while (< low high)
                             do (let ((mid (logandc2 (truncate (+ low high 2) 2) 1)))
                                  (if (< char-code (aref ,encoder-table mid))
                                      (setf high (- mid 2))
                                      (setf low mid)))
                             finally (return (if (eql char-code (aref ,encoder-table low))
                                                 (aref ,encoder-table (1+ low))
                                                 (handle-error)))))
                  'char-code)))))))

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
