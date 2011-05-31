(in-package :sb-external-format)

(deftype eol-style ()
  `(member :cr :lf :crlf))

(declaim (eol-style *default-eol-style*))
(defvar *default-eol-style* #+win32 :crlr #-win32 :lf)

(defstruct external-format
  (character-encoding (missing-arg) :type character-encoding)
  (eol-style *default-eol-style* :type eol-style))

(defun find-external-format (external-format &optional (errorp t))
  (etypecase external-format
    (external-format external-format)
    (symbol
     (awhen (find-character-encoding external-format errorp)
       (make-external-format :character-encoding it)))
    (cons
     (destructuring-bind (name &key (eol-style *default-eol-style*)) external-format
       (make-external-format
        :character-encoding (find-character-encoding name errorp)
        :eol-style eol-style)))))

(defun external-format-eol-info (external-format)
  (let* ((format (find-external-format external-format t))
         (encoding (external-format-character-encoding external-format)))
    (cdr (or (assoc (external-format-eol-style format)
                    (character-encoding-eol-info encoding))
             (error "~S is not a supported EOL style with ~S."
                    (external-format-eol-style format)
                    encoding)))))

