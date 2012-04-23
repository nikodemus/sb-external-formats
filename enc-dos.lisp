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

;;;; CP437

(define-unibyte-character-encoding :cp437
    (:cp-347 :oem427 :oem-437 :pc8 :pc-8)
  "Encoding representing the character set of the original IBM PC."
  "unicode/CP437.TXT")

;;;; CP 737

(define-unibyte-character-encoding :cp737
    ()
  "MS-DOS encoding for Greek. (See also CP869.)"
  "unicode/CP737.TXT")

;;;; CP 775

(define-unibyte-character-encoding :cp775
    ()
  "MS-DOS encoding for Estonian, Lithuanian, and Latvian."
  "unicode/CP775.TXT")

;;;; CP 850

(define-unibyte-character-encoding :cp850
  ()
  "MS-DOS encoding for Western Europe."
  "unicode/CP850.TXT")

;;;; CP 852

(define-unibyte-character-encoding :cp852
    ()
  "MS-DOS encoding for Central Europe."
  "unicode/CP852.TXT")

;;;; CP 855

(define-unibyte-character-encoding :cp855
    ()
  "MS-DOS encoding for Cyrillic."
  "unicode/CP855.TXT")

;;;; CP 857

(define-unibyte-character-encoding :cp857
    ()
  "MS-DOS encoding for Turkish."
  "unicode/CP857.TXT")

;;;; CP 860

(define-unibyte-character-encoding :cp860
    ()
  "MS-DOS encoding for Portuguese."
  "unicode/CP860.TXT")

;;;; CP 861

(define-unibyte-character-encoding :cp861
    ()
  "MS-DOS encoding for Icelandic."
  "unicode/CP861.TXT")

;;;; CP 862

(define-unibyte-character-encoding :cp862
    ()
  "MS-DOS encoding for Herbew."
  "unicode/CP862.TXT")

;;;; CP 863

(define-unibyte-character-encoding :cp863
    ()
  "MS-DOS encoding for French, mainly used in Quebec."
  "unicode/CP863.TXT")

;;;; CP 864

(define-unibyte-character-encoding :cp864
    ()
  "MS-DOS encoding for Arabic."
  "unicode/CP864.TXT")

;;;; CP 865

(define-unibyte-character-encoding :cp865
    ()
  "MS-DOS encoding for Nordic languages."
  "unicode/CP865.TXT")

;;;; CP 866

(define-unibyte-character-encoding :cp866
    ()
  "MS-DOS encoding for Cyrillic."
  "unicode/CP866.TXT")

;;;; CP 869

(define-unibyte-character-encoding :cp869
    ()
  "MS-DOS encoding for Greek. (See also CP737.)"
  "unicode/CP869.TXT")

;;;; CP 874

(define-unibyte-character-encoding :cp874
    ()
  "MS-DOS encoding for Thai."
  "unicode/CP874.TXT")
