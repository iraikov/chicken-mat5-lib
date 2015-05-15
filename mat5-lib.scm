;;
;; 
;; Definitions and read/write routines for MAT 5.0 binary format.
;;
;; Copyright 2005-2015 Ivan Raikov.
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; A full copy of the GPL license can be found at
;; <http://www.gnu.org/licenses/>.
;;


(module mat5-lib 


	( 
	 MAT5:data-type? 		 

	 miINT8 miUINT8 miINT16 miUINT16 miINT32 miUINT32 
	 miSINGLE miDOUBLE miINT64 miUINT64 miMATRIX 
	 miCOMPRESSED miUTF8 miUTF16 miUTF32

	 make-MAT5:header MAT5:header?
	 MAT5:header-magic MAT5:header-text MAT5:header-subsys
	 MAT5:header-version MAT5:header-eport

	 make-MAT5:data-element MAT5:data-element?
	 MAT5:data-element-type MAT5:data-element-bytes MAT5:data-element-data 

	 MAT5:array? MAT5:object MAT5:structure MAT5:cell-array MAT5:sparse-array MAT5:num-array
	 MAT5:dimensions?
	 MAT5:numeric-type? 
	 MAT5:array-type? 

	 init-MAT5:cell MAT5:cell? MAT5:cell-data MAT5:cell-dims
	 vector->MAT5:cell
	 MAT5:cell-dims 
	 MAT5:cell-ref  
	 MAT5:cell-set!

	 MAT5:array-foldi
	 MAT5:array->srfi-4-vector

	 MAT5:read-header		 
	 MAT5:read-data-element

	 MAT5:write-header
	 MAT5:write-data-element

	 MAT5:debug-level
	 )

	(import scheme chicken)
	(require-library srfi-1 srfi-13 srfi-14 data-structures extras posix)
	(require-library endian-blob srfi-63)
	(import (only srfi-1 first second third last list-tabulate filter every)
		(only srfi-13 string-pad-right string-trim-right)
		(only srfi-14 char-set char-set-union char-set:whitespace)
		(only data-structures alist-ref string-intersperse)
		(only extras fprintf sprintf)
		(only posix file-write open-output-file* file-mkstemp)
		(only srfi-63 
		      A:floR64b A:floR32b A:floR16b
		      A:fixZ64b A:fixZ32b A:fixZ16b A:fixZ8b 
		      A:fixN64b A:fixN32b A:fixN16b A:fixN8b
		      array? make-array array-dimensions array-ref array-set! )
		(only endian-blob MSB LSB))
	(require-extension srfi-4 datatype endian-port iset z3)


;--------------------
;  Message routines
;
;

(define MAT5:debug-level (make-parameter 0))


(define (MAT5:warning x . rest)
  (let loop ((port (open-output-string)) (objs (cons x rest)))
    (if (null? objs)
	(begin
	  (newline port)
	  (print-error-message (get-output-string port) 
			       (current-error-port) "MAT5 warning:"))
	(begin (display (car objs) port)
	       (display " " port)
	       (loop port (cdr objs))))))


(define (MAT5:error x . rest)
  (let ((port (open-output-string)))
    (if (endian-port? x)
	(begin
	  (display "[" port)
	  (display (endian-port-filename x) port)
	  (display ":" port)
	  (display (endian-port-pos x) port)
	  (display "] " port)))
    (let loop ((objs (if (endian-port? x) rest (cons x rest))))
      (if (null? objs)
	  (begin
	    (newline port)
	    (error 'MAT5 (get-output-string port)))
	  (begin (display (car objs) port)
		 (display " " port)
		 (loop (cdr objs)))
	  ))
    ))


(define-syntax  MAT5:debug
   (syntax-rules  ()
      ((_ level x ...)
       (if (>= (MAT5:debug-level) level)
	   (print (sprintf "MAT5[~A]: " level) x ...)))
      ))


;----------------------------------
;  General-purpose utility routines
;

(define (byte-vector->zstring x)
  (let ((ws  (char-set-union char-set:whitespace (char-set (integer->char 0))))
	(s   (blob->string x)))
    (string-trim-right s ws)))

(define (natural? x) (and (integer? x) (positive? x)))

(define (natural-list? x)
  (and (list? x) (every natural? x)))

(define (positive-or-zero? x) (and (integer? x) (or (zero? x) (positive? x))))

(define (positive-or-zero-list? x)
  (and (list? x) (every positive-or-zero? x)))


(define (string-list? x)
  (and (list? x) (every string? x)))





; Procedure: 
; align-eport-pos:: ENDIAN-PORT -> UNDEFINED
;
; Set file position to 64-bit boundary. Returns the # bytes skipped
;
;   * eport    an endian port 
;
(define (align-eport-pos eport)
  (let* ((pos  (endian-port-pos eport))
	 (pos1  (let loop ((pos1 pos))
		   (if (not (zero? (modulo pos1 8))) 
		       (loop (+ pos1 1)) pos1))))
    (endian-port-setpos eport pos1)
    (- pos1 pos)))



;-------------------------------------
;  MAT-File constants
;

(define MAT5:header-text-size    116)
(define MAT5:header-subsys-size    8)


; Version and magic numbers
(define MAT5:lsb-version  #x0001)
(define MAT5:lsb-magic    #x494D)
(define MAT5:msb-version  #x0100)
(define MAT5:msb-magic    #x4D49)

;
; MAT-File array flags (represented as bit positions)
;
(define mxLOGICAL_FLAG  1)
(define mxGLOBAL_FLAG   2)
(define mxCOMPLEX_FLAG  3)

(define MAT5:field-name-length 32)

;-------------------------------------
;  Structure and datatype definitions
;

; Datatype: MAT5:data-type
;
; MAT-file data types 
(define-datatype MAT5:data-type MAT5:data-type? 
  (miINT8)
  (miUINT8)
  (miINT16)      
  (miUINT16)     
  (miINT32)      
  (miUINT32)     
  (miSINGLE)     
  (miDOUBLE)     
  (miINT64)      
  (miUINT64)     
  (miMATRIX)     
  (miCOMPRESSED)
  (miUTF8)       
  (miUTF16)      
  (miUTF32))

;
; MAT-File array classes
;
(define-datatype MAT5:array-class MAT5:array-class? 
  (mxCELL_CLASS)
  (mxSTRUCT_CLASS)
  (mxOBJECT_CLASS)
  (mxCHAR_CLASS)
  (mxSPARSE_CLASS)
  (mxDOUBLE_CLASS)
  (mxSINGLE_CLASS)
  (mxINT8_CLASS)
  (mxUINT8_CLASS)
  (mxINT16_CLASS)
  (mxUINT16_CLASS)
  (mxINT32_CLASS)
  (mxUINT32_CLASS))


;  Structure: MAT5:header
;
;  MAT-file Level 5 Header Format 
;  
;  * text     is a byte vector of size 116
;  * subsys   is a byte vector of size 8
;  * version  is unsigned 16-bit version number
;  * eport    is an endian port (see module endian-port)
; 
(define-record MAT5:header  magic text subsys version eport)


;  Structure: MAT5:data-element
;
;  MAT-file Level 5 data element  
;  
;  * type     is of type MAT5:data-type
;  * bytes    is the size of the data element in bytes
;  * data     is the data element object -- either a fixnum/flonum or an array
; 
(define-record MAT5:data-element  type bytes data)


;  Structure: MAT5:array-flags
;
;  MAT-file Level 5 array flags
;
;  * flags    a bit vector that contains the mxLOGICAL_FLAG, mxGLOBAL_FLAG,
;             mxCOMPLEX_FLAG flags, if set.
;  * class    array class; see the definition of MAT5:array-class datatype
;  * nzmax    maximum number of non-zero elements in a sparse matrix
;
(define-record MAT5:array-flags  flags  class  nzmax)


; Datatype: MAT5:array
;
; A representation of the different types of MAT-file arrays
;
(define-datatype MAT5:array MAT5:array? 


  ;
  ; Object
  ;
  ;  * dims        dimensions (a list of positive integers) -- these are the
  ;                dimensions of the cells contained in the structure, if any
  ;  * class       class name (string)
  ;  * fields      a cell in which every element is an alist of  fields
  ;
  (MAT5:object      (name         string?)
		    (dims         natural-list?)
		    (class-name   string?)
		    (field-names  string-list?)
		    (fields       MAT5:cell?))

  ; Structure
  ;
  ;  * dims        dimensions (a list of positive integers) -- these are the
  ;                dimensions of the cells contained in the structure, if any
  ;  * fields      a cell in which every element is an alist of  fields
  ;
  (MAT5:structure   (name         string?)
		    (dims         natural-list?)
		    (field-names  string-list?)
		    (fields       MAT5:cell?))

  ;
  ; Cell array
  ;
  ;  * dims        dimensions (a list of positive integers)
  ;  * cell        a MAT5:cell object (nested R5RS vectors)
  ;
  (MAT5:cell-array    (name       string?)
		      (dims       natural-list?)
		      (cell       MAT5:cell?))

  ;
  ; Homogeneous sparse array
  ;
  ;  * data-type   array element MAT5 type (can only be numeric type)
  ;  * dims        dimensions (a list of positive integers)
  ;  * row-index   row indices of non-zero elements
  ;  * col-index   column indices of non-zero elements
  ;  * real        real part (an SRFI-47 array)
  ;  * imag        imaginary part (array or #f)
  ;
  (MAT5:sparse-array    (name       string?)
			(data-type  MAT5:numeric-type?)
                        (dims       MAT5:dimensions?)
			(row-index  natural-list?)
			(col-index  natural-list?)
			(real       (lambda (x) (array? x)))
			(imag       (lambda (x) (or (array? x) (not x)))))

  ;
  ; Homogeneous numeric array
  ;
  ;  * data-type  array element MAT5 type (can only be numeric type)
  ;  * dims       dimensions (a list of positive integers)
  ;  * real       real part (an SRFI-47 array)
  ;  * imag       imaginary part (array or #f)
  ;
  (MAT5:num-array  (name       string?)
		   (data-type  MAT5:numeric-type?)
		   (dims       MAT5:dimensions?) 
		   (real       (lambda (x) (array? x)))
		   (imag       (lambda (x) (or (array? x) (not x)))))
)

;
; The dimensions of a MAT5 object/struct/cell/array are defined as a
; list of positive integers, OR a list of positive integers where the
; last element is the symbol ??  The second format is used when the
; array data is represented by a stream of column vectors, and it is
; not known how many elements are in the stream ahead of time. 
;
(define (MAT5:dimensions? x)
  (let* ((rx   (reverse x))
	 (last (car rx)))
    (cond ((natural? last) (natural-list? (cdr rx)))
	  ((eq? '?? last)  (natural-list? (cdr rx)))
	  (else #f))))



;-----------------------
; Record pretty printers
;

(define-record-printer (array x out)
  (define m 5)
  (let* ((dims  (array-dimensions x))
	 (vdx  (last dims)))
    (letrec ((arpp  (lambda (dims idxs lst)
		      (cond ((null? (cdr dims))
			     (let ((v  (list-tabulate 
					(if (> m vdx) vdx m)  
					(lambda (i) 
					  (apply array-ref x (reverse (cons i idxs)))))))
			     (cons (if (< m vdx) (append v '(...)) v) lst)))
			    (else
			     (let ((dim  (car dims))
				   (lst1 '()))
			       (do ((idx 0 (+ 1 idx)))
				   ((or (>= idx dim) (>= idx m)) 
				    (if (>= idx m) 
					(cons (reverse (cons '... lst1)) lst)
					(cons (reverse lst1) lst)))
				 (set! lst1 (arpp (cdr dims) (cons idx idxs) lst1)))))))))

      (fprintf out "#(array ~S)" (reverse (arpp dims '() '())))
      )))
    
  

(define-record-printer (MAT5:header x out)
  (fprintf out "#(MAT5:header magic=0x~X text=~S subsys=~S version=~S)"
	   (MAT5:header-magic x)
	   (MAT5:header-text x) 
	   (MAT5:header-subsys x) 
	   (MAT5:header-version x)) )


(define-record-printer (MAT5:data-element x out)
  (fprintf out "#(MAT5:data-element type=~S bytes=~S data=~S)"
	   (MAT5:data-element-type x)
	   (MAT5:data-element-bytes x)
	   (MAT5:data-element-data x)) )


(define-record-printer (MAT5:data-type x out)
  (fprintf out "~A" (MAT5:data-type->string x)) )


(define-record-printer (MAT5:array-class x out)
  (fprintf out "~A" (MAT5:array-class->string x)) )


(define-record-printer (MAT5:array x out)
  (cases MAT5:array x
	 (MAT5:object (name dims class field-names fields)
		      (fprintf out "#(MAT5:object name=~S dims=~S class=~S field-names=~A fields=~S)"
			       name dims class field-names fields))

	 (MAT5:structure (name dims field-names fields)
			 (fprintf out "#(MAT5:structure name=~S dims=~S field-names=~A fields=~S)"
				  name dims field-names fields))
	 
	 (MAT5:cell-array (name dims cell)
			  (fprintf out "#(MAT5:cell name=~S dims=~S cell=~S)"
				   name dims cell))
	 
	 (MAT5:sparse-array (name data-type dims row-index col-index real imag) 
	  (fprintf out "#(MAT5:sparse-array name=~S data-type=~A dims=~S row-index=~S col-index=~S real=~S imag=~S)"
		   name data-type dims row-index col-index real imag))
	 
	 (MAT5:num-array (name data-type dims  real imag)
			 (fprintf out "#(MAT5:num-array name=~S data-type=~A dims=~S real=~S imag=~S)"
				  name data-type dims  real imag))
	 
	 (else           "#<MAT5:array>")))


(define-record-printer (MAT5:array-flags x out)
  (let ((flags (MAT5:array-flags-flags x))
	(class (MAT5:array-flags-class x))
	(nzmax (MAT5:array-flags-nzmax x)))
    (let ((sflags (filter (lambda(x) x)
			  (list (if (bit-vector-ref flags mxLOGICAL_FLAG) "mxLOGICAL_FLAG" #f)
				(if (bit-vector-ref flags mxGLOBAL_FLAG)  "mxGLOBAL_FLAG" #f)
				(if (bit-vector-ref flags mxCOMPLEX_FLAG) "mxCOMPLEX_FLAG" #f)))))
      (fprintf out "#(MAT5:array-flags flags=~A  class=~S  nzmax=~S)"
	       (string-intersperse sflags "|") class nzmax))))


(define-record-printer (MAT5:cell x out)
  (let ((dims (MAT5:cell-dims x))
	(data (MAT5:cell-data x)))
    (fprintf out "#(MAT5:cell dims=~A  data=~S)" dims data)))

;------------------------------------------------------------
; Predicates and routines for handling MAT-file datatypes and
; structures
;
;
  
; Procedure: 
; MAT5:complex-array?:: MAT5:ARRAY-FLAGS -> BOOLEAN
;
; Returns true if the complex flag is set in the given array flags 
;
(define (MAT5:complex-array? flags)
  (bit-vector-ref (MAT5:array-flags-flags flags)  mxCOMPLEX_FLAG))


; Procedure: 
; MAT5:sparse-class?:: MAT5:ARRAY-FLAGS -> BOOLEAN
;
; Returns true if the class field in the given array flags is sparse
;
(define (MAT5:sparse-class? flags)
  (cases MAT5:array-class (MAT5:array-flags-class flags)
	 (mxSPARSE_CLASS () #t)
	 (else              #f)))


; Procedure: 
; MAT5:cell-class?:: MAT5:ARRAY-FLAGS -> BOOLEAN
;
; Returns true if the class field in the given array flags is cell
;
(define (MAT5:cell-class? flags)
  (cases MAT5:array-class (MAT5:array-flags-class flags)
	 (mxCELL_CLASS () #t)
	 (else              #f)))


; Procedure: 
; MAT5:structure-class?:: MAT5:ARRAY-FLAGS -> BOOLEAN
;
; Returns true if the class field in the given array flags is structure
;
(define (MAT5:structure-class? flags)
  (cases MAT5:array-class (MAT5:array-flags-class flags)
	 (mxSTRUCT_CLASS () #t)
	 (else              #f)))


; Procedure: 
; MAT5:object-class?:: MAT5:ARRAY-FLAGS -> BOOLEAN
;
; Returns true if the class field in the given array flags is object
;
(define (MAT5:object-class? flags)
  (cases MAT5:array-class (MAT5:array-flags-class flags)
	 (mxOBJECT_CLASS () #t)
	 (else              #f)))


; Procedure: 
; MAT5:numeric-type?:: MAT5:DATA-TYPE -> BOOLEAN
;
; Return true if type is an atomic numeric type
;
(define (MAT5:numeric-type? t)
  (cases MAT5:data-type t
	 (miINT8   ()     #t)
	 (miUINT8  ()     #t)
	 (miINT16  ()     #t)
	 (miUINT16 ()     #t)
	 (miINT32  ()     #t)
	 (miUINT32 ()     #t)
	 (miSINGLE ()     #t)
	 (miDOUBLE ()     #t)
	 (miINT64  ()     #t)
	 (miUINT64 ()     #t)
	 (miUTF8   ()     #t)
	 (miUTF16  ()     #t)
	 (miUTF32  ()     #t)
	 (else            #f)))


; Procedure: 
; MAT5:array-type?:: MAT5:DATA-TYPE -> BOOLEAN
;
; Return true if type is array type (miMATRIX)
;
(define (MAT5:array-type? t)
  (cases MAT5:data-type t
	 (miMATRIX  ()     #t)
	 (else            #f)))


; Procedure: 
; MAT5:sizeof:: MAT5:ARRAY-FLAGS -> UINTEGER
;
; Returns the size in bytes of the atomic datatype t
;
; t must not be miMATRIX or miCOMPRESSED
;
(define (MAT5:sizeof t)
  (cases MAT5:data-type t
	 (miINT8   ()     1)
	 (miUINT8  ()     1)
	 (miINT16  ()     2)
	 (miUINT16 ()     2)
	 (miINT32  ()     4)
	 (miUINT32 ()     4)
	 (miSINGLE ()     4)
	 (miDOUBLE ()     8)
	 (miINT64  ()     8)
	 (miUINT64 ()     8)
	 (miUTF8   ()     1)
	 (miUTF16  ()     2)
	 (miUTF32  ()     4)
	 (else            #f)))


; Procedure: 
; MAT5:data-type->string:: MAT5:DATA-TYPE -> STRING
;
; Returns the string identifier for the given datatype
;
(define (MAT5:data-type->string t)
  (cases MAT5:data-type t
	 (miINT8   ()     "miINT8")
	 (miUINT8  ()     "miUINT8")
	 (miINT16  ()     "miINT16")
	 (miUINT16 ()     "miUINT16")
	 (miINT32  ()     "miINT32")
	 (miUINT32 ()     "miUINT32")
	 (miSINGLE ()     "miSINGLE")
	 ;;      RESERVED     #x00000008
	 (miDOUBLE ()     "miDOUBLE")
	 ;;      RESERVED     #x0000000A
	 ;;      RESERVED     #x0000000B
	 (miINT64  ()     "miINT64")
	 (miUINT64 ()     "miUINT64")
	 (miMATRIX ()     "miMATRIX")
	 (miCOMPRESSED () "miCOMPRESSED")
	 (miUTF8   ()     "miUTF8")
	 (miUTF16  ()     "miUTF16")
	 (miUTF32  ()     "miUTF32")
	 (else            #f)))


; Procedure: 
; MAT5:data-type->array-class:: MAT5:DATA-TYPE -> MAT5:ARRAY-FLAGS
;
; Return the numeric array class corresponding to the given datatype
;
(define (MAT5:data-type->array-class t)
  (cases MAT5:data-type t
	 (miINT8   ()     (mxINT8_CLASS))
	 (miUINT8  ()     (mxUINT8_CLASS))
	 (miINT16  ()     (mxINT16_CLASS))
	 (miUINT16 ()     (mxUINT16_CLASS))
	 (miINT32  ()     (mxINT32_CLASS))
	 (miUINT32 ()     (mxUINT32_CLASS))
	 (miSINGLE ()     (mxSINGLE_CLASS))
	 ;;      RESERVED     #x00000008
	 (miDOUBLE ()     (mxDOUBLE_CLASS))
	 ;;      RESERVED     #x0000000A
	 ;;      RESERVED     #x0000000B
	 (else            #f)))


; Procedure: 
; MAT5:data-type->word:: MAT5:DATA-TYPE -> UINTEGER
;
; Return the numeric code for the given datatype
;
(define (MAT5:data-type->word t)
  (cases MAT5:data-type t
	 (miINT8   ()     #x00000001)
	 (miUINT8  ()     #x00000002)
	 (miINT16  ()     #x00000003)
	 (miUINT16 ()     #x00000004)
	 (miINT32  ()     #x00000005)
	 (miUINT32 ()     #x00000006)
	 (miSINGLE ()     #x00000007)
	 ;;      RESERVED     #x00000008
	 (miDOUBLE ()     #x00000009)
	 ;;      RESERVED     #x0000000A
	 ;;      RESERVED     #x0000000B
	 (miINT64  ()     #x0000000C)
	 (miUINT64 ()     #x0000000D)
	 (miMATRIX ()     #x0000000E)
	 (miCOMPRESSED () #x0000000F)
	 (miUTF8   ()     #x00000010)
	 (miUTF16  ()     #x00000011)
	 (miUTF32  ()     #x00000012)
	 (else            #f)))


; Procedure: 
; MAT5:word->data-type:: UINTEGER -> MAT5:DATA-TYPE
; 
; Return the datatype corresponding to the given numeric value, or #f
;
(define (MAT5:word->data-type x)
  (cond ((= x #x00000001)   (miINT8))
	((= x #x00000002)   (miUINT8))
	((= x #x00000003)   (miINT16))
	((= x #x00000004)   (miUINT16))
	((= x #x00000005)   (miINT32))
	((= x #x00000006)   (miUINT32))
	((= x #x00000007)   (miSINGLE))
	((= x #x00000009)   (miDOUBLE))
	((= x #x0000000C)   (miINT64))
	((= x #x0000000D)   (miUINT64))
	((= x #x0000000E)   (miMATRIX))
	((= x #x0000000F)   (miCOMPRESSED))
	((= x #x00000010)   (miUTF8))
	((= x #x00000011)   (miUTF16))
	((= x #x00000012)   (miUTF32))
	(else               #f)))

	 
; Procedure:
; MAT5:array-class->string:: MAT5:ARRAY-CLASS -> STRING
;
; Return the numeric code corresponding to the given array class
;
(define (MAT5:array-class->string x)
  (cases MAT5:array-class x
	 (mxCELL_CLASS   ()    "mxCELL_CLASS")
	 (mxSTRUCT_CLASS ()    "mxSTRUCT_CLASS")
	 (mxOBJECT_CLASS ()    "mxOBJECT_CLASS")
	 (mxCHAR_CLASS   ()    "mxCHAR_CLASS")
	 (mxSPARSE_CLASS ()    "mxSPARSE_CLASS")
	 (mxDOUBLE_CLASS ()    "mxDOUBLE_CLASS")
	 (mxSINGLE_CLASS ()    "mxSINGLE_CLASS")
	 (mxINT8_CLASS   ()    "mxINT8_CLASS")
	 (mxUINT8_CLASS  ()    "mxUINT8_CLASS")
	 (mxINT16_CLASS  ()    "mxINT16_CLASS")
	 (mxUINT16_CLASS ()    "mxUINT16_CLASS")
	 (mxINT32_CLASS  ()    "mxINT32_CLASS")
	 (mxUINT32_CLASS ()    "mxUINT32_CLASS")
	 (else                 #f)))

	 
; Procedure: 
; MAT5:array-class->wordMAT5:ARRAY-CLASS -> UINTEGER
;
; Returns the numeric code corresponding to the given array class
;
(define (MAT5:array-class->word x)
  (cases MAT5:array-class x
	 (mxCELL_CLASS   ()    #x01)
	 (mxSTRUCT_CLASS ()    #x02)
	 (mxOBJECT_CLASS ()    #x03)
	 (mxCHAR_CLASS   ()    #x04)
	 (mxSPARSE_CLASS ()    #x05)
	 (mxDOUBLE_CLASS ()    #x06)
	 (mxSINGLE_CLASS ()    #x07)
	 (mxINT8_CLASS   ()    #x08)
	 (mxUINT8_CLASS  ()    #x09)
	 (mxINT16_CLASS  ()    #x0A)
	 (mxUINT16_CLASS ()    #x0B)
	 (mxINT32_CLASS  ()    #x0C)
	 (mxUINT32_CLASS ()    #x0D)
	 (else                 #f)))


; Procedure: 
; MAT5:word->array-class:: UINTEGER -> MAT5:ARRAY-CLASS
;
; 
; Return the array class corresponding to the given numeric code, or
; #f
;
(define (MAT5:word->array-class x)
  (cond  ((= x #x01)   (mxCELL_CLASS))
	 ((= x #x02)   (mxSTRUCT_CLASS))
	 ((= x #x03)   (mxOBJECT_CLASS))
	 ((= x #x04)   (mxCHAR_CLASS))
	 ((= x #x05)   (mxSPARSE_CLASS))
	 ((= x #x06)   (mxDOUBLE_CLASS))
	 ((= x #x07)   (mxSINGLE_CLASS))
	 ((= x #x08)   (mxINT8_CLASS))
	 ((= x #x09)   (mxUINT8_CLASS))
	 ((= x #x0A)   (mxINT16_CLASS))
	 ((= x #x0B)   (mxUINT16_CLASS))
	 ((= x #x0C)   (mxINT32_CLASS))
	 ((= x #x0D)   (mxUINT32_CLASS))
	 (else         #f)))


; Procedure:
; MAT5:array-vector-ops:: MAT5:DATA-TYPE -> PROTO * VECTOR-SET * VECTOR-REF * VECTOR-LEN * VECTOR?
;
; Returns a set of routines used for the creation and manipulation of
; homogenous numeric vectors and arrays. Used by the read-array-
; functions below.
;
(define (MAT5:array-vector-ops data-type)
  (cases MAT5:data-type data-type
	 (miINT8   ()       (values A:fixZ8b   s8vector-set!  s8vector-ref  s8vector-length  s8vector?))
	 (miUINT8  ()       (values A:fixN8b   u8vector-set!  u8vector-ref  u8vector-length  u8vector?))
	 (miINT16  ()       (values A:fixZ16b  s16vector-set! s16vector-ref s16vector-length s16vector?))
	 (miUINT16 ()       (values A:fixN16b  u16vector-set! u16vector-ref u16vector-length u16vector?))
	 (miINT32  ()       (values A:fixZ32b  s32vector-set! s32vector-ref s32vector-length s32vector?))
	 (miUINT32 ()       (values A:fixN32b  u32vector-set! u32vector-ref u32vector-length u32vector?))
;;	 (miINT64  ()       (values A:fixZ64b  s64vector-set! s64vector-ref s64vector-length))
;;	 (miUINT64 ()       (values A:fixN64b  u64vector-set! u64vector-ref u64vector-length))
	 (miUTF8   ()       (values A:fixN8b   u8vector-ref   u8vector-ref  u8vector-length  u8vector?))
	 (miUTF16  ()       (values A:fixN16b  u16vector-set! u16vector-ref u16vector-length u16vector?))
	 (miUTF32  ()       (values A:fixN32b  u32vector-set! u32vector-ref u32vector-length u32vector?))
	 (miSINGLE ()       (values A:floR32b  f32vector-set! f32vector-ref f32vector-length f32vector?))
	 (miDOUBLE ()       (values A:floR64b  f64vector-set! f64vector-ref f64vector-length f64vector?))
	 (miMATRIX ()       (MAT5:error "nested arrays not permitted in numeric arrays"))
	 (else              (MAT5:error "unrecognized type " data-type))))


; Procedure:
; MAT5:array-vector-length:: MAT5:DATA-TYPE * UINTEGER -> UINTEGER
;
; Returns the length of the vector that would be necessary to hold all
; values for an array of the specified size and type.
;
;   * data-type   array/vector type
;   * data-size   array/vector size in bytes
;
(define (MAT5:array-vector-length data-type data-size)
  (cases MAT5:data-type data-type
	 (miINT8   ()        data-size)
	 (miUINT8  ()        data-size)
	 (miINT16  ()        (/ data-size (MAT5:sizeof data-type)))
	 (miUINT16 ()        (/ data-size (MAT5:sizeof data-type)))
	 (miINT32  ()        (/ data-size (MAT5:sizeof data-type)))
	 (miUINT32 ()        (/ data-size (MAT5:sizeof data-type)))
;;	 (miINT64  ()        (/ data-size (MAT5:sizeof data-type)))
;;	 (miUINT64 ()        (/ data-size (MAT5:sizeof data-type)))
	 (miUTF8   ()        data-size)
	 (miUTF16  ()        (/ data-size (MAT5:sizeof data-type)))
	 (miUTF32  ()        (/ data-size (MAT5:sizeof data-type)))
	 (miSINGLE ()        (/ data-size (MAT5:sizeof data-type)))
	 (miDOUBLE ()        (/ data-size (MAT5:sizeof data-type)))
	 (miMATRIX ()       (MAT5:error "nested arrays not permitted in numeric arrays"))
	 (else              (MAT5:error "unrecognized type " data-type))))
  

; Procedure:
; MAT5:array-vector-make:: MAT5:DATA-TYPE * UINTEGER -> VECTOR
;
; Returns a vector used to hold the values for an array of the
; specified size and type.  Used by the read-array- functions below.
;
;   * data-type   array/vector type
;   * data-size   array/vector size in bytes
;
(define (MAT5:array-vector-make data-type data-size)
  (let ((len (MAT5:array-vector-length data-type data-size)))
    (cases MAT5:data-type data-type
	 (miINT8   ()        (make-s8vector   len))
	 (miUINT8  ()        (make-u8vector   len))
	 (miINT16  ()        (make-s16vector  len))
	 (miUINT16 ()        (make-u16vector  len))
	 (miINT32  ()        (make-s32vector  len))
	 (miUINT32 ()        (make-u32vector  len))
;;	 (miINT64  ()        (make-s64vector  len))
;;	 (miUINT64 ()        (make-u64vector  len))
	 (miUTF8   ()        (make-u8vector   len))
	 (miUTF16  ()        (make-u16vector  len))
	 (miUTF32  ()        (make-u32vector  len))
	 (miSINGLE ()        (make-f32vector  len))
	 (miDOUBLE ()        (make-f64vector  len))
	 (miMATRIX ()       (MAT5:error "nested arrays not permitted in numeric arrays"))
	 (else              (MAT5:error "unrecognized type " data-type)))))
  

; Procedure:
; MAT5:vector-make:: MAT5:DATA-TYPE * UINTEGER -> VECTOR
;
; Returns a vector used to hold the values of the specified type.
;
;   * data-type   vector type
;   * len         vector length
;
(define (MAT5:vector-make data-type len)
    (cases MAT5:data-type data-type
	 (miINT8   ()        (make-s8vector   len))
	 (miUINT8  ()        (make-u8vector   len))
	 (miINT16  ()        (make-s16vector  len))
	 (miUINT16 ()        (make-u16vector  len))
	 (miINT32  ()        (make-s32vector  len))
	 (miUINT32 ()        (make-u32vector  len))
;;	 (miINT64  ()        (make-s64vector  len))
;;	 (miUINT64 ()        (make-u64vector  len))
	 (miUTF8   ()        (make-u8vector   len))
	 (miUTF16  ()        (make-u16vector  len))
	 (miUTF32  ()        (make-u32vector  len))
	 (miSINGLE ()        (make-f32vector  len))
	 (miDOUBLE ()        (make-f64vector  len))
	 (miMATRIX ()       (MAT5:error "nested arrays not permitted in numeric arrays"))
	 (else              (MAT5:error "unrecognized type " data-type))))
  

;--------------------
;  MAT5 cell routines
;
;  A MAT5 cell is represented as an R5RS non-homogenous array that
;  contains elements of type MAT5:array
;

;  Structure: MAT5:cell
;
;  A representation of MAT5 cells.
;
;  * dims is the cell dimensions (see MAT5:dimensions?)  
;  * data is an R5RS non-homogenous array that contains 
;    elements of type MAT5:array.
(define-record MAT5:cell  dims  data)


; Procedure:
; init-MAT5:cell:: UINTEGER * ... -> MAT5:CELL
;
; Create a new MAT5:cell object with the specified dimensions. The
; dimensions must all be positive integers, and at least one dimension
; must be specified.
;
(define (init-MAT5:cell d . dimensions)
  (let ((dims (cons d dimensions)))
    (if (not (natural-list? dims))   (MAT5:error "invalid cell dimensions")
	(let ((ar  (make-vector (car dims))))
	  (init-cell1 ar (cdr dims))
	  (make-MAT5:cell dims ar)))
    ))


; helper function for init-MAT5:cell above
(define (init-cell1 v dims)
  (if (not (null? dims))
      (let loop ((i 0) (len (vector-length v)))
	(if (not (zero? len))
	    (let ((v1  (make-vector (car dims))))
	      (vector-set! v i v1)
	      (init-cell1 v1 (cdr dims))
	      (loop (+ i 1) (- len 1)))
	    ))
      ))


; Procedure:
; MAT5:cell-ref:: MAT5:CELL * UINTEGER ... -> VALUE
;
; Given a MAT5:cell object and an index, returns the value found at
; that index in the cell. The index must be a list of positive
; integers, and it must be within the bounds of the cell dimensions.
;
(define (MAT5:cell-ref x i . rest)
  (let ((ar  (MAT5:cell-data x))
	(il  (cons i rest)))
    (if (not (positive-or-zero-list? il))
	(MAT5:error "invalid cell dimensions")
	(MAT5:cell-ref1 ar il))
    ))

  
;; helper function for MAT5:cell-ref above
;;  i is a list of dimensions
(define (MAT5:cell-ref1 x i)
  (cond ((vector? x)  (if (null? i) (MAT5:error "cell dimension mismatch")
			  (MAT5:cell-ref1 (vector-ref x (car i)) (cdr i))))
	(else    (if (null? i) x   (MAT5:error "cell dimension mismatch")))))


; Procedure:
; MAT5:cell-set!:: MAT5:CELL * VALUE * UINTEGER ... -> UNDEFINED
;
; Given a MAT5:cell object and an index, destructively replaces the
; element at the given index of the cell with the given value.
;
(define (MAT5:cell-set! x  v  i . rest)
  (let ((ar  (MAT5:cell-data x))
	(il  (cons i rest)))
    (if (not (positive-or-zero-list? il))
	(MAT5:error "invalid cell dimensions")
	(MAT5:cell-set1 ar v il))
    ))
  

; helper function for MAT5:cell-set! above
(define (MAT5:cell-set1 x v i)
  (cond ((null? i)        (MAT5:error "invalid cell dimensions"))
	((null? (cdr i))  (if (and (vector? x) (not (vector? (vector-ref x (car i)))))
			      (vector-set! x (car i) v)
			      (MAT5:error "cell dimension mismatch")))
	(else             (if (vector? x)
			      (MAT5:cell-set1 (vector-ref x (car i)) v (cdr i))
			      (MAT5:error "cell dimension mismatch")))
	))

;---------------------------------------
;  MAT5 vector/array conversion routines
;


; Procedure: 
; vector->array:: VECTOR-OPS VECTOR ARRAY-PROTOTYPE DIMENSIONS -> ARRAY
;
; Based on vector->array from SRFI-63 reference implementation
; Copyright (C) 2001, 2003, 2005 Aubrey Jaffer
;
(define (vector->array vops vect prototype dimensions . rest)
  (let-optionals  rest ((order 'row-major))
   (let* ((vector-length (alist-ref 'vector-length vops))
	  (vector-ref (alist-ref 'vector-ref vops))
	  (vdx (vector-length vect)))
     (if (not (eqv? vdx (apply * dimensions))) (MAT5:error "incompatible dimensions"))
     (letrec ((ra (apply make-array prototype dimensions))
	      (v2ra  (lambda (dims idxs)
		       (cond ((null? dims)
			      (set! vdx (+ -1 vdx))
			      (apply array-set! ra (vector-ref vect vdx) (reverse idxs)))
			     (else
			      (do ((idx (+ -1 (car dims)) (+ -1 idx)))
				  ((negative? idx) vect)
				(v2ra (cdr dims) (cons idx idxs)))))))
	      (v2ca   (lambda (dims idxs)
			(cond ((null? dims)
			       (set! vdx (+ -1 vdx))
			       (apply array-set! ra (vector-ref vect vdx) idxs))
			      (else
			       (do ((idx (+ -1 (car dims)) (+ -1 idx)))
				   ((negative? idx) vect)
				 (v2ca (cdr dims) (cons idx idxs))))))))
       (if (eq? order 'row-major)
	   (v2ra dimensions '())
	   (v2ca (reverse dimensions) '()))
	 ra))))


; Procedure:
; MAT5:array->srfi-4-vector:: ARRAY * MAKE-VECTOR * VECTOR-SET! [* ORDER] -> SRFI-4-VECTOR

(define (MAT5:array->srfi-4-vector ar make-vector vector-set! . rest)

  (let-optionals  rest ((order 'row-major))

   (let* ((dims (array-dimensions ar))
	  (vdx  (apply * dims))
	  (rv   (make-vector vdx)))

     (letrec (
	      (ra2v  (lambda (dims idxs)
		       (cond ((null? dims)
			      (let ((val (apply array-ref ar (reverse idxs))))
				(set! vdx (+ -1 vdx))
				(vector-set! rv vdx val)))
			     (else
			      (do ((idx (+ -1 (car dims)) (+ -1 idx)))
				  ((negative? idx) rv)
				(ra2v (cdr dims) (cons idx idxs))))
			     )))

	      (ca2v   (lambda (dims idxs)
			(cond ((null? dims)
			       (let ((val (apply array-ref ar idxs)))
				 (set! vdx (+ -1 vdx))
				 (vector-set! rv vdx val)))
			      (else
			       (do ((idx (+ -1 (car dims)) (+ -1 idx)))
				   ((negative? idx) rv)
				 (ca2v (cdr dims) (cons idx idxs)))))))
	      )

       (if (eq? order 'row-major)
	   (ra2v dims '())
	   (ca2v (reverse dims) '()))

	 rv))))


; Procedure:
; vector->MAT5:cell:: VECTOR * UINTEGER LIST [* ORDER] -> MAT5:CELL
;
; Given a vector, creates a new MAT5:cell object that consists of the
; elements of the vector. The vector must be of length equal to the
; total size of the array, and its elements are used to initialize the
; cell in either row-major order (left to right and top to bottom), or
; in column-major order (top to bottom and then left to right).
;
; The optional argument ORDER specifies the initialization order and
; can be either 'row-major or 'col-major. The default is 'row-major.
;
; This is based on vector->array from SRFI-63 reference implementation
; Copyright (C) 2001, 2003, 2005 Aubrey Jaffer
(define (vector->MAT5:cell vect dimensions . rest)
  (let-optionals  rest ((order 'row-major))
   (let* ((vdx (vector-length vect)))
     (if (not (eqv? vdx (apply * dimensions))) (MAT5:error "incompatible dimensions"))
     (letrec ((ra (apply init-MAT5:cell  dimensions))
	      (v2ra  (lambda (dims idxs)
		       (cond ((null? dims)
			      (set! vdx (+ -1 vdx))
			      (apply MAT5:cell-set! ra (vector-ref vect vdx) (reverse idxs)))
			     (else
			      (do ((idx (+ -1 (car dims)) (+ -1 idx)))
				  ((negative? idx) vect)
				(v2ra (cdr dims) (cons idx idxs)))))))
	      (v2ca   (lambda (dims idxs)
			(cond ((null? dims)
			       (set! vdx (+ -1 vdx))
			       (apply MAT5:cell-set! ra (vector-ref vect vdx) idxs))
			      (else
			       (do ((idx (+ -1 (car dims)) (+ -1 idx)))
				   ((negative? idx) vect)
				 (v2ca (cdr dims) (cons idx idxs))))))))
       (if (eq? order 'row-major)
		(v2ra dimensions '())
		(v2ca (reverse dimensions) '()))
       ra))
   ))


; Procedure:
; MAT5:array-foldi:: (INDEX * VALUE * AX -> AX) [* ORDER] -> MAT5:ARRAY * AX -> AX
;
; Iterator function for non-homogenous MAT5:array objects; that is,
; objects that are either MAT5:cell-array (MAT5 cell) or
; MAT5:structure (MAT5 structure). 
; 
; Analogous to the list iterator, this procedure repeatedly applies
; the given function to each element of the array, and accumulates the
; return value. The order of iteration is specified by the optional
; argument ORDER, which can be 'row-major (left to right and top to
; bottom) or 'col-major (top to bottom and then left to right). The
; default is 'row-major.
;
(define (MAT5:array-foldi  f . rest)
  (let-optionals  rest ((order 'row-major))
   (lambda (x ax)
     (let-values (((dims vdx arr elm-ref)  
		   (cases MAT5:array x
			  (MAT5:cell-array (name dims cell) 
					   (let ((vdx (apply * dims)))
					     (values dims vdx cell MAT5:cell-ref)))
			  (MAT5:structure (name dims field-names fields) 
					  (let* ((flen  (length field-names))
						 (vdx   (* (apply * dims) flen))) 
					    (values dims vdx fields MAT5:cell-ref)))
			  (MAT5:object (name dims class-name field-names fields)
				       (let* ((flen  (length field-names))
					      (vdx   (* (apply * dims) flen))) 
					 (values dims vdx fields MAT5:cell-ref)))
			  (else   (MAT5:error "invalid MAT5 array")))))
		 (letrec ((ra2v  (lambda (dims idxs ax)
				   (cond ((null? dims)
					  (set! vdx (+ -1 vdx))
					  (f vdx  (apply elm-ref arr (reverse idxs)) ax))
					 (else
					  (let ((dim (car dims)))
					    (do ((idx 0 (+ 1 idx)))
						((>= idx dim) ax)
					      (set! ax (ra2v (cdr dims) (cons idx idxs) ax))))))))
			  (ca2v   (lambda (dims idxs ax)
				    (cond ((null? dims)
					   (set! vdx (+ -1 vdx))
					   (f vdx  (apply elm-ref arr  idxs) ax))
					  (else
					   (let ((dim (car dims)))
					     (do ((idx 0 (+ 1 idx)))
						 ((>= idx dim) ax)
					     (set! ax (ca2v (cdr dims) (cons idx idxs) ax)))))))))
		   (if (eq? order 'row-major)
		       (ra2v dims '() ax)
		       (ca2v (reverse dims) '() ax)))))
   ))


; Procedure:
; array-foldi
;
(define (array-foldi  f . rest)
  (let-optionals  rest ((order 'row-major))
   (lambda (arr ax)
     (if (not (array? arr)) (MAT5:error "invalid array"))
     (let ((dims (array-dimensions arr)))
       (letrec ((vdx   (apply * dims))
		(ra2v  (lambda (dims idxs ax)
			 (cond ((null? dims)
				(set! vdx (+ -1 vdx))
				(f vdx  (apply array-ref arr (reverse idxs)) ax))
			       (else
				(let ((dim (car dims)))
				  (do ((idx 0 (+ 1 idx)))
				      ((>= idx dim) ax)
				    (set! ax (ra2v (cdr dims) (cons idx idxs) ax))))))))
		(ca2v   (lambda (dims idxs ax)
			  (cond ((null? dims)
				 (set! vdx (+ -1 vdx))
				 (f vdx  (apply array-ref arr idxs) ax))
				(else
				 (let ((dim (car dims)))
				   (do ((idx 0 (+ 1 idx)))
				       ((>= idx dim) ax)
				     (set! ax (ca2v (cdr dims) (cons idx idxs) ax)))))))))
	 (if (eq? order 'row-major)
	     (ra2v dims '() ax)
	     (ca2v (reverse dims) '() ax)))))
   ))


(define (update-array-name name x)
    (cases MAT5:array x
	 (MAT5:object
	  (name1 dims class field-names fields)
	  (if (string=? name1 "")
	      (MAT5:object name dims class field-names fields) x))

	 (MAT5:structure 
	  (name1 dims field-names fields)
	  (if (string=? name1 "")
	      (MAT5:structure name dims field-names fields) x))

	 
	 (MAT5:cell-array
	  (name1 dims cell)
	  (if (string=? name1 "")
	      (MAT5:cell-array name dims cell) x))
			  
	 
	 (MAT5:sparse-array
	  (name1 data-type dims row-index col-index real imag) 
	  (if (string=? name1 "")
	      (MAT5:sparse-array name data-type dims row-index col-index real imag) x))
	 
	 (MAT5:num-array
	  (name1 data-type dims  real imag)
	  (if (string=? name1 "")
	      (MAT5:num-array name data-type dims real imag) x))
	 
	 (else           x)
	 ))


;--------------------------
; MAT-file Reading Routines
;

; Procedure:
; MAT5:read-header:: ENDIAN-PORT -> MAT5:HEADER
;
; Reads a MAT5 header from the given endian port. Returns a
; MAT5:header record.
;
(define (MAT5:read-header eport)
  (endian-port-setpos eport 0)
  (let* ((text     (endian-port-read-byte-vector  eport MAT5:header-text-size MSB))
	 (subsys   (endian-port-read-byte-vector  eport MAT5:header-subsys-size MSB))
	 (version  (endian-port-read-int2 eport MSB))
	 (magic    (endian-port-read-int2 eport MSB)))
    (cond ((= magic MAT5:lsb-magic) (endian-port-set-littlendian! eport))
	  ((= magic MAT5:msb-magic) (endian-port-set-bigendian! eport))
	  (else                     (MAT5:error "MAT-file magic number not found")))
    (if (not (or (= version MAT5:msb-version) (= version MAT5:lsb-version)))
	(MAT5:warning "unknown MAT-file version " version))
    (make-MAT5:header magic (byte-vector->zstring text) (byte-vector->zstring subsys) version eport)
    ))



; Procedure:
; read-data-element-header:: ENDIAN-PORT -> MAT5:DATA-TYPE * DATA-SIZE * BYTES
;
; Reads the header of a MAT5 data element.
;
; Returns (values data-type data-size bytes)
;
(define (read-data-element-header eport)
  (let ((type-word  (endian-port-read-int4 eport)))

    (MAT5:debug 2 "read-data-element-header: type-word = " 
		(if (number? type-word) 
		    (sprintf "0x~X" type-word) 
		    type-word))
    (MAT5:debug 2 "read-data-element-header: small element = " 
		(if (number? type-word) 
		    (not (zero? (bitwise-and #xFFFF0000 type-word)))
		    type-word))

    ;; Check for small data element format: when reading a MAT-file,
    ;; determine that we are processing a small data element by
    ;; comparing the value of the first two bytes of the tag with the
    ;; value zero. If these two bytes are not zero, the tag uses the
    ;; small data element format.
    (if (not type-word)
	(values #f #f #f)
	(if (zero? (bitwise-and #xFFFF0000 type-word))
	    (let ((data-type (MAT5:word->data-type type-word)))
	      (values   data-type
			(endian-port-read-int4 eport)
			8))
	    (let ((data-type (MAT5:word->data-type (bitwise-and #x0000FFFF type-word))))
	      (values  data-type
		       (arithmetic-shift (bitwise-and #xFFFF0000 type-word) -16)
		       4)))
	)))


; Procedure:
; read-data-element:: ENDIAN-PORT * DATA-TYPE * SIZE -> MAT5:DATA-ELEMENT
;
; Given an eport and MAT5 data type, read a word of that type from
; the eport. Returns a record of type MAT5:data-element. 
;
; This function is parameterized over the routines for reading numeric
; array data. See function read-array for understanding of the two
; interfaces.
;
(define (read-data-element read-sparse-array-data 
			   read-num-array-data)
  (let ((read-array (read-array read-sparse-array-data 
				read-num-array-data)))
    (lambda (eport type size)
      (if (MAT5:numeric-type? type) 
	  (read-num-data-element eport type size)
	  (cases MAT5:data-type type
		 (miMATRIX ()     (let-values (((array bytes)  (read-array eport)))
					      (make-MAT5:data-element type bytes array)))
		 (miCOMPRESSED () (read-compressed-data-element eport size))
		 
		 (else (MAT5:error eport "unrecognized type " type)))
	  ))
    ))


; Given a compressed data element, uncompress it, create a temporary
; eport that points to the uncompressed data, then call
; MAT5:read-data-element with that eport
(define (read-compressed-data-element eport size)

  (let* ((zdata  (endian-port-read-byte-vector eport size MSB))
	 (dd (MAT5:debug 2 "read-compressed-data-element: length zdata = " 
			 (blob-size zdata)))
	 (dd (MAT5:debug 3 "read-compressed-data-element: zdata = " 
			 zdata))
	 (zstr   (z3:decode-buffer (substring (blob->string zdata) 2))))

    (MAT5:debug 2 "read-compressed-data-element: data decoded" )

    (let-values (((fd temp-path) (file-mkstemp "/tmp/mat5-lib.XXXXXX")))
      (file-write fd zstr) 

      (MAT5:debug 2 "read-compressed-data-element: decoded data written to temp file" )

      (let* ((temp-port (open-output-file* fd))
	     (ezport (port->endian-port temp-port)))

	  (if (eq? (endian-port-byte-order eport) MSB)
	      (endian-port-set-bigendian! ezport)
	      (endian-port-set-littlendian! ezport))
	  (endian-port-setpos ezport 0)
	  (let ((elms (MAT5:read-data-element ezport)))
	    (MAT5:debug 2 "read-compressed-data-element: elements read" )
	    (close-endian-port ezport)
	    (delete-file temp-path)
	    (cons size elms)
	    ))
	))
    )

; Procedure:
; read-num-data-element:: ENDIAN-PORT * MAT5:DATA-TYPE * SIZE -> MAT5:DATA-ELEMENT
;
; Reads an atomic numeric data element (i.e. one that is not a matrix).
;
(define (read-num-data-element eport type size)
  (cases MAT5:data-type type
	 (miINT8   ()     (make-MAT5:data-element type (MAT5:sizeof type)  (endian-port-read-int1 eport)))
	 (miUINT8  ()     (make-MAT5:data-element type (MAT5:sizeof type)  (endian-port-read-int1 eport)))
	 (miINT16  ()     (make-MAT5:data-element type (MAT5:sizeof type)  (endian-port-read-int2 eport)))
	 (miUINT16 ()     (make-MAT5:data-element type (MAT5:sizeof type)  (endian-port-read-int2 eport)))
	 (miINT32  ()     (make-MAT5:data-element type (MAT5:sizeof type)  (endian-port-read-int4 eport)))
	 (miUINT32 ()     (make-MAT5:data-element type (MAT5:sizeof type)  (endian-port-read-int4 eport)))
	 (miSINGLE ()     (let ((fpword (endian-port-read-ieee-float32 eport)))
				      (make-MAT5:data-element type (MAT5:sizeof type) fpword)))
	 (miDOUBLE ()     (let ((fpword (endian-port-read-ieee-float64 eport)))
			    (make-MAT5:data-element type (MAT5:sizeof type) fpword)))
	 ;;	 (miINT64  ()     (make-MAT5:data-element type (MAT5:sizeof type)  (endian-port-read-int8 eport)))
	 ;;	 (miUINT64 ()     (make-MAT5:data-element type (MAT5:sizeof type)  (endian-port-read-int8 eport)))
	 (miUTF8   ()     (make-MAT5:data-element type (MAT5:sizeof type)  (endian-port-read-int1 eport)))
	 (miUTF16  ()     (make-MAT5:data-element type (MAT5:sizeof type)  (endian-port-read-int2 eport)))
	 (miUTF32  ()     (make-MAT5:data-element type (MAT5:sizeof type)  (endian-port-read-int4 eport)))
	 (else            (MAT5:error eport "unrecognized type " type))))
	 


; Procedure:
; read-array-flags:: ENDIAN-PORT -> MAT5:ARRAY-FLAGS * BYTES
;
; Reads array flags data element from the given eport, and returns the
; array flags and how many bytes were read.
;
(define (read-array-flags eport)
  (let-values
   (((data-type data-size header-bytes) (read-data-element-header eport)))

   (if (not data-type) (MAT5:error eport "invalid data element type"))
   (cases MAT5:data-type data-type
	  (miUINT32 ()  
		    (let* ((flags-word  (endian-port-read-int4 eport))
			   (flags       (integer->bit-vector (arithmetic-shift 
							      (bitwise-and #x0000FF00 flags-word) -8)))
			   (class-word  (bitwise-and #x000000FF flags-word))
			   (class       (MAT5:word->array-class class-word))
			   (nzmax       (endian-port-read-int4 eport))
			   (pad-bytes   (align-eport-pos eport)))

		      (if (not class) (MAT5:error eport "invalid class: " class-word))
		      (values (make-MAT5:array-flags flags class nzmax) 
			      (+ header-bytes pad-bytes data-size))))
	  (else
	   (MAT5:error eport "array flags data element is not of type UINT32; type is "
		       data-type)))))
	 

; Procedure:
; read-array-dimensions:: ENDIAN-PORT -> UINTEGER LIST * BYTES
;
; Reads array dimensions data element from the given eport, and
; returns the dimensions as a list of positive integers, and how many
; bytes were read.
;
(define (read-array-dimensions eport)
  (let-values
   (((data-type data-size header-bytes) (read-data-element-header eport)))
   (if (not data-type) (MAT5:error eport "invalid data element type"))
   (let* ((count      (/ data-size (MAT5:sizeof data-type))))
     (cases MAT5:data-type data-type
	    (miINT32 ()  (if (> count 1)
			     (let loop ((i 1) (lst (list (endian-port-read-int4 eport))))
			       (if (< i count)
				   (loop (+ i 1) (cons (endian-port-read-int4 eport) lst))
				   (let ((pad-bytes (align-eport-pos eport)))
				     (values (reverse lst) (+ header-bytes pad-bytes data-size)))))
			     (MAT5:error eport count " array dimensions found; at least 2 array dimensions required")))
	    (else         (MAT5:error eport "array dimension data element is not of type INT32"))))))

	
; Procedure:
; read-array-name:: ENDIAN-PORT -> STRING * BYTES
; 
;
; Reads array name data element from the given eport, and returns the
; name as a string, and how many bytes were read.
;
(define (read-array-name eport)
  (let-values
   (((data-type data-size header-bytes) (read-data-element-header eport)))
   (if (not data-type) (MAT5:error eport "invalid data element type"))
   (cases MAT5:data-type data-type
	  (miINT8 ()   (let* ((bv  (endian-port-read-byte-vector eport data-size MSB))
			      (pad-bytes  (align-eport-pos eport)))
			 (values (byte-vector->zstring bv) (+ header-bytes pad-bytes data-size))))
	  (else        (MAT5:error eport "array name data element is not of type INT8")))))


; Procedure:
; read-row-index:: ENDIAN-PORT * MAT5:ARRAY-FLAGS -> UINTEGER LIST * BYTES
;
; Reads a sparse array row-index data element from the given
; eport. Argument array-flags is used to determine the number of
; non-zero rows.
;
(define (read-row-index eport array-flags)
  (let-values
   (((data-type data-size header-bytes) (read-data-element-header eport)))
   (if (not data-type) (MAT5:error eport "invalid data element type"))
   (let*  ((count      (/ data-size (MAT5:sizeof data-type)))
	   (nzmax      (MAT5:array-flags-nzmax array-flags)))
     (cases MAT5:data-type data-type
	    (miINT32 ()   (if (= nzmax count)
			      (let loop ((i 1) (lst (list (endian-port-read-int4 eport))))
				(if (< i count)
				    (loop (+ i 1) (cons (endian-port-read-int4 eport) lst))
				    (let ((pad-bytes  (align-eport-pos eport)))
				      (values (reverse lst) (+ header-bytes pad-bytes data-size)))))
			      (MAT5:error eport "mismatch between ir count and nzmax: ir count = " 
					  count " nzmax = " nzmax)))
	    (else          (MAT5:error eport "array row index data element is not of type INT32"))))))


; Procedure:
; read-col-index:: ENDIAN-PORT * MAT5:ARRAY-FLAGS -> UINTEGER LIST * BYTES
;
; Reads a sparse array column-index data element from the given
; eport. Argument array-flags is used to determine the number of
; non-zero rows.
;
(define (read-col-index eport array-dims)
  (let-values
   (((data-type data-size header-bytes) (read-data-element-header eport)))
   (if (not data-type) (MAT5:error eport "invalid data element type"))
   (let* ((count      (/ data-size (MAT5:sizeof data-type))))
     (cases MAT5:data-type data-type
	    (miINT32 ()   (if (= 1 (- count (second array-dims)))
			      (let loop ((i 1) (lst (list (endian-port-read-int4 eport))))
				(if (< i count)
				    (loop (+ i 1) (cons (endian-port-read-int4 eport) lst))
				    (let ((pad-bytes  (align-eport-pos eport)))
				      (values (reverse lst) (+ header-bytes pad-bytes data-size)))))
			      (MAT5:error eport "mismatch between jc count and second dimension: jc count = " count
					  " dimensions = " array-dims)))
	    (else (MAT5:error eport "array column index data element is not of type INT32"))))))


; Procedure:
; read-fieldname-len
;
;
; Read field name length (for structures and objects)
;
(define (read-fieldname-len eport)
  (let-values
   (((data-type data-size header-bytes)  (read-data-element-header eport)))
   (if (not data-type) (MAT5:error eport "invalid data element type"))
   (cases MAT5:data-type data-type
	  (miINT32 ()     (let* ((fieldname-len    (endian-port-read-int4 eport))
				 (pad-bytes (align-eport-pos eport)))
			    (values fieldname-len (+ header-bytes pad-bytes data-size))))
	  (else           (MAT5:error eport "field name length data element is not of type INT32")))))


; Procedure:
; read-field-names:: ENDIAN-PORT * UINTEGER -> STRING LIST * BYTES
;
; Reads field names (for structures and objects). Returns the field
; names as a list of strings, and how many bytes were read.
;
(define (read-field-names eport fieldname-len)
  (let-values
   (((data-type data-size header-bytes)  (read-data-element-header eport)))
   (if (not data-type) (MAT5:error eport "invalid data element type"))
   (cases MAT5:data-type data-type
	    (miINT8 ()      (let loop ((names '())   (len (/ data-size fieldname-len)))
			      (if (zero? len)
				  (let ((pad-bytes (align-eport-pos eport))
					(bv->string (lambda (x) (byte-vector->zstring x))))
				    (values (map bv->string (reverse names))  (+ header-bytes pad-bytes data-size)))
				  (let  ((name  (endian-port-read-byte-vector eport fieldname-len MSB)))
				    (loop (cons name names) (- len 1))))))
	    (else           (MAT5:error eport "field name data element is not of type INT8")))))

; Procedure:
; read-fields:: ENDIAN-PORT * STRING LIST -> VALUE LIST * BYTES
;
; Given an endian port, and a list of field names, reads the field
; values for a MAT5 structure. This function is parameterized over the
; routines for reading numeric array data. See function read-array for
; understanding of the two interfaces.

(define (read-fields read-sparse-array-data 
		     read-num-array-data)
  (lambda (eport field-names)
    (let ((read-data-element (read-data-element read-sparse-array-data 
						read-num-array-data)))
      (let loop ((fields '()) (field-names field-names) (bytes 0))
	(if (null? field-names)  (values fields bytes)
	    (let-values
	     (((data-type data-size header-bytes) (read-data-element-header eport)))
	     (if (not data-type) (MAT5:error eport "invalid data element type"))
	     (let* ((data-element   (read-data-element eport data-type data-size))
		    (data           (MAT5:data-element-data data-element))
		    (data-bytes     (MAT5:data-element-bytes data-element))
		    (field          (update-array-name (car field-names) data)))
	       (loop (cons field fields) (cdr field-names) 
		     (+ bytes header-bytes data-bytes)))
	     ))
	))
    ))


; Procedure:
; read-struct-data:: ENDIAN-PORT * MAT5:ARRAY-FLAGS * MAT5:DIMENSIONS * STRING LIST -> 
;                    MAT5:CELL * BYTES
;
; Reads the data for a MAT5 structure. The structure is represented
; as a MAT5:cell object, where each element is a list of field values. 
;
(define (read-struct-data read-sparse-array-data 
			  read-num-array-data)
  (let ((read-fields (read-fields read-sparse-array-data 
				  read-num-array-data)))
    (lambda (eport array-flags array-dims field-names)
      (let ((vector-data (make-vector (apply * array-dims))))
	(let loop ((i 0)  (len (apply * array-dims)) (bytes 0))
	  (if (zero? len)
	      (let ((pad-bytes  (align-eport-pos eport)))
		(values (vector->MAT5:cell vector-data array-dims 'col-major) 
			(+ pad-bytes bytes)))
	      (let-values (((fields fields-bytes)  (read-fields eport field-names)))
			  (vector-set! vector-data i (reverse fields))
			  (loop (+ i 1) (- len 1) (+ bytes fields-bytes)))
	      ))
	))
    ))


; Procedure:
; read-cell-data:: ENDIAN-PORT * MAT5:ARRAY-FLAGS * MAT5:DIMENSIONS -> 
;                  MAT5:CELL * BYTES
;
; Reads the data for a MAT5 cell.
;
(define (read-cell-data read-sparse-array-data 
			read-num-array-data)
  (let ((read-data-element (read-data-element read-sparse-array-data 
					      read-num-array-data)))
    (lambda (eport array-flags array-dims)
      (let ((vector-data (make-vector (apply * array-dims))))
	(let loop ((i 0)  (len (apply * array-dims)) (bytes 0))
	  (if (zero? len)
	      (let ((pad-bytes  (align-eport-pos eport)))
		(values (vector->MAT5:cell vector-data array-dims 'col-major) 
			(+ pad-bytes bytes)))
	      (let-values
	       (((data-type data-size header-bytes) (read-data-element-header eport)))
	       (if (not data-type) (MAT5:error eport "invalid data element type"))
	       (let ((data-element (read-data-element eport data-type data-size)))
		 (vector-set! vector-data i (MAT5:data-element-data data-element))
		 (loop (+ i 1)  (- len 1) (+ bytes header-bytes data-size))
		 ))
	      ))
	))
    ))


; Procedure: 
; read-sparse-array-data:: ENDIAN-PORT * MAT5:ARRAY-FLAGS * ROW-INDEX * COL-INDEX -> 
;                          VECTOR (of MAT5:data-element) * BYTES
;
;
; Reads the data for a sparse numeric array.
;
(define (read-sparse-array-data eport array-flags row-index col-index)
  (let-values
   (((data-type data-size header-bytes) (read-data-element-header eport)))
   (if (not data-type) (MAT5:error eport "invalid data element type"))
   (if (not (MAT5:numeric-type? data-type)) (MAT5:error eport "non-numeric sparse array data"))
   (if (not (= (MAT5:array-vector-length data-type data-size) 
	       (* (length row-index) (length col-index))))
       (MAT5:error eport "incompatible dimensions: data length is " 
		   (MAT5:array-vector-length data-type data-size)
		   " sparse array dimensions are specified as " 
		   row-index " " col-index))
   (let-values   
    (((prototype vector-set! vector-ref vector-length vector?)  
      (MAT5:array-vector-ops data-type)))
    (let ((vector-data (MAT5:array-vector-make data-type data-size)))
      (let loop ((i 0) (len (vector-length vector-data)))
	(if (zero? len)
	    (let ((vops     `((vector-ref . ,vector-ref)
			      (vector-length . ,vector-length)))
		  (pad-bytes  (align-eport-pos eport)))
	      (values data-type
		      (vector->array vops vector-data (prototype)  
				     (list (length row-index) (length col-index)) 'col-major) 
		      (+ pad-bytes header-bytes data-size)))
	    (let ((data-element (read-num-data-element eport data-type data-size)))
	      (vector-set! vector-data i (MAT5:data-element-data data-element))
	      (loop (+ i 1)  (- len 1)))
	    ))
      ))
   ))

; Procedure: 
; read-num-array-data:: ENDIAN-PORT * MAT5:ARRAY-FLAGS * MAT5:DIMENSIONS -> 
;                       ARRAY * BYTES
;
; Reads the data for a homogeneous numeric array. 
(define (read-num-array-data eport array-flags array-dims)
  (let-values

   (((data-type data-size header-bytes) (read-data-element-header eport)))

   (if (not data-type) (MAT5:error eport "invalid data element type"))
   (if (not (MAT5:numeric-type? data-type)) (MAT5:error eport "non-numeric array data"))
   (if (not (= (MAT5:array-vector-length data-type data-size) (apply * array-dims)))
       (MAT5:error eport "incompatible dimensions: data length is " 
		   (MAT5:array-vector-length data-type data-size)
		   " array dimensions are specified as " array-dims))
   (let-values   
    (((prototype vector-set! vector-ref vector-length vector?)  
      (MAT5:array-vector-ops data-type)))

    (let ((vector-data (MAT5:array-vector-make data-type data-size)))

      (MAT5:debug 3 "read-num-array-data: vector-data = " vector-data)

      (let loop ((i 0) (len (vector-length vector-data)))

	(if (zero? len)

	    (let* ((vops      `((vector-ref . ,vector-ref)
				(vector-length . ,vector-length)))
		   (pad-bytes  (align-eport-pos eport)))

	      (values  data-type
		       (vector->array vops vector-data (prototype) array-dims 'col-major)  
		       (+ pad-bytes header-bytes data-size)))

	    (let ((data-element (read-num-data-element eport data-type data-size)))
	      (vector-set! vector-data i (MAT5:data-element-data data-element))
	      (loop (+ i 1)  (- len 1)))
	    ))
      ))
   ))


; Procedure:
; read-array
;
(define (read-array read-sparse-array-data 
		    read-num-array-data)
  (lambda (eport)
    (let ((read-struct-data (read-struct-data read-sparse-array-data 
					      read-num-array-data))
	  (read-cell-data (read-cell-data read-sparse-array-data 
					  read-num-array-data)))
	  
    (let*-values 
     (((array-flags flags-bytes)  (read-array-flags eport))
      ((array-dims  dims-bytes)   (read-array-dimensions eport))
      ((array-name  name-bytes)   (read-array-name eport)))
     (cond  
      
      
      ;; read an object
      ((MAT5:object-class? array-flags)  
       (begin
	 (let*-values (((class-name      class-name-bytes)      (read-array-name eport))
		       ((fieldname-len   fieldname-len-bytes)   (read-fieldname-len eport))
		       ((field-names     field-names-bytes)     (read-field-names eport fieldname-len))
		       ((fields          fields-bytes)          (read-struct-data eport array-flags array-dims 
										  field-names)))
		      (values  (MAT5:object array-name array-dims class-name field-names fields)
			       (+ flags-bytes dims-bytes name-bytes class-name-bytes 
				  fieldname-len-bytes field-names-bytes fields-bytes)))))
      
      ;; read a structure
      ((MAT5:structure-class? array-flags)  
       (begin
	 (let*-values (((fieldname-len   fieldname-len-bytes)   (read-fieldname-len eport))
		       ((field-names     field-names-bytes)     (read-field-names eport fieldname-len))
		       ((fields          fields-bytes)          (read-struct-data eport array-flags array-dims
										  field-names)))
		      
		      (values  (MAT5:structure array-name array-dims field-names fields)
			       (+ flags-bytes dims-bytes name-bytes fieldname-len-bytes
				  field-names-bytes fields-bytes)))))
      
      ;; read a cell array
      ((MAT5:cell-class? array-flags)  
       (begin
	 (let*-values (((cell   cell-bytes)   
			(read-cell-data eport array-flags array-dims)))
		      (values  (MAT5:cell-array array-name array-dims cell)
			       (+ flags-bytes dims-bytes name-bytes cell-bytes)))))
      
      ;; read a sparse array
      ((MAT5:sparse-class? array-flags)  
       (begin
	 (if (not (= (length array-dims) 2)) 
	     (MAT5:error eport "read-array supports only two-dimensional sparse arrays"))
	 (let*-values (((row-index   row-bytes)    (read-row-index eport array-flags))
		       ((col-index   col-bytes)    (read-col-index eport array-dims))
		       ((data-type real-part   real-bytes)   
			(read-sparse-array-data eport array-flags row-index col-index))
		       ((dummy imag-part   imag-bytes)   
			(if (MAT5:complex-array? array-flags)
			    (read-sparse-array-data eport array-flags row-index col-index)
			    (values #f #f 0))))
		      (values  (MAT5:sparse-array
				array-name data-type array-dims row-index col-index real-part imag-part)
			       (+ flags-bytes dims-bytes name-bytes row-bytes col-bytes real-bytes imag-bytes)))
	 ))
      
      ;; read a homogeneous numeric array
      (else
       (let*-values (((data-type real-part real-bytes)   
		      (read-num-array-data eport array-flags array-dims))
		     ((dummy imag-part imag-bytes)   
		      (if (MAT5:complex-array? array-flags)
			  (read-num-array-data eport array-flags array-dims)  (values #f #f 0))))
	 (values  (MAT5:num-array array-name data-type array-dims real-part imag-part)
		  (+ flags-bytes dims-bytes name-bytes real-bytes imag-bytes)))
       ))
     ))
    ))

(define strict-read-data-element
  (read-data-element  read-sparse-array-data
		      read-num-array-data))


; Procedure:
; MAT5:read-data-element:: ENDIAN-PORT -> MAT5:DATA-ELEMENT
;
; Reads a MAT5 data element from the given endian port.
;
(define (MAT5:read-data-element eport)
  (let-values  (((data-type data-size header-bytes)  
		 (read-data-element-header eport)))
    (MAT5:debug 2 "MAT5:read-data-element: data-type = " data-type)
    (MAT5:debug 2 "MAT5:read-data-element: data-size = " data-size)
    (MAT5:debug 2 "MAT5:read-data-element: header-bytes = " header-bytes)
    (and data-type
	 (let ((data
		(let  loop  ((i 0) (lst '()))
		  (if (< i data-size)
		      (let ((word  (strict-read-data-element eport data-type data-size)))
			(if (pair? word) 
			    (loop (+ i (car word)) (append (cdr word) lst))
			    (loop (+ i (MAT5:data-element-bytes word)) (cons word lst))))
		      (if (= i data-size)
			  (reverse lst)
			  (MAT5:error eport "data element size mismatch: i = " i " data-size = " data-size))))))
	   data))
    ))

  
;--------------------------
;
; MAT-file Writing Routines
;

; Procedure: 
; write-pad-bytes:: ENDIAN-PORT ->  BYTES
;
; Writes pad bytes to a file, so that it is aligned on a 64-bit
; boundary. Returns the number of bytes written.
;
(define (write-pad-bytes  eport)
  (let* ((pos  (endian-port-pos eport))
	 (pos1  (let loop ((pos1 pos))
		   (if (not (zero? (modulo pos1 8))) 
		       (loop (+ pos1 1)) pos1))))
    (if (> pos1 pos)
	(let* ((dx   (- pos1 pos))
	       (bv   (make-blob dx)))
	  (endian-port-write-byte-vector eport bv)
	  dx)  0)))


; Procedure:
; MAT5:write-header:: ENDIAN-PORT * STRING * STRING -> UNDEFINED
;
; Writes a MAT5 header to the given endian port. Arguments TEXT and
; SUBSYS are strings. If they are longer than their maximum permitted
; lengths (116 and 8, respectively), they will be truncated.
;
(define (MAT5:write-header eport text subsys)
  (let* ((text     (let ((src   (cond ((string? text)  (string-pad-right text MAT5:header-text-size  #\nul))
				      (else     (MAT5:error eport "text argument is of invalid type: " text)))))
			 (string->blob src)))
	 (subsys   (let ((src   (cond ((string? subsys)  (string-pad-right subsys MAT5:header-subsys-size  #\nul))
				      (else     (MAT5:error eport "subsys argument is of invalid type: " subsys))))) 
			 (string->blob src)))
	 (magic    MAT5:msb-magic))
    (endian-port-setpos eport 0)
    (endian-port-write-byte-vector eport text MSB)
    (endian-port-write-byte-vector eport subsys MSB)
    (endian-port-write-int2 eport MAT5:msb-version MSB)
    (endian-port-write-int2 eport magic)
    (write-pad-bytes eport)))


; Procedure:
; write-num-data-element:: ENDIAN-PORT * MAT5:DATA-TYPE * VALUE -> BYTES
;
; Writes a data atom (a number or a char) to the given endian
; port. Returns the number of bytes written.
;
(define (write-num-data-element eport type data)
  (cases MAT5:data-type type
	 (miINT8   ()     (endian-port-write-int1 eport  data))
	 (miUINT8  ()     (endian-port-write-int1 eport  data))
	 (miINT16  ()     (endian-port-write-int2 eport  data))
	 (miUINT16 ()     (endian-port-write-int2 eport  data))
	 (miINT32  ()     (endian-port-write-int4 eport  data))
	 (miUINT32 ()     (endian-port-write-int4 eport  data))
	 (miSINGLE ()     (endian-port-write-ieee-float32 eport  data))
	 (miDOUBLE ()     (endian-port-write-ieee-float64 eport  data))
	 ;;	 (miINT64  ()     (endian-port-write-int8 eport  data) 8)
	 ;;	 (miUINT64 ()     (endian-port-write-int8 eport  data) 8)
	 (miUTF8   ()     (endian-port-write-int1 eport  data))
	 (miUTF16  ()     (endian-port-write-int2 eport  data))
	 (miUTF32  ()     (endian-port-write-int4 eport  data))
	 (else            (MAT5:error eport "unrecognized type " type))
	 ))

; Procedure:
; write-data-element:: ENDIAN-PORT * MAT5:DATA-TYPE * VALUE -> BYTES
;
; Writes a data atom or an array/cell/structure to the given endian
; port.
(define (write-data-element write-sparse-array-data
			    write-num-array-data)
  (let ((write-array (write-array write-sparse-array-data 
				  write-num-array-data)))
    (lambda (eport type data . rest)
      (let-optionals  rest ((include-header-bytes #f))
        (if (MAT5:numeric-type? type)
	    (write-num-data-element eport type data)
	    (cases MAT5:data-type type
		   (miMATRIX ()     (write-array eport  data include-header-bytes))
		   (else            (MAT5:error eport "unrecognized type " type))
		   ))
	))
    ))
  

; Procedure:
; write-data-element-header
;
;
(define (write-data-element-header eport data-type data-size . rest)
  (let-optionals  
   rest ((small #f))
   (let ((type-word (MAT5:data-type->word data-type)))
     (if (and small (<= data-size 4))
	 (let* ((bytes  (endian-port-write-int2 eport data-size))
		(bytes  (+ bytes (endian-port-write-int2 eport type-word))))
	   bytes)
	 (let* ((bytes  (endian-port-write-int4 eport type-word))
		(bytes  (+ bytes (endian-port-write-int4 eport data-size))))
	   bytes)))
   ))


; Procedure:
; write-array-flags
;
;
(define (write-array-flags eport flags class nzmax . rest)
  (let-optionals 
   rest  ((small #f))
   (let* ((data-type   (miUINT32))
	  (data-size   (* 2 (MAT5:sizeof data-type)))
	  (flags-word  (bitwise-ior  (if (bit-vector-ref flags mxLOGICAL_FLAG) #b00000010 0)
				     (if (bit-vector-ref flags mxGLOBAL_FLAG)  #b00000100 0)
				     (if (bit-vector-ref flags mxCOMPLEX_FLAG) #b00001000 0)))
	  (flags-word  (bitwise-ior (arithmetic-shift flags-word 8)
				    (MAT5:array-class->word class)))
	  (bytes       (write-data-element-header eport data-type  data-size small))
	  (bytes       (+ bytes (endian-port-write-int4 eport flags-word)))
	  (bytes       (+ bytes (endian-port-write-int4 eport nzmax)))
	  (bytes       (+ bytes (write-pad-bytes eport))))
     bytes)))
     
	 
; Procedure:
; write-array-dimensions
;
;
(define (write-array-dimensions eport dims . rest)
  (let-optionals rest  ((small #f))
   (if (null? dims) (MAT5:error "empty dimension list"))
   (if (not (MAT5:dimensions? dims)) (MAT5:error "invalid dimension list"))
   (let* ((data-type   (miINT32))
	  (data-size   (* (length dims) (MAT5:sizeof data-type)))
	  (bytes       (write-data-element-header eport data-type  data-size small))
	  (bytes       (+ bytes 
			  (let loop ((dims dims) (bytes 0))
			    (cond ((not (null? dims))
				   (let* ((dim    (car dims))
					  (x      (if (eq? dim '??) 0 dim))
					  (bytes  (+ bytes (endian-port-write-int4 eport x))))
				     (loop (cdr dims) bytes)))
				  (else bytes)))))
	  (bytes       (+ bytes (write-pad-bytes eport))))
     bytes)))


; Procedure:
; write-array-name
;
;
(define (write-array-name eport name . rest)
  (let-optionals 
   rest  ((small #f))
   (let* ((bv          (string->blob name))
	  (data-type   (miINT8))
	  (data-size   (* (blob-size bv) (MAT5:sizeof data-type)))
	  (bytes       (write-data-element-header eport data-type  data-size small))
	  (bytes       (+ bytes (endian-port-write-byte-vector eport bv MSB)))
	  (bytes       (+ bytes (write-pad-bytes eport))))
     bytes)))
   

; Procedure:
; write-row-index
;
;
(define (write-row-index eport row-index . rest)
  (let-optionals 
   rest  ((small #f))
   (if (null? row-index) (MAT5:error "empty row-index list"))
   (let* ((data-type   (miINT32))
	  (data-size   (* (length row-index) (MAT5:sizeof data-type)))
	  (bytes       (write-data-element-header eport data-type  data-size small))
	  (bytes       (+ bytes (let loop ((row-index row-index) (bytes 0))
				  (cond ((not (null? row-index))
					 (let ((bytes (+ bytes (endian-port-write-int4 eport (car row-index)))))
					   (loop (cdr row-index) bytes)))))))
	  (bytes       (+ bytes (write-pad-bytes eport))))
     bytes)))


; Procedure:
; write-col-index
;
;
(define (write-col-index eport col-index . rest)
  (let-optionals 
   rest  ((small #f))
   (if (null? col-index) (MAT5:error "empty col-index list"))
   (let* ((data-type   (miINT32))
	  (data-size   (* (length col-index) (MAT5:sizeof data-type)))
	  (bytes       (write-data-element-header eport data-type  data-size small))
	  (bytes       (+ bytes (let loop ((col-index col-index) (bytes 0))
				  (cond ((not (null? col-index))
					 (let ((bytes (+ bytes (endian-port-write-int4 eport (car col-index)))))
					   (loop (cdr col-index) bytes)))))))
	  (bytes       (+ bytes (write-pad-bytes eport))))
     bytes)))


; Procedure:
; write-fieldname-len
;
;
(define (write-fieldname-len eport len . rest)
  (let-optionals 
   rest  ((small #f))
   (let* ((data-type   (miINT32))
	  (data-size   (MAT5:sizeof data-type))
	  (bytes       (write-data-element-header eport data-type  data-size small))
	  (bytes       (+ bytes (endian-port-write-int4 eport len)))
	  (bytes       (+ bytes (write-pad-bytes eport))))
     bytes)))
   

; Procedure:
; write-field-names
;
;
(define (write-field-names eport fieldname-len field-names . rest)
  (let-optionals 
   rest  ((small #f))
   (if (null? field-names) (MAT5:error "empty field-names list"))
   (let* ((data-type   (miINT8))
	  (data-size   (* (length field-names) (* fieldname-len (MAT5:sizeof data-type))))
	  (bytes       (write-data-element-header eport data-type  data-size small))
	  (bytes       (+ bytes (let loop ((field-names field-names) (bytes 0))
				  (let* ((field-name  (string-pad-right (car field-names) fieldname-len #\nul))
					 (bv          (string->blob field-name)))
				    (let ((bytes  (+ bytes (endian-port-write-byte-vector eport bv MSB))))
				      (if (null? (cdr field-names))
					  bytes
					  (loop (cdr field-names) bytes)))))))
	  (bytes       (+  bytes (write-pad-bytes eport))))
     bytes)))

; Procedure:
; write-fields

(define (write-fields write-sparse-array-data 
		      write-num-array-data)
  (lambda (eport fields)
    (let ((write-data-element  (write-data-element write-sparse-array-data
						   write-num-array-data)))
      (let loop ((fields fields) (bytes 0))
	(if (null? fields)  (+ bytes (write-pad-bytes eport))
	    (loop (cdr fields) (+ bytes (write-data-element eport (miMATRIX) (car fields) #t))))))))


; Procedure:
; write-struct-data
;
; the same as write-cell-data, only we expect each element of the cell
; to be a list of fields (MAT5 data elements), instead of individual
; data elements.
;
(define (write-struct-data  write-sparse-array-data
			    write-num-array-data)
  (let ((write-fields  (write-fields write-sparse-array-data
				     write-num-array-data)))
    (lambda (eport x)
      (let* ((write-el    (lambda (i x ax) (+ ax (write-fields eport x))))
	     (data-size   ((MAT5:array-foldi write-el 'col-major) x 0))
	     (bytes       (+ data-size (write-pad-bytes eport))))
	bytes))))
    


; Procedure:
; write-cell-data
;
;
(define (write-cell-data  write-sparse-array-data
			  write-num-array-data)
  (let ((write-data-element  (write-data-element write-sparse-array-data
						 write-num-array-data)))
    (lambda (eport x)
      (let* ((write-el    (lambda (i x ax) (+ ax (write-data-element eport (miMATRIX) x #t))))
	     (data-size   ((MAT5:array-foldi write-el 'col-major) x 0))
	     (bytes       (+ data-size (write-pad-bytes eport))))
	bytes))))
    

; Procedure:
; write-sparse-array-data
;
;
(define (write-sparse-array-data eport data-type x)
  (write-num-array-data eport data-type x))
    

; Procedure:
; write-num-array-data
;
;
(define (write-num-array-data eport data-type x)
  (let ((begin-pos    (endian-port-pos eport))
	(bytes        (write-data-element-header eport data-type 0)))
    (let* ((write-el    (lambda (i x ax) 
			  (+ ax (write-num-data-element eport data-type x))))
	   (data-size   ((array-foldi write-el 'col-major) x 0))
	   (bytes       (+ bytes data-size (write-pad-bytes eport)))
	   (end-pos     (endian-port-pos eport)))
      (endian-port-setpos eport begin-pos)
      (write-data-element-header eport data-type data-size)
      (endian-port-setpos eport end-pos)
      (values bytes (array-dimensions x)))))
    

; Procedure:
; write-array
;
;
(define (write-array write-sparse-array-data 
		     write-num-array-data)
  (lambda (eport x . rest)
    (let-optionals  rest ((include-header-bytes #f))
    (cases MAT5:array x
	   (MAT5:object (name dims class-name field-names fields)
			(let* ((flags        (integer->bit-vector 0))
			       (begin-pos    (endian-port-pos eport))
			       (header-bytes (write-data-element-header eport (miMATRIX) 0))
			       (bytes        (write-array-flags eport flags (mxOBJECT_CLASS) 0 #t))
			       (bytes        (+ bytes (write-array-dimensions eport dims #t)))
			       (bytes        (+ bytes (write-array-name eport name #t)))
			       (bytes        (+ bytes (write-array-name eport class-name #t)))
			       (bytes        (+ bytes (write-fieldname-len eport MAT5:field-name-length #t)))
			       (bytes        (+ bytes (write-field-names eport MAT5:field-name-length
									 field-names #t)))
			       (bytes        (+ bytes ((write-struct-data write-sparse-array-data
									  write-num-array-data) eport x)))
			       (end-pos      (endian-port-pos eport)))
			  (endian-port-setpos eport begin-pos)
			  (write-data-element-header eport (miMATRIX) bytes)
			  (endian-port-setpos eport end-pos)
			  (if include-header-bytes (+ bytes header-bytes)
			      bytes)))
	   
	   
	   (MAT5:structure (name dims field-names fields)
			   (let* ((flags        (integer->bit-vector 0))
				  (begin-pos    (endian-port-pos eport))
				  (header-bytes (write-data-element-header eport (miMATRIX) 0))
				  (bytes        (write-array-flags eport flags (mxSTRUCT_CLASS) 0 #t))
				  (bytes        (+ bytes (write-array-dimensions eport dims #t)))
				  (bytes        (+ bytes (write-array-name eport name #t)))
				  (bytes        (+ bytes (write-fieldname-len eport MAT5:field-name-length #t)))
				  (bytes        (+ bytes (write-field-names eport MAT5:field-name-length
									    field-names #t)))
				  (bytes        (+ bytes ((write-struct-data write-sparse-array-data
									     write-num-array-data) eport x)))
				  (end-pos      (endian-port-pos eport)))
			     (endian-port-setpos eport begin-pos)
			     (write-data-element-header eport (miMATRIX) bytes)
			     (endian-port-setpos eport end-pos)
			     (if include-header-bytes (+ bytes header-bytes)
				 bytes)))
	   
	   (MAT5:cell-array (name dims cell)
			    (let* ((flags        (integer->bit-vector 0))
				   (begin-pos    (endian-port-pos eport))
				   (header-bytes (write-data-element-header eport (miMATRIX) 0))
				   (bytes        (write-array-flags eport flags (mxCELL_CLASS) 0 #t))
				   (bytes        (+ bytes (write-array-dimensions eport dims #t)))
				   (bytes        (+ bytes (write-array-name eport name #t)))
				   (bytes        (+ bytes ((write-cell-data write-sparse-array-data
									    write-num-array-data) 
							   eport x)))
				   (end-pos      (endian-port-pos eport)))
			      (endian-port-setpos eport begin-pos)
			      (write-data-element-header eport (miMATRIX) bytes)
			      (endian-port-setpos eport end-pos)
			      (if include-header-bytes (+ bytes header-bytes)
				  bytes)))


	   (MAT5:sparse-array (name data-type dims row-index col-index real imag) 
			      (let* ((flags        (integer->bit-vector 0))
				     (flags        (if imag (bit-vector-set! flags mxCOMPLEX_FLAG #t) flags))
				     (nzmax        (* (length row-index) (length col-index)))
				     (begin-pos    (endian-port-pos eport))
				     (header-bytes (write-data-element-header eport (miMATRIX) 0))
				     (bytes        (write-array-flags eport flags (mxSPARSE_CLASS) nzmax #t))
				     (bytes        (+ bytes (write-array-dimensions eport dims #t)))	
				     (bytes        (+ bytes (write-array-name eport name #t)))
				     (bytes        (+ bytes (write-row-index eport row-index)))
				     (bytes        (+ bytes (write-col-index eport col-index)))
				     (bytes        (+ bytes (write-sparse-array-data eport data-type real)))
				     (bytes        (if imag 
						       (+ bytes (write-sparse-array-data eport data-type imag))
						       bytes))
				     (end-pos      (endian-port-pos eport)))
				(endian-port-setpos eport begin-pos)
				(write-data-element-header eport (miMATRIX) bytes)
				(endian-port-setpos eport end-pos)
				(if include-header-bytes (+ bytes header-bytes)
				    bytes)))
	   
	   
	   (MAT5:num-array (name data-type dims real imag)
			   (let* ((flags        (integer->bit-vector 0))
				  (flags        (if imag (bit-vector-set! flags mxCOMPLEX_FLAG #t) flags))
				  (class        (MAT5:data-type->array-class data-type))
				  (begin-pos    (endian-port-pos eport))
				  (header-bytes (write-data-element-header eport (miMATRIX) 0))
				  (bytes        (write-array-flags eport flags class 0 #t))
				  (dims-pos     (endian-port-pos eport))
				  (bytes        (+ bytes (write-array-dimensions eport dims #t)))	
				  (bytes        (+ bytes (write-array-name eport name #t))))
			     (let-values (((data-size real-dims) (write-num-array-data eport data-type real)))
					 (let* ((bytes (+ bytes data-size))
						(bytes (if imag 
							   (let-values 
							    (((data-size imag-dims)
							      (write-num-array-data eport data-type imag)))
							    (+ bytes data-size))
							   bytes))
						(end-pos      (endian-port-pos eport)))
					   (endian-port-setpos eport begin-pos)
					   (write-data-element-header eport (miMATRIX) bytes)
					   (endian-port-setpos eport dims-pos)
					   (write-array-dimensions eport real-dims #t)
					   (endian-port-setpos eport end-pos)
					   (if include-header-bytes (+ bytes header-bytes)
					       bytes)))))
	   
	   (else (MAT5:error "invalid array type"))
	   ))
    ))


; Procedure:
; write-vector
;
;
(define (write-vector eport type vops vect)
  
  (let* ((vector-length  (alist-ref 'vector-length vops))
	 (vector-ref     (alist-ref 'vector-ref vops))
	 (len            (vector-length vect))
	 (size           (* (MAT5:sizeof type) len))
	 (bytes          (write-data-element-header eport type size))
	 (bytes          (+ bytes (let loop ((i 0)  (bytes 0))
				    (if (< i len) bytes
					(let ((v (vector-ref vect i)))
					  (loop (+ i 1)  (+ bytes (write-num-data-element eport type v)))
					  ))
				    ))
			 ))
    (write-pad-bytes  eport)
    bytes))


; Procedure:
; write-string
;
;
(define (write-string eport  str)    
  (let* ((type  (miINT8))
	 (size  (* (MAT5:sizeof type) (length str)))
	 (bytes  (write-data-element-header eport type size))
	 (bytes  (+ bytes (let loop ((chars (string->list str))  (bytes 0))
			    (if (null? chars) bytes
				(let ((bytes (+ bytes (write-num-data-element eport type (car chars)))))
				  (loop (cdr chars) bytes)))
			    ))
		 ))
    bytes))


(define s8vops   `((vector-ref .    ,s8vector-ref)
		   (vector-length . ,s8vector-length)))
(define u8vops   `((vector-ref .    ,u8vector-ref)
		   (vector-length . ,u8vector-length)))
(define s16vops  `((vector-ref .   ,s16vector-ref)
		   (vector-length . ,s16vector-length)))
(define u16vops  `((vector-ref .   ,u16vector-ref)
		   (vector-length . ,u16vector-length)))
(define s32vops  `((vector-ref .   ,s32vector-ref)
		   (vector-length . ,s32vector-length)))
(define u32vops  `((vector-ref .   ,u32vector-ref)
		   (vector-length . ,u32vector-length)))
;;(define s64vops  `((vector-ref .   ,s64vector-ref)
;;		   (vector-length . ,s64vector-length)))
;;(define u64vops  `((vector-ref .   ,u64vector-ref)
;;		   (vector-length . ,u64vector-length)))
(define f32vops  `((vector-ref .   ,f32vector-ref)
		   (vector-length . ,f32vector-length)))
(define f64vops  `((vector-ref .   ,f64vector-ref)
		   (vector-length . ,f64vector-length)))



(define strict-write-data-element
  (write-data-element  write-sparse-array-data
		       write-num-array-data))


; Procedure:
; MAT5:write-data-element:: ENDIAN-PORT * VALUE -> BYTES
;
;
; Writes a MAT5 data element to the given endian port.
;
(define (MAT5:write-data-element eport data-element)
  (cond ((MAT5:array? data-element)  (strict-write-data-element eport (miMATRIX) data-element))
	
	((string? data-element)      (write-string eport data-element))
	
	((s8vector? data-element)    (write-vector eport (miINT8)   s8vops data-element))

	((u8vector? data-element)    (write-vector eport (miUINT8)  u8vops data-element))

	((s16vector? data-element)   (write-vector eport (miINT16)  s16vops data-element))

	((u16vector? data-element)   (write-vector eport (miUINT16) u16vops data-element))

	((s32vector? data-element)   (write-vector eport (miINT32)  s32vops data-element))

	((u32vector? data-element)   (write-vector eport (miUINT32) u32vops data-element))

;;	((s64vector? data-element)   (write-vector eport (miINT64)  s64vops data-element))

;;	((u64vector? data-element)   (write-vector eport (miUINT64) u64vops data-element))

	((f32vector? data-element)   (write-vector eport (miSINGLE) f32vops data-element))

	((f64vector? data-element)   (write-vector eport (miDOUBLE) f64vops data-element))

	(else        (MAT5:error "element " data-element " is of unknown type"))))



)

