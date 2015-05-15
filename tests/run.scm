;;
;; 
;; MAT 5.0 library test cases.
;;

(require-extension test files datatype srfi-4 srfi-63 endian-port mat5-lib)

(define (mat5-num-array-real x)
  (cases MAT5:array x
	 (MAT5:num-array  (name ty dims real imag) real)
	 (else #f)))
			  

(define (mat5-num-array-imag x)
  (cases MAT5:array x
	 (MAT5:num-array  (name ty dims real imag) imag)
	 (else #f)))
			  

(define (mat5:test:spec)

  (let-values (((fd temp-path) (file-mkstemp "/tmp/mat5-test.XXXXXX")))

    (let* ((temp-port (open-output-file* fd))
	  (eport (port->endian-port temp-port)))

      (test-group
       "MAT5 library numeric array test from specification document p. 1-20"
       
       (let ((aa
	      (let ((real (make-array (A:floR32b 0) 2 2))
		    (imag (make-array (A:floR32b 0) 2 2)))
		
		(array-set! real 1.0  0 0)
		(array-set! real 3.0  0 1)
		(array-set! real 2.0  1 0)
		(array-set! real 4.0  1 1)
		
		(array-set! imag 1.0  0 0)

		(MAT5:num-array  "my_array" (miSINGLE) '(2 2) real imag))))
	 
	 
	 (test-assert "Write MAT5 file header" 
		      (MAT5:write-header eport "MATLAB 5.0 MAT-file" ""))
	 
	 (test-assert "Write test array aa" 
		      (MAT5:write-data-element eport aa))
	 
	 (test-assert "Close endian port for writing" 
		      (close-endian-port eport))
	 
	 (let ((eport (open-endian-port 'read temp-path)))
	   
	   (test-assert "Read header from file" 
			(MAT5:read-header eport))
	   
	   (let ((aa1 (MAT5:data-element-data (car (MAT5:read-data-element eport)))))
	     (test "Compare real data from file array with original array"
		   (mat5-num-array-real aa)
		   (mat5-num-array-real aa1))
	     )
	   
	   (close-endian-port eport)

           (delete-file temp-path)
   
	   ))
       ))
    ))

  
(define (mat5:test:r+w)
  (let* ((eport+path
	  (let-values (((fd temp-path) (file-mkstemp "/tmp/mat5-test.XXXXXX")))
	    (let ((temp-port (open-output-file* fd)))
	      (cons (port->endian-port temp-port) temp-path))))

	 (eport (car eport+path))
	 (temp-path (cdr eport+path))

	 (aa
	  (let ((av (make-array (A:floR64b 0) 2 3 4)))
	    
	    (array-set! av 2.0  0 0 0)
	    (array-set! av 7.0  0 0 1)
	    (array-set! av 8.0  0 0 2)
	    (array-set! av 9.0  0 0 3)
	    
	    (array-set! av 1.0  0 1 0)
	    (array-set! av 13.0 0 1 1)
	    (array-set! av 14.0 0 1 2)
	    (array-set! av 15.0 0 1 3)
	    
	    (array-set! av 4.0  0 2 0)
	    (array-set! av 19.0 0 2 1)
	    (array-set! av 20.0 0 2 2)
	    (array-set! av 21.0 0 2 3)
	    
	    (array-set! av 5.0  1 0 0)
	    (array-set! av 10.0 1 0 1)
	    (array-set! av 11.0 1 0 2)
	    (array-set! av 12.0 1 0 3)
	    
	    (array-set! av 3.0  1 1 0)
	    (array-set! av 16.0 1 1 1)
	    (array-set! av 17.0 1 1 2)
	    (array-set! av 18.0 1 1 3)
	    
	    (array-set! av 6.0  1 2 0)
	    (array-set! av 22.0 1 2 1)
	    (array-set! av 23.0 1 2 2)
	    (array-set! av 24.0 1 2 3)
	    
	    (MAT5:num-array  "aa1" (miDOUBLE) '(2 3 4) av  #f)))
	 )

    (test-group "MAT5 data reading and writing"

		(test-assert "Write MAT5 file header" 
			     (MAT5:write-header eport "MATLAB 5.0 MAT-file" ""))
		
		(test-assert "Write test array aa" 
			     (MAT5:write-data-element eport aa))
		
		(close-endian-port eport)
		
		(let ((eport (open-endian-port 'read temp-path)))
		  
		  (test-assert "Read header from file" 
			       (MAT5:read-header eport))
		  
		  (let ((aa1 (MAT5:read-data-element eport)))
		    
		    (test "Compare array from file with original array"
			  aa (MAT5:data-element-data (car aa1)))
		    
		    (close-endian-port eport)
		    
		    )))

    (delete-file temp-path)

    ))



(define (mat5:teststruct:r+w)

   
  (let* ((eport+path
	  (let-values (((fd temp-path) (file-mkstemp "/tmp/mat5-test.XXXXXX")))
	    (let ((temp-port (open-output-file* fd)))
	      (cons (port->endian-port temp-port) temp-path))))
	 (eport (car eport+path))
	 (temp-path (cdr eport+path))
	 (aa
	  (let ((av (init-MAT5:cell 1 1))
		(w  (make-array (A:floR64b 0) 1 1))
		(y  (make-array (A:floR64b 0) 1 1))
		(z  (make-array (A:floR64b 0) 1 1)))
	    
	    (array-set! w 1.0  0 0)
	    (array-set! y 2.0  0 0)
	    (array-set! z 3.0  0 0)
	    
	    (MAT5:cell-set! av 
			    `(,(MAT5:num-array "w" (miDOUBLE) '(1 1) w #f)
			      ,(MAT5:num-array "y" (miDOUBLE) '(1 1) y #f)
			      ,(MAT5:num-array "z" (miDOUBLE) '(1 1) z #f))
			    0 0)

	    (MAT5:structure  "aa1" '(1 1) '("w" "y" "z")  av)))
	 )

  (test-group
   "MAT5 read+write test for structures"

   (test-assert "Write MAT5 file header" 
	      (MAT5:write-header eport "MATLAB 5.0 MAT-file" ""))

   (test-assert "Write test structure aa" 
	      (MAT5:write-data-element eport aa))

   (close-endian-port eport)

   (open-endian-port 'read temp-path)

   (test-assert "Read header from file" 
		(MAT5:read-header eport))

   (let ((aa1 (MAT5:read-data-element eport)))

     (print "structure from file = " 
	    (MAT5:data-element-data (car aa1)))

     (test "Compare structure from file with original structure"
	   aa (MAT5:data-element-data (car aa1))))

   (close-endian-port eport)

   (delete-file temp-path)
   )))


(mat5:test:spec)
(mat5:test:r+w)
(mat5:teststruct:r+w)

