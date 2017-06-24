
;(my-last lst) returns the last number of the list
(define my-last
	(lambda (lst)
		(cond ((null? lst) (error "empty list"))
			  (else (car (reduceToOne lst)))
		)
	)
)

;helper func to reduce a list to size of one
(define reduceToOne
	(lambda (lst)
		(cond ((= (length lst) 1) lst)
		      (else (reduceToOne (cdr lst)))
		)
	)
)

;error fun to display message ok?
(define error
	(lambda (msg)
		(display msg)
	)
)

;append element to the list
(define snoc
	(lambda (x lst)
		(cond ((null? lst) (list x))
			  (else (cons (car lst) (snoc x (cdr lst))))
		)
	)
)

;get range into a list
(define range
	(lambda (x)
		(reverse (rangerevrsd x))
	)
)

;get range reversed 
(define rangerevrsd
	(lambda (x)
		(cond ((= x 0) (list) )
        	  ((< x 0) (list) )
        	  ((> x 0) (cons (- x 1) (rangerevrsd (- x 1)) ) )
  		)		
	)
)

;deep-sum a list
;adapted from http://www.cs.sfu.ca/CourseCentral/383/tjd/scheme-intro.html
(define deep-sum
	(lambda (lst)
		(cond
            ((null? lst)
                0)
            ((list? (car lst))
                (+ (deep-sum (car lst)) (deep-sum (cdr lst))))
            ((number? (car lst))
                (+ (car lst) (deep-sum (cdr lst))))
            (else
                (deep-sum (cdr lst)))
        )
	)
)

;count primes
(define count-primes
	(lambda (n)
		(cond ((= n 0) 0 )
        	  ((< n 0) 0 )
        	  ((> n 0) (countTruths (map isprime (range (+ 1 n)))) )
  		)
	)
)

(define isprime
	(lambda (n)
		(cond ((< n 2) false )
			  ((= n 2) true )
			  ((> n 2) (moddy n (ceiling (sqrt n))))
  		)
	)
)

(define moddy
	(lambda (n x)
		(cond 
			  ((= x 2) (cond ((= (modulo n x) 0) false)
							 (else true) 
					   ) 
			  )
			  (else (cond ((= (modulo n x) 0) false)
						  (else (moddy n (- x 1)) 
						  ) 
					)
			  )	  
  		)
	)
)

(define countTruths
	(lambda (lst)
		(cond
            ((null? lst)
                0)
            ((true? (car lst))
                (+ 1 (countTruths (cdr lst))))
            (else
                (countTruths (cdr lst)))
        )
	)
)

(define true?
	(lambda (x)
		(false? (false? x))
	)
)

;check whether x is the number 0 or 1
(define is-bit?
	(lambda (x)
		(cond
            ((integer? x)
                (cond
            		((or (= x 0) (= x 1))
                		true)
            		(else
                		false)
        		)
            )
            (else
                false)
        )
	)
)

;returns true if lst is the empty list, or if it contains only bits
(define is-bit-seq?
	(lambda (lst)
		(cond
            ((equal? lst '())
                true
            )
            ((is-bit? (car lst))
        		(is-bit-seq? (cdr lst))
            )
            (else
                false)
        )
	)
)

;returns a list of all the bit sequences of length n.
(define all-bit-seqs
	(lambda (n)
		(cond
			((= n 0)
				(list)
			)
			(else
				(map get-bit (range (expt 2 n)) (getlistofn n))
			)
		)
	)
)

(define get-bit
	(lambda (x n)
		(cond
			((= (length (getbitanysize x)) n)
				(getbitanysize x)
			)
			((< (length (getbitanysize x)) n)
				(padzeros (- n (length (getbitanysize x))) (getbitanysize x))
			)
		)
	)
)

(define padzeros
	(lambda (x lst)
		(cond
			((= x 0)
				lst
			)
			(else
				(cons 0 (padzeros (- x 1) lst))
			)
		)
	)
)

(define getbitanysize
	(lambda (x)
		(reverse (getbitanysizervsd x))
	)
)

(define getbitanysizervsd
	(lambda (x)
		(cond
			((= x 0)
				(list)
			)
			((= (remainder x 2) 1)
				(cons 1 (getbitanysizervsd (quotient x 2)))
			)
			((= (remainder x 2) 0)
				(cons 0 (getbitanysizervsd (quotient x 2)))
			)			
		)
	)
)

(define getlistofn
	(lambda (n)
		(map (lambda (x) n) (range (expt 2 n)))
	)
)
