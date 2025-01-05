;; The following follows the structured presented in pp. x--y in
;; Structure and Interpretation of Computer Programs, Second Edition

;; Define some fundamental predicates
(define (variable? v) (symbol? v))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? exp num)
  (and (number? exp) (= num exp)))

;; Define a constructor, predicate and selectors for sum expressions
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))
(define (sum? exp)
  (and (pair? exp) (eq? (car exp) '+)))
(define (addend exp) (cadr exp))
(define (augend exp) (caddr exp))

;; Define a constructor, predicate and selectors for product expressions
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))
(define (product? exp)
  (and (pair? exp) (eq? (car exp) '*)))
(define (multiplier exp) (cadr exp))
(define (multiplicand exp) (caddr exp))

;; Define a constructor, predicate and selectors for exponentiation expressions
(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((and (number? base) (number? exponent))
         (expt base exponent))
        (else (list '** base exponent))))
(define (exponentiation? exp)
  (and (pair? exp) (eq? (car exp) '**)))
(define (base exp) (cadr exp))
(define (exponent exp) (caddr exp))

;; Define a constructor, predicate and selectors for quotient expressions
(define (make-quotient n d)
  (cond ((=number? n 0) 0)
        ((=number? d 1) n)
        ((and (number? n) (number? d)) (/ n d))
        ((equal? n d) 1)
        (else (list '/ n d))))
(define (quotient? exp)
  (and (pair? exp) (eq? (car exp) '/)))
(define (numerator exp) (cadr exp))
(define (denominator exp) (caddr exp))

;; Define a constructor, predicate and selectors for sin expressions
(define (make-sine x)
  (cond ((=number? x 0) 0)
        ((number? x) (sin x))
        (else (list 'sin x))))
(define (sine? exp)
  (and (pair? exp) (eq? (car exp) 'sin)))
(define (sine-arg exp) (cadr exp))

;; Define a constructor, predicate and selectors for cos expressions
(define (make-cosine x)
  (cond ((=number? x 0) 1)
        ((number? x) (cos x))
        (else (list 'cos x))))
(define (cosine? exp)
  (and (pair? exp) (eq? (car exp) 'cos)))
(define (cosine-arg exp) (cadr exp))

;; Define the core differentiation procedure
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                  (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
           (make-product
             (exponent exp)
             (make-exponentiation (base exp)
                                 (make-sum (exponent exp) -1)))
           (deriv (base exp) var)))
        ((quotient? exp)
         (make-quotient
           (make-sum
             (make-product (deriv (numerator exp) var)
                          (denominator exp))
             (make-product (make-product -1 (numerator exp))
                          (deriv (denominator exp) var)))
           (make-power (denominator exp) 2)))
        ((sine? exp)
         (make-product
           (make-cosine (sine-arg exp))
           (deriv (sine-arg exp) var)))
        ((cosine? exp)
         (make-product -1
           (make-product
             (make-sine (cosine-arg exp))
             (deriv (cosine-arg exp) var))))
        (else (error "unknown expression type" exp))))
