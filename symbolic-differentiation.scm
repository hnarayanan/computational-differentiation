;; The following follows the structured presented in pp. x--y in
;; Structure and Interpretation of Computer Programs, Second Edition

;; Define some predicates
(define (variable? v) (symbol? v))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? exp num)
  (and (number? exp) (= num exp)))
(define (sum? exp)
  (and (pair? exp) (eq? (car exp) '+)))
(define (product? exp)
  (and (pair? exp) (eq? (car exp) '*)))

;; Define a constructor and selectors for sum expressions
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))
(define (addend exp) (cadr exp))
(define (augend exp) (caddr exp))

;; Define a constructor and selectors for product expressions
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))
(define (multiplier exp) (cadr exp))
(define (multiplicand exp) (caddr exp))

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
        (else (error "Unknown expression type" exp))))
