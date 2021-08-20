# Introduction to Data Abstraction

### Exercise 2.1

```scheme title="Code from the text"
(define (numer x) (car x))
(define (denom x) (cdr x))
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))
```
```scheme title="My answer"
(define (make-rat n d)
  (let ((g (gcd n d)))
    (let ((abs-n (abs (/ n g)))
          (abs-d (abs (/ d g))))
      (cond ((< 0 (* n d)) (cons abs-n abs-d))
            ((> 0 (* n d)) (cons (- abs-n) abs-d))
            (else (error "Cannot accept 0 for numerator or denominator"))))))
```

### Exercise 2.2

```scheme title="Code from the text"
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))
```
```scheme title="My answer"
;Segments library
(define (make-segment start-point end-point)
  (cons start-point end-point))
(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))

;Points library
(define (make-point x y)
  (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (midpoint-segment seg)
  (let ((start-seg (start-segment seg))
        (end-seg (end-segment seg)))
    (make-point (/ (+ (x-point start-seg) (x-point end-seg)) 2)
                (/ (+ (y-point start-seg) (y-point end-seg)) 2))))
```
I'm a bit conflicted about passing in points into "make-segment". Shouldn't the user of make-segment be unaware that the underlying implementation of segments are points? Instead of 2 points, maybe "make-segment" should accept 4 numbers (the x and y coordinates of the 2 points).

But then again, by passing in the coordinates of points, am I not making an assumption about the implementation of points? They could very well be implemented in terms of angles and magnitudes, not x and y coordinates. In other words, I might be making an even worse abstraction error, since x and y coordinates are 2 levels down the abstraction ladder while segments are only one level down. I guess it's a general rule - you can't create something without knowing anything about its implementation.  

### Exercise 2.3
```scheme title="My answer"
;Addition to points library
(define (distance-between-points p1 p2)
  (sqrt (+ (square (- (y-point p1) (y-point p2)))
           (square (- (x-point p1) (x-point p2))))))

;Addition to segments library
(define (get-segment-length segment)
  (let ((p1 (start-segment segment))
        (p2 (end-segment segment)))
    (distance-between-points p1 p2)))

;First rectangles implementation
(define (make-rectangle breadth-segment length-segment)
  (cons breadth-segment length-segment))
(define (get-breadth rectangle)
  (get-segment-length (car rectangle)))
(define (get-length rectangle)
  (get-segment-length (cdr rectangle)))

;Now we construct the get-area and get-perimeter procedures. 
;These procedures are one abstraction level above rectangles.
;In a more general system they should get the area and perimeter of any shape, 
;but here let us assume rectangles are the only shapes that exist.

(define (get-area rectangle)
  (* (get-breadth rectangle) (get-length rectangle)))
(define (get-perimeter rectangle)
  (+ (* 2 (get-breadth rectangle))
     (* 2 (get-length rectangle))))

;Tests
(define (test-area-and-perimeter)
  (let ((breadth-segment (make-segment (make-point 0 0) (make-point 5 0)))
        (length-segment (make-segment (make-point 0 0) (make-point 0 5))))
    (let ((rectangle (make-rectangle breadth-segment length-segment)))
      (check (get-area rectangle) => 25)
      (check (get-perimeter rectangle) => 20))))

;The alternate implementation of rectangles below skips the segment abstraction and directly uses points.
(define (make-rectangle top-left top-right bottom-left bottom-right)
  (list top-left top-right bottom-left bottom-right))
(define (get-breadth rectangle)
  (min (distance-between-points (car rectangle) (cadr rectangle))
       (distance-between-points (car rectangle) (caddr rectangle))))
(define (get-length rectangle)
  (max (distance-between-points (car rectangle) (cadr rectangle))
       (distance-between-points (car rectangle) (caddr rectangle))))

;Tests
(define (test-area-and-perimeter)
  (let ((rectangle (make-rectangle (make-point 0 5) (make-point 3 5) (make-point 0 0) (make-point 3 0))))
    (check (get-area rectangle) => 15)
    (check (get-perimeter rectangle) => 16)))
```

### Exercise 2.4
```scheme title="My answer"
;Verifying that car works the way it should.
(define (verify-car)
  (equal? (car (cons 1 2)) 1))

;Defining an analogous cdr.
(define (cdr z)
  (z (lambda(p q) q)))
(define (verify-cdr)
  (equal? (cdr (cons 1 2)) 2))
```

### Exercise 2.5
```scheme title="My answer"
(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (car x)
  (define (car-iter count val)
    (if (equal? (modulo val 2) 0)
        (car-iter (+ count 1) (/ val 2))
        count))
  (car-iter 0 x))

(define (cdr x)
  (define (car-iter count val)
    (if (equal? (modulo val 3) 0)
        (car-iter (+ count 1) (/ val 3))
        count))
  (car-iter 0 x))
```

### Exercise 2.6
```scheme title="Code from the text"
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
```
Let's follow the hint and try simplifying this monster.
```scheme title="My answer"
(add-1 zero)
(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x))))
(lambda (f) (lambda (x) (f ((lambda (x) x) x))))
(lambda (f) (lambda (x) (f x)))

;Here we see that zero means the function f is applied
;to x 0 times, one means it is applied one time, and so on.

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define (add a b)
  (lambda (f)
    (lambda (x)
      ((a f) ((b f) x)))))
```
