#lang racket

(require racket/gui)

(struct vec-2D (x y) #:transparent)

(define (add-vec2D a b)
  (vec-2D (+ (vec-2D-x a) (vec-2D-x b))
          (+ (vec-2D-y a) (vec-2D-y b))))

(define snake (list (vec-2D 0 0)))

(define (keep-on-screen pos)
        (vec-2D (if (> (vec-2D-x pos) 15)
                    0
                    (if (< (vec-2D-x pos) 0) 15 (vec-2D-x pos)))
                (if (> (vec-2D-y pos) 15)
                    0
                    (if (< (vec-2D-y pos) 0) 15 (vec-2D-y pos)))))

(define (move snake dir)
  (let ([head (last snake)])
    (append snake (list (keep-on-screen (add-vec2D head dir))))))

(define up (vec-2D 0 -1))
(define down (vec-2D 0 1))
(define right (vec-2D 1 0))
(define left (vec-2D -1 0))

(define (shift lst)
  (append (rest lst) (list (first lst))))

(define (shift-times lst times)
  (if (equal? times 0)
      lst
      (shift-times (shift lst) (- times 1))))

(define muj-list (list 1 2 3 4))

(define (collision? snake)
  (if (equal? (length snake) 1)
      #f
      (let ([orp (lambda (a b)
                   (or a b))]
            [zip (lambda (a b)
                   (map append '(a) b))])
        (not (empty? (filter identity (map (lambda (x)
                                             (ormap equal? snake (shift-times snake x)))
                                           (range 1 (length snake)))))))))


(define direction down)

(define frame (new frame% [label "Snake"]
                   [min-width 256]
                   [min-height 256]
                   [stretchable-width 0]
                   [style (list 'no-resize-border)]
   	 	[stretchable-height 0]))

(define game% (class canvas%
                 (define/override (on-char event)
                   (let ([c (send event get-key-code)])
                     (cond
                       [(equal? c 'up) (set! direction up) (void)]
                       [(equal? c 'down) (set! direction down) (void)]
                       [(equal? c 'left) (set! direction left) (void)]
                       [(equal? c 'right) (set! direction right) (void)]
                       [else (void)])))
                 (super-new)))
(define canvas (new game% [parent frame]))
                 

(send frame show #t)

(define (draw-snake! snake dc)
  (if (empty? snake)
      (void)
      (let ([head (first snake)])
        (send dc draw-rectangle (* (vec-2D-x head) 16)
                                (* (vec-2D-y head) 16)
                                16
                                16)
        (draw-snake! (rest snake) dc))))

(define (draw-points! points dc)
  (if (empty? points)
      (void)
      (let ([head (first points)])
        (send dc draw-ellipse (* (vec-2D-x head) 16)
                                (* (vec-2D-y head) 16)
                                16
                                16)
        (draw-points! (rest points) dc))))

(define (filter-collisions snake points)
  (if (empty? snake)
      points
      (filter-collisions (rest snake) (filter (lambda (x) (not (equal? (first snake) x)))
              points))))

(define (collision-with-points? snake points)
  (if (empty? snake)
      #f
      (if (ormap (lambda (x) (equal? x (first snake)))
                 points)
          #t
          (collision-with-points? (rest snake) points))))

(define (generate-random-point!)
  (vec-2D (- (random 1 16) 1) (- (random 1 16) 1)))

(define (game-loop snake dc points)
  (send dc clear)
  (if (collision? snake)
      (send dc draw-text (string-append "Prohrál jsi! Tvé score je: " (number->string (- (length snake) 1))) 0 0)
  (begin
    (draw-points! points dc)
    (draw-snake! snake dc)
    (sleep (max (/ 1 (length snake)) 1/10))
    (if (collision-with-points? snake points)
        (game-loop (move snake direction) dc (filter-collisions snake points))
        (if (< 95 (random 1 100))
            (game-loop (rest (move snake direction)) dc (append points (list (generate-random-point!))))
            (game-loop (rest (move snake direction)) dc points))))))

(thread (lambda () (game-loop snake (send canvas get-dc) (list (generate-random-point!)))))