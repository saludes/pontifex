#lang racket
(provide launch)

;; http://www.nplaumann.de/8-kryptographie/1-der-solitaire-verschluesselungsalgorithmus.html


  (require racket/class racket/match racket/list)
  (require games/cards racket/draw)

  (define (make-jocker-bm deck v)
    (let* ([bm-path (collection-file-path "Kij5eAxiq.png" "pontifex")]
           [bm (read-bitmap bm-path)]
           [cw (send (car deck) card-width)]
           [ch (send (car deck) card-height)]
           [sx (/ cw (send bm get-width))]
           [sy (/ ch (send bm get-height))])
      (define dc (new bitmap-dc% [bitmap (make-bitmap cw ch)]))
      (send dc draw-bitmap-section-smooth bm 0 0 cw ch 0 0 (send bm get-width) (send bm get-height))
      ;; (send dc set-text-foreground "red")
      (send dc set-font (make-object font% 18 'system 'normal 'bold))
      (send dc draw-text (symbol->string v) 55 75)
      (send dc get-bitmap)))
  
  (define (make-jocker deck name)
    (let ([front-bm (make-jocker-bm deck name)])
      (make-card front-bm #f 0 name)))
  
  
  (define deck
    (let* ([deck (make-deck)]
           [ja (make-jocker deck 'A)]
           [jb (make-jocker deck 'B)])
      (append (list ja jb) deck)))
  
  
  (define (card->int card)
    (match (send card get-value)
      ['A 53]
      ['B 53]
      [n  (let ([suit (send card get-suit-id)])
            (cond
              [(not (number? n)) (error "numeric card number")]
              [(or (> n 13) (<= n 0)) (error "card number not in range 1..13:" n)]
              [else (+ (* (- suit 1) 13) (- n 1))]))]))
  
  (define (int->card n)
    (cond
      [(= n 52) 'A]
      [(= n 53) 'B]
      [(and (< n 53) (>= n 0))
       (let ([assoc-suit '((0 . clubs) (1 . diamonds) (2 . hearts) (3 . spades))]
             [nc (remainder n 13)]
             [nsuit (quotient n 13)])
         (cons (cdr (assoc nsuit assoc-suit)) (+ 1 nc)))]
      [else (error "not a valid int for card" n)]))
  
  
  (define (int->pos deck j)
    (modulo j (length deck)))
  
  (define (deck-ref deck dpos)
    (list-ref deck (int->pos deck dpos)))
  
  (define (remove-card-at deck dpos)
    (let ([cd (deck-ref deck dpos)])
      (values cd (remove cd deck))))
  
  (define (insert-card-at deck dpos cd)
    (if (= dpos (length deck))
        (append deck (list cd))
        (let*-values ([(pos) (int->pos deck dpos)]
                      [(head tail) (split-at deck pos)])
          (append head (list cd) tail))))
  
  
  
  (define (move-card-down deck n [dn 1])
    (let-values ([(cd l) (remove-card-at deck n)]
                 [(n2) (+ n dn)])
      (insert-card-at l n2 cd)))
  
  
  (define (triple-cut deck b-pos a-pos)
    (let ([head (take deck b-pos)]
          [tail (drop deck (+ 1 a-pos))]
          [middle (take (drop deck b-pos) (+ 1 (- a-pos b-pos)))])
      (append tail middle head)))
  
  
  (define (count-cut deck n)
    (let*-values
        ([(m) (if (< n 0) (+ (length deck) n -1) n)]
         [(head tail) (split-at deck (- m 1))]
         [(pivot tail2) (split-at tail 1)])
      (append tail2 pivot head)))
  

(define (find-card deck n [suit-id 0])
  (let* ([cd? (λ (cd) (and
                       (eq? (send cd get-value) n)
                       (eq? (send cd get-suit-id) suit-id)))]
         [results (filter cd? deck)])
    (apply values (for/first ([i (in-naturals)] [c deck] #:when (cd? c)) (list i c)))))

(define (find x y) #f)

(define (find-jockers deck)
  (let-values ([(ja ca) (find-card deck 'A)]
               [(jb cb) (find-card deck 'B)])
    (apply values (sort (list ja jb)))))


(define (jocker? c)
  (zero? (send c get-suit-id)))
  
(define (new-key deck)
  (let step ([d deck])
    (let* ([d1 (step-deck d)]
           [i (card->int (first d1))]
           [c (list-ref d1 (deck-ref d1 (+ i 1)))])
      (if (jocker? c) step (list c d1)))))
  
  
  
  (define (step-deck deck)
    (let*-values ([(ja _) (find-card deck 'A)]
                  [(deck-1) (move-card-down deck ja)]
                  [(jb _) (find-card deck-1 'B)]
                  [(deck-2) (move-card-down deck-1 jb 2)]
                  [(j1 j2) (find-jockers deck-2)]
                  [(deck-3) (triple-cut deck-2 j1 j2)]
                  [(j) (card->int (last deck-3))])
      (count-cut deck-3 j))) 
  
  
  (define (launch)
    (define tb (make-table "Pontifex" 25 1))
    (for ([c deck]) (send c flip))
    (send tb add-cards (shuffle deck) 0 0 (λ (i) (values (* i 30) 0)))
    (send tb show #t))

  (module* main #f (launch)) 
