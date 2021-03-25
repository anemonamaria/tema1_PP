#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)

;; ATENȚIE: Pentru această etapă a temei este necesar să implementați
;;          întâi TDA-ul queue în fișierul queue.rkt.
;; Reveniți la sarcinile din acest fișier după ce ați implementat tipul 
;; queue și ați verificat implementarea folosind checker-ul.


; Structura counter nu se modifică.
; Ceea ce se modifică este implementarea câmpului queue:
; - în loc de listă, acesta va fi o coadă (o structură de tip queue)
; - acest lucru nu este vizibil în definiția structurii counter,
;   ci în implementarea operațiilor acestui tip de date (tipul counter)
(define-struct counter (index tt et queue) #:transparent)


; TODO
; Actualizați funcțiile de mai jos astfel încât ele să folosească
; o structură de tip queue pentru reprezentarea cozii de persoane.
; Elementele cozii continuă să fie perechi (nume . nr_produse).
; Este esențial să respectați "bariera de abstractizare", adică să
; operați cu coada doar folosind interfața acestui TDA:
; - empty-queue
; - queue-empty?
; - enqueue
; - dequeue
; - top
; Obs: Doar câteva funcții vor necesita actualizări.
(define (empty-counter index)           ; testată de checker
  (make-counter index 0 0 empty-queue))

(define (update f counters index)
  (if (null? counters) counters
      (cond
        ((equal? (counter-index (car counters)) index) (cons (f (car counters)) (cdr counters)))
        (else (cons (car counters) (update f (cdr counters) index))))))

(define (tt+ minutes)
  (lambda (C) 
    (match C
      [(counter index tt et queue)
       (struct-copy counter C [index index] [tt (+ tt minutes)] [et et] [queue queue])])))

(define (et+ minutes)
  (lambda (C)
    (match C
      [(counter index tt et queue)
       (struct-copy counter C [index index] [tt tt] [et (+ et minutes)] [queue queue])])))

(define (add-to-counter name items)     ; testată de checker
  (λ (C)                                ; nu modificați felul în care funcția își primește argumentele
    (struct-copy counter C [index (counter-index C)]
                           [tt (+ (counter-tt C) items)]
                           [et (cond
                                  ((queue-empty? (counter-queue C)) (+ (counter-et C) items))
                                  (else (counter-et C)))]
                           [queue (enqueue (cons name items) (counter-queue C))])))

(define (general-func g f max-index tt-et-max counters)
  (cond ((null? counters) (cons max-index tt-et-max))
        ((equal? (g  (f counters) tt-et-max) #t) (general-func g f (counter-index (car counters)) (f counters) (cdr counters)))
        (else (general-func g f max-index tt-et-max (cdr counters)))))

(define (give-me-tt counters)
  (counter-tt (car counters)))

(define (give-me-et counters)
  (counter-et (car counters)))

(define (min-tt counters)
  (general-func < give-me-tt 999999999999 999999999999 counters))
(define (min-et counters)
  (general-func < give-me-et 999999999999 999999999999 counters))

(define (remove-first-from-counter C)   ; testată de checker
;'your-code-here)
  (if (queue-empty? (counter-queue C))
      (struct-copy counter C [index (counter-index C)] [tt 0] [et 0] [queue null]) 
      (struct-copy counter C [index (counter-index C)]
                             [tt (total-time (dequeue (counter-queue C)))]
                             [et (cond
                                   ((queue-empty? (dequeue (counter-queue C))) 0)
                                   (else (cdr (top (dequeue (counter-queue C)))))) ] ; (cdr (car (cdr (counter-queue C))))
                             [queue (dequeue (counter-queue C))])))
(define (total-time queue)
   (if (queue-empty? queue)
       0
       (+ (cdr (top queue)) (total-time (dequeue queue)))))
; TODO
; Implementați o funcție care calculează starea unei case după un număr dat de minute.
; Funcția presupune, fără să verifice, că în acest timp nu a ieșit nimeni din coadă, 
; deci va avea efect doar asupra câmpurilor tt și et.
; (cu alte cuvinte, este responsabilitatea utilizatorului să nu apeleze această funcție
; cu minutes > timpul până la ieșirea primului client din coadă)
; Atenție: casele fără clienți nu trebuie să ajungă la timpi negativi!
(define (pass-time-through-counter minutes)
  (λ (C)
    (struct-copy counter C [index (counter-index C)]
                           [tt (cond
                                 ((>= (- (counter-tt C) minutes) 0) (- (counter-tt C) minutes))
                                 (else 0))]
                           [et (cond
                                 ((>= (- (counter-et C) minutes) 0) (- (counter-et C) minutes))
                                 (else 0))]
                           [queue (counter-queue C)])))
  

; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 2, apar modificări în:
; - formatul listei de cereri (parametrul requests)
; - formatul rezultatului funcției (explicat mai jos)
; requests conține 4 tipuri de cereri (3 moștenite din etapa 2 plus una nouă):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă            (ca înainte)
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute       (ca înainte)
;   - (ensure <average>) - cât timp tt-ul mediu al tuturor caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)         (ca înainte)
;   - <x> - trec <x> minute de la ultima cerere, iar starea caselor se actualizează
;           corespunzător (cu efect asupra câmpurilor tt, et, queue)                     (   NOU!   )
; Obs: Au dispărut cererile de tip remove-first, pentru a lăsa loc unui mecanism mai 
; sofisticat de a scoate clienții din coadă (pe măsură ce trece timpul).
; Sistemul trebuie să proceseze cele 4 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele cu tt minim (dintre casele la care au voie)  (ca înainte)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți) (ca înainte)
; - tt-ul mediu (ttmed) se calculează pentru toate casele (și cele fast, și cele slow), 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>       (ca înainte)
; - când timpul prin sistem avansează cu <x> minute, tt-ul, et-ul și queue-ul tuturor 
;   caselor se actualizează pentru a reflecta trecerea timpului; dacă unul sau mai mulți 
;   clienți termină de stat la coadă, ieșirile lor sunt contorizate în ordine cronologică.
; Rezultatul funcției serve va fi o pereche cu punct între:
; - lista sortată cronologic a clienților care au părăsit supermarketul
;   - fiecare element din listă va avea forma (index_casă . nume)
;   - dacă mai mulți clienți ies simultan, sortați-i crescător după indexul casei
; - lista caselor în starea finală (ca rezultatul din etapele 1 și 2)
; Obs: Pentru a contoriza ieșirile din cozi, puteți să lucrați într-o funcție ajutătoare
; (cu un parametru în plus față de funcția serve), pe care serve doar o apelează.
(define (serve requests fast-counters slow-counters)
  (if (null? requests)
      (append fast-counters slow-counters)
      (match (car requests)
        [(list 'ensure average) (cond
                                  ((> (/ (calculate-all-tt (append fast-counters slow-counters)) (length (append fast-counters slow-counters))) average)
                                   (serve requests fast-counters (append slow-counters (list (empty-counter (+ (length (append fast-counters slow-counters)) 1))))))
                                  (else (serve (cdr requests) fast-counters slow-counters)))]

        [(list name n-items)  (cond 
                              ((<= n-items ITEMS) (cond
                                                    ((<= (cdr (min-tt fast-counters)) (cdr (min-tt slow-counters)))
                                                     (serve (cdr requests) (update (add-to-counter name n-items) fast-counters (car (min-tt fast-counters))) slow-counters))
                                                    (else (serve (cdr requests) fast-counters (update (add-to-counter name n-items) slow-counters (car (min-tt slow-counters)))))))
                              (else (serve (cdr requests) fast-counters (update (add-to-counter name n-items) slow-counters (car (min-tt slow-counters))))))]
      [(list 'delay index minutes)  (cond
                                       ((null? (find-counter fast-counters index))
                                        (serve (cdr requests) fast-counters (update (increase-tt-et minutes) slow-counters index)))
                                       (else (serve (cdr requests) (update (increase-tt-et minutes) fast-counters index) slow-counters)))])))

(define (find-counter counters index)
   (cond ((null? counters) null)
         ((= (counter-index (car counters)) index) (car counters))
         (else (find-counter (cdr counters) index))))

(define (increase-tt-et minutes)
   (lambda (C)
     (match C
       [(counter index tt et queue)
       (struct-copy counter C [index index] [tt (+ tt minutes)] [et (+ et minutes)] [queue queue])])))

(define (calculate-all-tt counters)
   (if (null? counters) 0
         (+ (counter-tt (car counters)) (calculate-all-tt (cdr counters)))))

(define C1 (empty-counter 1))
(define C2 (empty-counter 2))
(define C3 (empty-counter 3))
(define C4 (empty-counter 4))
(define C5 (make-counter 5 12 8 (queue '((remus . 6) (vivi . 4)) '() 2 0)))


(serve '(2 (ana 14) 6)
                     (list C1)
                     (list C2 C3))
(list
                                    '()
                                    (counter 1 0 0 (queue '() '() 0 0))
                                    (counter 2 8 8 (queue '() '((ana . 14)) 0 1))
                                    (counter 3 0 0 (queue '() '() 0 0)))