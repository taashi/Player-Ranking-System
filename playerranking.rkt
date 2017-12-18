;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(provide
 tie
 defeated
 defeated?
 outranks
 outranked-by
 power-ranking)



;;; An Outcome is one of
;;;     -- a Tie
;;;     -- a Defeat
;;;
;;; OBSERVER TEMPLATE:
;;; outcome-fn : Outcome -> ??
#;
(define (outcome-fn o)
  (cond ((tie? o) ...)
        ((defeat? o) ...)))

;;A Competitor is represented as a String (any string will do).

;; REPRESENTATION:
;; A tied  is represented as a struct   (make-tied c1 c2)
;; with the following fields:
;; c1 :  A Competitor is represented as a String (any string will do).
;; c2 :  A Competitor is represented as a String (any string will do).
                        

;; IMPLEMENTATION
(define-struct tied(c1 c2))

;; CONSTRUCTOR TEMPLATE
;; (make-tied String String)

;; OBSERVER TEMPLATE
;; tied-fn : Tie -> ??
(define (tied-fn t)
  (...
   (tied-c1 t)
   (tied-c2 t)))

;; REPRESENTATION:
;; A defeat  is represented as a struct   (make-defeat c1 c2)
;; with the following fields:
;; c1 :  A Competitor is represented as a String (any string will do).
;; c2 :  A Competitor is represented as a String (any string will do).
                        

;; IMPLEMENTATION
(define-struct defeat(c1 c2))

;; CONSTRUCTOR TEMPLATE
;; (make-defeat String String)

;; OBSERVER TEMPLATE
;; defeat-fn : Tied -> ??
(define (defeat-fn d)
  (...
   (defeat-c1 d)
   (defeat-c2 d)))

;; An OutcomeList is represented as a list of Outcomes.

;; CONSTRUCTOR TEMPLATE AND INTERPRETATION
;; empty                  -- the empty sequence
;; (cons o or)
;;   WHERE:
;;    o  is an Outcome    -- the first outcome
;;                           in the sequence
;;    or is an OutcomeList  -- the rest of the 
;;                           outcomes in the sequence

;; OBSERVER TEMPLATE:
;; ol-fn : OutcomeList -> ??
;(define (ol-fn list)
; (cond
;   [(empty? list) ...]
;  [else (... (first list)
;             (ol-fn (rest list)))]))

;; A CompetitorList is represented as a list of Competitors.

;; CONSTRUCTOR TEMPLATE AND INTERPRETATION
;; empty                  -- the empty sequence
;; (cons c cr)
;;   WHERE:
;;    o  is a Competitor    -- the first competitor
;;                           in the sequence
;;    cr is a CompetitorList  -- the rest of the 
;;                           competitors in the sequence

;; OBSERVER TEMPLATE:
;; cl-fn : CompetitorList -> ??
;(define (cl-fn list)
; (cond
;  [(empty? list) ...]
;  [else (... (first list)
;             (cl-fn (rest list)))]))

;;; tie : Competitor Competitor -> Tie
;;; GIVEN: the names of two competitors
;;; RETURNS: an indication that the two competitors have
;;;     engaged in a contest, and the outcome was a tie
;;; EXAMPLE:  (tie "A" "B")->(make-tied "A" "B")
;;STRATEGY : Use constructor template of tied
(define(tie c1 c2)
  (make-tied c1 c2))
;;; defeated : Competitor Competitor -> Defeat
;;; GIVEN: the names of two competitors
;;; RETURNS: an indication that the two competitors have
;;;     engaged in a contest, with the first competitor
;;;     defeating the second
;;; EXAMPLE: (defeated "A" "B")->(make-defeat "A" "B")
;;STRATEGY : Use constructor template of defeat
(define(defeated c1 c2)
  (make-defeat c1 c2))
;;; defeated? : Competitor Competitor OutcomeList -> Boolean
;;; GIVEN: the names of two competitors and a list of outcomes
;;; RETURNS: true if and only if one or more of the outcomes indicates
;;;     the first competitor has defeated or tied the second
;;; EXAMPLES:
;;;     (defeated? "A" "B" (list (defeated "A" "B") (tie "B" "C")))
;;;  => true
;;;
;;;     (defeated? "A" "C" (list (defeated "A" "B") (tie "B" "C")))
;;;  => false
;;;
;;;     (defeated? "B" "A" (list (defeated "A" "B") (tie "B" "C")))
;;;  => false
;;;
;;;     (defeated? "B" "C" (list (defeated "A" "B") (tie "B" "C")))
;;;  => true
;;;
;;;     (defeated? "C" "B" (list (defeated "A" "B") (tie "B" "C")))
;;;  => true
;;STRATEGY: Using HOF on OutcomeList
(define(defeated? com1 com2 outlist)
  (ormap
   ;;Outcome->Boolean
   ;;GIVEN: Outcome which can be a tie or defeat
   ;;RETURNS: true if  the outcome indicates
   ;;;     the first competitor has defeated or tied the second
   (lambda(el) (is-defeated? com1 com2 el)) outlist))
;;is-defeated?: Competitor Competitor  Outcome-> Boolean
;;GIVEN:  Names of the two competitors and an outcome whih can either be defeat
;;or tie
;;RETURNS: True if the first competitor has defeated or
;tied the second competitor
;;STRATEGY: Use observer template of the OutcomeList
;;EXAMPLES: (is-defeated? "A" "B"(defeated "A" "B"))
;->true

(define (is-defeated? com1 com2 el)
  (cond
    [(defeat? el) (and (equal? com1 (defeat-c1 el))
                       (equal? com2 (defeat-c2 el)))]
    [(tied? el) (or(and (equal? com1 (tied-c1 el))
                        (equal? com2(tied-c2 el)))
                   (and (equal? com2 (tied-c1 el))
                        (equal? com1 (tied-c2 el))))]
    [else false]))
;;TESTS
(begin-for-test
  (check-equal? (defeated? "C" "B" (list (defeated "A" "B") (tie "B" "C")))
                #true "true should be returned")
  (check-equal? (defeated? "A" "B"(list(tie "D" "B")(defeated "A" "B")))
                #true "true should be returned")
  (check-equal? (defeated? "A" "B"(list(tie "D" "C")(tie "A" "B")))
                #true "true should be returned")
  (check-equal? (defeated? "B" "A" (list (defeated "D" "E") (tie "F" "G")))
                #false "false should be returned")
  (check-equal? (is-defeated? "B" "A" '())
                #false "false should be returned"))
;;; outranks : Competitor OutcomeList -> CompetitorList
;;; GIVEN: the name of a competitor and a list of outcomes
;;; RETURNS: a list of the competitors outranked by the given
;;;     competitor, in alphabetical order
;;; NOTE: it is possible for a competitor to outrank itself
;;; EXAMPLES:
;;;     (outranks "A" (list (defeated "A" "B") (tie "B" "C")))
;;;  => (list "B" "C")
;;;
;;;     (outranks "B" (list (defeated "A" "B") (defeated "B" "A")))
;;;  => (list "A" "B")
;;;
;;;     (outranks "C" (list (defeated "A" "B") (tie "B" "C")))
;;;  => (list "B" "C")
;;STRATEGY: Use HOF sort on CompetitorList 
(define(outranks com outlist)
  (sort(next-outranks (check-def-tie com outlist) outlist empty) string<=?))

;;next-outranks: CompetitorList OutcomeList CompetitorList->CompetitorList
;GIVEN:A list of competitors that have been defeated or tied
;by the given competitor,
;a list of outcomes and
;a list of competitors defeated or tied by the given
;competitor so far(dt-so-far)
;WHERE: dt-so-far is the list of competitors defeated or tied by the given
;Competitor so far
;;RETURNS:a list of the competitors outranked by the given
;;;     competitor
;;STRATEGY:Conditions on the CompetitorList(comlist)
;;HALTING MEASURE:
;Recur on length of competitor list till the outlist becomes empty
;;EXAMPLES: (next-outranks '("B")(list (defeated "A" "B") (defeated "B" "C")
;(defeated "C" "D")) '())->(list "D" "C" "B")
(define (next-outranks comlist outlist dt-so-far)
  (cond
    [(empty? comlist) empty]
    [(not (member? (first comlist) dt-so-far))
     (remove-duplicates (check-defeat-tie comlist outlist dt-so-far) )]
    [else(next-outranks (rest comlist)
                        outlist (cons (first comlist) dt-so-far))]))
;;check-defeat-tie :CompetitorList OutcomeList CompetitorList->CompetitorList
;GIVEN:A list of competitors that have been defeated or tied
;by the given competitor,
;a list of outcomes and
;a list of competitors defeated or tied by the given
;competitor so far(dt-so-far)
;WHERE: dt-so-far is the list of competitors defeated or tied by the given
;Competitor so far.
;;RETURNS:a list of the competitors outranked by the given
;;;     competitor
;;STRATEGY: Use append and helper function
;;EXMAPLES: (check-defeat-tie (list "B" "A")
;(list(defeated "A" "B") (defeated "B" "C")
;(defeated "C" "D"))(list "A" "C"))
;->(list "B" "A")
(define (check-defeat-tie comlist outlist dt-so-far)
  (append
   (next-outranks
    (check-def-tie
     (first comlist) outlist )
    outlist
    (cons (first comlist)  dt-so-far))
   (next-outranks
    (rest comlist)outlist dt-so-far)comlist))
;;check-def-tie: Competitor OutcomeList->CompetitorList
;;GIVEN: Name of a competitor and an outcomelist
;;RETURNS:The list of competitors that have been defeated by given competitor
;;STRATEGY: Use HOF on OutcomeList
;;EXAMPLES:(check-def-tie "A"(list (defeated "A" "B")
;(defeated "B" "C") (defeated "C" "D")))->(list "B")
(define (check-def-tie com outlist)
  (foldr append empty (map
                       ;;Outcome->Competitor
                       ;;GIVEN: Outcome which is defeat or tie
                       ;;RETURNS: The list of competitors
                       ;that have been defeated by given competitor
                       (lambda (el) (check-list com el)) outlist)))
;;check-list : Outcome Competitor -> CompetitorList
;;GIVEN:  Outcome which is defeat or tie
;;RETURNS: The list of competitors that have been defeated by given competitor
;;STRATEGY: Use observer template of the OutcomeList
;;EXAMPLES:  (check-list "A" (tie "A" "B"))->
;(list "A" "B")
(define (check-list com el)
  (cond
    [(defeat? el) (if (equal? com (defeat-c1 el))
                      (list(defeat-c2 el) )
                      empty)]
    [(tied? el) (if (or (equal? com (tied-c2 el))
                        (equal? com (tied-c1 el)))
                    (list (tied-c1 el) (tied-c2 el))
                    empty)]))
;;remove-duplicates: CompetitorList->CompetitorList
;;GIVEN:CompetitorList with duplicate competitors
;;RETURNS: CompetitorList with duplicates removed
;;STRATEGY: Use observer template of CompetitorList
;;EXAMPLES:(remove-duplicates (list "A" "B" "C" "C" "C"))
;->(list "A" "B" "C")
(define(remove-duplicates lst)
  (cond
    [(empty? lst)empty]
    [(member? (first lst)(rest lst))
     (remove-duplicates(rest lst))]
    [else(cons(first lst)(remove-duplicates(rest lst)))]))
;TESTS
(begin-for-test
  (check-equal? (outranks "C" (list (defeated "A" "B") (tie "B" "C")))
                (list "B" "C")(list "B" "C"))
  (check-equal?(outranks "A" (list (defeated "A" "B") (tie "B" "C")))
               (list "B" "C"))
  (check-equal?(outranks "B"(list(defeated "A" "B")(tie "B" "C")
                                 (defeated "C" "D")(tie "B" "E")))
               (list "B" "C" "D" "E"))
  (check-equal?(outranks "E" (list (defeated "A" "D")(defeated "A" "E")
                                   (defeated "C" "B") (defeated "C" "F")
                                   (tie "D" "B")(defeated "F" "E")))
               (list)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; outranked-by : Competitor OutcomeList -> CompetitorList
;;; GIVEN: the name of a competitor and a list of outcomes
;;; RETURNS: a list of the competitors that outrank the given
;;;     competitor, in alphabetical order
;;; NOTE: it is possible for a competitor to outrank itself
;;; EXAMPLES:
;;;     (outranked-by "A" (list (defeated "A" "B") (tie "B" "C")))
;;;  => (list)
;;;
;;;     (outranked-by "B" (list (defeated "A" "B") (defeated "B" "A")))
;;;  => (list "A" "B")
;;;
;;;     (outranked-by "C" (list (defeated "A" "B") (tie "B" "C")))
;;;  => (list "A" "B" "C")
;;STRATEGY: Use HOF on CompetitorList 
(define (outranked-by com list)
  (sort (next-outranked-by(check-tie-def com list) list empty)string<=?))


;;next-outranked-by: CompetitorList OutcomeList CompetitorList->CompetitorList
;GIVEN:A list of competitors that have defeated or tied the given competitor,
;a list of outcomes and
;a list of competitors that have defeated or tied the  given
;competitor so far(dt-so-far)
;WHERE: dt-so-far is the list of competitors defeated or tied by the given
;Competitor so far.
;;RETURNS:a list of the competitors that outrank the given competitor
;;STRATEGY:Conditions on the CompetitorList(comlist)
;;HALTING MEASURE:
;Recur on length of competitor list till the outlist becomes empty
;;EXAMPLES: (next-outranked-by '("B")(list (defeated "A" "B") (defeated "B" "C")
;(defeated "C" "D")) '())->(list "A" "B")
(define (next-outranked-by comlist outlist dt-so-far)
  (cond
    [(empty? comlist) empty]
    [(not (member? (first comlist)dt-so-far))
     (remove-duplicates (check-tie-defeat comlist outlist dt-so-far))]
    [else(next-outranked-by (rest comlist) outlist
                            (cons (first comlist) dt-so-far))]))
;;check-tie-defeat:CompetitorList OutcomeList CompetitorList->CompetitorList
;GIVEN:A list of competitors that have defeated or tied the given competitor,
;a list of outcomes and
;a list of competitors that have defeated or tied the  given
;competitor so far(dt-so-far)
;WHERE: dt-so-far is the list of competitors defeated or tied by the given
;Competitor so far.
;;RETURNS:a list of the competitors that outrank the given competitor
;;EXAMPLE (check-tie-defeat '("B")(list (defeated "A" "B") (defeated "B" "C")
;(defeated "C" "D")) '())->(list "A" "B")
;;STRATEGY: Use append and helper function
(define(check-tie-defeat comlist outlist dt-so-far)
        (append
                                 (next-outranked-by
                                  (check-tie-def (first comlist) outlist)
                                  outlist
                                  (cons (first comlist) dt-so-far))
                                 (next-outranked-by
                                  (rest comlist)outlist dt-so-far)comlist))
;;check-tie-def: Competitor OutcomeList->CompetitorList
;;GIVEN:  Name of a competitor and an outcomelist
;;RETURNS:The list of competitors that have defeated the given competitor
;;STRATEGY: Use HOF on OutcomeList
;;EXAMPLES: (check-tie-def "A"(list (defeated "A" "B")
;(defeated "B" "C") (defeated "C" "D")))->'()  
(define (check-tie-def com outlist)
  (foldr append empty (map
                       ;;Outcome->CompetitorList
                       ;;GIVEN: Outcome which is defeat or tie
                       ;;RETURNS: The list of competitors
                       ;that have defeated the given competitor
                       (lambda (el) (check-list-ob com el))
                       outlist)))
;;check-list-ob : Outcome Competitor -> CompetitorList
;;GIVEN: Outcome which is defeat or tie and name of competitor 
;;RETURNS:The list of competitors
;that have defeated the given competitor
;;STRATEGY: Use observer template of the Outcome
;;EXAMPLES:  (check-list-ob "A" (tie "A" "B"))->
;(list "A" "B")
(define (check-list-ob com el)
  (cond
    [(defeat? el) (if (equal? com (defeat-c2 el))
                      (list(defeat-c1 el) )
                      empty)]
    [(tied? el) (if (or (equal? com (tied-c2 el))
                        (equal? com (tied-c1 el)))
                    (list (tied-c1 el) (tied-c2 el))
                    empty)]))
;;TESTS
(begin-for-test
  (check-equal? (outranked-by "C" (list (defeated "A" "B") (tie "B" "C")))
                (list "A" "B" "C")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; power-ranking : OutcomeList -> CompetitorList
;;; GIVEN: a list of outcomes
;;; RETURNS: a list of all competitors mentioned by one or more
;;;     of the outcomes, without repetitions, with competitor A
;;;     coming before competitor B in the list if and only if
;;;     the power-ranking of A is higher than the power ranking
;;;     of B.
;;; EXAMPLE:
;;;     (power-ranking
;;;      (list (defeated "A" "D")
;;;            (defeated "A" "E")
;;;            (defeated "C" "B")
;;;            (defeated "C" "F")
;;;            (tie "D" "B")
;;;            (defeated "F" "E")))
;;;  => (list "C"  
;;;           "A"   
;;;           "F"  
;;;           "E"  
;;;           "B"   
;;;           "D")
;;STRATEGY: Use HOF on OutcomeList
(define (power-ranking outlist)
  (sort(obtaincompetitors outlist)
       ;;;;Competitor1 Competitor2->boolean
                       ;;GIVEN:Names of two competitors
                       ;;RETURNS:  True if the powerranking(c1)>powerranking(c2)
       (lambda (el1 el2)
       (powerrankingcharacterstics? el1 el2 outlist))))


;;powerrankingcharacterstics: Competitor Competitor Outcomelist->Boolean
;;GIVEN:Names of two competitors and outcomelist
;;RETURNS: True if the powerranking(c1)>powerranking(c2)
;;STRATEGY: Use conditions on the competitors
;;EXAMPLES: (powerrankingcharacterstics ? "A" "B"(list (defeated "A" "B" )
;(tie "B" "A") (defeated "C" "A")))->true

(define(powerrankingcharacterstics? c1 c2 outlist)
(cond
[(cond1 c1 c2 outlist) true]
[ (cond2 c1 c2 outlist)true]
[(cond3 c1 c2 outlist) true]
[(cond4 c1 c2 outlist) true]
[else false]))
;;cond1:Competitor COmpetitor OutcomeList->Boolean
;GIVEN:Names of two competitors and list of outcomes
;;RETURNS:True if the Number of competitors outranked-by(c2)>
; Number of competitors outranked-by(c1)
;STRATEGY: Use simpler fn
;;EXAMPLE: (cond1"A" "B"(list (defeated "A" "B" )
;(tie "B" "A") (defeated "C" "A")))->false
(define(cond1 c1 c2 outlist)
(<(length(outranked-by c1 outlist)) (length(outranked-by c2 outlist))))
;;cond2:Competitor COmpetitor OutcomeList->Boolean
;GIVEN:Names of two competitors and list of outcomes
;;RETURNS :true if the Number of competitors outranked-by(c2)=
;Number of competitors outranked-by(c1) and
;Number of competitors outranks(c1)>Number of competitors outranks(c2)
;STRATEGY: Use simpler fn
;;EXAMPLE  (cond2 "C" "A"(list (defeated "A" "B" )
;(tie "B" "A") (defeated "C" "A")))->false
(define(cond2 c1 c2 outlist)
  (and(=(length(outranked-by c1 outlist)) (length(outranked-by c2 outlist)))
(>(length(outranks c1 outlist))(length(outranks c2 outlist)))))
;;cond3:Competitor COmpetitor OutcomeList->Boolean
;GIVEN:Names of two competitors and list of outcomes
;;RETURNS :true if the Number of competitors outranked-by(c2)=
;Number of competitors outranked-by(c1) and
;Number of competitors outranks(c1)=Number of competitors outranks(c2) and
;Non-loosing percetange(c1)> Non-loosing percentage (c2)
;STRATEGY:Use simpler fn
;;EXAMPLE: (cond3 "A" "B"(list (defeated "A" "B" )
;(tie "B" "A") (defeated "C" "A")))->true
(define (cond3 c1 c2 outlist)
  (and
   (=(length(outranked-by c1 outlist)) (length(outranked-by c2 outlist)))
(=(length(outranks c1 outlist))(length(outranks c2 outlist)))
(>(nlp c1 outlist)(nlp c2 outlist))))
;;tests

;;cond4:Competitor Competitor OutcomeList->Boolean
;;GIVEN:Names of two competitors and list of outcomes
;;RETURNS :true if the Number of competitors outranked-by(c2)=
;Number of competitors outranked-by(c1) and
;Number of competitors outranks(c1)=Number of competitors outranks(c2) and
;Non-loosing percetange(c1)= Non-loosing percentage (c2) and
;name of competitor(c1)<name of competitor(c2)
;STRATEGY: Use simpler fn
;;EXAMPLE:(cond4 "A" "B"(list (defeated "A" "B" )
;(tie "B" "A") (defeated "C" "A")))->false
(define (cond4 c1 c2 outlist)
  (and
   (=(length(outranked-by c1 outlist)) (length(outranked-by c2 outlist)))
(=(length(outranks c1 outlist))(length(outranks c2 outlist)))
(=(nlp c1 outlist)(nlp c2 outlist))
(string<? c1 c2 )))

;;obtaincompetitors:OutcomeList->CompetitorList
;GIVEN:  list of outcomes
;;RETURNS:All the competitors participating in an outcome
;;STRATEGY: Use HOF on OutcomeList
;;EXAMPLE:(obtaincompetitors (list (defeated "A" "B")(tie "D" "E")))
;->(list "A" "D" "B" "E")
(define(obtaincompetitors list)
(remove-duplicates
(append(map
        ;;Outcome->Competitor
   ;;GIVEN: Outcome which is defeat or tie
   ;;RETURNS: First competitor participating in an outcome
        (lambda(el)(c1comp el)) list)
(map
;;Outcome->Competitor
   ;;GIVEN: Outcome which is defeat or tie
   ;;RETURNS:  second competitor participating in an outcome
 (lambda(el)(c2comp el)) list))))
;;c1comp: Outcome-> Competitor
;;GIVEN:Outcome which is defeat or tie
;;RETURNS:First COmpetitor participating in an outcome
;;EXAMPLE:(c1comp (defeated "A" "B"))->"A"
;;STRATEGY: Use observer template of outcome
(define(c1comp el)
(cond
[(tied? el) (tied-c1 el)]
[(defeat? el)(defeat-c1 el)]))
;;c2comp: Outcome-> Competitor
;;GIVEN:Outcome which is defeat or tie
;;RETURNS:Second COmpetitor participating in an outcome
;;EXAMPLE:(c2comp (defeated "A" "B"))->"B"
;;STRATEGY: Use observer template of outcome
(define(c2comp el)
(cond
[(tied? el) (tied-c2 el)]
[(defeat? el)(defeat-c2 el)]))

;;def-total: Competitor Outcomelist->PosInt
;;GIVEN:Name of  competitor and list of outcomes
;;RETURNS: number of outcomes in which competitor
;defeats or ties another competitor
;;STRATEGY:Use HOF on OutcomeList
;;EXAMPLES:(def-total "A" (list(defeated "A" "B")(tie "A" "C")))->2
(define (def-total com list)
(length(filter
        ;Outcome->Boolean
   ;;GIVEN: Outcome
   ;;RETURNS: true if the outcome list is not empty
        (lambda(el)(not(empty? el)))(map
    ;;Outcome->Competitor
   ;;GIVEN: Outcome which can be a defeat or tie
   ;;RETURNS: name of given Competitor if has defeated or tied
                                     ; another competitor
 (lambda(el)
(numberofdefeats com el)) list))))
;;numberofdefeats: Competitor Outcome-> Competitor
;;GIVEN:Name of  competitor and list of outcomes
;;RETURNS:name of given Competitor if has defeated or tied another competitor
;;STRATEGY:Use observer template of outcome
;;EXAMPLES:(numberofdefeats "A" (defeated "A" "B"))->"A"
 (define(numberofdefeats com el)
(cond
[(defeat? el) (if(equal? com (defeat-c1 el))
(defeat-c1 el)
empty)]
[(tied? el)(if(or(equal? com(tied-c1 el))(equal? com(tied-c2 el)))
(tied-c1 el)
empty)]))
 ;;TESTS
(begin-for-test
  (check-equal?(numberofdefeats "A" (defeated "A" "B"))
"A")
   (check-equal? (numberofdefeats "A" (tie "C" "D"))
'()))
;;outcome-total: Competitor Outcomelist->PosInt
;;GIVEN:Name of  competitor and list of outcomes
;;RETURNS:Total number of outcomesof a competitor
;;STRATEGY: Use HOF on OutcomeList
;;EXAMPLES: (outcome-total "A" (list (defeated "A" "B" )
;(tie "B" "A") (defeated "C" "A")))->3
 (define(outcome-total com list)
(length
(filter
 ;;Outcome->Boolean
   ;;GIVEN: Outcome
   ;;RETURNS: true if the outcome list is not empty
 (lambda(el)(not(empty? el))) 
        ;;Outcome->Competitor
   ;;GIVEN: Outcome which can be a defeat or tie
   ;;RETURNS:  given competitor if it appears in the
        ;;outcome
      (map  (lambda(el)(numberofoutcomes com el)) list))))


;;numberofoutcomes: Competitor Outcome->Competitor
 ;;GIVEN: Name of a competitor and an outcome which is defeat or tie
 ;;RETURNS: Competitor if it appears in the outcome
 ;;EXAMPLE: (numberofoutcomes "A" (defeated "A" "B"))->"A"
 ;;STRATEGY: Use observer template of outcome
(define(numberofoutcomes com el)
(cond
[(defeat? el)(if(or(equal? com(defeat-c1 el)) (equal? com (defeat-c2 el)))
(defeat-c1 el)
empty)]
[(tied? el)(if(or(equal? com(tied-c1 el)) (equal? com(tied-c2 el)))
(tied-c1 el)
empty)]))
;;nlp:Competitor OutcomeList->PosInt
;;GIVEN:Name of  competitor and list of outcomes
;;RETURNS: The non losing percentage of a competitor
;;STRATEGY: Transcribe formula
;;EXAMPLES: (nlp  "A" (list (defeated "A" "B" )(tie "B" "A")
;(defeated "C" "A")))->66.6
(define (nlp com list)
(*(/(def-total com list)(outcome-total com list)) 100))


;;TESTS
(begin-for-test(check-equal? (numberofoutcomes "A" (tie "C" "D"))
'()))


;;TESTS
(begin-for-test
  (check-equal?(power-ranking
      (list (defeated "A" "D")
            (defeated "A" "E")
            (defeated "C" "B")
            (defeated "C" "F")
            (tie "D" "B")
            (defeated "F" "E")))
               (list "C" "A" "F" "E" "B" "D"))
  (check-equal? (powerrankingcharacterstics? "C" "D"
       (list (defeated "C" "D") (defeated "D" "C") (defeated "C" "E")))
#true)
  )
