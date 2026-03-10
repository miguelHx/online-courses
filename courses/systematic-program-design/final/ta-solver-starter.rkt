;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ta-solver-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; ta-solver-starter.rkt



;  PROBLEM 1:
;
;  Consider a social network similar to Twitter called Chirper. Each user has a name, a note about
;  whether or not they are a verified user, and follows some number of people.
;
;  Design a data definition for Chirper, including a template that is tail recursive and avoids
;  cycles.
;
;  Then design a function called most-followers which determines which user in a Chirper Network is
;  followed by the most people.
;

(define-struct chirper-user (name verified? following))
;; ChirperUser is (make-chirper-user String Boolean (listof ChirperUser))
;; interp. the user's name, whether or not they are verified, and a list of users they are following

;; template: structural recursion (arbitrary-arity tree), encapsulate w/ local, tail-recursive w/ worklist,
;;           context-preserving accumulator - what users we have visited so far

(define (fn-for-chirper c0)
  ;; todo is (listof ChirperUser); a worklist accumulator
  ;; visited is (listof String); context preserving accumulator, names of users already visited
  (local [(define (fn-for-chirper-user cu todo visited) 
            (if (member (chirper-user-name cu) visited)
                (fn-for-lof todo visited)
                (fn-for-lof (append (chirper-user-following cu) todo)
                            (cons (chirper-user-name cu) visited))))
          (define (fn-for-lof todo visited)
            (cond [(empty? todo) (...)]
                  [else
                   (fn-for-chirper-user (first todo)
                                        (rest todo)
                                        visited)]))]
    (fn-for-chirper-user c0 empty empty)))

(define ch-network-1-A
  (shared ((-A- (make-chirper-user "A" true (list -B- -D-)))
           (-B- (make-chirper-user "B" true (list -C- -E-)))
           (-C- (make-chirper-user "C" false (list -B-)))
           (-D- (make-chirper-user "D" true (list -E-)))
           (-E- (make-chirper-user "E" true (list -F- -A-)))
           (-F- (make-chirper-user "F" true (list -E-))))
    -A-))
(define ch-network-1-B
  (shared ((-A- (make-chirper-user "A" true (list -B- -D-)))
           (-B- (make-chirper-user "B" true (list -C- -E-)))
           (-C- (make-chirper-user "C" false (list -B-)))
           (-D- (make-chirper-user "D" true (list -E-)))
           (-E- (make-chirper-user "E" true (list -F- -A-)))
           (-F- (make-chirper-user "F" true (list -E-))))
    -B-))
(define ch-network-1-C
  (shared ((-A- (make-chirper-user "A" true (list -B- -D-)))
           (-B- (make-chirper-user "B" true (list -C- -E-)))
           (-C- (make-chirper-user "C" false (list -B-)))
           (-D- (make-chirper-user "D" true (list -E-)))
           (-E- (make-chirper-user "E" true (list -F- -A-)))
           (-F- (make-chirper-user "F" true (list -E-))))
    -C-))
(define ch-network-1-D
  (shared ((-A- (make-chirper-user "A" true (list -B- -D-)))
           (-B- (make-chirper-user "B" true (list -C- -E-)))
           (-C- (make-chirper-user "C" false (list -B-)))
           (-D- (make-chirper-user "D" true (list -E-)))
           (-E- (make-chirper-user "E" true (list -F- -A-)))
           (-F- (make-chirper-user "F" true (list -E-))))
    -D-))
(define ch-network-1-E
  (shared ((-A- (make-chirper-user "A" true (list -B- -D-)))
           (-B- (make-chirper-user "B" true (list -C- -E-)))
           (-C- (make-chirper-user "C" false (list -B-)))
           (-D- (make-chirper-user "D" true (list -E-)))
           (-E- (make-chirper-user "E" true (list -F- -A-)))
           (-F- (make-chirper-user "F" true (list -E-))))
    -E-))
(define ch-network-1-F
  (shared ((-A- (make-chirper-user "A" true (list -B- -D-)))
           (-B- (make-chirper-user "B" true (list -C- -E-)))
           (-C- (make-chirper-user "C" false (list -B-)))
           (-D- (make-chirper-user "D" true (list -E-)))
           (-E- (make-chirper-user "E" true (list -F- -A-)))
           (-F- (make-chirper-user "F" true (list -E-))))
    -F-))

(define ch-network-2-A
  (shared ((-A- (make-chirper-user "A" true (list -D-)))
           (-B- (make-chirper-user "B" true (list -D-)))
           (-C- (make-chirper-user "C" false (list -D- -A-)))
           (-D- (make-chirper-user "D" true (list)))
           (-E- (make-chirper-user "E" true (list -D-)))
           (-F- (make-chirper-user "F" true (list -D-))))
    -A-))
(define ch-network-2-B
  (shared ((-A- (make-chirper-user "A" true (list -D-)))
           (-B- (make-chirper-user "B" true (list -D-)))
           (-C- (make-chirper-user "C" false (list -D- -A-)))
           (-D- (make-chirper-user "D" true (list)))
           (-E- (make-chirper-user "E" true (list -D-)))
           (-F- (make-chirper-user "F" true (list -D-))))
    -B-))
(define ch-network-2-C
  (shared ((-A- (make-chirper-user "A" true (list -D-)))
           (-B- (make-chirper-user "B" true (list -D-)))
           (-C- (make-chirper-user "C" false (list -D- -A-)))
           (-D- (make-chirper-user "D" true (list)))
           (-E- (make-chirper-user "E" true (list -D-)))
           (-F- (make-chirper-user "F" true (list -D-))))
    -C-))
(define ch-network-2-D
  (shared ((-A- (make-chirper-user "A" true (list -D-)))
           (-B- (make-chirper-user "B" true (list -D-)))
           (-C- (make-chirper-user "C" false (list -D- -A-)))
           (-D- (make-chirper-user "D" true (list)))
           (-E- (make-chirper-user "E" true (list -D-)))
           (-F- (make-chirper-user "F" true (list -D-))))
    -D-))
(define ch-network-2-E
  (shared ((-A- (make-chirper-user "A" true (list -D-)))
           (-B- (make-chirper-user "B" true (list -D-)))
           (-C- (make-chirper-user "C" false (list -D- -A-)))
           (-D- (make-chirper-user "D" true (list)))
           (-E- (make-chirper-user "E" true (list -D-)))
           (-F- (make-chirper-user "F" true (list -D-))))
    -E-))
(define ch-network-2-F
  (shared ((-A- (make-chirper-user "A" true (list -D-)))
           (-B- (make-chirper-user "B" true (list -D-)))
           (-C- (make-chirper-user "C" false (list -D- -A-)))
           (-D- (make-chirper-user "D" true (list)))
           (-E- (make-chirper-user "E" true (list -D-)))
           (-F- (make-chirper-user "F" true (list -D-))))
    -F-))


;; (listof ChirperUser) -> String
;; given a chirper network, determine which user has the most followers.
(check-expect (most-followers empty) "")
(check-expect (most-followers (list ch-network-1-A ch-network-1-B ch-network-1-C
                                    ch-network-1-D ch-network-1-E ch-network-1-F))
              "E")
(check-expect (most-followers (list ch-network-2-A ch-network-2-B ch-network-2-C
                                    ch-network-2-D ch-network-2-E ch-network-2-F))
              "D")

;(define (most-followers locu) "A") ;stub

;; template: structural recursion, encapsulated w/ local, accumulator for result
(define (most-followers locu0)
  ;; visited-so-far is (listof String); users we have visited so far globally
  (local [(define (get-global-user-with-followers-list locu visited-so-far)
            (cond [(empty? locu) empty]
                  [else
                   ; locuwfc - list of chirper user with followers count
                   (local [(define locuwfc-and-visited (fn-for-chirper-user (first locu) empty visited-so-far empty))
                           (define locuwfc (first locuwfc-and-visited))
                           (define visited (second locuwfc-and-visited))]
                     (merge-locuwfc locuwfc
                                    (get-global-user-with-followers-list (rest locu) visited)))]))

          ;; (listof (pair ChirperUser Number)) (listof (pair ChirperUser Number)) -> (listof (pair ChirperUser Number))
          ;; merges two lists of pairs together, combining follower counts and/or appending new pairs
          (define (merge-locuwfc locuwfc1 locuwfc2)
            (cond [(empty? locuwfc1) locuwfc2]
                  [else
                   (local [(define ch-user-1 (first (first locuwfc1)))
                           (define follower-count-1 (second (first locuwfc1)))
                           (define find-result (assoc ch-user-1 locuwfc2))]
                     (if (not (false? find-result))
                         (cons (list ch-user-1 (+ follower-count-1 (second find-result)))
                               (merge-locuwfc (rest locuwfc1) (remove find-result locuwfc2)))
                         (cons (first locuwfc1)
                               (merge-locuwfc (rest locuwfc1) locuwfc2))))]))

          ;; todo is (listof ChirperUser); a worklist accumulator
          ;; visited is (listof String); context preserving accumulator, names of users already visited
          ;; rsf is (listof (pair ChirperUser Number)); a context preserving accumulator, a list of
          ;;                                            users with their follower counts
          (define (fn-for-chirper-user cu todo visited rsf)
            (if (member (chirper-user-name cu) visited)
                (fn-for-lof todo visited rsf)
                (fn-for-lof (append (chirper-user-following cu) todo)
                            (cons (chirper-user-name cu) visited)
                            (next-rsf rsf (chirper-user-following cu)))))

          ;; (listof (pair ChirperUser Number)) (listof ChirperUser) -> (listof (pair ChirperUser Number))
          ;; given a list of users with follower counts, and a list of users, increment follower count of
          ;; users with counts if a user appears in following.
          ;; if a user does not appear in users with followers, add new entry to users with followers
          (define (next-rsf locuwfc following-list)
            (cond [(empty? locuwfc) (map (lambda (following) (list following 1)) following-list)]
                  [else
                   (local [(define ch-user (first (first locuwfc)))
                           (define follower-count (second (first locuwfc)))]
                     (if (member? ch-user following-list)
                         (cons (list ch-user (add1 follower-count))
                               (next-rsf (rest locuwfc) (remove ch-user following-list)))
                         (cons (first locuwfc)
                               (next-rsf (rest locuwfc) following-list))))]))
          
          (define (fn-for-lof todo visited rsf)
            (cond [(empty? todo) (list rsf visited)]
                  [else
                   (fn-for-chirper-user (first todo)
                                        (rest todo)
                                        visited
                                        rsf)]))

          ;; (listof (pair ChirperUser Number)) -> ChirperUser
          ;; given a list of chirper users with follower counts, extract the user with the maximum count.
          ;; ASSUME: input list will never be empty
          (define (extract-max-follower-user locuwfc0)
            (local [(define first-user (first (first locuwfc0)))
                    (define first-follower-count (second (first locuwfc0)))
                    ;; msf is Number; max followers so far
                    ;; mcusf is ChirperUser; max user so far
                    (define (find-max-user locuwfc msf mcusf)
                      (cond [(empty? locuwfc) mcusf]
                            [else
                             (local [(define ch-user (first (first locuwfc)))
                                     (define follower-cnt (second (first locuwfc)))]
                               (if (> follower-cnt msf)
                                   (find-max-user (rest locuwfc) follower-cnt ch-user)
                                   (find-max-user (rest locuwfc) msf mcusf)))]))]
              (find-max-user locuwfc0 first-follower-count first-user)))

          (define global-user-with-followers-list (get-global-user-with-followers-list locu0 empty))]
    (if (empty? global-user-with-followers-list)
        ""
        (chirper-user-name (extract-max-follower-user global-user-with-followers-list)))))

;  PROBLEM 2:
;
;  In UBC's version of How to Code, there are often more than 800 students taking
;  the course in any given semester, meaning there are often over 40 Teaching Assistants.
;
;  Designing a schedule for them by hand is hard work - luckily we've learned enough now to write
;  a program to do it for us!
;
;  Below are some data definitions for a simplified version of a TA schedule. There are some
;  number of slots that must be filled, each represented by a natural number. Each TA is
;  available for some of these slots, and has a maximum number of shifts they can work.
;
;  Design a search program that consumes a list of TAs and a list of Slots, and produces one
;  valid schedule where each Slot is assigned to a TA, and no TA is working more than their
;  maximum shifts. If no such schedules exist, produce false.
;
;  You should supplement the given check-expects and remember to follow the recipe!



;; Slot is Natural
;; interp. each TA slot has a number, is the same length, and none overlap

(define-struct ta (name max avail))
;; TA is (make-ta String Natural (listof Slot))
;; interp. the TA's name, number of slots they can work, and slots they're available for

(define SOBA (make-ta "Soba" 2 (list 1 3)))
(define UDON (make-ta "Udon" 1 (list 3 4)))
(define RAMEN (make-ta "Ramen" 1 (list 2)))

(define NOODLE-TAs (list SOBA UDON RAMEN))

(define MIGUEL (make-ta "Miguel" 1 (list 1 2 3 4)))
(define ALICE (make-ta "Alice" 1 (list 1 2)))
(define BOB (make-ta "Bob" 2 (list 3 5 6 7 8)))

(define TAS-1 (list MIGUEL ALICE BOB))

(define-struct assignment (ta slot))
;; Assignment is (make-assignment TA Slot)
;; interp. the TA is assigned to work the slot

;; Schedule is (listof Assignment)


;; ============================= FUNCTIONS


;; (listof TA) (listof Slot) -> Schedule or false
;; produce valid schedule given TAs and Slots; false if impossible

(check-expect (schedule-tas empty empty) empty)
(check-expect (schedule-tas empty (list 1 2)) false)
(check-expect (schedule-tas (list SOBA) empty) empty)

(check-expect (schedule-tas (list SOBA) (list 1)) (list (make-assignment SOBA 1)))
(check-expect (schedule-tas (list SOBA) (list 2)) false)
(check-expect (schedule-tas (list SOBA) (list 1 3)) (list (make-assignment SOBA 3)
                                                          (make-assignment SOBA 1)))

(check-expect (schedule-tas NOODLE-TAs (list 1 2 3 4))
              (list
               (make-assignment UDON 4)
               (make-assignment SOBA 3)
               (make-assignment RAMEN 2)
               (make-assignment SOBA 1)))

(check-expect (schedule-tas NOODLE-TAs (list 1 2 3 4 5)) false)
(check-expect (schedule-tas NOODLE-TAs (list 7)) false)
(check-expect (schedule-tas NOODLE-TAs (list 3 4)) (list (make-assignment UDON 4)
                                                         (make-assignment SOBA 3)))

(check-expect (schedule-tas TAS-1 (list 1 2)) (list (make-assignment ALICE 2)
                                                    (make-assignment MIGUEL 1)))
(check-expect (schedule-tas TAS-1 (list 1 2 8)) (list (make-assignment BOB 8)
                                                      (make-assignment ALICE 2)
                                                      (make-assignment MIGUEL 1)))
(check-expect (schedule-tas TAS-1 (list 1 2 8)) (list (make-assignment BOB 8)
                                                      (make-assignment ALICE 2)
                                                      (make-assignment MIGUEL 1)))
(check-expect (schedule-tas TAS-1 (list 1 2 8 9)) false)
(check-expect (schedule-tas TAS-1 (list 1 2 3)) (list (make-assignment BOB 3)
                                                      (make-assignment ALICE 2)
                                                      (make-assignment MIGUEL 1)))


;(define (schedule-tas tas slots) empty) ;stub


;; template: all selectors for ta, mutual recursion for arbitary-arity tree for multiple potential slots to take,
;; backtracking search when potential slot doesn't work out, accumulator for solution so far,
;; generative recursion - stop trivial case where slots is empty

;; (listof TA) (listof Slot) -> Schedule
;; a list of TAs and a list of Slots, and produces one
;; valid schedule where each Slot is assigned to a TA, and no TA is working more than their
;; maximum shifts. If no such schedules exist, produce false.
(define (schedule-tas tas slots)
  ;;Termination argument:
  ;; trivial case:  slots is empty
  ;; reduction step: remove slot from state when we find one in list of TAs
  ;; argument: the reduction step removes the slots by one, so
  ;;           eventually the slots will be empty or false will be produced if no
  ;;           corresponding slot is found
  (local [(define-struct state (lota slots rsf))
          ;; State is (make-state (listof TA) (listof Slot) (listof Assignment))
          ;; interp. the current possible search state and result so far

          ;; (state-rsf s) is (listof Assignment); a context-preserving accumulator tracking the solution assignments so far
          (define (solve--state s)
            (if (solved? s)
                (state-rsf s)
                (solve--los (next-states s))))

          ;; State -> Boolean
          ;; produces true if a state is solved, otherwise false.
          ;; a state is solved when (state-slots s) is empty
          (define (solved? s)
            (empty? (state-slots s)))

          ;; State -> (listof State)
          ;; given the current state, produce a list of new states.
          ;; Using the first slot from slots, find all possible open slots from tas.
          ;; Remove ta avail slot and decrement max, remove first element of slots,
          ;; and update rsf for each next state
          ;; ASSUME: states will never be empty when this is called
          (define (next-states s)
            (local [(define found-ta-slot (find-next-open-ta-slot (state-lota s) (first (state-slots s))))]
              (if (false? found-ta-slot)
                  empty
                  (cons (make-state (remove-ta-slot (state-lota s) found-ta-slot)
                                    (rest (state-slots s))
                                    (cons found-ta-slot (state-rsf s)))
                        (next-states (make-state (remove-ta-slot (state-lota s) found-ta-slot)
                                                 (state-slots s)
                                                 (state-rsf s)))))))

          ;; (listof TA) Slot -> Assignment or false
          ;; given a list of TAs and a slot, find the first open slot
          ;; and return an assignment, otherwise return false
          (define (find-next-open-ta-slot lota slot)
            (cond [(empty? lota) false]
                  [else
                   (if (and (member? slot (ta-avail (first lota)))
                            (> (ta-max (first lota)) 0))
                       (make-assignment (first lota) slot)
                       (find-next-open-ta-slot (rest lota) slot))]))

          ;; (listof TA) Assignment -> (listof TA)
          ;; produces list of TA but without assignment TA and slot,
          ;; with the max value reduced by 1
          ;; ASSUME: found TA will always have max >= 1
          (define (remove-ta-slot lota a)
            (cond [(empty? lota) empty]
                  [else
                   (if (and (string=? (ta-name (first lota)) (ta-name (assignment-ta a)))
                            (member? (assignment-slot a) (ta-avail (first lota))))
                       (cons (make-ta (ta-name (first lota))
                                      (sub1 (ta-max (first lota)))
                                      (remove (assignment-slot a) (ta-avail (first lota))))
                             (remove-ta-slot (rest lota) a))
                       (cons (first lota)
                             (remove-ta-slot (rest lota) a)))]))

          (define (solve--los los)
            (cond [(empty? los) false]
                  [else
                   (local [(define try (solve--state (first los)))]
                     (if (not (false? try))
                         try
                         (solve--los (rest los))))]))

          ;; Schedule -> Schedule
          ;; given a schedule (listof Assignment), generate
          ;; the same schedule but with original TAs, since the given schedule is assumed to be modified in order
          ;; for the search to work originally. This is just plugging the holes.
          (define (extract-original-tas schedule)
            (cond [(empty? schedule) empty]
                  [else
                   (cons (make-assignment (list-ref tas (find-ta-index (ta-name (assignment-ta (first schedule))) tas))
                                          (assignment-slot (first schedule)))
                         (extract-original-tas (rest schedule)))]))

          ;; String -> Natural
          ;; given a ta name, find the corresponding index in the original list of tas.
          ;; ASSUME: name is guaranteed to be in listof TAs
          (define (find-ta-index name lota)
            (cond [(empty? lota) (error "TA name must exist in tas")]
                  [else
                   (if (string=? name (ta-name (first lota)))
                       0
                       (+ 1 (find-ta-index name (rest lota))))]))

          (define sched (solve--state (make-state tas slots empty)))]
    (if (not (false? sched))
        (extract-original-tas sched)
        false)))



