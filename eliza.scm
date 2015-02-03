;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eliza-simple.scm
;;
;; derived from code by Peter Norvig published in PARADIGMS OF ARTIFICIAL
;; INTELLIGENCE PROGRAMMING, Morgan Kaufmann publishers, 1992.
;;
;; simplified and modified for class demonstration by Lee Spector
;; (lspector@hampshire.edu), 1994.
;;
;; rewritten for PLT Scheme by Lee Spector
;; (lspector@hampshire.edu), 2009.
;;
;; converted to Gauche Scheme by Nguyễn Thái Ngọc Duy
;; (pclouds@gmail.com), 2015.

#|

This file contains Gauche Scheme source code for a simplified version
of the ELIZA conversation program. The code is adapted from Peter
Norvig's code, but many changes have been made to avoid the use of
advanced constructs and to translate Common Lisp to Scheme. To run the
system evaluate all of the code in this file and then evaluate (eliza).
The dialogue may be terminated either by an explicit abort or by
typing "bye" to the Eliza prompt.

|#

(define-module eliza
  (export tell-eliza))
(select-module eliza)

(use srfi-1)
(use srfi-27)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sample rule set

(define *eliza-rules*
 '(((?*x hello ?*y)
    (How do you do.  Please state your problem.))
   ((?*x computer ?*y)
    (Do computers worry you?)
    (What do you think about machines?)
    (Why do you mention computers?)
    (What do you think machines have to do with your problem?))
   ((?*x name ?*y)
    (I am not interested in names))
   ((?*x sorry ?*y)
    (Please do not apologize)
    (Apologies are not necessary)
    (What feelings do you have when you apologize))
   ((?*x I remember ?*y)
    (Do you often think of ?*y)
    (Does thinking of ?*y bring anything else to mind?)
    (What else do you remember)
    (Why do you recall ?*y right now?)
    (What in the present situation reminds you of ?*y)
    (What is the connection between me and ?*y))
   ((?*x do you remember ?*y)
    (Did you think I would forget ?*y ?)
    (Why do you think I should recall ?*y now)
    (What about ?*y)
    (You mentioned ?*y))
   ((?*x if ?*y)
    (Do you really think its likely that ?*y)
    (Do you wish that ?*y)
    (What do you think about ?*y)
    (Really-- if ?*y))
   ((?*x I dreamt ?*y)
    (Really-- ?*y)
    (Have you ever fantasized ?*y while you were awake?)
    (Have you dreamt ?*y before?))
   ((?*x dream about ?*y)
    (How do you feel about ?*y in reality?))
   ((?*x dream ?*y)
    (What does this dream suggest to you?)
    (Do you dream often?)
    (What persons appear in your dreams?)
    (Do not you believe that dream has to do with your problem?))
   ((?*x my mother ?*y)
    (Who else in your family ?*y)
    (Tell me more about your family))
   ((?*x my father ?*y)
    (Your father)
    (Does he influence you strongly?)
    (What else comes to mind when you think of your father?))
   ((?*x I want ?*y)
    (What would it mean if you got ?*y)
    (Why do you want ?*y)
    (Suppose you got ?*y soon))
   ((?*x I am glad ?*y)
    (How have I helped you to be ?*y)
    (What makes you happy just now)
    (Can you explain why you are suddenly ?*y))
   ((?*x I am sad ?*y)
    (I am sorry to hear you are depressed)
    (I am sure its not pleasant to be sad))
   ((?*x are like ?*y)
    (What resemblance do you see between ?*x and ?*y))
   ((?*x is like ?*y)
    (In what way is it that ?*x is like ?*y)
    (What resemblance do you see?)
    (Could there really be some connection?)
    (How?))
   ((?*x alike ?*y)
    (In what way?)
    (What similarities are there?))
   ((?*x same ?*y)
    (What other connections do you see?))
   ((?*x I was ?*y)
    (Were you really?)
    (Perhaps I already knew you were ?*y)
    (Why do you tell me you were ?*y now?))
   ((?*x was I ?*y)
    (What if you were ?*y ?)
    (Do you thin you were ?*y)
    (What would it mean if you were ?*y))
   ((?*x I am ?*y)
    (In what way are you ?*y)
    (Do you want to be ?*y ?))
   ((?*x am I ?*y)
    (Do you believe you are ?*y)
    (Would you want to be ?*y)
    (You wish I would tell you you are ?*y)
    (What would it mean if you were ?*y))
   ((?*x am ?*y)
    (Why do you say "AM?")
    (I do not understand that))
   ((?*x are you ?*y)
    (Why are you interested in whether I am ?*y or not?)
    (Would you prefer if I were not ?*y)
    (Perhaps I am ?*y in your fantasies))
   ((?*x you are ?*y)
    (What makes you think I am ?*y ?))
   ((?*x because ?*y)
    (Is that the real reason?)
    (What other reasons might there be?)
    (Does that reason seem to explain anything else?))
   ((?*x were you ?*y)
    (Perhaps I was ?*y)
    (What do you think?)
    (What if I had been ?*y))
   ((?*x I can't ?*y)
    (Maybe you could ?*y now)
    (What if you could ?*y ?))
   ((?*x I feel ?*y)
    (Do you often feel ?*y ?))
   ((?*x I felt ?*y)
    (What other feelings do you have?))
   ((?*x I ?*y you ?*z)
    (Perhaps in your fantasy we ?*y each other))
   ((?*x why don't you ?*y)
    (Should you ?*y yourself?)
    (Do you believe I do not ?*y)
    (Perhaps I will ?*y in good time))
   ((?*x yes ?*y)
    (You seem quite positive)
    (You are sure)
    (I understand))
   ((?*x no ?*y)
    (Why not?)
    (You are being a bit negative)
    (Are you saying "NO" just to be negative?))
   ((?*x someone ?*y)
    (Can you be more specific?))
   ((?*x everyone ?*y)
    (surely not everyone)
    (Can you think of anyone in particular?)
    (Who for example?)
    (You are thinking of a special person))
   ((?*x always ?*y)
    (Can you think of a specific example)
    (When?)
    (What incident are you thinking of?)
    (Really-- always))
   ((?*x what ?*y)
    (Why do you ask?)
    (Does that question interest you?)
    (What is it you really want to know?)
    (What do you think?)
    (What comes to your mind when you ask that?))
   ((?*x perhaps ?*y)
    (You do not seem quite certain))
   ((?*x are ?*y)
    (Did you think they might not be ?*y)
    (Possibly they are ?*y))
   ((bye)
    (*is back to being crazy*))
   ((shut up tree)
    (*is back to being crazy*))
   ((shut up)
    (*is back to being crazy*))
   ((?*x)
    (Very interesting)
    (I am not sure I understand you fully)
    (What does that suggest to you?)
    (Please continue)
    (Go on)
    (Do you feel strongly about discussing such things?))))

(define (rest list)
  (if (list? list) (cdr list) #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; variable/binding handling functions
;; (Norvig used association lists and sublis)

;; returns a copy of list with all instances of old replaced with new
(define (subst new old list)
  (cond ((null? list) '())
        ((not (pair? list))
         (if (equal? list old) new list))
        (else (cons (subst new old (car list))
                    (subst new old (cdr list))))))

;; Returns list with substitutions made as indicated in in varval-list.
(define (subvv varval-list list)
  (dolist (varval-pair varval-list)
    (set! list (subst (second varval-pair) (first varval-pair) list)))
  list)

;; Is x a variable (a symbol beginning with `?')?
(define (variable-p x)
  (and (symbol? x)
       (equal? (string-ref (symbol->string x) 0)
               #\?)))

;; Is this a segment-matching pattern: (?*var ...)
(define (segment-pattern-p pat)
  (and (pair? pat)
       (not (pair? (car pat)))
       (>= (string-length (symbol->string (car pat))) 2)
       (equal? (string-ref (symbol->string (car pat)) 0) #\?)
       (equal? (string-ref (symbol->string (car pat)) 1) #\*)))

;; Find a (variable value) pair in a binding list.
(define (get-binding var bindings)
  (cond ((null? bindings) #f)
        ((equal? var (caar bindings))
         (car bindings))
        (else (get-binding var (cdr bindings)))))

;; Get the value part of a single binding.
(define (binding-val binding)
  (cadr binding))

;; Get the value part (for var) from a binding list.
(define (lookup var bindings)
  (binding-val (get-binding var bindings)))

;; Add a (var value) pair to a binding list.
(define (extend-bindings var val bindings)
  (cons (list var val) bindings))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pattern matching

;; Does VAR match input?  Uses (or updates) and returns bindings.
(define (match-variable var input bindings)
  (let ((binding (get-binding var bindings)))
    (cond ((not binding)
           (extend-bindings var input bindings))
          ((equal? input (binding-val binding))
           bindings)
          (else #f))))

;; Match pattern against input in the context of the bindings
(define (pat-match pattern input :optional (bindings '()))
  (cond ((equal? bindings #f) #f)
        ((variable-p pattern)
         (match-variable pattern input bindings))
        ((equal? pattern input) bindings)
        ((segment-pattern-p pattern)
         (segment-match pattern input bindings))
        ((and (pair? pattern) (pair? input))
         (pat-match (rest pattern)
                    (rest input)
                    (pat-match (first pattern)
                               (first input)
                               bindings)))
        (else #f)))

;; Match the segment pattern (?*var remainder) against input.
;; our segment match is not as robust as Norvig's
(define (segment-match pattern input bindings)
  (let ((var (first pattern))
        (remainder (rest pattern)))
    (if (null? remainder)
        (match-variable var input bindings)
        (if (member (first remainder) input)
            (pat-match remainder
                       (member (first remainder) input)
                       (match-variable var
                                       (upto (first remainder) input)
                                       bindings))
            #f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utilities

;; Returns the list up to, but not including the first element that is
;; equal? to the given item.
(define (upto item list)
  (cond ((null? list) '())
        ((equal? item (car list)) '())
        (else (cons (car list) (upto item (cdr list))))))

;; Choose an element from a list at random.
(define (random-elt choices)
  (list-ref choices (random-integer (length choices))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rule access

(define (rule-pattern rule)
  (first rule))

(define (rule-responses rule)
  (rest rule))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; viewpoint switching

;; Change I to you and vice versa, and so on.
;; ours is more complicated than Norvig's because we can't use sublis
;; to do substitutions in parallel
(define (switch-viewpoint words)
  (subvv '((**I you) (**you I) (**me you) (**am are))
         (subvv '((I **I) (you **you) (me **me) (am **am))
                words)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; top level

;; Find some rule with which to transform the input.
(define (use-eliza-rules input)
  (let ((match-result #f)(matching-rule #f))
    ;; find the matching rule
    (dolist (rule *eliza-rules*)
      (unless matching-rule
        (set! match-result
              (pat-match (rule-pattern rule) input))
        (when match-result
          (set! matching-rule rule))))
    ;; return the result of the substitutions
    (subvv (switch-viewpoint match-result)
           (random-elt (rule-responses matching-rule)))))

;; Returns str with punctuation replaced with spaces.
(define (remove-punctuation str)
  (let ((punctuation (string->list ".,;:`!?#-()\\\""))
        (str-list (string->list str)))
    (dolist (c punctuation)
      (set! str-list (subst #\space c str-list)))
    (list->string str-list)))

;; Read an input line, ignoring punctuation.
(define (no-punct input)
  (read (open-input-string
         (string-append "(" (remove-punctuation input) ")"))))

(define (flatten input)
  (define (*flatten input output)
    (cond
     ((null? input)
      output)
     ((pair? input)
      (*flatten (cdr input)
                (*flatten (car input) output)))
     (else
      (cons input output))))
  (reverse (*flatten input '())))

;; Respond to user input using pattern matching rules.
(define (tell-eliza input)
  (let* ((input (no-punct input))
         (response (use-eliza-rules input)))
    (string-join
     (map x->string
          (flatten response)))))

(define (main args)
  (define (get-input)
    (display "eliza> ")
    (flush)
    (read-line))
  (let loop ((input (get-input)))
    (let ((response (tell-eliza input)))
      (print response)
      (flush)
      (unless (string-scan response "*is back")
        (loop (get-input))))))

;;; Local Variables:
;;; indent-tabs-mode: nil
;;; End:
