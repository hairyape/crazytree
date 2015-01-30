(define-module brain
  (export say-something))

(select-module brain)
(use srfi-27)

(define *greetings*
  '("Hi ~a!"
    "Hi ~a"
    "Hi ~a"
    "Hi ~a"
    "Hey ~a"
    "Hey ~a"
    "Hey ~a"
    "Yo ~a"
    "Yo ~a"
    "Hello ~a"
    "Hello ~a"
    "Hello ~a"
    "Hello ~a"
    "Hello ~a"
    "Hello ~a!"
    "Hello ~a!"
    "Hello ~a!"
    "Hello ~a!"
    "Hello ~a!"
    "Hello ~a! You are looking lovely today!"))

(define *dropping*
  '("*drops a bomb on ~a's head*"
    "*drops a bowl of petunias on ~a's head*"
    "*drops a cake on ~a's head*"
    "*drops a candy on ~a's head*"
    "*drops a chocobo on ~a's head*"
    "*drops a coin on ~a's head*"
    "*drops a cookie on ~a's head*"
    "*drops a drunken pirate on ~a's head*"
    "*drops a freight train on ~a's head*"
    "*drops a fruit on ~a's head*"
    "*drops a mouboo on ~a's head*"
    "*drops an angry cat on ~a's head*"
    "*drops an angry polish spelling of a rare element with the atomic number 78 on ~a's head*"
    "*drops an anvil on ~a's head*"
    "*drops an apple on ~a's head*"
    "*drops an iten on ~a's head*"
    "*drops a penguin on ~a's head*"
    "*drops a piano on ~a's head*"
    "*drops a piece of moon rock on ~a's head*"
    "*drops a pin on ~a's head*"
    "*drops a rock on ~a's head*"
    "*drops a tub of paint on ~a's head*"
    "*drops a wet mop on ~a's head*"
    "*drops some bass on ~a's head*"
    "*drops Voldemort on ~a's head*"))

(define *special-drops*
  '(("ShaiN2" . "*drops a nurse on ~a*")
    ("Shainen" . "*drops a nurse on ~a*")
    ("Silent Dawn" . "*drops a box of chocolate on ~a*")))

(define *jokes*
  '("How did the tree get drunk? On root beer."
    "If I do it for you, then I have to do it for everybody."
    "I'm not telling you!"
    "What did the tree wear to the pool party? Swimming trunks."
    "What do trees give to their dogs? Treets."
    "What do you call a tree that only eats meat? Carniforous."
    "What do you call a tree who's always envious? Evergreen."
    "What is the tree's least favourite month? Sep-timber!"
    "What kind of tree can fit into your hand? A palm-tree."
    "What was the tree's favorite subject in school? Chemistree."
    "Why doesn't the tree need sudo? Because it has root."
    "Why was the cat afraid of the tree? Because of its bark."
    "Why was the tree executed? For treeson."))

(define (tell-joke)
  (list-ref *jokes* (random-integer (length *jokes*))))

(define (react speaker *list*)
  (format (list-ref *list* (random-integer (length *list*))) speaker))

(define (one-of str list)
  (find (lambda (x)
	  (string=? str x))
	list))

(define (say-something speech speaker)
  (cond
   ((one-of speech
	    '("hi tree" "hi tree!" "hello tree" "hello tree!" "hey tree"))
    (react speaker *greetings*))
   ((one-of speech
	    '("*kicks tree*" "*kick tree*"))
    (if (and
	 (< (random-integer 10) 1)
	 (assoc speaker *special-drops*))
	(format (cdr (assoc speaker *special-drops*)) speaker)
	(react speaker *dropping*)))
   ((one-of speech
	    '("*pokes tree*" "*poke tree*"))
    "*tickles*")
   ((string=? speech "*waters tree*")
    "ewwwww")
   ((and (string-scan speech "tell me a joke")
	 (string-scan speech "tree"))
    (tell-joke))))
