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
    "~a!!!!"
    "~a!!!"
    "~a!!"
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
    "Hello ~a! You are looking lovely today!"
    "Welcome back ~a"
    "Welcome back ~a"
    "Welcome back ~a"
    "~a is back!!"
    ))

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
    "*drops a GM on ~a"
    "*drops a penguin on ~a's head*"
    "*drops a piano on ~a's head*"
    "*drops a piece of moon rock on ~a's head*"
    "*drops a pin on ~a's head*"
    "*drops a rock on ~a's head*"
    "*drops a tub of paint on ~a's head*"
    "*drops a wet mop on ~a's head*"
    "*drops some bass on ~a's head*"
    "*drops Voldemort on ~a's head*"
    "Hu hu hu.. ~a kicked me!"
    "Ouch.."
    "Ouchy.."
    "*drops dead*"
    "*sighs*"
    "Leaf me alone."
    ))

(define *special-drops*
  '(("ShaiN2" . "*drops a nurse on ~a*")
    ("Shainen" . "*drops a nurse on ~a*")
    ("Silent Dawn" . "*drops a box of chocolate on ~a*")
    ("veryape" . "*drops a chest of rares on ~a")
    ("veryapeGM" . "*drops a chest of rares on ~a")
    ))

(define *die*
  '("*drops a bomb on ~a's head*"
    "*drops a bowl of petunias on ~a's head*"
    "*drops a drunken pirate on ~a's head*"
    "*drops a freight train on ~a's head*"
    "*drops a mouboo on ~a's head*"
    "*drops an angry cat on ~a's head*"
    "*drops an angry polish spelling of a rare element with the atomic number 78 on ~a's head*"
    "*drops an iten on ~a's head*"
    "*drops a piano on ~a's head*"
    "*drops a piece of moon rock on ~a's head*"
    "*drops Voldemort on ~a's head*"
    "*drops dead*"
    "*sighs*"
    "Avada Kedavra!"
    "Make me!"
    "Never!!"
    "You die, ~a!"
    "You die, ~a!"
    "You die, ~a!"
    "You die, ~a!"
    "No!"
    "In a minute.."
    "Suuure... I'll get right on it"
    ))

(define *healing*
  '("Eat an apple, they're good for you."
    "If I do it for you, then I have to do it for everybody."
    "Oh, go drink a potion or something."
    "Whoops! I lost my spellbook."
    "no mana"
    ))

(define *whoami*
  '("An undercover GM."
    "An exiled GM."
    "I'm not telling you!"
    "I'm a bot! I'll be level 99 one day! Mwahahahaaha!!!111!"
    ))

(define *jokes*
  '("How did the tree get drunk? On root beer."
    "Do you think I'm crazy?"
    "I miss Confused Tree :("
    "I'm not telling you!"
    "*sighs*"
    "If I do it for you, then I have to do it for everybody."
    "What did the beaver say to the tree? It's been nice gnawing you."
    "What did the little tree say to the big tree? Leaf me alone."
    "What did the tree wear to the pool party? Swimming trunks."
    "What do trees give to their dogs? Treets."
    "What do you call a tree that only eats meat? Carniforous."
    "What do you call a tree who's always envious? Evergreen."
    "What is the tree's least favourite month? Sep-timber!"
    "What kind of tree can fit into your hand? A palm-tree."
    "What was the tree's favorite subject in school? Chemistree."
    "Why did the leaf go to the doctor? It was feeling green."
    "Why doesn't the tree need sudo? Because it has root."
    "Why was the cat afraid of the tree? Because of its bark."
    "Why was the tree executed? For treeson."
    ))

(define (rarely) (= (random-integer 10) 0))
(define (maybe) (= (random-integer 2) 0))

(define (tell-joke)
  (list-ref *jokes* (random-integer (length *jokes*))))

(define (react speaker *list*)
  (let ((fmt (list-ref *list* (random-integer (length *list*)))))
    (if (string-scan fmt "~a")
	(format fmt speaker)
	fmt)))

(define (one-of str list)
  (find (lambda (x)
	  (string-scan str x))
	list))

(define (say-something speech speaker)
  (cond
   ((and (string-scan speech "tell me a joke")
	 (string-scan speech "tree"))
    (tell-joke))
   ((and (string-scan speech "heal me")
	 (string-scan speech "tree"))
    (react speaker *healing*))
   ((and (string-scan speech "tree")
	 (string-scan speech "what are you"))
    (react speaker *whoami*))
   ((one-of speech
	    '("hi tree" "hi tree!" "hello tree" "hello tree!"
	      "hey tree" "tree?"))
    (if (and (maybe) (string=? "mahouking" speaker))
	"hi noob"
	(react speaker *greetings*)))
   ((and (maybe)
	 (one-of speech '("hi all" "hello everyone" "hello all"
			  "hello everybody" "hi everyone")))
    (react speaker *greetings*))
   ((one-of speech
	    '("*kicks tree*" "*kick tree*" "shake tree" "shakes tree"))
    (if (and
	 (rarely)
	 (assoc speaker *special-drops*))
	(format (cdr (assoc speaker *special-drops*)) speaker)
	(react speaker *dropping*)))
   ((string-scan speech "die tree")
    (react speaker *die*))
   ((one-of speech
	    '("*pokes tree*" "*poke tree*"))
    "*tickles*")
   ((string-scan speech "*waters tree*")
    "ewwwww")
   ((and (string=? speaker "MMH$")
	 (string-scan speech "Your MMH$ is now OPEN for Business")
	 (maybe))
    "*wishes MMH$ a good business day*")
   ((and (maybe) (rxmatch #/appy (.*) to all/ speech))
    (let ((match (rxmatch #/appy (.*) to all/ speech)))
      (format "Happy ~a to ~a!" (rxmatch-substring match 1) speaker)))
   ((string-scan speech "*burns tree*") (format "*curses ~a and dies*" speaker))
   ((string-scan speech "*bites tree*") "hahaha... good one!")
   ))
