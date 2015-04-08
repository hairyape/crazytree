(define-module brain
  (export say-something make-face disengage))

(select-module brain)
(use srfi-27)
(use srfi-13)
(use srfi-19)
(use eliza)
(use config)

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
    "Hello and welcome to the Aperture Science computer-aided enrichment center."
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
    "*drops a magic eightball on ~a's head*"
    "*drops a GM on ~a"
    "*drops a penguin on ~a's head*"
    "*drops a whale on ~a's head*"
    "*drops an elephant on ~a's head*"
    "*drops a piano on ~a's head*"
    "*drops a piece of moon rock on ~a's head*"
    "*drops a pin on ~a's head*"
    "*drops a rock on ~a's head*"
    "*drops a tub of paint on ~a's head*"
    "*drops a wet mop on ~a's head*"
    "*drops some bass on ~a's head*"
    "*drops Voldemort on ~a's head*"
    "*drops a sandworm on ~a"
    "*drops a princess on ~a"
    "*drops a prince on ~a"
    "*drops an idea in ~a's head"
    "*drops The Hitchhiker's Guide to the Galaxy on ~a's head"
    "*drops Luvia on ~a"
    "Hu hu hu.. ~a kicked me!"
    "Ouch.."
    "Ouchy.."
    "*drops dead*"
    "*sighs*"
    "Leaf me alone."
    "Stop it! I doesn't drop branches, try the Druid tree for once!"
    ))

(define *special-drops*
  '(("ShaiN2" . "*drops a nurse on ~a*")
    ("Shainen" . "*drops a nurse on ~a*")
    ("Silent Dawn" . "*drops a box of chocolate on ~a*")
    ("veryape" . "*drops a chest of rares on ~a*")
    ("veryapeGM" . "*drops a chest of rares on ~a*")
    ("Ginaria" . "*drops a bluepar on ~a*")
    ("mahouking" . "*drops 100l of oil and one feather on ~a*")
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
    "Somebody said I'm a Chinese copy of Confused Tree"
    "I am your evil twin."
    "I don't remember anything after I woke up! What happened to me?"
    "I don't know. Why am I here??"
    "Who are you?"
    "On the 8th day, God was bored and said 'There will be bots'. So here I am."
    "♪ I'm your hell, I'm your dream, I'm nothing in between ♪♪"
    "♪♪ Aperture Science. We do what we must, because.. we can ♪"
    "I'm just a reincarnation of a copy."
    ))

(define *jokes*
  '("How did the tree get drunk? On root beer."
    "Do you think I'm lazy?"
    "I miss Confused Tree :("
    "I miss CrazyTree :("
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
    "How do trees get on the internet? They log in."
    "Why did the pine tree get into trouble? Because it was being knotty."
    "Did you hear the one about the oak tree? It's a corn-y one!"
    "What do you call a blonde in a tree with a briefcase? Branch Manager."
    "How is an apple like a lawyer? They both look good hanging from a tree."
    "Why did the sheriff arrest the tree? Because its leaves rustled."
    "I'm too tired, ask someone else."
    "If you are trying to get me to tell jokes you are barking up the wrong tree!"
    "You wodden think they were funny anyhow. Leaf me alone!"
    "What is brown and sticky? A stick."
    ))

(define *burning*
  '("*curses ~a and dies %%c*"
    "Help! I'm on fire!"
    "Oh hot.. hot hot!"
    "*is glowing*"
    "*is flaming*"
    "ehemm. where are firefighters? I need them now!"
    "*is so hot!*"
    ))

(define *no-idea*
  '("what?"
    "what??"
    "what?"
    "whatever"
    "hmm..."
    "hmm..."
    "huh?"
    "*yawns*"
    "Wait a minute.."
    "What are you talking about?"
    "Who are you?"
    "What about me?"
    "I don't know what you are talking about"
    "Excuse me?"
    "very interesting"
    "really?"
    "go on..."
    "*scratches its leafy head*"
    "*feels a disturbance in the force*"
    "%%j"
    "*senses a disturbance in the force*"
    "*humming*"
    "I'm bored.."
    "%%U"
    "%%["
    ))

(define *pain*
  '("Ouch.."
    "Ouchy.."
    "Argh.."
    "Eckk..."
    "*howls*"
    "*screams*"
    "*groans*"
    "*cries*"
    "*faints*"
    "%%k"
    "Why.. What did I do to you? %%i"
    ))

(define *hurt-actions*
  '("eat" "shoot" "pluck" "torture" "slap" "poison"
    "break" "stab" "throw"))

(define *blocked* #f)
(define *emote-ok* #t)

(define *eliza-mode* (make-hash-table 'equal?))

(define *start-time* (current-time))
(define *chat-count* 0)
(define *ignore-count* 0)

(define-syntax rarely
  (syntax-rules ()
    ((_ expr ...)
     (and (= (random-integer 10) 0) expr ...)))) ; 10%

(define-syntax maybe
  (syntax-rules ()
    ((_ expr ...)
     (and (= (random-integer 2) 0) expr ...)))) ; 50%

(define-syntax likely
  (syntax-rules ()
    ((_ expr ...)
     (and (not (= (random-integer 4) 0)) expr ...)))) ; 75%

(define (nick-name speaker)
  (let ((nick (cond
               ((string=? speaker "Chung")
                (maybe (random-from-list '("young" "noob"))))
               ((string=? speaker "mahouking")
                (likely "noob")))))
    (if (string? nick)
        nick
        speaker)))

(define (random-from-list list)
  (list-ref list (random-integer (length list))))

(define (tell-joke)
  (random-from-list *jokes*))

(define (react speaker list)
  (let ((fmt (random-from-list list)))
    (if (string-scan fmt "~a")
        (format fmt speaker)
        fmt)))

(define (one-of str list)
  (find (lambda (x)
          (string-scan str x))
        list))

(define (*say-tree speech speaker)
  (cond
   ((one-of speech '("tell me a joke" "tell a joke"))
    (tell-joke))
   ((string-scan speech "heal me")
    (react speaker *healing*))
   ((or (string-scan speech "what are you")
        (string-scan speech "who are you"))
    (react speaker *whoami*))
   ((one-of speech
            '("hi tree" "hello tree" "hey tree" "heya tree" "hiya tree"))
    (begin
      (set! *blocked* #f)
      (react speaker *greetings*)))
   ((one-of speech
            '("kicks tree" "kick tree" "shake tree" "shakes tree"))
    (if (likely (assoc speaker *special-drops*))
        (format (cdr (assoc speaker *special-drops*)) speaker)
        (react speaker *dropping*)))
   ((one-of speech '("die tree" "*nukes tree" "*nuke tree"
                     "*kill tree" "*kills tree"))
    (react speaker *die*))
   ((one-of speech
            '("pokes tree" "poke tree"))
    "*tickles*")
   ((one-of speech '("water tree" "*pee" "waters tree"
                     "licks tree" "lick tree"))
    "ewwwww %%^")
   ((one-of speech '("burns tree" "burn tree"))
    (react speaker *burning*))
   ((string-scan speech "*cuts")
    (format "*curses ~a and dies %%c*" speaker))
   ((string-scan speech "*bites tree*")
    "hahaha... good one!")
   ((one-of speech '("*loves tree" "*hugs tree"))
    (random-from-list
     '("♪♪ and IIII.. will alwayyyys loooovvve youuuuu ♪♪ %%]"
       "♪♪ nothing's gonna change my love for you, you oughta know by now how much I love you.. ♪ %%]"
       "♪ ..and then I go and spoil it all, by saying something stupid like: \"I love you.\" ♪"
       "♪ ..won't you find a place for me? somewhere in your heart... ♪♪"
       "thank you"
       "♪♪ ..I can't love another when my heart is somewhere far away.. ♪"
       "%%]")))
   ((one-of speech '("*hates tree" "*hate tree"))
    (react speaker '("right back at you!"
                     "ok..."
                     "*pats ~a, let it go..*"
                     "hu hu hu .. ~a hates me")))
   ((one-of speech *hurt-actions*)
    (random-from-list *pain*))
   ((string-scan speech "bye")
    (format "*waves goodbye to ~a in tears, come back soon!*" speaker))
   ((string-scan speech "bad tree")
    (random-from-list '("I'm not bad! You are bad!"
                        "OK I'm bad"
                        "I'm just a littttle bad"
                        "Whisper suggestions to me, maybe I can improve")))
   ((string-scan speech "stop making face")
    (set! *emote-ok* #f)
    "%%S")
   ((string-scan speech "show your face")
    (set! *emote-ok* #t)
    "%%_")
   ((string-scan speech "how old are you")
    (let* ((time (time->seconds
                  (time-difference (current-time) *start-time*)))
           (day-in-secs (* 24 60 60))
           (trunc (lambda (x frac)  ; do I really need to do this manuall??
                    (/ (round (* x frac)) frac)))
           (days (/ time day-in-secs)))
      (format "uptime ~a days" (trunc days 100))))
   ((string-scan speech "how chatty are you")
    (format "answered ~a times, ignored ~a times" *chat-count* *ignore-count*))
   ((string-scan speech "shut up")
    (begin
      (set! *blocked* #t)
      "*goes hide in a corner %%S*"))
   ))

(define (*say-no-tree speech speaker)
  (cond
   ((maybe (one-of speech '("hi all" "hello everyone" "hello all"
                            "hello everybody" "hi everyone" "hey all"
                            "hiya everyone")))
    (react speaker *greetings*))
   ((maybe (string=? speaker "MMH$")
           (string-scan speech "your mmh$ is now open for business"))
    "*wishes MMH$ a good business day*")
   ((maybe (rxmatch #/appy (.*) to all/ speech))
    (let ((match (rxmatch #/appy (.*) to all/ speech)))
      (format "Happy ~a to ~a!" (rxmatch-substring match 1) speaker)))))

(define (*no-idea speech speaker)
  (if (maybe (string-scan speech "tree"))
      (react speaker *no-idea*)
      (inc! *ignore-count*)))

(define (*say-something speech speaker)
  (define (string-or-f str)
    (if (string? str)
        str
        #f))
  (or
   (and (string-scan speech "tree")
        (string-or-f (*say-tree speech speaker)))
   (string-or-f (*say-no-tree speech speaker))
   (string-or-f (*no-idea speech speaker))))

(define (cleanup-message msg)
  ;; reorder for chaining with $
  (define (re regex subst string)
    (regexp-replace-all regex string subst ))
  ($ re (string-downcase charname) "tree"
     $ re #/##./ ""
     $ re (format "~a tree" (string-downcase adjective)) "tree"
     $ string-downcase msg))

(define (say-something speech speaker)
  (let* ((speech (cleanup-message speech))
         (nick (nick-name speaker))
         (was-blocked *blocked*)
         (reply (*say-something speech nick)))
    (cond
     ((and was-blocked *blocked*) #f)
     ((and (string-scan speech "talk to me")
           (string-scan speech "tree")
           (not (hash-table-exists? *eliza-mode* speaker)))
      (hash-table-put! *eliza-mode* speaker #t)
      (format "*puts eliza hat on, say \"bye\" to stop, continue ~a..*"
              nick))
     ((hash-table-exists? *eliza-mode* speaker)
      (let ((response (tell-eliza speech)))
        (if (string-scan response "*is back")
            (begin
              (hash-table-delete! *eliza-mode* speaker)
              ;; "shut up" could set this to #t in *say-something
              (set! *blocked* #f)))
        response))
     ((string? reply)
      (inc! *chat-count*)
      reply))))

(define (disengage name)
  (hash-table-delete! *eliza-mode* name))

(define (*make-face emote)
  (case emote
    ((2) 2)
    ((3) (random-from-list '(3 103)))
    ((5) (random-from-list '(5 7)))
    ((7) (random-from-list '(5 7)))
    ((101) 101)
    ((110) 4)))

(define *lastface* (current-time))

(define (make-face emote)
;  (let time (time->seconds (time-difference (current-time) *lastface*)))
  (if (<
        (time->seconds (time-difference (current-time) *lastface*))
        emote-limit)
    ()
    (if *emote-ok*
      (begin
          (set! *lastface* (current-time))
          (*make-face emote)))))

;;; Local Variables:
;;; indent-tabs-mode: nil
;;; End:
