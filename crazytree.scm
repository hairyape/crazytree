#!/usr/bin/env gosh
(use binary.io)
(use gauche.net)
(use gauche.parameter)
(use gauche.record)
(use gauche.uvector)
(use srfi-60)
(use srfi-98)
(use scheme.char)

(use brain)
(use repl)

(define-record-type world #t #t
  address port name (online-users) maintenance new)

(define-record-type login-data #t #t
  session1 account session2 old-ip last-login gender world)

(define-record-type map-data #t #t
  map-name address port char-id)

(define tmwa-version 0)
(define tmwa-options 0)

(define-syntax expect-u16
  (syntax-rules ()
    ((_ expect) (let ((actual (read-u16)))
                  (unless (= actual expect)
                    (raise (format "expected ~a got ~a"
                                   expect actual)))))))
(define-syntax log
  (syntax-rules ()
    ((_ fmt exp ...)
     (display (format (string-append fmt "\n") exp ...)
              ;; avoid current-output-port because it could be
              ;; redirected to a socket
              (standard-error-port)))))

(define-syntax log-after
  (syntax-rules ()
    ((_ expr fmt ...)
     (let ((result expr))
       (log fmt ...)
       result))))

;;; (connect address port . body)
;;;
;;; make a connection to the given address:port and evaluates body
;;; with current ports already set to the socket, and default endian
;;; little.
(define-syntax connect
  (syntax-rules ()
    ((_ address port . body)
     (call-with-client-socket
        (make-client-socket 'inet address port)
      (lambda (in out)
        (parameterize ((default-endian 'little-endian)
                       (current-input-port in)
                       (current-output-port out))
          (begin . body)))))))

;;; generate a loop that calls read-key, then dispatch it to the
;;; proper handler based on the value. Each handler is in form
;;; (key (func ((arg1 expr) (arg2 expr)))), where func has
;;; form (func arg1 arg2)
(define-macro (dispatch read-key . handlers)
  `(let loop ((key (,read-key)))
     (case key
       ,@(map (lambda (handler)
                (let ((key (car handler))
                      (func (caadr handler))
                      (args (cadr (cadr handler))))
                  `((,key) (let* ,args
                             (let ((result (,func ,@(map car args))))
                               (if (eq? result 'cont)
                                      (loop (,read-key))
                                      result))))))
              handlers)
       (else (raise (format "Unable to handle ~a" key))))))

(define (read-code)
  (let ((code (read-u16)))
    (log ">~x" code)
    code))

(define (read-u8v len)
  (read-uvector <u8vector> len))

(define (read-str len)
  (let* ((s (u8vector->string (read-u8v len)))
         (pos (string-scan s #\null)))
    (if (number? pos)
        (substring s 0 pos)
        s)))

(define (write-str str len)
  (write-uvector
   (string->u8vector
    (format  "~v,,,'\x00a" len str))))

(define (number->v4addr n)
  (define (shr/and x)
    (logand (ash n (- x)) #xff))
  (format "~a.~a.~a.~a"
          (logand n #xff)
          (shr/and 8)
          (shr/and 16)
          (shr/and 24)))

(define (emote-text emote)
  (case emote
    ((1) "Disgust")
    ((2) "Surprise")
    ((3) "Happy")
    ((4) "Sad")
    ((5) "Evil")
    ((6) "Wink")
    ((7) "Angel")
    ((8) "Blush")
    ((9) "Tongue")
    ((10) "Grin")
    ((11) "Upset")
    ((12) "Perturbed")
    ((13) "Blah")
    ((101) "Kitty")
    ((102) "xD")
    ((103) "^.^")
    ((104) "Heart eye")
    ((105) "Gold eye")
    ((106) "Sleepy")
    ((107) "u.u")
    ((108) "-.-'")
    ((109) "Surprised")
    ((110) "Dead")
    ((111) "Look away")
    ((112) "Sad")
    ((113) "Palmhead")
    ((114) "Evil")
    ((115) "Angry")
    ((116) "Purple Sad")
    ((117) "Insult Buble")
    ((118) "Heart")
    ((119) "Emote")
    ((120) "Pumpkin")
    ((121) "Evil")
    ((122) "Epic")
    ((123) "Bad geek")
    ((124) "Mimi")
    ((125) "Alien")
    ((126) "Troll")
    ((127) "Metal")
    ((128) "Crying")
    (else (format "~a" emote))))

(define (login-error-text code)
  (case code
    ((0) "Unregistered ID.")
    ((1) "Wrong password.")
    ((2) "Account expired.")
    ((3) "Rejected from server.")
    ((4) (string-append
          "You have been permanently banned from the game. "
          "Please contact the GM team."))
    ((5) "Client too old.")
    ((6) (format
          (string-append
           "You have been temporarily banned from the game "
           "until ~a.\nPlease contact the GM team via the forums.")
          date))
    ((7) "Server overpopulated.")
    ((9) "This user name is already taken.")
    ((10) "Wrong name.")
    ((11) "Incorrect email.")
    ((99) "Username permanently erased.")
    (else (format "Unknown error ~a" code))))

(define (chat-message msg)
  (let* ((str (format "CrazyTree : ~a" msg))
         (len (+ (string-length str) 1)))
    (log "<~a" str)
    (sys-sleep 1)
    (write-u16 #x8c)
    (write-u16 (+ len 4))
    (write-str str len)
    (flush)))

;;; Being handling
(define being (make-hash-table 'eqv?))

;;; actual hash table update is done when we get being-name-response
(define (add-being id job)
  (if (and (< id 110000000)
           (or (<= job 25)
               (and (>= job 4001)
                    (<= job 4049))))
      (unless (hash-table-exists? being id)
        (log "adding being ~a job ~a" id job)
        (write-u16 #x94)
        (write-u32 id)
        (flush))))

(define (delete-being id)
  (log "delete being ~a" (being-name id))
  (hash-table-delete! being id))

(define (being-name id)
  (hash-table-get being id id))

(define (being-action u8v)
  'cont)

(define (being-change-direction u8v)
  'cont)

(define (being-change-looks u8v)
  'cont)

(define (being-change-looks-2 u8v)
  'cont)

(define late-id 0)
(define late-msg "")

(define (cleanup-message msg)
  (let* ((s1 (string-downcase msg))
         (s2 (regexp-replace-all "crazytree" s1 "tree"))
         (s3 (regexp-replace-all #/##./ s2 "")))
    s3))

(define (being-chat len id msg)
  (log "~a> ~a" (being-name id) msg)
  (if (hash-table-exists? being id)
      (let* ((sender (hash-table-get being id))
             (reply (say-something (cleanup-message msg) sender)))
        (if (string? reply)
            (chat-message reply)))
      (begin
        (set! late-id id)
        (set! late-msg msg)
        (add-being id 1)))
  'cont)

(define (being-emotion id emote)
  (log "~a> (emote) ~a" (being-name id) (emote-text emote))
  'cont)

(define (being-move id speed stun-mode status-effects options job rest)
  (log "being-move id ~a" (being-name id))
  (add-being id job)
  'cont)

(define (being-move-2 u8v)
  'cont)

(define (being-name-response id name)
  (log "ID ~a => name ~a" id name)
  (hash-table-put! being id name)
  (if (= id late-id)
      (begin
        (being-chat 0 late-id late-msg)
        (set! late-id 0)))
  'cont)

(define (being-remove id dead-flag)
  (log "ID ~a remove ~a" (being-name id) dead-flag)
  (unless (= dead-flag 1)
    (delete-being id))
  'cont)

(define (being-resurrect u8v)
  'cont)

(define (being-self-effect u8v)
  'cont)

(define (being-spawn u8v)
  'cont)

(define (being-status-change u8v)
  'cont)

(define (being-visible id speed stun-mode status-effects options job rest)
  (log "being ~a is visible" (being-name id))
  (add-being id job)
  'cont)

;;; Player handling
(define (player-arrow-equip u8v)
  'cont)

(define (player-arrow-message u8v)
  'cont)

(define (player-attack-range u8v)
  'cont)

(define (player-chat len msg)
  (log "> ~a" msg)
  'cont)

(define (player-equip u8v)
  'cont)

(define (player-equipment len remaining)
  'cont)

(define (player-guild-party-info rest)
  'cont)

(define (player-guild-party-info u8v)
  'cont)

(define (player-inventory len remaining)
  'cont)

(define (player-inventory-use u8v)
  'cont)

(define (player-move id speed stun-mode status-effects options job rest)
  (log "player-move id ~a job ~a" (being-name id) job)
  (add-being id job)
  'cont)

(define (player-move-to-attack u8v)
  'cont)

(define (player-skills len remaining)
  'cont)

(define (player-stat-update-1 u8v)
  'cont)

(define (player-stat-update-2 u8v)
  'cont)

(define (player-stat-update-3 u8v)
  'cont)

(define (player-stat-update-4 u8v)
  'cont)

(define (player-stat-update-5 u8v)
  'cont)

(define (player-stat-update-6 u8v)
  'cont)

(define (player-status-change u8v)
  'cont)

(define (player-stop u8v)
  'cont)

(define (player-warp u8v)
  'cont)

(define (player-unequip u8v)
  'cont)

(define (player-update-1 id speed stun-mode status-effects options job remaining)
  (log "player-update-1 id ~a job ~a" (being-name id) job)
  (add-being id job)
  'cont)

(define (player-update-2 id speed stun-mode status-effects options job rest)
  (log "player-update-2 id ~a job ~a" (being-name id) job)
  (add-being id job)
  'cont)

;;; Misc
(define (gm-chat len msg)
  (log "GM> ~a"  msg)
  'cont)

(define (item-dropped u8v)
  'cont)

(define (item-remove u8v)
  'cont)

(define (item-visible u8v)
  'cont)

(define (party-info len name remaining)
  'cont)

(define (party-invited u8v)
  'cont)

(define (party-update-hp u8v)
  'cont)

(define (party-update-coords u8v)
  'cont)

(define (trade-request u8v)
  (write-u16 #xe7)
  (write-u8 4)                          ; reject
  (flush)
  'cont)

(define (whisper len nick msg)
  (log "~a> ~a" nick msg)
  'cont)

(define (whisper-response u8v)
  'cont)

;;; login server handling
(define (request-version)
  (write-u16 #x7530)
  (flush))

(define (request-version-response username password
                                  b1 b2 b3 b4
                                  options)
  (set! tmwa-version (logior (ash b1 16) (ash b2 8) b3))
  (set! tmwa-options options)
  (login-request username password)
  'cont)

(define (login-request username password)
  (write-u16 #x64)
  (write-u32 0)                       ; client version
  (write-str username 24)
  (write-str password 24)
  (write-u8 3)                        ; flags
  (flush))

(define (update-host len host)
  (log "Update host ~a" host)
  'cont)

(define (read-world fake-id)
  (let* ((address (number->v4addr (read-u32)))
         (port (read-u16))
         (name (read-str 20))
         (online-users (read-u16))
         (maintenance (read-u16))
         (new (read-u16)))
    (make-world address port name online-users maintenance name)))

(define (login-response len session1 account session2 old-ip
                         last-login unused gender nr-worlds
                         worlds)
  (make-login-data session1 account session2 old-ip
                   last-login gender worlds))

;;; char server handling
(define (connect-char-server account session1 session2)
  (write-u16 #x65)
  (write-u32 account)
  (write-u32 session1)
  (write-u32 session2)
  (write-u16 1)                         ; CLIENT_TMW_PROTOCOL_VERSION
  (write-u8 1)                          ; gender
  (flush))

(define (connect-char-response)
  (define (read-char fake-id)
    (read-u8v 106))

  (read-u32) ; manaplus: we get 4 useless bytes before the real answer comes in
  (expect-u16 #x6b)
  (let* ((len (read-u16))
         (slots (read-u16))
         (version (read-u8))
         (skip (read-u8v 17))
         (nr-chars (/ (- len 24) 106))
         (chars (map read-char (make-list nr-chars))))
    chars))

(define (select-char slot)
  (write-u16 #x66)
  (write-u8 slot)
  (flush))

(define (char-map-info)
  (expect-u16 #x71)
  (let* ((char-id (read-u32))
         (map-name (read-str 16))
         (address (number->v4addr (read-u32)))
         (port (read-u16)))
    (make-map-data map-name address port char-id)))

;;; map server handling
(define (connect-map-server account session1 session2 char-id)
  (write-u16 #x72)
  (write-u32 account)
  (write-u32 char-id)
  (write-u32 session1)
  (write-u32 session2)
  (write-u8 1)                          ; gender
  (flush)
  (read-u32) ; manaplus: we get 4 useless bytes before the real answer comes in
  (expect-u16 #x73)
  (read-u32)                            ; tick
  (read-u8v 3)          ; coordinates
  (read-u16))                           ; unknown?

(define (map-loaded)
  (write-u16 #x7d)
  (flush))

(define (login-handler username password)
  (request-version)
  (dispatch read-code
            (#x7531
             (request-version-response ((username username)
                                        (password password)
                                        (b1 (read-u8))
                                        (b2 (read-u8))
                                        (b3 (read-u8))
                                        (b4 (read-u8))
                                        (options (read-u32)))))
            (#x63
             (update-host ((len (read-u16))
                           (host (read-str (- len 4))))))
            (#x69
             (login-response ((len (read-u16))
                              (session1 (read-u32))
                              (account (read-u32))
                              (session2 (read-u32))
                              (old-ip (read-u32))
                              (last-login (read-str 24))
                              (unused (read-u16))
                              (gender (read-u8))
                              (nr-worlds (/ (- len 47) 32))
                              (worlds (map read-world
                                           (make-list nr-worlds))))))
            (#x6a
             (login-error ((code (read-u8))
                           (date (read-str 20))
                           (text (login-error-text code)))))))

(define (char-handler login-data slot)
  (connect-char-server (login-data-account login-data)
                       (login-data-session1 login-data)
                       (login-data-session2 login-data))
  (connect-char-response)
  (select-char slot)
  (char-map-info))

(define (run-client)
  (dispatch read-u16
            (#x08a (being-action ((u8v (read-u8v 27)))))
            (#x09c (being-change-direction ((u8v (read-u8v 7)))))
            (#x0c3 (being-change-looks ((u8v (read-u8v 6)))))
            (#x1d7 (being-change-looks-2 ((u8v (read-u8v 9)))))
            (#x08d (being-chat ((len (read-u16))
                                (id (read-u32))
                                (msg (read-str (- len 8))))))
            (#x0c0 (being-emotion ((id (read-u32))
                                   (emote (read-u8)))))
            (#x07b (being-move ((id (read-u32))
                                (speed (read-u16))
                                (stun-mode (read-u16))
                                (status-effects (read-u16))
                                (options (read-u16))
                                (job (read-u16))
                                (rest (read-u8v 44)))))
            (#x086 (being-move-2 ((u8v (read-u8v 14)))))
            (#x095 (being-name-response ((id (read-u32))
                                         (name (read-str 24)))))
            (#x080 (being-remove ((id (read-u32))
                                  (dead-flag (read-u8)))))
            (#x148 (being-resurrect ((u8v (read-u8v 6)))))
            (#x19b (being-self-effect ((u8v (read-u8v 8)))))
            (#x07c (being-spawn ((u8v (read-u8v 39)))))
            (#x196 (being-status-change ((u8v (read-u8v 7)))))
            (#x078 (being-visible ((id (read-u32))
                                   (speed (read-u16))
                                   (stun-mode (read-u16))
                                   (status-effects (read-u16))
                                   (options (read-u16))
                                   (job (read-u16))
                                   (rest (read-u8v 38)))))

            (#x13c (player-arrow-equip ((u8v (read-u8v 2)))))
            (#x13b (player-arrow-message ((u8v (read-u8v 2)))))
            (#x13a (player-attack-range ((u8v (read-u8v 2)))))
            (#x08e (player-chat ((len (read-u16))
                                 (msg (read-str (- len 4))))))
            (#x0aa (player-equip ((u8v (read-u8v 5)))))
            (#x0a4 (player-equipment ((len (read-u16))
                                      (remaining (read-u8v (- len 4))))))
            (#x195 (player-guild-party-info ((u8v (read-u8v 100)))))
            (#x1ee (player-inventory ((len (read-u16))
                                      (remaining (read-u8v (- len 4))))))
            (#x1c8 (player-inventory-use ((u8v (read-u8v 11)))))
            (#x1da (player-move ((id (read-u32))
                                 (speed (read-u16))
                                 (stun-mode (read-u16))
                                 (status-effects (read-u16))
                                 (options (read-u16))
                                 (job (read-u16))
                                 (rest (read-u8v 44)))))
            (#x139 (player-move-to-attack ((u8v (read-u8v 14)))))
            (#x10f (player-skills ((len (read-u16))
                                   (remaining (read-u8v (- len 4))))))
            (#x0b0 (player-stat-update-1 ((u8v (read-u8v 6)))))
            (#x0b1 (player-stat-update-2 ((u8v (read-u8v 6)))))
            (#x141 (player-stat-update-3 ((u8v (read-u8v 12)))))
            (#x0bc (player-stat-update-4 ((u8v (read-u8v 4)))))
            (#x0bd (player-stat-update-5 ((u8v (read-u8v 42)))))
            (#x0be (player-stat-update-6 ((u8v (read-u8v 3)))))
            (#x119 (player-status-change ((u8v (read-u8v 11)))))
            (#x088 (player-stop ((u8v (read-u8v 8)))))
            (#x0ac (player-unequip ((u8v (read-u8v 5)))))
            (#x1d8 (player-update-1 ((id (read-u32))
                                     (speed (read-u16))
                                     (stun-mode (read-u16))
                                     (status-effects (read-u16))
                                     (options (read-u16))
                                     (job (read-u16))
                                     (remaining (read-u8v 38)))))
            (#x1d9 (player-update-2 ((id (read-u32))
                                     (speed (read-u16))
                                     (stun-mode (read-u16))
                                     (status-effects (read-u16))
                                     (options (read-u16))
                                     (job (read-u16))
                                     (rest (read-u8v 37)))))
            (#x091 (player-warp ((u8v (read-u8v 20)))))

            (#x09a (gm-chat ((len (read-u16))
                             (msg (read-str (- len 4))))))
            (#x09e (item-dropped ((u8v (read-u8v 15)))))
            (#x0a1 (item-remove ((u8v (read-u8v 4)))))
            (#x09d (item-visible ((u8v (read-u8v 15)))))
            (#x0fb (party-info ((len (read-u16))
                                (name (read-str 24))
                                (remaining (read-u8v (- len 28))))))
            (#x0fe (party-invited ((u8v (read-u8v 28)))))
            (#x107 (party-update-coords ((u8v (read-u8v 8)))))
            (#x106 (party-update-hp ((u8v (read-u8v 8)))))
            (#x0e5 (trade-request ((u8v (read-u8v 24)))))
            (#x097 (whisper ((len (read-u16))
                             (nick (read-str 24))
                             (msg (read-str (- len 28))))))
            (#x098 (whisper-response ((u8v (read-u8v 1)))))))

(define (main args)
  (let ((username "crazytree")
        (password (get-environment-variable "CRAZYPASS")))
    (unless (string? password)
      (raise "$CRAZYPASS not defined"))
    (run-repl-server)
    (let* ((login-data (connect "server.themanaworld.org" 6901
                                (login-handler username password)))
           (first-world (car (login-data-world login-data)))
           (char-slot 0)
           (map-data (connect (world-address first-world) (world-port first-world)
                              (char-handler login-data char-slot))))
      (connect (map-data-address map-data) (map-data-port map-data)
               (connect-map-server (login-data-account login-data)
                                   (login-data-session1 login-data)
                                   (login-data-session2 login-data)
                                   (map-data-char-id map-data))
               (map-loaded)
               (run-client)))))
