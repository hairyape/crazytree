(use binary.io)
(use gauche.selector)
(use gauche.process)
(use srfi-19)

(define (health-check in)
  (let ((sel (make <selector>))
        (ok #t))
    (selector-add! sel in
                   (lambda (input flag)
                     (if (eof-object? (read-u8 input))
                         (raise "eof"))
                     (set! ok #t))
                   '(r x))
    (let loop ()
      (set! ok #f)
      (selector-select sel '(60 0))
      (if ok
          (loop)
          (raise "Hanging")))))

(define (main args)
  (let loop ()
    (let* ((p (run-process (cdr args) :output :pipe))
           (in (process-output p)))
      (display (format "Started at ~a\n" (current-date))
               (standard-error-port))
      (guard
       (e (else
           (display (format "Error: ~a\n" e) (standard-error-port))
           (process-kill p)
           (process-wait p)))
       (health-check in))
      (display (format "Restarting in 30 seconds from ~a\n" (current-date))
               (standard-error-port))
      (sys-sleep 30)
      (loop))))

;;; Local Variables:
;;; indent-tabs-mode: nil
;;; End:
