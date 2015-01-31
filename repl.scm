(define-module repl
  (export run-repl-server))
(select-module repl)

(use gauche.net)
(use gauche.parameter)
(use gauche.threads)
(use gauche.uvector)
(use srfi-13)

(define *repl-port* 5678)

(define (process-repl sock)
  (call-with-client-socket sock
    (lambda (in out)
      (parameterize ((current-input-port in)
		     (current-output-port out))
	(read-eval-print-loop)))))

(define (run-server port proc)
  (let1 server (make-server-socket 'inet port :reuse-addr? #t)
    (thread-start!
     (make-thread
      (lambda ()
        (while #t
          (let1 client (socket-accept server)
            (thread-start!
             (make-thread
              (lambda () (proc client)))))))))))

(define (run-repl-server)
  (run-server *repl-port* process-repl))
