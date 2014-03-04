;; -*- scheme -*-

(define-module snow.gauche-extio-utils
  (use gauche.vport)
  (export snow-force-output
          make-virutal-input-port
          )
  ;; (use gauche.net)

  (define (snow-force-output . maybe-port)
    (let ((port (if (null? maybe-port) (current-output-port)
                    (car maybe-port))))
      (flush port)))


  (define (make-virutal-input-port . args)
    (apply make <virtual-input-port> args))

  )
