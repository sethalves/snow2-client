;; -*- scheme -*-

(define-module seth.gauche-socket-utils
  (export make-sockaddr)
  (use gauche.net)
  (define (make-sockaddr server port)
    (make <sockaddr-in> :host server :port port)))
