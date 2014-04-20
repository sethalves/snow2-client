;; -*- scheme -*-

(define-module snow.gauche-genport-utils
  (use gauche.vport)
  (export make-virutal-input-port)
  (define (make-virutal-input-port . args)
    (apply make <virtual-input-port> args)))
