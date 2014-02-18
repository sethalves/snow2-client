;; -*- scheme -*-

(define-module snow.gauche-filesys-utils
  ;; export sys-symlink from the main gauche module to a place where
  ;; an r7rs library can get at it.
  (export sys-symlink sys-getcwd sys-chdir))
