;; -*- scheme -*-



;; http://trac.sacrideo.us/wg/wiki/NetworkEndpointsCowan
;; http://trac.sacrideo.us/wg/wiki/SettingsListsCowan
;; http://trac.sacrideo.us/wg/wiki/NetworkPortsCowan
;; http://trac.sacrideo.us/wg/wiki/DatagramChannelsCowan

;; http://srfi.schemers.org/srfi-106/srfi-106.html

;; http://synthcode.com/scheme/chibi/lib/chibi/net.html
;; http://practical-scheme.net/gauche/man/gauche-refe_88.html#Networking
;; http://practical-scheme.net/gauche/man/gauche-refe_91.html
;; https://code.google.com/p/foment/wiki/Sockets


(define-library (seth network-socket)
  (export make-network-listener
          socket:ssl-listen
          open-network-server
          socket:ssl-accept
          socket:udp-server

          open-network-client
          socket:make-ssl-client
          socket:make-udp-client
          socket:make-port-pair

          socket:inbound-read-port
          socket:tls-inbound-read-port
          socket:udp-inbound-read-port
          socket:port-pair-inbound-read-port

          socket:outbound-write-port
          socket:tls-outbound-write-port
          socket:udp-outbound-write-port
          socket:port-pair-outbound-write-port

          socket:udp-read-from
          socket:udp-write-to

          socket:send-eof
          socket:port-pair-send-eof

          socket:close
          close-network-listener
          socket:tls-close
          socket:udp-close
          socket:port-pair-close

          network-endpoint-host
          network-endpoint-port
          make-network-endpoint

          socket:local-address
          socket:remote-address

          socket:tls-local-address
          socket:tls-remote-address

          socket:udp-local-address
          socket:udp-remote-address
          )

  (import (scheme base))
  (cond-expand
   (chibi (import (chibi filesystem)
                  (chibi net)
                  (scheme read)
                  (scheme write)
                  ;; (seth port-extras)
                  ))
   (chicken (import (openssl) (udp) (posix) (ports)
                    ;; (tcp6) ;; XXX tcp6 is ignoring timeout parameters
                    (tcp)
                    ))

   (foment
    ;; http://srfi.schemers.org/srfi-106/srfi-106.html
    ;; https://code.google.com/p/foment/wiki/Sockets
    (import (scheme read)
            (scheme write)
            (only (foment base) accept-socket address-family bind-socket
                  connect-socket inet ip-protocol listen-socket make-socket
                  shutdown-socket *shut-rdwr* *shut-wr* socket-close
                  socket-domain socket-output-port stream tcp)
            (snow bytevector)))

   (gauche (import (scheme read) (scheme write)
                   (gauche)
                   (r7rs)
                   (gauche net)
                   (seth gauche-socket-utils)
                   (snow bytevector)
                   ;; (seth port-extras)
                   ))
   (sagittarius
    ;; http://ktakashi.github.io/sagittarius-ref.html#lib.sagittarius.socket
    (import (scheme read)
            (scheme write)
            (only (rnrs)
                  transcoded-port
                  make-transcoder
                  ;; utf-8-codec
                  latin-1-codec
                  eol-style)
            (sagittarius socket)
            (snow bytevector)
            ;; (seth port-extras)
            ))
   )
  (begin

    ;;
    ;; general
    ;;

    (define (settings-list-get key sl default)
      (cadr (or (assq key sl) (list #t default))))


    (cond-expand
     (chicken
      (tcp-read-timeout #f)
      (tcp-write-timeout #f)
      (tcp-accept-timeout #f))
     (else))


    ;; http://trac.sacrideo.us/wg/wiki/NetworkEndpointsCowan
    (cond-expand
     ((or chibi chicken foment sagittarius)
      (define (network-endpoint-host addr) (car addr))
      (define (network-endpoint-port addr) (cadr addr))
      (define (make-network-endpoint hostname port) (list hostname port)))
     (gauche
      (define (network-endpoint-host addr)
        (inet-address->string (sockaddr-addr addr) AF_INET))
      (define (network-endpoint-port addr)
        (sockaddr-port addr))
      (define (make-network-endpoint hostname port)
        ;; (make <sockaddr-in>
        ;;   :host (if hostname hostname :any)
        ;;   :port port)
        (make-sockaddr (if hostname hostname :any) port)
        ))

     (else))


    ;;
    ;; tcp sockets
    ;;

    (cond-expand
     (chibi
      (define (make-network-listener settings-list)
        (let* ((host (settings-list-get 'host settings-list "127.0.0.1"))
               (port (settings-list-get 'port settings-list 0))
               (addr-info (get-address-info
                           host port
                           (make-address-info address-family/inet
                                              socket-type/stream
                                              ip-proto/tcp))))
          (make-listener-socket addr-info 5))))
     (chicken
        (define (make-network-listener settings-list)
          (let* ((host (settings-list-get 'host settings-list "127.0.0.1"))
                 (port (settings-list-get 'port settings-list 0))
                 (local-addr (make-network-endpoint host port)))
            (tcp-listen (network-endpoint-port local-addr)
                        4 (network-endpoint-host local-addr))
            ;; (tcp-listen (network-endpoint-port local-addr) 4)
            ;; (let ((sock (socket af/inet sock/stream)))
            ;;   (socket-bind sock (inet-address "0.0.0.0" port))
            ;;   (socket-listen sock 4)
            ;;   sock)
            )))
     (foment
      (define (make-network-listener settings-list)
        (let* ((host (settings-list-get 'host settings-list "127.0.0.1"))
               (port (settings-list-get 'port settings-list 0))
               (s (make-socket (address-family inet)
                               (socket-domain stream) (ip-protocol tcp))))
          (bind-socket s (if host host "127.0.0.1")
                       (number->string port)
                       (address-family inet) (socket-domain stream)
                       (ip-protocol tcp))
          (listen-socket s)
          s)))
     (gauche
      (define (make-network-listener settings-list)
        (let* ((host (settings-list-get 'host settings-list "127.0.0.1"))
               (port (settings-list-get 'port settings-list 0))
               (local-addr (make-network-endpoint host port)))
          (make-server-socket 'inet (network-endpoint-port local-addr)
                              :reuse-addr? #t))))
     (sagittarius
      (define (make-network-listener settings-list)
        (let* ((host (settings-list-get 'host settings-list "127.0.0.1"))
               (port (settings-list-get 'port settings-list 0)))
          (make-server-socket
           (if (string? port) port (number->string port)))))))

    (cond-expand
     (chibi
      (define (open-network-server listen-sock)
        (let* ((addrinfo (get-address-info "127.0.0.1" 0))
               (fd (accept listen-sock
                           (address-info-address addrinfo)
                           (address-info-address-length addrinfo))))
          (list fd
                (open-input-file-descriptor fd)
                (open-output-file-descriptor fd)))))
     (chicken
      (define (open-network-server listen-sock)
        (let-values (((in-port out-port) (tcp-accept listen-sock)))
          (list in-port out-port))))
     (foment
      (define (open-network-server listen-sock)
        (accept-socket listen-sock)))
     ((or gauche sagittarius)
      (define (open-network-server listen-sock)
        (socket-accept listen-sock))))

    (cond-expand
     (chibi
      (define (close-network-listener sock)
        (close-file-descriptor sock)))
     (chicken
      (define (close-network-listener sock)
        (tcp-close sock)))
     (foment
      (define (close-network-listener sock)
        (shutdown-socket sock *shut-rdwr*)))
     ((or gauche sagittarius)
      (define (close-network-listener sock)
        (socket-close sock))))

    (cond-expand
     (chibi
      ;; (define (open-network-client settings-list)
      ;;   (let* ((host (settings-list-get 'host settings-list #f))
      ;;          (port (settings-list-get 'port settings-list 0))
      ;;          (addr (get-address-info host port))
      ;;          (sock (socket (address-info-family addr)
      ;;                        (address-info-socket-type addr)
      ;;                        (address-info-protocol addr))))
      ;;     (connect sock
      ;;              (address-info-address addr)
      ;;              (address-info-address-length addr))
      ;;     (set-file-descriptor-flags! sock open/non-block)
      ;;     (list sock
      ;;           (open-input-file-descriptor sock #t)
      ;;           (open-output-file-descriptor sock #t))))

      (define (open-network-client settings-list)
        (let ((host (settings-list-get 'host settings-list #f))
              (port (settings-list-get 'port settings-list 0)))
          (open-net-io host port)))
      )
     (chicken
      (define (open-network-client settings-list)
        (let* ((host (settings-list-get 'host settings-list #f))
               (port (settings-list-get 'port settings-list 0)))
          (let-values (((read-port write-port)
                        (tcp-connect host port)))
            (list read-port write-port)))))
     (foment
      (define (open-network-client settings-list)
        (let* ((addr (make-network-endpoint
                      (settings-list-get 'host settings-list #f)
                      (settings-list-get 'port settings-list 0)))
               (host (network-endpoint-host addr))
               (port (network-endpoint-port addr))
               (s (make-socket (address-family inet)
                               (socket-domain stream)
                               (ip-protocol tcp))))
          (connect-socket s host (number->string port)
                          (address-family inet) (socket-domain stream)
                          0 (ip-protocol tcp))
          s)))
     (gauche
      (define (open-network-client settings-list)
        (let* ((addr (make-network-endpoint
                      (settings-list-get 'host settings-list #f)
                      (settings-list-get 'port settings-list 0)))
               (host (network-endpoint-host addr))
               (port (network-endpoint-port addr))
               )
          (make-client-socket host port))))
     (sagittarius
      (define (open-network-client settings-list)
        (let ((host (settings-list-get 'host settings-list #f))
              (port (settings-list-get 'port settings-list 0)))
          (make-client-socket
           host (if (string? port) port (number->string port))))))
     )

    (cond-expand
     (chibi
      (define (socket:outbound-write-port sock)
        (car (cddr sock)))
      (define (socket:inbound-read-port sock)
        (cadr sock)))
     (chicken
      (define (socket:outbound-write-port sock)
        (cadr sock))
      (define (socket:inbound-read-port sock)
        (car sock)))
     (foment
      (define (socket:outbound-write-port sock) sock)
      (define (socket:inbound-read-port sock) sock))
     (gauche
      (define (socket:outbound-write-port sock)
        (socket-output-port sock))
      (define (socket:inbound-read-port sock)
        (socket-input-port sock)))
     (sagittarius
      ;; (define (bin->textual port)
      ;;   (transcoded-port port (make-transcoder
      ;;                          (latin-1-codec)
      ;;                          (eol-style none))))
      ;; (define (socket:outbound-write-port sock)
      ;;   (bin->textual (socket-output-port sock)))

      ;; (define (socket:inbound-read-port sock)
      ;;   (bin->textual (socket-input-port sock)))

      (define (socket:outbound-write-port sock)
        (socket-output-port sock))

      (define (socket:inbound-read-port sock)
        (socket-input-port sock)))

     )

    (cond-expand
     (chibi
      (define (socket:send-eof sock)
        (close-output-port (car (cddr sock)))
        (close-file-descriptor (car sock)) ;; XXX how to cause a shutdown?
        ))
     (chicken
      (define (socket:send-eof sock)
        ;; (tcp-shutdown (cadr sock) tcp/wr)
        (close-output-port (cadr sock))))
     (foment
      (define (socket:send-eof sock)
        (shutdown-socket sock *shut-wr*)))
     ((or gauche sagittarius)
      (define (socket:send-eof sock)
        (socket-shutdown sock SHUT_WR))))

    (cond-expand
     (chibi
      (define (socket:close sock)
        (close-input-port (cadr sock))
        (close-output-port (car (cddr sock)))
        (close-file-descriptor (car sock))))
     (chicken
      (define (socket:close sock)
        (close-input-port (car sock))
        (close-output-port (cadr sock))))
     (foment
      (define (socket:close sock)
        (shutdown-socket sock *shut-rdwr*)))
     ((or gauche sagittarius)
      (define (socket:close sock)
        (socket-close sock))))

    (cond-expand
     ((or chibi foment)
      (define (socket:local-address sock)
        (error "not implemented -- socket:local-address"))
      (define (socket:remote-address sock)
        (error "not implemented -- socket:remote-address")))
     (chicken
      (define (socket:local-address sock)
        (cond ((tcp-listener? sock)
               (make-network-endpoint #f (tcp-listener-port sock)))
              (else
               (let-values
                   (((local-host remote-host) (tcp-addresses (car sock)))
                    ((local-port remote-port) (tcp-port-numbers (car sock))))
                 (make-network-endpoint local-host local-port)))))
      (define (socket:remote-address sock)
        (let-values (((local-host remote-host) (tcp-addresses (cadr sock)))
                     ((local-port remote-port) (tcp-port-numbers (cadr sock))))
          (make-network-endpoint remote-host remote-port))))
     ((or gauche sagittarius)
      (define (socket:local-address sock)
        (socket-getsockname sock))
      (define (socket:remote-address sock)
        (socket-getpeername sock))))

    ;;
    ;; ssl sockets
    ;;

    (cond-expand
     ((or chibi foment)
      (define (socket:ssl-listen local-addr)
        (error "not implemented -- socket:ssl-listen")))
     (chicken
      (define (socket:ssl-listen local-addr)
        (let ((listener (ssl-listen (network-endpoint-port local-addr))))
          (ssl-load-certificate-chain! listener "certificate-chain.pem")
          (ssl-load-private-key! listener "private-key.pem")
          listener)))
     ((or gauche sagittarius)
      (define (socket:ssl-listen local-addr)
        (error "not implemented -- socket:ssl-listen"))))

    (cond-expand
     ((or chibi foment)
      (define (socket:ssl-accept listen-sock)
        (error "not implemented -- socket:ssl-accept")))
     (chicken
      (define (socket:ssl-accept listen-sock)
        (let-values (((in-port out-port) (ssl-accept listen-sock)))
          (list in-port out-port))))
     ((or gauche sagittarius)
      (define (socket:ssl-accept listen-sock)
        (error "not implemented -- socket:ssl-accept"))))

    (cond-expand
     ((or chibi foment)
      (define (socket:make-ssl-client local-addr remote-addr)
        (error "not implemented -- socket:make-ssl-client")))
     (chicken
      (define (socket:make-ssl-client local-addr remote-addr)
        (let-values (((insock outsock)
                      (ssl-connect (network-endpoint-host remote-addr)
                                   (network-endpoint-port remote-addr))))
          (list insock outsock))))
     ((or gauche sagittarius)
      ;; (define (socket:make-ssl-client local-addr remote-addr)
      ;;   (subprocess:spawn
      ;;    (list "openssl" "s_client"
      ;;          "-quiet" "-bugs"
      ;;          "-no_ign_eof" "-connect"
      ;;          (string-append
      ;;           (network-endpoint-host remote-addr) ":"
      ;;           (number->string (network-endpoint-port remote-addr))))))
      (define (socket:make-ssl-client local-addr remote-addr)
        (error "not implemented -- socket:make-ssl-client"))))

    (cond-expand
     ((or chibi foment)
      (define (socket:tls-outbound-write-port sock)
        (error "not implemented -- socket:tls-outbound-write-port"))
      (define (socket:tls-inbound-read-port sock)
        (error "not implemented -- socket:tls-inbound-read-port")))
     (chicken
      (define (socket:tls-outbound-write-port sock)
        (car sock))
      (define (socket:tls-inbound-read-port sock)
        (cadr sock)))
     ((or gauche sagittarius)
      (define (socket:tls-outbound-write-port sock)
        (car sock))
      (define (socket:tls-inbound-read-port sock)
        (cadr sock))))

    (cond-expand
     ((or chibi foment)
      (define (socket:tls-close sock session)
        (error "not implemented -- socket:tls-close")))
     (chicken
      (define (socket:tls-close sock session)
        (close-input-port (ssl-port->tcp-port (car sock)))
        (close-output-port (ssl-port->tcp-port (cadr sock)))))
     ((or gauche sagittarius)
      (define (socket:tls-close sock)
        (close-input-port (car sock))
        (close-output-port (cadr sock)))))

    (cond-expand
     ((or chibi chicken foment)
      (define (socket:tls-local-address sock)
        (make-network-endpoint #f 0))
      (define (socket:tls-remote-address sock)
        (make-network-endpoint #f 0)))
     (chicken
      (define (socket:tls-local-address sock)))
     ((or gauche sagittarius)
      (define (socket:tls-local-address sock)
        (make-network-endpoint #f 0))
      (define (socket:tls-remote-address sock)
        (make-network-endpoint #f 0))))



    ;;
    ;; udp sockets
    ;;

    (cond-expand
     (chibi
      (define (socket:udp-server local-addr)
        (let* ((addr-info (get-address-info
                           (network-endpoint-host local-addr)
                           (network-endpoint-port local-addr)
                           (make-address-info address-family/inet
                                              socket-type/datagram
                                              ip-proto/udp)))
               (sock (socket (address-info-family addr-info)
                             (address-info-socket-type addr-info)
                             (address-info-protocol addr-info))))
          (cond
           ((not sock)
            (error "couldn't create socket"))
           ((not (set-socket-option! sock level/socket socket-opt/reuseaddr 1))
            (error "couldn't set the socket to be reusable" addr-info))
           ((not (bind sock
                       (address-info-address addr-info)
                       (address-info-address-length addr-info)))
            (close-file-descriptor sock)
            (error "couldn't bind socket" sock addr-info))
           (else
            ;; sock
            (list sock
                  (open-input-file-descriptor sock #t)
                  (open-output-file-descriptor sock #t))
            )))))
     (chicken
      (define (socket:udp-server local-addr)
        (let ((sock (udp-open-socket)))
          (udp-bind! sock
                     (network-endpoint-host local-addr)
                     (network-endpoint-port local-addr))
          sock)
        ))
     (foment
      (define (socket:udp-server local-addr)
        (error "not implemented -- socket:udp-server")))
     ((or gauche sagittarius)
      (define (socket:udp-server local-addr)
        (let ((sock (make-socket PF_INET SOCK_DGRAM)))
          (socket-setsockopt sock SOL_SOCKET SO_REUSEADDR 1)
          (socket-bind sock local-addr)
          sock))))

    (cond-expand
     (chibi
      (define (socket:make-udp-client local-addr remote-addr)
        (let ((addr (get-address-info (network-endpoint-host remote-addr)
                                      (network-endpoint-port remote-addr)
                                      (make-address-info address-family/inet
                                                         socket-type/datagram
                                                         ip-proto/udp))))
          (let ((sock (socket
                       (address-info-family addr)
                       (address-info-socket-type addr)
                       (address-info-protocol addr))))
            (connect sock
                     (address-info-address addr)
                     (address-info-address-length addr))
            (set-file-descriptor-flags! sock open/non-block)
            (list sock
                  (open-input-file-descriptor sock #t)
                  (open-output-file-descriptor sock #t))))))
     (chicken
      (define (socket:make-udp-client local-addr remote-addr)
        (let ((sock (udp-open-socket)))
          (udp-bind! sock #f (network-endpoint-port local-addr))
          (udp-connect! sock
                        (network-endpoint-host remote-addr)
                        (network-endpoint-port remote-addr))
          sock)))
     (foment
      (define (socket:make-udp-client local-addr remote-addr)
        (error "not implemented -- socket:make-udp-client")))
     ((or gauche sagittarius)
      (define (socket:make-udp-client local-addr remote-addr)
        (let ((sock (make-socket PF_INET SOCK_DGRAM)))
          (socket-bind sock local-addr)
          (socket-setsockopt sock SOL_SOCKET SO_REUSEADDR 1)
          (socket-connect sock remote-addr)
          sock))))

    (cond-expand
     (chibi
      (define (socket:udp-outbound-write-port sock)
        (car (cddr sock)))
      (define (socket:udp-inbound-read-port sock)
        (cadr sock)))
     (chicken
      (define (socket:udp-outbound-write-port sock)
        (make-output-port
         (lambda (data) (udp-send sock data)) ;; write
         (lambda () #t))) ;; close
      (define (socket:udp-inbound-read-port sock)
        (make-input-port
         (let ((buffer '()))
           (lambda () ;; read
             (cond ((null? buffer)
                    ;; (receive
                    ;;  (n data from-host from-port)
                    ;;  (udp-recvfrom sock 3000)
                    ;;  (set! buffer (string->list (substring data 0 n))))
                    (let-values (((n data from-host from-port)
                                  (udp-recvfrom sock 3000)))
                      (set! buffer (string->list (substring data 0 n))))
                    ))
             (let ((result (car buffer)))
               (set! buffer (cdr buffer))
               result)))
         (lambda () #t) ;; ready?
         (lambda () #t)))) ;; close
     (foment
      (define (socket:udp-outbound-write-port sock) sock)
      (define (socket:udp-inbound-read-port sock) sock))
     ((or gauche sagittarius)
      (define (socket:udp-outbound-write-port sock)
        (socket-output-port sock))
      (define (socket:udp-inbound-read-port sock)
        (socket-input-port sock))))

    (cond-expand
     ((or chibi foment)
      (define (socket:udp-read-from sock)
        (error "not implemented -- socket:udp-read-from"))
      (define (socket:udp-write-to data sock remote-addr)
        (error "not implemented -- socket:udp-write-to")))
     (chicken
      (define (socket:udp-read-from sock)
        ;; (receive (n data from-host from-port)
        ;;          (udp-recvfrom sock 3000)
        ;;          (list data (make-network-endpoint from-host from-port)))
        (let-values (((n data from-host from-port)
                      (udp-recvfrom sock 3000)))
          (list data (make-network-endpoint from-host from-port)))
        )
      (define (socket:udp-write-to data sock remote-addr)
        (udp-sendto sock
                    (network-endpoint-host remote-addr)
                    (network-endpoint-port remote-addr)
                    data)))
     (gauche
      (define (socket:udp-read-from sock)
        (let* ((buff (make-bytevector 3000))
               (addr
                ;; (make <sockaddr-in>)
                (make-sockaddr :any 0)
                ))
          ;; (receive (how-many remote-addr)
          ;;          (socket-recvfrom! sock buff (list addr))
          ;;          (list (bytevector-copy-partial buff 0 how-many)
          ;;                remote-addr))
          (let-values (((how-many remote-addr)
                        (socket-recvfrom! sock buff (list addr))))
            (list (bytevector-copy-partial buff 0 how-many) remote-addr))))
      (define (socket:udp-write-to data sock destination-addr)
        (socket-sendto sock data destination-addr)))

     (sagittarius
      (define (socket:udp-read-from sock)
        (error "not implemented -- socket:udp-read-from"))
      (define (socket:udp-write-to data sock destination-addr)
        (error "not implemented -- socket:udp-write-to"))))


    (cond-expand
     (chibi
      (define (socket:udp-close sock)
        (close-input-port (cadr sock))
        (close-output-port (car (cddr sock)))
        (close-file-descriptor (car sock))))
     (chicken
      (define (socket:udp-close sock)
        (udp-close-socket sock)))
     (foment
      (define (socket:udp-close sock)
        (shutdown-socket sock *shut-rdwr*)))
     ((or gauche sagittarius)
      (define (socket:udp-close sock)
        (socket-close sock))))

    (cond-expand
     (chibi
      (define (socket:udp-local-address sock)
        (error "not implemented -- socket:udp-local-address")))
     (chicken
      (define (socket:udp-local-address sock)
        (cond ((and (udp-socket? (car sock))
                    (udp-bound? (car sock)))
               (make-network-endpoint #f (udp-bound-port (car sock))))
              (else (make-network-endpoint #f 0)))))
     (foment
      (define (socket:udp-local-address sock)
        (error "not implemented -- socket:udp-local-address")))
     ((or gauche sagittarius)
      (define (socket:udp-local-address sock)
        (socket-getsockname sock))))

    (cond-expand
     (chibi
      (define (socket:udp-remote-address sock)
        (error "not implemented -- socket:udp-remote-address")))
     (chicken
      (define (socket:udp-remote-address sock)
        (make-network-endpoint #f 0)))
     (foment
      (define (socket:udp-remote-address sock)
        (error "not implemented -- socket:udp-remote-address")))
     ((or gauche sagittarius)
      (define (socket:udp-remote-address sock)
        (socket-getpeername sock))))

    ;;
    ;; port pairs
    ;;

    (cond-expand
     ((or chibi chicken foment gauche sagittarius)
      (define (socket:make-port-pair readable-port writable-port)
        (list readable-port writable-port))

      (define (socket:port-pair-outbound-write-port sock)
        (cadr sock))

      (define (socket:port-pair-inbound-read-port sock)
        (car sock))

      (define (socket:port-pair-send-eof sock)
        (close-output-port (cadr sock)))

      (define (socket:port-pair-close sock)
        (close-input-port (car sock))
        (close-output-port (cadr sock)))))

    ))
