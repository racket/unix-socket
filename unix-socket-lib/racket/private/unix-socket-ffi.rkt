;; FFI functions, constants, and types for unix domain sockets (unsafe)
#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define)
(provide (protect-out (all-defined-out)))

;; platform : (U 'macosx 'linux #f)
;; Data structures and error handling code differs between the platforms.
(define platform
  (cond
    [(eq? (system-type 'os) 'macosx)
     'macosx]
    [(regexp-match? #rx"^Linux" (system-type 'machine))
     'linux]
    [else
     #f]))

(define unix-socket-available?
  (and platform #t))

;; ========================================
;; Constants

;; linux: bits/socket.h; macosx: sys/socket.h
(define AF-UNIX 1)
(define SOCK-STREAM 1)

;; linux: asm-generic/{errno-base,errno}.h; macosx: sys/errno.h
(define EINTR           4)
(define EAGAIN          (case platform [(linux) 11]  [(macosx) 35]))
(define EWOULDBLOCK     EAGAIN)
(define EINPROGRESS     (case platform [(linux) 115] [(macosx) 36]))

;; linux: asm-generic/fcntl.h; macosx: sys/fcntl.h
(define F_SETFL         4)
(define O_NONBLOCK      (case platform [(linux) #o4000] [(macosx) 4]))

;; linux: asm-generic/socket.h; macosx: sys/socket.h
(define SOL_SOCKET      (case platform [(linux) 1] [(macosx) #xFFFF]))
(define SO_ERROR        (case platform [(linux) 4] [(macosx) #x1007]))

;; linux: sys/un.h; macosx: sys/un.h
(define UNIX-PATH-MAX   (case platform [(linux) 108] [else    104]))

;; linux: bits/sockaddr.h; macosx: sys/un.h
(define _sa_family (case platform [(linux) _ushort] [(macosx) _ubyte]))

;; linux: bits/types.h; macosx: i386/_types.h
(define _socklen_t (case platform [(linux) _uint] [else    _uint32]))

(define-cstruct _linux_sockaddr_un
  ([sun_family _sa_family]
   [sun_path   (make-array-type _byte UNIX-PATH-MAX)]))

(define-cstruct _macosx_sockaddr_un
  ([sun_len    _ubyte]
   [sun_family _sa_family]
   [sun_path   (make-array-type _byte UNIX-PATH-MAX)]))

(define _sockaddr_un-pointer
  (case platform
    [(linux)  _linux_sockaddr_un-pointer]
    [(macosx) _macosx_sockaddr_un-pointer]
    [else     _pointer]))

(define (make-sockaddr path-bytes)
  (case platform
    [(linux)
     (make-linux_sockaddr_un AF-UNIX path-bytes)]
    [(macosx)
     (make-macosx_sockaddr_un (bytes-length path-bytes) AF-UNIX path-bytes)]))

;; ========================================
;; System functions

(define-ffi-definer define-libc (ffi-lib #f)
  #:default-make-fail make-not-available)

(define-libc socket
  (_fun #:save-errno 'posix
        _int _int _int -> _int))

(define-libc connect
  (_fun #:save-errno 'posix
        _int _sockaddr_un-pointer _int -> _int))

(define-libc bind
  (_fun #:save-errno 'posix
        _int _sockaddr_un-pointer _int -> _int))

(define-libc listen
  (_fun #:save-errno 'posix
        _int _int -> _int))

(define-libc accept
  (_fun #:save-errno 'posix
        _int (_pointer = #f) (_pointer = #f)
        -> _int))

(define-libc close
  (_fun #:save-errno 'posix
        _int -> _int))

(define-libc fcntl
  (_fun #:save-errno 'posix
        _int _int _int -> _int))

(define-libc getsockopt
  (_fun #:save-errno 'posix
        _int _int _int (value : (_ptr io _int) = 0) (len : (_ptr io _uint32) = (ctype-sizeof _int))
        -> (result : _int)
        -> (cond [(zero? result)
                   value]
                  [else
                   (error 'getsockopt "error~a" (errno-error-lines (saved-errno)))])))

(define strerror-name
  (case platform
    [(linux) "__xpg_strerror_r"]
    [else    "strerror_r"]))

(define strerror_r
  (get-ffi-obj strerror-name #f
               (_fun (errno) ::
                     (errno : _int)
                     (buf : _bytes = (make-bytes 1000))
                     (buf-len : _size = (bytes-length buf))
                     -> _void
                     -> (cast buf _bytes _string/locale))
               (lambda ()
                 (lambda (errno) #f))))

(define (errno-error-lines errno)
  (define err (strerror_r errno))
  (format "\n  errno: ~a~a" errno (if err (format "\n  error: ~a" err) "")))


;; ========================================
;; Racket constants and functions

(define MZFD_CREATE_READ 1)
(define MZFD_CREATE_WRITE 2)

(define-libc scheme_make_fd_output_port
  (_fun _int _racket _bool _bool _bool -> _racket))

(define-libc scheme_fd_to_semaphore
  (_fun _intptr _int _bool -> _racket))
