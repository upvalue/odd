;; (define (bad-function) (#t))
;; (define bad-function (named-lambda bad-function () (#t))
;; (define bad-function (lambda () #t))
;; (define bad-function (named-lambda bad-function () (#t))

(define (bad-function)
  (#t))

(bad-function)
