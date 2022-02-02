(define (string-first s)
  (substring s 0 1))

(define (string-rest s)
  (substring s 1 (string-length s)))

(define (auxiliary currency solution)
  (cond [(= (string-length currency) 0) solution]
        [(delimater? (string-first currency))
         (cond [(> (string-length currency) 3)
                (auxiliary (string-rest currency)
                           solution)]
               [else (auxiliary (string-rest currency)
                                (string-append solution (string-first currency)))])]
        [else (auxiliary (string-rest currency)
                         (string-append solution (string-first currency)))]))

(define (decode-currency currency)
  (auxiliary currency ""))

(check-expect (decode-currency "1.1") "1.1")
(check-expect (decode-currency "1,234.5") "1234.5")
(check-expect (decode-currency "1,345") "1345")
(check-expect (decode-currency "1,234.56") "1234.56")
(check-expect (decode-currency "1,23456.789") "123456789")
(check-expect (decode-currency "123,456,789.1234,5,6,7,8,1.11") "123456789123456781.11")
(check-expect (decode-currency "1,2.3,4.5,6.7,89") "1234567,89")
(check-expect (decode-currency "123,456.789.1,2.3,4.5,6.7,89") "1234567891234567,89")
