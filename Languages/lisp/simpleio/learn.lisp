;; write binary data
(with-open-file (out "nice.txt"
		     :direction :output
		     :if-exists :supersede
		     :element-type '(unsigned-byte 8))
  (write-byte #b10101010 out))


;; read byte from left to right

;; byte is a pair -> size, pos
(ldb (byte 3 5) #b10110000)
(ldb (byte 4 2) #b11100000)
(ldb (byte 8 3) #b11110000)
(ldb (byte 8 4) #b11110000)

(ldb (byte 3 1) #b111000)

(ldb (byte 16 16) #xdeadbabe)

