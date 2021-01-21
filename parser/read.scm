(require easy
         cj-io-util)

(def char= char=?)

(def. (char.double-quote? v) (char= v #\"))
(def. char.symbol-start? char-alpha?)
(def. char.symbol-end? (complement char-alphanumeric?))
(def. (char.semicolon? v) (char= v #\;))
(def. (char.hash? v) (char= v #\#))


(defclass ParseResult
  (defclass (ParseSuccess value
                          rest))
  (defclass ParseFailure ;; XX: add context, i.e. str at the start
                           ;; of the parsing
    (defclass (ParseFailurePrematureEof))
    (defclass (ParseFailureInvalidTokenAfterHash)))
  (defclass (ParseEof)))


;; Normal language:
;;   --tokenizer--> tokens --lexer--> AST -> interpreter| compiler
;;   --------------------------------
;;            parsing
;; Lisp:
;;   --read--> sexprs --lexan/maroexp-->   AST
;;   ---------             -----------
;;    reading        lexical analysis/macro-expansion
;;   ------------------------------------------------
;;            parsing

;; #!key  #!void #\a  #!optional
;; ` '

;; [ ]  #()

;; #u8(12 34)

;; #f #false
;; #t #true

;; (let (alse 1) #false)




(def digit? (both char? char-digit?))

(def (char->decimal [digit? c])
  "Take a char in the digit range ('0'..'9') and return the corresponding number"
  (- (char->integer c) 48))

(TEST
 > (char->decimal #\5)
 5)

(def (eread-number str) -> ParseResult?
  (def (lp sum str)
    (if (.empty? str)
        (ParseSuccess sum str)
        (let (c (.first str))
          (if (char-digit? c)
              (lp (+ (* sum 10) (char->decimal c))
                  (.rest str))
              (ParseSuccess sum str)))))
  (lp 0 str))

(TEST
 > (eread-number "985")
 [(ParseSuccess) 985 ""])


(def (eread-symbol str)
  (def (lp rcs str)
    (def (return)
      (ParseSuccess (string->symbol (.string-reverse rcs)) str))
          
    (return)
    (cons c rcs)

    (return)
    )
  (lp '() str))


;; eread-string: if encountering the end of the string, before seeing
;; the #\" that marks its end, return (ParseFailurePrematureEof)

;; ...  "abc\"cde"fgh ...
;; ...  "abc\ndef" .....
' ;; XXX
(TEST
 > (eread-string "foo\" bar")
 (ParseSuccess "foo" " bar")
 > (eread-string "foo bar")
 (ParseFailurePrematureEof))



(def (skip-comment str) -> string?
     (if (equal? (.first str) #\newline)
         (.rest str)
         (skip-comment (.rest str))))

(TEST
 > (skip-comment "foo bar \n etc.")
 " etc."
 > (skip-comment "foo bar \n ;;etc.")
 " ;;etc.")

(def (eread-boolean str)
  (if (equal? (.first str) #\f)
      (ParseSuccess #f (.rest str)) 
      (if (equal? (.first str) #\t)
          (ParseSuccess #t (.rest str))
          (ParseFailureInvalidTokenAfterHash))))



(def (eread str)
  (pmatch (.first str)
          (.whitespace?  (eread (.rest str)))
          (.digit? (eread-number str))
          (.symbol-start? (eread-symbol str))
          (.double-quote? (eread-string (.rest str)))
          (.semicolon? (eread (skip-comment (.rest str))))
          (.hash? (eread-boolean (.rest str)))))


(TEST
 > (def p (=>* eread))
 > (p "1234")
 [(ParseSuccess) 1234 ""]
 > (p " 1234")
 [(ParseSuccess) 1234 ""]
 ;; XXX re-enable
 ;; > (p " +1234 ")
 ;; [(ParseSuccess) 1234 " "]
 ;; > (p " -1234")
 ;; [(ParseSuccess) -1234 ""]
 ;; > (p "  \"aoeiwagh\"")
 ;; [(ParseSuccess) "aoeiwagh" ""]
 ;; > (p "  aoeiwagh")
 ;; [(ParseSuccess) aoeiwagh ""]
 > (eread "#f")
 [(ParseSuccess) #f ""]
 > (eread "#t")
 [(ParseSuccess) #t ""]
 > (eread "#32523")
 [(ParseFailureInvalidTokenAfterHash)]
 ;;> (eread "#fat")
 ;;[(ParseSuccess) #f "at"] XXX wrong test result
 > (eread "  \n;; hi there \n #f")
 [(ParseSuccess) #f ""]
 )


'(TEST
 > (def p (=>* eread))
 > (p "

 ;; Hi there 123
456 (7 89)


;; and so..
")
 [(ParseSuccess) 456 " (7 89)\n\n\n;; and so..\n"]
 > (p (.rest #))
 [(ParseSuccess) (7 89) "\n\n\n;; and so..\n"]
 > (p (.rest #))
 [(ParseEof)]
 )



(def (slurp path)
  (call-with-input-file (list path: path
                              char-encoding: 'UTF-8)
    (lambda (port)
      (read-line port #f))))

(def (penultimate-test)
  (=> (directory-path-stream "lib")
      (.filter (C .ends-with? _ ".scm"))
      (.for-each (lambda (path)
                   (let ((gamb (call-with-input-file path read-all))
                         (ours (=> (slurp path)
                                   eread
                                   ((lambda (res)
                                      (if (ParseSuccess? res)
                                          (.value res)
                                          (error "parse failure" res)))))))
                     (unless (equal? gamb ours)
                             (error "incorrect parse result")))))))

