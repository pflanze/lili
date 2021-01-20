(require easy
         cj-io-util)


(defclass ParseResult
  (defclass (ParseSuccess value
                          rest))
  (defclass (ParseFailure) ;; XX: add context, i.e. str at the start
                           ;; of the parsing
    (defclass (ParseFailurePrematureEof)))
  (defclass (ParseEof)))


(def digit? (both char? char-digit?))

(def (char->decimal [digit? c])
  "Take a char in the digit range ('0'..'9') and return the corresponding number"
  (- (char->integer c) 48))

(TEST
 > (char->decimal #\5)
 5)

(def (eread-number str)
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


(def char-symbol-start? char-alpha?)
(def char-symbol-end? (complement char-alphanumeric?))

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

(TEST
 > (eread-string "foo\" bar")
 (ParseSuccess "foo" " bar")
 > (eread-string "foo bar")
 (ParseFailurePrematureEof))



(def (skip-comment str) -> string?
     ....)

(TEST
 > (skip-comment "foo bar \n etc.")
 " etc."
 > (skip-comment "foo bar \n ;;etc.")
 " ;;etc.")


(def (eread str)
  (pmatch (.first str)
          (char-whitespace?  (eread (.rest str)))
          (char-digit? (eread-number str))
          (char-symbol-start? (eread-symbol str))
          ((C char=? _ #\") (eread-string (.rest str)))))




(TEST
 > (def p (=>* eread))
 > (p "1234")
 [(ParseSuccess) 1234 ""]
 > (p " 1234")
 [(ParseSuccess) 1234 ""]
 > (p " +1234 ")
 [(ParseSuccess) 1234 " "]
 > (p " -1234")
 [(ParseSuccess) -1234 ""]
 > (p "  \"aoeiwagh\"")
 [(ParseSuccess) "aoeiwagh" ""]
 > (p "  aoeiwagh")
 [(ParseSuccess) aoeiwagh ""]
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

