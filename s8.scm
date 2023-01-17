(define get-operator (lambda (op-symbol)
  (cond
    ((equal? op-symbol '+) +)
    ((equal? op-symbol '-) -)
    ((equal? op-symbol '*) *)
    ((equal? op-symbol '/) /)
    (else (error "s7-interpret: operator not implemented -->" op-symbol)))))

(define operator? (lambda (s)
  (or (equal? s '+) (equal? '- s) (equal? '* s) (equal? '/ s))
))

(define actuals? (lambda (e)
  (if (equal? e '())
      #t
      (and (expr? (car e)) (actuals? (cdr e)))
    )
))

(define operation?(lambda (e)
		(and (list? e) (>= (length e) 2) (operator? (car e)) (actuals? (cdr e)))
))

(define expr? (lambda (e)
  (cond
    ((symbol? e) #t)
    ((number? e) #t)
    ((if-stmt? e) #t)
    ((cond-stmt? e) #t)
    ((let-stmt? e) #t)
    ((let*-stmt? e) #t)
    ((operation? e) #t)
    (else #f)
  )
))

(define define-stmt? (lambda (e)
    ;; An expression is a define statement
    ;; if it is a list, and the first element
    ;; is define, the second element is a symbol,
    ;; and the third element is an expression.
    (and (list? e) (= (length e) 3) (equal? (car e) 'define) (symbol? (cadr e)) (expr? (caddr e)) )))

(define var-bind-stmt? (lambda (e)
  (if (equal? e '())
    #t
    (and (equal? (length (car e)) 2) (symbol? (caar e)) (expr? (cadar e)) (var-bind-stmt? (cdr e)))
    )
))

(define let*-stmt? (lambda (e)
  (and (list? e) (equal? (length e) 3) (equal? (car e) 'let*) (var-bind-stmt? (cadr e)) (expr? (caddr e)))
))

(define let-stmt? (lambda (e)
  (and (list? e) (equal? (length e) 3) (equal? (car e) 'let) (var-bind-stmt? (cadr e)) (expr? (caddr e)))
))

(define if-stmt? (lambda (e)
    ;;Check if in if format
    (and (list? e) (= (length e) 4) (equal? (car e) 'if) (expr? (cadr e))
      (expr? (caddr e)) (expr? (cadddr e))
    )
    )
)

(define else-cond? (lambda (e)
  (and (list? e) (= (length e) 2) (equal? (car e) 'else) (expr? (cadr e)))
  )
)

(define cond-list? (lambda (e)
  (if (else-cond? (car e))
    (equal? (length e) 1)
    (and (expr? (caar e)) (expr? (cadar e)) (cond-list? (cdr e)))
  )
))

(define cond-stmt? (lambda (e)
    (and (list? e) (>= (length e) 3) (equal? (car e) 'cond)
      (else-cond? (car (reverse e))) (cond-list? (cdr e)))
  )
)

(define get-value (lambda (var env)
    (cond
      ;; If the environment is empty, then we could not find
      ;; a binding in this environment.
      ((null? env) 'ERROR)

      ;; Check if the first pair is a binding for the
      ;; variable that we are looking for, and if it is
      ;; return the current value of the variable.
      ((equal? (caar env) var) (cdar env))

      ;; Otherwise, search in the remaning of the environment.
      (else (get-value var (cdr env))))))

(define extend-env (lambda (var val old-env)
      ;; Add the new variable binding to the
      ;; beginning of the old environment.
      (cons (cons var val) old-env)))

(define repl (lambda (env)
  (let* (
         ; first print out some prompt
         (dummy1 (display "cs305> "))

         ; READ an expression
         (expr (read))

         ; Form the new environment
         (new-env (if (define-stmt? expr)
                      (extend-env (cadr expr) (s7-interpret (caddr expr) env) env)
                      env))

         ; EVALuate the expression read
         (val (if (define-stmt? expr)
                  (cadr expr)
                  (s7-interpret expr env)))

         ; PRINT the value evaluated together
         ; with a prompt as MIT interpreter does
         (dummy2 (display "cs305: "))
         (dummy3 (display val))

         ; get ready for the next prompt
         (dummy4 (newline))
         (dummy4 (newline)))
     (repl new-env))))

(define process-cond (lambda (e env)
    (if (else-cond? (car e))
      (s7-interpret (cadr (car e)) env)
      (if (equal? 0 (s7-interpret (caar e) env))
        (process-cond (cdr e) env)
        (s7-interpret (cadar e) env)
      )
    )
  )
)

(define apply-let* (lambda (e env)
    (if (equal? '() e)
    env
    (apply-let* (cdr e) (extend-env (caar e) (s7-interpret (cadar e) env) env))
    )
))

(define process-let* (lambda (e env)
    (s7-interpret (caddr e) (apply-let* (cadr e) env))
))

(define apply-let (lambda (e env newenv)
    (if (equal? '() e)
    newenv
    (apply-let(cdr e) env (extend-env (caar e) (s7-interpret (cadar e) env) newenv))
    )
))

(define process-let (lambda (e env newenv)
    (s7-interpret (caddr e) (apply-let (cadr e) env newenv))
))

(define s7-interpret (lambda (e env)
  (cond
    ;; If the input expression is a number, then
    ;; the value of the expression is that number.
    ((number? e) e)

    ;; If the input expression is a symbol, then
    ;; get the current value binding of this variable.
    ((symbol? e) (get-value e env))

    ;; Otherwise, we must see a list.
    ((not (list? e)) 'ERROR)

    ;; First evaluate the value of the operands
    ;; and the procedure of the expression.
    (else

      (cond
        ;; is the list an if statement
        ((if-stmt? e)
          (if (equal? 0 (s7-interpret (cadr e) env))
              ;; if zero, eval last expr and return
              (s7-interpret (cadddr e) env)
              ;; else run the then statement
              (s7-interpret (caddr e) env)
          )
        )

        ((cond-stmt? e)
          (process-cond (cdr e) env)
        )

        ((let-stmt? e)
        (process-let e env env)
        )

        ((let*-stmt? e)
        (process-let* e env)
        )

        ((operation? e)
          (let ((operands (map s7-interpret (cdr e) (make-list (length (cdr e)) env)))
                (operator (get-operator (car e))))

            ;; And finally apply the operator to the
            ;; values of the operands
            (apply operator operands))

        )
        (else 'ERROR)
      )


    ))))

(define intr (lambda () (repl '())))
