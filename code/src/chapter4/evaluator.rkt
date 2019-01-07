#lang racket

(require rnrs/mutable-pairs-6)
(require compatibility/mlist)

;; ============
;; MAIN METHODS
;; ============

;; eval
(define (my-eval exp env)
  ((analyze exp) env))

;; analyze
(define (analyze exp)
  (cond [(self-evaluating? exp)
         (analyze-self-evaluating exp)]
        [(variable? exp)
         (analyze-variable exp)]
        [(quote? exp)
         (analyze-quote exp)]
        [(assignment? exp)
         (analyze-assignment exp)]
        [(definition? exp)
         (analyze-definition exp)]
        [(if? exp)
         (analyze-if exp)]
        [(and? exp)
         (analyze (and->if exp))]
        [(or? exp)
         (analyze (or->if exp))]
        [(lambda? exp)
         (analyze-lambda exp)]
        [(begin? exp)
         (analyze-sequence (begin-actions exp))]
        [(cond? exp)
         (analyze (cond->if exp))]
        [(let? exp)
         (analyze (let->application exp))]
        [(let*? exp)
         (analyze (let*->let exp))]
        [(letrec? exp)
         (analyze (letrec->let exp))]
        [(assert? exp)
         (analyze-assert exp)]
        [(application? exp)
         (analyze-application exp)]
        [else
         (error "Unknown type of expression -- analyze " exp)]))

;; ===========
;; EXPRESSIONS
;; ===========

;; self-evaluating
;; format: number, string or ()
(define (self-evaluating? exp)
  (or (number? exp)
      (string? exp)
      (null? exp)))
(define (analyze-self-evaluating exp)
  (lambda (env) exp))

;; variable
;; format: symbol
(define (variable? exp)
  (symbol? exp))
(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))

;; quote
;; format: (quote exp)
(define (quote? exp)
  (tagged-list? exp 'quote))
(define (quote-content exp)
  (cadr exp))
(define (analyze-quote exp)
  (let ((content (quote-content exp)))
    (lambda (env) content)))

;; assignment
;; format: (set! #variable #value)
(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp)
  (cadr exp))
(define (assignment-value-exp exp)
  (caddr exp))
(define (make-assignment variable value)
  (list 'set! variable value))
(define (analyze-assignment exp)
  (let [(var (assignment-variable exp))
        (analyzed-value-exp (analyze (assignment-value-exp exp)))]
    (lambda (env)
      (set-variable-value! var
                           (analyzed-value-exp env)
                           env))))

;; definition
;; format: (define #variable #value)
;; format: (define (#variable #params) #body)
(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value-exp exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))
(define (analyze-definition exp)
  (let ([var (definition-variable exp)]
        [analyzed-value-exp (analyze (definition-value-exp exp))])
    (lambda (env)
      (define-variable! var
        (analyzed-value-exp env)
        env))))

;; condition
;; format: (if predicate consequent alternative)
;; format: (if predicate consequent)
(define (if? exp)
  (tagged-list? exp 'if))
(define (if-predicate exp)
  (cadr exp))
(define (if-consequent exp)
  (caddr exp))
(define (if-have-alternative? exp)
  (not (null? (cdddr exp))))
(define (if-alternative exp)
  (cadddr exp))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))
(define (make-if-no-alternative predicate consequent)
  (list 'if predicate consequent))
(define (analyze-if exp)
  (let ([analyzed-predicate (analyze (if-predicate exp))]
        [analyzed-consequent (analyze (if-consequent exp))])
    (if (if-have-alternative? exp)
        (let ([analyzed-alternative (analyze (if-alternative exp))])
          (lambda (env)
            (if (true? (actual-value (analyzed-predicate env)))
                (analyzed-consequent env)
                (analyzed-alternative env))))
        (lambda (env)
          (if (true? (actual-value (analyzed-predicate env)))
              (analyzed-consequent env)
              'undefined)))))

;; lambda
;; format: (lambda (#param0 (lazy #param1) (lazy-memo #param2) ...) #exp0 #exp1 ...)
(define (lambda? exp)
  (tagged-list? exp 'lambda))
(define (lambda-parameters exp)
  (cadr exp))
(define (lambda-body-exps exp)
  (cddr exp))
(define (make-lambda parameters body-exps)
  (cons 'lambda (cons parameters body-exps)))
(define (analyze-lambda exp)
  (define (extract-parameters parameters)
    (map (lambda (param)
           (cond [(variable? param) (cons 'standard param)]
                 ; param -> (cons 'standard param)
                 [(and (tagged-list? param 'lazy)
                       (= (length param) 2))
                  (cons 'lazy (cadr param))]
                 ; (list 'lazy param) -> (cons 'lazy param)
                 [(and (tagged-list? param 'lazy-memo)
                       (= (length param) 2))
                  (cons 'lazy-memo (cadr param))]
                 ; (list 'lazy-memo param) -> (cons 'lazy-memo param)
                 [else
                  (error "Parameter format error -- analyze-lambda" param)]))
         parameters))
  (let ((extracted-parameters (extract-parameters (lambda-parameters exp)))
        (analyzed-body-exp (analyze-sequence (lambda-body-exps exp))))
    (lambda (env)
      (make-compound-procedure extracted-parameters
                               analyzed-body-exp
                               env))))

;; sequence
;; format: (begin exp0 exp1 ... expn)
(define (begin? exp)
  (tagged-list? exp 'begin))
(define (begin-actions exp)
  (cdr exp))
(define (make-begin exps)
  (cons 'begin exps))
(define (sequence->exp exps)
  (cond [(null? exps)
         (error "At lease one expression expected")]
        [(null? (cdr exps))
         (car exps)]
        [else (make-begin exps)]))
(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env)
      (proc1 env)
      (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc
                            (car rest-procs))
              (cdr rest-procs))))
  (let ((analyzed-exps (map analyze exps)))
    (if (null? analyzed-exps)
        (error "Empty expression sequence -- analyze-sequence")
        (loop (car analyzed-exps) (cdr analyzed-exps)))))

;; application
;; format: (#procedure #argument0 #argument1 ...)
(define (application? exp)
  (pair? exp))
(define (application-procedure-exp application-exp)
  (car application-exp))
(define (application-argument-exps application-exp)
  (cdr application-exp))
(define (analyze-application exp)
  (let [(analyzed-procedure-exp (analyze (application-procedure-exp exp)))
        (analyzed-argument-exps (map analyze (application-argument-exps exp)))]
    (lambda (env)
      (execute-application (actual-value (analyzed-procedure-exp env))
                           analyzed-argument-exps
                           env))))
(define (execute-application procedure-value analyzed-argument-exps env)
  (define (create-arg-bindings parameters arguments)
    (cond [(and (null? parameters)
                (null? arguments))
           '()]
          [(or (null? parameters)
               (null? arguments))
           (error "Argument number mismatch -- execute-application" arguments)]
          [else
           (let* ([first-param (car parameters)]
                  [first-param-rule (car first-param)]
                  [first-param-var (cdr first-param)]
                  [first-arg (car arguments)]
                  [first-value
                   (cond [(eq? first-param-rule 'standard)
                          (first-arg env)]
                         [(eq? first-param-rule 'lazy)
                          (my-delay first-arg env)]
                         [(eq? first-param-rule 'lazy-memo)
                          (my-delay-memo first-arg env)]
                         [else
                          (error "Wrong parameter rule -- execute-application" first-param-rule)])]
                  [first-binding
                   (make-binding first-param-var first-value)])
             (cons first-binding
                   (create-arg-bindings (cdr parameters)
                                        (cdr arguments))))]))
  (cond [(primitive-procedure? procedure-value)
         (apply-primitive-procedure procedure-value
                                    (map (lambda (exp)
                                           (actual-value (exp env)))
                                         analyzed-argument-exps))]
        [(compound-procedure? procedure-value)
         ((compound-procedure-body procedure-value)
          (extend-environment (create-arg-bindings (compound-procedure-parameters procedure-value)
                                                   analyzed-argument-exps)
                              (compound-procedure-environment procedure-value)))]
        [else
         (error "Unknown procedure type -- execute application" procedure-value)]))

;; assert
;; format: (assert #exp)
(define (assert? exp)
  (tagged-list? exp 'assert))
(define (assert-exp exp)
  (cadr exp))
(define (analyze-assert exp)
  (let ([analyzed-exp (analyze (assert-exp exp))])
    (lambda (env)
      (if (true? (actual-value (analyzed-exp env)))
          'undefined
          (error "Assertion failed")))))

;; ===========
;; DERIVATIVES
;; ===========

;; logic
;; format: (and #exp0 #exp1 #exp2 ...)
;; format: (or #exp0 #exp1 #exp2 ...)
(define (and? exp)
  (tagged-list? exp 'and))
(define (or? exp)
  (tagged-list? exp 'or))
(define (logic-clauses exp)
  (cdr exp))
(define (and->if exp)
  (define (expand-clauses clauses)
    (if (null? clauses)
        'true
        (make-if (car clauses)
                 (expand-clauses (cdr clauses))
                 'false)))
  (expand-clauses (logic-clauses exp)))
(define (or->if exp)
  (define (expand-clauses clauses)
    (if (null? clauses)
        'false
        (make-if (car clauses)
                 'true
                 (expand-clauses (cdr clauses)))))
  (expand-clauses (logic-clauses exp)))

;; cond
(define (cond? exp)
  (tagged-list? exp 'cond))
(define (cond-clauses exp)
  (cdr exp))
(define (cond-clause-predicate clause)
  (car clause))
(define (cond-clause-actions clause)
  (cdr clause))
(define (cond-else-clause? clause)
  (eq? (cond-clause-predicate clause)
       'else))
(define (cond->if exp)
  (define (expand-clauses clauses)
    (if (null? clauses)
        (error "Cond must have at least one clause")
        (let ([first (car clauses)]
              [rest (cdr clauses)])
          (cond [(cond-else-clause? first)
                 (if (null? rest)
                     (sequence->exp (cond-clause-actions first))
                     (error "Else clause isn't last clause" first))]
                [(null? rest)
                 (make-if-no-alternative (cond-clause-predicate first)
                                         (sequence->exp (cond-clause-actions first)))]
                [else
                 (make-if (cond-clause-predicate first)
                          (sequence->exp (cond-clause-actions first))
                          (expand-clauses rest))]))))
  (expand-clauses (cond-clauses exp)))

;; let
;; format: (let #bindings #body-exps)
(define (let? exp)
  (tagged-list? exp 'let))
(define (let-bindings exp)
  (cadr exp))
(define (let-body-exps exp)
  (cddr exp))
(define (make-let bindings body)
  (cons 'let (cons bindings body)))
(define (let->application exp)
  (let* ([bindings (let-bindings exp)]
         [body-exps (let-body-exps exp)]
         [parameters (map car bindings)]
         [arguments (map cadr bindings)])
    (cons (make-lambda parameters body-exps)
          arguments)))

;; let*
(define (let*? exp)
  (tagged-list? exp 'let*))
(define (let*->let exp)
  (define (transform-bindings bindings)
    (if (null? (cdr bindings))
        (make-let (list (car bindings))
                  (let-body-exps exp))
        (make-let (list (car bindings))
                  (list (transform-bindings (cdr bindings))))))
  (if (null? (let-bindings exp))
      (sequence->exp (let-body-exps exp))
      (transform-bindings (let-bindings exp))))

;; letrec
(define (letrec? exp) (tagged-list? exp 'letrec))
(define (letrec->let exp)
  (define (create-empty-bindings vars)
    (map (lambda (var) (list var (list 'quote 'unassigned)))
         vars))
  (define (insert-assignments bindings exps)
    (define (insert-helper bindings)
      (if (null? bindings)
          exps
          (cons (make-assignment (caar bindings)
                                 (cadar bindings))
                (insert-helper (cdr bindings)))))
    (insert-helper bindings))
  (let ((bindings (let-bindings exp))
        (body (let-body-exps exp)))
    (if (null? bindings)
        (sequence->exp body)
        (make-let (create-empty-bindings (map car
                                              bindings))
                  (insert-assignments bindings body)))))

;; ======
;; VALUES
;; ======

;; compound procedure
;; format: (compound #parameters #body #environment)
(define (compound-procedure? proc)
  (tagged-list? proc 'compound))
(define (compound-procedure-parameters proc)
  (cadr proc))
(define (compound-procedure-body proc)
  (caddr proc))
(define (compound-procedure-environment proc)
  (cadddr proc))
(define (make-compound-procedure parameters analyzed-body-exp env)
  (list 'compound parameters analyzed-body-exp env))

;; primitive procedure
;; format: (primitive #implementation)
(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))
(define (make-primitive-binding pair-of-var-and-implementation)
  (make-binding (car pair-of-var-and-implementation)
                (list 'primitive
                      (cdr pair-of-var-and-implementation))))
(define (primitive-procedure-bindings)
  (map make-primitive-binding
       (list (cons '+ +)
             (cons '- -)
             (cons '* *)
             (cons 'quotient quotient)
             (cons 'remainder remainder)
             (cons 'modulo modulo)
             (cons '> >)
             (cons '< <)
             (cons '= =)
             (cons 'not not)
             (cons 'car car)
             (cons 'cdr cdr)
             (cons 'cons cons)
             (cons 'list list)
             (cons 'append append)
             (cons 'null? null?))))
(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc)
         args))

;; special values
(define (special-value-bindings)
  (list (mcons 'true #t)
        (mcons 'false #f)))
(define (true? x)
  (not (false? x)))
(define (false? x)
  (eq? x #f))

;; thunk
;; format: (thunk #analyzed-expression #environment)
(define (thunk? value-obj)
  (tagged-list? value-obj 'thunk))
(define (thunk-analyzed-exp thunk)
  (mcar (mcdr thunk)))
(define (thunk-env thunk)
  (mcar (mcdr (mcdr thunk))))
(define (my-delay analyzed-exp env)
  (mlist 'thunk analyzed-exp env))

;; thunk-memo
;; format: (thunk-memo #analyzed-expression #environment)
(define (thunk-memo? value-obj)
  (tagged-list? value-obj 'thunk-memo))
(define (thunk-memo-analyzed-exp thunk-memo)
  (mcar (mcdr thunk-memo)))
(define (thunk-memo-env thunk-memo)
  (mcar (mcdr (mcdr thunk-memo))))
(define (my-delay-memo analyzed-exp env)
  (mlist 'thunk-memo analyzed-exp env))

;; evaluated-thunk
;; format: (evaluated-thunk #value)
(define (evaluated-thunk? value-obj)
  (tagged-list? value-obj 'evaluated-thunk))
(define (evaluated-thunk-value evaluated-thunk)
  (mcar (mcdr evaluated-thunk)))

;; force
(define (my-force value-obj)
  (cond [(thunk? value-obj)
         (actual-value ((thunk-analyzed-exp value-obj)
                        (thunk-env value-obj)))]
        [(thunk-memo? value-obj)
         (let ((result (actual-value ((thunk-memo-analyzed-exp value-obj)
                                      (thunk-memo-env value-obj)))))
           (set-car! value-obj 'evaluated-thunk)
           (set-cdr! value-obj (mlist result))
           result)]
        [(evaluated-thunk? value-obj)
         (evaluated-thunk-value value-obj)]
        [else value-obj]))
(define (actual-value value-obj)
  (my-force value-obj))

;; ===========
;; ENVIRONMENT
;; ===========

;; binding implementation
(define (make-binding var val)
  (mcons var val))
(define (binding-var binding)
  (mcar binding))
(define (binding-val binding)
  (mcdr binding))
(define (binding-traversal bindings binding-operation)
  ; use #f to indicate failure
  ; make sure #f is not valid return value of binding-operation
  ; (i.e. by wrapping valid return value with a list)
  (if (null? bindings)
      #f
      (let ((first-binding-result (binding-operation (car bindings))))
        (if first-binding-result
            first-binding-result
            (binding-traversal (cdr bindings)
                               binding-operation)))))

;; frame implementation
(define (make-frame bindings)
  (mlist bindings))
(define (frame-bindings frame)
  (mcar frame))

;; environment implementation
(define the-empty-environment '())
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define (frame-traversal env frame-operation)
  ; use #f to indicate failure
  ; make sure #f is not valid return value of frame-operation
  ; (i.e. by wrapping valid return value with a list)
  (if (eq? env the-empty-environment)
      #f
      (let ((first-frame-result (frame-operation (first-frame env))))
        (if first-frame-result
            first-frame-result
            (frame-traversal (enclosing-environment env)
                             frame-operation)))))

;; environment operations
(define (lookup-variable-value variable env)
  (let ((result (frame-traversal
                 env
                 (lambda (frame)
                   (binding-traversal
                    (frame-bindings frame)
                    (lambda (binding)
                      (if (eq? variable (binding-var binding))
                          (list (binding-val binding))
                          #f)))))))
    (if result
        (car result)
        (error "Undefined variable" variable))))
(define (set-variable-value! variable value env)
  (let ((result (frame-traversal
                 env
                 (lambda (frame)
                   (binding-traversal
                    (frame-bindings frame)
                    (lambda (binding)
                      (if (eq? variable (binding-var binding))
                          (begin (set-cdr! binding value)
                                 (list 'undefined))
                          #f)))))))
    (if result
        (car result)
        (error "Undefined variable" variable))))
(define (define-variable! variable value env)
  (let* ([frame (first-frame env)]
         [result (binding-traversal
                  (frame-bindings frame)
                  (lambda (binding)
                    (if (eq? variable (mcar binding))
                        (begin (set-cdr! binding value)
                               (list 'undefined))
                        #f)))])
    (if result
        (car result)
        (add-binding-to-frame! variable value frame))))
(define (add-binding-to-frame! variable value frame)
  (set-car! frame (cons (make-binding variable value)
                        (frame-bindings frame)))
  'undefined)
(define (extend-environment new-bindings env)
  (if (null? new-bindings)
      env
      (cons (make-frame new-bindings) env)))

;; =====
;; UTILS
;; =====

(define (tagged-list? exp tag)
  (or (and (pair? exp)
           (eq? (car exp) tag))
      (and (mpair? exp)
           (eq? (mcar exp) tag))))

;; =========
;; MAIN LOOP
;; =========

(define (setup-environment)
  (extend-environment (append (primitive-procedure-bindings)
                              (special-value-bindings))
                      the-empty-environment))
(define the-global-environment (setup-environment))
(define (driver-loop)
  (define (input-prompt)
    (newline)
    (display ">>> "))
  (define (output-prompt)
    (display "<<< "))
  (define (user-print object)
    (cond [(compound-procedure? object)
           (display (list 'compound-procedure
                          (compound-procedure-parameters object)
                          (compound-procedure-body object)))]
          [else (display object)])
    (newline))
  (input-prompt)
  (let ((input (read)))
    (let ([output (actual-value (my-eval input
                                         the-global-environment))])
      (output-prompt)
      (user-print output)))
  (driver-loop))
        
                           
