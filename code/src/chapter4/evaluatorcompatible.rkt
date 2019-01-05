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
;; note: analyze always produces (lambda (env) ...)
(define (analyze exp)
  (cond [(self-evaluating? exp)
         (analyze-self-evaluating exp)]
        [(variable? exp)
         (analyze-variable exp)]
        [(lambda? exp)
         (analyze-lambda exp)]
        [(application? exp)
         (analyze-application exp)]
        [else
         (error "Unknown type of expression -- analyze " exp)]))

;; thunk
;; format: (thunk #analyzed-expression #environment)
(define (thunk? value-obj)
  (tagged-list? value-obj 'thunk))
(define (thunk-analyzed-exp thunk)
  (cadr thunk))
(define (thunk-env thunk)
  (caddr thunk))

(define (thunk-memo? value-obj)
  (tagged-list? value-obj 'thunk-memo))
(define (thunk-memo-analyzed-exp thunk-memo)
  (cadr thunk-memo))
(define (thunk-memo-env thunk-memo)
  (caddr thunk-memo))

(define (evaluated-thunk? value-obj)
  (tagged-list? value-obj 'evaluated-thunk))
(define (evaluated-thunk-value evaluated-thunk)
  (cadr evaluated-thunk))

(define (my-delay analyzed-exp env)
  (list 'thunk analyzed-exp env))
(define (my-delay-memo analyzed-exp env)
  (list 'thunk-memo analyzed-exp env))
(define (my-force value-obj)
  (cond [(thunk? value-obj)
         (my-force ((thunk-analyzed-exp value-obj)
                    (thunk-env value-obj)))]
        [(thunk-memo? value-obj)
         (let ((result (my-force ((thunk-memo-analyzed-exp value-obj)
                                  (thunk-memo-env value-obj)))))
           (set-car! value-obj 'evaluated-thunk)
           (set-cdr! value-obj (list result))
           result)]
        [(evaluated-thunk? value-obj)
         (evaluated-thunk-value value-obj)]
        [else value-obj]))

;; ===========
;; ENVIRONMENT
;; ===========

(define (make-binding var val)
  (cons var val))
(define (binding-var binding)
  (car binding))
(define (binding-val binding)
  (cdr binding))

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
(define (lookup-variable-value variable env)
  (let ((result (frame-traversal
                 env
                 (lambda (frame)
                   (binding-traversal
                    frame
                    (lambda (binding)
                      (if (eq? variable (binding-var binding))
                          (list (cdr binding))
                          #f)))))))
    (if result
        (car result)
        (error "Undefined variable" variable))))
(define (extend-environment new-bindings env)
  (if (null? new-bindings)
      env
      (cons new-bindings env)))
(define (setup-environment)
  (extend-environment (append primitive-procedure-bindings
                              special-value-bindings)
                      the-empty-environment))

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

;; lambda
;; format: (lambda (param0 (lazy param1) (lazy-memo param2) ...) exp0 exp1 ...)
(define (lambda? exp)
  (tagged-list? exp 'lambda))
(define (lambda-parameters exp)
  (cadr exp))
(define (lambda-body-exps exp)
  (cddr exp))
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
      (execute-application (my-force (analyzed-procedure-exp env))
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
                                           (my-force (exp env)))
                                         analyzed-argument-exps))]
        [(compound-procedure? procedure-value)
         ((compound-procedure-body procedure-value)
          (extend-environment (create-arg-bindings (compound-procedure-parameters procedure-value)
                                                   analyzed-argument-exps)
                              (compound-procedure-environment procedure-value)))]
        [else
         (error "Unknown procedure type -- execute application" procedure-value)]))

;; ======
;; VALUES
;; ======

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

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))
(define (make-primitive-binding pair-of-var-and-implementation)
  (list (car pair-of-var-and-implementation)
        'primitive
        (cdr pair-of-var-and-implementation)))
(define primitive-procedure-bindings
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
             (cons 'append append)
             (cons 'null? null?))))
(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc)
         args))

(define special-value-bindings
  (list (cons 'true #t)
        (cons 'false #f)))

;; =====
;; UTILS
;; =====

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

(define (tagged-list? exp tag)
  (and (pair? exp)
       (eq? (car exp) tag)))

;; ====
;; MAIN
;; ====

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
    (let ((output (my-eval input the-global-environment)))
      (output-prompt)
      (user-print output)))
  (driver-loop))
        
                           
