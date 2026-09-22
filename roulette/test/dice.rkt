#lang at-exp racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (only-in (submod roulette/example/dice reader) read-syntax)
         (only-in "../example/disrupt/core.rkt" query pmf-hash)
         racket/string
         rackunit
         syntax/parse
         "util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-namespace-anchor here)
(define anchored-ns (namespace-anchor->namespace here))
(define ϵ 0.000001)

(define (check-program t . lines)
  (define prog (string-join lines))
  (define stx (read-syntax 'anonymous (open-input-string prog)))
  (define mod
    (syntax-parse stx
      [(_ name lang form ... final)
       #'(module anonymous lang
           (provide result)
           form ...
           (define result final))]))
  (define ns (make-base-namespace))
  (namespace-attach-module anchored-ns "../example/disrupt/core.rkt" ns)
  (parameterize ([current-namespace ns])
    (eval mod)
    (define result (pmf-hash (query (dynamic-require ''anonymous 'result))))
    (with-check-info (['program prog] ['result result])
      (cond
        [(= t 1.0) (check-close ϵ result (hash #t 1.0))]
        [(= t 0.0) (check-close ϵ result (hash #f 1.0))]
        [else (check-close ϵ result (hash #t t #f (- 1 t)))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  @check-program[0.4]{
    let x = flip 0.4 in x
  }

  @check-program[0.6]{
    let x = flip 0.4 in !x
  }

  @check-program[(/ 0.4 0.46)]{
    let x = flip 0.4 in let y = flip 0.1 in let z = observe x || y in x
  }

  @check-program[(/ 0.06 0.46)]{
    let x = flip 0.4 in let y = flip 0.1 in let z = observe x || y in !x
  }

  @check-program[0.4]{
    let x = (flip 0.1, flip 0.4) in snd x
  }

  @check-program[0.4]{
    let x = (flip 0.1, (flip 0.4, flip 0.7)) in fst (snd x)
  }

  @check-program[0.6]{
    let x = (flip 0.1, (flip 0.4, flip 0.7)) in ! fst (snd x)
  }

  @check-program[0.4]{
    if flip 0.4 then true else false
  }

  @check-program[0.24]{
    if flip 0.4 then flip 0.6 else false
  }

  @check-program[0.0]{
    if flip 0.4 then let z = observe false in flip 0.6 else false
  }

  @check-program[0.4]{
    let x = discrete(0.1, 0.4, 0.5) in x == int(2, 1)
  }

  @check-program[(/ 0.4 0.9)]{
    let x = discrete(0.1, 0.4, 0.5) in let z = observe ! (x == int(2, 0)) in x == int(2, 1)
  }

  @check-program[1.0]{
    let x = discrete(0.1, 0.4, 0.5) in let z = observe ! (x == int(2, 0) || x == int(2,1)) in x == int(2, 2)
  }

  @check-program[(/ 0.5 0.6)]{
    let x = discrete(0.1, 0.4, 0.5) in let z = observe ! (x == int(2, 1)) in x == int(2, 2)
  }

  @check-program[1.0]{
    let x = int(3, 0) + int(3, 1) in x == int(3, 1)
  }

  @check-program[0.1]{
    let x = discrete(0.1, 0.4, 0.5) + int(2, 1) in x == int(2, 1)
  }

  @check-program[0.4]{
    let x = discrete(0.1, 0.4, 0.5) + discrete(1.0, 0.0, 0.0) in x == int(2, 1)
  }

  @check-program[0.25]{
    let x = discrete(0.25, 0.25, 0.25, 0.25) in
    let y = discrete(0.25, 0.25, 0.25, 0.25) in
    (x + y) == int(2, 1)
  }

  @check-program[0.1]{
    let x = discrete(0.3, 0.1, 0.2, 0.2, 0.2) in
    let y = discrete(0.1, 0.3, 0.2, 0.2, 0.2) in
    let sum = x + y in
    let z = observe x == int(3, 1) in
    sum == int(3, 1)
  }

  @check-program[0.125]{
    let x = discrete(0.125, 0.125, 0.125, 0.125, 0.125, 0.125, 0.125, 0.125) in
    let y = discrete(0.125, 0.125, 0.125, 0.125, 0.125, 0.125, 0.125, 0.125) in
    (x + y) == int(3, 1)
  }

  @check-program[1.0]{
    let x = int(3, 0) - int(3, 1) in x == int(3, 7)
  }

  @check-program[0.5]{
    let x = discrete(0.1, 0.4, 0.5) - int(2, 1) in x == int(2, 1)
  }

  @check-program[0.5]{
    let x = discrete(0.1, 0.4, 0.5) - discrete(0.0, 1.0, 0.0) in x == int(2, 1)
  }

  @check-program[0.125]{
    let x = discrete(0.125, 0.125, 0.125, 0.125, 0.125, 0.125, 0.125, 0.125) in
    let y = discrete(0.125, 0.125, 0.125, 0.125, 0.125, 0.125, 0.125, 0.125) in
    (x - y) == int(3, 1)
  }

  @check-program[(/ 3.0 20.0)]{
    discrete(0.1, 0.2, 0.3, 0.4) < discrete(0.4, 0.3, 0.2, 0.1)
  }

  @check-program[(/ 7.0 20.0)]{
    let x = discrete(0.1, 0.2, 0.3, 0.4) in
    let y = discrete(0.4, 0.3, 0.2, 0.1) in
    x <= y
  }

  @check-program[(/ 23.0 50.0)]{
     let x = discrete(0.1, 0.2, 0.3, 0.4) in
     let y = discrete(0.4, 0.3, 0.2, 0.1) in
     (x + y) < int(2, 2)
  }

  @check-program[(/ 5.0 23.0)]{
     let x = discrete(0.1, 0.2, 0.3, 0.4) in
     let y = discrete(0.4, 0.3, 0.2, 0.1) in
     let tmp = observe (x + y) < int(2, 2) in
     x == y
  }

  @check-program[0.0]{
    true <=> false
  }

  @check-program[1.0]{
    false <=> false
  }

  @check-program[0.58]{
    flip 0.1 <=> flip 0.4
  }

  @check-program[1.0]{
    true ^ false
  }

  @check-program[0.0]{
    false ^ false
  }

  @check-program[0.42]{
    flip 0.1 ^ flip 0.4
  }

  #|
  @check-program[1.0]{
    let x = int(3, 0) * int(3, 1) in x == int(3, 0)
  }

  @check-program[1.0]{
    let x = int(3, 2) * int(3, 2) in x == int(3, 4)
  }

  @check-program[1.0]{
    let x = int(3, 3) * int(3, 3) in x == int(3, 1)
  }

  @check-program[1.0]{
    let x = int(4, 3) * int(4, 3) in x == int(4, 9)
  }

  @check-program[1.0]{
    let x = int(4, 3) * int(4, 3) * int(4, 3) in x == int(4, 11)
  }

  @check-program[0.6]{
    let x = discrete(0.1, 0.4, 0.5, 0.0) * int(2, 2) in x == int(2, 0)
  }
  |#

  @check-program[1.0]{
    let x = int(4, 1) in let y = x << 2 in y == int(4, 4)
  }

  @check-program[1.0]{
    let x = int(4, 1) in let y = x << 5 in y == int(4, 0)
  }

  @check-program[1.0]{
    let x = int(4, 8) in let y = x >> 2 in y == int(4, 2)
  }

  @check-program[1.0]{
    let x = int(4, 12) in let y = x >> 1 in y == int(4, 6)
  }

  @check-program[1.0]{
    let x = int(4, 12) in let y = x >> 5 in y == int(4, 0)
  }

  @check-program[1.0]{
    let x = int(2, 2) in int(2, 1) == (x >> 1)
  }

  #|
  @check-program[1.0]{
    let a = int(2,2) in
    let _x1 = if ( nth_bit(int(2,1), a)) then true else false in
    let _x0 = if ( nth_bit(int(2,0), a)) then _x1 else true in
    let b = a in
    let _y1 = if ( nth_bit(int(2,1), b)) then true else false in
    let _y0 = if ( nth_bit(int(2,1), b >> 1)) then _y1 else true in
    _x0 <=> _y0
  }
  |#

  @check-program[0.4]{
    let u = uniform(4, 0, 10) in u < int(4, 4)
  }

  @check-program[0.4]{
    let d = discrete(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1) in d < int(4, 4)
  }

  @check-program[0.0]{
    let u = uniform(3, 2, 6) in u == int(3, 0)
  }

  @check-program[0.0]{
    let d = discrete(0., 0., 0.25, 0.25, 0.25, 0.25) in d == int(3, 0)
  }

  @check-program[1.0]{
    let u = uniform(3, 3, 4) in u == int(3, 3)
  }

  @check-program[1.0]{
    let d = discrete(0., 0., 0., 1., 0.) in d == int(3, 3)
  }

  @check-program[0.25]{
    let u = uniform(2, 1, 4) in
    let d = discrete(0., 0.5, 0.25, 0.25) in
    u == d && u < int(2, 3)
  }

  @check-program[0.421875]{
    let b = binomial(3, 4, 0.25) in b == int(3, 1)
  }

  @check-program[0.5]{
    let b = binomial(5, 29, 0.5) in b <= int(5, 14)
  }

  @check-program[1.0]{
    let b = binomial(3, 0, 0.5) in b == int(3, 0)
  }

  @check-program[0.3]{
    let b = binomial(3, 1, 0.3) in b == int(3, 1)
  }

  @check-program[0.25]{
    fun foo(test: bool) {
      (flip 0.5) && test
    }
    foo(true) && foo(true)
  }

  @check-program[0.0]{
    fun foo(test: bool) {
      (flip 0.5) && test
    }
    foo(true) && foo(false)
  }

  @check-program[0.06250]{
    fun foo(test: bool) {
      (flip 0.5) && test
    }
    foo(flip 0.5) && foo(flip 0.5)
  }

  @check-program[1.0]{
    fun foo(test: bool) {
      let tmp = observe test in
      true
    }
    let z = flip 0.5 in
    let tmp = foo(z) in
    z
  }

  @check-program[(/ 0.4 0.46)]{
    fun foo(test1: bool, test2: bool) {
      let k = observe test1 || test2 in
      false
    }
    let f1 = flip 0.4 in
    let f2 = flip 0.1 in
    let tmp = foo(f1, f2) in
    f1
  }

  @check-program[(/ 0.4 0.46)]{
    fun foo(test1: (bool, bool)) {
      let k = observe (fst test1) || (snd test1) in
      false
    }
    let f1 = flip 0.4 in
    let tmp = foo((f1, flip 0.1)) in f1
  }

  @check-program[(/ 0.4 0.9)]{
    fun foo(test1: int(2)) {
      let k = observe !(test1 == int(2, 0)) in
      false
    }
    let f1 = discrete(0.1, 0.4, 0.5) in
    let tmp = foo(f1) in f1 == int(2, 1)
  }

  #|
  let test_nthbit1 _ =
    let prog = "
      let f1 = discrete(0.1, 0.4, 0.3, 0.2) in
      nth_bit(int(2, 1), f1)" in
    assert_feq 0.6 (parse_and_prob prog)

  let test_nthbit2 _ =
    let prog = "
      let f1 = discrete(0.1, 0.4, 0.3, 0.2) in
      nth_bit(int(2, 0), f1)" in
    assert_feq 0.5 (parse_and_prob prog)

  let test_nthbit3 _ =
    let prog = "
      let a = int(2, 1) in nth_bit(int(2,1), a)" in
    assert_feq 1.0 (parse_and_prob prog)

  let test_nthbit4 _ =
    let prog = "
      let a = int(2, 1) in nth_bit(int(2,0), a)" in
    assert_feq 0.0 (parse_and_prob prog)
  |#

  @check-program[0.25]{
    fun sendchar(key: int(2), observation: int(2)) {
      let gen = discrete(0.5, 0.25, 0.125, 0.125) in
      let enc = key + gen in
      observe observation == enc
    }
    let key = discrete(0.25, 0.25, 0.25, 0.25) in
    let tmp = sendchar(key, int(2, 0)) in
    let tmp = sendchar(key, int(2, 1)) in
    let tmp = sendchar(key, int(2, 2)) in
    let tmp = sendchar(key, int(2, 3)) in
    key == int(2, 0)
  }

  @check-program[0.25]{
    fun sendchar(arg: (int(2), int(2))) {
      let key = fst arg in
      let observation = snd arg in
      let gen = discrete(0.5, 0.25, 0.125, 0.125) in    // sample a foolang character
      let enc = key + gen in                            // encrypt the character
      let tmp = observe observation == enc in
      (key, observation + int(2, 1))
    }
    // sample a uniform random key: a=0, b=1, c=2, d=3
    let key = discrete(0.25, 0.25, 0.25, 0.25) in
    // observe the ciphertext cccc
    let tmp = iterate(sendchar, (key, int(2, 2)), 4) in
    key == int(2, 0)
  }

  @check-program[0.284172]{
    let burglary = flip 0.001 in
    let earthquake = flip 0.002 in
    let alarm = if burglary then if earthquake then flip 0.95 else flip 0.94 else if earthquake then flip 0.29 else flip 0.001 in
    let john = 	if alarm then flip 0.9 else flip 0.05 in
    let mary = 	if alarm then flip 0.7 else flip 0.01 in
    let temp = observe john in
    let temp = observe mary in
    burglary
  }

  @check-program[0.25]{
    let c1 = flip 0.5 in
    let c2 = flip 0.5 in
    c1 && c2
  }

  @check-program[1.0]{
    let c1 = discrete(0.1, 0.4, 0.5) in
    let c2 = int(2, 1) in
    (c1 == c2) || (c1 != c2)
  }

  @check-program[1.0]{
    let c1 = int(3, 0) in
    let c2 = int(3, 1) in
    (c1 - c2) == int(3, 7)
  }

  @check-program[0.45]{
    let coin1 = if flip 0.5 then int(6, 10) else int(6, 25) in
    let coin2 = if flip 0.5 then int(6, 10) else int(6, 25) in
    let s1 = if flip(0.8) then coin1 else int(6, 0) in
    let s2 = if flip 0.8 then coin2 + s1 else s1 in
    let candy = s2 >= int(6, 15) in
    let tmp = observe candy in
    coin1 == int(6, 10)
  }

  @check-program[0.25]{
    fun sendchar(key: int(2), observation: int(2)) {
      let gen = discrete(0.5, 0.25, 0.125, 0.125) in
      let enc = key + gen in
      observe observation == enc
    }
    fun loop(key: int(2), observation: int(2)): bool {
      let tmp = sendchar(key, observation) in
      if observation == int(2, 3)
        then true
        else loop(key, observation + int(2, 1))
    }
    let key = discrete(0.25, 0.25, 0.25, 0.25) in
    let tmp = loop(key, int(2, 0)) in
    key == int(2, 0)
  }

  #|
  @check-program[1.0]{
    fun fac(n: int(7)): int(7) {
      if n == int(7, 0) then int(7, 1) else n * fac(n - int(7, 1))
    }
    fac(int(7, 5)) == int(7, 120)
  }
  |#

  @check-program[1.0]{
    fun fib(n: int(7)): int(7) {
      if n < int(7, 2) then n else fib(n - int(7, 1)) + fib(n - int(7, 2))
    }
    fib(int(7, 11)) == int(7, 89)
  }

  @check-program[1.0]{
    let xs = [true, false, false] in
    (head xs) && !(head (tail xs)) && !(head (tail (tail xs)))
  }

  #|
  @check-program[1.0]{
    let xs = [true, false, false] in
    (length xs) == int(4, 3)
  }

  @check-program[1.0]{
    let xs = [] : list(bool) in
    (length xs) == int(4, 0)
  }
  |#

  @check-program[1.0]{
    fun index(n: int(2), xs: list(bool)): bool {
      if n == int(2, 0) then
        head xs
      else
        index(n - int(2, 1), tail xs)
    }
    let xs = [true, false, false] in
    !index(int(2, 2), xs) && !index(int(2, 1), xs) && index(int(2, 0), xs)
  }

  @check-program[(+ (* 0.2 0.5) (* 0.4 0.5))]{
    let xs = [flip 0.2, flip 0.4] in
    let ys = if flip 0.5 then (head xs) :: xs else tail xs in
    head ys
  }

  @check-program[0.5625]{
    uniform(3, 0, 8) <= uniform(3, 0, 8)
  }

  @check-program[0.5625]{
    let a = uniform(3, 0, 8) in
    let b = uniform(3, 0, 8) in
    a <= b
  }

  @check-program[0.4375]{
    uniform(3, 0, 8) < uniform(3, 0, 8)
  }

  @check-program[0.4375]{
    let a = uniform(3, 0, 8) in
    let b = uniform(3, 0, 8) in
    a < b
  })
