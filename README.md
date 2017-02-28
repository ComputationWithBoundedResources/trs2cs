# trs2cs

This tool transforms a Term Rewrite System into a Constraint System for
polynomial interpretations over the natural numbers for runtime innermost
complexity modulo usable argument positions.

The output is compatible with the constraint solver
[GUBS](https://github.com/ComputationWithBoundedResources/gubs).


```bash
$ cat jones1.trs
(VAR a k ls x)

(RULES 
   rev(ls) -> r1(ls,empty)
   r1(empty,a) -> a
   r1(cons(x,k),a) -> r1(k,cons(x,a))
)

$ #trs2cs accepts TPDB WST and XML format
$ trs2cs jones1.trs
(>=
 (r1
  (cons (var x) (var k))
  (var a))
 (+
  (r1
   (var k)
   (cons (var x) (var a)))
  1))
(>=
 (r1 (empty) (var a))
 (+ (var a) 1))
(>=
 (rev (var ls))
 (+ (r1 (var ls) (empty)) 1))
(>=
 (cons (var _x1) (var _x2))
 0)
(>= (empty) 0)
(>= (r1 (var _x1) (var _x2)) 0)
(>= (rev (var _x1)) 0)
(>=
 (+
  (+ (var _x1) (var _x2))
  (_f1))
 (cons (var _x1) (var _x2)))
(>= (_f2) (empty))

$ #run gubs-0.3.0.0 to generate a polynomial interpretation
$ gubs <(trs2cs jones1.trs)
SUCCESS
_f1() = 1;
_f2() = 1;
cons(x0,x1) = 1 + x1;
empty() = 1;
r1(x0,x1) = 2*x0 + x1;
rev(x0) = 2 + 2*x0;
```

## Installation

The recommended way to install `trs2cs` is via [`stack`](http://haskellstack.org).

```bash
$ git clone http://github.com/ComputationWithBoundedResources/trs2cs \
  && cd trs2cs \
  && STACK_YAML=stack-head.yaml stack build trs2cs

$ STACK_YAML=stack-head.yaml stack exec trs2cs -- file.trs
```
