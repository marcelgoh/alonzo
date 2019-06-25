# Alonzo
Alonzo is a λ-calculus interpreter.

## Setup

Alonzo requires `cabal` to build and run.

+ To compile the executable, run `cabal build`.
+ To run the interpreter, enter `cabal run`.
+ To delete generated files, use `cabal clean`

## Usage

When Alonzo is first run, a prompt will appear:

```
+----------------------------------------------+
|        ALONZO λ-CALCULUS INTERPRETER         |
|   Author: Marcel Goh (Release: 25.VI.2019)   |
|            Type "Ctrl-C" to quit.            |
+----------------------------------------------+
]=>
```

The interpreter accepts three types of statements:

+ Assignment: `<name> = <term>+`, e.g. `OMEGA = \x.x x`
+ Term evaluation: `<name | term>+`, e.g. `CONS (\x.\y.x y y) OR`
+ Name lookup: `<name>`, e.g. `OMEGA`

In the first two cases, Alonzo will attempt to evaluate terms by repeatedly performing β-reduction. Here are some examples of this in action:

```
]=> PAIR = CONS ONE TRUE
(λf.((f (λf.(λx.(f x)))) (λp.(λq.p)))) -- PAIR
]=> CDR PAIR
(λp.(λq.p)) -- TRUE
]=> ONE
(λf.(λx.(f x))) -- ONE
]=> \a.\b.a b
(λa.(λb.(a b))) -- ONE
```

Alonzo will remember terms that have been given names using the `=` operator. A name can only hold one term at a time, so a previous term will be replaced if a name is assigned to twice. Upon startup, Alonzo has the following names in its environment:

```
S -- λx.λy.λz.x z (y z)
K -- λx.λy.x
I -- λx.x
Y -- λg.(λx.g (x x)) (λx.g (x x))   [fixed-point combinator]
TRUE -- λp.λq.p
FALSE -- λp.λq.q
AND -- λp.λq.p q p
OR -- λp.λq.p p q
NOT -- λp.p FALSE TRUE
IF -- λp.λa.λb.p a b
ZERO -- λf.λx.x
ONE -- λf.λx.f x
[...]
TEN -- λf.λx.f (f (f (f (f (f (f (f (f (f x)))))))))
ISZERO -- λn.n (λx.FALSE) TRUE
SUCC -- λn.λf.λx.f (n f x)
PRED -- λn.λf.λx.n (λg.λh.h (g f)) (λu.x) (λu.u)  [PRED FALSE == FALSE]
PLUS -- λm.λn.λf.λx.m f (n f x)
TIMES -- λm.λn.λf.m (n f)
POW -- λb.λe.e b
CONS -- λx.λy.λf.f x y
CAR -- λp.p TRUE
CDR -- λp.p FALSE
SUMREC -- \r.\n.IF (ISZERO n) FALSE (PLUS n (r (PRED n)))
FACTREC -- \r.\n.IF (ISZERO n) ONE (TIMES n (r (PRED n)))
```

Calling `Y SUMREC n` computes the partial sum up to `n` and calling `Y FACTREC n` calculates the factorial of `n`. The user should avoid reassigning these names.

## Authors

Code: Marcel Goh  
Created as an end-of-term project for NPRG005 Non-procedural Programming at Charles University in Prague.
