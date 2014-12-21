TLang
=====

静的型付けな関数型言語を実装するにあたってのプロトタイプな言語
Common Lisp 製(微妙にSBCL依存)
型推論は実装していないので全部型を書かないといけない


使い方
=====

```
$ sbcl --load toplevel.lisp 
```

とするとREPLに入る

```
  $sbcl --load toplevel.lisp source.tl
```

とすると source.tl を型検査して out.lisp　を吐く
out.lisp はただの Common Lisp のコード


サンプルコード
=====

```
def fact (n:Int): Int -> 
  if (n = 0) 
    1
    n * fact[n-1]

def fib (n:Int):Int -> 
  if ((n = 0) or (n = 1)) 
    n
    fib[n-2] + fib[n-1] 

def repeat (check:(Int->Bool)): (Int->((Int->Int)->Int)) ->
  (init:Int): (Int -> Int) -> Int ->
  (f:(Int->Int)): Int ->
    if (check[init]) 
      f[init]
      repeat[check][init+1][f]


print fact[5]
print repeat[(x:Int):Bool -> (x % 29 = 0)][1][(x:Int):Int -> x*3]
```


BNF
====

```
PROGRAM  ::= 
    EXPR  
  | PROGRAM EXPR

EXPR     ::= 
    LITERAL 
  | VARIABLE 
  | COMPOUND

LITERAL  ::= 
    INTEGER 
  | BOOLEAN

INTEGER  ::= '0' | '1' | '2' | '3' | ...
BOOLEAN  ::= 'true' | 'false'
VARIABLE ::= 'x' | 'y' | 'z' | ...

COMPOUND ::= 
    EXPR '=' EXPR
  | EXPR '+' EXPR
  | EXPR '*' EXPR
  | EXPR '-' EXPR
  | EXPR '%' EXPR
  | EXPR 'and' EXPR
  | 'not' EXPR
  | 'if' EXPR EXPR EXPR
  | '(' EXPR ')'
  | EXPR'[' EXPR ']'
  | LAMBDA
  | 'def' VARIABLE LAMBDA

LAMBDA ::= '(' VARIABLE ':' TYPE ')' ':' TYPE '->' EXPR

TYPE     ::= 
    Int 
  | Bool 
  | '(' TYPE ')'
  | TYPE '->' TYPE
```
