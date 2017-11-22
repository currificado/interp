# interp
Toy interpreter for evaluating expressions

To compile and use it:

$ ghc --make interp.hs  
$ ./interp -vc foo.txt  
Let "y" (Add (Num 1) (Num 2)) (Let "x" (Add (Num 3) (Num 4)) (Add (Var "x") (Var "y")))  
10

$ cat foo.txt  
let y (add 1 2) (let x (add 3 4) (add x y))  
