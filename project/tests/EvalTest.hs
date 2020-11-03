module EvalTest where

import Test.Tasty (testGroup)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)
--import Test.Tasty.QuickCheck 

import Ast
import Eval

-- provide tests that show your run/eval works

-- Integers
zero = (ValInt 0)
one = (ValInt 1)
none = (ValInt (-1))
two = (ValInt 2)
ntwo = (ValInt (-2))
three = (ValInt 3)
nthree = (ValInt (-3))
four = (ValInt 4)
nfour = (ValInt (-4))
five = (ValInt 5)

-- Floats
fzero = (ValFloat 0.0)
fnzeropointfive = (ValFloat -0.5)
fzeropointfive = (ValFloat 0.5)
fone = (ValFloat 1.0)
fonepointfive = (ValFloat 1.5)
fnone = (ValFloat (-1.0))
fnonepointfive = (ValFloat -1.5)
ftwo = (ValFloat 2.0)
ftwopointfive = (ValFloat 2.5)
fntwo = (ValFloat (-2.0))
fntwopointfive = (ValFloat -2.5)
fthree = (ValFloat 3.0)
fthreepointfive = (ValFloat 3.5)
fnthree = (ValFloat (-3.0))
fnthreepointfive = (ValFloat -3.5)
ffour = (ValFloat 4.0)
ffourpointfive = (ValFloat 4.5)
ffive = (ValFloat 5.0)
fnfour = (ValFloat (-4.0))
fnfourpointfive = (ValFloat -4.5)

--Booleans
t = (ValBool True) 
f = (ValBool False)

--Chars
a = (ValChar 'a')
b = (ValChar 'b')
c = (ValChar 'c')
d = (ValChar 'd')
e = (ValChar 'e')

--Lists
list1 = [zero, one, two]
list2 = [three, four, five]

list3 = [fzero, fone, ftwo]
list4 = [fthree, ffour, ffive]

list5 = [t, f, t, f, t]
list6 = [f, t, f, t, f]

list7 = [a, b, c, d, e]
list8 = [e, d, c, b, a]

evalTest = testGroup
      "Eval Test"
      [
         testCase "Basic Arithmetic" $
            do 
              -- Addition Test Cases (Integers)
              assertEqual "2 + 4 =? "    6    (exec (Plus two four))
              assertEqual "4 + 2 =? "    6    (exec (Plus four two)
              assertEqual "2 + -1 =? "   1    (exec (Plus two none))
              assertEqual "-1 + 2 =? "   1    (exec (Plus none two))
              assertEqual "-3 + -2 =? "(-5)    (exec (Plus nthree ntwo))
              assertEqual "-2 + -3 =? "(-5)    (exec (Plus ntwo nthree))
              assertEqual "0 + 0 =? "    0    (exec (Plus zero zero))
              assertEqual "4 + 0 =? "    4    (exec (Plus four zero))
              assertEqual "0 + 4 =? "    4    (exec (Plus zero four))
              assertEqual "0 + -2 =? "  -2    (exec (Plus zero ntwo))
              assertEqual "-2 + 0 =? "  -2    (exec (Plus ntwo zero))
              
              -- Addition Test Cases (Floats)
              assertEqual "2.0 + 4.0 =? "    6.0    (exec (Plus ftwo ffour))
              assertEqual "4.5 + 2.5 =? "    7.0    (exec (Plus ffourpointfive ftwopointfive)
              assertEqual "2.0 + -1.0 =? "   1.0    (exec (Plus ftwo fnone))
              assertEqual "-1.5 + 2.5 =? "   1.0   (exec (Plus fnonepointfive ftwopointfive))
              assertEqual "-3.0 + -2.0 =? "(-5.0)    (exec (Plus fnthree fntwo))
              assertEqual "-2.5 + -3.5 =? "(-6.0)    (exec (Plus fntwopointfive fnthreepointfive))
              assertEqual "0.0 + 0.0 =? "    0.0    (exec (Plus fzero fzero))
              assertEqual "4.0 + 0.0 =? "    4.0    (exec (Plus ffour fzero))
              assertEqual "0.5 + 4.5 =? "    5.0    (exec (Plus fzeropointfive ffourpointfive))
              assertEqual "0.0 + -2.0 =? "  -2.0    (exec (Plus fzero fntwo))
              assertEqual "-2.5 + 0.5 =? "  -2.0    (exec (Plus fntwopointfive fzeropointfive))
              
              --Subtraction Test Cases (Integers)
              assertEqual "2 - 4 =? "    (-2)    (exec (Minus two four))
              assertEqual "4 - 2 =? "      2    (exec (Minus four two))
              assertEqual "2 - (-1) =? "   3    (exec (Minus two none))
              assertEqual "-1 - 2 =? "   (-3)    (exec (Minus none two))
              assertEqual "-3 - (-2) =? "(-1)    (exec (Minus nthree ntwo))
              assertEqual "-2 - (-3) =? "  1    (exec (Minus ntwo nthree))
              assertEqual "0 - 0 =? "      0    (exec (Minus zero zero))
              assertEqual "4 - 0 =? "      4    (exec (Minus four zero))
              assertEqual "0 - 4 =? "    (-4)    (exec (Minus zero four))
              assertEqual "0 - (-2) =? "   2    (exec (Minus zero ntwo))
              assertEqual "-2 - 0 =? "   (-2)    (exec (Minus ntwo zero))
              
              -- Subtraction Test Cases (Floats)
              assertEqual "2.0 - 4.0 =? "    (-2.0)    (exec (Minus ftwo ffour))
              assertEqual "4.5 - 2.5 =? "      2.0    (exec (Minus ffourpointfive ftwopointfive)
              assertEqual "2.0 - -1.0 =? "     3.0    (exec (Minus ftwo fnone))
              assertEqual "-1.5 - 2.5 =? "   (-4.0)   (exec (Minus fnonepointfive ftwopointfive))
              assertEqual "-3.0 - -2.0 =? "  (-1.0)    (exec (Minus fnthree fntwo))
              assertEqual "-2.5 - -3.5 =? "   1.0    (exec (Minus fntwopointfive fnthreepointfive))
              assertEqual "0.0 - 0.0 =? "     0.0    (exec (Minus fzero fzero))
              assertEqual "4.0 - 0.0 =? "     4.0    (exec (Minus ffour fzero))
              assertEqual "0.5 - 4.5 =? "   (-4.0)    (exec (Minus fzeropointfive ffourpointfive))
              assertEqual "0.0 - -2.0 =? "    2.0    (exec (Minus fzero fntwo))
              assertEqual "-2.5 - 0.5 =? "  (-3.0)    (exec (Minus fntwopointfive fzeropointfive))
              
              -- Multiplication Test Cases (Integers)

              assertEqual "2 * 4 =? "    8    (exec (Mult two four))
              assertEqual "4 * 2 =? "    8    (exec (Mult four two))
              assertEqual "2 * -1 =? " (-2)   (exec (Mult two none))
              assertEqual "-1 * 2 =? " (-2)   (exec (Mult none two))
              assertEqual "-3 * -2 =? "  6    (exec (Mult nthree ntwo))
              assertEqual "-2 * -3 =? "  6    (exec (Mult ntwo nthree))
              assertEqual "0 * 0 =? "    0    (exec (Mult zero zero))
              assertEqual "4 * 0 =? "    0    (exec (Mult four zero))
              assertEqual "4 * 4 =? "   16    (exec (Mult four four))
              assertEqual "0 * -2 =? "   0    (exec (Mult zero ntwo))
              assertEqual "-2 * 0 =? "   0    (exec (Mult ntwo zero))
              
              -- Multiplication Test Cases (Floats)
              assertEqual "2.0 * 4.0 =? "    (8.0)    (exec (Mult ftwo ffour))
              assertEqual "4.5 * 2.5 =? "     11.25   (exec (Mult ffourpointfive ftwopointfive)
              assertEqual "2.0 * -1.0 =? "   (-2.0)   (exec (Mult ftwo fnone))
              assertEqual "-1.5 * 2.5 =? "   (-3.75)  (exec (Mult fnonepointfive ftwopointfive))
              assertEqual "-3.0 * -2.0 =? "    6.0    (exec (Mult fnthree fntwo))
              assertEqual "-2.5 * -3.5 =? "    8.75   (exec (Mult fntwopointfive fnthreepointfive))
              assertEqual "0.0 * 0.0 =? "      0.0    (exec (Mult fzero fzero))
              assertEqual "4.0 * 0.0 =? "      4.0    (exec (Mult ffour fzero))
              assertEqual "0.5 * 4.5 =? "      2.25   (exec (Mult fzeropointfive ffourpointfive))
              assertEqual "0.0 * -2.0 =? "     0.0    (exec (Mult fzero fntwo))
              assertEqual "-2.5 * 0.5 =? "    -1.25   (exec (Mult fntwopointfive fzeropointfive))

              -- (Floating-Point) Division Test Cases
              assertEqual "2.0 / 4.0 =? "    0.5    (exec (FloatingPointDiv ftwo ffour))
              assertEqual "4.0 / 2.0 =? "    2.0    (exec (FloatingPointDiv ffour ftwo))
              assertEqual "2.0 / -1.0 =? " (-2.0)   (exec (FloatingPointDiv ftwo fnone))
              assertEqual "-1.0 / 2.0 =? " (-0.5)   (exec (FloatingPointDiv fnone ftwo))
              assertEqual "-3.0 / -2.0 =? "  1.5    (exec (FloatingPointDiv fnthree fntwo))
              assertEqual "-2.0 / -2.0 =? "  1.0    (exec (FloatingPointDiv fntwo fntwo))
              assertEqual "0.0 / 0.0 =? "    err "you can't divide by zero"    (exec (FloatingPointDiv fzero fzero))
              assertEqual "4.0 / 0.0 =? "    err "you can't divide by zero"    (exec (FloatingPointDiv ffour fzero))
              assertEqual "0.0 / 4.0 =? "    0.0    (exec (FloatingPointDiv fzero ffour))
              assertEqual "0.0 / -2.0 =? "   0.0    (exec (FloatingPointDiv fzero fntwo))
              assertEqual "-2.0 / 0.0 =? "   err "you can't divide by zero"    (exec (FloatingPointDiv fntwo fzero))
              
              -- Integer Division Test Cases
              assertEqual "2 // 4 =? "    0    (exec (IntegerDiv two four))
              assertEqual "4 // 2 =? "    2    (exec (IntegerDiv four two))
              assertEqual "2 // -1 =? " (-2)   (exec (IntegerDiv two none))
              assertEqual "-1 // 2 =? "   1    (exec (IntegerDiv none two))
              assertEqual "-3 // -2 =? "  1    (exec (IntegerDiv nthree ntwo))
              assertEqual "-2 // -2 =? "  1    (exec (IntegerDiv ntwo nthree))
              assertEqual "0 // 0 =? "    err "you can't divide by zero"    (exec (IntegerDiv zero zero))
              assertEqual "4 // 0 =? "    err "you can't divide by zero"    (exec (IntegerDiv four zero))
              assertEqual "0 // 4 =? "    0    (exec (IntegerDiv zero four))
              assertEqual "0 // -2 =? "   0    (exec (IntegerDiv zero ntwo))
              assertEqual "-2 // 0 =? "   err "you can't divide by zero"    (exec (IntegerDiv ntwo zero))

              
              -- Modulus Test Cases (Integers ONLY)
              assertEqual "2 % 4 =? "    2    (exec (Mod two four))
              assertEqual "4 % 2 =? "    0    (exec (Mod four two))
              assertEqual "2 % -1 =? "   0    (exec (Mod two none))
              assertEqual "-1 % 2 =? "   1    (exec (Mod none two))
              assertEqual "-3 % -2 =? " -1    (exec (Mod nthree ntwo))
              assertEqual "-2 % -2 =? "  0    (exec (Mod ntwo nthree))
              assertEqual "0 % 0 =? "    err "you can't mod by zero"    (exec (Mod zero zero))
              assertEqual "4 % 0 =? "    err "you can't mod by zero"    (exec (Mod four zero))
              assertEqual "0 % 4 =? "    0    (exec (Mod zero four))
              assertEqual "0 % -2 =? "   0    (exec (Mod zero ntwo))
              assertEqual "-2 % 0 =? "   err "you can't mod by zero"    (exec (Mod ntwo zero)),
              
         
        testCase "Exponentiation for Integers and Floats" $
            do 
              --  Floating-Point Exponentiation 
              assertEqual "2.0 ^ 4.0 =? "    16.0    (exec (Fpe ftwo ffour))
              assertEqual "4.0 ^ 2.0 =? "    16.0    (exec (Fpe ffour ftwo))
              assertEqual "2.0 ^ -1.0 =? "    0.5    (exec (Fpe ftwo fnone))
              assertEqual "-1.0 ^ 2.0 =? "  (-1.0)   (exec (Fpe fnone ftwo))
              assertEqual "-4.0 ^ -2.0 =? " (-0.625) (exec (Fpe fnfour fntwo))
              assertEqual "-2.0 ^ -2.0 =? "  (-0.25) (exec (Fpe fntwo fntwo))
              assertEqual "0.0 ^ 0.0 =? "    1.0     (exec (Fpe fzero fzero))
              assertEqual "4.0 ^ 0.0 =? "    1.0     (exec (Fpe ffour fzero))
              assertEqual "0.0 ^ 4.0 =? "    0.0     (exec (Fpe fzero ffour))
              assertEqual "0.0 ^ -2.0 =? "   err "can't be raised to a negative power"   (exec (Fpe fzero fntwo))
              assertEqual "-2.0 ^ 0.0 =? "   (-1.0)  (exec (FloatingPointDiv fntwo fzero))
              
              --  Integer Exponentiation 
              assertEqual "2 ** 4 =? "    16       (exec (Exp two four))
              assertEqual "4 ** 2 =? "    16       (exec (Exp four two))
              assertEqual "2 ** -1 =? "   0.5      (exec (Exp two none))
              assertEqual "-1 ** 2 =? "  (-1)      (exec (Exp none two))
              assertEqual "-4 ** -2 =? " (-0.625)  (exec (Exp nfour ntwo))
              assertEqual "-2 ** -2 =? "  (-0.25)  (exec (Exp ntwo ntwo))
              assertEqual "0 ** 0 =? "    1        (exec (Exp zero zero))
              assertEqual "4 ** 0 =? "    1        (exec (Exp four zero))
              assertEqual "0 ** 4 =? "    0        (exec (Exp zero four))
              assertEqual "0 ** -2 =? "   err "can't be raised to a negative power"   (exec (Exp zero ntwo))
              assertEqual "-2 ** 0 =? "   (-1)     (exec (Exp ntwo zero)),

         testCase "Compound Arithmetic" $
            do 
              assertEqual "2 + 4 * 3 =? "                  14    (exec (Plus two (Mult four three)))
              assertEqual "(2 + -4) * 3 =? "               (-6)  (exec (Mult (Plus two nfour) three))
              assertEqual "2 * 3 + 3 * 2 - 4 =? "          8     (exec (Minus (Plus (Mult two three) (Mult three two)) four))
              assertEqual "2 * (3 + 3) * (2 - 4) =? "      (-24) (exec (Mult (Mult two (Plus three three)) (Minus two four)))
              assertEqual "(4 * -4) // 2 =? "              (-8)  (exec (IntegerDiv (Mult four nfour) two))
              assertEqual "(2 // 2) + (4 // 1) - 0 =? "    5     (exec (Sub (Plus (IntegerDiv two two) (IntegerDiv four one)) zero))              
              assertEqual "(2 - 4) // 0 =? " err "you can't divide by zero" (exec(IntegerDiv (Minus two four)zero)),


         testCase "If Statements" $
            do 
              assertEqual "if 3 then 4 else 2 =? "        4  (exec (If three four two))
              assertEqual "if 0 then 1 else 4"            4  (exec (If zero one four))
              assertEqual "if 3 * 0 then 1 else 2  =? "   2  (exec (If (Mult three zero) one two))
              assertEqual "if 3 * 2 then 1 else 2  =? "   1  (exec (If (Mult three two) one two))
              assertEqual "if 9 // 3 then 2 else 5  =? "  2  (exec (If (IntegerDiv three zero) two five))
              assertEqual "if 0 // 8 then 2 else 5  =? "  5  (exec (If (Mult three two) two five)),

         testCase "Let Statements" $
            do 
              assertEqual "let x = 4 in x * 2 =? "                   8  (exec (Let "x" four (Mult (Var "x") two)))
              assertEqual "let x = 4 * -2 in x - 2 =? "              (-10)  (exec (Let "x" (Mult four ntwo) (Sub (Var "x") two)))
              assertEqual "let x = 2 in let y = x + 1 in y * 2 =? "  6  (exec (Let "x" two (Let "y" (Plus (Var "x") one)  (Mult (Var "y") two)))),
          
         testCase "Relational Operators" $
            do 
              -- Equal Test Cases for Int, Floats, Boolean, Char, Lists, Variables (Not Functions)
              
                          -- Int Test Cases

              assertBool "0 == 0 =? "        (exec (Eq zero zero))
              assertBool "1 == 1 =? "        (exec (Eq one one))
              assertBool "-1 == -1 =? "      (exec (Eq none none))
              assertBool "2 == 2 =? "        (exec (Eq two two))
              assertBool "-2 == -2 =? "      (exec (Eq ntwo ntwo))
              assertBool "3 == 3 =? "        (exec (Eq three three))
              assertBool "-3 == -3 =? "      (exec (Eq nthree nthree))
              assertBool "4 == 4 =? "        (exec (Eq four four))
              assertBool "-4 == -4 =? "      (exec (Eq nfour nfour))
              
                          -- Float Test Cases
              assertBool "0.0 == 0.0 =? "     (exec (Eq fzero fzero))
              assertBool "0.5 == 0.5 =? "     (exec (Eq fzeropointfive fzeropointfive))
              assertBool "-0.5 == -0.5 =? "   (exec (Eq fnzeropointfive fnzeropointfive))
              assertBool "1.0 == 1.0 =? "     (exec (Eq fone fone))
              assertBool "1.5 == 1.5 =? "     (exec (Eq fonepointfive fonepointfive)) 
              assertBool "-1.0 == -1.0 =? "   (exec (Eq fnone fnone))
              assertBool "-1.5 == -1.5 =? "   (exec (Eq fnonepointfive fnonepointfive)) 
              assertBool "2.0 == 2.0 =? "     (exec (Eq ftwo ftwo))
              assertBool "2.5 == 2.5 =? "     (exec (Eq ftwopointfive ftwopointfive)) 
              assertBool "-2.0 == -2.0 =? "   (exec (Eq fntwo fntwo))
              assertBool "-2.5 == -2.5 =? "   (exec (Eq fntwopointfive fntwopointfive))
              assertBool "3.0 == 3.0 =? "     (exec (Eq fthree fthree))
              assertBool "3.5 == 3.5 =? "     (exec (Eq fthreepointfive fthreepointfive))  
              assertBool "-3.0 == -3.0 =? "   (exec (Eq fnthree fnthree))
              assertBool "-3.5 == -3.5 =? "   (exec (Eq fnthreepointfive fnthreepointfive)) 
              assertBool "4.0 == 4.0 =? "     (exec (Eq ffour ffour))
              assertBool "-4.0 == -4.0 =? "   (exec (Eq fnfour fnfour))

              
                          -- Boolean Test Cases
              assertBool "T == T =? "   (exec (Eq t t))
              assertBool "F == F =? "   (exec (Eq f f))
              
                          --Char Test Cases
              
              assertBool "a == a =? " (exec (Eq (ValChar 'a') (ValChar 'a')))
              assertBool "b == b =? " (exec (Eq (ValChar 'b') (ValChar 'b')))
              assertBool "c == c =? " (exec (Eq (ValChar 'c') (ValChar 'c')))
              assertBool "d == d =? " (exec (Eq (ValChar 'd') (ValChar 'd')))
              assertBool "e == e =? " (exec (Eq (ValChar 'e') (ValChar 'e')))
              assertBool "f == f =? " (exec (Eq (ValChar 'f') (ValChar 'f')))
              assertBool "g == g =? " (exec (Eq (ValChar 'g') (ValChar 'g')))
              assertBool "h == h =? " (exec (Eq (ValChar 'h') (ValChar 'h')))
              assertBool "i == i =? " (exec (Eq (ValChar 'i') (ValChar 'i')))
              assertBool "j == j =? " (exec (Eq (ValChar 'j') (ValChar 'j')))
              assertBool "k == k =? " (exec (Eq (ValChar 'k') (ValChar 'k')))
              assertBool "l == l =? " (exec (Eq (ValChar 'l') (ValChar 'l')))
              assertBool "m == m =? " (exec (Eq (ValChar 'm') (ValChar 'm')))
              assertBool "n == n =? " (exec (Eq (ValChar 'n') (ValChar 'n')))
              assertBool "o == o =? " (exec (Eq (ValChar 'o') (ValChar 'o')))
              assertBool "p == p =? " (exec (Eq (ValChar 'p') (ValChar 'p')))
              assertBool "q == q =? " (exec (Eq (ValChar 'q') (ValChar 'q')))
              assertBool "r == r =? " (exec (Eq (ValChar 'r') (ValChar 'r')))
              assertBool "s == s =? " (exec (Eq (ValChar 's') (ValChar 's')))
              assertBool "t == t =? " (exec (Eq (ValChar 't') (ValChar 't')))
              assertBool "u == u =? " (exec (Eq (ValChar 'u') (ValChar 'u')))
              assertBool "v == v =? " (exec (Eq (ValChar 'v') (ValChar 'v')))
              assertBool "w == w =? " (exec (Eq (ValChar 'w') (ValChar 'w')))
              assertBool "x == x =? " (exec (Eq (ValChar 'x') (ValChar 'x')))
              assertBool "y == y =? " (exec (Eq (ValChar 'y') (ValChar 'y')))
              assertBool "z == z =? " (exec (Eq (ValChar 'z') (ValChar 'z')))

                          -- List Test Cases 
              assertBool "[0,1,2] == [0,1,2] =? "   (exec (Eq list1 list1))
              assertBool "[3,4,5] == [3,4,5] =? "   (exec (Eq list2 list2))
              assertBool "[0.0,1.0,2.0] == [0.0,1.0,2.0] =? "   (exec (Eq list3 list3))
              assertBool "[3.0,4.0,5.0] == [3.0,4.0,5.0] =? "   (exec (Eq list4 list4))
              assertBool "[True,False,True,False,True] == [True,False,True,False,True] =? "   (exec (Eq list5 list5))
              assertBool "[False,True,False,True,False] == [False,True,False,True,False] =? "   (exec (Eq list6 list6))
              assertBool "['a','b','c','d','e'] == ['a','b','c','d','e'] =? "   (exec (Eq list7 list7))
              assertBool "['e','d','c','b','a'] == ['e','d','c','b','a'] =? "   (exec (Eq list8 list8))
              

              
              
              -- Unequal Test Cases for Int, Floats, Boolean, Char, Lists, Variables (Not Functions)
                          -- Int Test Cases  

              assertBool "0 /= -4 =? " (exec (Neq zero nfour))
              assertBool "0 /= -3 =? " (exec (Neq zero nthree))
              assertBool "0 /= -2 =? " (exec (Neq zero ntwo))
              assertBool "0 /= -1 =? " (exec (Neq zero none))
              assertBool "0 /= 1 =? "  (exec (Neq zero one)) 
              assertBool "0 /= 2 =? "  (exec (Neq zero two))
              assertBool "0 /= 3 =? "  (exec (Neq zero three))
              assertBool "0 /= 4 =? "  (exec (Neq zero four))        
    
              assertBool "1 /= -4 =? " (exec (Neq one nfour))
              assertBool "1 /= -3 =? " (exec (Neq one nthree))
              assertBool "1 /= -2 =? " (exec (Neq one ntwo)) 
              assertBool "1 /= -1 =? " (exec (Neq one none))
              assertBool "1 /= 0 =? "  (exec (Neq one zero))
              assertBool "1 /= 2 =? "  (exec (Neq one two)) 
              assertBool "1 /= 3 =? "  (exec (Neq one three))
              assertBool "1 /= 4 =? "  (exec (Neq one four))
             
              assertBool "2 /= -4 =? " (exec (Neq two nfour))
              assertBool "2 /= -3 =? " (exec (Neq two nthree))
              assertBool "2 /= -2 =? " (exec (Neq two ntwo))
              assertBool "2 /= -1 =? " (exec (Neq two none)) 
              assertBool "2 /= 0 =? "  (exec (Neq two zero))
              assertBool "2 /= 1 =? "  (exec (Neq two one)) 
              assertBool "2 /= 3 =? "  (exec (Neq two three))
              assertBool "2 /= 4 =? "  (exec (Neq two four))

              assertBool "3 /= -4 =? " (exec (Neq three nfour))
              assertBool "3 /= -3 =? " (exec (Neq three nthree))
              assertBool "3 /= -2 =? " (exec (Neq three ntwo))
              assertBool "3 /= -1 =? " (exec (Neq three none)) 
              assertBool "3 /= 0 =? " (exec (Neq three zero))
              assertBool "3 /= 1 =? " (exec (Neq three one)) 
              assertBool "3 /= 2 =? " (exec (Neq three two))
              assertBool "3 /= 4 =? " (exec (Neq three four)) 
              
              assertBool "4 /= -4 =? " (exec (Neq four nfour))
              assertBool "4 /= -3 =? " (exec (Neq four nthree))
              assertBool "4 /= -2 =? " (exec (Neq four ntwo))
              assertBool "4 /= -1 =? " (exec (Neq four none)) 
              assertBool "4 /= 0 =? " (exec (Neq four zero))
              assertBool "4 /= 1 =? " (exec (Neq four one)) 
              assertBool "4 /= 2 =? " (exec (Neq four two))
              assertBool "4 /= 3 =? " (exec (Neq four three))
              
              
                          -- Float Test Cases
              assertBool "0.0 /= -4.5 =? " (exec (Neq fzero fnfourpointfive))
              assertBool "0.0 /= -4.0 =? " (exec (Neq fzero fnfour))
              assertBool "0.0 /= -3.5 =? " (exec (Neq fzero fnthreepointfive))
              assertBool "0.0 /= -3.0 =? " (exec (Neq fzero fnthree))
              assertBool "0.0 /= -2.5 =? " (exec (Neq fzero fntwopointfive))
              assertBool "0.0 /= -2.0 =? " (exec (Neq fzero fntwo))
              assertBool "0.0 /= -1.5 =? " (exec (Neq fzero fnonepointfive))
              assertBool "0.0 /= -1.0 =? " (exec (Neq fzero fnone))
              assertBool "0.0 /= -0.5 =? " (exec (Neq fzero fnzeropointfive))
              assertBool "0.0 /= 0.5 =? " (exec (Neq fzero fzeropointfive))
              assertBool "0.0 /= 1.0 =? " (exec (Neq fzero fone))
              assertBool "0.0 /= 1.5 =? " (exec (Neq fzero fonepointfive))
              assertBool "0.0 /= 2.0 =? " (exec (Neq fzero ftwo))
              ssertBool "0.0 /= 2.5 =? " (exec (Neq fzero ftwopointfive))
              assertBool "0.0 /= 3.0 =? " (exec (Neq fzero fthree))
              assertBool "0.0 /= 3.5 =? " (exec (Neq fzero fthreepointfive))
              assertBool "0.0 /= 4.0 =? " (exec (Neq fzero ffour)) 
              assertBool "0.0 /= 4.5 =? " (exec (Neq fzero ffourpointfive))
    
              assertBool "1.0 /= -4.5 =? " (exec (Neq fone fnfourpointfive))
              assertBool "1.0 /= -4.0 =? " (exec (Neq fone fnfour))
              assertBool "1.0 /= -3.5 =? " (exec (Neq fone fnthreepointfive))
              assertBool "1.0 /= -3.0 =? " (exec (Neq fone fnthree))
              assertBool "1.0 /= -2.5 =? " (exec (Neq fone fntwopointfive))
              assertBool "1.0 /= -2.0 =? " (exec (Neq fone fntwo))
              assertBool "1.0 /= -1.5 =? " (exec (Neq fone fnonepointfive))
              assertBool "1.0 /= -1.0 =? " (exec (Neq fone fnone))
              assertBool "1.0 /= -0.5 =? " (exec (Neq fone fnzeropointfive))
              assertBool "1.0 /= 0.5 =? " (exec (Neq fone fzeropointfive))
              assertBool "1.0 /= 0.0 =? " (exec (Neq fone zero))
              assertBool "1.0 /= 1.5 =? " (exec (Neq fone fonepointfive))
              assertBool "1.0 /= 2.0 =? " (exec (Neq fone ftwo))
              ssertBool "1.0 /= 2.5 =? " (exec (Neq fone ftwopointfive))
              assertBool "1.0 /= 3.0 =? " (exec (Neq fone fthree))
              assertBool "1.0 /= 3.5 =? " (exec (Neq fone fthreepointfive))
              assertBool "1.0 /= 4.0 =? " (exec (Neq fone ffour)) 
              assertBool "1.0 /= 4.5 =? " (exec (Neq fone ffourpointfive))  
             
              assertBool "2.0 /= -4.5 =? " (exec (Neq ftwo fnfourpointfive))
              assertBool "2.0 /= -4.0 =? " (exec (Neq ftwo fnfour))
              assertBool "2.0 /= -3.5 =? " (exec (Neq ftwo fnthreepointfive))
              assertBool "2.0 /= -3.0 =? " (exec (Neq ftwo fnthree))
              assertBool "2.0 /= -2.5 =? " (exec (Neq ftwo fntwopointfive))
              assertBool "2.0 /= -2.0 =? " (exec (Neq ftwo fntwo))
              assertBool "2.0 /= -1.5 =? " (exec (Neq ftwo fnonepointfive))
              assertBool "2.0 /= -1.0 =? " (exec (Neq ftwo fnone))
              assertBool "2.0 /= -0.5 =? " (exec (Neq ftwo fnzeropointfive))
              assertBool "2.0 /= 0.0 =? " (exec (Neq ftwo fzero))
              assertBool "2.0 /= 0.5 =? " (exec (Neq ftwo fzeropointfive))
              assertBool "2.0 /= 1.0 =? " (exec (Neq ftwo fone))
              assertBool "2.0 /= 1.5 =? " (exec (Neq ftwo fonepointfive))
              ssertBool  "2.0 /= 2.5 =? " (exec (Neq ftwo ftwopointfive))
              assertBool "2.0 /= 3.0 =? " (exec (Neq ftwo fthree))
              assertBool "2.0 /= 3.5 =? " (exec (Neq ftwo fthreepointfive))
              assertBool "2.0 /= 4.0 =? " (exec (Neq ftwo ffour)) 
              assertBool "2.0 /= 4.5 =? " (exec (Neq ftwo ffourpointfive))
              
             
              assertBool "3.0 /= -4.5 =? " (exec (Neq fthree fnfourpointfive))
              assertBool "3.0 /= -4.0 =? " (exec (Neq fthree fnfour))
              assertBool "3.0 /= -3.5 =? " (exec (Neq fthree fnthreepointfive))
              assertBool "3.0 /= -3.0 =? " (exec (Neq fthree fnthree))
              assertBool "3.0 /= -2.5 =? " (exec (Neq fthree fntwopointfive))
              assertBool "3.0 /= -2.0 =? " (exec (Neq fthree fntwo))
              assertBool "3.0 /= -1.5 =? " (exec (Neq fthree fnonepointfive))
              assertBool "3.0 /= -1.0 =? " (exec (Neq fthree fnone))
              assertBool "3.0 /= -0.5 =? " (exec (Neq fthree fnzeropointfive))
              assertBool "3.0 /= 0.0 =? " (exec (Neq fthree fzero))
              assertBool "3.0 /= 0.5 =? " (exec (Neq fthree fzeropointfive))
              assertBool "3.0 /= 1.0 =? " (exec (Neq fthree fone))
              assertBool "3.0 /= 1.5 =? " (exec (Neq fthree fonepointfive))
              assertBool "3.0 /= 2.0 =? " (exec (Neq fthree ftwo))
              ssertBool  "3.0 /= 2.5 =? " (exec (Neq fthree ftwopointfive))
              assertBool "3.0 /= 3.5 =? " (exec (Neq fthree fthreepointfive))
              assertBool "3.0 /= 4.0 =? " (exec (Neq fthree ffour)) 
              assertBool "3.0 /= 4.5 =? " (exec (Neq fthree ffourpointfive))
              
              assertBool "4.0 /= -4.5 =? " (exec (Neq ffour fnfourpointfive))
              assertBool "4.0 /= -4.0 =? " (exec (Neq ffour fnfour))
              assertBool "4.0 /= -3.5 =? " (exec (Neq ffour fnthreepointfive))
              assertBool "4.0 /= -3.0 =? " (exec (Neq ffour fnthree))
              assertBool "4.0 /= -2.5 =? " (exec (Neq ffour fntwopointfive))
              assertBool "4.0 /= -2.0 =? " (exec (Neq ffour fntwo))
              assertBool "4.0 /= -1.5 =? " (exec (Neq ffour fnonepointfive))
              assertBool "4.0 /= -1.0 =? " (exec (Neq ffour fnone))
              assertBool "4.0 /= -0.5 =? " (exec (Neq ffour fnzeropointfive))
              assertBool "4.0 /= 0.0 =? " (exec (Neq ffour fzero))
              assertBool "4.0 /= 0.5 =? " (exec (Neq ffour fzeropointfive))
              assertBool "4.0 /= 1.0 =? " (exec (Neq ffour fone))
              assertBool "4.0 /= 1.5 =? " (exec (Neq ffour fonepointfive))
              assertBool "4.0 /= 2.0 =? " (exec (Neq ffour ftwo))
              ssertBool  "4.0 /= 2.5 =? " (exec (Neq ffour ftwopointfive))
              assertBool "4.0 /= 3.0 =? " (exec (Neq ffour fthree))
              assertBool "4.0 /= 3.5 =? " (exec (Neq ffour fthreepointfive)) 
              assertBool "4.0 /= 4.5 =? " (exec (Neq ffour ffourpointfive))
              
                          -- Boolean Test Cases
              assertBool "T /= F =? " (exec (Neq t f))
              assertBool "F /= T =? " (exec (Neq f t))
              
                          --Char Test Cases
              
              assertBool "a /= b =? " (exec (Neq (ValChar 'a') (ValChar 'b')))
              assertBool "b /= c =? " (exec (Neq (ValChar 'b') (ValChar 'c')))
              assertBool "c /= d =? " (exec (Neq (ValChar 'c') (ValChar 'd')))
              assertBool "d /= e =? " (exec (Neq (ValChar 'd') (ValChar 'e')))
              assertBool "e /= f =? " (exec (Neq (ValChar 'e') (ValChar 'f')))
              assertBool "f /== g =? "(exec (Neq (ValChar 'f') (ValChar 'g')))
              assertBool "g /= h =? " (exec (Neq (ValChar 'g') (ValChar 'h')))
              assertBool "h /= i =? " (exec (Neq (ValChar 'h') (ValChar 'i')))
              assertBool "i /= j =? " (exec (Neq (ValChar 'i') (ValChar 'j')))
              assertBool "j /= k =? " (exec (Neq (ValChar 'j') (ValChar 'k')))
              assertBool "k /= l =? " (exec (Neq (ValChar 'k') (ValChar 'l')))
              assertBool "l /= m =? " (exec (Neq (ValChar 'l') (ValChar 'm')))
              assertBool "m /= n =? " (exec (Neq (ValChar 'm') (ValChar 'n')))
              assertBool "n /= o =? " (exec (Neq (ValChar 'n') (ValChar 'o')))
              assertBool "o /= p =? " (exec (Neq (ValChar 'o') (ValChar 'p')))
              assertBool "p /= q =? " (exec (Neq (ValChar 'p') (ValChar 'q')))
              assertBool "q /= r =? " (exec (Neq (ValChar 'q') (ValChar 'r')))
              assertBool "r /= s =? " (exec (Neq (ValChar 'r') (ValChar 's')))
              assertBool "s /= t =? " (exec (Neq (ValChar 's') (ValChar 't')))
              assertBool "t /= u =? " (exec (Neq (ValChar 't') (ValChar 'u')))
              assertBool "u /= v =? " (exec (Neq (ValChar 'u') (ValChar 'v')))
              assertBool "v /= w =? " (exec (Neq (ValChar 'v') (ValChar 'w')))
              assertBool "w /= x =? " (exec (Neq (ValChar 'w') (ValChar 'x')))
              assertBool "x /= y =? " (exec (Neq (ValChar 'x') (ValChar 'y')))
              assertBool "y /= z =? " (exec (Neq (ValChar 'y') (ValChar 'z')))
              assertBool "z /= a =? " (exec (Neq (ValChar 'z') (ValChar 'a')))

                          -- List Test Cases 
                          
              assertBool "[0,1,2] /= [3,4,5] =? "   (exec (Neq list1 list2))
              assertBool "[0,1,2] /= [3.0,4.0,5.0] =? "   (exec (Neq list1 list4))
              assertBool "[0,1,2] /= [True,False,True,False,True] =? "   (exec (Neq list1 list5))
              assertBool "[0,1,2] /= [False,True,False,True,False] =? "   (exec (Neq list1 list6))
              assertBool "[0,1,2] /= ['a','b','c','d','e'] =? "   (exec (Neq list1 list7))
              assertBool "[0,1,2] /= ['e','d','c','b','a'] =? "   (exec (Neq list1 list8))
              
              assertBool "[3,4,5] /= [0,1,2] =? "   (exec (Neq list2 list1))
              assertBool "[3,4,5] /= [0.0,1.0,2.0] =? "   (exec (Neq list2 list3))
              assertBool "[3,4,5] /= [True,False,True,False,True] =? "   (exec (Neq list2 list5))
              assertBool "[3,4,5] /= [False,True,False,True,False]  =? "   (exec (Neq list2 list6))
              assertBool "[3,4,5] /= ['a','b','c','d','e'] =? "   (exec (Neq list2 list7))
              assertBool "[3,4,5] /= ['e','d','c','b','a'] =? "   (exec (Neq list2 list8))
              
              assertBool "[True,False,True,False,True] /= [False,True,False,True,False] =? "   (exec (Neq list5 list6))
              assertBool "[True,False,True,False,True] /= [0,1,2] =? "   (exec (Neq list5 list7))
              assertBool "[True,False,True,False,True] /= [3,4,5] =? "   (exec (Neq list5 list8))
              assertBool "[True,False,True,False,True] /= [0.0,1.0,2.0] =? "   (exec (Neq list5 list1))
              assertBool "[True,False,True,False,True] /= [3.0,4.0,5.0] =? "   (exec (Neq list5 list2))
              assertBool "[True,False,True,False,True] /= ['e','d','c','b','a'] =? "   (exec (Neq list5 list3))
              assertBool "[True,False,True,False,True] /= ['e','d','c','b','a'] =? "   (exec (Neq list5 list4))

              assertBool "['a','b','c','d','e'] /= ['e','d','c','b','a'] =? "   (exec (Neq list7 list8))
              assertBool "['a','b','c','d','e'] /= [0,1,2] =? "   (exec (Neq list7 list1))
              assertBool "['a','b','c','d','e'] /= [3,4,5] =? "   (exec (Neq list7 list2))
              assertBool "['a','b','c','d','e'] /= [0.0,1.0,2.0] =? "   (exec (Neq list7 list3))
              assertBool "['a','b','c','d','e'] /= [3.0,4.0,5.0] =? "   (exec (Neq list7 list4))
              assertBool "['a','b','c','d','e'] /= [True,False,True,False,True] =? "   (exec (Neq list7 list5))
              assertBool "['a','b','c','d','e'] /= [False,True,False,True,False] =? "   (exec (Neq list7 list6))
  
              
                         
               
               -- Less-than Test Cases (Integers & Floats)
                           -- Int Test Cases
                           
              assertBool "-4 < -3 =? " (exec (Lt nfour nthree))
              assertBool "-4 < -2 =? " (exec (Lt nfour ntwo))
              assertBool "-4 < -1 =? " (exec (Lt nfour none)) 
              assertBool "-4 < 0 =? " (exec (Lt nfour zero))
              assertBool "-4 < 1 =? " (exec (Lt nfour one)) 
              assertBool "-4 < 2 =? " (exec (Lt nfour two))
              assertBool "-4 < 3 =? " (exec (Lt nfour three))
              assertBool "-4 < 4 =? " (exec (Lt nfour four)) 
              
              assertBool "-3 < -2 =? " (exec (Lt nthree ntwo))
              assertBool "-3 < -1 =? " (exec (Lt nthree none)) 
              assertBool "-3 < 0 =? " (exec (Lt nthree zero))
              assertBool "-3 < 1 =? " (exec (Lt nthree one)) 
              assertBool "-3 < 2 =? " (exec (Lt nthree two))
              assertBool "-3 < 3 =? " (exec (Lt nthree three))
              assertBool "-3 < 4 =? " (exec (Lt nthree four))             
                          
              assertBool "-2 < -1 =? " (exec (Lt ntwo none)) 
              assertBool "-2 < 0 =? " (exec (Lt ntwo zero))
              assertBool "-2 < 1 =? " (exec (Lt ntwo one))
              assertBool "-2 < 2 =? " (exec (Lt ntwo two))
              assertBool "-2 < 3 =? " (exec (Lt ntwo three))
              assertBool "-2 < 4 =? " (exec (Lt ntwo four))
              
              assertBool "-1 < 0 =? " (exec (Lt none zero))
              assertBool "-1 < 1 =? " (exec (Lt none one))
              assertBool "-1 < 2 =? " (exec (Lt none two)) 
              assertBool "-1 < 3 =? " (exec (Lt none three))
              assertBool "-1 < 4 =? " (exec (Lt none four))             
              
              assertBool "0 < 1 =? " (exec (Lt zero one)) 
              assertBool "0 < 2 =? " (exec (Lt zero two))
              assertBool "0 < 3 =? " (exec (Lt zero three))
              assertBool "0 < 4 =? " (exec (Lt zero four))        

              assertBool "1 < 2 =? " (exec (Lt one two)) 
              assertBool "1 < 3 =? " (exec (Lt one three))
              assertBool "1 < 4 =? " (exec (Lt one four))
             
              assertBool "2 < 3 =? " (exec (Lt two three))
              assertBool "2 < 4 =? " (exec (Lt two four))
              
              assertBool "3 < 4 =? " (exec (Lt three four)) 
              
                           -- Float Test Cases
                           
              assertBool "-4.0 < -3.5 =? " (exec (Lt fnthree fnthreepointfive)) 
              assertBool "-4.0 < -3.0 =? " (exec (Lt fnthree fnthree))             
              assertBool "-4.0 < -2.5 =? " (exec (Lt fnthree fntwopointfive))
              assertBool "-4.0 < -2.0 =? " (exec (Lt fnthree fntwo))
              assertBool "-4.0 < -1.5 =? " (exec (Lt fnthree fnonepointfive))
              assertBool "-4.0 < -1.0 =? " (exec (Lt fnthree fnone))
              assertBool "-4.0 < -0.5 =? " (exec (Lt fnthree fnzeropointfive))
              assertBool "-4.0 < 0.0 =? " (exec (Lt fnthree fzero))
              assertBool "-4.0 < 0.5 =? " (exec (Lt fnthree fzeropointfive))
              assertBool "-4.0 < 1.0 =? " (exec (Lt fnthree fone))
              assertBool "-4.0 < 1.5 =? " (exec (Lt fnthree fonepointfive))
              assertBool "-4.0 < 2.0 =? " (exec (Lt fnthree ftwo))
              ssertBool  "-4.0 < 2.5 =? " (exec (Lt fnthree ftwopointfive))
              assertBool "-4.0 < 3.0 =? " (exec (Lt fnthree fthree))
              assertBool "-4.0 < 3.5 =? " (exec (Lt fnthree fthreepointfive))
              assertBool "-4.0 < 4.0 =? " (exec (Lt fnthree ffour)) 
              assertBool "-4.0 < 4.5 =? " (exec (Lt fnthree ffourpointfive))
              
              assertBool "-3.0 < -2.5 =? " (exec (Lt fnthree fntwopointfive))
              assertBool "-3.0 < -2.0 =? " (exec (Lt fnthree fntwo))
              assertBool "-3.0 < -1.5 =? " (exec (Lt fnthree fnonepointfive))
              assertBool "-3.0 < -1.0 =? " (exec (Lt fnthree fnone))
              assertBool "-3.0 < -0.5 =? " (exec (Lt fnthree fnzeropointfive))
              assertBool "-3.0 < 0.0 =? " (exec (Lt fnthree fzero))
              assertBool "-3.0 < 0.5 =? " (exec (Lt fnthree fzeropointfive))
              assertBool "-3.0 < 1.0 =? " (exec (Lt fnthree fone))
              assertBool "-3.0 < 1.5 =? " (exec (Lt fnthree fonepointfive))
              assertBool "-3.0 < 2.0 =? " (exec (Lt fnthree ftwo))
              ssertBool  "-3.0 < 2.5 =? " (exec (Lt fnthree ftwopointfive))
              assertBool "-3.0 < 3.0 =? " (exec (Lt fnthree fthree))
              assertBool "-3.0 < 3.5 =? " (exec (Lt fnthree fthreepointfive))
              assertBool "-3.0 < 4.0 =? " (exec (Lt fnthree ffour)) 
              assertBool "-3.0 < 4.5 =? " (exec (Lt fnthree ffourpointfive))
              
              assertBool "-2.0 < -1.5 =? " (exec (Lt fntwo fnonepointfive))
              assertBool "-2.0 < -1.0 =? " (exec (Lt fntwo fnone))
              assertBool "-2.0 < -0.5 =? " (exec (Lt fntwo fnzeropointfive))
              assertBool "-2.0 < 0.0 =? " (exec (Lt fntwo fzero))
              assertBool "-2.0 < 0.5 =? " (exec (Lt fntwo fzeropointfive))
              assertBool "-2.0 < 1.0 =? " (exec (Lt fntwo fone))
              assertBool "-2.0 < 1.5 =? " (exec (Lt fntwo fonepointfive))
              assertBool "-2.0 < 2.0 =? " (exec (Lt fntwo ftwo))
              ssertBool  "-2.0 < 2.5 =? " (exec (Lt fntwo ftwopointfive))
              assertBool "-2.0 < 3.0 =? " (exec (Lt fntwo fthree))
              assertBool "-2.0 < 3.5 =? " (exec (Lt fntwo fthreepointfive))
              assertBool "-2.0 < 4.0 =? " (exec (Lt fntwo ffour)) 
              assertBool "-2.0 < 4.5 =? " (exec (Lt fntwo ffourpointfive))
              
              assertBool "-1.0 < -0.5 =? " (exec (Lt fnone fnzeropointfive))
              assertBool "-1.0 < 0.0 =? " (exec (Lt fnone fzero))
              assertBool "-1.0 < 0.5 =? " (exec (Lt fnone fzeropointfive))
              assertBool "-1.0 < 1.0 =? " (exec (Lt fnone fone))
              assertBool "-1.0 < 1.5 =? " (exec (Lt fnone fonepointfive))
              assertBool "-1.0 < 2.0 =? " (exec (Lt fnone ftwo))
              ssertBool  "-1.0 < 2.5 =? " (exec (Lt fnone ftwopointfive))
              assertBool "-1.0 < 3.0 =? " (exec (Lt fnone fthree))
              assertBool "-1.0 < 3.5 =? " (exec (Lt fnone fthreepointfive))
              assertBool "-1.0 < 4.0 =? " (exec (Lt fnone ffour)) 
              assertBool "-1.0 < 4.5 =? " (exec (Lt fnone ffourpointfive))
              
              assertBool "0.0 < 0.5 =? " (exec (Lt fzero fzeropointfive))
              assertBool "0.0 < 1.0 =? " (exec (Lt fzero fone))
              assertBool "0.0 < 1.5 =? " (exec (Lt fzero fonepointfive))
              assertBool "0.0 < 2.0 =? " (exec (Lt fzero ftwo))
              ssertBool  "0.0 < 2.5 =? " (exec (Lt fzero ftwopointfive))
              assertBool "0.0 < 3.0 =? " (exec (Lt fzero fthree))
              assertBool "0.0 < 3.5 =? " (exec (Lt fzero fthreepointfive))
              assertBool "0.0 < 4.0 =? " (exec (Lt fzero ffour)) 
              assertBool "0.0 < 4.5 =? " (exec (Lt fzero ffourpointfive))
    

              assertBool "1.0 < 1.5 =? " (exec (Lt fone fonepointfive))
              assertBool "1.0 < 2.0 =? " (exec (Lt fone ftwo))
              ssertBool "1.0 < 2.5 =? " (exec (Lt fone ftwopointfive))
              assertBool "1.0 < 3.0 =? " (exec (Lt fone fthree))
              assertBool "1.0 < 3.5 =? " (exec (Lt fone fthreepointfive))
              assertBool "1.0 < 4.0 =? " (exec (Lt fone ffour)) 
              assertBool "1.0 < 4.5 =? " (exec (Lt fone ffourpointfive))
              

              assertBool  "2.0 < 2.5 =? " (exec (Lt ftwo ftwopointfive))
              assertBool "2.0 < 3.0 =? " (exec (Lt ftwo fthree))
              assertBool "2.0 < 3.5 =? " (exec (Lt ftwo fthreepointfive))
              assertBool "2.0 < 4.0 =? " (exec (Lt ftwo ffour)) 
              assertBool "2.0 < 4.5 =? " (exec (Lt ftwo ffourpointfive))
              
             
             
              assertBool "3.0 < 3.5 =? " (exec (Lt fthree fthreepointfive))
              assertBool "3.0 < 4.0 =? " (exec (Lt fthree ffour)) 
              assertBool "3.0 < 4.5 =? " (exec (Lt fthree ffourpointfive))


              assertBool "4.0 < 4.5 =? " (exec (Lt ffour ffourpointfive))
               
               
               -- Less-than-or-equal Test Cases (Integers & Floats)
               
                          --Int Cases
              assertBool "-4 <= -4 =? " (exec (Lte nfour nfour))
              assertBool "-4 <= -3 =? " (exec (Lte nfour nthree))
              assertBool "-4 <= -2 =? " (exec (Lte nfour ntwo))
              assertBool "-4 <= -1 =? " (exec (Lte nfour none)) 
              assertBool "-4 <= 0 =? " (exec (Lte nfour zero))
              assertBool "-4 <= 1 =? " (exec (Lte nfour one)) 
              assertBool "-4 <= 2 =? " (exec (Lte nfour two))
              assertBool "-4 <= 3 =? " (exec (Lte nfour three))
              assertBool "-4 <= 4 =? " (exec (Lte nfour four)) 
              
              assertBool "-3 <= -3 =? " (exec (Lte nthree ntwo))
              assertBool "-3 <= -2 =? " (exec (Lte nthree ntwo))
              assertBool "-3 <= -1 =? " (exec (Lte nthree none)) 
              assertBool "-3 <= 0 =? " (exec (Lte nthree zero))
              assertBool "-3 <= 1 =? " (exec (Lte nthree one)) 
              assertBool "-3 <= 2 =? " (exec (Lte nthree two))
              assertBool "-3 <= 3 =? " (exec (Lte nthree three))
              assertBool "-3 <= 4 =? " (exec (Lte nthree four))             
                          
              assertBool "-2 <= -2 =? " (exec (Lte ntwo ntwo))
              assertBool "-2 <= -1 =? " (exec (Lte ntwo none)) 
              assertBool "-2 <= 0 =? " (exec (Lte ntwo zero))
              assertBool "-2 <= 1 =? " (exec (Lte ntwo one))
              assertBool "-2 <= 2 =? " (exec (Lte ntwo two))
              assertBool "-2 <= 3 =? " (exec (Lte ntwo three))
              assertBool "-2 <= 4 =? " (exec (Lte ntwo four))
              
              assertBool "-1 <= -1 =? " (exec (Lte none none))
              assertBool "-1 <= 0 =? " (exec (Lte none zero))
              assertBool "-1 <= 1 =? " (exec (Lte none one))
              assertBool "-1 <= 2 =? " (exec (Lte none two)) 
              assertBool "-1 <= 3 =? " (exec (Lte none three))
              assertBool "-1 <= 4 =? " (exec (Lte none four))             
              
              assertBool "0 <= 0 =? " (exec (Lte zero zero)) 
              assertBool "0 <= 1 =? " (exec (Lte zero one)) 
              assertBool "0 <= 2 =? " (exec (Lte zero two))
              assertBool "0 <= 3 =? " (exec (Lte zero three))
              assertBool "0 <= 4 =? " (exec (Lte zero four))        

              assertBool "1 <= 1 =? " (exec (Lte one one))
              assertBool "1 <= 2 =? " (exec (Lte one two)) 
              assertBool "1 <= 3 =? " (exec (Lte one three))
              assertBool "1 <= 4 =? " (exec (Lte one four))
             
              assertBool "2 <= 2 =? " (exec (Lte two two))
              assertBool "2 <= 3 =? " (exec (Lte two three))
              assertBool "2 <= 4 =? " (exec (Lte two four))
              
              assertBool "3 <= 3 =? " (exec (Lte three three))
              assertBool "3 <= 4 =? " (exec (Lte three four))
              
                       --Float Cases
                       
              assertBool "-4.0 < -4.0 =? " (exec (Lte fnthree fnfour))          
              assertBool "-4.0 < -3.5 =? " (exec (Lte fnthree fnthreepointfive)) 
              assertBool "-4.0 < -3.0 =? " (exec (Lte fnthree fnthree))             
              assertBool "-4.0 < -2.5 =? " (exec (Lte fnthree fntwopointfive))
              assertBool "-4.0 < -2.0 =? " (exec (Lte fnthree fntwo))
              assertBool "-4.0 < -1.5 =? " (exec (Lte fnthree fnonepointfive))
              assertBool "-4.0 < -1.0 =? " (exec (Lte fnthree fnone))
              assertBool "-4.0 < -0.5 =? " (exec (Lte fnthree fnzeropointfive))
              assertBool "-4.0 < 0.0 =? " (exec (Lte fnthree fzero))
              assertBool "-4.0 < 0.5 =? " (exec (Lte fnthree fzeropointfive))
              assertBool "-4.0 < 1.0 =? " (exec (Lte fnthree fone))
              assertBool "-4.0 < 1.5 =? " (exec (Lte fnthree fonepointfive))
              assertBool "-4.0 < 2.0 =? " (exec (Lte fnthree ftwo))
              ssertBool  "-4.0 < 2.5 =? " (exec (Lte fnthree ftwopointfive))
              assertBool "-4.0 < 3.0 =? " (exec (Lte fnthree fthree))
              assertBool "-4.0 < 3.5 =? " (exec (Lte fnthree fthreepointfive))
              assertBool "-4.0 < 4.0 =? " (exec (Lte fnthree ffour)) 
              assertBool "-4.0 < 4.5 =? " (exec (Lte fnthree ffourpointfive))
              
              assertBool "-3.0 < -3.0 =? " (exec (Lte fnthree fnthree))
              assertBool "-3.0 < -2.5 =? " (exec (Lte fnthree fntwopointfive))
              assertBool "-3.0 < -2.0 =? " (exec (Lte fnthree fntwo))
              assertBool "-3.0 < -1.5 =? " (exec (Lte fnthree fnonepointfive))
              assertBool "-3.0 < -1.0 =? " (exec (Lte fnthree fnone))
              assertBool "-3.0 < -0.5 =? " (exec (Lte fnthree fnzeropointfive))
              assertBool "-3.0 < 0.0 =? " (exec (Lte fnthree fzero))
              assertBool "-3.0 < 0.5 =? " (exec (Lte fnthree fzeropointfive))
              assertBool "-3.0 < 1.0 =? " (exec (Lte fnthree fone))
              assertBool "-3.0 < 1.5 =? " (exec (Lte fnthree fonepointfive))
              assertBool "-3.0 < 2.0 =? " (exec (Lte fnthree ftwo))
              ssertBool  "-3.0 < 2.5 =? " (exec (Lte fnthree ftwopointfive))
              assertBool "-3.0 < 3.0 =? " (exec (Lte fnthree fthree))
              assertBool "-3.0 < 3.5 =? " (exec (Lte fnthree fthreepointfive))
              assertBool "-3.0 < 4.0 =? " (exec (Lte fnthree ffour)) 
              assertBool "-3.0 < 4.5 =? " (exec (Lte fnthree ffourpointfive))
              
              assertBool "-2.0 < -2.0 =? " (exec (Lte fntwo fntwo))
              assertBool "-2.0 < -1.5 =? " (exec (Lte fntwo fnonepointfive))
              assertBool "-2.0 < -1.0 =? " (exec (Lte fntwo fnone))
              assertBool "-2.0 < -0.5 =? " (exec (Lte fntwo fnzeropointfive))
              assertBool "-2.0 < 0.0 =? " (exec (Lte fntwo fzero))
              assertBool "-2.0 < 0.5 =? " (exec (Lte fntwo fzeropointfive))
              assertBool "-2.0 < 1.0 =? " (exec (Lte fntwo fone))
              assertBool "-2.0 < 1.5 =? " (exec (Lte fntwo fonepointfive))
              assertBool "-2.0 < 2.0 =? " (exec (Lte fntwo ftwo))
              ssertBool  "-2.0 < 2.5 =? " (exec (Lte fntwo ftwopointfive))
              assertBool "-2.0 < 3.0 =? " (exec (Lte fntwo fthree))
              assertBool "-2.0 < 3.5 =? " (exec (Lte fntwo fthreepointfive))
              assertBool "-2.0 < 4.0 =? " (exec (Lte fntwo ffour)) 
              assertBool "-2.0 < 4.5 =? " (exec (Lte fntwo ffourpointfive))
              
              assertBool "-1.0 < -1.0 =? " (exec (Lte fnone fnone))
              assertBool "-1.0 < -0.5 =? " (exec (Lte fnone fnzeropointfive))
              assertBool "-1.0 < 0.0 =? " (exec (Lte fnone fzero))
              assertBool "-1.0 < 0.5 =? " (exec (Lte fnone fzeropointfive))
              assertBool "-1.0 < 1.0 =? " (exec (Lte fnone fone))
              assertBool "-1.0 < 1.5 =? " (exec (Lte fnone fonepointfive))
              assertBool "-1.0 < 2.0 =? " (exec (Lte fnone ftwo))
              ssertBool  "-1.0 < 2.5 =? " (exec (Lte fnone ftwopointfive))
              assertBool "-1.0 < 3.0 =? " (exec (Lte fnone fthree))
              assertBool "-1.0 < 3.5 =? " (exec (Lte fnone fthreepointfive))
              assertBool "-1.0 < 4.0 =? " (exec (Lte fnone ffour)) 
              assertBool "-1.0 < 4.5 =? " (exec (Lte fnone ffourpointfive))
              
              assertBool "0.0 < 0.0 =? " (exec (Lte fzero fzero))
              assertBool "0.0 < 0.5 =? " (exec (Lte fzero fzeropointfive))
              assertBool "0.0 < 1.0 =? " (exec (Lte fzero fone))
              assertBool "0.0 < 1.5 =? " (exec (Lte fzero fonepointfive))
              assertBool "0.0 < 2.0 =? " (exec (Lte fzero ftwo))
              ssertBool  "0.0 < 2.5 =? " (exec (Lte fzero ftwopointfive))
              assertBool "0.0 < 3.0 =? " (exec (Lte fzero fthree))
              assertBool "0.0 < 3.5 =? " (exec (Lte fzero fthreepointfive))
              assertBool "0.0 < 4.0 =? " (exec (Lte fzero ffour)) 
              assertBool "0.0 < 4.5 =? " (exec (Lte fzero ffourpointfive))
    
              assertBool "1.0 < 1.0 =? " (exec (Lte fone fone))
              assertBool "1.0 < 1.5 =? " (exec (Lte fone fonepointfive))
              assertBool "1.0 < 2.0 =? " (exec (Lte fone ftwo))
              ssertBool "1.0 < 2.5 =? " (exec (Lte fone ftwopointfive))
              assertBool "1.0 < 3.0 =? " (exec (Lte fone fthree))
              assertBool "1.0 < 3.5 =? " (exec (Lte fone fthreepointfive))
              assertBool "1.0 < 4.0 =? " (exec (Lte fone ffour)) 
              assertBool "1.0 < 4.5 =? " (exec (Lte fone ffourpointfive))
              

              assertBool "2.0 < 2.0 =? " (exec (Lte ftwo ftwo))
              assertBool "2.0 < 2.5 =? " (exec (Lte ftwo ftwopointfive))
              assertBool "2.0 < 3.0 =? " (exec (Lte ftwo fthree))
              assertBool "2.0 < 3.5 =? " (exec (Lte ftwo fthreepointfive))
              assertBool "2.0 < 4.0 =? " (exec (Lte ftwo ffour)) 
              assertBool "2.0 < 4.5 =? " (exec (Lte ftwo ffourpointfive))
              
             
             
              assertBool "3.0 < 3.0 =? " (exec (Lte fthree fthree))
              assertBool "3.0 < 3.5 =? " (exec (Lte fthree fthreepointfive))
              assertBool "3.0 < 4.0 =? " (exec (Lte fthree ffour)) 
              assertBool "3.0 < 4.5 =? " (exec (Lte fthree ffourpointfive))


              assertBool "4.0 < 4.0 =? " (exec (Lte ffour ffour))
              assertBool "4.0 < 4.5 =? " (exec (Lte ffour ffourpointfive))
              
               
               
               -- Greater-than Test (Integers & Floats) 
                            -- Int Test Cases
              
              assertBool "-3 > -4 =? " (exec (Gt nthree nfour))
                    
              assertBool "-2 > -4 =? " (exec (Gt ntwo nfour))
              assertBool "-2 > -3 =? " (exec (Gt ntwo nthree))
              
              assertBool "-1 > -4 =? " (exec (Gt none nfour))
              assertBool "-1 > -3 =? " (exec (Gt none nthree))
              assertBool "-1 > -2 =? " (exec (Gt none ntwo))
              
              assertBool "0 > -4 =? " (exec (Gt zero nfour))
              assertBool "0 > -3 =? " (exec (Gt zero nthree))
              assertBool "0 > -2 =? " (exec (Gt zero ntwo))
              assertBool "0 > -1 =? " (exec (Gt zero none))
              
              assertBool "1 > -4 =? " (exec (Gt one nfour))
              assertBool "1 > -3 =? " (exec (Gt one nthree))
              assertBool "1 > -2 =? " (exec (Gt one ntwo))
              assertBool "1 > -1 =? " (exec (Gt one none))
              assertBool "1 > 0 =? " (exec (Gt one zero))
              
              assertBool "2 > -4 =? " (exec (Gt two nfour))
              assertBool "2 > -3 =? " (exec (Gt two nthree))
              assertBool "2 > -2 =? " (exec (Gt two ntwo))
              assertBool "2 > -1 =? " (exec (Gt two none))
              assertBool "2 > 0 =? " (exec (Gt two zero))
              assertBool "2 > 1 =? " (exec (Gt two one))
              
              assertBool "3 > -4 =? " (exec (Gt three nfour))
              assertBool "3 > -3 =? " (exec (Gt three nthree))
              assertBool "3 > -2 =? " (exec (Gt three ntwo))
              assertBool "3 > -1 =? " (exec (Gt three none))
              assertBool "3 > 0 =? " (exec (Gt three zero))
              assertBool "3 > 1 =? " (exec (Gt three one))
              assertBool "3 > 2 =? " (exec (Gt three two))
              
              assertBool "4 > -4 =? " (exec (Gt four nfour))
              assertBool "4 > -3 =? " (exec (Gt four nthree))
              assertBool "4 > -2 =? " (exec (Gt four ntwo))
              assertBool "4 > -1 =? " (exec (Gt four none))
              assertBool "4 > 0 =? " (exec (Gt four zero))
              assertBool "4 > 1 =? " (exec (Gt four one))
              assertBool "4 > 2 =? " (exec (Gt four two))
              assertBool "4 > 3 =? " (exec (Gt four three))
              
                        --Float Cases
              
              assertBool "-3.0 > -4.0 =? " (exec (Gt fnthree fnfour))
              assertBool "-3.0 > -3.5 =? " (exec (Gt fnthree fnthreepointfive))
                    
              assertBool "-2.0 > -4.0 =? " (exec (Gt fntwo fnfour))
              assertBool "-2.0 > -3.5 =? " (exec (Gt fntwo fnthreepointfive))
              assertBool "-2.0 > -3.0 =? " (exec (Gt fntwo fnthree))
              assertBool "-2.0 > -2.5 =? " (exec (Gt fntwo fntwopointfive))
              
              assertBool "-1.0 > -4.0 =? " (exec (Gt fnone fnfour))
              assertBool "-1.0 > -3.5 =? " (exec (Gt fnone fnthreepointfive))
              assertBool "-1.0 > -3.0 =? " (exec (Gt fnone fnthree))
              assertBool "-1.0 > -2.5 =? " (exec (Gt fnone fntwopointfive))
              assertBool "-1.0 > -2.0 =? " (exec (Gt fnone fntwo))
              assertBool "-1.0 > -1.5 =? " (exec (Gt fnone fnonepointfive))
              
              assertBool "0.0 > -4.0 =? " (exec (Gt fzero fnfour))
              assertBool "0.0 > -3.5 =? " (exec (Gt fzero fnthreepointfive))
              assertBool "0.0 > -3.0 =? " (exec (Gt fzero fnthree))
              assertBool "0.0 > -2.5 =? " (exec (Gt fzero fntwopointfive))
              assertBool "0.0 > -2.0 =? " (exec (Gt fzero fntwo))
              assertBool "0.0 > -1.5 =? " (exec (Gt fzero fnonepointfive))
              assertBool "0.0 > -1.0 =? " (exec (Gt fzero fnone))
              assertBool "0.0 > -0.5 =? " (exec (Gt fzero fnzeropointfive))
              
              assertBool "1.0 > -4.0 =? " (exec (Gt fone fnfour))
              assertBool "1.0 > -3.5 =? " (exec (Gt fone fnthreepointfive))
              assertBool "1.0 > -3.0 =? " (exec (Gt fone fnthree))
              assertBool "1.0 > -2.5 =? " (exec (Gt fone fntwopointfive))
              assertBool "1.0 > -2.0 =? " (exec (Gt fone fntwo))
              assertBool "1.0 > -1.5 =? " (exec (Gt fone fnonepointfive))
              assertBool "1.0 > -1.0 =? " (exec (Gt fone fnone))
              assertBool "1.0 > -0.5 =? " (exec (Gt fone fnzeropointfive))
              assertBool "1.0 > 0.0 =? " (exec (Gt fone fzero))
              assertBool "1.0 > 0.5 =? " (exec (Gt fone fzeropointfive))
              
              assertBool "2.0 > -4.0 =? " (exec (Gt ftwo fnfour))
              assertBool "2.0 > -3.5 =? " (exec (Gt ftwo fnthreepointfive))
              assertBool "2.0 > -3.0 =? " (exec (Gt ftwo fnthree))
              assertBool "2.0 > -2.5 =? " (exec (Gt ftwo fntwopointfive))
              assertBool "2.0 > -2.0 =? " (exec (Gt ftwo fntwo))
              assertBool "2.0 > -1.5 =? " (exec (Gt ftwo fnonepointfive))
              assertBool "2.0 > -1.0 =? " (exec (Gt ftwo fnone))
              assertBool "2.0 > -0.5 =? " (exec (Gt ftwo fnzeropointfive))
              assertBool "2.0 > 0.0 =? " (exec (Gt ftwo fzero))
              assertBool "2.0 > 0.5 =? " (exec (Gt ftwo fzeropointfive))
              assertBool "2.0 > 1.0 =? " (exec (Gt ftwo fone))
              assertBool "2.0 > 1.5 =? " (exec (Gt ftwo fonepointfive))
              
              assertBool "3.0 > -4.0 =? " (exec (Gt fthree fnfour))
              assertBool "3.0 > -3.5 =? " (exec (Gt fthree fnthreepointfive))
              assertBool "3.0 > -3.0 =? " (exec (Gt fthree fnthree))
              assertBool "3.0 > -2.5 =? " (exec (Gt fthree fntwopointfive))
              assertBool "3.0 > -2.0 =? " (exec (Gt fthree fntwo))
              assertBool "3.0 > -1.5 =? " (exec (Gt fthree fnonepointfive))
              assertBool "3.0 > -1.0 =? " (exec (Gt fthree fnone))
              assertBool "3.0 > -0.5 =? " (exec (Gt fthree fnzeropointfive))
              assertBool "3.0 > 0.0 =? " (exec (Gt fthree fzero))
              assertBool "3.0 > 0.5 =? " (exec (Gt fthree fzeropointfive))
              assertBool "3.0 > 1.0 =? " (exec (Gt fthree fone))
              assertBool "3.0 > 1.5 =? " (exec (Gt fthree fonepointfive))
              assertBool "3.0 > 2.0 =? " (exec (Gt fthree ftwo))
              assertBool "3.0 > 2.5 =? " (exec (Gt fthree ftwopointfive))
              
              assertBool "4.0 > -4.0 =? " (exec (Gt ffour fnfour))
              assertBool "4.0 > -3.5 =? " (exec (Gt ffour fnthreepointfive))
              assertBool "4.0 > -3.0 =? " (exec (Gt ffour fnthree))
              assertBool "4.0 > -2.5 =? " (exec (Gt ffour fntwopointfive))
              assertBool "4.0 > -2.0 =? " (exec (Gt ffour fntwo))
              assertBool "4.0 > -1.5 =? " (exec (Gt ffour fnonepointfive))
              assertBool "4.0 > -1.0 =? " (exec (Gt ffour fnone))
              assertBool "4.0 > -0.5 =? " (exec (Gt ffour fnzeropointfive))
              assertBool "4.0 > 0.0 =? " (exec (Gt ffour fzero))
              assertBool "4.0 > 0.5 =? " (exec (Gt ffour fzeropointfive))
              assertBool "4.0 > 1.0 =? " (exec (Gt ffour fone))
              assertBool "4.0 > 1.5 =? " (exec (Gt ffour fonepointfive))
              assertBool "4.0 > 2.0 =? " (exec (Gt ffour ftwo))
              assertBool "4.0 > 2.5 =? " (exec (Gt ffour ftwopointfive))
              assertBool "4.0 > 3.0 =? " (exec (Gt ffour fthree))
              assertBool "4.0 > 3.5 =? " (exec (Gt ffour fthreepointfive))   
               
               -- Greater-than-or-equal Test Cases (Integers & Floats)
               
                             -- Int Test Cases
              
              assertBool "-3 >= -4 =? " (exec (Gte nthree nfour))
                    
              assertBool "-2 >= -4 =? " (exec (Gte ntwo nfour))
              assertBool "-2 >= -3 =? " (exec (Gte ntwo nthree))
              
              assertBool "-1 >= -4 =? " (exec (Gte none nfour))
              assertBool "-1 >= -3 =? " (exec (Gte none nthree))
              assertBool "-1 >= -2 =? " (exec (Gte none ntwo))
              
              assertBool "0 >= -4 =? " (exec (Gte zero nfour))
              assertBool "0 >= -3 =? " (exec (Gte zero nthree))
              assertBool "0 >= -2 =? " (exec (Gte zero ntwo))
              assertBool "0 >= -1 =? " (exec (Gte zero none))
              
              assertBool "1 >= -4 =? " (exec (Gte one nfour))
              assertBool "1 >= -3 =? " (exec (Gte one nthree))
              assertBool "1 >= -2 =? " (exec (Gte one ntwo))
              assertBool "1 >= -1 =? " (exec (Gte one none))
              assertBool "1 >= 0 =? " (exec (Gte one zero))
              
              assertBool "2 >= -4 =? " (exec (Gte two nfour))
              assertBool "2 >= -3 =? " (exec (Gte two nthree))
              assertBool "2 >= -2 =? " (exec (Gte two ntwo))
              assertBool "2 >= -1 =? " (exec (Gte two none))
              assertBool "2 >= 0 =? " (exec (Gte two zero))
              assertBool "2 >= 1 =? " (exec (Gte two one))
              
              assertBool "3 >= -4 =? " (exec (Gte three nfour))
              assertBool "3 >= -3 =? " (exec (Gte three nthree))
              assertBool "3 >= -2 =? " (exec (Gte three ntwo))
              assertBool "3 >= -1 =? " (exec (Gte three none))
              assertBool "3 >= 0 =? " (exec (Gte three zero))
              assertBool "3 >= 1 =? " (exec (Gte three one))
              assertBool "3 >= 2 =? " (exec (Gte three two))
              
              assertBool "4 >= -4 =? " (exec (Gte four nfour))
              assertBool "4 >= -3 =? " (exec (Gte four nthree))
              assertBool "4 >= -2 =? " (exec (Gte four ntwo))
              assertBool "4 >= -1 =? " (exec (Gte four none))
              assertBool "4 >= 0 =? " (exec (Gte four zero))
              assertBool "4 >= 1 =? " (exec (Gte four one))
              assertBool "4 >= 2 =? " (exec (Gte four two))
              assertBool "4 >= 3 =? " (exec (Gte four three))
              
                        --Float Cases
              
              assertBool "-3.0 >= -4.0 =? " (exec (Gte fnthree fnfour))
              assertBool "-3.0 >= -3.5 =? " (exec (Gte fnthree fnthreepointfive))
              assertBool "-3.0 >= -3.0 =? " (exec (Gte fnthree fnthree))
                    
              assertBool "-2.0 >= -4.0 =? " (exec (Gte fntwo fnfour))
              assertBool "-2.0 >= -3.5 =? " (exec (Gte fntwo fnthreepointfive))
              assertBool "-2.0 >= -3.0 =? " (exec (Gte fntwo fnthree))
              assertBool "-2.0 >= -2.5 =? " (exec (Gte fntwo fntwopointfive))
              assertBool "-2.0 >= -2.0 =? " (exec (Gte fntwo fntwo))
              
              assertBool "-1.0 >= -4.0 =? " (exec (Gte fnone fnfour))
              assertBool "-1.0 >= -3.5 =? " (exec (Gte fnone fnthreepointfive))
              assertBool "-1.0 >= -3.0 =? " (exec (Gte fnone fnthree))
              assertBool "-1.0 >= -2.5 =? " (exec (Gte fnone fntwopointfive))
              assertBool "-1.0 >= -2.0 =? " (exec (Gte fnone fntwo))
              assertBool "-1.0 >= -1.5 =? " (exec (Gte fnone fnonepointfive))
              assertBool "-1.0 >= -1.0 =? " (exec (Gte fnone fnone))
              
              assertBool "0.0 >= -4.0 =? " (exec (Gte fzero fnfour))
              assertBool "0.0 >= -3.5 =? " (exec (Gte fzero fnthreepointfive))
              assertBool "0.0 >= -3.0 =? " (exec (Gte fzero fnthree))
              assertBool "0.0 >= -2.5 =? " (exec (Gte fzero fntwopointfive))
              assertBool "0.0 >= -2.0 =? " (exec (Gte fzero fntwo))
              assertBool "0.0 >= -1.5 =? " (exec (Gte fzero fnonepointfive))
              assertBool "0.0 >= -1.0 =? " (exec (Gte fzero fnone))
              assertBool "0.0 >= -0.5 =? " (exec (Gte fzero fnzeropointfive))
              assertBool "0.0 >= 0.0 =? " (exec (Gte fzero fzero))
              
              assertBool "1.0 >= -4.0 =? " (exec (Gte fone fnfour))
              assertBool "1.0 >= -3.5 =? " (exec (Gte fone fnthreepointfive))
              assertBool "1.0 >= -3.0 =? " (exec (Gte fone fnthree))
              assertBool "1.0 >= -2.5 =? " (exec (Gte fone fntwopointfive))
              assertBool "1.0 >= -2.0 =? " (exec (Gte fone fntwo))
              assertBool "1.0 >= -1.5 =? " (exec (Gte fone fnonepointfive))
              assertBool "1.0 >= -1.0 =? " (exec (Gte fone fnone))
              assertBool "1.0 >= -0.5 =? " (exec (Gte fone fnzeropointfive))
              assertBool "1.0 >= 0.0 =? " (exec (Gte fone fzero))
              assertBool "1.0 >= 0.5 =? " (exec (Gte fone fzeropointfive))
              assertBool "1.0 >= 1.0 =? " (exec (Gte fone fone))
              
              assertBool "2.0 >= -4.0 =? " (exec (Gte ftwo fnfour))
              assertBool "2.0 >= -3.5 =? " (exec (Gte ftwo fnthreepointfive))
              assertBool "2.0 >= -3.0 =? " (exec (Gte ftwo fnthree))
              assertBool "2.0 >= -2.5 =? " (exec (Gte ftwo fntwopointfive))
              assertBool "2.0 >= -2.0 =? " (exec (Gte ftwo fntwo))
              assertBool "2.0 >= -1.5 =? " (exec (Gte ftwo fnonepointfive))
              assertBool "2.0 >= -1.0 =? " (exec (Gte ftwo fnone))
              assertBool "2.0 >= -0.5 =? " (exec (Gte ftwo fnzeropointfive))
              assertBool "2.0 >= 0.0 =? " (exec (Gte ftwo fzero))
              assertBool "2.0 >= 0.5 =? " (exec (Gte ftwo fzeropointfive))
              assertBool "2.0 >= 1.0 =? " (exec (Gte ftwo fone))
              assertBool "2.0 >= 1.5 =? " (exec (Gte ftwo fonepointfive))
              assertBool "2.0 >= 2.0 =? " (exec (Gte ftwo ftwo))
              
              assertBool "3.0 >= -4.0 =? " (exec (Gte fthree fnfour))
              assertBool "3.0 >= -3.5 =? " (exec (Gte fthree fnthreepointfive))
              assertBool "3.0 >= -3.0 =? " (exec (Gte fthree fnthree))
              assertBool "3.0 >= -2.5 =? " (exec (Gte fthree fntwopointfive))
              assertBool "3.0 >= -2.0 =? " (exec (Gte fthree fntwo))
              assertBool "3.0 >= -1.5 =? " (exec (Gte fthree fnonepointfive))
              assertBool "3.0 >= -1.0 =? " (exec (Gte fthree fnone))
              assertBool "3.0 >= -0.5 =? " (exec (Gte fthree fnzeropointfive))
              assertBool "3.0 >= 0.0 =? " (exec (Gte fthree fzero))
              assertBool "3.0 >= 0.5 =? " (exec (Gte fthree fzeropointfive))
              assertBool "3.0 >= 1.0 =? " (exec (Gte fthree fone))
              assertBool "3.0 >= 1.5 =? " (exec (Gte fthree fonepointfive))
              assertBool "3.0 >= 2.0 =? " (exec (Gte fthree ftwo))
              assertBool "3.0 >= 2.5 =? " (exec (Gte fthree ftwopointfive))
              assertBool "3.0 >= 3.0 =? " (exec (Gte fthree fthree))
              
              assertBool "4.0 >= -4.0 =? " (exec (Gte ffour fnfour))
              assertBool "4.0 >= -3.5 =? " (exec (Gte ffour fnthreepointfive))
              assertBool "4.0 >= -3.0 =? " (exec (Gte ffour fnthree))
              assertBool "4.0 >= -2.5 =? " (exec (Gte ffour fntwopointfive))
              assertBool "4.0 >= -2.0 =? " (exec (Gte ffour fntwo))
              assertBool "4.0 >= -1.5 =? " (exec (Gte ffour fnonepointfive))
              assertBool "4.0 >= -1.0 =? " (exec (Gte ffour fnone))
              assertBool "4.0 >= -0.5 =? " (exec (Gte ffour fnzeropointfive))
              assertBool "4.0 >= 0.0 =? " (exec (Gte ffour fzero))
              assertBool "4.0 >= 0.5 =? " (exec (Gte ffour fzeropointfive))
              assertBool "4.0 >= 1.0 =? " (exec (Gte ffour fone))
              assertBool "4.0 >= 1.5 =? " (exec (Gte ffour fonepointfive))
              assertBool "4.0 >= 2.0 =? " (exec (Gte ffour ftwo))
              assertBool "4.0 >= 2.5 =? " (exec (Gte ffour ftwopointfive))
              assertBool "4.0 >= 3.0 =? " (exec (Gte ffour fthree))
              assertBool "4.0 >= 3.5 =? " (exec (Gte ffour fthreepointfive))
              assertBool "4.0 >= 4.0 =? " (exec (Gte ffour ffour)),
              
              
 
         testCase "Boolean Operations" $
         
            do 
            -- Boolean And Test Cases
              assertBool "T & T =? " (exec (And t t))
              
            -- Boolean Or Test Cases
              assertBool "T || F =? " (exec (Or t f))
              assertBool "F || T =? " (exec (Or f t))
              assertBool "T || T =? " (exec (Or t t)),

         testCase "Prefix Operators and Functions" $
         
            do 
            -- Boolean Not Test Cases
              assertBool "not F =? " (exec (Not f))
              
            -- Unary Minus Test Cases for Ints and Floats
                  -- Int Cases
              assertEqual "- (-4) =? " 4 (exec (UMinus (nfour)))
              assertEqual "- (-3) =? " 3 (exec (UMinus (nthree)))
              assertEqual "- (-2) =? " 2 (exec (UMinus (ntwo)))
              assertEqual "- (-1) =? " 1 (exec (UMinus (none)))
              assertEqual "- (0) =? "  0 (exec (UMinus (zero)))
              assertEqual "- (1) =? " -1 (exec (UMinus (one)))
              assertEqual "- (2) =? " -2 (exec (UMinus (two)))
              assertEqual "- (3) =? " -3 (exec (UMinus (three)))
              assertEqual "- (4) =? " -4 (exec (UMinus (four)))
              
                    -- Float Cases
              assertEqual "- (-4.5) =? " 4.5 (exec (UMinus (fnfourpointfive)))
              assertEqual "- (-4.0) =? " 4.0 (exec (UMinus (fnfour)))
              assertEqual "- (-3.5) =? " 3.5 (exec (UMinus (fnthreepointfive)))
              assertEqual "- (-3.0) =? " 3.0 (exec (UMinus (fnthree)))
              assertEqual "- (-2.5) =? " 2.5 (exec (UMinus (fntwopointfive)))
              assertEqual "- (-2.0) =? " 2.0 (exec (UMinus (fntwo)))
              assertEqual "- (-1.5) =? " 1.5 (exec (UMinus (fnonepointfive)))
              assertEqual "- (-1.0) =? " 1.0 (exec (UMinus (fnone)))
              assertEqual "- (-0.5) =? " 0.5 (exec (UMinus (fnzeropointfive)))
              assertEqual "- (0.0) =? "  0.0 (exec (UMinus (fzero)))
              assertEqual "- (0.5) =? " (-0.5) (exec (UMinus (fzeropointfive)))
              assertEqual "- (1.0) =? " (-1.0) (exec (UMinus (fone)))
              assertEqual "- (1.5) =? " (-1.5) (exec (UMinus (fonepointfive)))
              assertEqual "- (2.0) =? " (-2.0) (exec (UMinus (ftwo)))
              assertEqual "- (2.5) =? " (-2.5) (exec (UMinus (ftwopointfive)))
              assertEqual "- (3.0) =? " (-3.0) (exec (UMinus (fthree)))
              assertEqual "- (-3.5) =? "(-3.5) (exec (UMinus (fthreepointfive)))
              assertEqual "- (4.0) =? " (-4.0) (exec (UMinus (ffour)))
              assertEqual "- (4.5) =? " (-4.5) (exec (UMinus (ffourpointfive)))
              
            -- Print Test Cases

              
         testCase "List Operations" $
         
            do 
            -- Cons Test Cases
            assertEqual "[1, 2] =?"  (exec (Cons (one) (Cons (two) Nil)))
            assertEqual "[1, 2, 3] =?"  (exec (Cons (one) (Cons (two) (Cons (three) Nil))))
            
            -- Concat Test Cases
            assertEqual "[0,1,2,3,4,5] =?" (exec (Concat (list1 list2)))
            assertEqual "[0.0,1.0,2.0,3.0,4.0,5.0] =?" (exec (Concat (list3 list4)))
            
            -- Index Test Cases
            assertEqual "[0,1,2] !! 0 =?" (exec (Index (list1 zero)))
            assertEqual "[0,1,2] !! 1 =?" (exec (Index (list1 one)))
            assertEqual "[0,1,2] !! 2 =?" (exec (Index (list1 two)))
            assertEqual "[3,4,5] !! 0 =?" (exec (Index (list2 zero)))
            assertEqual "[3,4,5] !! 1 =?" (exec (Index (list2 one)))
            assertEqual "[3,4,5] !! 2 =?" (exec (Index (list2 two)))
                                          ,

         testCase "Predefined Functions" $
         
            do 
            -- head function Test Cases 

              
            -- tail function Test Cases
             
              
            -- elem function Test Cases
            
            
            -- map function Test Cases
            
            
            -- filter function Test Cases

                                          ,
         
    ]
