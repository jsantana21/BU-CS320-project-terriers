module CheckTest where

import Test.Tasty (testGroup)
import Test.Tasty.HUnit (assertEqual, assertBool, testCase)
--import Test.Tasty.QuickCheck

import Ast
import Check

-- provide tests that show your check works

tests = testGroup "CheckTest"
[
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
fnfour = (ValFloat (-4.0))
fnfourpointfive = (ValFloat -4.5)

--Booleans
t = (ValBool True)
f = (ValBool False)

--Chars
b = (ValChar 'b')
c = (ValChar 'c')

--Lists
list1 = [zero, one, two]
list2 = [three, four, five]


checkTest = testGroup
      "Check Test"
      [
         testCase "Basic Arithmetic" $
            do
              -- Addition Test Cases

              assertEqual "False + False =? "  TypeError "can't add bools" (check(Plus f f))
              assertEqual "False + True =? "   TypeError "can't add bools" (check(Plus f t))
              assertEqual "True + False =? "   TypeError "can't add bools" (check(Plus t f))
              assertEqual "True + True =? "    TypeError "can't add bools" (check(Plus t t))
              assertEqual "'c' + -2 =? "       TypeError "can't add chars" (check(Plus c ntwo))
              assertEqual "'c' + True =?"      TypeError "can't add bools and chars" (check(Plus c t))
              assertEqual "True + 2 =?"        TypeError "can't add bools and ints" (check(Plus t two))
              assertEqual "False + 2 =?"       TypeError "can't add bools and ints" (check(Plus f two)) 
              assertEqual "True + 1.5 =?"      TypeError "can't add bools and floats" (check(Plus t fonepointfive))
              assertEqual "False + 'c' =?"     TypeError "can't add bools and chars" (check(Plus f c))                    


              --Subtraction Test Cases
              assertEqual "False - False =? "  TypeError "can't minus bools" (check(Minus f f))
              assertEqual "False - True =? "   TypeError "can't minus bools" (check(Minus f t))
              assertEqual "True - False =? "   TypeError "can't minus bools" (check(Minus t f))
              assertEqual "True - True =? "    TypeError "can't minus bools" (check(Minus t t))
              assertEqual "'c' - -2 =? "       TypeError "can't minus char" (check(Minus c ntwo))
              assertEqual "'c' - True =?"      TypeError "can't minus bools and chars" (check(Minus c t))
              assertEqual "True - 2 =?"        TypeError "can't minus bools and ints" (check(Minus t two))
              assertEqual "False - 2 =?"       TypeError "can't minus bools and ints" (check(Minus f two)) 
              assertEqual "True - 1.5 =?"      TypeError "can't minus bools and floats" (check(Minus t fonepointfive))
              assertEqual "False - 'c' =?"     TypeError "can't minus bools and chars" (check(Minus f c))        

              -- Multiplication Test Cases
              assertEqual "False * False =? "   TypeError "can't mult bools" (check(Mult f f))
              assertEqual "False * True =? "    TypeError "can't mult bools"  (check(Mult f t))
              assertEqual "True * False =? "    TypeError "can't mult bools"  (check(Mult t f))
              assertEqual "True * True =? "     TypeError "can't mult bools"  (check(Mult t t))
              assertEqual "'c' * 2 =? "         TypeError "can't mult char"  (check(Mult c two))
              assertEqual "'c' * True =?"      TypeError "can't mult bools and chars" (check(Mult c t))
              assertEqual "True * 2 =?"        TypeError "can't mult bools and ints" (check(Mult t two))
              assertEqual "False * 2 =?"       TypeError "can't mult bools and ints" (check(Mult f two)) 
              assertEqual "True * 1.5 =?"      TypeError "can't mult bools and floats" (check(Mult t fonepointfive))
              assertEqual "False * 'c' =?"     TypeError "can't mult bools and chars" (check(Mult f c))  
              assertEqual "'b' * 'c' =?"       TypeError "need floats not chars" (check(Mult b c))

              -- (Floating-Point) Division Test Cases
              assertEqual "False / False =? "   TypeError "can't div bools" (check(FloatingPointDiv f f))
              assertEqual "False / True =? "    TypeError "can't div bools" (check(FloatingPointDiv f t))
              assertEqual "True / False =? "    TypeError "can't div bools" (check(FloatingPointDiv t f))
              assertEqual "True / True =? "     TypeError "can't div bools" (check(FloatingPointDiv t t))
              assertEqual "'c' / -2 =? "        TypeError "can't div char" (check(FloatingPointDiv c ntwo))
              assertEqual "4 / 2 =? "           TypeError "need floats not ints" (check(FloatingPointDivv four two))
              assertEqual "True / 2 =?"        TypeError "need floats not bools and ints" (check(FloatingPointDiv t two))
              assertEqual "False / 2 =?"       TypeError "need floats not bools and ints" (check(FloatingPointDiv f two)) 
              assertEqual "True / 1.5 =?"      TypeError "need floats not bools" (check(FloatingPointDiv t fonepointfive))
              assertEqual "False / 'c' =?"     TypeError "need floats not bools and chars" (check(FloatingPointDiv f c))
              assertEqual "'b' / 'c' =?"       TypeError "need floats not chars" (check(FloatingPointDiv b c))

              -- Integer Division Test Cases
              assertEqual "False // False =? "   TypeError "can't div bools" (check(IntegerDiv f t))
              assertEqual "False // True =? "    TypeError "can't div bools" (check(IntegerDiv f t))
              assertEqual "True // False =? "    TypeError "can't div bools" (check(IntegerDiv t f))
              assertEqual "True // True =? "     TypeError "can't div bools" (check(IntegerDiv t t))
              assertEqual "'c' // -2 =? "        TypeError "can't div char" (check(IntegerDiv c ntwo))
              assertEqual "4.5 // 1.5 =? "       TypeError "need ints not floats" (check(IntegerDiv fonepointfive ffourpointfive))
              assertEqual "True // 2 =?"        TypeError "can't div bools"  (check(IntegerDiv t two))
              assertEqual "False // 2 =?"       TypeError "can't div bools" (check(IntegerDiv f two)) 
              assertEqual "True // 1.5 =?"      TypeError "can't div bools" (check(IntegerDiv t fonepointfive))
              assertEqual "False // 'c' =?"     TypeError "can't div bools and chars" (check(IntegerDiv f c)) 
              assertEqual "'b' // 'c' =?"       TypeError "can't div chars" (check(IntegerDiv b c))

              -- Modulus Test Cases (Integers ONLY)
              assertEqual "False % False =? "   TypeError "can't mod bools" (check(Mod f f))
              assertEqual "False % True =? "    TypeError "can't mod bools" (check(Mod f t))
              assertEqual "True % False =? "    TypeError "can't mod bools" (check(Mod t f))
              assertEqual "True % True =? "     TypeError "can't mod bools" (check(Mod t t))
              assertEqual "'c' % -2 =? "        TypeError "can't mod char with ints" (check(Mod c ntwo))
              assertEqual "4.5 % 1.5 =? "       TypeError "can't mod floats" (check(Mod fonepointfive ffourpointfive))
              assertEqual "True % 2 =?"        TypeError "can't mod bools"  (check(Mod t two))
              assertEqual "False % 2 =?"       TypeError "can't mod bools" (check(Mod f two)) 
              assertEqual "True % 1.5 =?"      TypeError "can't mod bools" (check(Mod t fonepointfive))
              assertEqual "False % 'c' =?"     TypeError "can't mod bools and chars" (check(Mod f c)) 
              assertEqual "'b' // 'c' =?"       TypeError "can't mod floats" (check(Mod b c))     



        testCase "Exponentiation for Integers and Floats"  $
            do
              --  Floating-Point Exponentiation
              assertEqual "False ** False =? "    TypeError "can't exp bools" (check(Fpe f f))
              assertEqual "False ** True =? "    TypeError "can't exp bools" (check(Fpe f t))
              assertEqual "True ** False =? "    TypeError "can't exp bools" (check(Fpe t f))
              assertEqual "True ** True =? "     TypeError "can't exp bools" (check(Fpe t t))
              assertEqual "'c' ** -2 =? "        TypeError "can't exp char" (check(Fpe c ntwo))          
              assertEqual "True ** 2 =?"        TypeError "can't exp bools"  (check(Fpe t two))
              assertEqual "False ** 2 =?"       TypeError "can't exp bools" (check(Fpe f two)) 
              assertEqual "True ** 1.5 =?"      TypeError "can't exp bools" (check(Fpe t fonepointfive))
              assertEqual "False ** 'c' =?"     TypeError "can't exp bools and chars" (check(Fpe f c))
              assertEqual "'b' // 'c' =?"       TypeError "can't exp floats" (check(Fpe b c)) 


              --  Integer Exponentiation
              assertEqual "False ^False =? "    TypeError "can't exp bools" (check(Exp f f))
              assertEqual "False ^ True =? "    TypeError "can't exp bools" (check(Exp f t))
              assertEqual "True ^ False =? "    TypeError "can't exp bools" (check(Exp t f))
              assertEqual "True ^ True =? "     TypeError "can't exp bools" (check(Exp t t))
              assertEqual "'c' ^ -2 =? "        TypeError "can't exp char" (check(Exp c ntwo))
              assertEqual "True ^ 2 =?"        TypeError "can't exp bools"  (check(Exp t two))
              assertEqual "False ^ 2 =?"       TypeError "can't exp bools" (check(Exp f two)) 
              assertEqual "True ^ 1.5 =?"      TypeError "can't exp bools" (check(Exp t fonepointfive))
              assertEqual "False ^ 'c' =?"     TypeError "can't exp bools and chars" (check(Exp f c)) 


         testCase "Let Statements" $
            do
              assertEqual "let x = 4 in y * 2 =? "        UndefinedVarUse "y undefined" (check(Let "x" four (Mult (Var "y") two)))
              assertEqual "let x = 4 * -2 in y - 2 =? "   UndefinedVarUse "y undefined" (check(Let "x" (Mult four ntwo) (Sub (Var "y") two)))


         testCase "Relational Operators" $
            do
              -- Equal Test Cases for Int, Floats, Boolean, Char, Lists

              -- Eq
              assertEqual "0 == False =? "     TypeError "can't eq int with bool" (check(Eq zero f))
              assertEqual "1 == 'c' =? "       TypeError "can't eq int with char" (check(Eq one c))
              assertEqual "3 == [0,1,2] =? "   TypeError "can't eq int with a list" (check(Eq three list1))
              assertEqual "0.0 == False =? "     TypeError "can't eq float with bool" (check(Eq fzero f))
              assertEqual "1.0 == 'c' =? "       TypeError "can't eq float with char" (check(Eq fone c))
              assertEqual "3.0 == [0,1,2] =? "   TypeError "can't eq float with a list" (check(Eq fthree list1))
              assertEqual "True == 3 =? "    TypeError "can't eq bool with int" (check(Eq t three))
              assertEqual "True == 2.5 =? "    TypeError "can't eq bool with float" (check(Eq t ftwopointfive))
              assertEqual "False == 'c' =? "   TypeError "can't eq bool with char" (check(Eq f c))
              assertEqual "False == [0,1,2] =? " TypeError "can't eq bool with a list" (check(Eq f list1))
              assertEqual "'b' == 1 =? "       TypeError "can't eq char with int" (check(Eq b one))
              assertEqual "'b' == 2.5 =? "       TypeError "can't eq char with float" (check(Eq b ftwopointfive))
              assertEqual "'b' == True =? "    TypeError "can't eq char with bool" (check(Eq b t))
              assertEqual "'b' == [0,1,2] =? "    TypeError "can't eq char with list" (check(Eq b list1))
              assertEqual "[3,4,5] == 1 =? "     TypeError "can't eq list with int" (check(Eq list2 one))
              assertEqual "[3,4,5] == 2.5 =? "   TypeError "can't eq list with float" (check(Eq list2 ftwopointfive))
              assertEqual "[3,4,5] == True =? "  TypeError "can't eq list with bool" (check(Eq list2b t))
              assertEqual "[3,4,5] == 'c' =? "   TypeError "can't eq list with char" (check(Eq list2 c))



              -- NeqInt
              
              assertEqual "0 /= False =? "     TypeError "can't neq int with bool" (check(Neq zero f))
              assertEqual "1 /= 'c' =? "       TypeError "can't neq int with char" (check(Neq one c))
              assertEqual "3 /= [0,1,2] =? "   TypeError "can't neq int with a list" (check(Neq three list1))
              assertEqual "0.0 /= False =? "     TypeError "can't neq float with bool" (check(Neq fzero f))
              assertEqual "1.0 /= 'c' =? "       TypeError "can't neq float with char" (check(Neq fone c))
              assertEqual "3.0 /= [0,1,2] =? "   TypeError "can't neq float with a list" (check(Neq fthree list1))
              assertEqual "True /= 3 =? "    TypeError "can't neq bool with int" (check(Neq t three))
              assertEqual "True /= 2.5 =? "    TypeError "can't neq bool with float" (check(Neq t ftwopointfive))
              assertEqual "False /= 'c' =? "   TypeError "can't neq bool with char" (check(Neq f c))
              assertEqual "False /= [0,1,2] =? " TypeError "can't neq bool with a list" (check(Neq f list1))
              assertEqual "'b' /= 1 =? "       TypeError "can't neq char with int" (check(Neq b one))
              assertEqual "'b' /= 2.5 =? "       TypeError "can't neq char with float" (check(Neq b ftwopointfive))
              assertEqual "'b' /= True =? "    TypeError "can't neq char with bool" (check(Neq b t))
              assertEqual "'b' /= [0,1,2] =? "    TypeError "can't neq char with list" (check(Neq b list1))
              assertEqual "[3,4,5] /= 1 =? "     TypeError "can't neq list with int" (check(Neq list2 one))
              assertEqual "[3,4,5] /= 2.5 =? "   TypeError "can't neq list with float" (check(Neq list2 ftwopointfive))
              assertEqual "[3,4,5] /= True =? "  TypeError "can't neq list with bool" (check(Neq list2b t))
              assertEqual "[3,4,5] /= 'c' =? "   TypeError "can't neq list with char" (check(Neq list2 c))



             --Lt

             assertEqual "0 < False =? "       TypeError "can't lt int with bool" (check(Lt zero f))
             assertEqual "1 < 'c' =? "         TypeError "can't lt int with char" (check(Lt one c))
             assertEqual "3 < [0,1,2] =? "     TypeError "can't lt int with a list" (check(Lt three list1))
             assertEqual "0.0 < False =? "     TypeError "can't lt float with bool" (check(Lt fzero f))
             assertEqual "1.0 < 'c' =? "       TypeError "can't lt float with char" (check(Lt fone c))
             assertEqual "3.0 < [0,1,2] =? "   TypeError "can't lt float with a list" (check(Lt fthree list1))
             assertEqual "True < 3 =? "        TypeError "can't lt bool with int" (check(Lt t three))
             assertEqual "True < 2.5 =? "      TypeError "can't lt bool with float" (check(Lt t ftwopointfive))
             assertEqual "False < 'c' =? "     TypeError "can't lt bool with char" (check(Lt f c))
             assertEqual "False < [0,1,2] =? " TypeError "can't lt bool with a list" (check(Lt f list1))
             assertEqual "'b' < 1 =? "         TypeError "can't lt char with int" (check(Lt b one))
             assertEqual "'b' < 2.5 =? "       TypeError "can't lt char with float" (check(Lt b ftwopointfive))
             assertEqual "'b' < True =? "      TypeError "can't lt char with bool" (check(Lt b t))
             assertEqual "'b' < [0,1,2] =? "   TypeError "can't lt char with list" (check(Lt b list1))
             assertEqual "[3,4,5] < 1 =? "     TypeError "can't lt list with int" (check(Lt list2 one))
             assertEqual "[3,4,5] < 2.5 =? "   TypeError "can't lt list with float" (check(Lt list2 ftwopointfive))
             assertEqual "[3,4,5] < True =? "  TypeError "can't lt list with bool" (check(Lt list2b t))
             assertEqual "[3,4,5] < 'c' =? "   TypeError "can't lt list with char" (check(Lt list2 c))

             -- Lte

             assertEqual "0 <= False =? "       TypeError "can't lte int with bool" (check(Lte zero f))
             assertEqual "1 <= 'c' =? "         TypeError "can't lte int with char" (check(Lte one c))
             assertEqual "3 <= [0,1,2] =? "     TypeError "can't lte int with a list" (check(Lte three list1))
             assertEqual "0.0 <= False =? "     TypeError "can't lte float with bool" (check(Lte fzero f))
             assertEqual "1.0 <= 'c' =? "       TypeError "can't lte float with char" (check(Lte fone c))
             assertEqual "3.0 <= [0,1,2] =? "   TypeError "can't lte float with a list" (check(Lte fthree list1))
             assertEqual "True <= 3 =? "        TypeError "can't lte bool with int" (check(Lte t three))
             assertEqual "True <= 2.5 =? "      TypeError "can't lte bool with float" (check(Lte t ftwopointfive))
             assertEqual "False <= 'c' =? "     TypeError "can't lte bool with char" (check(Lte f c))
             assertEqual "False <= [0,1,2] =? " TypeError "can't lte bool with a list" (check(Lte f list1))
             assertEqual "'b' <= 1 =? "         TypeError "can't lte char with int" (check(Lte b one))
             assertEqual "'b' <= 2.5 =? "       TypeError "can't lte char with float" (check(Lte b ftwopointfive))
             assertEqual "'b' <= True =? "      TypeError "can't lte char with bool" (check(Lte b t))
             assertEqual "'b' <= [0,1,2] =? "   TypeError "can't lte char with list" (check(Lte b list1))
             assertEqual "[3,4,5] <= 1 =? "     TypeError "can't lte list with int" (check(Lte list2 one))
             assertEqual "[3,4,5] <= 2.5 =? "   TypeError "can't lte list with float" (check(Lte list2 ftwopointfive))
             assertEqual "[3,4,5] <= True =? "  TypeError "can't lte list with bool" (check(Lte list2b t))
             assertEqual "[3,4,5] <= 'c' =? "   TypeError "can't lte list with char" (check(Lte list2 c))
             
             


             -- Gt

             assertEqual "0 > False =? "       TypeError "can't gt int with bool" (check(Gt zero f))
             assertEqual "1 > 'c' =? "         TypeError "can't gt int with char" (check(Gt one c))
             assertEqual "3 > [0,1,2] =? "     TypeError "can't gt int with a list" (check(Gt three list1))
             assertEqual "0.0 > False =? "     TypeError "can't gt float with bool" (check(Gt fzero f))
             assertEqual "1.0 > 'c' =? "       TypeError "can't gt float with char" (check(Gt fone c))
             assertEqual "3.0 > [0,1,2] =? "   TypeError "can't gt float with a list" (check(Gt fthree list1))
             assertEqual "True > 3 =? "        TypeError "can't gt bool with int" (check(Gt t three))
             assertEqual "True > 2.5 =? "      TypeError "can't gt bool with float" (check(Gt t ftwopointfive))
             assertEqual "False > 'c' =? "     TypeError "can't gt bool with char" (check(Gt f c))
             assertEqual "False > [0,1,2] =? " TypeError "can't gt bool with a list" (check(Gt f list1))
             assertEqual "'b' > 1 =? "         TypeError "can't gt char with int" (check(Gt b one))
             assertEqual "'b' > 2.5 =? "       TypeError "can't gt char with float" (check(Gt b ftwopointfive))
             assertEqual "'b' > True =? "      TypeError "can't gt char with bool" (check(Gt b t))
             assertEqual "'b' > [0,1,2] =? "   TypeError "can't gt char with list" (check(Gt b list1))
             assertEqual "[3,4,5] > 1 =? "     TypeError "can't gte list with int" (check(Gt list2 one))
             assertEqual "[3,4,5] > 2.5 =? "   TypeError "can't gte list with float" (check(Gt list2 ftwopointfive))
             assertEqual "[3,4,5] > True =? "  TypeError "can't gte list with bool" (check(Gt list2b t))
             assertEqual "[3,4,5] > 'c' =? "   TypeError "can't gte list with char" (check(Gt list2 c))
             
             Gte

             --Gte

             assertEqual "0 >= False =? "       TypeError "can't gte int with bool" (check(Gte zero f))
             assertEqual "1 >= 'c' =? "         TypeError "can't gte int with char" (check(Gte one c))
             assertEqual "3 >= [0,1,2] =? "     TypeError "can't gte int with a list" (check(Gte three list1))
             assertEqual "0.0 >= False =? "     TypeError "can't gte float with bool" (check(Gte fzero f))
             assertEqual "1.0 >= 'c' =? "       TypeError "can't gte float with char" (check(Gte fone c))
             assertEqual "3.0 >= [0,1,2] =? "   TypeError "can't gte float with a list" (check(Gte fthree list1))
             assertEqual "True >= 3 =? "        TypeError "can't gte bool with int" (check(Gte t three))
             assertEqual "True >= 2.5 =? "      TypeError "can't gte bool with float" (check(Gte t ftwopointfive))
             assertEqual "False >= 'c' =? "     TypeError "can't gte bool with char" (check(Gte f c))
             assertEqual "False >= [0,1,2] =? " TypeError "can't gte bool with a list" (check(Gte f list1))
             assertEqual "'b' >= 1 =? "         TypeError "can't gte char with int" (check(Gte b one))
             assertEqual "'b' >= 2.5 =? "       TypeError "can't gte char with float" (check(Gte b ftwopointfive))
             assertEqual "'b' >= True =? "      TypeError "can't gte char with bool" (check(Gte b t))
             assertEqual "'b' >= [0,1,2] =? "   TypeError "can't gte char with list" (check(Gte b list1))
             assertEqual "[3,4,5] >= 1 =? "     TypeError "can't gte list with int" (check(Gte list2 one))
             assertEqual "[3,4,5] >= 2.5 =? "   TypeError "can't gte list with float" (check(Gte list2 ftwopointfive))
             assertEqual "[3,4,5] >= True =? "  TypeError "can't gte list with bool" (check(Gte list2b t))
             assertEqual "[3,4,5] >= 'c' =? "   TypeError "can't gte list with char" (check(Gte list2 c))
             
             


             -- And
             
             assertEqual "0 && False =? "       TypeError "can't and int with bool" (check(And zero f))
             assertEqual "1 && 'c' =? "         TypeError "can't and int with char" (check(And one c))
             assertEqual "3 && [0,1,2] =? "     TypeError "can't and int with a list" (check(And three list1))
             assertEqual "0.0 && False =? "     TypeError "can't and float with bool" (check(And fzero f))
             assertEqual "1.0 && 'c' =? "       TypeError "can't and float with char" (check(And fone c))
             assertEqual "3.0 && [0,1,2] =? "   TypeError "can't and float with a list" (check(And fthree list1))
             assertEqual "True && 3 =? "        TypeError "can't and bool with int" (check(And t three))
             assertEqual "True && 2.5 =? "      TypeError "can't and bool with float" (check(And t ftwopointfive))
             assertEqual "False && 'c' =? "     TypeError "can't and bool with char" (check(And f c))
             assertEqual "False && [0,1,2] =? " TypeError "can't and bool with a list" (check(And f list1))
             assertEqual "'b' && 1 =? "         TypeError "can't and char with int" (check(And b one))
             assertEqual "'b' && 2.5 =? "       TypeError "can't and char with float" (check(And b ftwopointfive))
             assertEqual "'b' && True =? "      TypeError "can't and char with bool" (check(And b t))
             assertEqual "'b' && [0,1,2] =? "   TypeError "can't and char with list" (check(And b list1))       
             assertEqual "[3,4,5] && 1 =? "     TypeError "can't and list with int" (check(And list2 one))
             assertEqual "[3,4,5] && 2.5 =? "   TypeError "can't and list with float" (check(And list2 ftwopointfive))
             assertEqual "[3,4,5] && True =? "  TypeError "can't and list with bool" (check(And list2b t))
             assertEqual "[3,4,5] && 'c' =? "   TypeError "can't and list with char" (check(And list2 c))

             -- Or
 
             assertEqual "0 || False =? "       TypeError "can't or int with bool" (check(Or zero f))
             assertEqual "1 || 'c' =? "         TypeError "can't or int with char" (check(Or one c))
             assertEqual "3 || [0,1,2] =? "     TypeError "can't or int with a list" (check(Or three list1))
             assertEqual "0.0 || False =? "     TypeError "can't or float with bool" (check(Or fzero f))
             assertEqual "1.0 || 'c' =? "       TypeError "can't or float with char" (check(Or fone c))
             assertEqual "3.0 || [0,1,2] =? "   TypeError "can't or float with a list" (check(Or fthree list1))
             assertEqual "True || 3 =? "        TypeError "can't or bool with int" (check(Or t three))
             assertEqual "True || 2.5 =? "      TypeError "can't or bool with float" (check(Or t ftwopointfive))
             assertEqual "False || 'c' =? "     TypeError "can't or bool with char" (check(Or f c))
             assertEqual "False || [0,1,2] =? " TypeError "can't or bool with a list" (check(Or f list1))
             assertEqual "'b' || 1 =? "         TypeError "can't or char with int" (check(Or b one))
             assertEqual "'b' || 2.5 =? "       TypeError "can't or char with float" (check(Or b ftwopointfive))
             assertEqual "'b' || True =? "      TypeError "can't or char with bool" (check(Or b t))
             assertEqual "'b' || [0,1,2] =? "   TypeError "can't or char with list" (check(Or b list1))
             assertEqual "[3,4,5] || 1 =? "         TypeError "can't or list with int" (check(Or list2 one))
             assertEqual "[3,4,5] || 2.5 =? "       TypeError "can't or list with float" (check(Or list2 ftwopointfive))
             assertEqual "[3,4,5] || True =? "      TypeError "can't or list with bool" (check(Or list2b t))
             assertEqual "[3,4,5] || 'c' =? "       TypeError "can't or list with char" (check(Or list2 c))

             -- Not

             assertEqual "!0 =?"       TypeError "can't not int" (check( Not zero ))
             assertEqual "! 'c' =? "   TypeError "can't not char" (check( Not c))
             assertEqual "!2.5 =? "    TypeError "can't not float" (check( Not ftwopointfive))
             assertEqual "![3,4,5] =? "    TypeError "can't not list" (check( Not list2))

             -- ValBool

             assertEqual " 5 =?"          TypeError "int not a bool" (check(ValBool five))
             assertEqual " 4.0 =?"        TypeError "float not a bool" (check(ValBool ffour))
             assertEqual " 'b' =? "       TypeError "char not a bool" (check(ValBool b))
             assertEqual " [0,1,2] =? "   TypeError "list not a bool" (check(ValBool list1))
             

             -- ValInt

             assertEqual " True =?"       TypeError "bool not a int" (check(ValInt t))
             assertEqual " 4.0 =?"        TypeError "float not a int" (check(ValInt ffour))
             assertEqual " 'b' =? "       TypeError "char not a int" (check(ValInt b))
             assertEqual " [0,1,2] =? "   TypeError "list not a int" (check(ValInt list1))

             -- ValChar

             assertEqual " True =?"       TypeError "bool not a char" (check(ValChar t))
             assertEqual " 4.0 =?"        TypeError "float not a char" (check(ValChar ffour))
             assertEqual " 1 =? "         TypeError "int not a char" (check(ValChar one))
             assertEqual " [0,1,2] =? "   TypeError "list not a char" (check(ValChar list1))

             -- ValFloat

             assertEqual " True =?"       TypeError "bool not a float" (check(ValFloat t))
             assertEqual " 'b' =?"        TypeError "char not a float" (check(ValFloat b))
             assertEqual " 1 =? "         TypeError "int not a float" (check(ValFloat one))
             assertEqual " [0,1,2] =? "   TypeError "list not a float" (check(ValFloat list1))

             -- UMinus

             assertEqual "!0 =?"           TypeError "can't uminus int" (check(UMinus zero ))
             assertEqual "! 'c' =? "       TypeError "can't uminus char" (check(UMinus c))
             assertEqual "!2.5 =? "        TypeError "can't uminus float" (check(UMinus ftwopointfive))
             assertEqual "![3,4,5] =? "    TypeError "can't uminus list" (check(UMinus list2))

             -- Index

             assertEqual "0 !! False =? "       TypeError "can't index int with bool" (check(Index zero f))
             assertEqual "1 !! 'c' =? "         TypeError "can't index int with char" (check(Index one c))
             assertEqual "3 !! [0,1,2] =? "     TypeError "can't index int with a list" (check(Index three list1))
             assertEqual "0.0 !! False =? "     TypeError "can't index float with bool" (check(Index fzero f))
             assertEqual "1.0 !! 'c' =? "       TypeError "can't index float with char" (check(Index fone c))
             assertEqual "3.0 !! [0,1,2] =? "   TypeError "can't index float with a list" (check(Index fthree list1))
             assertEqual "True !! 3 =? "        TypeError "can't index bool with int" (check(Index t three))
             assertEqual "True !! 2.5 =? "      TypeError "can't index bool with float" (check(Index t ftwopointfive))
             assertEqual "False !! 'c' =? "     TypeError "can't index bool with char" (check(Index f c))
             assertEqual "False !! [0,1,2] =? " TypeError "can't index bool with a list" (check(Index f list1))
             assertEqual "'b' !! 1 =? "         TypeError "can't index char with int" (check(Index b one))
             assertEqual "'b' !! 2.5 =? "       TypeError "can't index char with float" (check(Index b ftwopointfive))
             assertEqual "'b' !! True =? "      TypeError "can't index char with bool" (check(Index b t))
             assertEqual "'b' !! [0,1,2] =? "   TypeError "can't index char with list" (check(Index b list1))
             assertEqual "[3,4,5] !! 2.5 =? "   TypeError "can't index list with float" (check(Index list2 ftwopointfive))
             assertEqual "[3,4,5] !! True =? "  TypeError "can't index list with bool" (check(Index list2b t))
             assertEqual "[3,4,5] !! 'c' =? "   TypeError "can't index list with char" (check(Index list2 c))

             -- Cons
             
             assertEqual "0 : False =? "       TypeError "can't cons int with bool" (check(Cons zero f))
             assertEqual "1 : 'c' =? "         TypeError "can't cons int with char" (check(Cons one c))
             assertEqual "3 : [0,1,2] =? "     TypeError "can't cons int with a list" (check(Cons three list1))
             assertEqual "0.0 : False =? "     TypeError "can't cons float with bool" (check(Cons fzero f))
             assertEqual "1.0 : 'c' =? "       TypeError "can't cons float with char" (check(Cons fone c))
             assertEqual "3.0 : [0,1,2] =? "   TypeError "can't cons float with a list" (check(Cons fthree list1))
             assertEqual "True : 3 =? "        TypeError "can't cons bool with int" (check(Cons t three))
             assertEqual "True : 2.5 =? "      TypeError "can't cons bool with float" (check(Cons t ftwopointfive))
             assertEqual "False : 'c' =? "     TypeError "can't cons bool with char" (check(Cons f c))
             assertEqual "False : [0,1,2] =? " TypeError "can't cons bool with a list" (check(Cons f list1))
             assertEqual "'b' : 1 =? "         TypeError "can't cons char with int" (check(Cons b one))
             assertEqual "'b' : 2.5 =? "       TypeError "can't cons char with float" (check(Cons b ftwopointfive))
             assertEqual "'b' : True =? "      TypeError "can't cons char with bool" (check(Cons b t))
             assertEqual "'b' : [0,1,2] =? "   TypeError "can't cons char with list" (check(Cons b list1))
             assertEqual "[3,4,5] : 2.5 =? "   TypeError "can't cons list with float" (check(Cons list2 ftwopointfive))
             assertEqual "[3,4,5] : True =? "  TypeError "can't cons list with bool" (check(Cons list2b t))
             assertEqual "[3,4,5] : 'c' =? "   TypeError "can't cons list with char" (check(Cons list2 c))

             

             -- Concat

             assertEqual "0 ++ False =? "       TypeError "can't concat int with bool" (check(Concat zero f))
             assertEqual "1 ++ 'c' =? "         TypeError "can't concat int with char" (check(Concat one c))
             assertEqual "3 ++ [0,1,2] =? "     TypeError "can't concat int with a list" (check(Concat three list1))
             assertEqual "0.0 ++ False =? "     TypeError "can't concat float with bool" (check(Concat fzero f))
             assertEqual "1.0 ++ 'c' =? "       TypeError "can't concat float with char" (check(Concat fone c))
             assertEqual "3.0 ++ [0,1,2] =? "   TypeError "can't concat float with a list" (check(Concat fthree list1))
             assertEqual "True ++ 3 =? "        TypeError "can't concat bool with int" (check(Concat t three))
             assertEqual "True ++ 2.5 =? "      TypeError "can't concat bool with float" (check(Concat t ftwopointfive))
             assertEqual "False ++ 'c' =? "     TypeError "can't concat bool with char" (check(Concat f c))
             assertEqual "False ++ [0,1,2] =? " TypeError "can't concat bool with a list" (check(Concat f list1))
             assertEqual "'b' ++ 1 =? "         TypeError "can't concat char with int" (check(Concat b one))
             assertEqual "'b' ++ 2.5 =? "       TypeError "can't concat char with float" (check(Concat b ftwopointfive))
             assertEqual "'b' ++ True =? "      TypeError "can't concat char with bool" (check(Concat b t))
             assertEqual "'b' ++ [0,1,2] =? "   TypeError "can't concat char with list" (check(Concat b list1))
             assertEqual "[3,4,5] ++ 2.5 =? "   TypeError "can't concat list with float" (check(Concat list2 ftwopointfive))
             assertEqual "[3,4,5] ++ True =? "  TypeError "can't concat list with bool" (check(Concat list2b t))
             assertEqual "[3,4,5] ++ 'c' =? "   TypeError "can't concat list with char" (check(Concat list2 c))

             -- Var

             assertEqual " 4 "      TypeError "not a string" (check(Var four))
             assertEqual " False "  TypeError "not a string" (check(Var f))
             assertEqual " True "   TypeError "not a string" (check(Var t))
             assertEqual "2.5"      TypeError "not a string" (check(Var ftwopointfive))
             assertEqual "3.5"      TypeError "not a string" (check(Var fthreepointfive))
             assertEqual "5"        TypeError "not a string" (check(Var five))
             assertEqual "1.5"      TypeError "not a string" (check(Var fonepointfive))
             assertEqual "2"        TypeError "not a string" (check(Var two))
             assertEqual " [0,1,2] =? " TypeError "not a string" (check(Var list1))


             -- Lam

             assertEqual " \x -> y + 10 " UndefinedVarUse "y is undefined" (check(Lam x y))
             assertEqual "let x = 0 in (let y = x in x)"  UndefinedVarUse "reference in let binding" (check(Lam x y))
             assertEqual "\x -> x1" UndefinedVarUse "longer variable name" (check(Lam x x1))








































  ]
