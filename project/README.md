# Project

Follow the [instructions](INSTRUCTIONS.md), use this space to document your project for yourself and the graders.

## Names
* Kenneth Kim
* Juan Santana
* Julia Bighetto

## Summary
We plan to create a language named **ScarletRhett** based off of the week10 HW. We will add the vanilla features and add the mix-in features throughout the weeks. 

## Plan
Besides the "Vanilla" part, we plan to implement the following for Mix-In:

* Add an infix function composition operator (.). So you may write f . g instead of \x -> f (g x) (5 pts.)
* Warn when a variable is intruduced but never used (5 pts. total)
* Orderings on lists (quicksort) (5 pts.) 
* Writing a quickcheck generator and shrinker for your Ast and using it to test your parser (5 pts.)
* Overloaded operators and constants, automatic type conversion (as in Java or Python) (10-15 pts.)

As of this point, we are focusing on the broad picture when it comes to our individual responsibilites. These are the main areas we each want to work on first:

Kenneth: 
* Vanilla: EnvUnsafeLog.hs file, Parser.hs file, ParserMonad.hs file, ParserTests.hs file, some of CheckTest.hs
* Mix-In: Add an infix function composition operator (.). So you may write f . g instead of \x -> f (g x) (5 pts.)
* Mix-In: Writing a quickcheck generator and shrinker for your Ast and using it to test your parser (5 pts.)

Juan: 
* Vanilla: EvalTest.hs file, CheckTest.hs file, some of ParserTests.hs file, AST.hs file, some of Eval.hs
* Mix-In: Warn when a variable is intruduced but never used (5 pts.)
* Min-In: Orderings on lists (quicksort) (5 pts.) 
* Mix-In: Overloaded operators and constants, automatic type conversion (as in Java or Python) (10-15 pts.)

Julia:
* Vanilla: Eval.hs file, Check.hs file, Exec.hs file, some of EnvUnsafeLog.hs file
* Mix-In: Overloaded operators and constants, automatic type conversion (as in Java or Python) (10-15 pts.)
