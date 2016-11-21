# Algorithms

Algorithms for haskell training


Syntax not understandable yet:

reference: http://www.parsonsmatt.org/2015/12/09/exploratory_haskell.html

Pipe un function declaration as in
```Haskell
dpll :: Clauses -> Symbols -> Model -> Maybe Model
dpll clauses symbols model
  | all (`isTrueIn` model) clauses  = Just model
  | any (`isFalseIn` model) clauses = Nothing
  | otherwise =
```

solution:

https://en.wikibooks.org/wiki/Haskell/Control_structures


Between two expressions
```Haskell
<$>
```

Double arrow function?
```Haskell
map (>>= next) controlList
```
