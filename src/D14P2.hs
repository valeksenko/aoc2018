module D14P2 (
    recipecount
) where

import Data.List
import Data.Sequence ((|>))
import qualified Data.Sequence as S
import Data.Foldable (toList)

scoreSize = 10 

recipecount :: [Int] -> [Int] -> Int
recipecount goal initRecipes = excludeGoal . toList . fst $ until foundGoal tryRecipe (S.fromList initRecipes, (0,1))
    where
        excludeGoal recipes = if (drop (length recipes - length goal) recipes) == goal then length recipes - length goal else length recipes - length goal - 1
        foundGoal (recipes, (e1, e2)) = checkGoal . toList $ S.drop (S.length recipes - length goal - 1) recipes
        checkGoal recipes = (goal == take (length goal) recipes) || (goal == drop 1 recipes)
        tryRecipe (recipes, (e1, e2)) = useRecipe e1 e2 $ addRecipes recipes (S.index recipes e1) (S.index recipes e2)
        addRecipes recipes s1 s2 = if (s1+s2) >= 10 then recipes |> 1 |> (s1+s2) `mod` 10 else recipes |> (s1+s2)
        useRecipe e1 e2 recipes = (recipes, (nextIndex e1 recipes, nextIndex e2 recipes))
        nextIndex i recipes = ((S.index recipes i) + i + 1) `mod` S.length recipes

{-
https://adventofcode.com/2018/day/14#part2

TAs it turns out, you got the Elves' plan backwards. They actually want to know how many recipes appear on the scoreboard to the left of the first recipes whose scores are the digits from your puzzle input.

51589 first appears after 9 recipes.
01245 first appears after 5 recipes.
92510 first appears after 18 recipes.
59414 first appears after 2018 recipes.
How many recipes appear on the scoreboard to the left of the score sequence in your puzzle input?
-}