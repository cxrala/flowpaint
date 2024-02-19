module Interaction.Render 
(renderOutput)
where

-- in https://wiki.haskell.org/Yampa/reactimate,
-- corresponds to output/actuate

renderOutput :: Bool -> (Int, Int) -> IO ()
renderOutput _ = print