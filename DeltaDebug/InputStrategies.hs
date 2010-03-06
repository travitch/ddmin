module DeltaDebug.InputStrategies ( byLine
                                    --, byCToken
                                  -- , byWord
                                  , byChar
                                  ) where

import Data.List.Split (split, keepDelimsR, oneOf)

byLine = split (keepDelimsR $ oneOf "\n")
byChar s = s