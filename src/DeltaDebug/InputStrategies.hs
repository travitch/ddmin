module DeltaDebug.InputStrategies ( byLine
                                    --, byCToken
                                  -- , byWord
                                  , byChar
                                  ) where

import Data.List.Split (split, keepDelimsR, oneOf)

-- import Control.Applicative ((<*>), (<$), (<*), (<$>), (*>))
-- import Text.Parsec
-- import Text.Parsec.String (Parser)
-- import Text.Parsec.Expr
-- import Text.Parsec.Language (javaStyle)
-- import qualified Text.Parsec.Token as P

byLine = split (keepDelimsR $ oneOf "\n")
byChar s = s


-- Be aware of string literals
-- only break sections at a curly brace "open or close" and semicolons followed by newlines



-- Lex out:
-- * String literals
-- * Preprocessor statements (respect line continuation)
-- * Non-identifiers
-- * Whitespace-separated things

-- Scan and break at semicolons
-- If hitting a # at the beginning of a line, take the rest of the line (discarding line continuation markers)
-- If hitting a struct/union/ {, continue until the closing };





-- lexer = P.makeTokenParser javaStyle
-- identifier = P.identifier lexer
-- reserved = P.reserved lexer
-- operator = P.operator lexer
-- reservedOp = P.reservedOp lexer
-- charLiteral = P.charLiteral lexer
-- stringLiteral = P.stringLiteral lexer
-- natural = P

-- pragma
-- typedef
-- struct
-- union
-- varDecl
-- typeSpec