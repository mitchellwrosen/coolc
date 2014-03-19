{
module Lexer where

import           Data.Char (isUpper, toLower)
import           Data.Map  (Map)
import qualified Data.Map  as M
}

%wrapper "posn"

$any         = [. \n]
$lowercase   = a-z
$uppercase   = A-Z
$letter      = [$lowercase $uppercase]
$digit       = 0-9

$ident_char  = [$letter $digit _]

@ident       = $letter $ident_char*

@escape_seq  = \\ $any -- TODO: exclude \0
@string_char = $any # [\\ \n]
@string_item = @string_char | @escape_seq
@string      = \" @string_item* \"

cool :-

   -- Each has type :: AlexPosn -> String -> Lexeme

   $white+     ;                        -- ignore whitespace
   "--".*      ;                        -- ignore line comments

   $digit+     { mkL LInteger        }
   @ident      { keywordOrIdentifier }
   @string     { mkL LString         }

{

data LexemeClass = LInteger | LTypeIdent | LObjIdent | LString
                 deriving Show

data Lexeme = Lexeme
   { lexemeClass :: LexemeClass
   , lexemePosn  :: AlexPosn
   , lexemeStr   :: String
   } deriving Show

lexemeShow :: Lexeme -> String
lexemeShow (Lexeme LInteger _ str)   = "<int '" ++ str ++ "'>"
lexemeShow (Lexeme LTypeIdent _ str) = "<type '" ++ str ++ "'>"
lexemeShow (Lexeme LObjIdent _ str)  = "<obj '" ++ str ++ "'>"
lexemeShow (Lexeme LString _ str)    = "<str '" ++ str ++ "'>"

mkL :: LexemeClass -> AlexPosn -> String -> Lexeme
mkL cls posn str = Lexeme cls posn str

keywordOrIdentifier :: AlexPosn -> String -> Lexeme
keywordOrIdentifier posn str = mkL cls posn str
  where
    cls :: LexemeClass
    cls = maybe ident keyword $ M.lookup (map toLower str) keywords

    ident :: LexemeClass
    ident | beginsWithUpper = LTypeIdent
          | otherwise       = LObjIdent

    -- Stupid secondary check required, because "true" and "false" MUST begin
    -- with a lowercase letter, unlike all other case-insensitive keywords.
    -- Thus, check to see if LTrue or LFalse was returned, and replace with
    -- LTypeIdent if str begins with an uppercase letter. This is /almost/ an
    -- identity function. SO DUMB!
    keyword :: LexemeClass -> LexemeClass
    keyword LTrue  | beginsWithUpper = LTypeIdent
    keyword LFalse | beginsWithUpper = LTypeIdent
    keyword cls = cls

    beginsWithUpper :: Bool
    beginsWithUpper = isUpper (head str)

keywords :: Map String LexemeClass
keywords = M.fromList
    [ ("case"  , LCase  ), ("class", LClass), ("else", LElse), ("esac"    , LEsac    )
    , ("false" , LFalse ), ("fi"   , LFi   ), ("in"  , LIn  ), ("inherits", LInherits)
    , ("isvoid", LIsVoid), ("let"  , LLet  ), ("loop", LLoop), ("new"     , LNew     )
    , ("not"   , LNot   ), ("of"   , LOf   ), ("pool", LPool), ("then"    , LThen    )
    , ("true"  , LTrue  ), ("while", LWhile)
    ]

}
