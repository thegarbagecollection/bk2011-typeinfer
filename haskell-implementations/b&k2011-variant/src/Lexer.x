{
module Lexer where
import Tokens
-- imports from generated code copy (see below)
import Data.Word (Word8)
import Data.Char (ord)
import qualified Data.Bits
import Errors

}

--%wrapper "basic"

$alpha  = [a-zA-Z] 
$spc    = [\ ]
$lambda = [\\]

tokens :-
    "if"                { \s -> TokIf     }
    "then"              { \s -> TokThen   }
    "else"              { \s -> TokElse   }
    "true"              { \s -> TokTrue   }
    "false"             { \s -> TokFalse  }
    "await"             { \s -> TokAwait  }
    "0"                 { \s -> TokZero   }
    "\"                 { \s -> TokLambda }
    "."                 { \s -> TokDot    }
    "("                 { \s -> TokOP     }
    ")"                 { \s -> TokCP     }
    ":"                 { \s -> TokType   }
    "->"                { \s -> TokArrow  }
    "{"                 { \s -> TokOTup   }
    "}"                 { \s -> TokCTup   }
    ","                 { \s -> TokComma  }
    "let"               { \s -> TokLet    }
    "in"                { \s -> TokIn     }
    "="                 { \s -> TokEq     }
    ";"                 { \s -> TokSemicolon }
    "*"                 { \s -> TokBullet }
    $alpha+             { \s -> TokVar s  }
    $spc+               { \s -> TokSpace  }
    [1-9][0-9]*         { \s -> TokNZDigit (read s :: Int) } 
{

------------------------------------------------------------------------------------------
-- This code is all from the "basic" wrapper and its generated code;
-- modified appropriately to return an Either (containing error details)
-- rather than just throwing an exception. The license allows this.

-- required by the rest of the generated code
utf8Encode :: Char -> [Word8]
utf8Encode = map fromIntegral . go . ord
 where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `Data.Bits.shiftR` 6)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + (oc `Data.Bits.shiftR` 12)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + (oc `Data.Bits.shiftR` 18)
                        , 0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]

type Byte = Word8


type AlexInput = (Char,[Byte],String)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (c,_,_) = c

-- alexScanTokens :: String -> Either Error [token]
-- Note: this function modified to Either!
alexScanTokens str = go ('\n',[],str)
  where go inp__@(_,_bs,s) =
          case alexScan inp__ 0 of
                -- edited here: to Either
                AlexEOF -> Right []
                
                -- edited here: to Either, with more complete error message
                AlexError (prevChar, _, restString) -> Left $ LexFailure $ "LEXING FAILURE: previous character: " ++ show prevChar ++ "  , remainder of string: " ++ restString
                
                -- left alone
                AlexSkip  inp__' _ln     -> go inp__'
                
                -- AlexToken inp__' len act -> act (take len s) : go inp__'
                -- edited here: to Either
                AlexToken inp__' len act -> 
                    case go inp__' of
                        Right ls -> Right $ act (take len s) : ls
                        Left err -> Left err

alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (c,(b:bs),s) = Just (b,(c,bs,s))
alexGetByte (_,[],[])    = Nothing
alexGetByte (_,[],(c:s)) = case utf8Encode c of
                             (b:bs) -> Just (b, (c, bs, s))
                             [] -> Nothing
                             
------------------------------------------------------------------------------------------
-- Basic check
lexT = do 
    s <- getLine
    print (alexScanTokens s)
}