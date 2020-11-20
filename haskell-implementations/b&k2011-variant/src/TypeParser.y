{-
    Explicitly for parsing type strings, just to make checking / testing a little easier.
    
    Taken directly from Parser.y, reduced until only types remain.
-}

{
module TypeParser where
import Tokens
import Lexer
import Errors

}

%name parseType
%tokentype { Token }
%monad { (Either Error) }
%error { parseError }

%token
    var     { TokVar $$ }
    '('     { TokOP }
    ')'     { TokCP }
    arr     { TokArrow }
    '{'     { TokOTup }
    '}'     { TokCTup }
    ','     { TokComma }
    '*'     { TokBullet }


%right '.' arr 
%left ' '
%right else
%right '*'

        
%%

Type : '(' Type ')'                 { $2 }
     | Type arr Type                { FunctionLookup $1 $3 }        
     | TypeConstr                   { $1 }
     | '{' TypeList '}'             { TupleLookup $2 }
     | '*' Type                     { BulletTypeLookup $2 }
     
TypeConstr : var                    { TypeConstantLookup $1 }
           | var '(' TypeList ')'   { TypeConstructorLookup $1 $3 }
      
TypeList : Type                     { [$1] }
         | Type ',' TypeList        { $1 : $3 }


{ 

parseError :: [Token] -> Either Error a
parseError tok = Left $ ParseFailure $ "Parse error with remaining: " ++ concat (map show tok)


}