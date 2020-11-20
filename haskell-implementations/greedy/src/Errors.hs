module Errors where

data Error = LexFailure String 
            | ParseFailure String 
            | MacroFailure String
            | VariableRenamingFailure String
            | TypeCheckFailure String
            | TestParseFailure String
            
            
instance Show Error where
    show (LexFailure err) = err
    show (ParseFailure err) = err
    show (MacroFailure err) = err
    show (VariableRenamingFailure err) = err
    show (TypeCheckFailure err) = err
    show (TestParseFailure err) = err