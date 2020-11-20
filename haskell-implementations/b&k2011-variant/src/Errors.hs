module Errors where

data Error = LexFailure String 
            | ParseFailure String 
            | MacroFailure String
            | VariableRenamingFailure String
            | TypeInferenceFailure String
            | TestParseFailure String
            | TimeConstraintFailure String
            
            
    
instance Show Error where
    show (LexFailure err) = err
    show (ParseFailure err) = err
    show (MacroFailure err) = err
    show (VariableRenamingFailure err) = err
    show (TypeInferenceFailure err) = err
    show (TestParseFailure err) = err
    show (TimeConstraintFailure err) = err