{-#LANGUAGE OverloadedStrings#-}
module DomainModeling.TestSpec where

import           Data.Semigroup
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text


pluralize :: Text -> Text 
pluralize "" = ""
pluralize t = 
    case Text.last t of 
        's' -> t
        _ -> t <> "s"

