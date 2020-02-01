import           "directory"           System.Directory
import           "base"                Data.List
import           "base"                Data.Maybe
import           "base"                Data.Either
import           "base"                Control.Monad
import           "optparse-applicative"      Options.Applicative
import           "pretty-simple"       Text.Pretty.Simple
import           "uniplate"         Data.Generics.Uniplate.Data
--import           "base"               Data.Foldable
import           "base"               Control.Arrow
import           "text"                     Data.Text (Text)
import           "filepath"            System.FilePath
import                                Types
import                                ImportsParser
--import           "base"                Data.Monoid
--impodrt           "base"               System.IO
--import           "extra"             Control.Monad.Extra
import Control.Exception
import Debug.Trace
import System.IO.Error


--import Debug.Trace
--lttrace a b = trace (a ++ ":" ++ show b) b

-- todo: deal with comments
-- todo: deal with multilines
-- todo: deal with qualified imports
-- todo: add option to remove redundant files from package.yml
-- todo: imports formatter
-- todo: importify file (add package imports and labels)
-- todo: unique temp names
