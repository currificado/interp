import Data.List (intercalate)
import Data.Maybe (fromJust)
import Control.Monad (when)
import System.Console.GetOpt
import System.Environment (getArgs)

import Interpreter
import Parser

-- | Flag type
data Flag = Verbose | Source String
    deriving Show

options :: [OptDescr Flag]
options =
    [ Option ['v']     ["verbose"] (NoArg Verbose)          "Prints to stdout the expression of type `Expr' to be evaluated."
    , Option ['c']     []          (ReqArg Source "SOURCE") "File containing the program."
    ]

-- | Options record type
data Opts = Opts { verbose :: Bool
                 , source  :: Maybe String
                 }
            deriving Show

setFlag :: Flag -> Opts -> Opts
setFlag Verbose opts       = opts { verbose = True      }
setFlag (Source file) opts = opts { source  = Just file }

buildOpts :: [Flag] -> IO Opts
buildOpts = return . (foldl (flip setFlag) (Opts False Nothing))

header,info :: String
header = "Usage: ./interp [-v] -c SOURCE"
info = usageInfo header options

dump msg = ioError (userError (msg ++ info))

-- | Command-line parsing
compilerOpts :: [String] -> IO Opts
compilerOpts argv = case getOpt Permute options argv of
        (opts, garbage, []) ->  do record <- (buildOpts opts)
                                   case (source record) of 
                                       Nothing -> dump "-c option, which specifies the source file, is mandatory\n"
                                       (Just _)-> if (null garbage) then 
                                                      return record
                                                  else 
                                                      dump ("invalid arguments supplied: " ++ (intercalate ", " (map (('`':) . (++"'")) garbage)) ++ "\n")
        (_, _, errors)      ->  dump (concat errors)
        

main :: IO ()
main = do 
    argv <- getArgs
    opts <- compilerOpts argv
    let filename = fromJust (source opts)
    str <- readFile filename
    let rs = filter (null . snd) (runP parser str)
    case rs of
         [] -> error ("Could not parse `" ++ filename ++ "'")
         (r:_) -> do let expr = fst r
                     when (verbose opts) $ putStrLn (show  expr)
                     putStrLn $ show (eval expr)
