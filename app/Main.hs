module Main where

import System.Directory (doesFileExist)
import System.Process (callCommand)
import Control.Monad (forM_, filterM)
import Data.List (stripPrefix, isInfixOf)
import System.Environment (getArgs)

type Target = String
type Dependency = String
type Command = String

version :: String
version = "0.1.0"

parseLines :: [String] -> [(Target, Dependency, Command)]
parseLines = aux [] Nothing
  where
    aux :: [(Target, Dependency, Command)] -> Maybe (Target, Dependency) -> [String] -> [(Target, Dependency, Command)]
    aux acc _ [] = reverse acc
    aux acc currentTarget (line:rest)
      | ":" `isInfixOf` line =
          let (target, dep) = parseTargetAndDependency line
          in aux acc (Just (target, dep)) rest
      | otherwise =
          case currentTarget of
            Just (target, dep) -> aux ((target, dep, line) : acc) Nothing rest
            Nothing -> aux acc currentTarget rest

    parseTargetAndDependency :: String -> (Target, Dependency)
    parseTargetAndDependency line =
      let (target, rest) = break (== ':') line
          dependency = dropWhile (== ' ') $ drop 1 rest
      in (target, dependency)

replacePlaceholders :: Command -> Target -> Dependency -> Command
replacePlaceholders command target dependency =
  let replace :: String -> String -> String -> String
      replace placeholder value str = 
        case stripPrefix placeholder str of
          Just rest -> value <> replace placeholder value rest
          Nothing -> case str of
                       [] -> []
                       (x:xs) -> x : replace placeholder value xs
  in replace "$!" target (replace "$@" dependency command)

runBuildRule :: (Target, Dependency, Command) -> IO ()
runBuildRule (target, dependency, command) = do
  dependencyExists <- doesFileExist dependency
  if not dependencyExists
    then putStrLn $ "Dependency " <> dependency <> " does not exist."
    else do
      let finalCommand = replacePlaceholders command target dependency
      putStrLn $ "Running: " <> finalCommand
      callCommand finalCommand
      putStrLn $ "Built " <> target

findBuildFile :: IO (Maybe FilePath)
findBuildFile = do
  let possibleFiles = ["Cinderfile", "cinderfile", "cinderFile", "CinderFile"]
  existingFiles <- filterM doesFileExist possibleFiles
  return $ case existingFiles of
    (x:_) -> Just x
    [] -> Nothing

main :: IO ()
main = do
  args <- getArgs
  if "--version" `elem` args
    then putStrLn $ "Cinder Build Version: " <> version
    else do
      buildFile <- findBuildFile
      case buildFile of
        Just file -> do
          contents <- readFile file
          let inputLines = filter (not . null) $ lines contents
          let rules = parseLines inputLines
          forM_ rules runBuildRule
        Nothing -> putStrLn "No valid build file (Cinderfile, cinderfile, cinderFile, CinderFile) found."
