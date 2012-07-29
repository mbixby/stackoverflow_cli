{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative  
import Control.Monad
import Control.Exception as E
import System.Environment
import System.Console.GetOpt
import System.Exit
import System.IO
import System.Time
import System.Locale
       
import Data.Maybe
import Data.List
import Data.List.Utils
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as L
import Network.HTTP.Conduit
import Network.HTTP.Base
import Web.Encodings (decodeHtml)
            
            

-- Data types

data DisplayType    = DTSearchResults | DTQuestion | DTAnswer deriving Eq
data Sort           = Date | Score | Relevance deriving Eq
data Order          = Ascending | Descending deriving Eq
type SearchResults  = Maybe [Question]
type CommandData    = (SearchRequest, SearchResults, DisplayType, Maybe Question, Maybe Answer)

data SearchRequest  = SearchRequest { srQuery :: String
                                    , srTags :: String
                                    , srSort :: Sort
                                    , srOrder :: Order
                                    , srPage :: Int
                                    }
                                    
data Question      = Question       { questionTitle :: String
                                    , questionId :: Int
                                    , questionScore :: Int
                                    , questionContent :: Maybe String
                                    , questionTags :: [String]
                                    , questionOwner :: Maybe ShallowUser
                                    , answerCount :: Int
                                    , answers :: Maybe [Answer]
                                    , creationDate :: Integer
                                    } deriving (Show, Eq)
                                    
data Answer        = Answer         { answerScore   :: Int
                                    , answerContent :: Maybe String
                                    , answerOwner  :: ShallowUser
                                    } deriving (Show, Eq)

data ShallowUser   = ShallowUser    { name   :: String
                                    , reputation :: Int
                                    } deriving (Show, Eq)



-- Mappings for StackOverflow API
         
instance FromJSON Question where
    parseJSON (Object o) = Question <$>
                           o .: "title" <*>
                           o .: "question_id" <*>
                           o .: "score" <*>
                           o .:? "body" <*>
                           o .: "tags" <*>
                           o .:? "owner" <*>
                           o .: "answer_count" <*>
                           o .:? "answers" <*>
                           o .: "creation_date"
    parseJSON _          = fail "Expected Object"
    
instance FromJSON Answer where
    parseJSON (Object o) = Answer <$>
                           o .: "score" <*>
                           o .:? "body" <*>
                           o .: "owner"
    parseJSON _          = fail "Expected Object"

instance FromJSON ShallowUser where
    parseJSON (Object o) = ShallowUser <$>
                           o .: "display_name" <*>
                           o .: "reputation"
    parseJSON _          = fail "Expected Object"



-- | Main
main :: IO ()
main = do
  -- Get CLI arguments
  (args, expression) <- getArgs >>= parseArgs
  let tags = fromMaybe "" (determineTags args)
  let sorting = determineSort args
  let order = if Asc `elem` args then Ascending else Descending
  let searchRequest = SearchRequest expression tags sorting order 0
  
  -- Get search results
  searchResults <- search searchRequest
  printSearchResults (srPage searchRequest) searchResults
  
  -- Process additional user commands
  hSetBuffering stdout NoBuffering
  processInput (searchRequest, searchResults, DTSearchResults, Nothing, Nothing) False


-- | Processes user commands
processCommand :: String -- Input command
  -> CommandData         -- (SearchRequest, SearchResults, Current display type, Last displayed question, Last displayed answer)
  -> IO CommandData      -- Move along the same data with some changes
  
processCommand inp cdata@(sr, res, _, ldq, lda) = case inp of
  -- Quit
  "q" -> do putStrLn "Quitting..."; return cdata
  
  -- Get next (anything)
  "j" -> processCommandNextOrPrevious True cdata
                                            
  -- Get previous (anything)
  "k" -> processCommandNextOrPrevious False cdata
  
  -- Display search results
  "r" -> do printSearchResults (srPage sr) res; return (sr, res, DTSearchResults, ldq, lda)
  
  -- Display help
  "h" -> do putStrLn commandsInfo; return cdata
  
  -- Display answer
  "a" -> case ldq of
            Just qs -> if answerCount qs > 0
                          then do let answer = fromMaybe (head (fromJust (answers qs))) lda
                                  printAnswer answer
                                  return (sr, res, DTAnswer, ldq, Just answer)
                          else do putStrLn "There are no answers for this question"; return cdata          
            _       -> do putStrLn "Pick a question first"; return cdata
            
  -- Display question
  x   -> case maybeRead x of
          Just int  -> do let questionNo = int - 10 * srPage sr
                          if (questionNo >= 0) && (questionNo < length (fromJust res))
                            then do q <- question (questionId (fromJust res !! questionNo))
                                    printQuestion (Just q)
                                    return (sr, res, DTQuestion, Just q, Nothing)
                            else do putStrLn "This question is not in current results, navigate to the appropriate page"; return cdata
          _         -> do putStrLn "Unrecognized command"; return cdata


processCommandNextOrPrevious :: Bool -> CommandData -> IO CommandData
processCommandNextOrPrevious isNext cdata@(sr, res, dtype, ldq, lda) = case dtype of
  DTSearchResults -> do let newPage = (if isNext then succ else pred) (srPage sr)
                        if newPage < 0 
                          then  do  putStrLn "This was the first page."; return cdata
                          else  do  let newRequest = SearchRequest (srQuery sr) (srTags sr) (srSort sr) (srOrder sr) newPage
                                    searchResults <- search newRequest
                                    printSearchResults (srPage newRequest) searchResults
                                    return (newRequest, searchResults, dtype, Nothing, Nothing)
                      
  DTQuestion      -> do let shallowQuestions = fromJust res
                        let lastQuestion = fromJust ldq
                        let lastIndex = fromJust (findIndex (\a -> questionId a == questionId lastQuestion) shallowQuestions)
                        let nextIndex = (if isNext then succ else pred) lastIndex
                        if nextIndex >= length shallowQuestions
                          then do putStrLn "No further questions, you'll have to load up the next page of results.\n(Press r Enter followed by j Enter"
                                  return cdata
                          else if nextIndex < 0
                                    then do putStrLn "This was the first question."
                                            return cdata
                                    else processCommand (show nextIndex) cdata
                                
  DTAnswer        -> do let justAnswers = fromJust (answers (fromJust ldq))
                        let nextIndex = (if isNext then succ else pred) (fromJust (elemIndex (fromJust lda) justAnswers))
                        if nextIndex >= length justAnswers
                          then do putStrLn "No further answers."
                                  return cdata
                          else if nextIndex < 0
                                    then do putStrLn "This was the first answer."
                                            return cdata
                                    else processCommand "a" (sr, res, dtype, ldq, Just (justAnswers!!nextIndex))


-- | Processes user input and passes it to processCommand function      
processInput :: CommandData -> Bool -> IO ()
processInput cdata@(_, _, dtype, _, _) shouldQuit =
  unless shouldQuit $ do
          case dtype of DTSearchResults -> putStr "\nEnter command (Number|j|k|r|h|q) and press Enter: "
                        DTQuestion      -> putStr "\nEnter command (a|r|j|k|Number|h|q) and press Enter: "
                        DTAnswer        -> putStr "\nEnter command (j|k|Number|r|h|q) and press Enter: "
          input <- getLine
          newData <- processCommand input cdata
          processInput newData (input == "q")



-- | Searches for a given query, outputs list of questions
search  :: SearchRequest -> IO (Maybe [Question]) 
search req = do
  res <- customSimpleHttp $ urlForSearch req
  let root = (fromJust . decode) res :: Value
  let questionResult = parse parseRoot' root
  let questions = case questionResult of  Success a -> a
                                          _         -> error "Error while parsing data."                                         
  return questions

urlForSearch  :: SearchRequest -> String 
urlForSearch req = url where
  url = baseUrl ++ "&page=" ++ show (succ (srPage req)) ++ "&title=" ++ stitle ++ "&order=" ++ sorder ++ "&sort=" ++ ssort ++ "&tagged=" ++ srTags req
  baseUrl = "http://api.stackexchange.com/2.0/similar?pagesize=10&site=stackoverflow&filter=!3JxzbDknOM-hdCNX.ea4"
  stitle = urlEncode (srQuery req)
  sorder = if srOrder req == Ascending then "asc" else "desc"
  ssort = case srSort req of  Score     -> "votes"
                              Date      -> "creation"
                              Relevance -> "relevance"

parseRoot' :: Value -> Parser (Maybe [Question])
parseRoot' (Object o) = do
  res <- o .: "items"
  case res of
       (c:r)  -> return $ Just (c:r)
       _      -> return Nothing
parseRoot' _ = return Nothing

customSimpleHttp :: String -> IO L.ByteString
customSimpleHttp url = simpleHttp url `E.catch` httpExceptionHandler

httpExceptionHandler ::  HttpException -> IO L.ByteString
httpExceptionHandler e = error "Error while loading your request. Please check your query." >> return L.empty



-- | Returns question with a given ID, containing all answers
question :: Int -> IO Question     
question ids = do
  res <- customSimpleHttp $ urlForQuestion (show ids)
  let root = (fromJust . decode) res :: Value
  let questionResult = parse parseRoot root
  let questionRes = case questionResult of  
                      Success a -> a
                      _         -> error "Error while parsing data."
  return (fromJust questionRes)

urlForQuestion  :: String -> String
urlForQuestion ids = baseUrl ++ ids ++ restOfUrl where
  baseUrl = "http://api.stackexchange.com/2.0/questions/"
  restOfUrl = "?order=desc&sort=activity&site=stackoverflow&filter=!6NLCYgCh(o-Rj"

parseRoot :: Value -> Parser (Maybe Question)
parseRoot (Object o) = do
  res <- o .: "items"
  case res of
       (c:_) -> return $ Just c
       _     -> return Nothing
parseRoot _ = return Nothing


-- | Prints out question
printQuestion :: Maybe Question -> IO ()
printQuestion qs = do
  let q = fromJust qs
  putStrLn "┏╸"
  
  let spacesBeforeNextTitleLines = replicate ((length.show.questionScore) q) ' '
  let ftitle = decodeHtml (questionTitle q)
  let wrappedTitle = wrapText ftitle 60 "" ("┃     "++spacesBeforeNextTitleLines)
  (answersCount, formattedDate) <- getLabelsForQuestion q
  
  putStrLn $ "┃ " ++ show (questionScore q) ++ " ┃┃" ++ wrappedTitle ++ " ┃ " ++ answersCount ++ " ┃ " ++ formattedDate
  printDiv

  case questionContent q of 
    Just a  -> putStrLn (wrapText (decodeHtml a) 90 "┃ " "┃ ")
    _       -> putStrLn "┃ <No content>"
  printDiv
  
  let encapsuledTags = map (\a -> "<"++a++">") (questionTags q)
  let foldedTags = foldr1 (\a b -> a++", "++b) encapsuledTags
  putStrLn $ "┃ Tags: " ++ foldedTags
  printDiv
  
  putStrLn $ maybe "<No Author>" showUser (questionOwner q)
  putStrLn "┗╸"


-- | Prints out question one-liner
printShallowQuestion :: Int -> Question -> IO ()
printShallowQuestion orderNo q = do
  let spacesBeforeNextTitleLines = replicate (3 + (length.show) orderNo) ' ' ++ "┃  "
  let ftitle = decodeHtml (questionTitle q)
  let wrappedTitle = wrapText ftitle 70 "" ('┃':spacesBeforeNextTitleLines)
  (answersCount, formattedDate) <- getLabelsForQuestion q
  putStrLn $ "┃ " ++ show orderNo ++ ". ┃ " ++ wrappedTitle ++ " ┃ " ++ formattedDate ++ " ┃ Score: " ++ show (questionScore q) ++ " ┃ " ++ answersCount


-- | Convenience method. Returns common labels for question
getLabelsForQuestion :: Question -> IO (String, String)
getLabelsForQuestion q = do
  let answersLabel = if answerCount q == 1 then " answer" else " answers"
  let answersCount = show (answerCount q) ++ answersLabel
  date <- toCalendarTime (TOD (creationDate q) 0)
  let formattedDate = formatCalendarTime defaultTimeLocale "%b %e %Y" date
  return (answersCount, formattedDate)


-- | Prints out search results (questions)
printSearchResults :: Int -> Maybe [Question] -> IO ()
printSearchResults page (Just qs)  = do putStrLn "┏╸"; printSearchResults' qs (page*10); putStrLn "┗╸"
printSearchResults _    Nothing    = do hPutStrLn stderr "No results found for your query."
                                        exitWith (ExitFailure 1)

printSearchResults' :: [Question] -> Int -> IO()
printSearchResults' [] _ = return ()
printSearchResults' (q:rest) int = do
  printShallowQuestion int q
  printSearchResults' rest (succ int)


-- | Prints out single answer
printAnswer :: Answer -> IO ()
printAnswer q = do  
  putStrLn "┏╸"

  let content = maybe "<No Content>" decodeHtml (answerContent q)
  let wrappedTitle = wrapText content 90 "" "┃ "
  putStrLn $ "┃ " ++ show (answerScore q) ++ " ┃┃" ++ wrappedTitle
  printDiv

  putStrLn $ showUser (answerOwner q)
  putStrLn "┗╸"

printDiv :: IO()
printDiv = putStrLn "┣╸"


-- | String representation of a user
showUser :: ShallowUser -> String
showUser u = "┃ By " ++ name u ++ " (★" ++ show (reputation u) ++ ")"



-- | Wraps text with limit on number of characters in a line 
wrapText :: String  -- input text
  -> Int            -- max number of character in a line
  -> String         -- starting string on first line
  -> String         -- starting string on every other line
  -> String         -- lines of text 
  
wrapText input maxC lineStart1 lineStart = formatted where 
  formatted = foldl1 (\a b -> a++"\n"++b) wrapped
  wrapped = reverse (wrapText' (split " " withoutNewlines) [lineStart1] maxC lineStart)
  withoutNewlines = replace "\n" "\n┃ " input

wrapText' :: [String] -> [String] -> Int -> String -> [String] 
wrapText' [] out _ _ = out
wrapText' (word:input) out maxC lineSt = str where
  str = if length appendedLine < maxC 
          then wrapText' input output maxC lineSt
          else wrapText' input outputAndNewline maxC lineSt
  output = appendedLine : tail out
  outputAndNewline = newLine:out
  appendedLine = head out ++ " " ++ word
  newLine = lineSt ++ word

maybeRead :: Read a => String -> Maybe a
maybeRead s = case reads s of
    [(x, "")] -> Just x
    _         -> Nothing


-- | Argument flags
type Flags = [Flag]
data Flag
  = DateFlag
  | ScoreFlag
  | RelevanceFlag
  | Asc
  | Desc
  | Tags String
  | Version 
  | Help
  deriving (Eq, Show)

flags :: [OptDescr Flag]
flags =
  [Option ""   ["date"]           (NoArg DateFlag) 
      "Sort results by date."
  ,Option ""   ["score"]          (NoArg ScoreFlag) 
      "Sort results by score."
  ,Option "r"  ["relevance"]      (NoArg RelevanceFlag) 
      "Sort results by relevance. Default."
  ,Option "a"  ["asc"]            (NoArg Asc) 
      "Ascending order."
  ,Option "d"  ["desc"]           (NoArg Desc) 
      "Descending order. Default"
  ,Option "t"  ["tags"]           (ReqArg Tags "tags")
      "Comma-separated tags to boil down your search results with. \n E.g. \"-t=haskell;parsec\""
  ,Option "vV" ["version"]        (NoArg Version)
      "Print version and exit..."
  ,Option "h?" ["help"]           (NoArg Help)
      "Prints this help message."
  ]



-- | Parses arguments
--
-- Sources:
-- http://www.haskell.org/haskellwiki/Tutorials/Programming_Haskell/Argument_handling#Parsing_the_flags
-- http://cvs.haskell.org/Hugs/pages/libraries/base/System-Console-GetOpt.html
parseArgs :: [String] -> IO (Flags, String)
parseArgs argv = case getOpt Permute flags argv of
  (_,[],_) -> do
    hPutStrLn stderr $ "Please type a search query.\n\n" ++ usageInfo header flags
    exitWith (ExitFailure 1)
  
  (args,ex,[]) -> do
    let query = if null ex then "" else concat ex
    parseArgs' args flags query header
  
  (_,_,errs)   -> do
    hPutStrLn stderr (concat errs ++ usageInfo header flags)
    exitWith (ExitFailure 1)

  where header = usageHeader

parseArgs' :: Flags -> [OptDescr a] -> t -> String -> IO (Flags, t)
parseArgs' args descriptors query header
  | Help `elem` args      = do  putStr (usageInfo header descriptors)
                                exitSuccess
  | Version `elem` args   = do  putStrLn version
                                exitSuccess
  | otherwise             = return (nub (concatMap set args), query)

set :: t -> [t]
set f = [f]

determineTags :: Flags -> Maybe String
determineTags [] = Nothing
determineTags (Tags s:_) = Just s
determineTags (_:rest) = determineTags rest

determineSort :: Flags -> Sort
determineSort args
  | DateFlag `elem` args = Date
  | ScoreFlag `elem` args = Score
  | otherwise = Relevance


-- | Usage info 
usageHeader :: String
usageHeader = header ++ commandsInfo ++ notice where
  header = "Usage: so [-t tag1;tag2] [-adtvVh] [--version] [--help] [\"<search query>\"].\n"
  notice = "Don't forget to use quote marks on queries.\nArguments:"

commandsInfo :: String
commandsInfo =  "Commands: j,k       get next (j) or previous (k) page, question or answer\n\
                \          r         jump back to the list of search results\n\
                \          <Number>  jump to question with given order number\n\
                \          a         jump to answers of the displayed question\n\
                \          h         print this help message\n\
                \          q         quit app\n"


-- | Version info 
version :: String
version = "GNU CLI StackOverflow 1.0\n\
          \Copyright (C) 2007 Free Software Foundation, Inc.\n\
          \Complain about bugs & issues on https://github.com/mbixby/stackoverflow_cli\n\
          \License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>\n\
          \This is free software: you are free to change and redistribute it.\n\
          \There is NO WARRANTY, to the extent permitted by law."

