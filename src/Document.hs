{-# LANGUAGE InstanceSigs #-}

module Document
  ( jsonToDocument,
    xmlToDocument,
    markdownToDocument,
  )
where

import qualified JSON as J
import qualified Markdown as M
import qualified XML as X

data Header = Header
  { title :: String,
    author :: Maybe String,
    date :: Maybe String
  }
  deriving (Show)

data Content
  = Text String
  | Italic String
  | Bold String
  | Code String
  | Link String Content
  | Image String String
  | Paragraph [Content]
  | Section (Maybe String) [Content]
  | CodeBlock [Content]
  | List [Content]
  | Item [Content]
  deriving (Show)

newtype Document = Document (Header, Content)

instance Show Document where
  show :: Document -> String
  show (Document (header, content)) =
    "Header: " ++ show header ++ "\nContent: " ++ show content

-- JSON parsing

-- TODO: If a key is not found and
-- it is optional, return Nothing

jsonKeys :: J.JValue -> [String]
jsonKeys (J.JObject object) = map fst object
jsonKeys _ = []

jsonFindKey :: String -> J.JValue -> Either String J.JValue
jsonFindKey key (J.JObject object) =
  case lookup key object of
    Just value -> return value
    Nothing -> Left $ "Key " ++ key ++ " is not in " ++ show (map fst object)
jsonFindKey _ _ = Left "Object expected"

jsonRoot :: J.JValue -> Either String (Header, Content)
jsonRoot object = do
  header <- jsonFindKey "header" object >>= jsonToHeader
  content <- jsonFindKey "body" object >>= jsonToContent

  return (header, content)

jsonToHeader :: J.JValue -> Either String Header
jsonToHeader object = do
  title' <- jsonFindKey "title" object >>= jsonToString
  author' <- jsonFindKey "author" object >>= jsonToMaybeString
  date' <- jsonFindKey "date" object >>= jsonToMaybeString

  return $ Header title' author' date'

jsonToString :: J.JValue -> Either String String
jsonToString (J.JString string) = return string
jsonToString object = Left ("String expected, got: " ++ show object)

jsonToMaybeString :: J.JValue -> Either String (Maybe String)
jsonToMaybeString J.JNull = return Nothing
jsonToMaybeString object = Just <$> jsonToString object

jsonToDocument :: J.JValue -> Either String Document
jsonToDocument object =
  jsonRoot object >>= \(header, content) -> return $ Document (header, content)

jsonToContent :: J.JValue -> Either String Content
jsonToContent (J.JArray array) = jsonArrayToContent (J.JArray array)
jsonToContent (J.JObject object) = jsonObjectToContent (J.JObject object)
jsonToContent (J.JString string) = return $ Text string
jsonToContent (J.JNumber s [] 0) = return $ Text $ show s
jsonToContent (J.JNumber s f 0) = return $ Text $ show s ++ "." ++ concatMap show f
jsonToContent (J.JNumber s [] e) = return $ Text $ show s ++ "e" ++ show e
jsonToContent object = Left ("Did not expect this: " ++ show object)

jsonExecute :: String -> J.JValue -> Either String Content
jsonExecute "section" = jsonToSection
jsonExecute "bold" = jsonToBold
jsonExecute "italic" = jsonToItalic
jsonExecute "code" = jsonToCode
jsonExecute "codeblock" = jsonToCodeBlock
jsonExecute "list" = jsonToList
jsonExecute "link" = jsonToLink
jsonExecute "image" = jsonToImage
jsonExecute cmd = const (Left ("Unknown command: " ++ cmd))

jsonObjectToContent :: J.JValue -> Either String Content
jsonObjectToContent object =
  foldr
    ( \f acc -> case acc of
        Left _ -> jsonExecute f object
        _ -> acc
    )
    (Left "No valid tag found")
    $ jsonKeys object

jsonToSection :: J.JValue -> Either String Content
jsonToSection object = do
  section' <- jsonFindKey "section" object
  title' <- jsonFindKey "title" section' >>= jsonToString
  content <- jsonFindKey "content" section' >>= jsonToContent

  return $ Section (Just title') [content]

jsonToBold :: J.JValue -> Either String Content
jsonToBold object = do
  content <- jsonFindKey "bold" object >>= jsonToString
  return $ Bold content

jsonToItalic :: J.JValue -> Either String Content
jsonToItalic object = do
  content <- jsonFindKey "italic" object >>= jsonToString
  return $ Italic content

jsonToCode :: J.JValue -> Either String Content
jsonToCode object = do
  content <- jsonFindKey "code" object >>= jsonToString
  return $ Code content

jsonToCodeBlock :: J.JValue -> Either String Content
jsonToCodeBlock object = do
  array <- jsonFindKey "codeblock" object >>= jsonToArray
  inner <- mapM jsonToArray array
  content <- mapM (mapM jsonToContent) inner
  return $ CodeBlock (concat content)

jsonToList :: J.JValue -> Either String Content
jsonToList object = do
  array <- jsonFindKey "list" object >>= jsonToArray
  inner <- mapM jsonToArray array
  content <- mapM (mapM jsonToContent) inner
  return $ List (concat content)

jsonToLink :: J.JValue -> Either String Content
jsonToLink object = do
  link <- jsonFindKey "link" object >>= jsonToObject
  url <- jsonFindKey "url" link >>= jsonToString
  content <- jsonFindKey "content" link >>= jsonToContent
  return $ Link url content

jsonToImage :: J.JValue -> Either String Content
jsonToImage object = do
  image <- jsonFindKey "image" object >>= jsonToObject
  url <- jsonFindKey "url" image >>= jsonToString
  alt <- jsonFindKey "alt" image >>= jsonToArray
  firstAlt <- case alt of
    [a] -> jsonToString a
    _ -> Left "Array with one element expected"
  return $ Image url firstAlt

jsonArrayToContent :: J.JValue -> Either String Content
jsonArrayToContent (J.JArray array) = do
  content <- mapM jsonToContent array
  return $ Paragraph content
jsonArrayToContent _ = Left "Array expected"

jsonToArray :: J.JValue -> Either String [J.JValue]
jsonToArray (J.JArray array) = return array
jsonToArray _ = Left "Array expected"

jsonToObject :: J.JValue -> Either String J.JValue
jsonToObject (J.JObject object) = return $ J.JObject object
jsonToObject _ = Left "Object expected"

-- XML parsing

xmlToDocument :: X.XValue -> Either String Document
xmlToDocument = undefined

-- Markdown parsing

markdownToDocument :: M.MValue -> Either String Document
markdownToDocument = undefined
