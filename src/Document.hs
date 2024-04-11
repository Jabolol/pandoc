{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

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
-- TODO: Refactor jsonToHeader so that author and date are optional
-- TODO: Allow comments to enable `.jsonc` files

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
jsonToContent (J.JNumber s f e) = return $ Text $ show s ++ "." ++ concatMap show f ++ "e" ++ show e
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
jsonExecute cmd = const (Left ("Unknown tag: " ++ cmd))

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
    _ -> Left "Do not leave alt empty or have more than one alt"
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
-- TODO: Add support for comments <!-- whatever -->
-- TODO: Fix author/date allowing nested tags instead of just text

xmlToDocument :: X.XValue -> Either String Document
xmlToDocument tags =
  xmlRoot tags >>= \(header, content) -> return $ Document (header, content)

xmlRoot :: X.XValue -> Either String (Header, Content)
xmlRoot (X.XTag "document" _ children) = do
  header <- xmlFindTag "header" children >>= xmlToHeader
  content <- xmlFindTag "body" children >>= xmlToContent

  return (header, content)
xmlRoot object = Left ("Document expected, got: " ++ show object)

xmlFindTag :: String -> [X.XValue] -> Either String X.XValue
xmlFindTag tag tags = do
  let result =
        filter
          ( \case
              X.XTag name _ _ -> name == tag
              _ -> False
          )
          tags
  case result of
    [tag'] -> return tag'
    _ -> Left ("Tag " ++ tag ++ " not found")

xmlToHeader :: X.XValue -> Either String Header
xmlToHeader (X.XTag "header" attrs children) = do
  title' <- xmlFindAttribute "title" attrs
  author' <- xmlFindTag "author" children >>= xmlToMaybeString
  date' <- xmlFindTag "date" children >>= xmlToMaybeString

  return $ Header title' author' date'
xmlToHeader object = Left ("Header expected, got: " ++ show object)

xmlToString :: X.XValue -> Either String String
xmlToString (X.XText string) = return string
xmlToString (X.XTag _ _ children) = do
  content <- mapM xmlToString children
  return $ concat content

xmlToMaybeString :: X.XValue -> Either String (Maybe String)
xmlToMaybeString (X.XText "") = return Nothing
xmlToMaybeString object = Just <$> xmlToString object

xmlToContent :: X.XValue -> Either String Content
xmlToContent (X.XTag "body" _ children) = xmlToContent' children
xmlToContent object = Left ("Did not expect this: " ++ show object)

xmlToContent' :: [X.XValue] -> Either String Content
xmlToContent' [X.XTag "section" _ children] = do
  title' <- xmlFindTag "title" children >>= xmlToString
  content <- xmlFindTag "content" children >>= xmlToContent
  return $ Section (Just title') [content]
xmlToContent' [X.XTag "bold" _ [X.XText content]] = return $ Bold content
xmlToContent' [X.XTag "italic" _ [X.XText content]] = return $ Italic content
xmlToContent' [X.XTag "code" _ [X.XText content]] = return $ Code content
xmlToContent' [X.XTag "codeblock" _ children] = do
  content <- mapM xmlToContent children
  return $ CodeBlock content
xmlToContent' [X.XTag "list" _ children] = do
  content <- mapM xmlToContent children
  return $ List content
xmlToContent' [X.XTag "link" _ children] = do
  url <- xmlFindTag "url" children >>= xmlToString
  content <- xmlFindTag "content" children >>= xmlToContent
  return $ Link url content
xmlToContent' [X.XTag "image" _ children] = do
  url <- xmlFindTag "url" children >>= xmlToString
  alt <- xmlFindTag "alt" children >>= xmlToString
  return $ Image url alt
xmlToContent' children = do
  content <- mapM xmlToContent children
  return $ Paragraph content

xmlFindAttribute :: String -> [(String, String)] -> Either String String
xmlFindAttribute attr attrs =
  case lookup attr attrs of
    Just value -> return value
    Nothing -> Left ("Attribute " ++ attr ++ " not found")

-- Markdown parsing

markdownToDocument :: M.MValue -> Either String Document
markdownToDocument = undefined
