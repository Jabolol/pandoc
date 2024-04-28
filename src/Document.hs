{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Document
  ( fromDocument,
    toDocument,
  )
where

import qualified Data.Maybe as Y
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
  | Image String Content
  | Paragraph [Content]
  | Section (Maybe String) [Content]
  | CodeBlock [Content]
  | List [Content]
  | Item [Content]
  | Body [Content]
  deriving (Show)

newtype Document = Document (Header, Content)

instance Show Document where
  show :: Document -> String
  show (Document (header, content)) =
    "Header: " ++ show header ++ "\nContent: " ++ show content

-- JSON parsing

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
  content <- jsonFindKey "body" object >>= jsonToBody

  return (header, content)

jsonToHeader :: J.JValue -> Either String Header
jsonToHeader object = do
  title' <- jsonFindKey "title" object >>= jsonToString
  author' <- jsonToMaybeString $ jsonFindKey "author" object
  date' <- jsonToMaybeString $ jsonFindKey "date" object

  return $ Header title' author' date'

jsonToString :: J.JValue -> Either String String
jsonToString (J.JString string) = return string
jsonToString object = Left ("String expected, got: " ++ show object)

jsonToMaybeString :: Either String J.JValue -> Either String (Maybe String)
jsonToMaybeString (Right (J.JString string)) = return $ Just string
jsonToMaybeString (Right _) = Left "String expected"
jsonToMaybeString (Left _) = return Nothing

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

jsonToBody :: J.JValue -> Either String Content
jsonToBody object = do
  array <- jsonToArray object
  content <- mapM jsonToContent array
  return $ Body content

jsonToSection :: J.JValue -> Either String Content
jsonToSection object = do
  section' <- jsonFindKey "section" object
  title' <- jsonFindKey "title" section' >>= jsonToString
  array <- jsonFindKey "content" section' >>= jsonToArray
  content <- mapM jsonToContent array
  return $ Section (Just title') content

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
  content <- mapM jsonToContent array
  return $ CodeBlock content

jsonToList :: J.JValue -> Either String Content
jsonToList object = do
  array <- jsonFindKey "list" object >>= jsonToArray
  content <- mapM jsonToContent array
  return $ List content

jsonToLink :: J.JValue -> Either String Content
jsonToLink object = do
  link <- jsonFindKey "link" object >>= jsonToObject
  url <- jsonFindKey "url" link >>= jsonToString
  raw <- jsonFindKey "content" link >>= jsonToContent
  content <- case raw of
    Paragraph [Text c] -> Right $ Text c
    _ -> Left "Content must be text"
  return $ Link url content

jsonToImage :: J.JValue -> Either String Content
jsonToImage object = do
  image <- jsonFindKey "image" object >>= jsonToObject
  url <- jsonFindKey "url" image >>= jsonToString
  raw <- jsonFindKey "alt" image >>= jsonToContent
  alt <- case raw of
    Paragraph [Text c] -> Right $ Text c
    _ -> Left "Alt must be text"
  return $ Image url alt

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

xmlRoot :: X.XValue -> Either String (Header, Content)
xmlRoot (X.XTag "document" _ children) = do
  header <- xmlFindTag "header" children >>= xmlToHeader
  content <- xmlFindTag "body" children >>= xmlToContent

  return (header, content)
xmlRoot object = Left ("Document expected, got: " ++ show object)

xmlToHeader :: X.XValue -> Either String Header
xmlToHeader (X.XTag "header" attrs children) = do
  title' <- xmlFindAttribute "title" attrs
  author' <- xmlToMaybeString $ xmlFindTag "author" children
  date' <- xmlToMaybeString $ xmlFindTag "date" children

  return $ Header title' author' date'
xmlToHeader object = Left ("Header expected, got: " ++ show object)

xmlToString :: X.XValue -> Either String String
xmlToString (X.XText string) = return string
xmlToString (X.XTag _ _ children) = do
  content <- mapM xmlToString children
  return $ concat content

xmlToMaybeString :: Either String X.XValue -> Either String (Maybe String)
xmlToMaybeString (Right object) = Just <$> xmlToString object
xmlToMaybeString (Left _) = return Nothing

xmlToDocument :: X.XValue -> Either String Document
xmlToDocument tags =
  xmlRoot tags >>= \(header, content) -> return $ Document (header, content)

xmlToContent :: X.XValue -> Either String Content
xmlToContent (X.XTag "body" _ children) = xmlToBody children
xmlToContent (X.XTag "section" attrs children) = xmlToSection children attrs
xmlToContent (X.XTag "bold" _ children) = xmlToBold children
xmlToContent (X.XTag "italic" _ children) = xmlToItalic children
xmlToContent (X.XTag "code" _ children) = xmlToCode children
xmlToContent (X.XTag "codeblock" _ children) = xmlToCodeBlock children
xmlToContent (X.XTag "list" _ children) = xmlToList children
xmlToContent (X.XTag "link" attrs children) = xmlToLink children attrs
xmlToContent (X.XTag "image" attrs children) = xmlToImage children attrs
xmlToContent (X.XTag "paragraph" _ children) = xmlToParagraph children
xmlToContent (X.XText string) = return $ Text string
xmlToContent (X.XTag name _ _) = Left ("Unknown tag: " ++ name)

xmlToBody :: [X.XValue] -> Either String Content
xmlToBody children = do
  content' <- mapM xmlToContent children
  return $ Body content'

xmlToSection :: [X.XValue] -> [(String, String)] -> Either String Content
xmlToSection children attrs = do
  title' <- xmlFindAttribute "title" attrs
  content' <- mapM xmlToContent children
  return $ Section (Just title') content'

xmlToBold :: [X.XValue] -> Either String Content
xmlToBold [X.XText string] = Right $ Bold string
xmlToBold _ = Left "Only text is allowed in bold"

xmlToItalic :: [X.XValue] -> Either String Content
xmlToItalic [X.XText string] = Right $ Italic string
xmlToItalic _ = Left "Only text is allowed in italic"

xmlToCode :: [X.XValue] -> Either String Content
xmlToCode [X.XText string] = Right $ Code string
xmlToCode _ = Left "Only text is allowed in code"

xmlToCodeBlock :: [X.XValue] -> Either String Content
xmlToCodeBlock children = do
  content' <- mapM xmlToContent children
  return $ CodeBlock content'

xmlToList :: [X.XValue] -> Either String Content
xmlToList children = do
  content' <- mapM xmlToContent children
  return $ List content'

xmlToLink :: [X.XValue] -> [(String, String)] -> Either String Content
xmlToLink [X.XText content] attrs = do
  url <- xmlFindAttribute "url" attrs
  return $ Link url (Text content)
xmlToLink _ _ = Left "Link must have a url and content"

xmlToImage :: [X.XValue] -> [(String, String)] -> Either String Content
xmlToImage alt attrs = do
  url <- xmlFindAttribute "url" attrs
  first <- case alt of
    [X.XText string] -> return $ Text string
    _ -> Left "Image alt must be text"
  return $ Image url first

xmlToParagraph :: [X.XValue] -> Either String Content
xmlToParagraph children = do
  content' <- mapM xmlToContent children
  return $ Paragraph content'

xmlFindAttribute :: String -> [(String, String)] -> Either String String
xmlFindAttribute attr attrs =
  case lookup attr attrs of
    Just value -> return value
    Nothing -> Left ("Attribute " ++ attr ++ " not found")

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

-- Markdown parsing

markdownToDocument :: M.MValue -> Either String Document
markdownToDocument (M.MBody ((M.MMeta h) : b)) = do
  header <- markdownToHeader h
  content <- markdownToContent (M.MBody b)
  return $ Document (header, content)
markdownToDocument _ = Left "Document expected"

markdownToHeader :: [(String, String)] -> Either String Header
markdownToHeader attrs = do
  title' <- markdownFindKey "title" attrs
  author' <- markdownToMaybeString $ markdownFindKey "author" attrs
  date' <- markdownToMaybeString $ markdownFindKey "date" attrs

  return $ Header title' author' date'

markdownFindKey :: String -> [(String, String)] -> Either String String
markdownFindKey key attrs =
  case lookup key attrs of
    Just value -> return value
    Nothing -> Left ("Key " ++ key ++ " not found")

markdownToMaybeString :: Either String String -> Either String (Maybe String)
markdownToMaybeString (Right string) = return $ Just string
markdownToMaybeString (Left _) = return Nothing

markdownToContent :: M.MValue -> Either String Content
markdownToContent (M.MBody content) = do
  content' <- mapM markdownToContent content
  return $ Body content'
markdownToContent (M.MSection (M.MHeader _ title') body) = do
  content' <- mapM markdownToContent body
  return $ Section (Just title') content'
markdownToContent (M.MParagraph content) = do
  content' <- mapM markdownToContent content
  return $ Paragraph content'
markdownToContent (M.MBold string) = return $ Bold string
markdownToContent (M.MItalic string) = return $ Italic string
markdownToContent (M.MCode string) = return $ Code string
markdownToContent (M.MLink alt url) = return $ Link url $ Text alt
markdownToContent (M.MImage alt url) = return $ Image url $ Text alt
markdownToContent (M.MList content) = do
  content' <- mapM (\x -> markdownToContent (M.MParagraph [x])) content
  return $ List content'
markdownToContent (M.MCodeBlock content) = do
  content' <- mapM (\x -> markdownToContent (M.MParagraph [x])) content
  return $ CodeBlock content'
markdownToContent (M.MText _ string) = return $ Text string
markdownToContent d = Left ("Unexpected content: " ++ show d)

-- Document to JSON

documentToJson :: Document -> J.JValue
documentToJson doc = J.JObject [("header", header), ("body", body)]
  where
    (header, body) = documentToJson' doc

documentToJson' :: Document -> (J.JValue, J.JValue)
documentToJson' (Document (header, content)) =
  (headerToJson header, contentToJson content)

headerToJson :: Header -> J.JValue
headerToJson (Header title' author' date') =
  J.JObject $
    ("title", J.JString title')
      : Y.catMaybes
        [ fmap (\a -> ("author", J.JString a)) author',
          fmap (\d -> ("date", J.JString d)) date'
        ]

contentToJson :: Content -> J.JValue
contentToJson (Text string) = J.JString string
contentToJson (Italic string) =
  J.JObject
    [ ("italic", J.JString string)
    ]
contentToJson (Bold string) =
  J.JObject
    [ ("bold", J.JString string)
    ]
contentToJson (Code string) =
  J.JObject
    [ ("code", J.JString string)
    ]
contentToJson (Link url content) =
  J.JObject
    [ ( "link",
        J.JObject
          [ ("url", J.JString url),
            ("content", J.JArray [contentToJson content])
          ]
      )
    ]
contentToJson (Image url alt) =
  J.JObject
    [ ( "image",
        J.JObject
          [ ("url", J.JString url),
            ("alt", J.JArray [contentToJson alt])
          ]
      )
    ]
contentToJson (Paragraph content) = J.JArray $ map contentToJson content
contentToJson (Section title' content) =
  J.JObject
    [ ( "section",
        J.JObject
          [ ("title", J.JString $ Y.fromMaybe "" title'),
            ("content", J.JArray $ map contentToJson content)
          ]
      )
    ]
contentToJson (CodeBlock content) =
  J.JObject
    [ ("codeblock", J.JArray $ map contentToJson content)
    ]
contentToJson (List content) =
  J.JObject
    [ ("list", J.JArray $ map contentToJson content)
    ]
contentToJson (Item content) =
  J.JObject
    [ ("item", J.JArray $ map contentToJson content)
    ]
contentToJson (Body content) =
  J.JArray $ map contentToJson content

-- Document to XML

documentToXML :: Document -> X.XValue
documentToXML doc = X.XTag "document" [] [header, body]
  where
    (header, body) = documentToXML' doc

documentToXML' :: Document -> (X.XValue, X.XValue)
documentToXML' (Document (header, content)) =
  (headerToXML header, contentToXML content)

headerToXML :: Header -> X.XValue
headerToXML (Header title' author' date') =
  X.XTag "header" [("title", title')] $
    Y.catMaybes
      [ fmap (\a -> X.XTag "author" [] [X.XText a]) author',
        fmap (\d -> X.XTag "date" [] [X.XText d]) date'
      ]

contentToXML :: Content -> X.XValue
contentToXML (Text string) = X.XText string
contentToXML (Italic string) =
  X.XTag "italic" [] [X.XText string]
contentToXML (Bold string) =
  X.XTag "bold" [] [X.XText string]
contentToXML (Code string) =
  X.XTag "code" [] [X.XText string]
contentToXML (Link url content) =
  X.XTag "link" [("url", url)] [contentToXML content]
contentToXML (Image url alt) =
  X.XTag "image" [("url", url)] [contentToXML alt]
contentToXML (Paragraph content) =
  X.XTag "paragraph" [] $
    map contentToXML content
contentToXML (Section title' content) =
  X.XTag "section" [("title", Y.fromMaybe "" title')] $
    map contentToXML content
contentToXML (CodeBlock content) =
  X.XTag "codeblock" [] $
    map contentToXML content
contentToXML (List content) =
  X.XTag "list" [] $
    map contentToXML content
contentToXML (Item content) =
  X.XTag "item" [] $
    map contentToXML content
contentToXML (Body content) =
  X.XTag "body" [] $
    map contentToXML content

-- Document to Markdown

documentToMarkdown :: Document -> M.MValue
documentToMarkdown doc = M.MRoot (header, body)
  where
    (header, body) = documentToMarkdown' doc

documentToMarkdown' :: Document -> (M.MValue, M.MValue)
documentToMarkdown' (Document (header, content)) =
  (headerToMarkdown header, contentToMarkdown content)

headerToMarkdown :: Header -> M.MValue
headerToMarkdown (Header title' author' date') =
  M.MMeta $
    ("title", title')
      : Y.catMaybes
        [ fmap ("author",) author',
          fmap ("date",) date'
        ]

contentToMarkdown :: Content -> M.MValue
contentToMarkdown (Text string) = M.MText False string
contentToMarkdown (Italic string) = M.MItalic string
contentToMarkdown (Bold string) = M.MBold string
contentToMarkdown (Code string) = M.MCode string
contentToMarkdown (Link url (Text alt)) = M.MLink url alt
contentToMarkdown (Link url _) = M.MLink url ""
contentToMarkdown (Image url (Text alt)) = M.MImage url alt
contentToMarkdown (Image url _) = M.MImage url ""
contentToMarkdown (Paragraph content) =
  M.MParagraph $
    map contentToMarkdown content
contentToMarkdown (Section title' content) =
  M.MSection (M.MHeader 1 $ Y.fromMaybe "" title') $
    map contentToMarkdown content
contentToMarkdown (CodeBlock content) =
  M.MCodeBlock $
    map contentToMarkdown content
contentToMarkdown (List content) =
  M.MList $
    map contentToMarkdown content
contentToMarkdown (Item content) =
  M.MList $
    map contentToMarkdown content
contentToMarkdown (Body content) =
  M.MBody $
    map contentToMarkdown content

-- Document manipulation

toDocument :: String -> String -> Either String Document
toDocument "xml" x =
  Y.fromMaybe (Left "Failed to parse input file.") $
    X.parseXML x >>= Just . xmlToDocument
toDocument "json" x =
  Y.fromMaybe (Left "Failed to parse input file.") $
    J.parseJSON x >>= Just . jsonToDocument
toDocument "markdown" x =
  Y.fromMaybe (Left "Failed to parse input file.") $
    M.parseMarkdown x >>= Just . markdownToDocument
toDocument fmt _ = Left $ "Unsupported format: " ++ fmt

fromDocument :: String -> Document -> Either String String
fromDocument "xml" doc =
  Right $
    X.xToString $
      documentToXML doc
fromDocument "json" doc =
  Right $
    J.jToString $
      documentToJson doc
fromDocument "markdown" doc =
  Right $
    M.mToString $
      M.updateHeaders $
        documentToMarkdown doc
fromDocument fmt _ = Left $ "Unsupported format: " ++ fmt
