module Core
  ( J.parseJSON,
    J.jToString,
    X.xToString,
    X.parseXML,
    M.mToString,
    M.parseMarkdown,
    D.jsonToDocument,
    D.xmlToDocument,
    D.markdownToDocument,
    D.documentToJson,
    D.documentToXML,
    D.documentToMarkdown,
  )
where

import qualified Document as D
import qualified JSON as J
import qualified Markdown as M
import qualified XML as X
