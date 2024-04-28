import JSON
  ( JValue
      ( JArray,
        JBool,
        JNull,
        JNumber,
        JObject,
        JString,
        frac,
        int
      ),
    parseJSON,
  )
import qualified JSON as J
import Markdown
  ( MValue
      ( MBody,
        MBold,
        MCode,
        MCodeBlock,
        MHeader,
        MImage,
        MItalic,
        MLink,
        MList,
        MMeta,
        MParagraph,
        MSection,
        MText
      ),
    parseMarkdown,
  )
import Shared
  ( Parser (parse),
    char,
    digit,
    digitOneToNine,
    digits,
    escapeChar,
    isSpecial,
    separatedBy,
    spaces,
    string,
    surroundedBy,
    tabs,
    toTab,
    trim,
    trimNewlines,
  )
import Test.Hspec (describe, hspec, it, shouldBe)
import XML (XValue (XTag, XText), parseXML)

main :: IO ()
main = hspec $ do
  describe "trim" $ do
    it "should remove consecutive spaces" $ do
      trim "  hello  world  " `shouldBe` " hello world "

  describe "trimNewlines" $ do
    it "should remove trailing newlines from a string" $ do
      trimNewlines "hello\nworld\n\n\n" `shouldBe` "hello\nworld"

  describe "escapeChar" $ do
    it "should escape special characters" $ do
      escapeChar '"' `shouldBe` "\\\""
      escapeChar '\\' `shouldBe` "\\\\"
      escapeChar '\b' `shouldBe` "\\b"
      escapeChar '\f' `shouldBe` "\\f"
      escapeChar '\n' `shouldBe` "\\n"
      escapeChar '\r' `shouldBe` "\\r"
      escapeChar '\t' `shouldBe` "\\t"
      escapeChar 'a' `shouldBe` "a"

    describe "toTab" $ do
      it "should replace 4 spaces with a tab" $ do
        toTab "    hello" `shouldBe` "\thello"

    describe "surroundedBy" $ do
      it "should parse a string surrounded by quotes" $ do
        parse (surroundedBy (string "hello") (char '"')) "\"hello\"" `shouldBe` Just ("", "hello")

    describe "separatedBy" $ do
      it "should parse a list of strings separated by commas" $ do
        parse (separatedBy (string "hello") (char ',')) "hello,hello,hello" `shouldBe` Just ("", ["hello", "hello", "hello"])

    describe "spaces" $ do
      it "should parse spaces and newlines" $ do
        parse spaces " \n \n \n" `shouldBe` Just ("", " \n \n \n")

    describe "tabs" $ do
      it "should parse tabs and newlines" $ do
        parse tabs "\t\n\t\n\t\n" `shouldBe` Just ("", "\t\n\t\n\t\n")

    describe "string" $ do
      it "should parse a string" $ do
        parse (string "hello") "hello" `shouldBe` Just ("", "hello")
        parse (string "hello") "world" `shouldBe` Nothing
        parse (string "hello") "hello world" `shouldBe` Just (" world", "hello")

    describe "char" $ do
      it "should parse a character" $ do
        parse (char 'h') "hello" `shouldBe` Just ("ello", 'h')
        parse (char 'h') "world" `shouldBe` Nothing

    describe "digit" $ do
      it "should parse a digit" $ do
        parse digit "1" `shouldBe` Just ("", 1)
        parse digit "a" `shouldBe` Nothing

    describe "digits" $ do
      it "should parse a list of digits" $ do
        parse digits "123" `shouldBe` Just ("", "123")
        parse digits "abc" `shouldBe` Nothing

    describe "digitOneToNine" $ do
      it "should parse a digit between 1 and 9" $ do
        parse digitOneToNine "1" `shouldBe` Just ("", '1')
        parse digitOneToNine "0" `shouldBe` Nothing
        parse digitOneToNine "a" `shouldBe` Nothing

    describe "isSpecial" $ do
      it "should check if a character is a special character" $ do
        isSpecial '\\' `shouldBe` False
        isSpecial 'a' `shouldBe` False
        isSpecial '1' `shouldBe` False
        isSpecial ' ' `shouldBe` False
        isSpecial '\n' `shouldBe` False
        isSpecial '\t' `shouldBe` False
        isSpecial '\r' `shouldBe` False
        isSpecial '\b' `shouldBe` False
        isSpecial '\f' `shouldBe` False
        isSpecial '"' `shouldBe` False
        isSpecial '\'' `shouldBe` False
        isSpecial '`' `shouldBe` True
        isSpecial '!' `shouldBe` True
        isSpecial '@' `shouldBe` False
        isSpecial '#' `shouldBe` True
        isSpecial '$' `shouldBe` False
        isSpecial '%' `shouldBe` False
        isSpecial '^' `shouldBe` False
        isSpecial '&' `shouldBe` False
        isSpecial '*' `shouldBe` True
        isSpecial '(' `shouldBe` False
        isSpecial ')' `shouldBe` False
        isSpecial '-' `shouldBe` False
        isSpecial '_' `shouldBe` True
        isSpecial '=' `shouldBe` False
        isSpecial '+' `shouldBe` False
        isSpecial '[' `shouldBe` True
        isSpecial ']' `shouldBe` False
        isSpecial '{' `shouldBe` False
        isSpecial '}' `shouldBe` False
        isSpecial '|' `shouldBe` False
        isSpecial ';' `shouldBe` False
        isSpecial ':' `shouldBe` False
        isSpecial ',' `shouldBe` False
        isSpecial '.' `shouldBe` False
        isSpecial '<' `shouldBe` False
        isSpecial '>' `shouldBe` False
        isSpecial '/' `shouldBe` False
        isSpecial '?' `shouldBe` False
        isSpecial '~' `shouldBe` True
        isSpecial '`' `shouldBe` True
        isSpecial '@' `shouldBe` False
        isSpecial '#' `shouldBe` True
        isSpecial '$' `shouldBe` False
        isSpecial '%' `shouldBe` False
        isSpecial '^' `shouldBe` False
        isSpecial '&' `shouldBe` False
        isSpecial '*' `shouldBe` True
        isSpecial '(' `shouldBe` False
        isSpecial ')' `shouldBe` False
        isSpecial '-' `shouldBe` False

    describe "JSON parsing" $ do
      it "should parse a simple JSON object" $ do
        let json = "{\"hello\": \"world\"}"
        let parsed = case parseJSON json of
              Just obj -> obj
              _ -> error "Failed to parse JSON"
        parsed
          `shouldBe` JObject
            [ ("hello", JString "world")
            ]

      it "should parse a simple JSON array" $ do
        let json = "[\"hello\", \"world\"]"
        let parsed = case parseJSON json of
              Just obj -> obj
              _ -> error "Failed to parse JSON"
        parsed
          `shouldBe` JArray
            [ JString "hello",
              JString "world"
            ]

      it "should parse a simple JSON number" $ do
        let json = "{\"number\": 123}"
        let parsed = case parseJSON json of
              Just obj -> obj
              _ -> error "Failed to parse JSON"
        parsed
          `shouldBe` JObject
            [ ( "number",
                JNumber
                  { int = 123,
                    frac = [],
                    J.exp = 0
                  }
              )
            ]

      it "should parse a simple nested JSON object" $ do
        let json = "{\"hello\": {\"world\": \"hello\"}}"
        let parsed = case parseJSON json of
              Just obj -> obj
              _ -> error "Failed to parse JSON"
        parsed
          `shouldBe` JObject
            [ ( "hello",
                JObject
                  [ ("world", JString "hello")
                  ]
              )
            ]

      it "should parse a simple nested JSON array" $ do
        let json = "{\"hello\": [\"world\", \"hello\"]}"
        let parsed = case parseJSON json of
              Just obj -> obj
              _ -> error "Failed to parse JSON"
        parsed
          `shouldBe` JObject
            [ ( "hello",
                JArray
                  [ JString "world",
                    JString "hello"
                  ]
              )
            ]

      it "should parse a simple JSON boolean" $ do
        let json = "{\"hello\": true}"
        let parsed = case parseJSON json of
              Just obj -> obj
              _ -> error "Failed to parse JSON"
        parsed
          `shouldBe` JObject
            [ ("hello", JBool True)
            ]

      it "should parse a simple JSON null" $ do
        let json = "{\"hello\": null}"
        let parsed = case parseJSON json of
              Just obj -> obj
              _ -> error "Failed to parse JSON"
        parsed
          `shouldBe` JObject
            [ ("hello", JNull)
            ]

      it "should parse a simple JSON number with a fraction" $ do
        let json = "{\"number\": 123.456}"
        let parsed = case parseJSON json of
              Just obj -> obj
              _ -> error "Failed to parse JSON"
        parsed
          `shouldBe` JObject
            [ ( "number",
                JNumber
                  { int = 123,
                    frac = [4, 5, 6],
                    J.exp = 0
                  }
              )
            ]

      it "should parse an advanced JSON object" $ do
        let json = "{\"hello\": \"world\", \"number\": 123, \"bool\": true, \"null\": null, \"array\": [\"hello\", \"world\"], \"object\": {\"hello\": \"world\"}}"
        let parsed = case parseJSON json of
              Just obj -> obj
              _ -> error "Failed to parse JSON"
        parsed
          `shouldBe` JObject
            [ ("hello", JString "world"),
              ( "number",
                JNumber
                  { int = 123,
                    frac = [],
                    J.exp = 0
                  }
              ),
              ("bool", JBool True),
              ("null", JNull),
              ( "array",
                JArray
                  [ JString "hello",
                    JString "world"
                  ]
              ),
              ( "object",
                JObject
                  [ ("hello", JString "world")
                  ]
              )
            ]

      it "should parse an advanced JSON object with a number with an exponent" $ do
        let json = "{\"number\": 123.456e7}"
        let parsed = case parseJSON json of
              Just obj -> obj
              _ -> error "Failed to parse JSON"
        parsed
          `shouldBe` JObject
            [ ( "number",
                JNumber
                  { int = 123,
                    frac = [4, 5, 6],
                    J.exp = 7
                  }
              )
            ]

      it "should parse an advanced JSON object with a number with a negative exponent" $ do
        let json = "{\"number\": 123.456e-7}"
        let parsed = case parseJSON json of
              Just obj -> obj
              _ -> error "Failed to parse JSON"
        parsed
          `shouldBe` JObject
            [ ( "number",
                JNumber
                  { int = 123,
                    frac = [4, 5, 6],
                    J.exp = -7
                  }
              )
            ]

      it "should parse an advanced JSON object with a number with a positive exponent" $ do
        let json = "{\"number\": 123.456e+7}"
        let parsed = case parseJSON json of
              Just obj -> obj
              _ -> error "Failed to parse JSON"
        parsed
          `shouldBe` JObject
            [ ( "number",
                JNumber
                  { int = 123,
                    frac = [4, 5, 6],
                    J.exp = 7
                  }
              )
            ]

      it "should parse a large JSON object" $ do
        let json = "{\"hello\": \"world\", \"number\": 123, \"bool\": true, \"null\": null, \"array\": [\"hello\", \"world\"], \"object\": {\"hello\": \"world\"}, \"number2\": 123.456, \"number3\": 123.456e7, \"number4\": 123.456e-7, \"number5\": 123.456e+7}"
        let parsed = case parseJSON json of
              Just obj -> obj
              _ -> error "Failed to parse JSON"
        parsed
          `shouldBe` JObject
            [ ("hello", JString "world"),
              ("number", JNumber {int = 123, frac = [], J.exp = 0}),
              ("bool", JBool True),
              ("null", JNull),
              ( "array",
                JArray
                  [ JString "hello",
                    JString "world"
                  ]
              ),
              ( "object",
                JObject
                  [ ("hello", JString "world")
                  ]
              ),
              ( "number2",
                JNumber
                  { int = 123,
                    frac = [4, 5, 6],
                    J.exp = 0
                  }
              ),
              ( "number3",
                JNumber
                  { int = 123,
                    frac = [4, 5, 6],
                    J.exp = 7
                  }
              ),
              ( "number4",
                JNumber
                  { int = 123,
                    frac = [4, 5, 6],
                    J.exp = -7
                  }
              ),
              ( "number5",
                JNumber
                  { int = 123,
                    frac = [4, 5, 6],
                    J.exp = 7
                  }
              )
            ]

    describe "XML parsing" $ do
      it "should parse a simple XML object" $ do
        let xml = "<hello>world</hello>"
        let parsed = case parseXML xml of
              Just obj -> obj
              _ -> error "Failed to parse XML"
        parsed `shouldBe` XTag "hello" [] [XText "world"]

      it "should parse a simple XML object with attributes" $ do
        let xml = "<hello world=\"world\">world</hello>"
        let parsed = case parseXML xml of
              Just obj -> obj
              _ -> error "Failed to parse XML"
        parsed
          `shouldBe` XTag
            "hello"
            [ ("world", "world")
            ]
            [XText "world"]

      it "should parse a simple XML object with a nested object" $ do
        let xml = "<hello><world>world</world></hello>"
        let parsed = case parseXML xml of
              Just obj -> obj
              _ -> error "Failed to parse XML"
        parsed
          `shouldBe` XTag
            "hello"
            []
            [ XTag
                "world"
                []
                [ XText "world"
                ]
            ]

      it "should parse a simple XML object with a nested object with attributes" $ do
        let xml = "<hello><world world=\"world\">world</world></hello>"
        let parsed = case parseXML xml of
              Just obj -> obj
              _ -> error "Failed to parse XML"
        parsed
          `shouldBe` XTag
            "hello"
            []
            [ XTag
                "world"
                [ ("world", "world")
                ]
                [XText "world"]
            ]

      it "should parse a simple XML object with a nested object with a nested object" $ do
        let xml = "<hello><world><hello>world</hello></world></hello>"
        let parsed = case parseXML xml of
              Just obj -> obj
              _ -> error "Failed to parse XML"
        parsed
          `shouldBe` XTag
            "hello"
            []
            [ XTag
                "world"
                []
                [ XTag
                    "hello"
                    []
                    [ XText "world"
                    ]
                ]
            ]

      it "should parse a simple XML object with a nested object with a nested object with attributes" $ do
        let xml = "<hello><world><hello world=\"world\">world</hello></world></hello>"
        let parsed = case parseXML xml of
              Just obj -> obj
              _ -> error "Failed to parse XML"
        parsed
          `shouldBe` XTag
            "hello"
            []
            [ XTag
                "world"
                []
                [ XTag
                    "hello"
                    [ ("world", "world")
                    ]
                    [XText "world"]
                ]
            ]

      it "should parse a simple XML object with a nested object with a nested object with a nested object" $ do
        let xml = "<hello><world><hello><world>world</world></hello></world></hello>"
        let parsed = case parseXML xml of
              Just obj -> obj
              _ -> error "Failed to parse XML"
        parsed
          `shouldBe` XTag
            "hello"
            []
            [ XTag
                "world"
                []
                [ XTag "hello" [] [XTag "world" [] [XText "world"]]
                ]
            ]

      it "should parse a large XML object" $ do
        let xml = "<hello><world><hello><world>world</world></hello></world></hello>"
        let parsed = case parseXML xml of
              Just obj -> obj
              _ -> error "Failed to parse XML"
        parsed
          `shouldBe` XTag
            "hello"
            []
            [ XTag
                "world"
                []
                [ XTag "hello" [] [XTag "world" [] [XText "world"]]
                ]
            ]

      it "should parse a large XML object with attributes" $ do
        let xml = "<hello world=\"world\"><world world=\"world\"><hello>world</hello></world></hello>"
        let parsed = case parseXML xml of
              Just obj -> obj
              _ -> error "Failed to parse XML"
        parsed
          `shouldBe` XTag
            "hello"
            [ ("world", "world")
            ]
            [ XTag
                "world"
                [ ("world", "world")
                ]
                [XTag "hello" [] [XText "world"]]
            ]

      it "should parse a large XML object with attributes and text" $ do
        let xml = "<hello world=\"world\">world<world world=\"world\"><hello>world</hello></world></hello>"
        let parsed = case parseXML xml of
              Just obj -> obj
              _ -> error "Failed to parse XML"
        parsed
          `shouldBe` XTag
            "hello"
            [ ("world", "world")
            ]
            [ XText "world",
              XTag
                "world"
                [ ("world", "world")
                ]
                [XTag "hello" [] [XText "world"]]
            ]

      it "should parse a large XML object with attributes and text and a nested object" $ do
        let xml = "<hello world=\"world\">world<world world=\"world\"><hello>world</hello></world><world>world</world></hello>"
        let parsed = case parseXML xml of
              Just obj -> obj
              _ -> error "Failed to parse XML"
        parsed
          `shouldBe` XTag
            "hello"
            [ ("world", "world")
            ]
            [ XText "world",
              XTag
                "world"
                [ ("world", "world")
                ]
                [ XTag
                    "hello"
                    []
                    [ XText "world"
                    ]
                ],
              XTag "world" [] [XText "world"]
            ]

    describe "Markdown parsing" $ do
      it "should parse a simple markdown paragraph" $ do
        let markdown = "hello world\n"
        let parsed = case parseMarkdown markdown of
              Just obj -> obj
              _ -> error "Failed to parse Markdown"
        parsed
          `shouldBe` MBody
            [ MParagraph
                [ MText True "hello world"
                ]
            ]

      it "should parse a simple bold markdown paragraph" $ do
        let markdown = "hello **world** haha"
        let parsed = case parseMarkdown markdown of
              Just obj -> obj
              _ -> error "Failed to parse Markdown"
        parsed
          `shouldBe` MBody
            [ MParagraph
                [ MText False "hello ",
                  MBold "world",
                  MText False " haha"
                ]
            ]

      it "should parse a simple italic markdown paragraph" $ do
        let markdown = "hello *world* haha"
        let parsed = case parseMarkdown markdown of
              Just obj -> obj
              _ -> error "Failed to parse Markdown"
        parsed
          `shouldBe` MBody
            [ MParagraph
                [ MText False "hello ",
                  MItalic "world",
                  MText False " haha"
                ]
            ]

      it "should parse a section with simple text inside" $ do
        let markdown = "# hello world\nThis is **my** birthday"
        let parsed = case parseMarkdown markdown of
              Just obj -> obj
              _ -> error "Failed to parse Markdown"
        parsed
          `shouldBe` MBody
            [ MSection
                (MHeader 1 "hello world")
                [ MParagraph
                    [ MText False "This is ",
                      MBold "my",
                      MText False " birthday"
                    ]
                ]
            ]

      it "should parse a complicated file" $ do
        let markdown = "---\ntitle: Syntaxe MARKDOWN\nauthor: Fornes Leo\ndate: 2024-01-01\n---\n\nThis document is a simple example of the MARKDOWN syntax.\n\nEvery syntax element is displayed in this document.\n\n# header 1\n\nThis is a basic paragraph with text.\n\nThis is a paragraph with **bold**, *italic* and `code` text.\n\n## header 2\n\n```\nThis is a code block.\n```\n- list item 1\n- list item 2\n- list item 3\n\nThis is a paragraph with a [link](https://www.youtube.com/watch?v=dQw4w9WgXcQ&ab_channel=RickAstley).\nThis is a paragraph with an image ![Text to replace image](https://cdn-images-1.medium.com/max/697/1*tsHrUKwQXG1YZX0l957ISw.png).\n\n#### header 4\n\nEvery syntax element can be use separately or combined.\n\nThink about the different possible combinations.\n\nAll combined syntax elements aren't displayed in this document.\n\n"
        let parsed = case parseMarkdown markdown of
              Just obj -> obj
              _ -> error "Failed to parse Markdown"
        parsed
          `shouldBe` MBody
            [ MMeta [("title", "Syntaxe MARKDOWN"), ("author", "Fornes Leo"), ("date", "2024-01-01")],
              MParagraph
                [ MText True "This document is a simple example of the MARKDOWN syntax."
                ],
              MParagraph
                [ MText True "Every syntax element is displayed in this document."
                ],
              MSection
                (MHeader 1 "header 1")
                [ MParagraph
                    [ MText True "This is a basic paragraph with text."
                    ],
                  MParagraph
                    [ MText False "This is a paragraph with ",
                      MBold "bold",
                      MText False ", ",
                      MItalic "italic",
                      MText False " and ",
                      MCode "code",
                      MText True " text."
                    ],
                  MSection
                    (MHeader 2 "header 2")
                    [ MCodeBlock
                        [ MText True "This is a code block."
                        ],
                      MList
                        [ MText False "list item 1",
                          MText False "list item 2",
                          MText False "list item 3"
                        ],
                      MParagraph
                        [ MText False "This is a paragraph with a ",
                          MLink "link" "https://www.youtube.com/watch?v=dQw4w9WgXcQ&ab_channel=RickAstley",
                          MText True "."
                        ],
                      MParagraph
                        [ MText False "This is a paragraph with an image ",
                          MImage "Text to replace image" "https://cdn-images-1.medium.com/max/697/1*tsHrUKwQXG1YZX0l957ISw.png",
                          MText True "."
                        ],
                      MSection
                        (MHeader 4 "header 4")
                        [ MParagraph
                            [ MText True "Every syntax element can be use separately or combined."
                            ],
                          MParagraph
                            [ MText True "Think about the different possible combinations."
                            ],
                          MParagraph
                            [ MText True "All combined syntax elements aren't displayed in this document."
                            ]
                        ]
                    ]
                ]
            ]
