{-# LANGUAGE OverloadedStrings #-}

module SSESpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (counterexample)

import Network.SSE

-- ---------------------------------------------------------------------------
-- Spec
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "parseEvent" $ do
    describe "data field" $ do
      it "parses a single data field" $
        parseEvent "data: hello" `shouldBe`
          SSEEvent Nothing Nothing "hello" Nothing

      it "concatenates multiple data fields with newlines" $
        parseEvent "data: line1\ndata: line2\ndata: line3" `shouldBe`
          SSEEvent Nothing Nothing "line1\nline2\nline3" Nothing

      it "preserves empty data field (bare 'data:')" $
        parseEvent "data:" `shouldBe`
          SSEEvent Nothing Nothing "" Nothing

      it "strips exactly one leading space after colon" $
        parseEvent "data:  two-spaces" `shouldBe`
          SSEEvent Nothing Nothing " two-spaces" Nothing

      it "accepts data field with no space after colon" $
        parseEvent "data:nospace" `shouldBe`
          SSEEvent Nothing Nothing "nospace" Nothing

    describe "event field" $ do
      it "parses event type" $
        parseEvent "event: ping\ndata: {}" `shouldBe`
          SSEEvent Nothing (Just "ping") "{}" Nothing

      it "last event field wins" $
        parseEvent "event: first\nevent: second\ndata: x" `shouldBe`
          SSEEvent Nothing (Just "second") "x" Nothing

    describe "id field" $ do
      it "parses event id" $
        parseEvent "id: 42\ndata: x" `shouldBe`
          SSEEvent (Just "42") Nothing "x" Nothing

      it "last id field wins" $
        parseEvent "id: 1\nid: 2\ndata: x" `shouldBe`
          SSEEvent (Just "2") Nothing "x" Nothing

    describe "retry field" $ do
      it "parses valid retry value" $
        parseEvent "retry: 3000\ndata: x" `shouldBe`
          SSEEvent Nothing Nothing "x" (Just 3000)

      it "ignores non-numeric retry value" $
        parseEvent "retry: bad\ndata: x" `shouldBe`
          SSEEvent Nothing Nothing "x" Nothing

      it "ignores retry with decimal point" $
        parseEvent "retry: 1.5\ndata: x" `shouldBe`
          SSEEvent Nothing Nothing "x" Nothing

    describe "comments" $ do
      it "ignores lines starting with colon" $
        parseEvent ": this is a comment\ndata: hello" `shouldBe`
          SSEEvent Nothing Nothing "hello" Nothing

      it "ignores multiple comments" $
        parseEvent ": comment1\n: comment2\ndata: x" `shouldBe`
          SSEEvent Nothing Nothing "x" Nothing

      it "treats bare colon as comment" $
        parseEvent ":\ndata: x" `shouldBe`
          SSEEvent Nothing Nothing "x" Nothing

    describe "unknown fields" $ do
      it "ignores unknown field names per spec" $
        parseEvent "custom: value\ndata: x" `shouldBe`
          SSEEvent Nothing Nothing "x" Nothing

    describe "field without colon" $ do
      it "treats field-name-only line as empty-value field" $ do
        -- Per spec, a line with no colon is a field name with an empty value
        -- "data" with empty value appends "" to eventData
        sseEventData (parseEvent "data") `shouldBe` ""

    describe "line endings" $ do
      it "handles \\r\\n line endings within an event" $
        parseEvent "data: hello\r\ndata: world" `shouldBe`
          SSEEvent Nothing Nothing "hello\nworld" Nothing

      it "handles \\r line endings within an event" $
        parseEvent "data: hello\rdata: world" `shouldBe`
          SSEEvent Nothing Nothing "hello\nworld" Nothing

      it "handles mixed line endings within an event" $
        parseEvent "data: a\r\ndata: b\ndata: c\rdata: d" `shouldBe`
          SSEEvent Nothing Nothing "a\nb\nc\nd" Nothing

    describe "real-world examples" $ do
      it "parses a typical OpenAI streaming data chunk" $ do
        let ev = parseEvent "data: {\"choices\":[{\"delta\":{\"content\":\"Hi\"}}]}"
        sseEventData ev `shouldBe` "{\"choices\":[{\"delta\":{\"content\":\"Hi\"}}]}"

      it "parses the [DONE] sentinel" $
        parseEvent "data: [DONE]" `shouldBe`
          SSEEvent Nothing Nothing "[DONE]" Nothing

      it "parses an event with all four field types" $
        parseEvent "id: 1\nevent: update\nretry: 5000\ndata: payload" `shouldBe`
          SSEEvent (Just "1") (Just "update") "payload" (Just 5000)

  -- -------------------------------------------------------------------------
  describe "splitOnEventTerminator" $ do
    it "splits on \\n\\n" $ do
      let (sections, rest) = splitOnEventTerminator "a\n\nb"
      sections `shouldBe` ["a"]
      rest `shouldBe` "b"

    it "splits on \\r\\n\\r\\n" $ do
      let (sections, rest) = splitOnEventTerminator "a\r\n\r\nb"
      sections `shouldBe` ["a"]
      rest `shouldBe` "b"

    it "splits on \\r\\r" $ do
      let (sections, rest) = splitOnEventTerminator "a\r\rb"
      sections `shouldBe` ["a"]
      rest `shouldBe` "b"

    it "returns remainder when no terminator found" $ do
      let (sections, rest) = splitOnEventTerminator "no terminator here"
      sections `shouldBe` []
      rest `shouldBe` "no terminator here"

    it "returns empty remainder when input ends with terminator" $ do
      let (sections, rest) = splitOnEventTerminator "event\n\n"
      sections `shouldBe` ["event"]
      rest `shouldBe` ""

    it "handles multiple consecutive events" $ do
      let (sections, rest) = splitOnEventTerminator "a\n\nb\n\nc"
      sections `shouldBe` ["a", "b"]
      rest `shouldBe` "c"

  -- -------------------------------------------------------------------------
  describe "parseSSEChunks" $ do
    it "returns no events and empty remainder for empty input" $
      parseSSEChunks "" `shouldBe` SSEParseResult [] ""

    it "returns remainder for incomplete event" $
      parseSSEChunks "data: hello" `shouldBe`
        SSEParseResult [] "data: hello"

    it "returns event for terminated input" $
      parseSSEChunks "data: hello\n\n" `shouldBe`
        SSEParseResult [SSEEvent Nothing Nothing "hello" Nothing] ""

    it "returns multiple events" $ do
      let SSEParseResult events rest = parseSSEChunks "data: a\n\ndata: b\n\n"
      map sseEventData events `shouldBe` ["a", "b"]
      rest `shouldBe` ""

    it "correctly splits remainder from complete events" $ do
      let SSEParseResult events rest = parseSSEChunks "data: complete\n\ndata: incomplete"
      case events of
        [ev] -> do
          sseEventData ev `shouldBe` "complete"
          rest `shouldBe` "data: incomplete"
        _ -> expectationFailure ("expected 1 event, got " ++ show (length events))

    it "recovers a split event when remainder is prepended to next chunk" $ do
      let SSEParseResult _ rem1 = parseSSEChunks "data: hel"
          SSEParseResult events _ = parseSSEChunks (rem1 <> "lo\n\n")
      map sseEventData events `shouldBe` ["hello"]

    it "handles byte-by-byte delivery correctly" $ do
      let body = "data: first\n\ndata: second\n\n"
          go buf [] = parsedEvents (parseSSEChunks buf)
          go buf (b:bs) =
              let SSEParseResult evs buf' = parseSSEChunks (buf <> BS.singleton b)
              in evs ++ go buf' bs
          events = go BS.empty (BS.unpack body)
      map sseEventData events `shouldBe` ["first", "second"]

    it "handles chunked delivery across event boundaries" $ do
      let chunks = ["data", ": fi", "rst\n", "\nda", "ta: sec", "ond\n\n"]
          go buf []     = parsedEvents (parseSSEChunks buf)
          go buf (c:cs) =
              let SSEParseResult evs buf' = parseSSEChunks (buf <> c)
              in evs ++ go buf' cs
          events = go BS.empty chunks
      map sseEventData events `shouldBe` ["first", "second"]

    it "skips empty sections caused by consecutive blank lines" $ do
      let SSEParseResult events _ = parseSSEChunks "data: a\n\n\n\ndata: b\n\n"
      map sseEventData events `shouldBe` ["a", "b"]

    it "handles a comment-only event section without emitting an event" $ do
      -- A section with only comments still gets parsed but produces an empty-data event
      -- The section IS emitted (per spec); consumers should handle empty-data events
      let SSEParseResult events _ = parseSSEChunks ": comment only\n\ndata: real\n\n"
      -- The comment-only section produces an event with empty data
      length events `shouldBe` 2
      sseEventData (events !! 1) `shouldBe` "real"

  -- -------------------------------------------------------------------------
  describe "parseSSEComplete" $ do
    it "returns all events from a complete body" $ do
      let body = BS.concat
            [ "data: first\n\n"
            , "event: ping\ndata: {}\n\n"
            , "id: 99\ndata: last\n\n"
            ]
          events = parseSSEComplete body
      length events `shouldBe` 3
      sseEventData (events !! 0) `shouldBe` "first"
      sseEventType (events !! 1) `shouldBe` Just "ping"
      sseEventId   (events !! 2) `shouldBe` Just "99"

    it "returns empty list for empty body" $
      parseSSEComplete "" `shouldBe` []

    it "returns empty list when body has no terminated events" $
      parseSSEComplete "data: incomplete" `shouldBe` []

    it "handles a real OpenAI-style SSE stream" $ do
      let body = BS.concat
            [ "data: {\"choices\":[{\"delta\":{\"content\":\"Hello\"}}]}\n\n"
            , "data: {\"choices\":[{\"delta\":{\"content\":\" world\"}}]}\n\n"
            , "data: [DONE]\n\n"
            ]
          events = parseSSEComplete body
      length events `shouldBe` 3
      sseEventData (last events) `shouldBe` "[DONE]"

  -- -------------------------------------------------------------------------
  describe "properties" $ do
    prop "single-shot and byte-by-byte incremental parsing produce identical events" $
      \(n :: Int) ->
        let count = (abs n `mod` 8) + 1
            body  = BS.concat
              [ "data: item" <> BC.pack (show i) <> "\n\n"
              | i <- [1 .. count] ]
            oneShot = map sseEventData (parseSSEComplete body)
            byByte  = go BS.empty (BS.unpack body)
              where
                go buf []     = map sseEventData (parsedEvents (parseSSEChunks buf))
                go buf (b:bs) =
                    let SSEParseResult evs buf' = parseSSEChunks (buf <> BS.singleton b)
                    in map sseEventData evs ++ go buf' bs
        in counterexample ("body: " ++ show body) (oneShot == byByte)

    prop "splitting into two arbitrary chunks preserves all events" $
      \(splitAt_ :: Int) ->
        let body    = "data: alpha\n\ndata: beta\n\ndata: gamma\n\n"
            idx     = abs splitAt_ `mod` (BS.length body + 1)
            chunk1  = BS.take idx body
            chunk2  = BS.drop idx body
            onShot  = map sseEventData (parseSSEComplete body)
            twoPass = go BS.empty [chunk1, chunk2]
              where
                go buf []     = map sseEventData (parsedEvents (parseSSEChunks buf))
                go buf (c:cs) =
                    let SSEParseResult evs buf' = parseSSEChunks (buf <> c)
                    in map sseEventData evs ++ go buf' cs
        in counterexample ("split at " ++ show idx) (onShot == twoPass)
