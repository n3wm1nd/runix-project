{-# LANGUAGE OverloadedStrings #-}

-- | Server-Sent Events (SSE) decoding
--
-- Implements the SSE protocol as defined in the WHATWG specification:
-- https://html.spec.whatwg.org/multipage/server-sent-events.html
--
-- Events are separated by blank lines. Line terminators may be @\\n@, @\\r@,
-- or @\\r\\n@. Each event consists of named fields:
--
-- * @event@: the event type (absent means type is @"message"@)
-- * @data@: the event data; multiple occurrences are concatenated with newlines
-- * @id@: the event ID
-- * @retry@: reconnection time in milliseconds (must be all ASCII digits)
-- * Lines starting with @:@ are comments and are ignored
--
-- = Streaming usage
--
-- 'parseSSEChunks' is designed for incremental / streaming use. Feed it each
-- new chunk together with the unparsed remainder from the previous call:
--
-- @
-- go :: ByteString -> IO ()
-- go buffer = do
--   chunk <- receiveChunk
--   let SSEParseResult events rest = parseSSEChunks (buffer <> chunk)
--   mapM_ handleEvent events
--   go rest
-- @
--
-- = One-shot usage
--
-- 'parseSSEComplete' consumes a complete SSE body and returns all events.
module Network.SSE
  ( -- * Types
    SSEEvent(..)
  , SSEParseResult(..)

    -- * Decoding
  , parseSSEChunks
  , parseSSEComplete
  , parseEvent
  , splitOnEventTerminator
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Word (Word8)

-- | A parsed Server-Sent Event.
--
-- Per the spec, @sseEventType@ is absent when no @event:@ field appeared (the
-- consumer should treat that as @\"message\"@).  'sseEventData' is the raw
-- byte concatenation of all @data:@ fields separated by @\\n@ bytes.
data SSEEvent = SSEEvent
  { sseEventId   :: Maybe Text    -- ^ Value of the @id:@ field, if present
  , sseEventType :: Maybe Text    -- ^ Value of the @event:@ field, if present
  , sseEventData :: ByteString    -- ^ Concatenated @data:@ fields
  , sseRetry     :: Maybe Int     -- ^ Value of the @retry:@ field in ms, if valid
  } deriving (Show, Eq)

-- | Result of a single 'parseSSEChunks' call.
data SSEParseResult = SSEParseResult
  { parsedEvents :: [SSEEvent]  -- ^ Complete events found in this call
  , remainder    :: ByteString  -- ^ Bytes that did not yet form a complete event
  } deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Parse one or more raw chunks, returning all complete events found so far
-- and any trailing bytes that have not yet been terminated by a blank line.
--
-- Suitable for incremental / streaming consumption.  Prepend 'remainder' to
-- the next chunk before calling again.
parseSSEChunks :: ByteString -> SSEParseResult
parseSSEChunks input =
    let (completeSections, rest) = splitOnEventTerminator input
        events = map parseEvent (filter (not . BS.null) completeSections)
    in SSEParseResult events rest

-- | Parse a complete SSE body (e.g. a full HTTP response) and return every
-- event it contains, in order.
parseSSEComplete :: ByteString -> [SSEEvent]
parseSSEComplete = parsedEvents . parseSSEChunks

-- | Split a byte string into sections separated by blank lines, returning
-- the complete sections and any trailing bytes after the last terminator.
--
-- Per the SSE spec the following byte sequences all act as event terminators
-- (blank lines):
--
-- * @\\n\\n@
-- * @\\r\\n\\r\\n@
-- * @\\r\\r@
-- * @\\r\\n\\n@ (mixed — accepted leniently)
splitOnEventTerminator :: ByteString -> ([ByteString], ByteString)
splitOnEventTerminator input = go [] BS.empty input
  where
    go acc current bs =
        case BS.uncons bs of
            Nothing -> (reverse acc, current)
            Just (c, rest)
                | c == cr ->
                    case BS.uncons rest of
                        Just (c2, rest2)
                            | c2 == lf ->
                                -- Saw \r\n — peek for second terminator
                                case BS.uncons rest2 of
                                    Just (c3, rest3)
                                        | c3 == cr ->
                                            case BS.uncons rest3 of
                                                Just (c4, rest4) | c4 == lf ->
                                                    go (current : acc) BS.empty rest4  -- \r\n\r\n
                                                _ -> go acc (append3 current cr lf cr) rest3
                                        | c3 == lf ->
                                            go (current : acc) BS.empty rest3  -- \r\n\n
                                        | otherwise ->
                                            go acc (BS.snoc (BS.snoc current cr) lf) rest2
                                    Nothing ->
                                        go acc (BS.snoc (BS.snoc current cr) lf) rest2
                            | c2 == cr ->
                                go (current : acc) BS.empty rest2  -- \r\r
                            | otherwise ->
                                go acc (BS.snoc current cr) rest
                        Nothing -> go acc (BS.snoc current cr) rest
                | c == lf ->
                    case BS.uncons rest of
                        Just (c2, rest2) | c2 == lf ->
                            go (current : acc) BS.empty rest2  -- \n\n
                        _ -> go acc (BS.snoc current lf) rest
                | otherwise ->
                    go acc (BS.snoc current c) rest

    append3 :: ByteString -> Word8 -> Word8 -> Word8 -> ByteString
    append3 bs a b c_ = BS.snoc (BS.snoc (BS.snoc bs a) b) c_

-- | Parse a single, complete event section (the bytes between two blank
-- lines) into an 'SSEEvent'.
parseEvent :: ByteString -> SSEEvent
parseEvent eventBytes =
    foldl' processLine emptyEvent (splitLines eventBytes)
  where
    emptyEvent :: SSEEvent
    emptyEvent = SSEEvent Nothing Nothing BS.empty Nothing

    -- Split on \r\n, \n, or \r
    splitLines :: ByteString -> [ByteString]
    splitLines bs
        | BS.null bs = []
        | otherwise =
            case BS.findIndex (\c -> c == lf || c == cr) bs of
                Nothing -> [bs]
                Just idx ->
                    let (line, rest) = BS.splitAt idx bs
                        rest' = dropLineEnding rest
                    in line : splitLines rest'

    dropLineEnding :: ByteString -> ByteString
    dropLineEnding bs = case BS.uncons bs of
        Nothing -> BS.empty
        Just (c, rest)
            | c == cr -> case BS.uncons rest of
                Just (c2, rest2) | c2 == lf -> rest2  -- \r\n
                _ -> rest                              -- \r
            | c == lf -> rest                          -- \n
            | otherwise -> bs

    processLine :: SSEEvent -> ByteString -> SSEEvent
    processLine event line
        | BS.null line           = event  -- blank line inside section — ignored
        | BS.head line == colon  = event  -- comment
        | otherwise =
            let (field, afterField) = BS.break (== colon) line
                value = stripOptionalSpace (BS.drop 1 afterField)
            in applyField event (TE.decodeUtf8 field) value

    stripOptionalSpace :: ByteString -> ByteString
    stripOptionalSpace bs = case BS.uncons bs of
        Just (0x20, rest) -> rest
        _ -> bs

    applyField :: SSEEvent -> Text -> ByteString -> SSEEvent
    applyField event "event" value =
        event { sseEventType = Just (TE.decodeUtf8 value) }
    applyField event "data" value =
        let current = sseEventData event
            new     = if BS.null current then value else current <> "\n" <> value
        in event { sseEventData = new }
    applyField event "id" value =
        event { sseEventId = Just (TE.decodeUtf8 value) }
    applyField event "retry" value =
        -- Per spec, retry value must be ASCII digits only
        case reads (map (toEnum . fromIntegral) (BS.unpack value)) of
            [(n, "")] -> event { sseRetry = Just n }
            _         -> event
    applyField event _ _ = event

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

cr, lf, colon :: Word8
cr    = 13   -- '\r'
lf    = 10   -- '\n'
colon = 58   -- ':'
