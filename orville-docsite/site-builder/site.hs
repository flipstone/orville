--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Monad as Monad
import qualified Data.ByteString.Lazy as BSL
import Data.Default (def)
import qualified Data.Foldable as Fold
import qualified Data.List as List
import qualified Data.Set as Set
import Data.Ord (comparing)
import Data.Monoid (mappend)
import Hakyll
import Text.Pandoc.Highlighting (Style, breezeDark, styleToCss)
import Text.Pandoc.Options (ReaderOptions(..), WriterOptions(..))
import qualified Text.Read as Read
import qualified Skylighting
import qualified System.FilePath as FilePath

import qualified Data.Attoparsec.Text as AttoText
import qualified Data.Char as Char
import qualified Data.Map.Strict as Map
import qualified Data.NonEmptyText as NET
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.DList as DList


--------------------------------------------------------------------------------

config :: Configuration
config =
  def
    { previewHost = "0.0.0.0"
    }

pandocCodeStyle :: Style
pandocCodeStyle =
  breezeDark

renderPandocInStyle :: Item String -> Compiler (Item String)
renderPandocInStyle =
  renderPandocWith
    defaultHakyllReaderOptions
    (defaultHakyllWriterOptions
      { writerHighlightStyle = Just pandocCodeStyle
      })

data Section =
  Section
    { sectionName :: String
    , sectionPattern :: Pattern
    }

sections :: [Section]
sections =
  [ Section "tutorials" "tutorials/**"
  , Section "how-tos" "how-tos/**"
  , Section "explanations" "explanations/**"
  ]

sectionFiles :: Pattern
sectionFiles =
  foldr (.||.) (complement mempty) (map sectionPattern sections)

main :: IO ()
main = do
  hakyllWith config $ do
    snippetCache <- preprocess newSnippetCache
    let
      navContext = mkNavContext snippetCache

    match "images/*" $ do
      route   idRoute
      compile copyFileCompiler

    match "css/*" $ do
      route   idRoute
      compile compressCssCompiler

    create ["css/syntax.css"] $ do
      route idRoute
      compile (makeItem $ styleToCss pandocCodeStyle)

    match "contact.md" $ do
      route   $ setExtension "html"
      compile $ pandocCompiler
          >>= applyDefaultLayout navContext defaultContext
          >>= relativizeUrls

    match sectionFiles $ do
      route (setExtension "html")
      compile $
        getResourceBody
          >>= applyAsTemplate (pageCtx snippetCache)
          >>= renderPandocInStyle
          >>= loadAndApplyTemplate "templates/post.html" (pageCtx snippetCache)
          >>= saveSnapshot navLinksSnapshot
          >>= applyDefaultLayout navContext (pageCtx snippetCache)
          >>= relativizeUrls

    match "index.md" $ do
      route   $ setExtension "html"
      compile $ do
        getResourceBody
          >>= applyAsTemplate (mconcat [navContext, pageCtx snippetCache])
          >>= renderPandocInStyle
          >>= applyDefaultLayout navContext defaultContext
          >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

    create ["check-all-snippets-used"] $ do
      -- This rule doesn't have a route, which causes no actual file to get
      -- create for the site, which is what we want.
      compile $ do
        -- Depend on index since it gets compiled last. This way the other
        -- files will populate the snippet cache before we check if all
        -- snippets are used.
        _index <- load "index.md" :: Compiler (Item String)
        recompilingUnsafeCompiler $ failIfAnySnippetsUnused snippetCache
        makeItem ("Never seen" :: String)

--------------------------------------------------------------------------------
applyDefaultLayout ::
  Context a ->
  Context a ->
  Item a ->
  Compiler (Item String)
applyDefaultLayout navContext itemContext item = do
  loadAndApplyTemplate
    "templates/default.html"
    (navContext <> itemContext)
    item

{- | Nav links are generated from a named snapshot of the content items to
  avoid the dependency cycle introduced by an item needing a nav link to
  itself.
-}
navLinksSnapshot :: String
navLinksSnapshot =
  "navLinks"

mkNavContext :: SnippetCache -> Context a
mkNavContext snippetCache = do
  foldMap (sectionContext snippetCache) sections

sectionContext :: SnippetCache -> Section -> Context a
sectionContext snippetCache section =
  listField (sectionName section) (pageCtx snippetCache) $
    inNavOrder =<<
      loadAllSnapshots
        (sectionPattern section)
        navLinksSnapshot


inNavOrder :: (MonadMetadata m, MonadFail m) => [Item a] -> m [Item a]
inNavOrder =
  let
    pairWithNavOrder item = do
      navOrder <- getNavOrder (itemIdentifier item)
      pure (item, navOrder)
  in
    \items ->
      fmap
        (map fst . List.sortBy (comparing snd))
        (traverse pairWithNavOrder items)


getNavOrder :: (MonadMetadata m, MonadFail m) => Identifier -> m Int
getNavOrder identifier = do
  metadata <- getMetadata identifier
  case lookupString "navOrder" metadata of
    Nothing -> fail $ "navOrder not specified for " <> show identifier
    Just navOrder -> either fail pure (Read.readEither navOrder)


pageCtx :: SnippetCache -> Context String
pageCtx snippetCache =
  mconcat
    [ dateField "date" "%B %e, %Y"
    , sampleField snippetCache
    , defaultContext
    ]

sampleField :: SnippetCache -> Context a
sampleField snippetCache =
  functionField "sample" $ \args item -> do
    case args of
      (path:rest) -> do
        options <- failOnLeft (parseOptions rest)
        sample <-
          failOnLeft =<<
            recompilingUnsafeCompiler
              (readSample
                snippetCache
                ("../samples/" <> path)
                options)

        mkMarkdownCodeBlock path options sample

failOnLeft :: Either String a -> Compiler a
failOnLeft = either fail pure

readSample :: SnippetCache -> FilePath -> SampleOptions -> IO (Either String T.Text)
readSample snippetCache fullPath options =
  case sampleSnippets options of
    [] ->
      fmap Right . TIO.readFile $ fullPath
    snippetNames ->
      fmap (fmap T.unlines . sequence)
        . traverse (lookupSnippet snippetCache fullPath)
        $ snippetNames


data SampleOptions =
  SampleOptions
    { sampleSnippets :: [NET.NonEmptyText]
    , sampleFilename :: Maybe FilePath
    }

emptySampleOptions :: SampleOptions
emptySampleOptions =
  SampleOptions
    { sampleSnippets = []
    , sampleFilename = Nothing
    }

parseOptions :: [String] -> Either String SampleOptions
parseOptions =
  Monad.foldM parseOption emptySampleOptions

parseOption :: SampleOptions -> String -> Either String SampleOptions
parseOption options newOption =
  case T.splitOn "=" (T.pack newOption) of
    [snippetName] -> do
      neSnippetName <- mustBeNonEmpty "snippetName" snippetName
      pure (options { sampleSnippets = sampleSnippets options <> [neSnippetName] })

    ["filename", filename] -> do
      pure (options { sampleFilename = Just (T.unpack filename) })

    [otherOption, _value] -> Left ("Unrecognized option: " <> T.unpack otherOption)
    _ -> Left ("Invalid option format: " <> newOption)

mustBeNonEmpty :: String -> T.Text -> Either String NET.NonEmptyText
mustBeNonEmpty name value =
  case NET.fromText value of
    Nothing -> Left (name <> " cannot be empty")
    Just nonEmptyValue -> Right nonEmptyValue

mkMarkdownCodeBlock :: FilePath -> SampleOptions -> T.Text -> Compiler String
mkMarkdownCodeBlock path options body = do
  lang <- either fail pure (guessLang path)

  let
    labelAnnotation =
      case lang of
        "sh" -> "shell"
        "txt" -> "plaintext"
        otherLang -> otherLang

    filename =
      case sampleFilename options of
        Just filename -> filename
        Nothing ->
          -- drop the leading directory since that is generally a name for
          -- the example itself
          FilePath.joinPath
            . drop 1
            . FilePath.splitPath
            $ path

    label =
      case filename of
        "" -> labelAnnotation
        _ -> filename <> " : " <> labelAnnotation

  pure $
    unlines
      [ "<div class=\"codeblock-label\">" <> label <> "</div>"
      , "```" <> lang
      , T.unpack body
      , "```"
      ]

guessLang :: FilePath -> Either String String
guessLang path =
  case FilePath.takeExtensions path of
    ".hs" -> Right "haskell"
    ".sh" -> Right "sh"
    ".patch" -> Right "diff"
    ".txt" -> Right "txt"
    ext -> Left $ "Unable to determine language for " <> path

type SnippetMap =
  Map.Map NET.NonEmptyText T.Text

type SnippetCache =
  MVar.MVar (Map.Map FilePath SnippetCacheEntry)

newSnippetCache :: IO SnippetCache
newSnippetCache =
  MVar.newMVar Map.empty

failIfAnySnippetsUnused :: SnippetCache -> IO ()
failIfAnySnippetsUnused snippetCache =
  MVar.withMVar snippetCache (Fold.traverse_ failIfAnySnippetsUnusedForEntry)

failIfAnySnippetsUnusedForEntry :: MonadFail m => SnippetCacheEntry -> m ()
failIfAnySnippetsUnusedForEntry cacheEntry =
  let
    allKeys =
      Map.keysSet (snippetCacheEntrySnippets cacheEntry)

    unusedKeys =
      Set.difference allKeys (snippetCacheEntryUsedSnippets cacheEntry)
  in
    if Set.null unusedKeys
    then pure ()
    else
      fail
        $ "Unused snippets found in "
        <> snippetCacheEntryPath cacheEntry
        <> ": "
        <> show unusedKeys


lookupSnippet ::
  SnippetCache ->
  FilePath ->
  NET.NonEmptyText ->
  IO (Either String T.Text)
lookupSnippet cacheVar path snippetName =
  if snippetName == hiddenSnippetName
  then
    pure . Left $
      "Snippets with the name "
      <> show hiddenSnippetName
      <> " cannot be referenced in templates."
  else
    MVar.modifyMVar cacheVar $ \cacheMap -> do
      errOrMapAndEntry <-
        case Map.lookup path cacheMap of
          Just cacheEntry ->
            pure . Right $ (cacheMap, cacheEntry)
          Nothing -> do
            errOrSnippets <- loadSnippetsFile path
            case errOrSnippets of
              Left err -> pure . Left $ err
              Right snippets ->
                let
                  cacheEntry =
                    SnippetCacheEntry
                      { snippetCacheEntryPath = path
                      , snippetCacheEntrySnippets = snippets
                      , snippetCacheEntryUsedSnippets = Set.empty
                      }
                in
                  pure . Right $
                    ( Map.insert path cacheEntry cacheMap
                    , cacheEntry
                    )

      pure $
        case errOrMapAndEntry of
          Left err -> (cacheMap, Left err)
          Right (newMap, cacheEntry) ->
            let
              snippet =
                case Map.lookup snippetName (snippetCacheEntrySnippets cacheEntry) of
                  Nothing -> Left $ "Snippet " <> show snippetName <> " not found in " <> path
                  Just snippet -> Right snippet

              newMapWithSnippetMarkedUsed =
                Map.adjust
                  (markSnippetUsed snippetName)
                  path
                  newMap
            in
              ( newMapWithSnippetMarkedUsed
              , snippet
              )


data SnippetCacheEntry =
  SnippetCacheEntry
    { snippetCacheEntryPath :: FilePath
    , snippetCacheEntrySnippets :: SnippetMap
    , snippetCacheEntryUsedSnippets :: Set.Set NET.NonEmptyText
    }

loadSnippetsFile :: FilePath -> IO (Either String SnippetMap)
loadSnippetsFile =
  fmap (AttoText.parseOnly snippets) . TIO.readFile

markSnippetUsed :: NET.NonEmptyText -> SnippetCacheEntry -> SnippetCacheEntry
markSnippetUsed snippetName cacheEntry =
  cacheEntry
    { snippetCacheEntryUsedSnippets =
        Set.insert
          snippetName
          (snippetCacheEntryUsedSnippets cacheEntry)
    }

hiddenSnippetName :: NET.NonEmptyText
hiddenSnippetName =
  NET.new 'h' "idden"

snippets :: AttoText.Parser SnippetMap
snippets =
  let
    insertChunks snippetMarker chunks snippets =
      let
        name =
          snippetMarkerName snippetMarker
      in
        if name == hiddenSnippetName
        then pure snippets
        else
          if Map.member name snippets
          then
            fail $
              "Snippet names must be unique within their file. "
              <> show name
              <> " appears more than once."
          else
            pure $
              Map.insert
                name
                (T.concat . DList.toList $ chunks)
                snippets

    go snippets chunks snippetMarker = do
      let
        snippetLeadingChar =
          NET.head
            . snippetMarkerPrefix
            $ snippetMarker

      nextChunk <- AttoText.takeTill (== snippetLeadingChar)

      let
        newChunks =
          DList.snoc chunks nextChunk

      atEnd <- AttoText.atEnd

      if atEnd
      then insertChunks snippetMarker newChunks snippets
      else do
        nextNameOrBrace <-
          AttoText.eitherP
            (snippetMarkerWithMatchingPrefix snippetMarker)
            (AttoText.string (T.singleton snippetLeadingChar))

        case nextNameOrBrace of
          Left nextName -> do
            newSnippets <- insertChunks snippetMarker newChunks snippets
            go
              newSnippets
              DList.empty
              nextName

          Right followingBrace -> do
            go
              snippets
              (DList.snoc newChunks followingBrace)
              snippetMarker
       in
    go Map.empty DList.empty =<< snippetMarkerWithAnyPrefix


{- |

Parses a magic haskell comment marking the beginning of a snippet. The
snippet name may consist of any non-space characters. E.G.

> {- SNIPPET: header -}

or

> -- SNIPPET: header

or

> # SNIPPET: header

-}
snippetMarkerWithAnyPrefix :: AttoText.Parser SnippetMarker
snippetMarkerWithAnyPrefix = do
  SnippetMarker
    <$> nonEmptyWhile (not . Char.isSpace)
    <*> snippetMarkerBody

snippetMarkerWithMatchingPrefix :: SnippetMarker -> AttoText.Parser SnippetMarker
snippetMarkerWithMatchingPrefix marker = do
  SnippetMarker
    <$> nonEmptyString (snippetMarkerPrefix marker)
    <*> snippetMarkerBody

snippetMarkerBody :: AttoText.Parser NET.NonEmptyText
snippetMarkerBody = do
  AttoText.skipWhile AttoText.isHorizontalSpace
  AttoText.string "SNIPPET:"
  AttoText.skipWhile AttoText.isHorizontalSpace
  name <- nonEmptyWhile (not . Char.isSpace)
  AttoText.skipWhile AttoText.isHorizontalSpace
  AttoText.skipWhile (not . isEOLChar)
  AttoText.endOfLine
  pure name

nonEmptyWhile :: (Char -> Bool) -> AttoText.Parser NET.NonEmptyText
nonEmptyWhile predicate = do
  text <- AttoText.takeWhile1 predicate
  case NET.fromText text of
    Nothing -> fail "nonEmptyWhile: takeWhile1 returned empty text"
    Just nonEmptyText -> pure nonEmptyText

nonEmptyString :: NET.NonEmptyText -> AttoText.Parser NET.NonEmptyText
nonEmptyString targetText =
  AttoText.string (NET.toText targetText)
    *> pure targetText

data SnippetMarker =
  SnippetMarker
    { snippetMarkerPrefix :: NET.NonEmptyText
    , snippetMarkerName :: NET.NonEmptyText
    }

isEOLChar :: Char -> Bool
isEOLChar c =
  c == '\n' || c == '\r'
