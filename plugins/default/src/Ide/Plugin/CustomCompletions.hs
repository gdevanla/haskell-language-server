{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}



module Ide.Plugin.CustomCompletions
  (
    descriptor
  ) where

import Control.DeepSeq ( NFData )
import Control.Monad.Trans.Maybe
import Data.Aeson
import Data.Binary
import Data.Functor
import qualified Data.HashMap.Strict as Map
import Data.Hashable
import qualified Data.Text as T
import Data.Typeable
import Development.IDE as D
import Development.IDE.GHC.Compat (ParsedModule(ParsedModule), Type)
import Development.IDE.Core.Rules (useE)
import Development.IDE.Core.Shake (getDiagnostics, getHiddenDiagnostics, getIdeOptionsIO)
import GHC.Generics
import Ide.Plugin
import Ide.Types
import Language.Haskell.LSP.Types
import Text.Regex.TDFA.Text()

import Control.Monad.IO.Class

import qualified Language.Haskell.LSP.Core as LSP
import Development.IDE.Core.PositionMapping
import Data.Maybe
import qualified Language.Haskell.LSP.VFS as VFS
import Language.Haskell.LSP.Types.Capabilities

import Development.IDE.Plugin.Completions hiding (getCompletionsLSP)
import Development.IDE.Plugin.Completions.Types
import Development.IDE.Types.Options
import Development.IDE.Spans.LocalBindings
import qualified Text.Fuzzy as Fuzzy
import Outputable (Outputable)

--import Development.IDE.GHC.Compat
import Name
import GHC.Unicode
import Development.IDE.Spans.Common (emptySpanDoc)

-- ---------------------------------------------------------------------

descriptor :: PluginId -> PluginDescriptor
descriptor plId = (defaultPluginDescriptor plId)
  { pluginRules = exampleRules
  , pluginCommands = [PluginCommand "codelens.todo" "example adding" addTodoCmd]
  , pluginCodeActionProvider = Just codeAction
  , pluginCodeLensProvider   = Just codeLens
  , pluginHoverProvider      = Just hover
  , pluginSymbolsProvider    = Just symbols
  , pluginCompletionProvider = Just getCompletionsLSP
  }

-- ---------------------------------------------------------------------

hover :: IdeState -> TextDocumentPositionParams -> IO (Either ResponseError (Maybe Hover))
hover = request "Hover" blah (Right Nothing) foundHover

blah :: NormalizedFilePath -> Position -> Action (Maybe (Maybe Range, [T.Text]))
blah _ (Position line col)
  = return $ Just (Just (Range (Position line col) (Position (line+1) 0)), ["example hover 1\n"])

-- ---------------------------------------------------------------------
-- Generating Diagnostics via rules
-- ---------------------------------------------------------------------

data Example = Example
    deriving (Eq, Show, Typeable, Generic)
instance Hashable Example
instance NFData   Example
instance Binary   Example

type instance RuleResult Example = ()

exampleRules :: Rules ()
exampleRules = do
  define $ \Example file -> do
    _pm <- getParsedModule file
    let diag = mkDiag file "example" DsError (Range (Position 0 0) (Position 1 0)) "example diagnostic, hello world"
    return ([diag], Just ())

  action $ do
    files <- getFilesOfInterest
    void $ uses Example $ Map.keys files

mkDiag :: NormalizedFilePath
       -> DiagnosticSource
       -> DiagnosticSeverity
       -> Range
       -> T.Text
       -> FileDiagnostic
mkDiag file diagSource sev loc msg = (file, D.ShowDiag,)
    Diagnostic
    { _range    = loc
    , _severity = Just sev
    , _source   = Just diagSource
    , _message  = msg
    , _code     = Nothing
    , _tags     = Nothing
    , _relatedInformation = Nothing
    }

-- ---------------------------------------------------------------------
-- code actions
-- ---------------------------------------------------------------------

-- | Generate code actions.
codeAction :: CodeActionProvider
codeAction _lf state _pid (TextDocumentIdentifier uri) _range CodeActionContext{_diagnostics=List _xs} = do
    let Just nfp = uriToNormalizedFilePath $ toNormalizedUri uri
    Just (ParsedModule{},_) <- runIdeAction "example" (shakeExtras state) $ useWithStaleFast GetParsedModule nfp
    let
      title = "Add TODO Item 1"
      tedit = [TextEdit (Range (Position 2 0) (Position 2 0))
               "-- TODO1 added by Example Plugin directly\n"]
      edit  = WorkspaceEdit (Just $ Map.singleton uri $ List tedit) Nothing
    pure $ Right $ List
        [ CACodeAction $ CodeAction title (Just CodeActionQuickFix) (Just $ List []) (Just edit) Nothing ]

-- ---------------------------------------------------------------------

codeLens :: CodeLensProvider
codeLens _lf ideState plId CodeLensParams{_textDocument=TextDocumentIdentifier uri} = do
    logInfo (ideLogger ideState) "Example.codeLens entered (ideLogger)" -- AZ
    case uriToFilePath' uri of
      Just (toNormalizedFilePath -> filePath) -> do
        _ <- runIdeAction "Example.codeLens" (shakeExtras ideState) $ runMaybeT $ useE TypeCheck filePath
        _diag <- getDiagnostics ideState
        _hDiag <- getHiddenDiagnostics ideState
        let
          title = "Add TODO Item via Code Lens"
          -- tedit = [TextEdit (Range (Position 3 0) (Position 3 0))
          --      "-- TODO added by Example Plugin via code lens action\n"]
          -- edit = WorkspaceEdit (Just $ Map.singleton uri $ List tedit) Nothing
          range = Range (Position 3 0) (Position 4 0)
        let cmdParams = AddTodoParams uri "do abc"
        cmd <- mkLspCommand plId "codelens.todo" title (Just [(toJSON cmdParams)])
        pure $ Right $ List [ CodeLens range (Just cmd) Nothing ]
      Nothing -> pure $ Right $ List []

-- ---------------------------------------------------------------------
-- | Parameters for the addTodo PluginCommand.
data AddTodoParams = AddTodoParams
  { file   :: Uri  -- ^ Uri of the file to add the pragma to
  , todoText :: T.Text
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

addTodoCmd :: CommandFunction AddTodoParams
addTodoCmd _lf _ide (AddTodoParams uri todoText) = do
  let
    pos = Position 3 0
    textEdits = List
      [TextEdit (Range pos pos)
                  ("-- TODO:" <> todoText <> "\n")
      ]
    res = WorkspaceEdit
      (Just $ Map.singleton uri textEdits)
      Nothing
  return (Right Null, Just (WorkspaceApplyEdit, ApplyWorkspaceEditParams res))

-- ---------------------------------------------------------------------

foundHover :: (Maybe Range, [T.Text]) -> Either ResponseError (Maybe Hover)
foundHover (mbRange, contents) =
  Right $ Just $ Hover (HoverContents $ MarkupContent MkMarkdown
                        $ T.intercalate sectionSeparator contents) mbRange


-- | Respond to and log a hover or go-to-definition request
request
  :: T.Text
  -> (NormalizedFilePath -> Position -> Action (Maybe a))
  -> Either ResponseError b
  -> (a -> Either ResponseError b)
  -> IdeState
  -> TextDocumentPositionParams
  -> IO (Either ResponseError b)
request label getResults notFound found ide (TextDocumentPositionParams (TextDocumentIdentifier uri) pos _) = do
    mbResult <- case uriToFilePath' uri of
        Just path -> logAndRunRequest label getResults ide pos path
        Nothing   -> pure Nothing
    pure $ maybe notFound found mbResult

logAndRunRequest :: T.Text -> (NormalizedFilePath -> Position -> Action b)
                  -> IdeState -> Position -> String -> IO b
logAndRunRequest label getResults ide pos path = do
  let filePath = toNormalizedFilePath path
  logInfo (ideLogger ide) $
    label <> " request at position " <> T.pack (showPosition pos) <>
    " in file: " <> T.pack path
  runAction "Example" ide $ getResults filePath pos

-- ---------------------------------------------------------------------

symbols :: SymbolsProvider
symbols _lf _ide (DocumentSymbolParams _doc _mt)
    = pure $ Right [r]
    where
        r = DocumentSymbol name detail kind deprecation range selR chList
        name = "Example_symbol_name"
        detail = Nothing
        kind = SkVariable
        deprecation = Nothing
        range = Range (Position 2 0) (Position 2 5)
        selR = range
        chList = Nothing

-- ---------------------------------------------------------------------

-- completion :: CompletionProvider
-- completion _lf _ide (CompletionParams _doc _pos _mctxt _mt)
--     = pure $ Right $ Completions $ List [r]
--     where
--         r = CompletionItem label kind tags detail documentation deprecated preselect
--                            sortText filterText insertText insertTextFormat
--                            textEdit additionalTextEdits commitCharacters
--                            command xd
--         label = "my_completions_show_up_here"
--         kind = Nothing
--         tags = List []
--         detail = Nothing
--         documentation = Nothing
--         deprecated = Nothing
--         preselect = Nothing
--         sortText = Nothing
--         filterText = Nothing
--         insertText = Just "our_completions_are_going_to_get_better"
--         insertTextFormat = Nothing
--         textEdit = Nothing
--         additionalTextEdits = Nothing
--         commitCharacters = Nothing
--         command = Nothing
--         xd = Nothing

sampleCompletionItem :: CompletionItem
sampleCompletionItem =
    CompletionItem label kind tags detail documentation deprecated preselect
                           sortText filterText insertText insertTextFormat
                           textEdit additionalTextEdits commitCharacters
                           command xd
    where
        label = "my_completions_show_up_here"
        kind = Nothing
        tags = List []
        detail = Nothing
        documentation = Nothing
        deprecated = Nothing
        preselect = Nothing
        sortText = Nothing
        filterText = Nothing
        insertText = Just "completion_from_get_completion v2"
        insertTextFormat = Nothing
        textEdit = Nothing
        additionalTextEdits = Nothing
        commitCharacters = Nothing
        command = Nothing
        xd = Nothing


getCompletionsLSP :: CompletionProvider
getCompletionsLSP lsp ide
    CompletionParams {_textDocument=TextDocumentIdentifier uri
                     ,_position=position
                     ,_context=completionContext} = do
    contents <- LSP.getVirtualFileFunc lsp $ toNormalizedUri uri
    logInfo (ideLogger ide) "Inside custom completions ---------------------"
    logInfo (ideLogger ide) $ T.pack $ show contents
    fmap Right $ case (contents, uriToFilePath' uri) of
        (Just cnts, Just path) -> do
            let npath = toNormalizedFilePath' path
            (ideOpts, compls) <- runIdeAction "Completion" (shakeExtras ide) $ do
                -- for now lets use the current Rules provided by GHCIde. We will re-implement them laster ourselves.
                opts <- liftIO $ getIdeOptionsIO $ shakeExtras ide
                compls <- useWithStaleFast ProduceCompletions npath
                pm <- useWithStaleFast GetParsedModule npath
                binds <- fromMaybe (mempty, zeroMapping) <$> useWithStaleFast GetBindings npath
                pure (opts, fmap (,pm,binds) compls)
            case compls of
                Just ((cci', _), parsedMod, bindMap) -> do
                    pfix <- VFS.getCompletionPrefix position cnts
                    case (pfix, completionContext) of
                        (Just (VFS.PosPrefixInfo _ "" _ _), Just CompletionContext { _triggerCharacter = Just "."}) ->
                            return $ Completions $ List [sampleCompletionItem]
                        (Just pfix', _) -> do
                            -- TODO pass the real capabilities here (or remove the logic for snippets)
                            let fakeClientCapabilities = ClientCapabilities Nothing Nothing Nothing Nothing
                            -- return $ Completions $ List [sampleCompletionItem]
                            Completions . List <$> getCompletions ideOpts ide cci' parsedMod bindMap pfix' fakeClientCapabilities (WithSnippets True)
                        _ -> return (Completions $ List [])
                _ -> return $ Completions $ List [sampleCompletionItem]
        _ -> return $ Completions $ List []
-- ---------------------------------------------------------------------

_mkDefaultCompl :: T.Text -> T.Text -> CompletionItem
_mkDefaultCompl label insertText =
  CompletionItem label (Just CiKeyword) (List []) Nothing
    Nothing Nothing Nothing Nothing Nothing (Just insertText) (Just Snippet)
    Nothing Nothing Nothing Nothing Nothing

mkModCompl :: T.Text -> CompletionItem
mkModCompl _label =
  CompletionItem
    "mkModCompl"
    (Just CiModule)
    (List [])
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing

mkImportCompl :: T.Text -> T.Text -> CompletionItem
mkImportCompl enteredQual label =
  CompletionItem
    m
    (Just CiModule)
    (List [])
    (Just label)
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
  where
    m = fromMaybe "" (T.stripPrefix enteredQual label)

--Returns the cached completions for the given module and position
getCompletions
    :: IdeOptions
    -> IdeState
    -> CachedCompletions
    -> Maybe (ParsedModule, PositionMapping)
    -> (Bindings, PositionMapping)
    -> VFS.PosPrefixInfo
    -> ClientCapabilities
    -> WithSnippets
    -> IO [CompletionItem]
getCompletions _ideOpts ideState CC{..} _maybe_parsed (localBindings, bmapping) prefixInfo _cp _ws = do
    let VFS.PosPrefixInfo { fullLine, prefixModule, prefixText } = prefixInfo
        enteredQual = if T.null prefixModule then "" else prefixModule <> "."
        fullPrefix  = enteredQual <> prefixText
        _fulline = fullLine
        -- provide module names when used with qualifier
        filtModNameCompls = map mkModCompl $
            mapMaybe (T.stripPrefix enteredQual) $
            Fuzzy.simpleFilter fullPrefix allModNamesAsNS


        filtListWith f list =
            [ f label
            | label <- Fuzzy.simpleFilter fullPrefix list,
              enteredQual `T.isPrefixOf` label
            ]
        filtImportCompls = filtListWith (mkImportCompl enteredQual) importableModules


        PositionMapping bDelta = bmapping
        oldPos = fromDelta bDelta $ VFS.cursorPos prefixInfo
        startLoc = lowerRange oldPos
        endLoc = upperRange oldPos
        scope = getFuzzyScope localBindings startLoc endLoc
        scope' = map snd scope
        localCompls = map (uncurry localBindsToCompItem) $ getFuzzyScope localBindings startLoc endLoc
        localBindsToCompItem :: Name -> Maybe Type -> CompItem
        localBindsToCompItem name typ = CI ctyp pn thisModName ty pn Nothing emptySpanDoc (not $ isValOcc occ)
            where
              occ = nameOccName name
              ctyp = occNameToComKind Nothing occ
              pn = ppr name
              ty = ppr <$> typ
              thisModName = case nameModule_maybe name of
                Nothing -> Left $ nameSrcSpan name
                Just m -> Right $ ppr m

    logInfo (ideLogger ideState) $ "***** Postion *******"
    logInfo (ideLogger ideState) $ T.pack $ show startLoc
    logInfo (ideLogger ideState) $ T.pack $ show endLoc
    logInfo (ideLogger ideState) $ ppr scope'
    logInfo (ideLogger ideState) $ T.pack $ show localCompls

    let result
            | "import " `T.isPrefixOf` fullLine
            = filtImportCompls
            | otherwise
            = filtModNameCompls
    return result


    -- pos = VFS.cursorPos prefixInfo

    -- filtCompls = map Fuzzy.original $ Fuzzy.filter prefixText ctxCompls "" "" label False
    --     where
    --       mcc = case maybe_parsed of
    --         Nothing -> Nothing
    --         Just (pm, pmapping) ->
    --           let PositionMapping pDelta = pmapping
    --               position' = fromDelta pDelta pos
    --               lpos = lowerRange position'
    --               hpos = upperRange position'
    --           in getCContext lpos pm <|> getCContext hpos pm

    --       -- completions specific to the current context
    --       ctxCompls' = case mcc of
    --                     Nothing -> compls
    --                     Just TypeContext -> filter isTypeCompl compls
    --                     Just ValueContext -> filter (not . isTypeCompl) compls
    --                     Just _ -> filter (not . isTypeCompl) compls
    --       -- Add whether the text to insert has backticks
    --       ctxCompls = map (\comp -> comp { isInfix = infixCompls }) ctxCompls'

    --       infixCompls :: Maybe Backtick
    --       infixCompls = isUsedAsInfix fullLine prefixModule prefixText pos

    --       PositionMapping bDelta = bmapping
    --       oldPos = fromDelta bDelta $ VFS.cursorPos prefixInfo
    --       startLoc = lowerRange oldPos
    --       endLoc = upperRange oldPos
    --       localCompls = map (uncurry localBindsToCompItem) $ getFuzzyScope localBindings startLoc endLoc
    --       localBindsToCompItem :: Name -> Maybe Type -> CompItem
    --       localBindsToCompItem name typ = CI ctyp pn thisModName ty pn Nothing emptySpanDoc (not $ isValOcc occ)
    --         where
    --           occ = nameOccName name
    --           ctyp = occNameToComKind Nothing occ
    --           pn = ppr name
    --           ty = ppr <$> typ
    --           thisModName = case nameModule_maybe name of
    --             Nothing -> Left $ nameSrcSpan name
    --             Just m -> Right $ ppr m

    --       compls = if T.null prefixModule
    --         then localCompls ++ unqualCompls
    --         else Map.findWithDefault [] prefixModule $ getQualCompls qualCompls

    -- return filtCompls

ppr :: Outputable a => a -> T.Text
ppr = T.pack . prettyPrint


occNameToComKind :: Maybe T.Text -> OccName -> CompletionItemKind
occNameToComKind ty oc
  | isVarOcc oc = case occNameString oc of
    i : _ | isUpper i -> CiConstructor
    _ -> CiFunction
  | isTcOcc oc = case ty of
    Just t
      | "Constraint" `T.isSuffixOf` t ->
        CiClass
    _ -> CiStruct
  | isDataOcc oc = CiConstructor
  | otherwise = CiVariable
