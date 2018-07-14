{-# LANGUAGE TemplateHaskell       #-}

module Language.Haskell.LSP.TH.ClientCapabilities where

import           Data.Aeson.TH
import qualified Data.Aeson as A
import Language.Haskell.LSP.TH.Constants
import Language.Haskell.LSP.TH.CodeAction
import Language.Haskell.LSP.TH.List
import Data.Default

-- ---------------------------------------------------------------------
{-
New in 3.0
----------

WorkspaceClientCapabilities

define capabilities the editor / tool provides on the workspace:
/**
 * Workspace specific client capabilities.
 */
export interface WorkspaceClientCapabilities {
        /**
         * The client supports applying batch edits to the workspace by supporting
         * the request 'workspace/applyEdit'
         */
        applyEdit?: boolean;

        /**
         * Capabilities specific to `WorkspaceEdit`s
         */
        workspaceEdit?: {
                /**
                 * The client supports versioned document changes in `WorkspaceEdit`s
                 */
                documentChanges?: boolean;
        };

        /**
         * Capabilities specific to the `workspace/didChangeConfiguration` notification.
         */
        didChangeConfiguration?: {
                /**
                 * Did change configuration notification supports dynamic registration.
                 */
                dynamicRegistration?: boolean;
        };

        /**
         * Capabilities specific to the `workspace/didChangeWatchedFiles` notification.
         */
        didChangeWatchedFiles?: {
                /**
                 * Did change watched files notification supports dynamic registration.
                 */
                dynamicRegistration?: boolean;
        };

        /**
         * Capabilities specific to the `workspace/symbol` request.
         */
        symbol?: {
                /**
                 * Symbol request supports dynamic registration.
                 */
                dynamicRegistration?: boolean;
        };

        /**
         * Capabilities specific to the `workspace/executeCommand` request.
         */
        executeCommand?: {
                /**
                 * Execute command supports dynamic registration.
                 */
                dynamicRegistration?: boolean;
        };
}
-}

-- -------------------------------------

data WorkspaceEditClientCapabilities =
  WorkspaceEditClientCapabilities
  { _weccDocumentChanges :: Maybe Bool -- ^The client supports versioned document
                                   -- changes in `WorkspaceEdit`s
  } deriving (Show, Read, Eq)

$(deriveJSON lspOptions ''WorkspaceEditClientCapabilities)

-- -------------------------------------

data DidChangeConfigurationClientCapabilities =
  DidChangeConfigurationClientCapabilities
    { _dcccDynamicRegistration :: Maybe Bool -- ^Did change configuration
                                         -- notification supports dynamic
                                         -- registration.
    } deriving (Show, Read, Eq)

$(deriveJSON lspOptions ''DidChangeConfigurationClientCapabilities)

-- -------------------------------------

data DidChangeWatchedFilesClientCapabilities =
  DidChangeWatchedFilesClientCapabilities
    { _dcwfccDynamicRegistration :: Maybe Bool -- ^Did change watched files
                                         -- notification supports dynamic
                                         -- registration.
    } deriving (Show, Read, Eq)

$(deriveJSON lspOptions ''DidChangeWatchedFilesClientCapabilities)

-- -------------------------------------

data SymbolClientCapabilities =
  SymbolClientCapabilities
    { _sccDynamicRegistration :: Maybe Bool -- ^Symbol request supports dynamic
                                         -- registration.
    } deriving (Show, Read, Eq)

$(deriveJSON lspOptions ''SymbolClientCapabilities)

-- -------------------------------------

data ExecuteClientCapabilities =
  ExecuteClientCapabilities
    { _eccDynamicRegistration :: Maybe Bool -- ^Execute command supports dynamic
                                         -- registration.
    } deriving (Show, Read, Eq)

$(deriveJSON lspOptions ''ExecuteClientCapabilities)

-- -------------------------------------

data WorkspaceClientCapabilities =
  WorkspaceClientCapabilities
    { -- | The client supports applying batch edits to the workspace by supporting
      -- the request 'workspace/applyEdit'
      _wccApplyEdit :: Maybe Bool

      -- | Capabilities specific to `WorkspaceEdit`s
    , _wccWorkspaceEdit :: Maybe WorkspaceEditClientCapabilities

      -- | Capabilities specific to the `workspace/didChangeConfiguration` notification.
    , _wccDidChangeConfiguration :: Maybe DidChangeConfigurationClientCapabilities

       -- | Capabilities specific to the `workspace/didChangeWatchedFiles` notification.
    , _wccDidChangeWatchedFiles :: Maybe DidChangeWatchedFilesClientCapabilities

      -- | Capabilities specific to the `workspace/symbol` request.
    , _wccSymbol :: Maybe SymbolClientCapabilities

      -- | Capabilities specific to the `workspace/executeCommand` request.
    , _wccExecuteCommand :: Maybe ExecuteClientCapabilities
    } deriving (Show, Read, Eq)

$(deriveJSON lspOptions ''WorkspaceClientCapabilities)

instance Default WorkspaceClientCapabilities where
  def = WorkspaceClientCapabilities def def def def def def

-- ---------------------------------------------------------------------
{-
New in 3.0
----------

TextDocumentClientCapabilities
    define capabilities the editor / tool provides on text documents.

/**
 * Text document specific client capabilities.
 */
export interface TextDocumentClientCapabilities {

        synchronization?: {
                /**
                 * Whether text document synchronization supports dynamic registration.
                 */
                dynamicRegistration?: boolean;

                /**
                 * The client supports sending will save notifications.
                 */
                willSave?: boolean;

                /**
                 * The client supports sending a will save request and
                 * waits for a response providing text edits which will
                 * be applied to the document before it is saved.
                 */
                willSaveWaitUntil?: boolean;

                /**
                 * The client supports did save notifications.
                 */
                didSave?: boolean;
        }

        /**
         * Capabilities specific to the `textDocument/completion`
         */
        completion?: {
                /**
                 * Whether completion supports dynamic registration.
                 */
                dynamicRegistration?: boolean;

                /**
                 * The client supports the following `CompletionItem` specific
                 * capabilities.
                 */
                completionItem?: {
                        /**
                         * Client supports snippets as insert text.
                         *
                         * A snippet can define tab stops and placeholders with `$1`, `$2`
                         * and `${3:foo}`. `$0` defines the final tab stop, it defaults to
                         * the end of the snippet. Placeholders with equal identifiers are linked,
                         * that is typing in one will update others too.
                         */
                        snippetSupport?: boolean;
                }
        };

        /**
         * Capabilities specific to the `textDocument/hover`
         */
        hover?: {
                /**
                 * Whether hover supports dynamic registration.
                 */
                dynamicRegistration?: boolean;
        };

        /**
         * Capabilities specific to the `textDocument/signatureHelp`
         */
        signatureHelp?: {
                /**
                 * Whether signature help supports dynamic registration.
                 */
                dynamicRegistration?: boolean;
        };

        /**
         * Capabilities specific to the `textDocument/references`
         */
        references?: {
                /**
                 * Whether references supports dynamic registration.
                 */
                dynamicRegistration?: boolean;
        };

        /**
         * Capabilities specific to the `textDocument/documentHighlight`
         */
        documentHighlight?: {
                /**
                 * Whether document highlight supports dynamic registration.
                 */
                dynamicRegistration?: boolean;
        };

        /**
         * Capabilities specific to the `textDocument/documentSymbol`
         */
        documentSymbol?: {
                /**
                 * Whether document symbol supports dynamic registration.
                 */
                dynamicRegistration?: boolean;
        };

        /**
         * Capabilities specific to the `textDocument/formatting`
         */
        formatting?: {
                /**
                 * Whether formatting supports dynamic registration.
                 */
                dynamicRegistration?: boolean;
        };

        /**
         * Capabilities specific to the `textDocument/rangeFormatting`
         */
        rangeFormatting?: {
                /**
                 * Whether range formatting supports dynamic registration.
                 */
                dynamicRegistration?: boolean;
        };

        /**
         * Capabilities specific to the `textDocument/onTypeFormatting`
         */
        onTypeFormatting?: {
                /**
                 * Whether on type formatting supports dynamic registration.
                 */
                dynamicRegistration?: boolean;
        };

        /**
         * Capabilities specific to the `textDocument/definition`
         */
        definition?: {
                /**
                 * Whether definition supports dynamic registration.
                 */
                dynamicRegistration?: boolean;
        };

        /**
         * Capabilities specific to the `textDocument/codeAction`
         */
        codeAction?: {
                /**
                 * Whether code action supports dynamic registration.
                 */
                dynamicRegistration?: boolean;
        };

        /**
         * Capabilities specific to the `textDocument/codeLens`
         */
        codeLens?: {
                /**
                 * Whether code lens supports dynamic registration.
                 */
                dynamicRegistration?: boolean;
        };

        /**
         * Capabilities specific to the `textDocument/documentLink`
         */
        documentLink?: {
                /**
                 * Whether document link supports dynamic registration.
                 */
                dynamicRegistration?: boolean;
        };

        /**
         * Capabilities specific to the `textDocument/rename`
         */
        rename?: {
                /**
                 * Whether rename supports dynamic registration.
                 */
                dynamicRegistration?: boolean;
        };
}

-}

-- -------------------------------------

-- TODO:AZ: this name is Java-ridiculously long
data SynchronizationTextDocumentClientCapabilities =
  SynchronizationTextDocumentClientCapabilities
    { -- | Whether text document synchronization supports dynamic registration.
      _stdccDynamicRegistration :: Maybe Bool

      -- | The client supports sending will save notifications.
    , _stdccWillSave :: Maybe Bool

      -- | The client supports sending a will save request and waits for a
      -- response providing text edits which will be applied to the document
      -- before it is saved.
    , _stdccWillSaveWaitUntil :: Maybe Bool

      -- | The client supports did save notifications.
    , _stdccDidSave :: Maybe Bool
    } deriving (Show, Read, Eq)

$(deriveJSON lspOptions ''SynchronizationTextDocumentClientCapabilities)

instance Default SynchronizationTextDocumentClientCapabilities where
  def = SynchronizationTextDocumentClientCapabilities def def def def

-- -------------------------------------

data CompletionItemClientCapabilities =
  CompletionItemClientCapabilities
    {
      -- | Client supports snippets as insert text.
      --
      -- A snippet can define tab stops and placeholders with `$1`, `$2` and
      -- `${3:foo}`. `$0` defines the final tab stop, it defaults to the end of
      -- the snippet. Placeholders with equal identifiers are linked, that is
      -- typing in one will update others too.
      _ciccSnippetSupport :: Maybe Bool
    } deriving (Show, Read, Eq)

$(deriveJSON lspOptions ''CompletionItemClientCapabilities)

data CompletionClientCapabilities =
  CompletionClientCapabilities
    { _cccDynamicRegistration :: Maybe Bool -- ^Whether completion supports dynamic
                                         -- registration.
    , _cccCompletionItem :: Maybe CompletionItemClientCapabilities
    } deriving (Show, Read, Eq)

$(deriveJSON lspOptions ''CompletionClientCapabilities)

-- -------------------------------------

data HoverClientCapabilities =
  HoverClientCapabilities
    { _hccDynamicRegistration :: Maybe Bool
    } deriving (Show, Read, Eq)

$(deriveJSON lspOptions ''HoverClientCapabilities)

-- -------------------------------------

data SignatureHelpClientCapabilities =
  SignatureHelpClientCapabilities
    { _shccDynamicRegistration :: Maybe Bool
    } deriving (Show, Read, Eq)

$(deriveJSON lspOptions ''SignatureHelpClientCapabilities)

-- -------------------------------------

data ReferencesClientCapabilities =
  ReferencesClientCapabilities
    { _refccDynamicRegistration :: Maybe Bool
    } deriving (Show, Read, Eq)

$(deriveJSON lspOptions ''ReferencesClientCapabilities)

-- -------------------------------------

data DocumentHighlightClientCapabilities =
  DocumentHighlightClientCapabilities
    { _dhccDynamicRegistration :: Maybe Bool
    } deriving (Show, Read, Eq)

$(deriveJSON lspOptions ''DocumentHighlightClientCapabilities)

-- -------------------------------------

data DocumentSymbolClientCapabilities =
  DocumentSymbolClientCapabilities
    { _dsccDynamicRegistration :: Maybe Bool
    } deriving (Show, Read, Eq)

$(deriveJSON lspOptions ''DocumentSymbolClientCapabilities)

-- -------------------------------------

data FormattingClientCapabilities =
  FormattingClientCapabilities
    { _fccDynamicRegistration :: Maybe Bool
    } deriving (Show, Read, Eq)

$(deriveJSON lspOptions ''FormattingClientCapabilities)

-- -------------------------------------

data RangeFormattingClientCapabilities =
  RangeFormattingClientCapabilities
    { _dynamicRegistration :: Maybe Bool
    } deriving (Show, Read, Eq)

$(deriveJSON lspOptions ''RangeFormattingClientCapabilities)

-- -------------------------------------

data OnTypeFormattingClientCapabilities =
  OnTypeFormattingClientCapabilities
    { _otfccDynamicRegistration :: Maybe Bool
    } deriving (Show, Read, Eq)

$(deriveJSON lspOptions ''OnTypeFormattingClientCapabilities)

-- -------------------------------------

data DefinitionClientCapabilities =
  DefinitionClientCapabilities
    { _dccDynamicRegistration :: Maybe Bool
    } deriving (Show, Read, Eq)

$(deriveJSON lspOptions ''DefinitionClientCapabilities)

-- -------------------------------------

data CodeActionKindValueSet =
  CodeActionKindValueSet
   { _cakvsValueSet :: List CodeActionKind
   } deriving (Show, Read, Eq)

instance Default CodeActionKindValueSet where
  def = CodeActionKindValueSet (List allKinds)
    where allKinds = [ CodeActionQuickFix
                     , CodeActionRefactor
                     , CodeActionRefactorExtract
                     , CodeActionRefactorInline
                     , CodeActionRefactorRewrite
                     , CodeActionSource
                     , CodeActionSourceOrganizeImports
                     ]

$(deriveJSON lspOptions ''CodeActionKindValueSet)

data CodeActionLiteralSupport =
  CodeActionLiteralSupport
    { _calsCodeActionKind :: CodeActionKindValueSet -- ^ The code action kind is support with the following value set.
    } deriving (Show, Read, Eq)

$(deriveJSON lspOptions ''CodeActionLiteralSupport)

data CodeActionClientCapabilities =
  CodeActionClientCapabilities
    { _caccDynamicRegistration      :: Maybe Bool -- ^ Whether code action supports dynamic registration.
    , _caccCodeActionLiteralSupport :: Maybe CodeActionLiteralSupport -- ^ The client support code action literals as a valid response
                                                                 -- of the `textDocument/codeAction` request.
                                                                 -- Since 3.8.0
    } deriving (Show, Read, Eq)

$(deriveJSON lspOptions ''CodeActionClientCapabilities)

-- -------------------------------------

data CodeLensClientCapabilities =
  CodeLensClientCapabilities
    { _clccDynamicRegistration :: Maybe Bool
    } deriving (Show, Read, Eq)

$(deriveJSON lspOptions ''CodeLensClientCapabilities)

-- -------------------------------------

data DocumentLinkClientCapabilities =
  DocumentLinkClientCapabilities
    { _dlccDynamicRegistration :: Maybe Bool
    } deriving (Show, Read, Eq)

$(deriveJSON lspOptions ''DocumentLinkClientCapabilities)

-- -------------------------------------

data RenameClientCapabilities =
  RenameClientCapabilities
    { _renccDynamicRegistration :: Maybe Bool
    } deriving (Show, Read, Eq)

$(deriveJSON lspOptions ''RenameClientCapabilities)

-- -------------------------------------

data TextDocumentClientCapabilities =
  TextDocumentClientCapabilities
    { _tdccSynchronization :: Maybe SynchronizationTextDocumentClientCapabilities

      -- | Capabilities specific to the `textDocument/completion`
    , _tdccCompletion :: Maybe CompletionClientCapabilities

      -- | Capabilities specific to the `textDocument/hover`
    , _tdccHover :: Maybe HoverClientCapabilities

      -- | Capabilities specific to the `textDocument/signatureHelp`
    , _tdccSignatureHelp :: Maybe SignatureHelpClientCapabilities

      -- | Capabilities specific to the `textDocument/references`
    , _tdccReferences :: Maybe ReferencesClientCapabilities

      -- | Capabilities specific to the `textDocument/documentHighlight`
    , _tdccDocumentHighlight :: Maybe DocumentHighlightClientCapabilities

      -- | Capabilities specific to the `textDocument/documentSymbol`
    , _tdccDocumentSymbol :: Maybe DocumentSymbolClientCapabilities

      -- | Capabilities specific to the `textDocument/formatting`
    , _tdccFormatting :: Maybe FormattingClientCapabilities

      -- | Capabilities specific to the `textDocument/rangeFormatting`
    , _tdccRangeFormatting :: Maybe RangeFormattingClientCapabilities

      -- | Capabilities specific to the `textDocument/onTypeFormatting`
    , _tdccOnTypeFormatting :: Maybe OnTypeFormattingClientCapabilities

      -- | Capabilities specific to the `textDocument/definition`
    , _tdccDefinition :: Maybe DefinitionClientCapabilities

      -- | Capabilities specific to the `textDocument/codeAction`
    , _tdccCodeAction :: Maybe CodeActionClientCapabilities

      -- | Capabilities specific to the `textDocument/codeLens`
    , _tdccCodeLens :: Maybe CodeLensClientCapabilities

      -- | Capabilities specific to the `textDocument/documentLink`
    , _tdccDocumentLink :: Maybe DocumentLinkClientCapabilities

      -- | Capabilities specific to the `textDocument/rename`
    , _tdccRename :: Maybe RenameClientCapabilities
    } deriving (Show, Read, Eq)

$(deriveJSON lspOptions ''TextDocumentClientCapabilities)

instance Default TextDocumentClientCapabilities where
  def = TextDocumentClientCapabilities def def def def def def def
                                       def def def def def def def def

-- ---------------------------------------------------------------------
{-
New in 3.0

-----------

ClientCapabilities

now define capabilities for dynamic registration, workspace and text document
features the client supports. The experimental can be used to pass experimential
capabilities under development. For future compatibility a ClientCapabilities
object literal can have more properties set than currently defined. Servers
receiving a ClientCapabilities object literal with unknown properties should
ignore these properties. A missing property should be interpreted as an absence
of the capability. If a property is missing that defines sub properties all sub
properties should be interpreted as an absence of the capability.

Client capabilities got introduced with the version 3.0 of the protocol. They
therefore only describe capabilities that got introduced in 3.x or later.
Capabilities that existed in the 2.x version of the protocol are still mandatory
for clients. Clients cannot opt out of providing them. So even if a client omits
the ClientCapabilities.textDocument.synchronization it is still required that
the client provides text document synchronization (e.g. open, changed and close
notifications).

interface ClientCapabilities {
        /**
         * Workspace specific client capabilities.
         */
        workspace?: WorkspaceClientCapabilities;

        /**
         * Text document specific client capabilities.
         */
        textDocument?: TextDocumentClientCapabilities;

        /**
         * Experimental client capabilities.
         */
        experimental?: any;
}
-}

data ClientCapabilities =
  ClientCapabilities
    { _ccWorkspace    :: Maybe WorkspaceClientCapabilities
    , _ccTextDocument :: Maybe TextDocumentClientCapabilities
    , _ccExperimental :: Maybe A.Object
    } deriving (Show, Read, Eq)

$(deriveJSON lspOptions ''ClientCapabilities)

instance Default ClientCapabilities where
  def = ClientCapabilities def def def
