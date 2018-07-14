{-# LANGUAGE CPP                        #-}
{-# LANGUAGE TemplateHaskell            #-}
module Language.Haskell.LSP.TH.WorkspaceEdit where

import           Data.Aeson.TH
import qualified Data.HashMap.Strict                        as H
-- For <= 8.2.2
import           Data.Monoid                                ((<>))
import           Data.Text                                  (Text)
import           Language.Haskell.LSP.TH.Constants
import           Language.Haskell.LSP.TH.List
import           Language.Haskell.LSP.TH.Location
import           Language.Haskell.LSP.TH.Uri

-- ---------------------------------------------------------------------
{-
TextEdit

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#textedit

A textual edit applicable to a text document.

interface TextEdit {
    /**
     * The range of the text document to be manipulated. To insert
     * text into a document create a range where start === end.
     */
    range: Range;

    /**
     * The string to be inserted. For delete operations use an
     * empty string.
     */
    newText: string;
}


-}

data TextEdit =
  TextEdit
    { _teRange   :: Range
    , _teNewText :: Text
    } deriving (Show,Read,Eq)

deriveJSON lspOptions ''TextEdit

-- ---------------------------------------------------------------------
{-
VersionedTextDocumentIdentifier

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#versionedtextdocumentidentifier

    New: An identifier to denote a specific version of a text document.

interface VersionedTextDocumentIdentifier extends TextDocumentIdentifier {
    /**
	 * The version number of this document. If a versioned text document identifier
	 * is sent from the server to the client and the file is not open in the editor
	 * (the server has not received an open notification before) the server can send
	 * `null` to indicate that the version is known and the content on disk is the
	 * truth (as speced with document content ownership)
	 */
	version: number | null;
-}

type TextDocumentVersion = Maybe Int

data VersionedTextDocumentIdentifier =
  VersionedTextDocumentIdentifier
    { _vtdiUri     :: Uri
    , _vtdiVersion :: TextDocumentVersion
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''VersionedTextDocumentIdentifier

-- ---------------------------------------------------------------------
{-
New in 3.0
----------

TextDocumentEdit
https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#new-textdocumentedit

If multiple TextEdits are applied to a text document, all text edits describe
changes made to the initial document version. Execution wise text edits should
applied from the bottom to the top of the text document. Overlapping text edits
are not supported.

export interface TextDocumentEdit {
        /**
         * The text document to change.
         */
        textDocument: VersionedTextDocumentIdentifier;

        /**
         * The edits to be applied.
         */
        edits: TextEdit[];
}

-}

data TextDocumentEdit =
  TextDocumentEdit
    { _tdeTextDocument :: VersionedTextDocumentIdentifier
    , _tdeEdits        :: List TextEdit
    } deriving (Show, Read, Eq)

deriveJSON lspOptions ''TextDocumentEdit

-- ---------------------------------------------------------------------
{-
Changed in 3.0
--------------

WorkspaceEdit

https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#workspaceedit


Changed A workspace edit represents changes to many resources managed in the
workspace. The edit should either provide changes or documentChanges. If
documentChanges are present they are preferred over changes if the client can
handle versioned document edits.

export interface WorkspaceEdit {
        /**
         * Holds changes to existing resources.
         */
        changes?: { [uri: string]: TextEdit[]; };

        /**
         * An array of `TextDocumentEdit`s to express changes to specific a specific
         * version of a text document. Whether a client supports versioned document
         * edits is expressed via `WorkspaceClientCapabilities.versionedWorkspaceEdit`.
         */
        documentChanges?: TextDocumentEdit[];
}
-}

type WorkspaceEditMap = H.HashMap Uri (List TextEdit)

data WorkspaceEdit =
  WorkspaceEdit
    { _weChanges         :: Maybe WorkspaceEditMap
    , _weDocumentChanges :: Maybe (List TextDocumentEdit)
    } deriving (Show, Read, Eq)

instance Monoid WorkspaceEdit where
  mempty = WorkspaceEdit Nothing Nothing
  mappend (WorkspaceEdit a b) (WorkspaceEdit c d) = WorkspaceEdit (a <> c) (b <> d)

deriveJSON lspOptions ''WorkspaceEdit

#if __GLASGOW_HASKELL__ >= 804
instance Semigroup WorkspaceEdit where
  (<>) = mappend
#endif
