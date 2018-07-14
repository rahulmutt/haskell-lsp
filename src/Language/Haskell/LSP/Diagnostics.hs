{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-

Manage the "textDocument/publishDiagnostics" notifications to keep a local copy of the
diagnostics for a particular file and version, partitioned by source.
-}
module Language.Haskell.LSP.Diagnostics
  (
    DiagnosticStore
  , DiagnosticsBySource
  , StoreItem(..)
  , partitionBySource
  , flushBySource
  , updateDiagnostics
  , getDiagnosticParamsFor

  -- * for tests
  ) where

import qualified Data.SortedList as SL
import qualified Data.Map as Map
import qualified Language.Haskell.LSP.TH.DataTypesJSON      as J

-- ---------------------------------------------------------------------
{-# ANN module ("hlint: ignore Eta reduce" :: String) #-}
{-# ANN module ("hlint: ignore Redundant do" :: String) #-}
-- ---------------------------------------------------------------------

{-
We need a three level store

  Uri : Maybe TextDocumentVersion : Maybe DiagnosticSource : [Diagnostics]

For a given Uri, as soon as we see a new (Maybe TextDocumentVersion) we flush
all prior entries for the Uri.

-}

type DiagnosticStore = Map.Map J.Uri StoreItem

data StoreItem
  = StoreItem J.TextDocumentVersion DiagnosticsBySource
  deriving (Show,Eq)

type DiagnosticsBySource = Map.Map (Maybe J.DiagnosticSource) (SL.SortedList J.Diagnostic)

-- ---------------------------------------------------------------------

partitionBySource :: [J.Diagnostic] -> DiagnosticsBySource
partitionBySource diags = Map.fromListWith mappend $ map (\d -> (J._dsSource d, (SL.singleton d))) diags

-- ---------------------------------------------------------------------

flushBySource :: DiagnosticStore -> Maybe J.DiagnosticSource -> DiagnosticStore
flushBySource store Nothing       = store
flushBySource store (Just source) = Map.map remove store
  where
    remove (StoreItem mv diags) = StoreItem mv (Map.delete (Just source) diags)

-- ---------------------------------------------------------------------

updateDiagnostics :: DiagnosticStore
                  -> J.Uri -> J.TextDocumentVersion -> DiagnosticsBySource
                  -> DiagnosticStore
updateDiagnostics store uri mv newDiagsBySource = r
  where
    newStore :: DiagnosticStore
    newStore = Map.insert uri (StoreItem mv newDiagsBySource) store

    updateDbs dbs = Map.insert uri new store
      where
        new = StoreItem mv newDbs
        -- note: Map.union is left-biased, so for identical keys the first
        -- argument is used
        newDbs = Map.union newDiagsBySource dbs

    r = case Map.lookup uri store of
      Nothing -> newStore
      Just (StoreItem mvs dbs) ->
        if mvs /= mv
          then newStore
          else updateDbs dbs

-- ---------------------------------------------------------------------

getDiagnosticParamsFor :: Int -> DiagnosticStore -> J.Uri -> Maybe J.PublishDiagnosticsParams
getDiagnosticParamsFor maxDiagnostics ds uri =
  case Map.lookup uri ds of
    Nothing -> Nothing
    Just (StoreItem _ diags) ->
      Just $ J.PublishDiagnosticsParams uri (J.List (take maxDiagnostics $ SL.fromSortedList $ mconcat $ Map.elems diags))

-- ---------------------------------------------------------------------
