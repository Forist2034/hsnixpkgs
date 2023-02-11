{-# LANGUAGE LambdaCase #-}

module HsNixPkgs.BootTools.ReferenceGraph
  ( GraphNode (..),
    readRefGraphs,
    topologicalSort,
  )
where

import Control.Monad.State.Strict
import Data.Bifunctor
import Data.Foldable
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Hashable
import qualified Data.List as L
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

data GraphNode = GraphNode
  { storePath :: Text,
    exportedName :: Maybe Text,
    dependencies :: [Text]
  }
  deriving (Show)

instance Eq GraphNode where
  l == r = storePath l == storePath r

instance Hashable GraphNode where
  hashWithSalt s v = hashWithSalt s (storePath v)

parseRefGraph :: HM.HashMap Text Text -> Text -> [GraphNode]
parseRefGraph exported = go . T.lines
  where
    go [] = []
    go (sp : _ : lenS : xs) =
      let (deps, rest) = L.splitAt (read (T.unpack lenS)) xs
       in GraphNode
            { storePath = sp,
              exportedName = HM.lookup sp exported,
              dependencies = deps
            }
            : go rest
    go _ = error "Unexpected end"

readRefGraphs :: [(Text, Text)] -> [FilePath] -> IO [GraphNode]
readRefGraphs ex =
  fmap (foldMap' (parseRefGraph (HM.fromList ex)))
    . traverse TIO.readFile

topologicalSort :: [GraphNode] -> [GraphNode]
topologicalSort gn = reverse (fst (execState (traverse_ go gn) ([], HS.empty)))
  where
    nodeMap = HM.fromList (fmap (\n -> (storePath n, n)) gn)
    go :: GraphNode -> State ([GraphNode], HS.HashSet GraphNode) ()
    go x =
      gets (HS.member x . snd) >>= \case
        True -> pure ()
        False -> do
          modify (second (HS.insert x))
          traverse_
            (\sp -> go (fromJust (HM.lookup sp nodeMap)))
            (dependencies x)
          modify (first (x :))