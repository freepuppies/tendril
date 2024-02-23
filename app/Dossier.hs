module Dossier
  ( Dossier
  , include
  , query
  , documents
  , runDossier
  )
where

import Data.Bifunctor (second)
import Data.Functor ((<&>))
import Dossier.Document qualified as D
import Dossier.DocumentTree qualified as DT
import Effectful
import Effectful.Dispatch.Static

data Dossier :: Effect

type instance DispatchOf Dossier = Static NoSideEffects

newtype instance StaticRep Dossier = Dossier DT.DocumentTree

include :: Dossier :> es => D.Document -> Eff es ()
include doc = stateStaticRep \(Dossier tree) -> ((), Dossier $ DT.insert doc tree)

query :: Dossier :> es => D.DocumentPath -> Eff es (Maybe D.Document)
query docPath = getStaticRep <&> \(Dossier tree) -> DT.lookup docPath tree

documents :: Dossier :> es => Eff es [D.Document]
documents = getStaticRep <&> \(Dossier tree) -> DT.flatten tree

runDossier :: DT.DocumentTree -> Eff (Dossier : es) a -> Eff es (a, DT.DocumentTree)
runDossier tree = (<&> second \(Dossier tree') -> tree') . runStaticRep (Dossier tree)
