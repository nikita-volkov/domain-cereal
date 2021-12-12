module DomainCereal.InstanceDecs where

import qualified Data.Serialize as Cereal
import DomainCereal.Prelude
import qualified DomainCore.Model as Model
import qualified DomainCore.TH as DomainTH
import Language.Haskell.TH.Syntax
import THLego.Helpers

serialize :: Model.TypeDec -> [Dec]
serialize (Model.TypeDec typeName _) =
  pure $ InstanceD Nothing [] headType []
  where
    headType =
      AppT (ConT ''Cereal.Serialize) (ConT (textName typeName))
