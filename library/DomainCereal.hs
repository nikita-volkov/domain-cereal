module DomainCereal where

import qualified DomainCereal.InstanceDecs as InstanceDecs
import DomainCereal.Prelude
import qualified DomainCore.Deriver as Deriver

serializeDeriver :: Deriver.Deriver
serializeDeriver =
  Deriver.effectless InstanceDecs.serialize
