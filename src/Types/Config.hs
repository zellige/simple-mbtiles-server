{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Types.Config where

import qualified Options.Generic as OptionsGeneric

data CommandLine w
  = CommandLine
      { mbtilesFile :: w OptionsGeneric.::: Maybe FilePath OptionsGeneric.<?> "Mbtiles file to serve",
        browser :: w OptionsGeneric.::: Bool OptionsGeneric.<?> "Start a browser with a simple map to view tiles"
      }
  deriving (OptionsGeneric.Generic)

modifiers :: OptionsGeneric.Modifiers
modifiers =
  OptionsGeneric.defaultModifiers
    { OptionsGeneric.shortNameModifier = OptionsGeneric.firstLetter
    }

instance OptionsGeneric.ParseRecord (CommandLine OptionsGeneric.Wrapped) where
  parseRecord = OptionsGeneric.parseRecordWithModifiers modifiers

deriving instance Show (CommandLine OptionsGeneric.Unwrapped)
