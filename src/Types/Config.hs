{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Types.Config where

import Options.Generic

data CommandLine w
  = CommandLine
      { mbtilesFile :: w ::: Maybe FilePath <?> "Mbtiles file to serve"
      }
  deriving (Generic)

instance ParseRecord (CommandLine Wrapped)

deriving instance Show (CommandLine Unwrapped)
