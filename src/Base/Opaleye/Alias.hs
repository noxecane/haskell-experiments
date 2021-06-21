module Base.Opaleye.Alias where

import           Opaleye

type OLocalTime = Field SqlTimestamp
type OUTCTime   = Field SqlTimestamptz
type ODouble    = Field SqlFloat8
type OInt       = Field SqlInt4
type OInt64     = Field SqlInt8
type OText      = Field SqlText

type NOText     = FieldNullable SqlText
