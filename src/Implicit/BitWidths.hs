module Implicit.BitWidths where

import Lava.Vector

type AddressN = N11

type DataN = N5
type WordN = S (S (S (S (S DataN))))