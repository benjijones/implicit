module Lava.Ram where

import Lava.Bit
import Lava.Binary
import Lava.Word
import Lava.Vector

import qualified Lava.JList as JL
import qualified Data.IntMap as IM

import Data.List (unfoldr)

data RamInputs n m =
  RamInputs {
    ramData    :: Word n
  , ramAddress :: Word m
  , ramWrite   :: Bit
  }

-- | RAM of any width and size, with intialiser.
ram :: (N n, N m) => [Integer] -> RamAlgorithm -> RamInputs n m -> Word n
ram init pt inps = Vec $ primRam init pt $
  RamInps {
      dataBus     = velems (vrigid $ ramData inps)
    , addressBus  = velems (vrigid $ ramAddress inps)
    , writeEnable = ramWrite inps
  }

-- | Dual-port RAM of any width and size, with intialiser.
dualRam :: (N n, N m) => [Integer] -> RamAlgorithm
        -> (RamInputs n m, RamInputs n m) -> (Word n, Word n)
dualRam init pt (inps0, inps1) = (Vec out0, Vec out1)
  where
    (out0, out1) =
      primDualRam init pt
        ( RamInps {
            dataBus     = velems (vrigid $ ramData inps0)
          , addressBus  = velems (vrigid $ ramAddress inps0)
          , writeEnable = ramWrite inps0
          }
        , RamInps {
            dataBus     = velems (vrigid $ ramData inps1)
          , addressBus  = velems (vrigid $ ramAddress inps1)
          , writeEnable = ramWrite inps1
          }
        )

-- | Block RAM inputs; data-bus and address-bus can be of any width!
-- Use 'Lava.Ram.RamInputs' for stronger type-safety.
data RamInps =
  RamInps {
      dataBus :: [Bit]
    , addressBus :: [Bit]
    , writeEnable :: Bit
  }

-- | How should the RAM be built?  Used by the Xilinx Core Generator -
-- see Xilinx docs for details.
data RamAlgorithm =
  MinArea | Width1 | Width2 | Width4 | Width9 | Width18 | Width36

encodeRamAlgorithm :: RamAlgorithm -> String
encodeRamAlgorithm MinArea = ""
encodeRamAlgorithm Width1 = "16kx1"
encodeRamAlgorithm Width2 = "8kx2"
encodeRamAlgorithm Width4 = "4kx4"
encodeRamAlgorithm Width9 = "2kx9"
encodeRamAlgorithm Width18 = "1kx18"
encodeRamAlgorithm Width36 = "512x36"

-- | Single-port RAM with initialiser.  Use 'Lava.Ram.ram' for
-- stronger type-safety.
primRam :: [Integer] -> RamAlgorithm -> RamInps -> [Bit]
primRam init ramAlg ins =
    makeComponent "ram"
  {-   Inputs: -} ([writeEnable ins] ++ dataBus ins ++ addressBus ins)
  {-  Outputs: -} dwidth
  {- Simulate: -} (simRam dwidth awidth init)
  {-   Params: -} [ "init" :-> show init
                  , "dwidth" :-> show dwidth
                  , "awidth" :-> show awidth
                  , "primtype" :-> pt
                  ]
  {- Continue: -} id
  where
    pt     = encodeRamAlgorithm ramAlg
    dwidth = length (dataBus ins)
    awidth = length (addressBus ins)

-- | Dual-port RAM with initialiser.  Use 'Lava.Ram.dualRam' for
-- stronger type-safety.
primDualRam :: [Integer] -> RamAlgorithm -> (RamInps, RamInps) -> ([Bit], [Bit])
primDualRam init ramAlg (ins1, ins2) =
    makeComponent "dualRam"
  {-   Inputs: -} ([writeEnable ins1] ++ [writeEnable ins2] ++
                   dataBus ins1       ++ dataBus ins2       ++
                   addressBus ins1    ++ addressBus ins2    )
  {-  Outputs: -} (2*dwidth)
  {- Simulate: -} (simDualRam dwidth awidth init)
  {-   Params: -} [ "init" :-> show init
                  , "dwidth" :-> show dwidth
                  , "awidth" :-> show awidth
                  , "primtype" :-> pt
                  ]
  {- Continue: -} (splitAt dwidth)
  where
    pt     = encodeRamAlgorithm ramAlg
    dwidth = sameLength (dataBus ins1)    (dataBus ins2)
    awidth = sameLength (addressBus ins1) (addressBus ins2)
    sameLength xs ys = if length xs == length ys then length xs else
                         error "BlockRam ports must have same bus-widths"

simRam :: Int -> Int -> [Integer] -> [[Bool]] -> [[Bool]]
simRam dwidth awidth init (we:sigs) =
  trans $ unfoldr step (zero, initialMap, we, dbus, abus)
  where
    (dbus, abus)   = splitAt dwidth sigs
    init'          = map (\x -> natToSizedBin x dwidth) init
    initialMap     = IM.fromList $ zip [0..2^awidth-1] init'
    zero           = replicate dwidth False
    step (o, m, we, dbus, abus) = Just (o, next)
      where i      = binToNat (map head abus)
            m'     = if head we then IM.insert i (map head dbus) m else m
            output = IM.findWithDefault zero i m'
            next   = (output, m', tail we, map tail dbus, map tail abus)

-- Simulation function for dual-port RAMs.
simDualRam :: Int -> Int -> [Integer] -> [[Bool]] -> [[Bool]]
simDualRam dwidth awidth init (we1:we2:sigs) = trans $
    unfoldr step (zero, zero, initial, we1, we2, dbus1, dbus2, abus1, abus2)
  where
    (dbus, abus)    = splitAt (2*dwidth) sigs
    (abus1, abus2)  = splitAt awidth abus
    (dbus1, dbus2)  = splitAt dwidth dbus
    init'           = map (\x -> natToSizedBin x dwidth) init
    initial         = IM.fromList $ zip [0..2^awidth-1] init'
    zero            = replicate dwidth False
    step (o1, o2, m, we1, we2, dbus1, dbus2, abus1, abus2) =
        Just (o1 ++ o2, next)
      where i       = binToNat (map head abus1)
            j       = binToNat (map head abus2)
            output1 = IM.findWithDefault zero i m''
            output2 = IM.findWithDefault zero j m''
            m'      = if head we1 then IM.insert i (map head dbus1) m else m
            m''     = if head we2 then IM.insert j (map head dbus2) m' else m'
            next    = (output1, output2,
                       m'',
                       tail we1, tail we2,
                       map tail dbus1, map tail dbus2,
                       map tail abus1, map tail abus2)

lazyZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
lazyZipWith f [] bs = []
lazyZipWith f (a:as) bs = f a (hd bs) : lazyZipWith f as (tail bs)
  where
    hd [] = error "lazyZipWith: incompatible structures"
    hd (a:as) = a

trans :: [[a]] -> [[a]]
trans (x:xs) = lazyZipWith (:) x (trans xs)