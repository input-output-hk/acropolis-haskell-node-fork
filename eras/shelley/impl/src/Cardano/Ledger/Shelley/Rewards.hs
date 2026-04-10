{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Ledger.Shelley.Rewards (
  StakeShare (..),
  PoolRewardInfo (..),
  mkApparentPerformance,
  RewardType (..),
  Reward (..),
  LeaderOnlyReward (..),
  leaderRewardToGeneral,
  leaderRew,
  memberRew,
  aggregateRewards,
  filterRewards,
  sumRewards,
  aggregateCompactRewards,
  sumCompactRewards,
  rewardOnePoolMember,
  mkPoolRewardInfo,
)
where

import Cardano.Ledger.BaseTypes (
  BlocksMade (..),
  BoundedRational (..),
--  NonNegativeInterval,
--  NonZero,
  ProtVer,
  UnitInterval,
  nonZeroOr,
--  unNonZero,
--  recipNonZero,
--  toIntegerNonZero,
--  toRatioNonZero,
--  (/.),
  (%?),
 )
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
 )
import Cardano.Ledger.Binary.Coders (Decode (..), Encode (..), decode, encode, (!>), (<!))
import Cardano.Ledger.Coin (
  Coin (..),
  CompactForm,
  coinToRational,
  rationalToCoinViaFloor,
 )
import Cardano.Ledger.Compactible (fromCompact)
import Cardano.Ledger.Core
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.PoolParams (PoolParams (..))
import qualified Cardano.Ledger.Shelley.HardForks as HardForks
import Cardano.Ledger.State (Stake (..), maxPool')
import Cardano.Ledger.Slot (EpochNo (..))
import Cardano.Ledger.UMap (compactCoinOrError)
import Cardano.Ledger.Val ((<->))
import Control.DeepSeq (NFData)
import Control.Monad (guard)
import Data.Foldable (fold, foldMap')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import Data.Set (Set)
--import Data.Word (Word16)
import qualified Data.Set as Set
import qualified Data.VMap as VMap
import GHC.Generics (Generic)
import Lens.Micro ((^.))
import NoThunks.Class (NoThunks (..))
import Numeric.Natural (Natural)
import Quiet

--import Debug.Trace

-- | StakeShare type
newtype StakeShare = StakeShare {unStakeShare :: Rational}
  deriving (Generic, Ord, Eq, NoThunks)
  deriving (Show) via Quiet StakeShare

instance NFData StakeShare

-- | Calculate pool reward
mkApparentPerformance ::
  UnitInterval ->
  Rational ->
  Natural ->
  Natural ->
  Rational
mkApparentPerformance d_ sigma blocksN blocksTotal
  | sigma == 0 = 0
  | unboundRational d_ < 0.8 = beta / sigma
  | otherwise = 1
  where
    beta = fromIntegral blocksN / fromIntegral (max 1 blocksTotal)

-- | Calculate pool leader reward
leaderRew ::
  Coin ->
  PoolParams ->
  StakeShare ->
  StakeShare ->
  Coin
leaderRew f pool (StakeShare s) (StakeShare sigma)
  | f <= c = f
  | otherwise =
      c
        <> rationalToCoinViaFloor
          (coinToRational (f <-> c) * (m' + (1 - m') * s / sigma))
  where
    c = ppCost pool
    m = ppMargin pool
    m' = unboundRational m

-- | Calculate pool member reward
memberRew ::
  Coin ->
  PoolParams ->
  StakeShare ->
  StakeShare ->
  Coin
memberRew (Coin f') pool (StakeShare t) (StakeShare sigma)
  | f' <= c = mempty
  | otherwise =
      rationalToCoinViaFloor $
        fromIntegral (f' - c) * (1 - m') * t / sigma
  where
    Coin c = ppCost pool
    m = ppMargin pool
    m' = unboundRational m

sumRewards ::
  ProtVer ->
  Map (Credential 'Staking) (Set Reward) ->
  Coin
sumRewards protocolVersion rs = fold $ aggregateRewards protocolVersion rs

-- | Filter the reward payments to those that will actually be delivered. This
-- function exists since in Shelley, a stake credential earning rewards from
-- multiple sources would only receive one reward. So some of the coins are ignored,
-- because of this backward compatibility issue in early protocolVersions. Note that
-- both of the domains of the returned maps are a subset of the the domain of the input map 'rewards'
filterRewards ::
  ProtVer ->
  Map (Credential 'Staking) (Set Reward) ->
  ( Map (Credential 'Staking) (Set Reward) -- delivered
  , Map (Credential 'Staking) (Set Reward) -- ignored in Shelley Era
  )
filterRewards pv rewards =
  if HardForks.aggregatedRewards pv
    then (rewards, Map.empty)
    else
      let mp = Map.map Set.deleteFindMin rewards
       in (Map.map (Set.singleton . fst) mp, Map.filter (not . Set.null) $ Map.map snd mp)

-- | for each (Set (Reward c)) entry in the map, sum up the coin. In the ShelleyEra
--   some of the coins are ignored (because of backward compatibility) see 'filterRewards'
--   Note that domain of the returned map is a subset of the input map 'rewards'
aggregateRewards ::
  ProtVer ->
  Map (Credential 'Staking) (Set Reward) ->
  Map (Credential 'Staking) Coin
aggregateRewards pv rewards =
  Map.map (foldMap' rewardAmount) $ fst $ filterRewards pv rewards

-- ================================================
-- Compact Coin versions of sumRewards and aggregateCompactRewards

sumCompactRewards ::
  ProtVer ->
  Map (Credential 'Staking) (Set Reward) ->
  CompactForm Coin
sumCompactRewards protocolVersion rs = fold $ aggregateCompactRewards protocolVersion rs

-- | for each (Set (Reward c)) entry in the map, sum up the coin. In the ShelleyEra
--   some of the coins are ignored (because of backward compatibility) see 'filterRewards'
--   Note that the domain of the output map is a subset of the domain of the input rewards.
aggregateCompactRewards ::
  ProtVer ->
  Map (Credential 'Staking) (Set Reward) ->
  Map (Credential 'Staking) (CompactForm Coin)
aggregateCompactRewards pv rewards =
  Map.map (foldMap' (compactCoinOrError . rewardAmount)) $ fst $ filterRewards pv rewards

-- We argue that the call to 'compactCoinOrError' will never return error.
-- The Reward is stored in the LedgerState, and we know the sum of all Ada in the LedgerState cannot
-- exceed (maxBound :: Word64), So if the sum cannot exceed it, neither can any component of the sum.
-- We need a (CompactForm Coin) because the reward map is stored in the UMap, which stores rewards
-- as (CompactForm Coin). And aggregateRewards is used to update that part of the UMap.
-- See  Cardano.Ledger.Shelley.LedgerState.IncrementalStake(applyRUpdFiltered)

-- =====================================================

data LeaderOnlyReward = LeaderOnlyReward
  { lRewardPool :: !(KeyHash 'StakePool)
  , lRewardAmount :: !Coin
  }
  deriving (Eq, Ord, Show, Generic)

instance NoThunks LeaderOnlyReward

instance NFData LeaderOnlyReward

instance EncCBOR LeaderOnlyReward where
  encCBOR (LeaderOnlyReward pool c) = encode $ Rec LeaderOnlyReward !> To pool !> To c

instance DecCBOR LeaderOnlyReward where
  decCBOR = decode $ RecD LeaderOnlyReward <! From <! From

leaderRewardToGeneral :: LeaderOnlyReward -> Reward
leaderRewardToGeneral (LeaderOnlyReward poolId r) = Reward LeaderReward poolId r

-- | Stake Pool specific information needed to compute the rewards
-- for its members.
data PoolRewardInfo = PoolRewardInfo
  { poolRelativeStake :: !StakeShare
  -- ^ The stake pool's stake divided by the total stake
  , poolPot :: !Coin
  -- ^ The maximum rewards available for the entire pool
  , poolPs :: !PoolParams
  -- ^ The stake pool parameters
  , poolBlocks :: !Natural
  -- ^ The number of blocks the stake pool produced
  , poolLeaderReward :: !LeaderOnlyReward
  -- ^ The leader reward
  }
  deriving (Show, Eq, Ord, Generic)

instance NoThunks PoolRewardInfo

instance NFData PoolRewardInfo

instance EncCBOR PoolRewardInfo where
  encCBOR
    (PoolRewardInfo a b c d e) =
      encode $
        Rec PoolRewardInfo
          !> E (encCBOR . unStakeShare) a
          !> To b
          !> To c
          !> To d
          !> To e

instance DecCBOR PoolRewardInfo where
  decCBOR =
    decode
      ( RecD PoolRewardInfo
          <! D (StakeShare <$> decCBOR)
          <! From
          <! From
          <! From
          <! From
      )

notPoolOwner ::
  PoolParams ->
  Credential 'Staking ->
  Bool
notPoolOwner pps = \case
  KeyHashObj hk -> hk `Set.notMember` ppOwners pps
  ScriptHashObj _ -> True

-- | The stake pool member reward calculation
rewardOnePoolMember ::
  -- | The protocol version
  ProtVer ->
  -- | The total amount of stake in the system
  Coin ->
  -- | The set of registered stake credentials
  Set (Credential 'Staking) ->
  -- | Stake pool specific intermediate values needed
  -- to compute member rewards.
  PoolRewardInfo ->
  -- | The stake credential whose reward is being calculated.
  Credential 'Staking ->
  -- | The stake controlled by the stake credential
  -- in the previous parameter above.
  Coin ->
  -- | The reward for the given stake credential.
  -- This could be Nothing if the credential is no longer registered,
  -- if it is an owner, or if the reward is zero.
  Maybe Coin
rewardOnePoolMember
  pp
  totalStake
  addrsRew
  rewardInfo
  hk
  (Coin c) =
    let result = if prefilter && notPoolOwner (poolPs rewardInfo) hk && r /= Coin 0 then Just r else Nothing
    in --trace ("**** Calculating reward for " ++ show rewardInfo 
       --  ++ ": totalStake=" ++ show totalStake
       --  ++ ", hk=" ++ show hk
       --  ++ ", c=" ++ show c
       --  ++ ", prefilter=" ++ show prefilter
       --  ++ ", notPoolOwner=" ++ show (notPoolOwner (poolPs rewardInfo) hk)
       --  ++ ", hkMemberAddrsRew=" ++ show (hk `Set.member` addrsRew)
       --  ++ ", sigma=" ++ show sigma 
       --  ++ ", poolR=" ++ show poolR 
       --  ++ ", stakeShare=" ++ show stakeShare 
       --  ++ ", R=" ++ show r
       --  ++ ", finally=" ++ show result ++ " ****") $ 
       result
    where
      prefilter = HardForks.forgoRewardPrefilter pp || hk `Set.member` addrsRew
      pool = poolPs rewardInfo
      sigma = poolRelativeStake rewardInfo
      poolR = poolPot rewardInfo
      -- warning: totalStake could be zero!
      stakeShare =
        StakeShare $ c % unCoin totalStake
      r = memberRew poolR pool stakeShare sigma

--duplicate_maxPool' ::
--  PoolParams ->
--  NonNegativeInterval ->
--  NonZero Word16 ->
--  Coin ->
--  Rational ->
--  Rational ->
--  Coin
--duplicate_maxPool' pool a0 nOpt r sigma pR = 
--    trace ("**** maxPool': "
--            ++ "pool=" ++ show pool
--            ++ ", a0=" ++ show a0
--            ++ ", nOpt=" ++ show nOpt
--            ++ ", r=" ++ show r
--            ++ ", sigma=" ++ show sigma
--            ++ ", pR=" ++ show pR
--            ++ ", nonZeroZ0=" ++ show nonZeroZ0
--            ++ ", z0=" ++ show z0
--            ++ ", sigma'=" ++ show sigma'
--            ++ ", factor1=" ++ show factor1
--            ++ ", factor2=" ++ show factor2
--            ++ ", factor3=" ++ show factor3
--            ++ ", factor4=" ++ show factor4
--    ) $ rationalToCoinViaFloor $ factor1 * factor2
--  where
--    nonZeroZ0 = recipNonZero . toRatioNonZero $ toIntegerNonZero nOpt
--    z0 = unNonZero nonZeroZ0
--    sigma' = min sigma z0
--    p' = min pR z0
--    factor1 =
      -- This division is safe, because a0 is non-negative and we're adding one
      -- to it
--      coinToRational r / (1 + unboundRational a0)
--    factor2 = sigma' + p' * unboundRational a0 * factor3
--    factor3 = (sigma' - p' * factor4) /. nonZeroZ0
--    factor4 = (z0 - sigma') /. nonZeroZ0


-- | Calculate single stake pool specific values for the reward computation.
--
-- Note that if a stake pool has made no blocks in the given epoch, it will
-- get no rewards, and so we do not need to return 'PoolRewardInfo'. We do,
-- however, need to return the relative stake of the pool in order to
-- compute data for the stake pool ranking. Eventually we will remove
-- the ranking information out of the ledger code and into a separate service,
-- and at that point we can simplify this function to not care about ranking.
mkPoolRewardInfo ::
  EraPParams era =>
  EpochNo ->
  PParams era ->
  Coin ->
  BlocksMade ->
  Natural ->
  Stake ->
  VMap.VMap VMap.VB VMap.VB (Credential 'Staking) (KeyHash 'StakePool) ->
  Map (KeyHash 'StakePool) Coin ->
  Coin ->
  Coin ->
  PoolParams ->
  Either StakeShare PoolRewardInfo
mkPoolRewardInfo
  _epoch
  pp
  r
  blocks
  blocksTotal
  stake
  delegs
  stakePerPool
  totalStake
  activeStake
  pool = case Map.lookup (ppId pool) (unBlocksMade blocks) of
    -- This pool made no blocks this epoch. For the purposes of stake pool
    -- ranking only, we return the relative stake of this pool so that we
    -- can judge how likely it was that this pool made no blocks.
    Nothing -> Left $! StakeShare sigma
    -- This pool made no blocks, so we can proceed to calculate the
    -- intermediate values needed for the individual reward calculations.
    Just blocksN ->
      let Coin pledge = ppPledge pool
          -- warning: totalStake could be zero!
          pledgeRelative = pledge % unCoin totalStake
          sigmaA = pstakeTot %? unCoin activeStake
          Coin maxP =
            if pledge <= ostake
              then 
                 let maxpool' = maxPool' pp_a0 pp_nOpt r sigma pledgeRelative
                 in maxpool'
                 --    dmaxpool' = duplicate_maxPool' pool pp_a0 pp_nOpt r sigma pledgeRelative
                 --in if maxpool' /= dmaxpool' 
                 --    then trace ("**** maxPool' /= d_maxPool': " ++ show maxpool' ++ "/=" ++ show dmaxpool' ++ " ****") maxpool'
                 --    else maxpool'
              else mempty
          appPerf = mkApparentPerformance pp_d sigmaA blocksN blocksTotal
          poolR = rationalToCoinViaFloor (appPerf * fromIntegral maxP)
          lreward =
            leaderRew
              poolR
              pool
              (StakeShare $ ostake %? unCoin totalStake)
              (StakeShare sigma)
          rewardInfo =
            PoolRewardInfo
              { poolRelativeStake = StakeShare sigma
              , poolPot = poolR
              , poolPs = pool
              , poolBlocks = blocksN
              , poolLeaderReward = LeaderOnlyReward (ppId pool) lreward
              }
       in 
         --trace ("**** Calculating PoolRewardInfo: epoch=" ++ show epoch
         --    ++ ", blocksN=" ++ show blocksN
         --    ++ ", blocksTotal=" ++ show blocksTotal
         --    ++ ", pp_a0=" ++ show pp_a0
         --    ++ ", pp_nOpt=" ++ show pp_nOpt
         --    ++ ", sigma=" ++ show sigma
         --    ++ ", rewardInfo=" ++ show rewardInfo 
         --    ++ ", plege=" ++ show pledge
         --    ++ ", activeStake=" ++ show activeStake
         --    ++ ", pstakeTot=" ++ show pstakeTot
         --    ++ ", totalStake=" ++ show totalStake
         --    ++ ", ostake=" ++ show ostake
         --    ++ ", pledgeRelative=" ++ show pledgeRelative
         --    ++ ", sigmaA=" ++ show sigmaA
         --    ++ ", maxP=" ++ show maxP
         --    ++ ", appPerf=" ++ show appPerf
         --    ++ ", R=" ++ show r
         --    ++ ", rewardInfo=" ++ show rewardInfo ++ " ****") $
         Right $! rewardInfo
    where
      pp_d = pp ^. ppDG
      pp_a0 = pp ^. ppA0L
      pp_nOpt = (pp ^. ppNOptL) `nonZeroOr` error "nOpt is zero"
      Coin pstakeTot = Map.findWithDefault mempty (ppId pool) stakePerPool
      accOwnerStake c o = maybe c (c <>) $ do
        hk <- VMap.lookup (KeyHashObj o) delegs
        guard (hk == ppId pool)
        fromCompact <$> VMap.lookup (KeyHashObj o) (unStake stake)
      Coin ostake = Set.foldl' accOwnerStake mempty (ppOwners pool)
      sigma = pstakeTot %? unCoin totalStake
