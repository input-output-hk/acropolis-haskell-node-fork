About this project
==================

The purpose of this custom variant is to make internal computations of 
the Shelley rewards computation visible and available for exporing. 
See rewards computation code in `eras/shelley/impl/...` folder for
more dump info.

This node version is a combination of cardano-node.10.4.1 (commit 420c94fbb) 
with local code for Shelley era Ledger (taken from cardano-ledger commit 
a9e78ae63). Other parts of Ledger library are taken from default Cardano 
package database.

It's believed that other ledger parts can also be localized, by:
(a) checking out cardano-ledger commit a9e78ae63, 
(b) copying necessary code to local directory, 
(c) updating `cabal.project.local` file (by uncommenting/updating 
    proper projects in it). 

The node and ledger versions are not ideally compatible (some tests do not 
compile), but compatibility is good enough for Shelley era to work.

The code and changes here are definitely not perfect, and meant just to
make this dumping easy and not requiring a lot of additional effort.

Calculating rewards in Haskell node (some notes)
================================================

The following text is a partial description of the current Haskell 
implementation (in respect to awards computation).

When snapshots and account states are generated
-----------------------------------------------

Rewards calculation may take a long time, so its calculation is spread
to a whole epoch: it starts from `slot`:

```
    sr <- asks randomnessStabilisationWindow
    slot = epochInfoFirst ei e +* Duration sr
```

and continues for `randomnessStabilisationWindow` slots. However, the
values (accounts, snapshots), used for rewards calculation, are fixed
at earlier slot (at the epoch boundary). The following text explains details
of that.

1. Haskell node calls 
`cardano-ledger/eras/shelley/impl/src/Cardano/Ledger/Shelley/Rules/Rupd.hs`

The rewards calculation is activated in function (monad) `rupdTransition`. 
That's a transition rule: a function that's called at each slot change.
It modifies ledger state (along with `RupdEnv` --- Rupd environment, some
info that is passed between function calls), which is also modified by
other functions.

```
  TRC (RupdEnv b es, ru, s) <- judgmentContext
  (slotsPerEpoch, slot, slotForce, maxLL, asc, k, e) <- liftSTS $ do
```

Here we see that the context keeps `b` (blocks per epoch) and `es` (EpochState), 

Type `EpochState` in its turn contains account list (as first element), and the
account list could be e.g. used for calculation of reserves by `asReserves` function:

```"reserves=" ++ show (asReserves acnt)```

2. `EpochState` (and `AccountState`) in Rupd environment is updated by 
`bheadTransition` in
`cardano-ledger/eras/shelley/impl/src/Cardano/Ledger/Shelley/Rules/Tick.hs`.
This is the only place, where `RupdEnv` structure is created.

This call specifically takes `NewEpochState` epoch state (reflecting epoch state,
which was actual at the epoch boundary), and puts it into `RupdEnv` structure.

3. So, the values of epoch state (mark, set, go, also account states), used in
rewards calculation, reflect the ledger values, actual right after the corresponding
epoch is ended.

The order of functions evaluation is somewhat complicated, so values in the 
`RupdEnv` (and in subsequent call to `rupdTransition`) are updated
after the first slot of epoch start:

```
**** Transition: slot=SlotNo 14860753, epoch=EpochNo 231, reserves=Coin 12901374614727880
**** Transition: slot=SlotNo 14860800, epoch=EpochNo 232, reserves=Coin 12901374614727880
**** Transition: slot=SlotNo 14860801, epoch=EpochNo 232, reserves=Coin 12879420804989068
```

Reward accounts filtering
-------------------------

When rewards are paid to accounts, some payments are filtered out.
Here are the conditions.

The filters are the following:
1. 
2. 
3. The account must be registered as reward account, at the moment of reward
calculation. The information about actual rewards is kept in DState data
structure (a part of EpochState ledger info field).

3. DState data structure for filtering rewards accounts
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is a structure, which keeps info about delegates, reward accounts etc ---
some collection of info, joined from other places.

If one applies `rewards` function to it: `rewards ds`, then he/she receives
the map from reward accounts. By treating it like a set one may check, whether
the key could be considered a registered reward key.

The key question is when `ds` is calculated and what it contains.
This structure is updated in epoch transitions, and at the following places:

1. No certificates situation (some transition state?)

```drainWithdrawals :: DState era -> Withdrawals -> DState era
drainWithdrawals dState (Withdrawals wdrls) =
  dState {dsUnified = rewards dState UM.⨃ drainedRewardAccounts}
  where
    drainedRewardAccounts =
      Map.foldrWithKey
        ( \(RewardAccount _ cred) _coin ->
            Map.insert cred (UM.RDPair (UM.CompactCoin 0) (UM.CompactCoin 0))
            -- Note that the deposit (CompactCoin 0) will be ignored.
        )
        Map.empty
        wdrls```

If we have withdrawals from unregistered reward accounts, we add them to (registered) reward accounts -- delegsTransition, at Rules/Delegs.hs.
It happens when there are no certificates:

```  case certificates of
    Empty -> do
      let dState = certState ^. certDStateL
          withdrawals = tx ^. bodyTxL . withdrawalsTxBodyL
      validateTrans WithdrawalsNotInRewardsDELEGS $
        validateZeroRewards dState withdrawals network
      pure $ certState & certDStateL .~ drainWithdrawals dState withdrawals```

2. Normal update: while parsing certificate of registration 
(function delegationTransition in Shelley/Rules/Deleg.hs:

```    RegTxCert hk -> do
      -- (hk ∉ dom (rewards ds))
      UM.notMember hk (rewards ds) ?! StakeKeyAlreadyRegisteredDELEG hk
      let u1 = dsUnified ds
          deposit = compactCoinOrError (pp ^. ppKeyDepositL)
          u2 = RewDepUView u1 UM.∪ (hk, RDPair (UM.CompactCoin 0) deposit)
          u3 = PtrUView u2 UM.∪ (ptr, hk)
      pure (ds {dsUnified = u3})

    DelegStakeTxCert hk dpool -> do
      -- note that pattern match is used instead of cwitness and dpool, as in the spec
      -- (hk ∈ dom (rewards ds))
      UM.member hk (rewards ds) ?! StakeDelegationImpossibleDELEG hk

      pure (ds {dsUnified = delegations ds UM.⨃ Map.singleton hk dpool})```

And if key is not registered, we cannot delegate stake.

3. The structure is filtered at the end of epoch (against retired/deregistered stakes) -- 
at poolreap event (poolreap.hs)

So, we can conclude that `rewards ds` contains all registered reward addresses
at the moment of `EpochState` calculation.

Calculation of sigma
--------------------

Parameter 'c' in 'rewardStakePoolMember':

Seems that they take some portion of balance:
```        -- takes n elements of balance (keys-vectors)
        let !(steps, !balance') = VMap.splitAt n balance
```

And then calculate rewards for this small subset of the balance (presumably of the length n):
```
                        --- foldlWithKey: function (accum, key, vector) -> accum; accum; keys-vectors
            ans' = VMap.foldlWithKey (rewardStakePoolMember free) ans steps
```

```
rewardStakePoolMember ::
  FreeVars ->
  RewardAns ->
  Credential 'Staking ->
  CompactForm Coin ->
  RewardAns
```

(rewardStakePoolMember free) takes RewardAns accumulator (rewards paid so far), and then it's
supplied with two curried parameters: current key and current pool balance (taken as pair of values
from balance: 'key-coin' vector).

Then this function is wrapped into pulser, and then --- abstracted.
Pulser datatype has RLSP constructor:

```  RSLP ::
    (ans ~ RewardAns, m ~ ShelleyBase) =>
    !Int ->
    !FreeVars ->
    !(VMap.VMap VMap.VB VMap.VP (Credential 'Staking) (CompactForm Coin)) ->
    !ans ->
    RewardPulser m ans```

The second parameter contains global variables.
The third parameter is the balance (vector of pairs, credential and value).
The fourth parameter contains current results.

Finally, the enclosing pulser datatype 
is constructed in `startStep` function trailing construction:

```
      pulser =
        RSLP
          pulseSize
          free
          (unStake stake)
          (RewardAns Map.empty Map.empty)
```

And stake comes from here:

```
  let SnapShot stake delegs poolParams = ssStakeGo ss
```

Parameters updates timing and rewards calculation
-------------------------------------------------

Another important question: what are actual parameters used for rewards calculation?
Let's discuss example with decentralisation_constant updates (d).

This is excerpt from the governance info in Mainnet blockchain:

```
[6723005,213,1,0,[[213,[["Fi+UVUrIwiU4OiJIwkVlntqHDqqC0O8l/H3Ngg==",{"decentralisation_constant":[19,25]}],...
[7068441,213,1,0,[[214,[["Fi+UVUrIwiU4OiJIwkVlntqHDqqC0O8l/H3Ngg==",{"decentralisation_constant":[37,50]}],...
```

If I correctly understand, how these data should be read:

19/25 should be voted in 213, known at 213 + 6/10, and enacted in 214 (=213+1).
Next value (37/50) is enacted in 215, so value d=19/25 is actual just for one epoch.
Let's see how it's used in rewards calculation.

So, how Haskell node works with that: it seems that Haskell computes rewards in epoch 214 for epoch 213 (to be applied in epoch 215):

```
**** startStep computation: epoch=EpochNo 213, stake=Stake {unStake = fromList [...]}, 
     blocksMade=4230, k=NonZero {unNonZero = 2160}, reserves=13247093198353459, deltaR1=Coin 39741279595060, 
     d=4 % 5, expectedBlocks=4320, blocksMade=4230, eta=1 % 1, rPot=39746857813339, deltaT1=7949371562667,
     _R=Coin 31797486250672, activeStake=Coin 12106602864871837, stakePerPool=fromList [...]
**** startStep computation: epoch=EpochNo 214, stake=Stake {unStake = fromList [...]}, 
     blocksMade=4625, k=NonZero {unNonZero = 2160}, reserves=13230232787944838, deltaR1=Coin 38629941063285, 
     d=39 % 50, expectedBlocks=4752, blocksMade=4625, eta=4625 % 4752, rPot=38637041656541, deltaT1=7727408331308, 
     _R=Coin 30909633325233, activeStake=Coin 12758829109784350, stakePerPool=fromList [...]
**** startStep computation: epoch=EpochNo 215, stake=Stake {unStake = fromList [...]}, 
     blocksMade=5120, k=NonZero {unNonZero = 2160}, reserves=13212986170770203, deltaR1=Coin 39149588654133, 
     d=19 % 25, expectedBlocks=5184, blocksMade=5120, eta=80 % 81, rPot=39157090487879, deltaT1=7831418097575, 
     _R=Coin 31325672390304, activeStake=Coin 13382718156097189, stakePerPool=fromList [...]
```

So, that we see that d=19/25 is actually used in rewards calculation in epoch 214 (one epoch later the parameters were
enacted). But parameter 19/25 was actual at epoch 213 only (the Mark epoch).

Now, let's go to code. Haskell computes 'd' value for rewards calculation like this 
(eras/shelley/impl/src/Cardano/Ledger/Shelley/LedgerState/PulsingReward.hs):

```
startStep epochNo slotsPerEpoch b@(BlocksMade b') es'@(EpochState acnt ls' ss nm) maxSupply asc secparam =
      pr = es' ^. prevPParamsEpochStateL
      d = unboundRational (pr ^. ppDG)
```

It could be *guessed* that a previous parameter set for current epoch is used (that is: parameter set for epoch 213 
with current epoch 214): since they use prevPParamsEpochStateL method
