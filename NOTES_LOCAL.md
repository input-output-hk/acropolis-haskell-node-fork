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

Reward accounts filtering
=========================

When rewards are paid to accounts, some payments are filtered out.
Here are the conditions.

The filters are the following:
1. 
2. 
3. The account must be registered as reward account, at the moment of reward
calculation. The information about actual rewards is kept in DState data
structure (a part of EpochState ledger info field).

DState data structure for filtering rewards accounts
----------------------------------------------------

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
