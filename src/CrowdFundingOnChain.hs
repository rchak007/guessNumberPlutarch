{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}



-- 2. imports external/imports

module CrowdFundingOnChain (pcrowdValidatorW) where

-- from DAOValidator
import Plutarch.Api.V2 
import Plutarch.Api.V1 (PCredential (PPubKeyCredential, PScriptCredential))
import Plutarch.DataRepr
import Plutarch.Api.V1.Value 
import Plutarch.Bool
import Plutarch.Prelude
import Plutarch.Extra.ScriptContext (ptryFromInlineDatum, pfromPDatum)
import Utils (ppositiveSymbolValueOf, (#>), (#>=))
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont 

-- import Plutarch
-- import Plutarch.Prelude
-- import Plutarch.Api.V1 (
--   PCredential (PPubKeyCredential, PScriptCredential),
--  )
-- import Plutarch.Api.V2 
-- import Plutarch.DataRepr
-- import "liqwid-plutarch-extra" Plutarch.Extra.TermCont 
-- -- (
-- --   pletC,
-- --   pletFieldsC,
-- --   pmatchC,
-- --   ptryFromC
-- --  )
-- import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (..))
-- import Plutarch.Unsafe (punsafeCoerce)
-- import PlutusTx qualified

-- import Plutarch.Api.V2 
-- import Plutarch.Api.V1 (PCredential (PPubKeyCredential, PScriptCredential), PPubKeyHash, PPOSIXTime)
-- import Plutarch.DataRepr
-- import Plutarch.Api.V1.Value 
-- import Plutarch.Prelude
-- import Plutarch.Extra.IsData (EnumIsData (..), PlutusTypeEnumData)
-- import Plutarch.Extra.Value (psymbolValueOf')
-- import "liqwid-plutarch-extra" Plutarch.Extra.TermCont 

-- import qualified Prelude                                as P
-- import qualified Plutus.Script.Utils.V2.Typed.Scripts.Validators as Scripts
-- import qualified Plutus.Script.Utils.V2.Scripts as V2UtilsScripts
-- import qualified Plutus.V2.Ledger.Contexts                       as Contexts
-- import qualified Plutus.V2.Ledger.Api                            as LedgerApiV2
-- import qualified Plutus.V1.Ledger.Interval                            as LedgerIntervalV1
-- import qualified Plutus.V1.Ledger.Value                                as V1Value
-- import qualified Ledger                                          
-- import qualified PlutusTx
-- import PlutusTx.Prelude
-- import qualified Ledger.Ada                                      as Ada
-- -- import qualified Ledger.Constraints.TxConstraints                as Tx
-- import qualified Ledger.Value                                      as Value
-- import qualified Plutus.V2.Ledger.Tx                             as V2LedgerTx
-- import qualified Data.ByteString.Char8                   as B
-- import qualified Data.ByteString.Base16                  as B16
-- import qualified PlutusTx.Prelude                        as PlutusPrelude 



-- Reference
-- data PDaoDatum (s :: S) = 
--    PDaoDatum (Term s (PDataRecord '["approvedSignatories" ':= PBuiltinList (PAsData PPubKeyHash), "requiredNoOfSigs" ':= PInteger]))
                    -- I think PDataRecord is a list 
--  deriving stock (Generic)
--  deriving anyclass (PlutusType, PIsData, PDataFields)

data PDat (s :: S) = 
    PDat 
      ( Term 
          s 
          ( PDataRecord 
          '["beneficiary" ':= (PAsData PPubKeyHash)
          , "deadline" ':= PPOSIXTime
          , "aCurrency" ':= PCurrencySymbol
          , "aToken" ':= PTokenName
          , "targetAmount" ':= PInteger
          , "actualtargetAmountsoFar" ':= PInteger
          , "contributorsMap" ':= PBuiltinList (PAsData (PTuple (PAsData PPubKeyHash) PInteger)) ] ))
 deriving stock (Generic)
 deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType PDat where
  type DPTStrat _ = PlutusTypeData 

instance PTryFrom PData (PAsData PDat)
instance PTryFrom PData PDat 
  
-- from -- https://github.com/Plutonomicon/plutarch-plutus/blob/master/plutarch-docs/Types/PDataSum%20and%20PDataRecord.md
-- newtype Foo (s :: S) = Foo (Term s (PDataRecord '["fooField" ':= PInteger]))  

-- data PDat (s :: S) = PDat 
--     {
--         -- beneficiary :: Ledger.PaymentPubKeyHash
--         -- , deadline :: LedgerApiV2.POSIXTime
--         -- , aCurrency :: LedgerApiV2.CurrencySymbol
--         -- , aToken    :: LedgerApiV2.TokenName
--         targetAmount :: Term s PInteger 
--         , actualtargetAmountsoFar :: Term s PInteger 
--         -- , contributorsMap :: [(Ledger.PaymentPubKeyHash,Integer)] 
--     } 
--      -- deriving stock (Generic, Enum, Bounded)
--      deriving stock (Generic)
--      deriving anyclass (PlutusType, PIsData, PEq, PShow)

-- Haskell datum
-- data Dat = Dat 
--     {
--         beneficiary :: Ledger.PaymentPubKeyHash
--         , deadline :: LedgerApiV2.POSIXTime
--         , aCurrency :: LedgerApiV2.CurrencySymbol
--         , aToken    :: LedgerApiV2.TokenName
--         , targetAmount :: Integer
--         , actualtargetAmountsoFar :: Integer
--         , contributorsMap :: [(Ledger.PaymentPubKeyHash,Integer)] 
--     } deriving P.Show
-- PlutusTx.unstableMakeIsData ''Dat


-- instance DerivePlutusType PDat where
--   type DPTStrat _ = PlutusTypeEnumData 

-- instance PTryFrom PData (PAsData PDat)

-- PlutusTx.unstableMakeIsData ''Dat

        -- Keep hash of whole MAP - for now we will do with Map. 
        -- Later need to optimize since if we have 1000s ofcontributors its storage issue
            -- maybe we instead mint an NFT to them instead of storing them here etc. 


-- data Contribution 


-- data PRedeem (s :: S) = PContribute 
--     {
--         contribution ::  Term s PInteger   -- (Ledger.PaymentPubKeyHash,Integer)
--     } 
--               | PClose 
--  deriving stock (Generic)
--  deriving anyclass (PlutusType, PEq, PShow)

data PRedeem (s :: S) = 
      Contribute 
        ( Term 
          s 
          ( PDataRecord 
          '["contribution" ':= PInteger])) 
      | Close (Term s (PDataRecord '[]))
 deriving stock (Generic)
 deriving anyclass (PlutusType, PIsData, PEq, PShow)

instance DerivePlutusType PRedeem where
  type DPTStrat _ = PlutusTypeData 

instance PTryFrom PData (PAsData PRedeem)
instance PTryFrom PData PRedeem 

-- instance DerivePlutusType PRedeem where
--   type DPTStrat _ = PlutusTypeData 

-- instance PTryFrom PData (PAsData PRedeem)
-- instance PTryFrom PData PRedeem 

-- data PDaoAction (s :: S) = 
--     Add (Term s (PDataRecord '["pkh" ':= PPubKeyHash]))
--    | Remove (Term s (PDataRecord '["pkh" ':= PPubKeyHash]))
--    | Approve (Term s (PDataRecord '[]))
--  deriving stock (Generic)
--  deriving anyclass (PlutusType, PIsData, PEq, PShow)


-- instance DerivePlutusType PRedeem where
--   type DPTStrat _ = PlutusTypeEnumData 

-- instance PTryFrom PData (PAsData PRedeem)

-- PlutusTx.unstableMakeIsData ''Redeem

-- data Crowd
-- instance Scripts.ValidatorTypes Crowd where
--     type instance RedeemerType Crowd = Redeem
--     type instance DatumType Crowd = Dat

-- data Dt1 = Dt1 { 
--                    tAmount :: Integer 
--       } deriving P.Show


-- Define a function using plam
-- increment :: Term s pInteger -> Term s pInteger
-- increment = plam (\x -> x + 1)

-- -- 3. onchain code
-- {-# INLINABLE crowdValidator #-}
pcrowdValidator :: Term s  (PDat :--> PRedeem :--> PScriptContext :--> PUnit)
-- pcrowdValidator :: Term s PValidator 
pcrowdValidator = phoistAcyclic $ plam $ \_datum redeemer ctx -> unTermCont $ do 
  ctxF <- pletFieldsC @'["txInfo", "purpose"] ctx
  PSpending ((pfield @"_0" #) -> ownRef) <- pmatchC ctxF.purpose
  infoF <- pletFieldsC @'["inputs", "outputs", "signatories", "data"] ctxF.txInfo
  let ownInput = ptryOwnInput # infoF.inputs # ownRef 
  ownInputF <- pletFieldsC @'["value", "address"] ownInput   
  -- in popaque $ psignedByBeneficiary # ourDatum # ourRedeemer # ctx 
  pure $
        (pmatch redeemer $ \case
          Close _ -> 
            (pconstant () )
          Contribute _ ->
            perror
        )
    -- pif 
    --   (pmatch redeemer $ \case
    --       Close _ -> 
    --         pconstant True
    --       Contribute _ ->
    --         perror
    --   )
    --   (pconstant () )
    --   perror


pcrowdValidatorW :: ClosedTerm (PValidator)
pcrowdValidatorW = plam $ \datum redeemer ctx -> unTermCont $ do 
  (dat, _) <- ptryFromC @PDat datum 
  (redmr, _) <- ptryFromC @PRedeem redeemer 
  pure $ popaque $ pcrowdValidator # dat # redmr # ctx



ptryOwnInput :: (PIsListLike list PTxInInfo) => Term s (list PTxInInfo :--> PTxOutRef :--> PTxOut)
ptryOwnInput = phoistAcyclic $
  plam $ \inputs ownRef ->
    precList (\self x xs -> pletFields @'["outRef", "resolved"] x $ \txInFields -> pif (ownRef #== txInFields.outRef) txInFields.resolved (self # xs)) (const perror) # inputs


-- psignedByBeneficiary :: Term s (POurDatum :--> POurRedeemer :--> PScriptContext :--> PUnit)
-- psignedByBeneficiary = phoistAcyclic $ plam $ \datum redeemer _ctx -> unTermCont $ do 

      -- signedByBeneficiary :: Bool
      -- signedByBeneficiary = Contexts.txSignedBy txinfo $ Ledger.unPaymentPubKeyHash (beneficiary d)

alwaysSucceeds :: Term s (PAsData PDat :--> PAsData PRedeem :--> PAsData PScriptContext :--> PUnit)
alwaysSucceeds = plam $ \_datm _redm _ctx -> pconstant ()
-- pcrowdValidator = phoistAcyclic $ plam $ \datm redm ctx ->
--   (pconstant ())

-- pvalidateSmallChecks :: Term s (POurDatum :--> POurRedeemer :--> PScriptContext :--> PUnit)
-- pvalidateSmallChecks = phoistAcyclic $ plam $ \datum redeemer _ctx -> unTermCont $ do 
--     datumF <- pletFieldsC @'["password"] datum 
--     redeemF <- pletFieldsC @'["password"] redeemer 
--     pure $
--       pif ( (redeemF.password) #== datumF.password )
--       (pconstant ())
--       perror

-- emurgoValidator :: Term s (PCurrencySymbol :--> PDaoDatum :--> PDaoAction :--> PScriptContext :--> PUnit)
-- emurgoValidator = phoistAcyclic $ plam $ \stateCS dat redeemer ctx -> unTermCont $ do
--     ctxF <- pletFieldsC @'["txInfo", "purpose"] ctx


-- alwaysSucceeds :: Term s (PAsData PDatum :--> PAsData PRedeemer :--> PAsData PScriptContext :--> PUnit)
-- alwaysSucceeds = plam $ \datm redm ctx -> pconstant ()



-- foo :: Maybe PInteger -> PInteger
-- foo mb@(Maybe PInteger) = mb (\x -> x + 42) 0