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
{-# LANGUAGE DataKinds #-}

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
import Plutarch.Builtin as BI
import Plutarch.Lift
import PlutusCore (DefaultFun(..))
import Plutarch.Unsafe
import Control.Monad
import PlutusTx
import Plutarch
-- import Plutarch.Docs
-- import qualified Plutarch.Monadic as P
-- import Plutarch.Docs.Run (evalWithArgsT)

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

------- Orig Haskell type
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
pcrowdValidator = phoistAcyclic $ plam $ \dat redeemer ctx -> unTermCont $ do 
  ctxF <- pletFieldsC @'["txInfo", "purpose"] ctx
  PSpending ((pfield @"_0" #) -> ownRef) <- pmatchC ctxF.purpose
  infoF <- pletFieldsC @'["inputs", "outputs", "signatories", "data"] ctxF.txInfo
  -- inputsAll <- pconstant infoF.inputs
  let ownInput = ptryOwnInput # infoF.inputs # ownRef 
  ownInputF <- pletFieldsC @'["value", "address"] ownInput   
  sigs :: Term _ (PBuiltinList (PAsData PPubKeyHash)) <- pletC infoF.signatories
  datF <- pletFieldsC @'["beneficiary", "deadline", "aCurrency", "aToken", "targetAmount", "actualtargetAmountsoFar", "contributorsMap" ] dat
  -- in popaque $ psignedByBeneficiary # ourDatum # ourRedeemer # ctx 
   
  pure $
    pif
        (pmatch redeemer $ \case
          Close _ -> 
            -- get the beneficiary signatory in signatories list
            pelem # datF.beneficiary # sigs
            -- (pconstant ())
          Contribute _ ->

-- --    validation#1     - First haskell in comments
--       traceIfFalse "wrong input value" ( correctInputValue d )       -- NEED TO ADD Policy id cannot be blank.
      -- correctInputValue :: Dat -> Bool
      -- correctInputValue dt = checkInputFound getAllValuesTxIns (tokenValue <> Ada.lovelaceValueOf  (( amountInDatum (contributorsMap dt) )) <> minAda) (totalValueDatumTxin )  
--          This validates 3 parameters to be equal 
--             1st parameter - from actual Tx-ins Values , validates that the UTXO with NFT in its Values - bypasses other Tx-in w/o NFT like Fees Tx-in
--             2nd parameter - Values constructed based on Datum passed to the Validator
--             3rd parametr - Values constructed from Datum at the UTXO. 
            -- (pfilter # plam (\x -> pelem # x # datF.approvedSignatories)  


            perror
        )
    -- pif 
    --   (pmatch redeemer $ \case
    --       Close _ -> 
    --         pconstant True
    --       Contribute _ ->
    --         perror
    --   )
        (pconstant () )
        perror


-- Haskell 
--          && traceIfFalse "Not signed by beneficiary" signedByBeneficiary
      -- signedByBeneficiary :: Bool
      -- signedByBeneficiary = Contexts.txSignedBy txinfo $ Ledger.unPaymentPubKeyHash (beneficiary d)



pcrowdValidatorW :: ClosedTerm (PValidator)
pcrowdValidatorW = plam $ \datum redeemer ctx -> unTermCont $ do 
  (dat, _) <- ptryFromC @PDat datum 
  (redmr, _) <- ptryFromC @PRedeem redeemer 
  pure $ popaque $ pcrowdValidator # dat # redmr # ctx



-- getAllInputs :: Term s (PList PTxInInfo :-->  PList PTxOut)
-- -- pluList2 = pmap # (plam $ \x -> x + x3) # (pluList)
-- getAllInputs = phoistAcyclic $
--     plam $ \inp -> 
--       pmap # (\self x -> pletFields @'["resolved"] x) # inp
-- getAllInputs = phoistAcyclic $
--   plam $ \inputs -> pmap # (\res -> res.resolved)  # (pmap # \inp -> pletFields @'["outRef", "resolved"] inp # inputs)
-- --     precList (\self x xs -> pletFields @'["outRef", "resolved"] x $ \txInFields -> pif (ownRef #== txInFields.outRef) txInFields.resolved (self # xs)) (const perror) # inputs





--------------------------------------------------------------------------------------------------------------

--------------------------------------------------------------- simple TESTING plutarch while learning--------------------------------------
--------------------------------------------------------------------------------------------------------------

--------------------------------------------------------------------------------------------------------------
x3 :: Term s PInteger
x3 = pconstant 3

liftedX3 :: Integer
liftedX3 = plift x3

doubleH :: Integer -> Integer
doubleH i = i * 2

pluDoubleH :: Term s ( PInteger :--> PInteger )
pluDoubleH =  phoistAcyclic $ 
  plam $ \i -> 
    i


doubleX3 =  doubleH (liftedX3)
-- ghci> doubleX3
-- 6

-- doubleX32 :: Term s PInteger -> Integer
-- doubleX32 x =  doubleH (plift x)

-- ghci> doubleH (liftedX3)
-- 6

-- funcPinToInt :: Term s PInteger -> Integer
-- funcPinToInt pi = (plift pi)

pluList :: Term s (PList PInteger)
pluList = pcons # 2 #$ pcons # 6 #$ pcons # 12 # pnil

pluNullCheck = pnull # pluList
-- ghci> plift pluNullCheck 
-- False
pluLen = plength # pluList
-- ghci> plift pluLen
-- 3

hpList :: [Term s PInteger]
hpList = [x3, x3, x3]

-- pluToHList ::[ Term s PInteger] -> [Integer]
-- pluToHList [] = []
-- pluToHList [x] = [(plift x)]
-- pluToHList (x:xs) = (plift x) : pluToHList xs

headHpList = head hpList

-- pluToHlist = fmap (plift) hpList

hList :: [Integer]
hList = [1, 2, 3]

hToHList :: [Integer] -> [Integer]
hToHList [] = []
hToHList [x] = [x * x]
hToHList (x:xs) = (x* x) : hToHList xs



-- ghci> hToHList hList
-- [1,4,9]


-- pluToHaskList :: Term s (PList PInteger) :: [Term s PInteger]
-- pluToHaskList = pfix #$ plam f
--   where
--     f :: Term s (PInteger :--> PInteger) -> Term s PInteger -> Term s PInteger
--     f self n = pif (n #== 1) n $ n * (self #$ n - 1)




-- hList :: Term s (PList PInteger) :--> [Integer]
-- hList = phoistAcyclic $ plam $ \plist2612 ->
--   pmatch plist2612 $ \case
--     precList
--       (\self x xs) ->
--           pmatch x \case
--             PCons pi -> lift pi
--             PNil -> 0
--             (self # xs)
--       )
--       (const perror)
--       # pto  plist2612

-- psingletonTokenNameWithCS = phoistAcyclic $ plam $ \policyId val ->
--   pmatch val $ \(PValue val') ->
--     precList 
--       (\self x xs ->
--         pif 
--           (pfstBuiltin # x #== policyId)
--           ( pmatch (pfromData (psndBuiltin # x)) $ \(PMap tokens) ->
--               plet (phead # tokens) $ \fstTk ->
--                 (pif 
--                   (pnull # (ptail # tokens) #&& pfromData (psndBuiltin # fstTk) #== 1)
--                   (pfromData $ pfstBuiltin # fstTk)
--                   perror 
--                 )
--           )
--           (self # xs)
--         )
--       (const perror)
--       # pto val'




list4 :: [Integer]
list4 = [1,2,3,4]

-- plenList4 :: [Integer] :--> PInteger
-- plenList4 = plength list4

-- someListFunc :: Terms s (PList PInterger) -> [Integer]
-- someListFunc li = 




pluHead = phead # pluList
x = plift pluHead
-- ghci> plift pluHead
-- 2


-- funcAdd :: Integer -> Integer 
-- funcadd i = i + 5

pluList2 :: Term s (PList PInteger)
pluList2 = pmap # (plam $ \x -> x + x3) # (pluList)
-- ghci> plift (phead # pluList2)
-- 5

-- lenP = pmap # (\x -> pdiv # x # 2) # (pluList)


pFeList :: forall s. Term s (PList PByteString)
pFeList = pcon $ PSCons (phexByteStr "fe") $ pcon PSNil
pFeListHead = phead # pFeList


l1 :: [Integer]
l1 = [1,2,3,4]
l2 = fmap (\y -> y * 3) (fmap (\x -> x + 10) l1)



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


-- exampleFunction :: Term s (PInteger :--> PInteger)
-- exampleFunction = plam $ \x -> unTermCont $ do
--   x' <- plet $ x + 1 + 2 + 3
--   x' + x'

x' :: Term s PInteger
x' = pconstant 3

x'' :: Term s PInteger
x'' = pconstant 5

x''' = x'' + x'


-- scr1 :: Term s PScriptContext
-- ctxF <- pletFieldsC @'["txInfo", "purpose"] ctx





-- crowdCurrSymbBuilt = pdata crowdCurrSymb


-- crowdName :: (Term s PByteString)
-- crowdName = pconstant "CrowdFundingToken"
-- crowdNameBuilt = pdata crowdName
-- ghci> plift crowdName
-- "CrowdFundingToken"



-- crowdTokenNameBuilt = pdata crowdTokenName

crowdNTokens :: Term s PInteger
crowdNTokens = pconstant 5
crowdNTokensBuilt = pdata crowdNTokens
-- keys1 :: KeyGuarantees
-- keys1 = Sorted

-- -- crowdPair = (PBuiltinPair crowdTokenName crowdNTokens)
-- crowd1 = PMap crowdTokenName crowdNTokens
-- crowd1 = ppairDataBuiltin # ( crowdTokenName)  # (pdata crowdNTokensBuilt)
-- crowd1 = ppairData # ( crowdTokenName)  # (pdata crowdNTokens)


-- pluList2 :: Term s (PList PInteger)
-- pluList2 = pmap # (plam $ \x -> x + x3) # (pluList)

-- built1 :: Term s (PBuiltinPair PInteger PInteger)
-- built1 =  (PBuiltinPair crowdNTokens crowdNTokens)

-- data PInteger s

-- pluList :: Term s (PList PInteger)
-- pluList = pcons # 2 #$ pcons # 6 #$ pcons # 12 # pnil





int2 :: Term s PInteger
int2 = pconstant 5

int3 = pconstantData @PInteger 3
int4 = pconstantData @PInteger 4


-- data PBuiltinPair (a :: PType) (b :: PType) (s :: S)
-- built2 :: Term s ( PInteger , PInteger)
-- built2 =  PBuiltinPair # int1 # int2

int1ToPas = pdata int1
built3 :: Term s (PBuiltinPair (PAsData PInteger) (PAsData PInteger))
built3 = ppairDataBuiltin # int3 # int4
fst3 = pfstBuiltin # built3
snd3 = psndBuiltin # built3


builtPairList :: forall s. Term s (PBuiltinList (PBuiltinPair (PAsData PInteger) (PAsData PInteger)))
builtPairList = pcon $ PCons built3 $ pcon PNil



emptyPairList :: forall s. Term s (PBuiltinList (PBuiltinPair (PAsData PInteger) (PAsData PInteger)))
emptyPairList = pcon PNil

-- pmatch using the full list for testing
matchOnPairList :: forall {s :: S}. Term s PInteger
matchOnPairList = pmatch (pcon $ PCons built3 $ pcon PNil) $ \case
  PNil -> perror
  PCons pb _ -> (pfromData (pfstBuiltin # pb))
-- ghci> plift matchOnPairList 
-- 3

-- pmatch on variable instead of writing it out
matchOnPairList1 = pmatch (builtPairList) $ \case
  PNil -> perror
  PCons pb _ -> (pfromData (pfstBuiltin # pb))
-- ghci> plift matchOnPairList1
-- 3

-- pmatch but do from data after you get result and then plist to print it.
matchOnPairList2 = pmatch (builtPairList) $ \case
  PNil -> perror
  PCons pb _ -> (pfstBuiltin # pb)
-- ghci> plift (pfromData matchOnPairList2)
-- 3


matchOnList :: forall s. Term s PString
matchOnList = pmatch (pcon $ PCons (phexByteStr "fe") $ pcon PNil) $ \case
  PNil -> "hey hey there's nothing here!"
  PCons _ _ -> "oooo fancy!"
-- ghci> plift matchOnList 
-- "oooo fancy!"
-- pairList = pfromData builtPairList

threePdata :: Term s (PAsData PInteger)
threePdata = ( pdata 3 ) 

builtInt34pdata :: forall s. Term s (PBuiltinList (PAsData PInteger))
builtInt34pdata = pcon $ PCons (pdata 3) $ pcon PNil

builtInt34 :: forall s. Term s (PBuiltinList (PAsData PInteger))
builtInt34 = pcon $ PCons int3 $ pcon PNil

listOfBs :: forall s. Term s (PBuiltinList (PAsData PByteString))
listOfBs = pcon $ PCons (pdata $ phexByteStr "fe") $ pcon PNil

-- builtList :: forall s. Term s (PBuiltinList (PBuiltinPair (PAsData PInteger) (PAsData PInteger)))
-- builtList = BI.PCons # BI.PCons built3 
-- builtListHead = phead # builtList
-- pluList :: Term s (PList PInteger)
-- pluList = pcons # 2 #$ pcons # 6 #$ pcons # 12 # pnil


int3FromPas = pfromData fst3
-- ghci> plift int3FromPas 
-- 3

--  Below wont work since PBuiltinPair expects PAsData thats the main reason you struggled a lot - you kept using regular plutarch type.
-- built4 :: forall {s :: S}.
--      Term s (PBuiltinPair (PInteger) (PInteger))
-- built4 = ppairDataBuiltin # int1 # int2
-- src/CrowdFundingOnChain.hs:389:29: error:
--     • Couldn't match type ‘PInteger’ with ‘PAsData a0’
--       Expected: Term s (PAsData a0)
--         Actual: Term s PInteger
--     • In the second argument of ‘(#)’, namely ‘int1’
--       In the first argument of ‘(#)’, namely ‘ppairDataBuiltin # int1’
--       In the expression: ppairDataBuiltin # int1 # int2
--     |
-- 389 | built4 = ppairDataBuiltin # int1 # int2



-- ghci> :t built3
-- built3
--   :: forall {s :: S}.
--      Term s (PBuiltinPair (PAsData PInteger) (PAsData PInteger))




crowdAddress :: (Term s PByteString)
crowdAddress = pconstant "CrowdFundingSomeAddress"
crowdAddressBuilt = pdata crowdAddress

crowdCurrSymb :: forall {s :: S}. PCurrencySymbol s
crowdCurrSymb = PCurrencySymbol crowdName

crowdName :: (Term s PByteString)
crowdName = pconstant "CrowdFundingToken"
crowdNameBuilt = pdata crowdName

crowdTokenName :: forall {s :: S}. PTokenName s
crowdTokenName = PTokenName crowdName

int1 :: Term s PInteger
int1 = pconstant 12
-- ghci> plift (fib # int1)
-- 144


-- my1Val :: forall 
--     (keys :: KeyGuarantees)
--     (amounts :: AmountGuarantees)
--     (s :: S). PValue keys amounts s
-- my1Val = PValue ((PMap crowdCurrSymb  (PMap crowdTokenName int1)))	

-- PValue constructor - 
-- PValue (Term s (PMap keys PCurrencySymbol (PMap keys PTokenName PInteger)))

-- x1Map = (PMap (int1 int1)

------ My own breakdown below for preclist - 
psingletonTokenNameWithCS :: 
  forall 
    (keys :: KeyGuarantees)
    (amounts :: AmountGuarantees)
    (s :: S). 
  Term s (PAsData PCurrencySymbol :--> PValue keys amounts :--> PTokenName)
psingletonTokenNameWithCS = phoistAcyclic $ plam $ \policyId val ->
  pmatch val $ \(PValue val') ->
    precList 
      (\self x xs ->                                                                     -- this one takes the PValue which is map of CurrSymb and then map of token name qty.
        pif                                                                              -- pif -
          (pfstBuiltin # x #== policyId)                                                 --       next line is the condition
          ( pmatch (pfromData (psndBuiltin # x)) $ \(PMap tokens) ->                     --       next line is the Then when condition is true, Meaning we found policy id
              plet (phead # tokens) $ \fstTk ->                       --  since policy id matched now we look at tokens and qty. we do a let for variables. We set head 1st element
                (pif                                                                     -- Next if stmt
                  (pnull # (ptail # tokens) #&& pfromData (psndBuiltin # fstTk) #== 1)   -- since for this CurrSYm/Policy we expect only 1 (not more than 1) token , tail needs to be Null.
                  (pfromData $ pfstBuiltin # fstTk)                                      -- above condition is true we return Token Name since its 1st part (2nd part is qty)
                  perror                                                                 -- ELSE error since either qty >1 or Has some other tokens etc.. 
                )
          )
          (self # xs)                                                                     -- ELSE for 1st If. Policy id did not match so looks for remaining list. 
        )
      (const perror)
      # pto val'



-- https://plutonomicon.github.io/plutarch-plutus/Usage/Recursion

pfac :: Term s (PInteger :--> PInteger)
pfac = pfix #$ plam f
  where
    f :: Term s (PInteger :--> PInteger) -> Term s PInteger -> Term s PInteger
    f self n = pif (n #== 1) n $ n * (self #$ n - 1)

--- my own understanding - 
--- pfac 5
--- 5 * (pfac 4)
--- 5 * ( 4 * (pfac 3))
--- 5 * (4 * (3 * (pfac 2)))
--- 5 * (4 * (3 * ( 2* (pfac 1))))
--- 5 * (4 * (3 * ( 2* ( 1))))
--- 120


fib :: Term s (PInteger :--> PInteger)
fib = phoistAcyclic $ 
  pfix #$ plam 
    (\self n ->
     pif (n #== 0) 0 $ 
       pif (n #== 1) 1 $
         (self # (n - 1) + self # (n-2))
    )



-- pasConstr :: Term s (PData :--> PBuiltinPair PInteger (PBuiltinList PData))
-- pasConstr = punsafeBuiltin UnConstrData

constructorIdOf :: Term s (PData :--> PInteger)
constructorIdOf = plam $ \x -> pfstBuiltin #$ pasConstr # x

fieldsOf :: Term s (PData :--> PBuiltinList PData)
fieldsOf = plam $ \x -> psndBuiltin #$ pasConstr # x


-- https://gist.github.com/MangoIV/3bbe1f029805f0d829248f00c2cfc4a2
-- c1 = constructorIdOf evalWithArgsT [toData (Nothing :: Maybe ())]

-- evalWithArgsT :: ClosedTerm a -> [Data] -> Either Text (Script, ExBudget, [Text])
-- evalWithArgsT x args = do
--   cmp <- compile def x
--   let (escr, budg, trc) = evalScript $ applyArguments cmp args
--   scr <- first (pack . show) escr
--   pure (scr, budg, trc)