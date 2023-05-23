{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module SmallValidator where

import Plutarch
import Plutarch.Prelude
import Plutarch.Api.V1 (
  PCredential (PPubKeyCredential, PScriptCredential),
 )
import Plutarch.Api.V2 
import Plutarch.DataRepr
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont (
  pletC,
  pletFieldsC,
  pmatchC,
  ptryFromC
 )
import Plutarch.Lift (PConstantDecl, PUnsafeLiftDecl (..))
import Plutarch.Unsafe (punsafeCoerce)
import PlutusTx qualified

--data OurDatum = OurDatum {password :: BuiltinByteString}

data POurDatum (s :: S) = POurDatum (Term s (PDataRecord '[ "password" ':= PByteString ]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType POurDatum where 
    type DPTStrat _ = PlutusTypeData 

instance PTryFrom PData POurDatum 

data POurRedeemer (s :: S) = POurRedeemer (Term s (PDataRecord '[ "password" ':= PByteString ]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields)

instance DerivePlutusType POurRedeemer where 
    type DPTStrat _ = PlutusTypeData

instance PTryFrom PData POurRedeemer  

-- validateSmallChecks :: OurDatum -> OurRedeemer -> ScriptContext -> () 
pvalidateSmallChecks :: Term s (POurDatum :--> POurRedeemer :--> PScriptContext :--> PUnit)
pvalidateSmallChecks = phoistAcyclic $ plam $ \datum redeemer _ctx -> unTermCont $ do 
    -- ctxF <- pletFieldsC @'["txInfo"] ctx 
    -- infoF <- pletFieldsC @'["signatories"] ctxF.txInfo 
    datumF <- pletFieldsC @'["password"] datum 
    redeemF <- pletFieldsC @'["password"] redeemer 
    pure $
      pif ( (redeemF.password) #== datumF.password )
      (pconstant ())
      perror

pvalidateSmallChecksW :: Term s PValidator 
pvalidateSmallChecksW = phoistAcyclic $ plam $ \datum redeemer ctx ->
    let ourDatum :: Term _ POurDatum 
        ourDatum = punsafeCoerce datum 
        ourRedeemer :: Term _ POurRedeemer 
        ourRedeemer = punsafeCoerce redeemer
     in popaque $ pvalidateSmallChecks # ourDatum # ourRedeemer # ctx 