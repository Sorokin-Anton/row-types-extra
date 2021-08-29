{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE DataKinds #-}
module Data.Row.Tuples where

import Data.Row.Records
    ( KnownSymbol, Rec, type (.+), (.==), type (.==), Label, (.+) )
import Language.Haskell.TH
    ( newName,
      varE,
      varT,
      Q,
      Pat(VarP, TupP),
      Type(TupleT, VarT, AppT),
      Dec )
import Control.Monad (replicateM, zipWithM)



type family LabelF l a = r | r -> a





class MultiLabel l a where
  label :: l -> LabelF l a


type instance  LabelF (Label l) a = a -> Rec ( l .== a )
instance KnownSymbol l =>  MultiLabel (Label l) a where
  label l a = l .== a


-- Example: see `Constraint trick` for understanding
-- This can't compile without braces in `a .== b .+ (c .== d .+ e .== f)` cause asymettric constraint in `.+`

-- type instance  LabelF (Label l1, Label l2, Label l3) (a1 , a2, a3) = (a1, a2, a3) -> Rec ( l1 .== a1 .+ (l2 .== a2 .+ l3 .== a3))
-- instance ((KnownSymbol l1,
--            KnownSymbol l2, KnownSymbol l3),
--           x ~ (a1, a2, a3),
--           (Label l1 ~ ll1,
--            Label l2 ~ ll2,
--            Label l3 ~ ll3)) =>
--          MultiLabel (ll1, ll2, ll3) x where
--   label
--     (ll1, ll2, ll3)
--     (aa1, aa2, aa3)
--     = ((ll1 .== aa1)
--          .+
--            ((ll2 .== aa2)
--               .+ (ll3 .== aa3)))


mkMultilabelTupleInstance :: Int -> Q [Dec]
mkMultilabelTupleInstance len = do

  tupleVars <- fmap VarT <$> replicateM len (newName "tupleElem")
  labelSymbols <- replicateM len (newName "labelSymbol")
  labelNames <- fmap VarT <$> replicateM len (newName "label")
  x <- VarT <$> newName "x"
  tupleVarNames <- replicateM len (newName "tupleVar")
  labelVarNames <- replicateM len (newName "labelVar")

  let tupleConstr = foldl  AppT (TupleT len) tupleVars

      labelTuple = foldl  AppT (TupleT len) labelNames

      labelVarsPattern = TupP (map VarP labelVarNames)

      tupleVarsPattern = TupP (map VarP tupleVarNames)

      expr = foldr1 (\a b -> [| $a .+ $b |] ) $ zipWith (\l e -> [|$(varE l) .== $(varE e)|]) labelVarNames tupleVarNames

      typeLevelRecords = foldr1 (\a b -> [t| $a .+ $b |] ) $ zipWith (\l e -> [t| $(varT l) .== $(varT e) |]) labelSymbols tupleVarNames

      tupleConstrForTF =foldl  AppT (TupleT len) (map VarT tupleVarNames)

  ks <- mapM (\s -> [t| KnownSymbol $(varT  s) |]) labelSymbols
  ls <- mapM (\s -> [t| Label $(varT s) |]) labelSymbols
  es <- zipWithM (\s n -> [t| Label $(pure s) ~ $(pure n) |]) (map VarT labelSymbols) labelNames

  let constraintKnownSymbols = foldl  AppT (TupleT len) ks
      labelSymbolsEquals = foldl  AppT (TupleT len) es
      lst = foldl  AppT (TupleT len) ls


  [d|
    type instance LabelF $(pure lst) $(pure tupleConstrForTF) = $(pure tupleConstrForTF) -> Rec $typeLevelRecords
    instance ($(pure constraintKnownSymbols) , $(pure x) ~ $(pure tupleConstr), $(pure labelSymbolsEquals)) =>  MultiLabel $(pure labelTuple) $(pure x) where
      label $(pure labelVarsPattern) $(pure tupleVarsPattern) = $expr |]
