{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module ASA.Domain.Service.TH (
    funcTH_transit
  , instanceTH_IAppState
  ) where

import qualified GHC.Base
import qualified GHC.Show
import qualified Control.Monad.Fail
import Control.Monad.Trans.State.Lazy
import Language.Haskell.TH
import Control.Monad
import qualified Data.Text as T

import ASA.Domain.Service.Type
import ASA.Domain.Service.Utility


-- |
--
{-

> stack ghci --ghci-options=-XTemplateHaskell --ghci-options=-XQuasiQuotes
*Main> :m +Language.Haskell.TH

:{
 runQ [d|
   instance IAppState StartingStateData where
     actionS s (RequestW r@EntryRequest{})      = action s r
   |]
:}

[InstanceD Nothing [] (AppT (ConT Type.IAppState) (ConT Type.StartingState))
  [FunD Type.doActivity
    [Clause [VarP s_8,ConP Type.RequestW [AsP r_9 (RecP Type.InitRequest [])]] (NormalB (AppE (AppE (VarE Type.action) (VarE s_8)) (VarE r_9))) [],
     Clause [VarP s_10,ConP Type.RequestW [AsP r_11 (RecP Type.TerminateRequest [])]] (NormalB (AppE (AppE (VarE Type.action) (VarE s_10)) (VarE r_11))) []
    ]
  ]
]

[InstanceD Nothing [] (AppT (ConT Type.IAppState) (ConT Type.StartingStateData))
  [FunD Type.actionS 
    [Clause [VarP s_0,ConP Type.RequestW [] [AsP r_1 (RecP Type.EntryRequest [])]] (NormalB (AppE (AppE (VarE Type.action) (VarE s_0)) (VarE r_1))) []
    ]
  ]
]


:{
 runQ [d|
   data Request r where
     InitRequest :: Request InitRequest
   |]
:}

[DataD [] Request_12 [PlainTV r_14] Nothing [GadtC [InitRequest_13] [] (AppT (ConT Request_12) (ConT Type.InitRequest))] []]

-}

instanceTH_IAppState :: Name -> Q [Dec]
instanceTH_IAppState stName = do
  ns <- getGadtsContNames ''Request
  clauseList <- mapM go ns
  return $ [InstanceD Nothing [] (AppT (ConT ''IAppState) (ConT stName)) [FunD 'actionS clauseList]]

  where
    -- |
    --
    go n = do
      s <- newName "s"
      r <- newName "r"
      return $ Clause [VarP s, ConP 'RequestW [] [AsP r (RecP n [])]] (NormalB (AppE (AppE (VarE 'action) (VarE s)) (VarE r))) []

    -- |
    --
    getGadtsContNames :: Name -> Q [Name]
    getGadtsContNames n = reify n >>= \case
      TyConI (DataD _ _ _ _ cs _) -> mapM go' cs
      x -> fail $ "[ERROR] can not get data constructor. " ++ show x

      where
        go' (GadtC [name] _ _) = return name
        go' x = fail $ "[ERROR] can not get gadts data constructor. " ++ show x


-- |
--
{-

> stack ghci --ghci-options=-XTemplateHaskell --ghci-options=-XQuasiQuotes --ghci-options=-XLambdaCase
*Main> :m +Language.Haskell.TH

:{
 runQ [d|
    transit :: StateTransition -> AppContext ()
    transit StartingToRunning = get >>= \case
      AppStateW StartingState -> changeTo $ AppStateW RunningState
      AppStateW x -> fail $ "invalid state transition. trans:" ++ show StartingToRunning ++ ", curSt:" ++ show x
    transit RunningToStopping = get >>= \case
      AppStateW RunningState -> changeTo $ AppStateW StoppingState
      AppStateW x -> fail $ "invalid state transition. trans:" ++ show StartingToRunning ++ ", curSt:" ++ show x
   |]
:}

[SigD transit_0 (AppT (AppT ArrowT (ConT Type.StateTransition)) (AppT (ConT Type.AppStateContext) (TupleT 0))),
  FunD transit_0 [
    Clause [ConP Type.StartingToRunning []]
     (NormalB (InfixE (Just (UnboundVarE get)) (VarE GHC.Base.>>=) (Just (LamCaseE [
        Match (ConP Type.AppStateW [ConP Type.StartingState []]) (NormalB (InfixE (Just (UnboundVarE changeTo)) (VarE GHC.Base.$) (Just (AppE (ConE Type.AppStateW) (ConE Type.RunningState))))) [],
        Match (ConP Type.AppStateW [VarP x_5])
          (NormalB (InfixE
            (Just (VarE GHC.Base.fail))
            (VarE GHC.Base.$)
            (Just (InfixE
              (Just (LitE (StringL "invalid state transition. trans:")))
                (VarE GHC.Base.++)
                (Just (InfixE
                  (Just (AppE (VarE GHC.Show.show) (ConE Type.StartingToRunning)))
                  (VarE GHC.Base.++)
                  (Just (InfixE
                    (Just (LitE (StringL ", curSt:")))
                      (VarE GHC.Base.++)
                      (Just (AppE (VarE GHC.Show.show) (VarE x_1)))
                  ))
                ))
            ))
          )) []
      ])))) [],
    Clause [
      ConP Type.RunningToStopping []] (NormalB (InfixE (Just (UnboundVarE get)) (VarE GHC.Base.>>=) (Just (LamCaseE [
        Match (ConP Type.AppStateW [ConP Type.RunningState []]) (NormalB (InfixE (Just (UnboundVarE changeTo)) (VarE GHC.Base.$) (Just (AppE (ConE Type.AppStateW) (ConE Type.StoppingState))))) [],
        Match (VarP x_2) (NormalB (InfixE (Just (VarE GHC.Base.fail)) (VarE GHC.Base.$) (Just (InfixE (Just (LitE (StringL "invalid state transition. trans:"))) (VarE GHC.Base.++) (Just (InfixE (Just (AppE (VarE GHC.Show.show) (ConE Type.StartingToRunning))) (VarE GHC.Base.++) (Just (InfixE (Just (LitE (StringL ", curSt:"))) (VarE GHC.Base.++) (Just (AppE (VarE GHC.Show.show) (VarE x_2))))))))))) []])))) []]]
    ]
]

[SigD transit_2 (AppT (AppT ArrowT (ConT Type.StateTransition)) (AppT (ConT Type.AppContext) (TupleT 0))),
  FunD transit_2 [
    Clause [ConP Type.StartingToRunning [] []]
      (NormalB (InfixE (Just (VarE Control.Monad.Trans.State.Lazy.get)) (VarE GHC.Internal.Base.>>=) (Just (LamCaseE [
        Match (ConP Type.AppStateW [] [ConP Type.StartingState [] []]) (NormalB (InfixE (Just (UnboundVarE changeTo)) (VarE GHC.Internal.Base.$) (Just (AppE (ConE Type.AppStateW) (ConE Type.RunningState))))) [],
        Match (ConP Type.AppStateW [] [VarP x_3])
          (NormalB (InfixE 
            (Just (VarE GHC.Internal.Control.Monad.Fail.fail))
              (VarE GHC.Internal.Base.$) 
              (Just (InfixE
                (Just (LitE (StringL "invalid state transition. trans:")))
                  (VarE GHC.Internal.Base.++)
                  (Just (InfixE 
                    (Just (AppE (VarE GHC.Internal.Show.show) (ConE Type.StartingToRunning))) 
                    (VarE GHC.Internal.Base.++)
                    (Just (InfixE (Just (LitE (StringL ", curSt:")))
                      (VarE GHC.Internal.Base.++)
                      (Just (AppE (VarE GHC.Internal.Show.show) (VarE x_3)))
                    ))
                  ))
                ))
              )) []
        ])))) [],
    Clause [ConP Type.RunningToStopping [] []] (NormalB (InfixE (Just (VarE Control.Monad.Trans.State.Lazy.get)) (VarE GHC.Internal.Base.>>=) (Just (LamCaseE [
      Match (ConP Type.AppStateW [] [ConP Type.RunningState [] []]) (NormalB (InfixE (Just (UnboundVarE changeTo)) (VarE GHC.Internal.Base.$) (Just (AppE (ConE Type.AppStateW) (ConE Type.StoppingState))))) [],
      Match (ConP Type.AppStateW [] [VarP x_4]) (NormalB (InfixE (Just (VarE GHC.Internal.Control.Monad.Fail.fail)) (VarE GHC.Internal.Base.$) (Just (InfixE (Just (LitE (StringL "invalid state transition. trans:"))) (VarE GHC.Internal.Base.++) (Just (InfixE (Just (AppE (VarE GHC.Internal.Show.show) (ConE Type.StartingToRunning))) (VarE GHC.Internal.Base.++) (Just (InfixE (Just (LitE (StringL ", curSt:"))) (VarE GHC.Internal.Base.++) (Just (AppE (VarE GHC.Internal.Show.show) (VarE x_4))))))))))) []])))) []
    ]
]


-}

funcTH_transit :: Q [Dec]
funcTH_transit = do
  fname <- newName "transit"
  cons <- getContNames ''StateTransition
  clauses <- mapM makeClaues cons

  return $ [SigD fname (AppT (AppT ArrowT (ConT ''StateTransition)) (AppT (ConT ''AppStateContext) (TupleT 0))),
            FunD fname clauses
           ]

  where
    -- |
    --
    getContNames :: Name -> Q [Name]
    getContNames n = reify n >>= \case
      TyConI (DataD _ _ _ _ cs _) -> mapM go cs
      x -> fail $ "[ERROR] can not get data constructor. " ++ show x

      where
        go (NormalC x _) = return x
        go x = fail $ "[ERROR] can not get data constructor. " ++ show x

    -- |
    --
    makeClaues :: Name -> Q Clause
    makeClaues n = do
      x <- newName "x"
      (curSt, nexSt) <- getStName n
      return $ Clause
        [ConP n [] []]
        (NormalB
          (InfixE (Just (UnboundVarE 'get)) (VarE '(GHC.Base.>>=))
            (Just (LamCaseE [
              Match (ConP 'AppStateW [] [ConP curSt [] []])
                (NormalB (InfixE (Just (UnboundVarE 'changeTo)) (VarE '(GHC.Base.$)) (Just (AppE (ConE 'AppStateW) (ConE nexSt))) )) [],
              Match (ConP 'AppStateW [] [VarP x])
                (NormalB (InfixE
                  (Just (VarE 'Control.Monad.Fail.fail))
                  (VarE '(GHC.Base.$))
                  (Just (InfixE
                    (Just (LitE (StringL "invalid state transition. trans:")))
                      (VarE '(GHC.Base.++))
                      (Just (InfixE
                        (Just (AppE (VarE 'GHC.Show.show) (ConE n)))
                        (VarE '(GHC.Base.++))
                        (Just (InfixE
                          (Just (LitE (StringL ", curSt:")))
                            (VarE '(GHC.Base.++))
                            (Just (AppE (VarE 'GHC.Show.show) (VarE x)))
                        ))
                      ))
                  ))
                )) []
            ]))
          )
        )
        []

    -- |
    --
    getStName :: Name -> Q (Name, Name)
    getStName n = do
      let modName = "ASA.Domain.Service.Type."

      let stStrs = T.splitOn "To" $ T.replace modName "" $ T.pack $ show n
      when (2 /= length stStrs) $ fail $ "[ERROR] invalid StateTransition constructor. " ++ show n

      let curSt = mkName $ T.unpack $ modName `T.append` head stStrs `T.append` "State"
          nxtSt = mkName $ T.unpack $ modName `T.append` last stStrs `T.append` "State"

      return (curSt, nxtSt)
