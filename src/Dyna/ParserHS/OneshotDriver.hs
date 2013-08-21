---------------------------------------------------------------------------
-- | A driver which wraps the parser and accumulates state to hand off in a
-- single chunk to the rest of the pipeline.
--
-- XXX We'd like to have a much more incremental version as well, but the
-- easiest thing to do was to extricate the old parser's state handling code
-- to its own module first.  This is really quite bad, under just about any
-- metric you care to name.

--   Header material                                                      {{{

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Dyna.ParserHS.OneshotDriver where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.State
import qualified Data.ByteString                  as B
import qualified Data.ByteString.UTF8             as BU
import qualified Data.Map                         as M
import           Data.Maybe
import qualified Data.Set                         as S
import           Data.Monoid (mempty)
import           Dyna.Main.Defns
import           Dyna.Main.Exception
import           Dyna.ParserHS.Parser
import           Dyna.ParserHS.Printer
import           Dyna.ParserHS.SyntaxTheory
import           Dyna.ParserHS.Syntax
import           Dyna.ParserHS.Types
import           Dyna.Term.TTerm
import           Dyna.XXX.Trifecta (prettySpanLoc)
import           Text.Parser.LookAhead
import           Text.Trifecta
import qualified Text.PrettyPrint.Free            as PP

------------------------------------------------------------------------}}}
-- Output                                                               {{{

data ParsedDynaProgram = PDP
  { pdp_rules         :: [(RuleIx, DisposTab DFunctAr, Spanned Rule)]

  , pdp_aggrs         :: M.Map DFunctAr DAgg

  , pdp_gbc           :: S.Set DFunctAr

    -- | A rather ugly hack for resumable parsing: this records the set of
    -- pragmas to restore the current PCS.
  , pdp_parser_resume :: forall e . PP.Doc e
  }

------------------------------------------------------------------------}}}
-- Driver state                                                         {{{

-- | Parser Configuration State
data PCS = PCS
  { _pcs_dt_mk     :: NamedDefDispos
  , _pcs_dt_over   :: DisposTabOver DFunctAr

  , _pcs_gbc_set   :: S.Set DFunctAr

  , _pcs_iagg_map  :: M.Map DFunctAr DAgg
  , _pcs_instmap   :: M.Map B.ByteString ([DVar]
                                         ,ParsedInst
                                         ,Span)
    -- ^ Collects inst pragmas
    --
    -- XXX add arity to key?
  , _pcs_modemap   :: M.Map B.ByteString ([DVar]
                                         ,ParsedModeInst
                                         ,ParsedModeInst
                                         ,Span)
    -- ^ Collects mode pragmas
    --
    -- XXX add arity to key?
  , _pcs_operspec  :: OperSpec
  , _pcs_ruleix    :: RuleIx

  , _pcs_cache_dt  :: DisposTab DFunctAr
    -- ^ Cache the disposition table
  , _pcs_cache_dlc  :: DLCfg
    -- ^ Cache the parser configuration
  }
$(makeLenses ''PCS)

update_pcs_dt, update_pcs_dlc :: (Applicative m, MonadState PCS m) => m ()
update_pcs_dt = pcs_cache_dt <~ liftA2 dtmk (use pcs_dt_mk) (use pcs_dt_over)
update_pcs_dlc = pcs_cache_dlc <~ uses pcs_operspec mkDLC

dtmk :: NamedDefDispos -> DisposTabOver DFunctAr -> DisposTab DFunctAr
dtmk n = mkDisposTab (go n)
 where
  go NDDDyna = defDispos_dyna
  -- XXX disposTab_prologish

newtype PCM im a = PCM { unPCM :: StateT PCS im a }
 deriving (Alternative,Applicative,CharParsing,DeltaParsing,
           Functor,LookAheadParsing,Monad,MonadPlus,Parsing,TokenParsing)

instance (Monad im) => MonadState PCS (PCM im) where
  get = PCM get
  put = PCM . put
  state = PCM . state

defPCS :: PCS
defPCS = PCS { _pcs_dt_mk     = NDDDyna
             , _pcs_dt_over   = mempty

             , _pcs_gbc_set   = S.empty

             , _pcs_iagg_map  = M.empty

             , _pcs_instmap   = mempty -- XXX
             , _pcs_modemap   = mempty -- XXX

             , _pcs_operspec  = dynaOperSpec

             , _pcs_ruleix    = 0

             , _pcs_cache_dt  = dtmk (defPCS ^. pcs_dt_mk)
                                     (defPCS ^. pcs_dt_over)
             , _pcs_cache_dlc = mkDLC (defPCS ^. pcs_operspec)
             }

-- | Update the PCS to reflect a new pragma
pcsProcPragma :: (Parsing m, MonadState PCS m) => Spanned Pragma -> m ()

pcsProcPragma (PBackchain fa :~ _) = do
  pcs_gbc_set %= S.insert fa

pcsProcPragma (PDispos p s f as :~ _) = do
  pcs_dt_over %= disposOverMerge ((f,length as),p) (s,as)
  update_pcs_dt
  return ()
pcsProcPragma (PDisposDefl n :~ _) = do
  pcs_dt_mk .= n
  update_pcs_dt
  return ()

pcsProcPragma (PIAggr f n a :~ _) = pcs_iagg_map . at (f,n) .= Just a

pcsProcPragma (PInst (PNWA n as) i :~ s) = do
  im <- use pcs_instmap
  maybe (pcs_instmap %= M.insert n (as,i,s))
        -- XXX fix this error message once the new trifecta lands upstream
        -- with its ability to throw Err.
        (\(_,_,s') -> unexpected $ "duplicate definition of inst: "
                                      ++ (show n)
                                      ++ "(prior definition at "
                                      ++ (show s') ++ ")" )
      $ M.lookup n im
pcsProcPragma (PMode (PNWA n as) pmf pmt :~ s) = do
  mm <- use pcs_modemap
  maybe (pcs_modemap %= M.insert n (as,pmf,pmt,s))
        -- XXX fix this error message once the new trifecta lands upstream
        -- with its ability to throw Err.
        (\(_,_,_,s') -> unexpected $ "duplicate definition of mode: "
                                      ++ (show n)
                                      ++ "(prior definition at "
                                      ++ (show s') ++ ")" )
      $ M.lookup n mm
pcsProcPragma (PRuleIx r :~ _) = pcs_ruleix .= r

pcsProcPragma (POperAdd fx prec sym :~ _) = do
  pcs_operspec %= operSpecMut (BU.toString sym) fx (Just prec)
  update_pcs_dlc

pcsProcPragma (POperDel fx sym :~ _) = do
  pcs_operspec %= operSpecMut (BU.toString sym) fx Nothing
  update_pcs_dlc

sorryPragma :: Pragma -> Span -> a
sorryPragma p s = dynacSorry $ "Cannot handle pragma"
                             PP.<//> (PP.text $ show p)
                             PP.<//> "at"
                             PP.<//> prettySpanLoc s

pragmasFromPCS :: PCS -> PP.Doc e
pragmasFromPCS (PCS dt_mk
                    dt_over
                    gbcs
                    _
                    im
                    mm
                    _
                    rix
                    _
                    _
               ) =
  PP.vcat $ map renderPragma $
       (map PBackchain $ S.toList gbcs)
    ++ (map (\(((k,_),p),(s,as)) -> PDispos p s k as)
          $ M.toList dt_over)
    ++ [PDisposDefl dt_mk]
    -- XXX leaving out the item agg map, because that gets refined during
    -- the program's execution.
    -- ++ (map (\((f,a),agg) -> PIAggr f a agg) $ M.toList iaggmap)
    ++ (map (\(n,(as,i,_)) -> PInst (PNWA n as) i) $ M.toList im)
    ++ (map (\(n,(as,pmf,pmt,_)) -> PMode (PNWA n as) pmf pmt) $ M.toList mm)
    ++ [PRuleIx rix]

nextRule :: (DeltaParsing m, LookAheadParsing m, MonadState PCS m)
         => m (Maybe (Spanned Rule))
nextRule = go
 where
  go = do
    (l :~ s) <- use pcs_cache_dlc >>= parse
    case l of
      PLPragma  p -> pcsProcPragma (p :~ s) >> return Nothing
      PLRule r -> return (Just r)

oneshotDynaParser :: (DeltaParsing m, LookAheadParsing m)
                  => m ParsedDynaProgram
oneshotDynaParser = (postProcess =<<)
   $ flip runStateT defPCS
   $  optional (dynaWhiteSpace (someSpace))
   *> many (do
             mr <- nextRule
             case mr of
               Nothing -> return Nothing
               (Just r) -> do
                 rix <- pcs_ruleix <<%= (+1)
                 dt  <- use pcs_cache_dt
                 return $ Just (rix, dt, r))
 where
  postProcess (rs,pcs) = return $
    PDP (catMaybes rs)
        (pcs ^. pcs_iagg_map)
        (pcs ^. pcs_gbc_set)
        (pragmasFromPCS pcs)

------------------------------------------------------------------------}}}
