---------------------------------------------------------------------------
-- | A driver which wraps the parser and accumulates state to hand off in a
-- single chunk to the rest of the pipeline.
--
-- XXX We'd like to have a much more incremental version as well, but the
-- easiest thing to do was to extricate the old parser's state handling code
-- to its own module first.

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
import qualified Data.Set                         as S
import           Data.Monoid (mempty)
import           Dyna.Main.Defns
import           Dyna.Main.Exception
import           Dyna.ParserHS.Parser
import           Dyna.Term.SurfaceSyntax
import           Dyna.Term.TTerm
import           Dyna.XXX.Trifecta (prettySpanLoc)
import           Text.Parser.LookAhead
import           Text.Trifecta
import qualified Text.PrettyPrint.Free            as PP

------------------------------------------------------------------------}}}
-- Output                                                               {{{

data ParsedDynaProgram = PDP
  { pdp_rules         :: [(RuleIx, DisposTab, Spanned Rule)]

  , pdp_aggrs         :: M.Map DFunctAr DAgg

    -- | A rather ugly hack for resumable parsing: this records the set of
    -- pragmas to restore the current PCS.
  , pdp_parser_resume :: forall e . PP.Doc e
  }

------------------------------------------------------------------------}}}
-- Driver state                                                         {{{

-- | Parser Configuration State
data PCS = PCS
  { _pcs_dt_mk     :: String
  , _pcs_dt_over   :: DisposTabOver
  , _pcs_dt_cache  :: DisposTab
    -- ^ Cache the disposition table
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
  , _pcs_ot_cache  :: EOT
    -- ^ Cache the operator table so we are not rebuilding it
    -- before every parse operation
  , _pcs_ruleix    :: RuleIx
  }
$(makeLenses ''PCS)

mkdlc :: Maybe (S.Set String) -> PCS -> DLCfg
mkdlc aggs pcs = DLC (_pcs_ot_cache pcs)
                     (maybe genericAggregators ct aggs)
 where
  ct = fmap BU.fromString . choice . map (try . string) . S.toList

update_pcs_dt,
 update_pcs_ot :: (Applicative m, MonadState PCS m) => m ()
update_pcs_dt = pcs_dt_cache <~
                liftA2 ($) (uses pcs_dt_mk dtmk) (use pcs_dt_over)

update_pcs_ot = pcs_ot_cache <~ (flip mkEOT True <$> (use pcs_operspec))

dtmk :: String -> DisposTabOver -> DisposTab
dtmk "dyna"      = disposTab_dyna
dtmk "prologish" = disposTab_dyna
dtmk n           = dynacPanic $ "Unknown default disposition table:"
                                 PP.<//> PP.pretty n


newtype PCM im a = PCM { unPCM :: StateT PCS im a }
 deriving (Alternative,Applicative,CharParsing,DeltaParsing,
           Functor,LookAheadParsing,Monad,MonadPlus,Parsing,TokenParsing)

instance (Monad im) => MonadState PCS (PCM im) where
  get = PCM get
  put = PCM . put
  state = PCM . state

defPCS :: PCS
defPCS = PCS { _pcs_dt_mk     = "dyna"
             , _pcs_dt_over   = mempty
             , _pcs_dt_cache  = dtmk (defPCS ^. pcs_dt_mk)
                                     (defPCS ^. pcs_dt_over)

             , _pcs_iagg_map  = M.empty

             , _pcs_instmap   = mempty -- XXX
             , _pcs_modemap   = mempty -- XXX

             , _pcs_operspec  = defOperSpec
             , _pcs_ot_cache  = mkEOT (defPCS ^. pcs_operspec) True

             , _pcs_ruleix    = 0
             }

-- | Update the PCS to reflect a new pragma
pcsProcPragma :: (Parsing m, MonadState PCS m) => Spanned Pragma -> m ()
pcsProcPragma (PDispos s f as :~ _) = do
  pcs_dt_over %= dtoMerge (f,length as) (s,as)
  update_pcs_dt
  return ()
pcsProcPragma (PDisposDefl n :~ _) = do
  pcs_dt_mk .= n
  update_pcs_dt
  return ()
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

pcsProcPragma (p :~ s) = dynacSorry $ "Cannot handle pragma"
                                      PP.<//> (PP.text $ show p)
                                      PP.<//> "at"
                                      PP.<//> prettySpanLoc s

pragmasFromPCS :: PCS -> PP.Doc e
pragmasFromPCS (PCS dt_mk dt_over _
                    _
                    im mm
                    _ _
                    rix) =
  PP.vcat $ map renderPragma $
       (map (\((k,_),(s,as)) -> PDispos s k as)
          $ M.toList dt_over)
    ++ [PDisposDefl dt_mk]
    -- XXX leaving out the item agg map, because that gets refined during
    -- the program's execution.
    -- ++ (map (\((f,a),agg) -> PIAggr f a agg) $ M.toList iaggmap)
    ++ (map (\(n,(as,i,_)) -> PInst (PNWA n as) i) $ M.toList im)
    ++ (map (\(n,(as,pmf,pmt,_)) -> PMode (PNWA n as) pmf pmt) $ M.toList mm)
    ++ [PRuleIx rix]

nextRule :: (DeltaParsing m, LookAheadParsing m, MonadState PCS m)
         => Maybe (S.Set String)
         -> m (Spanned Rule)
nextRule aggs = go
 where
  go = do
    (l :~ s) <- gets (mkdlc aggs) >>= parse
    case l of
      LPragma  p -> pcsProcPragma (p :~ s) >> go
      LRule r -> return r

oneshotDynaParser :: (DeltaParsing m, LookAheadParsing m)
                  => Maybe (S.Set String)
                  -> m ParsedDynaProgram
oneshotDynaParser aggs = (postProcess =<<)
   $ flip runStateT defPCS
   $ many (try $ do
             r <- nextRule aggs
             rix <- pcs_ruleix <<%= (+1)
             dt  <- use pcs_dt_cache
             return $ (rix, dt, r))
     <* optional (dynaWhiteSpace (someSpace))
 where
  postProcess (rs,pcs) = return $ PDP rs (pcs ^. pcs_iagg_map) (pragmasFromPCS pcs)


------------------------------------------------------------------------}}}
