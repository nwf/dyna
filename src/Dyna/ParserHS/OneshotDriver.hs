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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Dyna.ParserHS.OneshotDriver where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.State
import qualified Data.ByteString                  as B
import qualified Data.Map                         as M
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
  { _pdp_rules    :: [(RuleIx, DisposTab, Spanned Rule)]
  , _pdp_next_rix :: RuleIx
  }

------------------------------------------------------------------------}}}
-- Driver state                                                         {{{

-- | Configuration state threaded into the parser
--
-- Note that this type is hidden with the exception of some accessors below.
data PCS = PCS
  { _pcs_dt_mk     :: DisposTabOver -> DisposTab
  , _pcs_dt_over   :: DisposTabOver
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
  , _pcs_opertab   :: EOT
    -- ^ Cache the operator table so we are not rebuilding it
    -- before every parse operation
  , _pcs_ruleix    :: RuleIx
  }
$(makeLenses ''PCS)

_pcs_dlc pcs = DLC (_pcs_opertab pcs)

pcs_dt = liftA2 ($) (use pcs_dt_mk) (use pcs_dt_over)

newtype PCM im a = PCM { unPCM :: StateT PCS im a }
 deriving (Alternative,Applicative,CharParsing,DeltaParsing,
           Functor,LookAheadParsing,Monad,MonadPlus,Parsing,TokenParsing)

instance (Monad im) => MonadState PCS (PCM im) where
  get = PCM get
  put = PCM . put
  state = PCM . state

defPCS :: PCS
defPCS = PCS { _pcs_dt_mk     = disposTab_dyna
             , _pcs_dt_over   = mempty
             , _pcs_instmap   = mempty -- XXX
             , _pcs_modemap   = mempty -- XXX
             , _pcs_operspec  = defOperSpec
             , _pcs_opertab   = mkEOT (defPCS ^. pcs_operspec) True
             , _pcs_ruleix    = 0
             }

-- | Update the PCS to reflect a new pragma
pcsProcPragma :: (Parsing m, MonadState PCS m) => Spanned Pragma -> m ()
pcsProcPragma (PDispos s f as :~ _) = do
  pcs_dt_over %= dtoMerge (f,length as) (s,as)
pcsProcPragma (PDisposDefl n :~ s) = do
  pcs_dt_mk .= case n of
                 "dyna" -> disposTab_dyna
                 "prologish" -> disposTab_dyna
                 _ -> dynacPanic $ "Unknown default disposition table:"
                                   PP.<//> PP.pretty n
                                   PP.<//> "at" PP.<//> prettySpanLoc s
pcsProcPragma (PInst (PNWA n as) pi :~ s) = do
  im <- use pcs_instmap
  maybe (pcs_instmap %= M.insert n (as,pi,s))
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

nextRule :: (DeltaParsing m, LookAheadParsing m, MonadState PCS m)
         => m (Spanned Rule)
nextRule = do
  (l :~ s) <- gets _pcs_dlc >>= parse
  case l of
    LPragma  p -> pcsProcPragma (p :~ s) >> nextRule
    LRule r -> return r

oneshotDynaParser :: (DeltaParsing m, LookAheadParsing m) => m ParsedDynaProgram
oneshotDynaParser = (postProcess =<<) $ flip runStateT defPCS
                                      $ many $ do
                                                r <- nextRule
                                                rix  <- pcs_ruleix <<%= (+1)
                                                dtmk <- use pcs_dt_mk
                                                dto  <- use pcs_dt_over
                                                return $ (rix, dtmk dto, r)
                                        <* whiteSpace
 where
  postProcess (rs,pcs) = return $ PDP rs (pcs^.pcs_ruleix)

------------------------------------------------------------------------}}}
