module Lnrpc.Services.ChainNotifier where

import           Proto.Chainrpc.Chainnotifier   ( BlockEpoch
                                                , ConfEvent
                                                , ConfRequest
                                                , SpendEvent
                                                , SpendRequest
                                                )
import           Protolude

registerBlockEpochNtfn :: BlockEpoch -> (BlockEpoch -> IO ()) -> IO ()
registerBlockEpochNtfn = _

registerConfirmationsNtfn :: ConfRequest -> (ConfEvent -> IO ()) -> IO ()
registerConfirmationsNtfn = _

registerSpendNtfn :: SpendRequest -> (SpendEvent -> IO ()) -> IO ()
registerSpendNtfn = _
