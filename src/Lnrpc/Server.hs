{-# LANGUAGE DataKinds #-}
module Lnrpc.Server where

import           Data.ProtoLens
import           Network.GRPC.Client            ( HeaderList
                                                , RPCCall(RPCCall)
                                                , streamReply
                                                )
import           Network.GRPC.HTTP2.ProtoLens   ( RPC(RPC) )
import           Proto.Routerrpc.Router         ( HtlcEvent
                                                , Router(Router)
                                                )
import           Protolude

subscribeHtlcEvents
        :: (HtlcEvent -> IO ())
        -> RPCCall
                   (RPC Router "subscribeHtlcEvents")
                   ((), HeaderList, HeaderList)
subscribeHtlcEvents handler =
        streamReply RPC () defMessage (\_ _ evt -> liftIO $ handler evt)
