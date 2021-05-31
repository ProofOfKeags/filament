{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module Lnrpc.BiDiStream where

import           Network.GRPC.Client            ( BiDiStep
                                                    ( Abort
                                                    , SendInput
                                                    , WaitOutput
                                                    )
                                                , CompressMode(Uncompressed)
                                                , RPCCall(RPCCall)
                                                , RunBiDiStep
                                                , steppedBiDiStream
                                                )
import           Network.GRPC.HTTP2.ProtoLens   ( RPC(RPC) )
import           Proto.Rpc                      ( ChannelAcceptRequest
                                                , ChannelAcceptResponse
                                                , Lightning
                                                )
import           Protolude

channelAcceptor
    :: (ChannelAcceptRequest -> IO (Maybe ChannelAcceptResponse))
    -> RPCCall (RPC Lightning "channelAcceptor") (Maybe ChannelAcceptRequest)
channelAcceptor f = steppedBiDiStream RPC Nothing $ \case
    Nothing ->
        pure
            $ (Nothing, WaitOutput (\_ _ o -> pure $ Just o) (\_ a _ -> pure a))
    Just x -> do
        accept <- liftIO $ f x
        case accept of
            Nothing -> pure (Nothing, Abort)
            Just r  -> pure (Nothing, SendInput Uncompressed r)
