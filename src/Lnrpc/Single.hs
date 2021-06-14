{-# LANGUAGE DataKinds #-}
module Lnrpc.Single where


import           Control.Lens                   ( (.~) )
import           Data.ProtoLens
import           Network.GRPC.Client            ( RPCCall
                                                , RawReply
                                                , singleRequest
                                                )
import           Network.GRPC.HTTP2.ProtoLens   ( RPC(RPC) )
import           Proto.Rpc                      ( ChannelEdge
                                                , GetInfoResponse
                                                , Lightning
                                                , NodeInfo
                                                )
import           Proto.Rpc_Fields               ( chanId
                                                , pubKey
                                                )
import           Protolude

getInfo :: RPCCall (RPC Lightning "getInfo") (RawReply GetInfoResponse)
getInfo = singleRequest RPC defMessage

getNodeInfo
        :: Text -> RPCCall (RPC Lightning "getNodeInfo") (RawReply NodeInfo)
getNodeInfo node = singleRequest RPC (defMessage & pubKey .~ node)

getChanInfo
        :: Word64
        -> RPCCall (RPC Lightning "getChanInfo") (RawReply ChannelEdge)
getChanInfo chan = singleRequest RPC (defMessage & chanId .~ chan)
