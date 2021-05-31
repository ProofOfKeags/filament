{-# LANGUAGE DataKinds #-}
module Lnrpc.Single where


import           Data.ProtoLens
import           Network.GRPC.Client            ( RPCCall
                                                , RawReply
                                                , singleRequest
                                                )
import           Network.GRPC.HTTP2.ProtoLens   ( RPC(RPC) )
import           Proto.Rpc                      ( GetInfoResponse
                                                , Lightning
                                                )

getInfo :: RPCCall (RPC Lightning "getInfo") (RawReply GetInfoResponse)
getInfo = singleRequest RPC defMessage
