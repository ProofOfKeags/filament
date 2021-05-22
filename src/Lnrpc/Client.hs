{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
module Lnrpc.Client where

import           Data.ByteArray.Encoding        ( Base(Base16)
                                                , convertToBase
                                                )
import           Data.ByteString.Char8          ( pack )
import           Data.Default.Class             ( def )
import           Data.X509                      ( SignedCertificate )
import           Data.X509.CertificateStore
import           Data.X509.File                 ( readSignedObject )
import           Data.X509.Memory               ( readSignedObjectFromMemory )
import           Network.GRPC.Client
import           Network.GRPC.HTTP2.Encoding    ( Decoding(Decoding)
                                                , Encoding(Encoding)
                                                )
import           Network.GRPC.HTTP2.ProtoLens
import           Network.GRPC.HTTP2.Types       ( IsRPC )
import           Network.HTTP2                  ( ErrorCode
                                                , ErrorCodeId
                                                )
import           Network.HTTP2.Client           ( HostName
                                                , PortNumber
                                                , TooMuchConcurrency
                                                    ( TooMuchConcurrency
                                                    )
                                                , defaultGoAwayHandler
                                                , ignoreFallbackHandler
                                                , newHttp2Client
                                                , newHttp2FrameConnection
                                                , runClientIO
                                                )
import           Network.TLS                    ( ClientHooks
                                                    ( onServerCertificate
                                                    )
                                                , ClientParams
                                                    ( clientHooks
                                                    , clientSupported
                                                    )
                                                , Supported(supportedCiphers)
                                                , defaultParamsClient
                                                )
import           Network.TLS.Extra              ( ciphersuite_strong_det )
import           Proto.Chainrpc.Chainnotifier
import           Protolude

type Macaroon = ByteString

data LnrpcClient = LnrpcClient
    { runLnrpcClient
          :: forall r a
           . IsRPC r
          => RPCCall r a
          -> IO (Either TooMuchConcurrency a)
    }

data LnrpcRes a = LnrpcRes
    { rpcHeaders  :: CIHeaderList
    , rpcTrailers :: Maybe CIHeaderList
    , rpcData     :: Either Text a
    }

readCertificate :: ByteString -> Maybe SignedCertificate
readCertificate raw = headMay $ readSignedObjectFromMemory raw

readCertificateFile :: FilePath -> IO (Maybe SignedCertificate)
readCertificateFile = fmap headMay . readSignedObject

newLnrpcClient
    :: HostName -> PortNumber -> Macaroon -> SignedCertificate -> IO LnrpcClient
newLnrpcClient host port macaroon cert = (either throwIO pure) =<< runClientIO
    do
        let store = makeCertificateStore [cert]
        conn   <- newHttp2FrameConnection host port (Just $ tlsParams store)
        client <- newHttp2Client conn
                                 8192
                                 8192
                                 []
                                 defaultGoAwayHandler
                                 ignoreFallbackHandler
        let runLnrpcClient rpc = do
                clientError <- runClientIO $ open
                    client
                    (pack host <> ":" <> pack (show port))
                    [("macaroon", convertToBase Base16 macaroon)]
                    (Timeout 30)
                    (Encoding uncompressed)
                    (Decoding uncompressed)
                    rpc
                either throwIO pure clientError
        pure $ LnrpcClient { .. }

tlsParams :: CertificateStore -> ClientParams
tlsParams extra = (defaultParamsClient "localhost" "")
    { clientHooks     = def
                            { onServerCertificate = onServerCertificate def
                                                        . (<> extra)
                            }
    , clientSupported = def { supportedCiphers = ciphersuite_strong_det }
    }


registerBlockEpochNtfn :: RPC ChainNotifier "registerBlockEpochNtfn"
registerBlockEpochNtfn = RPC
