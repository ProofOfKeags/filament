{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Lnrpc.Client where

import           Data.ByteArray.Encoding        ( Base(Base16)
                                                , convertToBase
                                                )
import qualified Data.ByteString
import           Data.ByteString.Char8          ( pack )
import           Data.Default.Class             ( def )
import           Data.ProtoLens                 ( defMessage )
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
import           Network.HTTP2.Client           ( ClientIO
                                                , HostName
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
import           Proto.Rpc
import           Protolude
import           System.FilePath                ( (</>) )

type Macaroon = ByteString

data LnrpcClient = LnrpcClient
    { runLnrpcClient
          :: forall r a
           . IsRPC r
          => Maybe Timeout
          -> RPCCall r a
          -> IO (Either TooMuchConcurrency a)
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
        let runLnrpcClient timeout rpc = do
                clientError <- runClientIO $ open
                    client
                    (pack host <> ":" <> pack (show port))
                    [("macaroon", convertToBase Base16 macaroon)]
                    timeout
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

registerBlockEpochNtfn
    :: a
    -> (a -> HeaderList -> BlockEpoch -> ClientIO a)
    -> RPCCall
           (RPC ChainNotifier "registerBlockEpochNtfn")
           (a, HeaderList, HeaderList)
registerBlockEpochNtfn s = streamReply RPC s defMessage

getInfo :: RPCCall (RPC Lightning "getInfo") (RawReply GetInfoResponse)
getInfo = singleRequest RPC defMessage

newDefaultLnrpcClientFromDir :: FilePath -> IO LnrpcClient
newDefaultLnrpcClientFromDir dir = do
    mCert <- readCertificateFile $ dir </> "tls.cert"
    cert  <- case mCert of
        Nothing -> throwIO (AssertionFailed "Invalid Certificate File")
        Just x  -> pure x
    mac <-
        Data.ByteString.readFile
        $   dir
        </> "data/chain/bitcoin/regtest/admin.macaroon"
    newLnrpcClient "127.0.0.1" 10009 mac cert
