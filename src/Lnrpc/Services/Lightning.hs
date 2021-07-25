module Lnrpc.Services.Lightning where

import           Proto.Rpc                      ( AbandonChannelRequest
                                                , AbandonChannelResponse
                                                , AddInvoiceResponse
                                                , BakeMacaroonRequest
                                                , BakeMacaroonResponse
                                                , ChanBackupExportRequest
                                                , ChanBackupSnapshot
                                                , ChanInfoRequest
                                                , ChannelAcceptRequest
                                                , ChannelAcceptResponse
                                                , ChannelBackup
                                                , ChannelBalanceRequest
                                                , ChannelBalanceResponse
                                                , ChannelEdge
                                                , ChannelGraph
                                                , ChannelGraphRequest
                                                , ChannelPoint
                                                , CloseChannelRequest
                                                , CloseStatusUpdate
                                                , ClosedChannelsRequest
                                                , ClosedChannelsResponse
                                                , ConnectPeerRequest
                                                , ConnectPeerResponse
                                                , DebugLevelRequest
                                                , DebugLevelResponse
                                                , DeleteAllPaymentsRequest
                                                , DeleteAllPaymentsResponse
                                                , DeleteMacaroonIDRequest
                                                , DeleteMacaroonIDResponse
                                                , DisconnectPeerRequest
                                                , DisconnectPeerResponse
                                                , EstimateFeeRequest
                                                , EstimateFeeResponse
                                                , ExportChannelBackupRequest
                                                , FeeReportRequest
                                                , FeeReportResponse
                                                , ForwardingHistoryRequest
                                                , ForwardingHistoryResponse
                                                , FundingStateStepResp
                                                , FundingTransitionMsg
                                                , GetInfoRequest
                                                , GetInfoResponse
                                                , GetRecoveryInfoRequest
                                                , GetRecoveryInfoResponse
                                                , GetTransactionsRequest
                                                , Invoice
                                                , ListChannelsRequest
                                                , ListChannelsResponse
                                                , ListInvoiceRequest
                                                , ListInvoiceResponse
                                                , ListMacaroonIDsRequest
                                                , ListMacaroonIDsResponse
                                                , ListPaymentsRequest
                                                , ListPaymentsResponse
                                                , ListPeersRequest
                                                , ListPeersResponse
                                                , ListPermissionsRequest
                                                , ListPermissionsResponse
                                                , ListUnspentRequest
                                                , ListUnspentResponse
                                                , NetworkInfo
                                                , NetworkInfoRequest
                                                , NewAddressRequest
                                                , NewAddressResponse
                                                , NodeInfo
                                                , NodeInfoRequest
                                                , NodeMetricsRequest
                                                , NodeMetricsResponse
                                                , OpenChannelRequest
                                                , OpenStatusUpdate
                                                , PayReq
                                                , PayReqString
                                                , PaymentHash
                                                , PendingChannelsRequest
                                                , PendingChannelsResponse
                                                , QueryRoutesRequest
                                                , QueryRoutesResponse
                                                , RestoreBackupResponse
                                                , RestoreChanBackupRequest
                                                , SendCoinsRequest
                                                , SendCoinsResponse
                                                , SendManyRequest
                                                , SendManyResponse
                                                , SendRequest
                                                , SendResponse
                                                , SendToRouteRequest
                                                , TransactionDetails
                                                )
import           Protolude

abandonChannel :: AbandonChannelRequest -> IO AbandonChannelResponse
abandonChannel = _

addInvoice :: Invoice -> IO AddInvoiceResponse
addInvoice = _

bakeMacaroon :: BakeMacaroonRequest -> IO BakeMacaroonResponse
bakeMacaroon = _

channelAcceptor :: (ChannelAcceptRequest -> IO ChannelAcceptResponse) -> IO ()
channelAcceptor = _

channelBalance :: ChannelBalanceRequest -> IO ChannelBalanceResponse
channelBalance = _

closeChannel :: CloseChannelRequest -> (CloseStatusUpdate -> IO ()) -> IO ()
closeChannel = _

closedChannels :: ClosedChannelsRequest -> IO ClosedChannelsResponse
closedChannels = _

connectPeer :: ConnectPeerRequest -> IO ConnectPeerResponse
connectPeer = _

debugLevel :: DebugLevelRequest -> IO DebugLevelResponse
debugLevel = _

decodePayReq :: PayReqString -> IO PayReq
decodePayReq = _

deleteAllPayments :: DeleteAllPaymentsRequest -> IO DeleteAllPaymentsResponse
deleteAllPayments = _

deleteMacaroonID :: DeleteMacaroonIDRequest -> IO DeleteMacaroonIDResponse
deleteMacaroonID = _

describeGraph :: ChannelGraphRequest -> IO ChannelGraph
describeGraph = _

disconnectPeer :: DisconnectPeerRequest -> IO DisconnectPeerResponse
disconnectPeer = _

estimateFee :: EstimateFeeRequest -> IO EstimateFeeResponse
estimateFee = _

exportAllChannelBackups :: ChanBackupExportRequest -> IO ChanBackupSnapshot
exportAllChannelBackups = _

exportChannelBackup :: ExportChannelBackupRequest -> IO ChannelBackup
exportChannelBackup = _

feeReport :: FeeReportRequest -> IO FeeReportResponse
feeReport = _

forwardingHistory :: ForwardingHistoryRequest -> IO ForwardingHistoryResponse
forwardingHistory = _

fundingStateStep :: FundingTransitionMsg -> IO FundingStateStepResp
fundingStateStep = _

getChanInfo :: ChanInfoRequest -> IO ChannelEdge
getChanInfo = _

getInfo :: GetInfoRequest -> IO GetInfoResponse
getInfo = _

getNetworkInfo :: NetworkInfoRequest -> IO NetworkInfo
getNetworkInfo = _

getNodeInfo :: NodeInfoRequest -> IO NodeInfo
getNodeInfo = _

getNodeMetrics :: NodeMetricsRequest -> IO NodeMetricsResponse
getNodeMetrics = _

getRecoveryInfo :: GetRecoveryInfoRequest -> IO GetRecoveryInfoResponse
getRecoveryInfo = _

getTransactions :: GetTransactionsRequest -> IO TransactionDetails
getTransactions = _

listChannels :: ListChannelsRequest -> IO ListChannelsResponse
listChannels = _

listInvoices :: ListInvoiceRequest -> IO ListInvoiceResponse
listInvoices = _

listMacaroonIDs :: ListMacaroonIDsRequest -> IO ListMacaroonIDsResponse
listMacaroonIDs = _

listPayments :: ListPaymentsRequest -> IO ListPaymentsResponse
listPayments = _

listPeers :: ListPeersRequest -> IO ListPeersResponse
listPeers = _

listPermissions :: ListPermissionsRequest -> IO ListPermissionsResponse
listPermissions = _

listUnspent :: ListUnspentRequest -> IO ListUnspentResponse
listUnspent = _

lookupInvoice :: PaymentHash -> IO Invoice
lookupInvoice = _

newAddress :: NewAddressRequest -> IO NewAddressResponse
newAddress = _

openChannel :: OpenChannelRequest -> (OpenStatusUpdate -> IO ()) -> IO ()
openChannel = _

openChannelSync :: OpenChannelRequest -> IO ChannelPoint
openChannelSync = _

pendingChannels :: PendingChannelsRequest -> IO PendingChannelsResponse
pendingChannels = _

queryRoutes :: QueryRoutesRequest -> IO QueryRoutesResponse
queryRoutes = _

restoreChannelBackups :: RestoreChanBackupRequest -> IO RestoreBackupResponse
restoreChannelBackups = _

sendCoins :: SendCoinsRequest -> IO SendCoinsResponse
sendCoins = _

sendMany :: SendManyRequest -> IO SendManyResponse
sendMany = _

sendPayment :: SendRequest -> (SendResponse -> IO ()) -> IO ()
sendPayment = _

sendPaymentSync :: SendRequest -> IO SendResponse
sendPaymentSync = _

sendToRouteSync = _
signMessage = _
stopDaemon = _
subscribeChannelBackups = _
subscribeChannelEvents = _
subscribeChannelGraph = _
subscribeInvoices = _
subscribePeerEvents = _
subscribeTransactions = _
updateChannelPolicy = _
verifyChanBackup = _
verifyMessage = _
walletBalance = _
