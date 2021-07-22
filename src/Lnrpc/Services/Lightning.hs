module Lnrpc.Services.Lightning where

import           Proto.Rpc                      ( AbandonChannelRequest
                                                , AbandonChannelResponse
                                                )
import           Protolude

abandonChannel :: AbandonChannelRequest -> IO AbandonChannelResponse
abandonChannel = _

addInvoice = _
bakeMacaroon = _
channelAcceptor = _
channelBalance = _
closeChannel = _
closedChannels = _
connectPeer = _
debugLevel = _
decodePayReq = _
deleteAllPayments = _
deleteMacaroonID = _
describeGraph = _
disconnectPeer = _
estimateFee = _
exportAllChannelBackups = _
exportChannelBackup = _
feeReport = _
forwardingHistory = _
fundingStateStep = _
getChanInfo = _
getInfo = _
getNetworkInfo = _
getNodeInfo = _
getNodeMetrics = _
getRecoveryInfo = _
getTransactions = _
listChannels = _
listInvoices = _
listMacaroonIDs = _
listPayments = _
listPeers = _
listPermissions = _
listUnspent = _
lookupInvoice = _
newAddress = _
openChannel = _
openChannelSync = _
pendingChannels = _
queryRoutes = _
restoreChannelBackups = _
sendCoins = _
sendMany = _
sendPayment = _
sendPaymentSync = _
sendToRoute = _
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
