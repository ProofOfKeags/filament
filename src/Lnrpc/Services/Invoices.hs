module Lnrpc.Services.Invoices where

import           Proto.Invoicesrpc.Invoices     ( AddHoldInvoiceRequest
                                                , CancelInvoiceMsg
                                                , CancelInvoiceResp
                                                , SettleInvoiceMsg
                                                , SettleInvoiceResp
                                                , SubscribeSingleInvoiceRequest
                                                )
import           Proto.Rpc                      ( Invoice )
import           Protolude

addHoldInvoice :: AddHoldInvoiceRequest -> IO AddHoldInvoiceRequest
addHoldInvoice = _

cancelInvoice :: CancelInvoiceMsg -> IO CancelInvoiceResp
cancelInvoice = _

settleInvoice :: SettleInvoiceMsg -> IO SettleInvoiceResp
settleInvoice = _

subscribeSingleInvoice :: SubscribeSingleInvoiceRequest -> (Invoice -> IO ()) -> IO ()
subscribeSingleInvoice = _
