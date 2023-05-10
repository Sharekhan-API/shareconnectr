source("R/routes.R")
source("R/exceptions.R")
source("R/connection.R")
source("R/constants.R")
source("R/restful.R")
source("R/utils.R")
source("R/websocket.R")


params <- list(
  "api_key" = "API_KEY",
  "api_secret" = "API_SECRET",
  "request_token" = "REQUEST_TOKEN",
  "access_token" = "ACCESS_TOKEN",
  "action" ="",
  "key" = list(""),
  "value" = list("")
)

connect <- create_connection_object(params = params)
# login_url <- get_login_url(connect, "VERSION_ID")
# login_url <- get_login_url(connect)
# login_url

# connect <- set_request_token(connect, "")
# fetch <- fetch_access_token(connect)
# fetch

# Funds
# fund_route_params = list("exchange" = "MX", "customerId" = "XXX")
# fund_response <- get_funds(connect, fund_route_params)
# fund_response

# Orders | New Order
# new_order_method_params = list(
# "customerId" = XXX,
# "scripCode" = XXX,
# "tradingSymbol" = "GOLDPETAL",
# "exchange" = "MX",
# "transactionType" = "B",
# "quantity" = 1,
# "disclosedQty" = 0,
# "price" = "5862",
# "triggerPrice" = "0",
# "rmsCode" = "ANY",
# "afterHour" = "N",
# "orderType" = "NORMAL",
# "channelUser" = "XXX",
# "validity" = "GFD",
# "requestType" = "NEW",
# "productType" = "INVESTMENT",
# "instrumentType" = "FS",
# "strikePrice" = "-1",
# "expiry" = "28/04/2023",
# "optionType" = "XX"
# )
# new_order_response <- place_order(connect, new_order_method_params)
# new_order_response

# Orders | Modify order
# modify_method_params = list(
  # "orderId"="XXX",
  # "customerId" = XXX,
  # "scripCode" = XXX,
  # "tradingSymbol" = "GOLDPETAL",
  # "exchange" = "MX",
  # "transactionType" = "B",
  # "quantity" = 2,
  # "disclosedQty" = 0,
  # "price" = "5867",
  # "triggerPrice" = "0",
  # "rmsCode" = "XXX",
  # "afterHour" = "N",
  # "orderType" = "NORMAL",
  # "channelUser" = "XXX",
  # "validity" = "GFD",
  # "requestType" = "MODIFY",
  # "productType" = "INVESTMENT",
  # "instrumentType" = "FS",
  # "strikePrice" = "-1",
  # "expiry" = "28/04/2023",
  # "optionType" = "XX"
# )
# modify_response <- modify_order(connect, modify_method_params)
# modify_response

# Orders | Cancel order
# cancel_method_params = list(
#   "orderId"="XXX",
#   "customerId" = XXX,
#   "scripCode" = XXX,
#   "tradingSymbol" = "GOLDPETAL",
#   "exchange" = "MX",
#   "transactionType" = "B",
#   "quantity" = 1,
#   "disclosedQty" = 0,
#   "price" = "5862",
#   "triggerPrice" = "0",
#   "rmsCode" = "XXX",
#   "afterHour" = "N",
#   "orderType" = "NORMAL",
#   "channelUser" = "XXX",
#   "validity" = "GFD",
#   "requestType" = "CANCEL",
#   "productType" = "INVESTMENT",
#   "instrumentType" = "FS",
#   "strikePrice" = "-1",
#   "expiry" = "28/04/2023",
#   "optionType" = "XX"
# )
# cancel_response <- cancel_order(connect, cancel_method_params)
# cancel_response


# Report all orders for the day | Orders History
# report_method_params = list(
#   "customerId" = XXX
# )
# report_day_response <- get_report_day(connect, report_method_params)
# report_day_response

# Trades Position - Trades History
# trade_params = list("customerId" = XXX)
# trade_position_response <- get_trade_position(connect, trade_params)
# trade_position_response

# Order History
# order_method_params = list(
#   "exchange" = "MX",
#   "customerId" = XXX,
#   "orderId" = "XXX"
# )
# order_history_response <- get_report_history(connect, order_method_params)
# order_history_response

# Reports History - Trade History
# report_method_params = list(
#   "exchange" = "NC",
#   "customerId" = XXX,
#   "orderId" = "XXX"
#   )
# report_trade_response <- get_report_trade(connect, report_method_params)
# report_trade_response


# Holdings
# holdings_params = list("customerId" = XXX)
# holdings_response <- get_holdings(connect, holdings_params)
# holdings_response

# ScripMaster
# master_params = list("exchange" = "NC")
# master_response <- get_master(connect, master_params)
# master_response


# Historical Data
# historical_method_params = list(
#   "exchange" = "NC",
#   "scripcode" = XXX,
#   "interval" = "daily"
# )
# historical_reaponse <- get_historical(connect, historical_method_params)
# historical_reaponse



# Websocket
# socketObject <- sharekhan_connect_object(params)
# sharekhanSocket.connect(socketObject)


