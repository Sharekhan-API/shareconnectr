library(jsonlite)
source("R/constants.R")
source("R/exceptions.R")

rest_api_call <- function(object,method,endpoint,route_params,method_params){
  if(!is_api_connected(object)){
    stop(NotConnectedToAPIException)
  }

  url <- get_api_endpoint(object,endpoint,route_params)
  print(url)
  r <- NULL
  method <- tolower(method)

  # print(paste("querying",url))
  # print(paste("method:",method))
  # print(route_params)
  # print(method_params)

  tryCatch({
    if(method=="get"){
      r <- httr::GET(url,query = method_params,
                     httr::add_headers(.headers = c("api-key" = object@api_key, "access-token" = object@access_token, "Content-Type" = "application/json")))
    } else if(method=="post"){
      r <- httr::POST(url,body = method_params, encode = "json",
                      httr::add_headers(.headers = c("api-key" = object@api_key, "access-token" = object@access_token, "Content-Type" = "application/json")))
    } else{
      stop(UnknwonHttpException)
    }
  }, error=function(e){
    message(e$message)
    stop(HttpException)
  })
  # print(r)

  if(r$status_code != 200){
    r <- httr::content(r)
    message(paste0(r$error_type,": ",r$message))
    if (is.null(r)) {
      r <- list()
    }

    r <- toJSON(r, pretty = TRUE, auto_unbox = TRUE)
    return(r)
  }

  if(!(r$headers$`content-type` %in% VALID_CONTENT_TYPE)){
    stop(DataException)
  }

  r <- suppressMessages(httr::content(r))

  if(any(class(r)=="data.frame") && NROW(r)>0){
    return(r)
  }

  if(any(class(r)=="data.frame") && NROW(r)<1){
    stop(NoDataException)
  }

  if(length(r$data)>0){
    if(is_sharekhan_error(toString(r$data[[1]]))){
      stop(SharekhanException)
    }
  }

  if(length(r$data)==0){
    stop(NoDataException)
  }

  if (is.null(r)) {
    r <- list()
  }

  return(r)
}

#'Function to get the fund details.
#'@description Gets available cash limit for different segments.
#'@param object An object of type sharekhanconnect with valid api_key and
#'access_token.
#'@param param This contains the request parameters.
#'@return Returns a json object with fund details, if successful.
#'@export
get_funds <- function(object, param){
  r <- NULL
  route_params <- list("exchange" = param$exchange, "customerId" = param$customerId)
  method_params <- list()
  tryCatch({r <- rest_api_call(object,"GET", "orders.fund", route_params, method_params)
  }, error=function(e){
    message(e$message)
  })

  r <- toJSON(r, pretty = TRUE, auto_unbox = TRUE, null = "null")

  return(r)
}

#'Function to place a new order.
#'@description Function to place a order with specified details.
#'@param object An object of type sharekhanconnect with valid api_key and
#'access_token.
#'@param param This contains the request parameters.
#'@return Returns a json object with order details, if successful.
#'@export
place_order <- function(object, param){
  params <- as.list(environment(), all=TRUE)

  params[["object"]] <- NULL

  keys <- names(params[["param"]])

  for(key in keys){
    if(is.null(params[[key]])){
      params[[key]] <- NULL
    }
  }
  route_params = list()
  method_params = params[[1]]

  r <- NULL

  tryCatch({r <- rest_api_call(object,"POST","orders.place", route_params, method_params)
  }, error=function(e){
    message(e$message)
  })

  r <- toJSON(r, pretty = TRUE, auto_unbox = TRUE, null = "null")

  return(r)
}

#'Function to modify an existing order.
#'@description Function to modify an existing order with specified details.
#'@param object An object of type sharekhanconnect with valid api_key and
#'access_token.
#'@param param This contains the request parameters.
#'@return Returns a json object with order details, if successful.
#'@export
modify_order <- function(object, param){
  params <- as.list(environment(), all=TRUE)

  params[["object"]] <- NULL

  keys <- names(params[["param"]])

  for(key in keys){
    if(is.null(params[[key]])){
      params[[key]] <- NULL
    }
  }
  route_params = list()
  method_params = params[[1]]

  r <- NULL

  tryCatch({r <- rest_api_call(object,"POST","orders.modify", route_params, method_params)
  }, error=function(e){
    message(e$message)
  })

  r <- toJSON(r, pretty = TRUE, auto_unbox = TRUE, null = "null")

  return(r)
}

#'Function to cancel an existing order.
#'@description Function to cancel an existing order with specified details.
#'@param object An object of type sharekhanconnect with valid api_key and
#'access_token.
#'@param param This contains the request parameters.
#'@return Returns a json object with order details, if successful.
#'@export
cancel_order <- function(object, param){
  params <- as.list(environment(), all=TRUE)

  params[["object"]] <- NULL

  keys <- names(params[["param"]])

  for(key in keys){
    if(is.null(params[[key]])){
      params[[key]] <- NULL
    }
  }
  route_params = list()
  method_params = params[[1]]

  r <- NULL

  tryCatch({r <- rest_api_call(object,"POST","orders.cancel",
                               route_params,method_params)
  }, error=function(e){
    message(e$message)
  })

  r <- toJSON(r, pretty = TRUE, auto_unbox = TRUE, null = "null")

  return(r)
}

#'Function to get the report of the day (Orders History).
#'@description Retrieves all orders for the day.
#'@param object An object of type sharekhanconnect with valid api_key and
#'access_token.
#'@param param This contains the request parameters.
#'@return Returns a json object with orders details, if successful.
#'@export
get_report_day <- function(object, param){
  r <- NULL
  route_params <- list("customerId" = param$customerId)
  method_params <- list()
  tryCatch({r <- rest_api_call(object,"GET", "report.day", route_params, method_params)
  }, error=function(e){
    message(e$message)
  })

  r <- toJSON(r, pretty = TRUE, auto_unbox = TRUE, null = "null")

  return(r)
}

#'Function to get the order history.
#'@description Retrieve an order's history.
#'@param object An object of type sharekhanconnect with valid api_key and
#'access_token.
#'@param param This contains the request parameters.
#'@return Returns a json object with order details, if successful.
#'@export
get_report_history <- function(object, param){
  r <- NULL
  route_params <- list("exchange" = param$exchange, "customerId" = param$customerId, "orderId" = param$orderId)
  method_params <- list()
  tryCatch({r <- rest_api_call(object,"GET", "report.history", route_params, method_params)
  }, error=function(e){
    message(e$message)
  })

  r <- toJSON(r, pretty = TRUE, auto_unbox = TRUE, null = "null")

  return(r)
}

#'Function to get the trade history.
#'@description Gets trade history.
#'@param object An object of type sharekhanconnect with valid api_key and
#'access_token.
#'@param param This contains the request parameters.
#'@return Returns a json object with trade details, if successful.
#'@export
get_report_trade <- function(object, param){
  r <- NULL
  route_params <- list("exchange" = param$exchange, "customerId" = param$customerId, "orderId" = param$orderId)
  method_params <- list()
  tryCatch({r <- rest_api_call(object,"GET", "report.trade", route_params, method_params)
  }, error=function(e){
    message(e$message)
  })

  r <- toJSON(r, pretty = TRUE, auto_unbox = TRUE, null = "null")

  return(r)
}

#'Function to get the trades details.
#'@description Gets trades history.
#'@param object An object of type sharekhanconnect with valid api_key and
#'access_token.
#'@param param This contains the request parameters.
#'@return Returns a json object with trades details, if successful.
#'@export
get_trade_position <- function(object, param){
  r <- NULL
  route_params <- list("customerId" = param$customerId)
  method_params <- list()
  tryCatch({r <- rest_api_call(object,"GET", "trades.position", route_params, method_params)
  }, error=function(e){
    message(e$message)
  })

  r <- toJSON(r, pretty = TRUE, auto_unbox = TRUE, null = "null")

  return(r)
}

#'Function to get the holdings details.
#'@description It contains user's long term stocks.
#'@param object An object of type sharekhanconnect with valid api_key and
#'access_token.
#'@param param This contains the request parameters.
#'@return Returns a json object with holdings details, if successful.
#'@export
get_holdings <- function(object, param){
  r <- NULL
  route_params <- list("customerId" = param$customerId)
  method_params <- list()
  tryCatch({r <- rest_api_call(object,"GET", "orders.holding", route_params, method_params)
  }, error=function(e){
    message(e$message)
  })

  r <- toJSON(r, pretty = TRUE, auto_unbox = TRUE, null = "null")

  return(r)
}

#'Function to get the active scrips for the day.
#'@description Gets active scrips for the day.
#'@param object An object of type sharekhanconnect with valid api_key and
#'access_token.
#'@param param This contains the request parameters.
#'@return Returns a json object with scrips details, if successful.
#'@export
get_master <- function(object, param){
  r <- NULL
  route_params <- list("exchange" = param$exchange)
  method_params <- list()
  tryCatch({r <- rest_api_call(object,"GET", "orders.master", route_params, method_params)
  }, error=function(e){
    message(e$message)
  })

  r <- toJSON(r, pretty = TRUE, auto_unbox = TRUE, null = "null")

  return(r)
}

#'Function to get the historical data upto last traded date.
#'@description Gets historical data upto last traded date. The candle data is
#'based on minutes. The highest timeframe is available upto 90 minutes.
#'@param object An object of type sharekhanconnect with valid api_key and
#'access_token.
#'@param param This contains the request parameters.
#'@return Returns a json object with historical data, if successful.
#'@export
get_historical <- function(object, param){
  r <- NULL
  route_params <- list("exchange" = param$exchange, "scripcode" = param$scripcode, "interval" = param$interval)
  method_params <- list()
  tryCatch({r <- rest_api_call(object,"GET", "orders.historical", route_params, method_params)
  }, error=function(e){
    message(e$message)
  })

  r <- toJSON(r, pretty = TRUE, auto_unbox = TRUE, null = "null")

  return(r)
}


