library(jsonlite)
library(base64)

setClass("sharekhanwebSocket",
         representation(

           access_token = "character",
           api_key="character",
           ws_url = "character",
           action="character",
           key = "list",
           value = "list"
         )
)

sharekhan_connect_object<-function(params){
  object = methods::new("sharekhanwebSocket")
  tryCatch({

    object@access_token=ifelse(is.null(params[["access_token"]]),message("access token cannot be blank"),
                               params[["access_token"]])
    object@api_key=ifelse(is.null(params[['api_key']]),'',params[["api_key"]])

    object@ws_url = ifelse(is.null(params[["ws_url"]]),api_websocket,
                           params[["ws_url"]])

    object@action=ifelse(is.null(params[["action"]]),message("action cannot be blank"),
                         params[["action"]])

    object@key=ifelse(is.null(params[["key"]]),message("key cannot be blank"),
                      params[["key"]])
    object@value=ifelse(is.null(params[["value"]]),message("value cannot be blank"),
                        params[["value"]])

  }, error=function(e){
    message("in error function",e$message)
  })
  return(object)
}

sharekhanSocket.connect<-(function(object){
  url<-paste(object@ws_url,"?ACCESS_TOKEN=",object@access_token,sep = "")
  message(url)
  ws <- websocket::WebSocket$new(url,autoConnect = FALSE)
  #message(ws)
  is_open= FALSE
  ws$onOpen(function(event){
    is_open<-TRUE
    message("connection is opened")
    fetch_data()
    send_ticks()
  })
  fetch_data<-function(){
    message("Fetching data")
    ws$send(toJSON(list("action" = object@action,"key" = object@key, "value" = object@value), auto_unbox = TRUE))
  }
  send_ticks<-function(){
    message("heartbeat")
    later::later(send_ticks,10)
  }

  ws$onMessage(function(event) {
    message(base64enc::base64decode(event$data))
    msg_data <- event$data
    cat("Client received message: \n",msg_data, "\n")

  })
  ws$onError(function(){
    cat("Error in the connection")
  })

  ws$connect()
})



