source("R/routes.R")
source("R/exceptions.R")
source("R/utils.R")

setClass("sharekhanconnect",
         representation(
           api_key = "character",
           api_secret = "character",
           request_token = "character",
           access_token = "character",
           routes = "list",
           root = "character",
           login = "character"
         )
)

is_connection_object <- function(object){
  return(class(object) == "sharekhanconnect")
}
is_valid_api_key <- function(object){
  if(!is_connection_object(object))return(FALSE)
  return(length(object@api_key) > 0)
}
is_valid_api_secret <- function(object){
  if(!is_connection_object(object))return(FALSE)
  return(length(object@api_secret) > 0)
}
is_valid_root <- function(object){
  if(!is_connection_object(object))return(FALSE)
  return(length(object@root) > 0)
}
is_valid_routes <- function(object){
  if(!is_connection_object(object))return(FALSE)
  return(length(object@routes) > 0)
}
is_valid_connection <- function(object){
  if(!is_connection_object(object))return(FALSE)
  return(
    is_valid_api_key(object) &
      is_valid_api_secret(object) &
      is_valid_root(object) &
      is_valid_routes(object)
  )
}
is_valid_access_token <- function(object){
  if(!is_connection_object(object))return(FALSE)
  return(length(object@access_token) > 0)
}
is_logged_connection <- function(object){
  if(!is_connection_object(object))return(FALSE)
  return(
    is_valid_connection(object) &
      is_valid_request_token(object)
  )
}
is_valid_request_token <- function(object){
  if(!is_connection_object(object))return(FALSE)
  return(length(object@request_token) > 0)
}
is_api_connected <- function(object){
  if(!is_connection_object(object))return(FALSE)
  return(
    is_logged_connection(object) &
      is_valid_access_token(object)
  )
}

#'Function to set the request token in the sharekhanconnect object.
#'@description Function to set request token (obtained from a successful
#'login flow) in the sharekhanconnect object.
#'@param object an S4 object of type sharekhanconnect.
#'@param request_token a string containing the request token from login flow.
#'@details  This function allows to set the request token of the sharekhan
#'connect object. The request token is obtained from a successful login flow.
#'After the login flow, the user is redirected to a pre-specified URL. The
#'request token is posted alongwith as a query parameter.
#'@return Returns the object with request token field set.
#'@seealso \code{\link[sharekhanconnect]{create_connection_object}}
#'@export
set_request_token <- function(object,request_token){
  if(!is_connection_object(object)){
    message("Invalid sharekhan connect object")
  }
  if(any(class(request_token)=="character")){
    object@request_token <- request_token
  }else{
    message("Invalid request token")
  }
  return(object)
}

#'Function to set the access token in the sharekhanconnect object.
#'@description This function sets the access token (obtained by sending a
#'hash of api key, request token and api secret) inside the sharekhanconnect
#'object.
#'@param object a sharekhanconnect object.
#'@param access_token Access token.
#'@return sharekhan connect object with access token set.
#'@seealso \code{\link[sharekhanconnect]{create_connection_object}}
#'@seealso \code{\link[sharekhanconnect]{fetch_access_token}}
#'@export
set_access_token <- function(object,access_token){
  if(!is_connection_object(object)){
    message("Invalid sharekhan connect object")
  }
  if(any(class(access_token)=="character")){
    object@access_token <- access_token
  }else{
    message("Invalid access token")
  }
  return(object)
}

#'Function to get the login URL.
#'@description Function to get the login URL (the login entry + api_key) from
#'the sharekhanconnect object.
#'@param object An object of sharekhanconnect type
#'@param vendor_key  A vendor_key with default value NULL
#'@return Returns the login URL.
#'@export
get_login_url <- function(object, version_id=NULL){
  if(!is_valid_connection(object)){
    message("Invalid sharekhan connect object")
    return(NULL)
  }

  if(!is.null(version_id)) {
    return(paste0(object@login,"?&api_key=",object@api_key,"&user_id=1234","&version_id=",version_id))
  } else {
    print("Version not provided")
  }

  return(paste0(object@login,"?&api_key=",object@api_key,"&user_id=12345"))
}

get_api_endpoint <- function(object,endpoint,route_params=list()){

  if(!is_valid_connection(object)){
    message("Invalid sharekhan connect object")
    return(NULL)
  }
  url <- paste0(object@root,glue::glue(object@routes[[endpoint]],
                                       .envir=create_route_env(route_params)))
  return(url)
}

#'Create a sharekhanconnect connection object.
#'@description This function returns an object of sharekhanconnect type, given the
#'input parametrs.
#'@param params a list mapping object property name to values. See details
#'for more.
#'@details This function creates an S4 object from the input parameters. The
#'required parameters are api_key, api_secret. If root (the api root), routes
#'(the list of api endpoints) and login (the login url) is supplied, they are
#'used. Otherwise the internally defined values are used. Only a single
#'instance of this class per `api_key` should be initialized.
#'Parameters should be a named list of all inputs to create a connection.
#'The important attributes are: api_key( character) - your api key;
#'api_secret(character) - your api secret (both of these are available on
#'the developer page in your sharekhan app, and must be supplied during object
#'creation); request_token(character) - should be set after a successful
#'login flow; access_token(character) - obtained using fetch_access_token
#'@return S4 object for sharekhanconnect which can be used in further api calls.
#'@export
create_connection_object <- function(params){
  object = methods::new("sharekhanconnect")

  tryCatch({
    object@api_key = params[["api_key"]]
    object@api_secret = params[["api_secret"]]
    object@request_token = params[["request_token"]]
    object@access_token = params[["access_token"]]
    object@root = ifelse(is.null(params[["root"]]),api_root,
                         params[["root"]])
    if(is.null(params[["routes"]])){
      object@routes = api_routes
    } else{
      object@routes = params[["routes"]]
    }
    object@login = ifelse(is.null(params[["login"]]),api_login,
                          params[["login"]])
  }, error=function(e){
    message(e$message)
    return(NULL)
  })

  if(!is_valid_connection(object)){
    message("Failed to create sharekhan connect object")
    return(NULL)
  }

  return(object)
}

#'Fetch access token given the api key, secret and a request token.
#'@description This function returns an access token given the inputs, that
#'can be used for subsequent api calls.
#'@param object An object of type sharekhanconnect, must have request token set.
#'@usage fetch_access_token(object)
#'@details This function generate the `access_token` by exchanging
#'`request_token`.The `request_token` obtained after the login flow. the
#'`access_token` required for all subsequent requests. The object passed in
#'this call must already have `request_token` set (along with api_key and
#'api_secret). A successful call also set the user data within the object.
#'@return A string containing the access token.
#'@seealso \code{\link[sharekhanconnect]{set_request_token}}
#'@export
fetch_access_token <- function(object){
  access_token <- NULL
  if(!is_logged_connection(object)){
    message("Invalid sharekhan connect object or request token not set")
    return(NULL)
  }

  # URL-SAFE
  parse_token = py$conversion_three(object@request_token)
  decStr = decryptionMethod(parse_token, object@api_secret)

  # Non-URL SAFE
  # decStr = decryptionMethod(object@request_token, object@api_secret)

  input_string <- py$conversion_two(decStr)
  # print(input_string)
  original <- strsplit(input_string, "\\|")[[1]]
  manipulated <- paste(original[2], "|", original[1], sep = "")
  # print(manipulated)
  output_string <- py$conversion_one(manipulated)
  encStr = encryptionMethod(output_string, object@api_secret)
  object@request_token <- py$conversion_two(encStr)
  # object@request_token <- py$conversion_two(encStr)
  # print(object@request_token)

  # Url safe
  # post_body <- list(
  #   "apiKey"= object@api_key,
  #   "requestToken"= object@request_token,
  #   "userId" = 12345,
  #   "versionId" = 1005
  # )

  # Non-url safe
  post_body <- list(
    "apiKey"= object@api_key,
    "requestToken"= object@request_token,
    "userId" = 12345
  )
  response <- NULL
  tryCatch({
    r <- httr::POST(get_api_endpoint(object,"api.token"),body=post_body, encode = "json")
    response <- httr::content(r)
    access_token <- response$data$token
  }, error=function(e){
    message(e$message)
    return(NULL)
  })

  if(r$status_code != 200){
    r <- httr::content(r)
    message(paste0(r$error_type,": ",r$message))
    return(NULL)
  }

  if(class(access_token)!="character"){
    message(InvalidAccessTokenException)
    return(NULL)
  }
  object = set_access_token(object,access_token)
  if(!is.null(response)) response <- toJSON(response, pretty = TRUE, auto_unbox = TRUE)
  return(response)
}


