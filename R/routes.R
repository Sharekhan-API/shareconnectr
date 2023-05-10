api_root = "https://api.sharekhan.com/skapi/services"
api_login = "https://api.sharekhan.com/skapi/auth/login.html"
api_websocket = "wss://stream.sharekhan.com/skstream/api/stream"

api_routes <- list()
api_routes[["api.token"]] = "/access/token"

api_routes[["orders.fund"]] = "/limitstmt/{exchange}/{customerId}"
api_routes[["orders.place"]] = "/orders"
api_routes[["orders.modify"]] = "/orders"
api_routes[["orders.cancel"]] = "/orders"

api_routes[["report.day"]] = "/reports/{customerId}"
api_routes[["report.history"]] = "/reports/{exchange}/{customerId}/{orderId}"
api_routes[["report.trade"]] = "/orders/{exchange}/{customerId}/{orderId}/trades"

api_routes[["trades.position"]] = "/trades/{customerId}"

api_routes[["orders.holding"]] = "/holdings/{customerId}"
api_routes[["orders.master"]] = "/master/{exchange}"
api_routes[["orders.historical"]] = "/historical/{exchange}/{scripcode}/{interval}"

