library(reticulate)

py_config <- import("cryptography.hazmat.backends", convert = TRUE)
py_cipher <- import("cryptography.hazmat.primitives", convert = TRUE)$ciphers$Cipher
py_algorithms <- import("cryptography.hazmat.primitives.ciphers.algorithms", convert = TRUE)
py_modes <- import("cryptography.hazmat.primitives.ciphers.modes", convert = TRUE)
py_base64 <- import("base64", convert = TRUE)
py_urllib <- import("urllib", convert = TRUE)



convert_three <- "def conversion_three(data):
                    import urllib.parse
                    return urllib.parse.unquote(data)"
py_run_string(convert_three)

convert_one <- "def conversion_one(data):
                    return data.encode('utf-8')"
py_run_string(convert_one)

convert_two <- "def conversion_two(data):
                    return data.decode('utf-8')"
py_run_string(convert_two)


iv <- py_base64$urlsafe_b64decode('AAAAAAAAAAAAAAAAAAAAAA==')

encryptionMethod <- function(plaintext, key) {
  key_encoded <- py$conversion_one(key)
  raw <- plaintext
  encryptor <- py_cipher(py_algorithms$AES(key_encoded), py_modes$GCM(iv, NULL, 16), backend=py_config$default_backend())$encryptor()
  convert <- "def convert_byte(first, second):
                  return first + second"
  py_run_string(convert)
  ciphertext <- py$convert_byte(encryptor$update(raw), encryptor$finalize())
  result <- py$convert_byte(ciphertext, encryptor$tag)
  return(base64UrlEncode(result))
}

decryptionMethod <- function(ciphertext, key) {
  key_encoded <- py$conversion_one(key)
  ciphertext <- py$conversion_one(ciphertext)
  enc_str <- base64UrlDecode(ciphertext)
  convert <- "def conversion(data):
                  return data[:-16]"
  py_run_string(convert)
  enc <- py$conversion(enc_str)
  decryptor <- py_cipher(py_algorithms$AES(key_encoded), py_modes$GCM(iv), backend=py_config$default_backend())$decryptor()
  return(decryptor$update(enc))
}

base64UrlEncode <- function(data) {
  convert <- "def conversion(data):
                  return data.rstrip(b'=')"
  py_run_string(convert)

  # Use this for - URL-SAFE
  # result = py$conversion(py_base64$urlsafe_b64encode(data))

  # Use this for - Non-URL SAFE
  result = py$conversion(py_base64$b64encode(data))
  return(result);
}

base64UrlDecode <- function(base64Url) {
  convert <- "def conversion(data):
                  padding = b'=' * (4 - (len(data) % 4))
                  return data+padding"
  py_run_string(convert)
  result <- py$conversion(base64Url)

  # Use this for - URL-SAFE
  # return(py_base64$urlsafe_b64decode(result))

  # Use this for - Non-URL SAFE
  return(py_base64$b64decode(result))
}


create_route_env <- function(params){
  if(!is.list(params)){
    params <- list()
  }
  route_env = list2env(params)
  return(route_env)
}

