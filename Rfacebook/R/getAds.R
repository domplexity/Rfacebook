getads <- function(campaign, token, fields,breakdowns,limit){
  
  #todo: ist leer? ist vector?
  
  url <- paste0("https://graph.facebook.com/v2.5/",
                campaign,
                "/insights/?level=ad&fields=",
                paste(fields,collapse = ","),
                "&breakdowns=",
                paste(breakdowns,collapse = ","),
                "&limit=",
                limit)
  
  #print(url)
  
  content <- callAPI(url=url, token=token)
  
  # error traps: retry 3 times if error
  error <- 0
  while (length(content$error_code)>0){
    cat("Error!\n")
    Sys.sleep(0.5)
    error <- error + 1
    content <- callAPI(url=url, token=token)		
    if (error==3){ stop(content$error_msg) }
  }
  if (length(content)==0){ 
    stop("Post could not be found")
  }
  
  return(content)
  
}
    