em <- function(v){
  if(is.null(v))
    return(NA)
  else
    return(format(v, scientific=FALSE))
}

fd <- function(datestring, format="datetime"){
  if (format=="datetime"){
    date <- as.POSIXct(datestring, format="%a %b %d %H:%M:%S %z %Y", tz="UTC")
  }
  if (format=="date"){
    date <- as.Date(datestring, format="%a %b %d %H:%M:%S %z %Y")
  }   
  return(date)
}

txt <- function(tweet){
  if(!is.null(tweet[["extended_tweet"]][["full_text"]]))
    return(tweet[["extended_tweet"]][["full_text"]])
  else
    return(tweet[["text"]])
}

ls <- function(list){
  if(length(list) > 0)
    return(paste(list, collapse=', ' ))
  else
    return(NA)
}