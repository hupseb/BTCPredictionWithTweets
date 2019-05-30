options(scipen=999)
Sys.setenv(TZ='UTC')

install.packages("httpuv")
install.packages("devtools")
install.packages("RODBC")
library("devtools")
devtools::install_github("mkearney/rtweet", force = TRUE, ref="d5c3233")
library("rtweet")
library("RODBC")
library("httpuv")

counter <- 0
print(counter)
print(Sys.time())
conn <- odbcDriverConnect('driver={SQL Server};server=localhost\\SQLEXPRESS;database=BitcoinTwts;trusted_connection=true')
tk <- create_token(app="BitcoinTweetsApp",consumer_key="xxx",consumer_secret="xxx", access_token="xxx", access_secret="xxx", set_renv = FALSE)
query = sprintf("SELECT [screen_name], [description] FROM [BitcoinTwts].[dbo].[f0_usersOutDescription] WHERE [screen_name] NOT IN (SELECT [screen_name] FROM [f0_usersOutDesc_bannedOrDeleted]) AND [screen_name] NOT IN (SELECT [screen_name] FROM [f0_usersOutDesc]) ORDER BY [screen_name]")
usersoutdesc <- sqlQuery(conn, query, errors = TRUE)
for(i in 1:nrow(usersoutdesc)) {
  item <- usersoutdesc[i,]
  print(item$screen_name)
  rtn <- NULL
  rtn <- tryCatch({get_timeline(toString(item$screen_name), n = 1, token = tk)},
                     warning = function(w){
                       print('------')
                       print(w$message)
                       print(Sys.time())
                       print('------')
                       if(grepl('limit', w$message)){
                         print("sleeping for ~3 minutes")
                         Sys.sleep(190)   
                         tk <- create_token(app="BitcoinTweetsApp",consumer_key="xxx",consumer_secret="xxx", access_token="xxx", access_secret="xxx", set_renv = FALSE)
                         return ("slept")
                       }else if(grepl('Not authorized', w$message)){
                         return ("suspended")
                       }else if(grepl('page', w$message)){
                         return ("deleted")
                       }else{
                         return ("unknown - maybe not tweets etc.")
                       }
                     })
  if(is.character(rtn) && grepl("slept",rtn)){
    next
  } else if (is.character(rtn) && grepl("suspended",rtn)){
    item$description <- "suspended"
    sqlSave(conn, item, tablename = "f0_usersOutDesc_bannedOrDeleted", append = TRUE, rownames = FALSE)
  } else if (is.character(rtn) && grepl("deleted",rtn)){
    item$description <- "deleted"
    sqlSave(conn, item, tablename = "f0_usersOutDesc_bannedOrDeleted", append = TRUE, rownames = FALSE)    
  } else if(!is.null(rtn) && is.list(rtn) && nrow(rtn) > 0){
    sqlSave(conn, rtn, tablename = "f0_usersOutDesc", append = TRUE, rownames = FALSE)
  } else {
    item$description <- "unknown - maybe not tweets etc."
    sqlSave(conn, item, tablename = "f0_usersOutDesc_bannedOrDeleted", append = TRUE, rownames = FALSE)
  }
  Sys.sleep(2)
  counter <- i
}

odbcClose(conn)
odbcCloseAll()
