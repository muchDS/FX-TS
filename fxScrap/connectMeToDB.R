connectMeToDB <- function(){
  
  
  address <- "23185.p.tld.pl"
  dbName <- "pg23185_muchdsdb"
  dbUser <- "pg23185_muchdsdb"
  pass <- "4"
  return(
  connection <- dbConnect(PostgreSQL(),
                          dbname = dbName,
                          user = dbUser,
                          password = pass,
                          host = address)
  )
}
