connectMeToDB <- function(){
  
  
  address <- "23185.p.tld.pl"
  dbName <- "pg23185_muchdsdb"
  dbUser <- "pg23185_muchdsdb"
  pass <- "!Muchdsdbpass"
  return(
  connection <- dbConnect(PostgreSQL(),
                          dbname = dbName,
                          user = dbUser,
                          password = pass,
                          host = address)
  )
}