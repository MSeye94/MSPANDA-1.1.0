## Function to create database SQLite and add table into database 
CreateDatabaseReferenceMap <- function(path_to_database, listTables) {
  con<-dbConnect(RSQLite::SQLite(), dbname = path_to_database)
  
  for (i in 1:length(listTables)) {
    dbWriteTable(con, names(listTables)[i], listTables[[i]])
  }
  
  dbDisconnect(con)
}