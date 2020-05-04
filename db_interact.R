library(RSQLite)


# db_interact.get_param <- function(query, param, as_vector=FALSE){
#   sqlite <- dbDriver('SQLite')
#   db_conn <- dbConnect(sqlite, dbname='/media/HDD/UDESC/OpenStack/OpenStack-Plots/network_metering_experiment.db')
#   result_set <- dbSendQuery(db_conn, query)
#   fetch_result <- dbFetch(result_set)
#   dbClearResult(result_set)
#   return( ifelse(as_vector, as.vector(t(fetch_result)), fetch_result) )
# }


db_interact.simple_get <- function(query, params=list(), as_vector=FALSE){
  sqlite <- dbDriver('SQLite')
  db_conn <- dbConnect(sqlite, dbname='/media/HDD/UDESC/OpenStack/OpenStack-Plots/network_metering_experiment.db')
  on.exit(dbDisconnect(db_conn))
  result_set <- dbSendQuery(db_conn, query)
  if(length(params)){
    dbBind(result_set, params)
  }
  fetch_result <- dbFetch(result_set)
  dbClearResult(result_set)

  if(as_vector){
    return(as.vector(t(fetch_result)))
  }

  return(fetch_result)
}

#FIXME should create 1 function with parameters to coulmn and table :)
db_interact.get_services <- function(){
  service_query <- "SELECT service_name FROM Service"
  return(db_interact.simple_get(service_query, as_vector=TRUE))
}

db_interact.get_images <- function(){
  service_query <- "SELECT image_name FROM OsImage"
  return(db_interact.simple_get(service_query, as_vector=TRUE))
}

db_interact.get_nics <- function(){
  service_query <- "SELECT network_interface FROM Metering GROUP BY network_interface"
  return(db_interact.simple_get(service_query, as_vector=TRUE))
}

db_interact.get_exec_list_by_image <- function(image){
  service_query <- "
        SELECT exec_id FROM Execution exec
          JOIN OsImage img ON exec.image_id = img.image_id
          WHERE image_name = ?"
  return(db_interact.simple_get(service_query, params=list(image), as_vector=TRUE))
}


#TODO: Refactor
db_interact.get <- function(){
  sqlite <- dbDriver('SQLite')
  db_conn <- dbConnect(sqlite, dbname='/media/HDD/UDESC/OpenStack/OpenStack-Plots/network_metering_experiment.db')
  on.exit(dbDisconnect(db_conn))

  default_query <- "

  SELECT  pkt.time as 'time',
          pkt.size_bytes as MB,
          pkt.sniff_timestamp as ts,
          img.image_name as image,
          met.network_interface as nic

  FROM OsImage img
  JOIN Execution ex ON img.image_id = ex.image_id
  JOIN Operation op ON ex.exec_id = op.exec_id
  JOIN Metering met ON op.operation_id = met.operation_id
  JOIN PacketInfo pkt ON met.metering_id = pkt.metering_id

  WHERE ex.exec_id = 1
  -- AND met.network_interface = 'lo'
  -- AND met.network_interface != 'lo'
  AND op.type = 'CREATE'
  AND img.image_name = 'fedora31' "

  result_set <- dbSendQuery(db_conn, default_query)
  fetch_result <- dbFetch(result_set)
  dbClearResult(result_set)

  fetch_result$time <- as.numeric(fetch_result$time)
  fetch_result$ts <- as.numeric(fetch_result$ts)
  fetch_result$MB <- fetch_result$MB/1000000

  return(fetch_result)
}
