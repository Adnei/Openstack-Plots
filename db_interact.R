library(RSQLite)
sqlite <- dbDriver('SQLite')

DEFAULT_DATABASE <- '/media/HDD/UDESC/OpenStack/tests_beta/final_version/exp_1/network_metering_experiment.db'

db_interact.simple_get <- function(query, params=list(), as_vector=FALSE){
  db_conn <- dbConnect(sqlite, dbname=DEFAULT_DATABASE)
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
db_interact.get_traffic_info <- function(image_name, operation, execution_id){
  params <- list(image_name = image_name,
    operation = operation,
    execution_id = execution_id)

  db_conn <- dbConnect(sqlite, dbname=DEFAULT_DATABASE)
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

  WHERE ex.exec_id = :execution_id
  -- AND met.network_interface = 'lo'
  -- AND met.network_interface != 'lo'
  AND op.type = :operation
  AND img.image_name = :image_name "

  result_set <- dbSendQuery(db_conn, default_query)
  dbBind(result_set, params)
  fetch_result <- dbFetch(result_set)
  dbClearResult(result_set)

  fetch_result$time <- as.numeric(fetch_result$time)
  fetch_result$ts <- as.numeric(fetch_result$ts)
  fetch_result$MB <- fetch_result$MB/1000000

  return(fetch_result)
}



db_interact.get_api_calls_counter <- function(image_name, operation, service, execution_id){
  params <- list(image_name = image_name,
    operation = operation,
    execution_id = execution_id,
    service = service)

    db_conn <- dbConnect(sqlite, dbname=DEFAULT_DATABASE)
    on.exit(dbDisconnect(db_conn))

    default_query <- "
      SELECT count(ri.user_agent) as calls

      FROM OsImage img
      JOIN Execution ex ON img.image_id = ex.image_id
      JOIN Operation op ON ex.exec_id = op.exec_id
      JOIN Metering met ON op.operation_id = met.operation_id
      JOIN PacketInfo pkt ON met.metering_id = pkt.metering_id
      JOIN RequestInfo ri ON pkt.packet_id = ri.packet_id
      JOIN Service sv ON ri.server_id = sv.service_id

      WHERE ex.exec_id = :execution_id
      AND op.type = :operation
      AND img.image_name = :image_name
      AND sv.service_name = :service
    "

    result_set <- dbSendQuery(db_conn, default_query)
    dbBind(result_set, params)
    fetch_result <- dbFetch(result_set)
    dbClearResult(result_set)

    return(as.numeric(fetch_result$calls))
}


db_interact.get_total_traffic <- function(image_name, operation, service, execution_id){
  service_cond <- 'AND sv.service_name = :service'
  params <- list(image_name = image_name,
    operation = operation,
    execution_id = execution_id,
    service = service)

  if( (service == 'MISC') || (is.null(service)) ){
    service_cond <- 'AND sv.service_name IS NULL'
    params <- list(image_name = image_name,
      operation = operation,
      execution_id = execution_id)
  }

  db_conn <- dbConnect(sqlite, dbname=DEFAULT_DATABASE)
  on.exit(dbDisconnect(db_conn))

  total_traffic_query <- "
    SELECT  sum(pkt.size_bytes) as traffic

    FROM OsImage img
    JOIN Execution ex ON img.image_id = ex.image_id
    JOIN Operation op ON ex.exec_id = op.exec_id
    JOIN Metering met ON op.operation_id = met.operation_id
    JOIN PacketInfo pkt ON met.metering_id = pkt.metering_id
    LEFT JOIN Service sv ON pkt.service_id = sv.service_id

    WHERE ex.exec_id = :execution_id
    AND op.type = :operation
    AND img.image_name = :image_name
  "
  total_traffic_query <- paste(total_traffic_query, service_cond)

  result_set <- dbSendQuery(db_conn, total_traffic_query)
  dbBind(result_set, params)
  fetch_result <- dbFetch(result_set)
  dbClearResult(result_set)

  return(as.numeric(fetch_result$traffic/1000000)) #Returns in MB


}
