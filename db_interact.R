library(RSQLite)
source('params.R')
sqlite <- dbDriver('SQLite')

db_interact.get_excluded_images <- function(db.df, db_list){
  result_db.df <- data.frame()
  for(db in db_list){
    params.DB_PATH <- unique(db.df[grep(db, db.df$database),]$database)
    if(length(params.DB_PATH) == 0) next
    db_images <- db_interact.get_images(database=params.DB_PATH)
    # print(db_images)
    selected_images <- unique(db.df[grep(db, db.df$database),]$image)
    # print(selected_images)
    excluded_images <- db_images[!(db_images %in% selected_images)]
    # print(excluded_images)
    if(length(excluded_images) > 0){
      aux_result.df <- data.frame(
        database = params.DB_PATH,
        excluded_image = excluded_images,
        stringsAsFactors = FALSE
      )
      result_db.df <- rbind(result_db.df, aux_result.df)
    }
  }
  return(result_db.df)
}

db_interact.filter_images_by_db <- function(db_list){
  all_images <- c()
  exec_arr <- c()
  db_arr <- c()
  for(db in db_list){
    params.DB_PATH <- paste0(params.COMMON_PATH, db)
    db_images <- db_interact.get_images(database=params.DB_PATH)
    n_exec <- length(db_interact.get_exec_list_by_image(db_images[1], database=params.DB_PATH))
    new_images <- db_images
    conflict_idx <- match(db_images, all_images)
    conflict_idx <- conflict_idx[!is.na(conflict_idx)]

    if(length(conflict_idx) > 0){
      old_n_exec <- exec_arr[c(conflict_idx)][1]
      if(n_exec > old_n_exec){
        db_arr[c(conflict_idx)] = params.DB_PATH
        exec_arr[c(conflict_idx)] = n_exec
      }
      conflict_images <- all_images[c(conflict_idx)]
      remove_idx <- match(conflict_images, new_images)
      remove_idx <- remove_idx[!is.na(remove_idx)]
      new_images <- new_images[-remove_idx]
    }


    all_images <- c(all_images, new_images)
    exec_arr <- c(exec_arr, rep(n_exec, length(new_images)))
    db_arr <- c(db_arr, rep(params.DB_PATH, length(new_images)))
  }

  if(length(db_arr) != length(exec_arr) || length(db_arr) != length(all_images)){
    print('ERROR!')
    print('Something went wrong during the database.dataframe (db.df) creation.')
    return(data.frame())
  }

  db.df <- data.frame(database = db_arr,
    n_exec = exec_arr,
    image = all_images,
    stringsAsFactors = FALSE)
  return(db.df)
}

db_interact.simple_get <- function(query, params=list(), as_vector=FALSE, database=params.DEFAULT_DATABASE){
  db_conn <- dbConnect(sqlite, dbname=database)
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
db_interact.get_services <- function(database=params.DEFAULT_DATABASE){
  service_query <- "SELECT service_name FROM Service"
  return(db_interact.simple_get(service_query, as_vector=TRUE, database=database))
}

db_interact.get_images <- function(database=params.DEFAULT_DATABASE){
  service_query <- "SELECT image_name FROM OsImage"
  return(db_interact.simple_get(service_query, as_vector=TRUE, database=database))
}

db_interact.get_nics <- function(database=params.DEFAULT_DATABASE){
  service_query <- "SELECT network_interface FROM Metering GROUP BY network_interface"
  return(db_interact.simple_get(service_query, as_vector=TRUE, database=database))
}

db_interact.get_exec_list_by_image <- function(image, database=params.DEFAULT_DATABASE){
  service_query <- "
        SELECT exec_id FROM Execution exec
          JOIN OsImage img ON exec.image_id = img.image_id
          WHERE image_name = ?"
  return(db_interact.simple_get(service_query, params=list(image), as_vector=TRUE, database=database))
}


#TODO: Refactor
#TODO Implement exclude image list for database
db_interact.get_traffic_info <- function(exclude_images=NULL, database=params.DEFAULT_DATABASE){
  # params <- list(exclude_images = exclude_images)

  db_conn <- dbConnect(sqlite, dbname=database)
  on.exit(dbDisconnect(db_conn))

  default_query <- "
    SELECT  pkt.time as 'time',
            pkt.size_bytes as MB,
            pkt.sniff_timestamp as ts,
            img.image_name as image,
            met.network_interface as nic,
            ex.exec_id as exec_id,
            op.type as operation

    FROM OsImage img
    JOIN Execution ex ON img.image_id = ex.image_id
    JOIN Operation op ON ex.exec_id = op.exec_id
    JOIN Metering met ON op.operation_id = met.operation_id
    JOIN PacketInfo pkt ON met.metering_id = pkt.metering_id


    -- WHERE img.image_name not in ()
    GROUP BY ex.exec_id, pkt.sniff_timestamp, op.type, img.image_name
  "

  result_set <- dbSendQuery(db_conn, default_query)
  # dbBind(result_set, params)
  fetch_result <- dbFetch(result_set)
  dbClearResult(result_set)

  fetch_result$exec_id <- as.numeric(fetch_result$exec_id)
  fetch_result$time <- as.numeric(fetch_result$time)
  fetch_result$ts <- as.numeric(fetch_result$ts)
  fetch_result$MB <- fetch_result$MB/1000000
  fetch_result$image <- as.character(fetch_result$image)
  fetch_result$operation <- as.character(fetch_result$operation)

  return(fetch_result)
}

db_interact.get_service_calls <- function(total=FALSE, database=params.DEFAULT_DATABASE){

  select_str <- '
  SELECT sv.service_name as service,
         count(sv.service_name) as calls,
         ex.exec_id as exec_id,
         op.type as operation,
         img.image_name as image'
  group_by_str <- 'GROUP BY sv.service_name, ex.exec_id, op.type, img.image_name'

  if(total == TRUE){
    select_str <- '
    SELECT count(sv.service_name) as calls,
           ex.exec_id as exec_id,
           op.type as operation,
           img.image_name as image'
    group_by_str <- 'GROUP BY ex.exec_id, op.type, img.image_name'
  }

  tables_str <- "
  FROM OsImage img
  JOIN Execution ex ON img.image_id = ex.image_id
  JOIN Operation op ON ex.exec_id = op.exec_id
  JOIN Metering met ON op.operation_id = met.operation_id
  JOIN PacketInfo pkt ON met.metering_id = pkt.metering_id
  JOIN RequestInfo ri ON pkt.packet_id = ri.packet_id
  JOIN Service sv ON ri.server_id = sv.service_id
  "

  default_query <- paste(select_str, tables_str, group_by_str)

  db_conn <- dbConnect(sqlite, dbname=database)
  on.exit(dbDisconnect(db_conn))

  result_set <- dbSendQuery(db_conn, default_query)
  fetch_result <- dbFetch(result_set)
  dbClearResult(result_set)

  fetch_result$calls <- as.numeric(fetch_result$calls)
  fetch_result$exec_id <- as.numeric(fetch_result$exec_id)

  return(fetch_result)
}


db_interact.get_service_traffic <- function(total=FALSE, database=params.DEFAULT_DATABASE){
  db_conn <- dbConnect(sqlite, dbname=database)
  on.exit(dbDisconnect(db_conn))



  group_by_str <- 'GROUP BY sv.service_name, ex.exec_id, op.type, img.image_name'
  select_str <- '
    SELECT  sv.service_name as service,
            sum(pkt.size_bytes) as traffic,
            ex.exec_id as exec_id,
            img.image_name as image,
            op.type as operation'
  if(total == TRUE){
    group_by_str <- 'GROUP BY ex.exec_id, op.type, img.image_name'
    select_str <- '
      SELECT  sum(pkt.size_bytes) as traffic,
              ex.exec_id as exec_id,
              img.image_name as image,
              op.type as operation'
  }
  tables_str <- '
    FROM OsImage img
    JOIN Execution ex ON img.image_id = ex.image_id
    JOIN Operation op ON ex.exec_id = op.exec_id
    JOIN Metering met ON op.operation_id = met.operation_id
    JOIN PacketInfo pkt ON met.metering_id = pkt.metering_id
    LEFT JOIN Service sv ON pkt.service_id = sv.service_id
  '

  traffic_query <- paste(select_str, tables_str, group_by_str)

  result_set <- dbSendQuery(db_conn, traffic_query)
  fetch_result <- dbFetch(result_set)
  dbClearResult(result_set)
  fetch_result$traffic <- as.numeric(fetch_result$traffic)/1000000 #Converts to MB
  fetch_result$exec_id <- as.numeric(fetch_result$exec_id)
  if(length(fetch_result$service) > 0 ){
    fetch_result$service <- as.character(fetch_result$service)
    if ( length(fetch_result[is.na(fetch_result$service),]$service) > 0){
      fetch_result[is.na(fetch_result$service),]$service <- 'MISC'
    }
  }
  fetch_result$image <- as.character(fetch_result$image)
  fetch_result$operation <- as.character(fetch_result$operation)

  return(fetch_result) #Returns in MB
}

#@DEPRECATED
db_interact.get_api_calls_counter <- function(image_name, operation, execution_id, service='TOTAL', database=params.DEFAULT_DATABASE){
  params <- list(image_name = image_name,
    operation = operation,
    execution_id = execution_id
  )

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
  "

  if(service != 'TOTAL'){
    params <- list(image_name = image_name,
      operation = operation,
      execution_id = execution_id,
      service = service)
    default_query <- paste(default_query, 'AND sv.service_name = :service')
  }

  db_conn <- dbConnect(sqlite, dbname=database)
  on.exit(dbDisconnect(db_conn))

  result_set <- dbSendQuery(db_conn, default_query)
  dbBind(result_set, params)
  fetch_result <- dbFetch(result_set)
  dbClearResult(result_set)

  return(as.numeric(fetch_result$calls))
}


#@DEPRECATED
db_interact.get_total_traffic <- function(image_name, operation, execution_id, service='TOTAL', database=params.DEFAULT_DATABASE){
  service_cond <- 'AND sv.service_name = :service'
  params <- list(image_name = image_name,
    operation = operation,
    execution_id = execution_id,
    service = service)

  if(service == 'TOTAL'){
    params <- list(image_name = image_name,
      operation = operation,
      execution_id = execution_id
    )
  } else if( (service == 'MISC') || (is.null(service)) ){
    service_cond <- 'AND sv.service_name IS NULL'
    params <- list(image_name = image_name,
      operation = operation,
      execution_id = execution_id)
  }

  db_conn <- dbConnect(sqlite, dbname=database)
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
  if(service != 'TOTAL'){
    total_traffic_query <- paste(total_traffic_query, service_cond)
  }
  result_set <- dbSendQuery(db_conn, total_traffic_query)
  dbBind(result_set, params)
  fetch_result <- dbFetch(result_set)
  dbClearResult(result_set)

  return(as.numeric(fetch_result$traffic/1000000)) #Returns in MB


}
