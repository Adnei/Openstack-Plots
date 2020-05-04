library(RSQLite)

data_handle.get <- function(){
  sqlite <- dbDriver('SQLite')
  db_conn <- dbConnect(sqlite, dbname='/media/HDD/UDESC/OpenStack/OpenStack-Plots/network_metering_experiment.db')
  on.exit(dbDisconnect(db_conn))

  default_query <- "

  SELECT  pkt.time as 'time',
          pkt.size_bytes as bytes,
          pkt.sniff_timestamp as ts,
          img.image_name as image

  FROM OsImage img
  JOIN Execution ex ON img.image_id = ex.image_id
  JOIN Operation op ON ex.exec_id = op.exec_id
  JOIN Metering met ON op.operation_id = met.operation_id
  JOIN PacketInfo pkt ON met.metering_id = pkt.metering_id

  WHERE ex.exec_id = 1
  -- AND met.network_interface = 'lo'
  AND met.network_interface != 'lo'
  AND op.type = 'CREATE'
  AND img.image_name = 'fedora31' "

  result_set <- dbSendQuery(db_conn, default_query)
  fetch_result <- dbFetch(result_set)
  dbClearResult(result_set)

  return(fetch_result)
}
