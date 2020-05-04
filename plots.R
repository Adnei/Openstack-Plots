library(RSQLite)
source('objects.R')
source('db_interact.R')
source('data_handler.R')
# TODO: Plots volume per packet and volume per second

plots_schema <- objects.build_schema()


# for(schema in plots_schema){
#   traffic <- db_interact.get_traffic(sch)
# }















# data <- db_interact.get()
# reference_time <- min(data$ts)
#
# coordinates <- data_handler.groupPackets(data, from=0, step=1, to=ceiling(max(data$ts) - min(data$ts)))


# pdf('')
# par(mar = c(6, 6, 3.5, 3.5), mgp = c(5, 1, 0))
# plot(
#   axis_x,axis_y,
#   lwd=1,
#   type = 'n',
#   xlab='Tempo (s)',
#   ylab = 'Volume de Tráfego (MB)',
#   cex.lab=1.2,
#   font.lab=2,
#   xaxt='n',
#   main=title
# )
