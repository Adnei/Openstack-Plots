library(magrittr)
library(knitr)
library(kableExtra)
library(ggplot2)
library(dplyr)
library(grid)
library(gridExtra)
library(reshape)
library(stringr)
source('objects.R')
source('db_interact.R')
source('data_handler.R')
source('boxplot/boxplot_final_v2.R')

################################################################################
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

boxplot_data.df <- data_handler.df_names_fix(boxplot_data.df)
boxplot_data.df$traffic <- as.numeric(boxplot_data.df$traffic)

create.df <- boxplot_data.df[boxplot_data.df$operation == 'CREATE',]
shelve.df <- boxplot_data.df[boxplot_data.df$operation == 'SHELVE',]


################### BOXPLOT ####################
ylim_min_value <- min(c(min(log(create.df$traffic + 1)), min(log(shelve.df$traffic + 1))) )
ylim_max_value <- max(c(max(log(create.df$traffic + 1)), max(log(shelve.df$traffic + 1))) )
if (params.LANG == 'PT'){
  create_plot <- ggplot(create.df, aes(x=image, y=log(traffic +1 ), fill=image)) +
    geom_boxplot() +
    ggtitle('Operação CREATE') +
    ylab("Tráfego (escala log10 +1)") +
    xlab("Imagem de SO") +
    ylim(ylim_min_value, ylim_max_value) +
    theme_bw() +
    theme(legend.position="none", axis.text.x=element_blank())
  shelve_plot <- ggplot(shelve.df, aes(x=image, y=log(traffic +1 ), fill=image)) +
    geom_boxplot() +
    ggtitle('Operação SHELVE') +
    ylim(ylim_min_value, ylim_max_value) +
    xlab("Imagem de SO") +
    theme_bw() +
    theme(axis.text.y=element_blank(), axis.title.y=element_blank(), axis.text.x=element_blank())
  grid.newpage()
} else {
  create_plot <- ggplot(create.df, aes(x=image, y=log(traffic +1 ), fill=image)) +
    geom_boxplot() +
    ggtitle('CREATE Operation') +
    ylab("Traffic (log10 +1 scale)") +
    xlab("OS Image") +
    ylim(ylim_min_value, ylim_max_value) +
    theme_bw() +
    theme(legend.position="none", axis.text.x=element_blank())
  shelve_plot <- ggplot(shelve.df, aes(x=image, y=log(traffic +1 ), fill=image)) +
    geom_boxplot() +
    ggtitle('SHELVE Operation') +
    ylim(ylim_min_value, ylim_max_value) +
    xlab("OS Image") +
    theme_bw() +
    theme(axis.text.y=element_blank(), axis.title.y=element_blank(), axis.text.x=element_blank())
  grid.newpage()
}
# boxplot_grid_obj <- grid.draw(cbind(ggplotGrob(create_plot), ggplotGrob(shelve_plot), size = "last"))
ggsave(filename=params.BOXPLOT_FILE, plot=grid.draw(cbind(ggplotGrob(create_plot), ggplotGrob(shelve_plot), size = "last")))

################### ECDF ####################
ylim_min_real <- min(c(min(create.df$traffic), min(shelve.df$traffic)) )
ylim_max_real <- max(c(max(create.df$traffic), max(shelve.df$traffic)) )
if (params.LANG == 'PT'){
  create_ecdf <- ggplot(create.df, aes(traffic, color=image)) +
    stat_ecdf(geom = "step") +
    ggtitle('Operação CREATE') +
    xlab("Tráfego (MB)") +
    ylab('Cumulative Distribution Function (CDF)') +
    theme_bw() +
    theme(legend.position="none")
  shelve_ecdf <- ggplot(shelve.df, aes(traffic, color=image)) +
    stat_ecdf(geom = "step") +
    ggtitle('Operação SHELVE') +
    xlab("Tráfego (MB)") +
    theme_bw() +
    theme(axis.text.y=element_blank(), axis.title.y=element_blank())
  grid.newpage()
} else{
  create_ecdf <- ggplot(create.df, aes(traffic, color=image)) +
    stat_ecdf(geom = "step") +
    ggtitle('CREATE Operation') +
    xlab("Traffic (MB)") +
    ylab('Cumulative Distribution Function') +
    theme_bw() +
    theme(legend.position="none")
  shelve_ecdf <- ggplot(shelve.df, aes(traffic, color=image)) +
    stat_ecdf(geom = "step") +
    ggtitle('SHELVE Operation') +
    xlab("Traffic (MB)") +
    theme_bw() +
    theme(axis.text.y=element_blank(), axis.title.y=element_blank())
  grid.newpage()
}
# ecdf_grid_obj <- grid.draw(cbind(ggplotGrob(create_ecdf), ggplotGrob(shelve_ecdf), size = "last"))
ggsave(filename=params.ECDF_FILE, plot=grid.draw(cbind(ggplotGrob(create_ecdf), ggplotGrob(shelve_ecdf), size = "last")))
###############################################

######################## DATA INFO ############################################
# exec_id is no longer needed
total_traffic.df$exec_id <- NULL
total_api.df$exec_id <- NULL

exec_time_grouped <- group_by(exec_time.df, image, operation)
summarised_exec_time <- summarise(
  exec_time_grouped,
  avg_sd_time = paste(round(mean(exec_time), digits=3), '+/-', round(sd(exec_time), digits=3)))

total_traffic_grouped <- group_by(total_traffic.df, image, operation)
summarised_total_traffic <- summarise(
  total_traffic_grouped,
  avg_sd_traffic = paste(round(mean(traffic), digits=3), '+/-', round(sd(traffic), digits=3)))

total_api_grouped <- group_by(total_api.df, image, operation)
summarised_total_api <- summarise(
  total_api_grouped,
  avg_sd_api = paste(ceiling(mean(calls)), '+/-', round(sd(calls), digits=3)))

# summarised_total_api_test <- data_handler_df_custom_order(summarised_total_api, 'operation', c('CREATE', 'SUSPEND', 'RESUME', 'STOP', 'SHELVE'))

aux.tibble <- inner_join(summarised_total_api, summarised_exec_time, by = c('image', 'operation'))
full_data_info.tibble <- inner_join(summarised_total_traffic, aux.tibble, by = c('image', 'operation'))

fixed_data_info.df <- data_handler.df_names_fix(as.data.frame(full_data_info.tibble))
if (params.LANG == 'PT'){
  data_info.table <- kable(fixed_data_info.df,
    format="latex",
    booktabs = T,
    caption="Data Information",
    col.names=c('Imagem', 'Operação', 'Tráfego Total - MB (média +/- sd)',
                'Chamadas API (média +/- sd)', 'Tempo de Exec. - segundos (média +/- sd)')) %>%
  kable_styling(latex_options = c("scale_down", "HOLD_position"))
} else {
  data_info.table <- kable(fixed_data_info.df,
    format="latex",
    booktabs = T,
    caption="Data Information",
    col.names=c('Image', 'Operation', 'Total Traffic - MB (mean +/- sd)',
                'Total API Calls (mean +/- sd)', 'Execution Time - seconds (mean +/- sd)')) %>%
  kable_styling(latex_options = c("scale_down", "HOLD_position"))
}
print(data_info.table)

################### BUILDING TABLES ####################

full_api_data <- tibble()
full_traffic_data <- tibble()

for(image in db.df$image){
  database_path <- db.df[db.df$image == image, ]$database
  service_api_calls.df <- db_interact.get_service_calls(database=database_path) #query for every image in the database. FIXME
  service_api_calls.df <- service_api_calls.df[service_api_calls.df$image == image, ]
  service_api_calls.df$exec_id <- NULL
  service_traffic.df <- db_interact.get_service_traffic(database=database_path) #query for every image in the database. FIXME
  service_traffic.df <- service_traffic.df[service_traffic.df$image == image, ]
  service_traffic.df$exec_id <- NULL
  service_api_calls.df$calls <- as.numeric(service_api_calls.df$calls)
  service_traffic.df$traffic <- as.numeric(service_traffic.df$traffic)

  #Grouping and summarising data
  api_calls_grouped.df <- group_by(service_api_calls.df, operation, service, image)
  service_traffic_grouped.df <- group_by(service_traffic.df, operation, service, image)
  summarised_api_calls.df <- summarise(
    api_calls_grouped.df,
    avg_sd = paste(ceiling(mean(calls)), '+/-', round(sd(calls, na.rm=TRUE),digits=3)))
  summarised_service_traffic.df <- summarise(
    service_traffic_grouped.df,
    avg_sd = paste(round(mean(traffic), digits=3), '+/-', round(sd(traffic, na.rm=TRUE),digits=3)))
  full_api_data <- bind_rows(full_api_data, summarised_api_calls.df)
  full_traffic_data <- bind_rows(full_traffic_data, summarised_service_traffic.df)
}

cast_api_data <- cast(full_api_data, operation+image~service)
cast_traffic_data <- cast(full_traffic_data, operation+image~service)
cast_api_data <- data_handler_df_custom_order(cast_api_data, 'operation', c('CREATE', 'SUSPEND', 'RESUME', 'STOP', 'SHELVE'))
cast_traffic_data <- data_handler_df_custom_order(cast_traffic_data, 'operation', c('CREATE', 'SUSPEND', 'RESUME', 'STOP', 'SHELVE'))
cast_api_data$operation <- as.character(cast_api_data$operation)
cast_traffic_data$operation <- as.character(cast_traffic_data$operation)

################### API AND TRAFFIC TABLES ####################

cast_api_data <- data_handler.df_names_fix(cast_api_data)

if (params.LANG == 'PT'){
  full_api.table <- kable(cast_api_data,
      align = 'c',
      format = 'latex',
      col.names = firstup(names(cast_api_data)),
      caption = "Chamadas API / Serviço (média +/- sd)") %>%
  kable_styling(latex_options = c("scale_down", "HOLD_position")) %>%
  collapse_rows(valign = 'middle')

  cast_traffic_data <- data_handler.df_names_fix(cast_traffic_data)
  full_traffic.table <- kable(cast_traffic_data,
      align = 'c',
      format = 'latex',
      col.names = firstup(names(cast_traffic_data)),
      caption = "Volume de Tráfego (MB) / Serviço (média +/- sd)") %>%
  kable_styling(latex_options = c("scale_down", "HOLD_position")) %>%
  collapse_rows(valign = 'middle')
} else {
  full_api.table <- kable(cast_api_data,
      align = 'c',
      format = 'latex',
      col.names = firstup(names(cast_api_data)),
      caption = "API Calls / Service (mean +/- sd)") %>%
  kable_styling(latex_options = c("scale_down", "HOLD_position")) %>%
  collapse_rows(valign = 'middle')

  cast_traffic_data <- data_handler.df_names_fix(cast_traffic_data)
  full_traffic.table <- kable(cast_traffic_data,
      align = 'c',
      format = 'latex',
      col.names = firstup(names(cast_traffic_data)),
      caption = "Traffic Volume (MB) / Service (mean +/- sd)") %>%
  kable_styling(latex_options = c("scale_down", "HOLD_position")) %>%
  collapse_rows(valign = 'middle')
}

print(full_api.table)
print(full_traffic.table)
