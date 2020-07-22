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
source('boxplot/boxplot_final.R')

################################################################################

df_names_fix <- function(my_df){
  fedora_name_fix <- function(x){
    str_replace(x, 'fedora31', 'Fedora 31')
  }

  ubuntu_name_fix <- function(x){
    str_replace(x, 'bionic_ubuntu', 'Ubuntu Bionic Beaver')
  }

  windows_name_fix <- function(x){
    str_replace(x, 'windows_server', 'Windows Server')
  }

  names_fix <- c(fedora_name_fix, ubuntu_name_fix, windows_name_fix)

  for(fix.fn in names_fix){
    my_df <- mutate_all(my_df, funs(fix.fn))
  }

  return(my_df)
}

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

boxplot_data.df <- df_names_fix(boxplot_data.df)
boxplot_data.df$traffic <- as.numeric(boxplot_data.df$traffic)

create.df <- boxplot_data.df[boxplot_data.df$operation == 'CREATE',]
shelve.df <- boxplot_data.df[boxplot_data.df$operation == 'SHELVE',]


################### BOXPLOT ####################

ylim_min_value <- min(c(min(log(create.df$traffic + 1)), min(log(shelve.df$traffic + 1))) )
ylim_max_value <- max(c(max(log(create.df$traffic + 1)), max(log(shelve.df$traffic + 1))) )
create_plot <- ggplot(create.df, aes(x=image, y=log(traffic +1 ), fill=image)) +
  geom_boxplot() +
  ggtitle('CREATE Operation') +
  ylab("Traffic (log10 +1 scale)") +
  xlab("OS Image") +
  theme(legend.position="none", axis.text.x=element_blank()) +
  ylim(ylim_min_value, ylim_max_value)
shelve_plot <- ggplot(shelve.df, aes(x=image, y=log(traffic +1 ), fill=image)) +
  geom_boxplot() +
  ggtitle('SHELVE Operation') +
  ylim(ylim_min_value, ylim_max_value) +
  xlab("OS Image") +
  theme(axis.text.y=element_blank(), axis.title.y=element_blank(), axis.text.x=element_blank())
grid.newpage()
# boxplot_grid_obj <- grid.draw(cbind(ggplotGrob(create_plot), ggplotGrob(shelve_plot), size = "last"))
ggsave(filename='boxplot_obj.pdf', plot=grid.draw(cbind(ggplotGrob(create_plot), ggplotGrob(shelve_plot), size = "last")))

################### ECDF ####################

ylim_min_real <- min(c(min(create.df$traffic), min(shelve.df$traffic)) )
ylim_max_real <- max(c(max(create.df$traffic), max(shelve.df$traffic)) )
create_ecdf <- ggplot(create.df, aes(traffic, color=image)) +
  stat_ecdf(geom = "step") +
  ggtitle('CREATE Operation') +
  xlab("Traffic (MB)") +
  ylab('Cumulative Distribution Function') +
  theme(legend.position="none")
shelve_ecdf <- ggplot(shelve.df, aes(traffic, color=image)) +
  stat_ecdf(geom = "step") +
  ggtitle('SHELVE Operation') +
  xlab("Traffic (MB)") +
  theme(axis.text.y=element_blank(), axis.title.y=element_blank())
grid.newpage()
# ecdf_grid_obj <- grid.draw(cbind(ggplotGrob(create_ecdf), ggplotGrob(shelve_ecdf), size = "last"))
ggsave(filename='ecdf_obj.pdf', plot=grid.draw(cbind(ggplotGrob(create_ecdf), ggplotGrob(shelve_ecdf), size = "last")))


######################## DATA INFO ############################################

data_info.df$total_api_calls_mean <- ceiling(as.numeric(data_info.df$total_api_calls_mean))
data_info.df <- df_names_fix(data_info.df)
data_info.table <- kable(data_info.df,
  format="latex",
  booktabs = T,
  caption="Data Information",
  col.names=c('Image', 'Operation', 'Mean - Execution time ',
              'SD - Execution time', 'Mean - Total traffic',
              'SD - Total traffic', 'Mean - Total api calls', 'SD - Total api calls')) %>%
kable_styling(latex_options = c("scale_down", "HOLD_position"))
print(data_info.table)

################### BUILDING TABLES ####################

full_api_data <- tibble()
full_traffic_data <- tibble()

for(database_path in unique(db.df$database)){
  service_api_calls.df <- db_interact.get_service_calls(database=database_path)
  service_api_calls.df$exec_id <- NULL
  service_traffic.df <- db_interact.get_service_traffic(database=database_path)
  service_traffic.df$exec_id <- NULL

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

  cast_api_data <- df_names_fix(cast_api_data)

  full_api.table <- kable(cast_api_data,
      align = 'c',
      format = 'latex',
      col.names = firstup(names(cast_api_data)),
      caption = "API Calls / Service (mean +/- sd)") %>%
  kable_styling(latex_options = c("scale_down", "HOLD_position")) %>%
  collapse_rows(valign = 'middle')

  cast_traffic_data <- df_names_fix(cast_traffic_data)
  full_traffic.table <- kable(cast_traffic_data,
      align = 'c',
      format = 'latex',
      col.names = firstup(names(cast_traffic_data)),
      caption = "Traffic Volume (MB) / Service (mean +/- sd)") %>%
  kable_styling(latex_options = c("scale_down", "HOLD_position")) %>%
  collapse_rows(valign = 'middle')

  print(full_api.table)
  print(full_traffic.table)
