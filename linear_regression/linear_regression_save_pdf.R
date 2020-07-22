library(dplyr)
library(magrittr)
library(knitr)
library(kableExtra)
library(ggplot2)
source('objects.R')
source('db_interact.R')
source('data_handler.R')
source('linear_regression/linear_regression.R')

################################################################################

df_names_fix <- function(my_df){
  names_fix <- c(
    function(x){
      str_replace(x, 'fedora31', 'Fedora 31')
    },

    function(x){
      str_replace(x, 'bionic_ubuntu', 'Ubuntu Bionic Beaver')
    },

    function(x){
      str_replace(x, 'windows_server', 'Windows Server')
    },

    function(x){
      str_replace(x, 'centos7', 'Centos 7 (1300 MB)')
    },

    function(x){
      str_replace(x, 'centos7_light', 'Centos 7 (898 MB)')
    },

    function(x){
      str_replace(x, 'cirros', 'Cirros')
    },

    function(x){
      str_replace(x, 'debian10qcow2', 'Debian 10')
    },

    function(x){
      str_replace(x, 'focal_ubuntu', 'Ubuntu Focal Fossa')
    },
  )

  for(fix.fn in names_fix){
    my_df <- mutate_all(my_df, funs(fix.fn))
  }

  return(my_df)
}

lr.df <- df_names_fix(lr.df)

filtered_op <- objects.operations[objects.operations == 'CREATE' | objects.operations == 'SHELVE']
lr_stats.list <- list(operation = vector('list', length(filtered_op)),
                          adjust = vector('list', length(filtered_op)),
                          resid = vector('list', length(filtered_op)),
                          confint = vector('list', length(filtered_op)))
col_arr <- c('#0000cc', '#6600ff', '#006600', '#cc9900', '#ff3300')
op_count <- 0
for(operation in filtered_op){
  op_count <- op_count + 1
  ############ REMOVING RAW IMAGE ############
  op_lr.df <- lr.df[lr.df$operation == operation & lr.df$image != 'debian10raw', ]
  op_lr.df$image_size = as.numeric(op_lr.df$image_size)
  op_lr.df$mean_total_traffic = as.numeric(op_lr.df$mean_total_traffic)
  adjust <- lm(op_lr.df$mean_total_traffic ~ op_lr.df$image_size)
  fit_adjust <- fitted(adjust)
  resid_adjust <- resid(adjust)
  cfnt_adjust <- confint(adjust, level=0.9)
  corr <- cor(op_lr.df$image_size, op_lr.df$mean_total_traffic)
  corr2 <- corr^2
  title <- paste('Linear Regression Model:', operation)
  lr_stats.list$operation[[op_count]] <- operation
  lr_stats.list$adjust[[op_count]] <- adjust
  lr_stats.list$resid[[op_count]] <- resid_adjust
  lr_stats.list$confint[[op_count]] <- cfnt_adjust
  lr.plot <- ggplot(op_lr.df, aes(image_size, mean_total_traffic)) +
  geom_point(aes(color=image)) +
  geom_smooth(method = "lm", level = 0.90,  formula = y ~ x) +
  ylab("Traffic Volume (MB)") +
  xlab("Image Size (MB)") +
  ggtitle(title)
  # print(lr.plot)
  pdf_file <- paste('linear_regression_', operation, '.pdf' )
  ggsave(filename=pdf_file, plot=lr.plot)
}
