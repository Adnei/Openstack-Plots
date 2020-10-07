library(dplyr)
library(magrittr)
library(knitr)
library(kableExtra)
library(ggplot2)
library(stringr)
source('objects.R')
source('db_interact.R')
source('data_handler.R')
source('linear_regression/linear_regression.R')

################################################################################

lr_data.df <- data_handler.df_names_fix(lr.df)

filtered_op <- objects.operations[objects.operations == 'CREATE' | objects.operations == 'SHELVE']
lr_stats.list <- list(operation = vector('list', length(filtered_op)),
                          adjust = vector('list', length(filtered_op)),
                          resid = vector('list', length(filtered_op)),
                          confint = vector('list', length(filtered_op)),
                          mmx = vector('list', length(filtered_op)),
                          mape = vector('list', length(filtered_op))
                        )
# col_arr <- c('#0000cc', '#6600ff', '#006600', '#cc9900', '#ff3300')
op_count <- 0
for(operation in filtered_op){
  op_count <- op_count + 1
  ############ REMOVING RAW IMAGE ############
  op_lr.df <- lr_data.df[lr_data.df$operation == operation & lr_data.df$image != 'debian10raw', ]
  op_lr.df$image_size = as.numeric(op_lr.df$image_size)
  op_lr.df$mean_total_traffic = as.numeric(op_lr.df$mean_total_traffic)
  ############# REMOVING SAMPLE ##############
  # sample_idx <- sample(c(2:(nrow(op_lr.df)-1)), 1)
  # actuals.df <- op_lr.df[sample_idx,]
  if(operation == 'CREATE'){
    actuals.df <- op_lr.df[op_lr.df$image == 'Centos 7 (898 MB)', ]
    op_lr.df <- op_lr.df[op_lr.df$image != 'Centos 7 (898 MB)', ]
  } else {
    actuals.df <- op_lr.df[op_lr.df$image == 'Debian 10', ]
    op_lr.df <- op_lr.df[op_lr.df$image != 'Debian 10', ]
  }

  predicted.df <- actuals.df
  predicted.df$image <- paste(predicted.df$image, '(predicted)')
  actuals.df$image <- paste(actuals.df$image, '(actual)')
  # op_lr.df <- op_lr.df[-c(sample_idx), ]
  ############################################
  adjust <- lm(mean_total_traffic ~ image_size, data = op_lr.df)
  fit_adjust <- fitted(adjust)
  resid_adjust <- resid(adjust)
  cfnt_adjust <- confint(adjust, level=0.9)
  corr <- cor(op_lr.df$image_size, op_lr.df$mean_total_traffic)
  corr2 <- corr^2
  predicted.df$mean_total_traffic <- predict(adjust, data.frame(image_size = c(predicted.df$image_size)))
  title <- paste('Linear Regression Model:', operation)
  lr_stats.list$operation[[op_count]] <- operation
  lr_stats.list$adjust[[op_count]] <- adjust
  lr_stats.list$resid[[op_count]] <- resid_adjust
  lr_stats.list$confint[[op_count]] <- cfnt_adjust
  mmx <- mean(min(actuals.df$mean_total_traffic, predicted.df$mean_total_traffic) / max(actuals.df$mean_total_traffic, predicted.df$mean_total_traffic))
  mape <- mean( ( abs(predicted.df$mean_total_traffic - actuals.df$mean_total_traffic) ) / actuals.df$mean_total_traffic)
  lr_stats.list$mmx[[op_count]] <- mmx
  lr_stats.list$mape[[op_count]] <- mape
  lr.plot <- ggplot(op_lr.df, aes(image_size, mean_total_traffic)) +
  geom_point(aes(color=image)) +
  geom_smooth(method = "lm", level = 0.90,  formula = y ~ x) +
  geom_point(data = predicted.df, mapping = aes(image_size, mean_total_traffic, color=image)) +
  geom_point(data = actuals.df, mapping = aes(image_size, mean_total_traffic, color=image)) +
  ylab("Traffic Volume (MB)") +
  xlab("Image Size (MB)") +
  theme_bw() +
  ggtitle(title)
  # print(lr.plot)
  pdf_file <- paste('linear_regression_2', operation, '.pdf', sep='')
  ggsave(filename=pdf_file, plot=lr.plot)
}


for(op_count in c(1:length(filtered_op))){
  print(lr_stats.list$operation[[op_count]])
  print(summary(lr_stats.list$adjust[[op_count]]))
  print('resid')
  print(lr_stats.list$resid[[op_count]])
  print('cofint')
  print(lr_stats.list$confint[[op_count]])
  print('mmx')
  print(lr_stats.list$mmx[[op_count]])
  print('mape')
  print(lr_stats.list$mape[[op_count]])
  cat("\n")
}
