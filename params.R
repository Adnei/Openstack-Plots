############################################################################################################
# @IMPORTANT !                                                                                             #
# This file is used for better parameterizing. It centralizes the parameterization of databases and labels #
# The English - Portuguese labels translation should be carefully thought                                  #
############################################################################################################

# These are all global variables. This file must be always imported
params.LANG <- 'PT'
#params.LANG <- 'EN'
params.COMMON_PATH <- '/media/HDD/UDESC/OpenStack/tests_beta/final_version/'
params.DB_PATH <- 'fedora_bionic_30/network_metering_experiment.db'
params.DEFAULT_DATABASE <- paste0(params.COMMON_PATH, params.DB_PATH)

###########################################################################################################
#                                   ECDF PARAMS
#params.ECDF_FILE <- 'ecdf.pdf'
#params.ECDF_FILE <- 'ecdf_X_PT.pdf'
params.ECDF_FILE <- 'ecdf_fedora31_PT.pdf'
###########################################################################################################
#                                   BOXPLOT PARAMS
#params.BOXPLOT_FILE <- 'boxplot.pdf'
#params.BOXPLOT_FILE <- 'boxplot_X_PT.pdf'
params.BOXPLOT_FILE <- 'boxplot_fedora31_PT.pdf'
############################################################################################################
#                                   db_list
#
params.db_list <- c(
  'fedora_bionic_30/network_metering_experiment.db'
)
# params.db_list <- c(
#   'fedora_bionic_30/network_metering_experiment.db',
#   'exp_windows/30_exec/network_metering_experiment.db',
#   'exp_1/network_metering_experiment.db',
#   'exp_2/network_metering_experiment.db'
# )
# params.db_list <- c(
#   'fedora_bionic_30/network_metering_experiment.db',
#   'exp_windows/30_exec/network_metering_experiment.db',
#   'exp_1/network_metering_experiment.db',
#   'exp_2/network_metering_experiment.db',
#   'exp_fedora32_30_2/network_metering_experiment.db',
#   'exp_freebsd_30/network_metering_experiment.db'
# )
############################################################################################################
