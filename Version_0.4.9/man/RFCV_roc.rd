\name{RFCV_roc}
\alias{RFCV_roc}
\title{RFCV_roc}
\usage{
RFCV_roc <- function(x,rf_estimate_group,seed,rf_tax_select,ntree=1000)
}
\description{
Generate roc model
}
\arguments{
  \item{x}{Dataframe containing taxonomy or List generated from RFCV function.}
  \item{seed_start}{Random number.}
  \item{ntree}{Tree parameters in random forest model.}
  \item{rf_tax_select}{Select the taxonomy.}
}
\examples{
##generate filter data
re=data_filter(dir = '.',min_relative = 0.001,min_ratio = 0.7,
               design = 'mapping.txt',adjust = F,pattern = 'L7')              
##modify data               
sp=re$filter_data$species


##Modify the group into two cases when multiple group exists. (Not necessary)
sp=RFCV_data_binary(sp,rf_estimate_group = 'Group_A',id_not = 'NO_Group_A')

##generate rfcv
re=RFCV(sp)
re$RFCV_result_plot$curve_plot

## generate roc
RFCV_roc(aaa,rf_tax_select='union',rf_estimate_group = 'Group_A')
}
