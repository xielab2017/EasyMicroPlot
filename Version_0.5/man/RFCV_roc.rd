\name{RFCV_roc}
\alias{RFCV_roc}
\title{RFCV_roc}
\usage{
RFCV_roc <- function(x,rf_estimate_group,rf_tax_select,ntree=1000)
}
\description{
Generate roc model
}
\arguments{
  \item{x}{Dataframe containing taxonomy or List generated from RFCV function.}
  \item{ntree}{Tree parameters in random forest model.[Default:1000]}
  \item{rf_tax_select}{Select the taxonomy.}
}
\examples{
## Load data
 library(EasyMicroPlot)
 data(EMP)

## Generate core data
 core_data <- data_filter(data = EMP$micro,design = EMP$mapping,
                             min_relative = 0.001,min_ratio = 0.7)            
## Prepare training data              
 sp <- core_data$filter_data$species

## Modify the group into two cases when multiple group exists. 
 sp <- RFCV_data_binary(sp,rf_estimate_group = 'CT',id_not = 'Cases')

## Generate rfcv
 RFCV_result <- RFCV(sp)

## Show intersection and union tax among all the best models under different random number seed
 RFCV_result$RFCV_result_plot$intersect_num
 RFCV_result$RFCV_result_plot$union_num

## ROC
## Parameter *rf_tax_select* allows users to select optional tax 
## **union** and **intersect** means the intersection and union tax of models above
RFCV_roc(RFCV_result,rf_tax_select = c("V143", "V83" , "V101", "V121"),rf_estimate_group = 'Cases')
RFCV_roc(RFCV_result,rf_tax_select = 'union',rf_estimate_group = 'Cases')
RFCV_roc(RFCV_result,rf_tax_select = 'intersect',rf_estimate_group = 'Cases')



}
