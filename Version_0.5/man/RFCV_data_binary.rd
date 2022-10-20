\name{RFCV_data_binary}
\alias{RFCV_data_binary}
\title{RFCV_data_binary}
\usage{
RFCV_data_binary <-function(data,rf_estimate_group,id_not='NOT')
}
\description{
Modify data into two levels for RFCV.
}
\arguments{
  \item{data}{Dataframe containing taxonomy or List generated from RFCV function.}
  \item{rf_estimate_group}{Select the group which is estimated.}
  \item{id_not}{Other groups name.[Default:NOT]}
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
 sp <- RFCV_data_binary(sp,rf_estimate_group = 'ID',id_not = 'NO_ID')

}
