\name{RFCV_data_binary}
\alias{RFCV_data_binary}
\title{RFCV_data_binary}
\usage{
RFCV_data_binary <-function(data,rf_estimate_group,id_not='NOT')
}
\description{
Modify data into two cases.
}
\arguments{
  \item{data}{Dataframe containing taxonomy or List generated from RFCV function.}
  \item{rf_estimate_group}{Select the group which is estimated.}
  \item{id_not}{Other groups name.}
}
\examples{
##generate filter data
re=data_filter(dir = '.',min_relative = 0.001,min_ratio = 0.7,
               design = 'mapping.txt',adjust = F,pattern = 'L7')              
##modify data               
sp=re$filter_data$species

##Modify the group into two cases when multiple group exists. 
sp=RFCV_data_binary(sp,rf_estimate_group = 'Group_A',id_not = 'NO_Group_A')

}
