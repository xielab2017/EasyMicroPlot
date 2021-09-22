\name{RFCV}
\alias{RFCV}
\title{RFCV}
\usage{
RFCV <- function(RF,seed_start=123,ntree=1000,kfold=5,rep=10,RF_importance=1,step=1,each_ouput=F,
                 x_break=1,cutoff_colour=c("red"),...)
}
\description{
Random forest together with cross-validation.
}
\arguments{
  \item{RF}{Dataframe containg taxonomy data, which should be generated from data_filter function.}
  \item{seed_start}{Random number.}
  \item{ntree}{Tree parameters in random forest model.}
  \item{kfold}{Number of folds in the cross-validation.}
  \item{rep}{Number of test in different random number.}
  \item{RF_importance}{A vector giving the subscripts which the function will be applied over.c(1, 2) MeanDecreaseAccuracy rows and MeanDecreaseGini}
  \item{step}{Each step when RFCV model generate. }
  \item{each_ouput}{Determin the varImpPlot output. [Default:False] }
  \item{x_break}{RFCV curve break for x axis. }
  \item{cutoff_colour}{Set the colour for cutoff line. }
  \item{palette}{Palette. }
}
\examples{
##generate filter data
re=data_filter(dir = '.',min_relative = 0.001,min_ratio = 0.7,
               design = 'mapping.txt',adjust = F,pattern = 'L7')              
##modify data               
sp=re$filter_data$species

##generate rfcv
re=RFCV(sp)
re$RFCV_result_plot$curve_plot
}
