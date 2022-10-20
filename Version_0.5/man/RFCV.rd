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
  \item{seed_start}{Random number.[Default:123]}
  \item{ntree}{Tree parameters in random forest model.[Default:1000]}
  \item{kfold}{Number of folds in the cross-validation.[Default:5]}
  \item{rep}{Number of test in different random number.[Default:10]}
  \item{RF_importance}{A vector giving the subscripts which the function will be applied over.c(1, 2) MeanDecreaseAccuracy and MeanDecreaseGini[Default:1]}
  \item{step}{Each step when RFCV model generate.[Default:1] }
  \item{each_ouput}{Determin the varImpPlot output.[Default:False] }
  \item{x_break}{RFCV curve break for x axis.[Default:1] }
  \item{cutoff_colour}{Set the colour for cutoff line.[Default:red] }
  \item{palette}{Palette. }
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

## Generate rfcv
 RFCV_result <- RFCV(sp)

## Show RFCV curve
 RFCV_result$RFCV_result_plot$curve_plot

## Show intersection and union tax among all the best models under different random number seed
 RFCV_result$RFCV_result_plot$intersect_num
 RFCV_result$RFCV_result_plot$union_num


}
