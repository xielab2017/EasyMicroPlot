\name{EMP_COR_RDA}
\alias{EMP_COR_RDA}
\title{EMP_COR_RDA}
\usage{
EMP_COR_RDA <-function(data,meta,...)
}
\description{
Correlation analysis between microbial data and meta information.
}
\arguments{
  \item{data}{An dataframe containing microbial relative abundance.}
  \item{meta}{An dataframe containing subject-related data.}
  \item{ellipse}{Determin the ellipse for groups. [Default:NULL]}
  \item{zoom}{Set the zoom parameter for arrow and sample sites. [Default: 1,1,1]}
  \item{arrow_col}{Arrow color for microbial and meta data. [Default: #F0E442,#CC79A7]}
  \item{palette}{Colour palette.}
  \item{seed}{Set the random seed to generate reproducible result. [Default:123]}
  \item{width}{width for html. [Default:15]}
  \item{height}{height for html. [Default:15]}
}
\examples{
## Load data
 library(EasyMicroPlot)
 data(EMP)

## Prepare the bacteria taxa data
 core_data <- data_filter(data = EMP$micro,design = EMP$mapping,
                           min_relative = 0.001,min_ratio = 0.7) 
 sp <- core_data$filter_data$species


## Result
 RDA_re <- EMP_COR_RDA(data = sp,meta=EMP$iron)

## plot
 RDA_re$plot$pic

## html plot
 RDA_re$plot$html

## The function performs an ANOVA like permutation test for Constrained Correspondence Analysis 
 RDA_re$model_information$model_permutest

## Fits an Environmental Vector or Factor onto an Ordination
 RDA_re$model_information$model_envfit

## Variance Inflation Factor
 RDA_re$model_information$model_vif


}
