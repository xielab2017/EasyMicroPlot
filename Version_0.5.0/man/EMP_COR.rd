\name{EMP_COR}
\alias{EMP_COR}
\title{EMP_COR}
\usage{
EMP_COR <-function(data,meta,...)
}
\description{
Correlation analysis between microbial data and meta information.
}
\arguments{
  \item{data}{An dataframe containing microbial relative abundance.}
  \item{meta}{An dataframe containing subject-related data.}
  \item{method}{"pearson" , "kendall", or "spearman" [Default:spearman]}
  \item{width}{width.[Default:10]}
  \item{height}{height.[Default:10}
  \item{cor_output}{Output.[Default:False]}
  \item{file_name}{file_name for output.[Default:cor_plot]}
}
\examples{
## Load data
 library(EasyMicroPlot)
 data(EMP)


## Prepare the bacteria taxa data
 core_data <- data_filter(data = EMP$micro,design = EMP$mapping,
                           min_relative = 0.001,min_ratio = 0.7) 
 sp <- core_data$filter_data$species


## Analysis and plot
## When ** cor_output = T **, cooc picture is generated in working directory.
 cor_re <- EMP_COR(data = sp,meta=EMP$iron,cor_output = T,method = 'spearman',aes_value = 1)
 cor_re <- EMP_COR(data = sp,meta=EMP$iron,cor_output = T,method = 'spearman',aes_value = 2)

}
