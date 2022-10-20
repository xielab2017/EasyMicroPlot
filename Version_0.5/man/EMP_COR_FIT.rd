\name{EMP_COR_FIT}
\alias{EMP_COR_FIT}
\title{EMP_COR_FIT}
\usage{
EMP_COR_FIT(data,meta = NULL,var_select,formula = y~poly(x,1,raw = T),eq_size=3,se = F,group = F,width = 5, height = 5,palette=palette)
}
\description{
Perform polynomial regression for variables.
}
\arguments{
  \item{data}{A dataframe containg relative abundance and group infomation.}
  \item{meta}{An dataframe containing subject-related data.}
  \item{formula}{a formula object. Using aesthetic names x and y instead of original variable names. [y~poly(x,1,raw = T)]}
  \item{eq_size}{Equation label size. [Default:3]}
  \item{se}{Display confidence interval around smooth. [Default:False]}
  \item{group}{Analysis by group. [Default:False]}
  \item{width,height}{Set the width and height of interactive html. [Default:5]}
  \item{palette}{Colour palette.}

}
\examples{
## Load data
 library(EasyMicroPlot)
 data(EMP)

## Data preparation 
 core_data <- data_filter(data = EMP$micro,design = EMP$mapping,
                           min_relative = 0.001,min_ratio = 0.7)
 sp <- core_data$filter_data$species


## Polynomial regression
 fit_result <- EMP_COR_FIT(data=sp,meta = EMP$iron,var_select = c('V19','SI_Iron.content'),
                 formula = y~poly(x,1,raw = T),width = 5,height = 5,se = F,group = F,eq_size = 3)
 
 fit_result <- EMP_COR_FIT(data=sp,meta = EMP$iron,var_select = c('V19','SI_Iron.content'),
                 formula = y~poly(x,2,raw = T),width = 5,height = 5,se = F,group = F,eq_size = 3)

## Polynomial regression for grouping
 fit_result <- EMP_COR_FIT(data=sp,meta = EMP$iron,var_select = c('V19','SI_Iron.content'),
                 formula = y~poly(x,1,raw = T),width = 5,height = 5,se = F,group = T,eq_size = 3)

## Plot
 fit_result$pic

## Interactive visualization
 fit_result$html



}
