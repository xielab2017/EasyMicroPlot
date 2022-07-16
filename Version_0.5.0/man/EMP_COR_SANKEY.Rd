\name{EMP_COR_SANKEY}
\alias{EMP_COR_SANKEY}
\title{EMP_COR_SANKEY}
\usage{
EMP_COR_SANKEY(data_list = data_list,rvalue = 0,pvalue = 0.05,...)
}
\description{
Perform Sankey plot for multiple data based on correlation analysis.
}
\arguments{
  \item{data_list}{A data list containg multiple data.}
  \item{cor_method}{"pearson" , "kendall", or "spearman" [Default:spearman]}
  \item{rvalue}{Set relation value forthreshold for correlation test. [Default:0.3]}
  \item{pvalue}{Set pvalue forthreshold for correlation test. [Default:0.05]}
  \item{positive_col}{Edge colour for positive relation. [Default:darkred]}
  \item{negtive_col}{Edge colour for negitive relation. [Default:steelblue]}
  \item{palette}{Colour palette for nodes.}
  \item{sankey_ouput}{Output sankey plot. [Default:False]}
  \item{file}{file name for sankey plot. [Default:Sankey.html]}

}
\examples{
## Load data
 library(EasyMicroPlot)
 data(EMP)

## Plot
 Sankey_pic<- EMP_COR_SANKEY(data_list = EMP$Sankey_data,rvalue = 0.3,pvalue = 0.05)
 Sankey_pic$plot

## Result for correlation analysis
 Sankey_pic$sankey_data

}
