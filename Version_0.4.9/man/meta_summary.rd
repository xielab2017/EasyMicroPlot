\name{meta_summary}
\alias{meta_summary}
\title{meta_summary}
\usage{
meta_summary <- function(data,estimate_group,missing_plot=T,keep_col= 1)
}
\description{
Summary and table for meta data.
}
\arguments{
  \item{data}{Data containing meta data.}
  \item{estimate_group}{Set estimate group for table.}
  \item{missing_plot}{Generate missing plot. [Default:True]}
  \item{keep_col}{Select reserved column. [Default:1]}
}
\examples{
## For summary of meta data
meta <- meta_summary(data,estimate_group = 'EM',keep_col = 1)

## For table
meta$summary_table

## For detailed summary of meta data
meta$summary_info
}
