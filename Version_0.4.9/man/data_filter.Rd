\name{data_filter}
\alias{data_filter}
\title{data_filter}
\usage{
data_filter(dir = dir,design = Mapping File,min_relative = min_relative,min_ratio = min_ratio,
                  adjust = T,pattern = pattern,output=F)
}
\description{
Filtering data at different condition.
}
\arguments{
  \item{dir}{An directory contain microbial relative abundance at different levels.}
  \item{min_relative}{Set the min relative abundance for filtering.}
  \item{min_ratio}{Set the min ratio presence for taxonomy.}
  \item{design}{Mapping File}
  \item{adjust}{Set addtional dummy tax in empty data when filtering. [Default:False]}
  \item{pattern}{Set key string character for relative abundance files.}
  \item{output}{Output filter data at different levels. [Default:False]}
}
\examples{
data_filter(dir = '.',design = 'mapping.txt',min_relative = 0.005,min_ratio = 0.7,
                  adjust = T,pattern = 'L4',output=F)
}
