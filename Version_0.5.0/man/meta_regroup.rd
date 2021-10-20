\name{meta_regroup}
\alias{meta_regroup}
\title{meta_regroup}
\usage{
meta_regroup <- function(data,new_col_id = 'Group'...)
}
\description{
Regroup for meta data.
}
\arguments{
  \item{data}{Data containing meta data.}
  \item{new_col_id}{Set new group id.[Default:Group]}
  \item{col_str}{Select categorical column.}
  \item{col_num}{Select continuous column.}
  \item{keep_col}{Select reserved column.[Default:SampleID]}
  \item{regroup}{Generate labeled regroup.Default:True] }
  \item{clust_min}{Set minimum cluster for continuous variable.[Default:2]}
  \item{clust_max}{Set maximum cluster for continuous variable.[Default:15]}
  \item{clust_method}{Set method in clust:ward.D, ward.D2,single,complete,average,mcquitty,median,centroid,kmeans. [Default:kmeans]}
  \item{clust_dis}{Set method in distance:euclidean,maximum,manhattan,canberra,binary,minkowski. [Default:euclidean]}
  \item{silent}{logical: should the report of result messages be suppressed?. [Default:False]}
}
\examples{
## Load data
 library(EasyMicroPlot)
 data(EMP)

## Data preparation
 meta_data <- EMP$meta
 meta_data <- na.omit(meta_data)
 meta_data <- meta_data[meta_data$Diarrhea=='n'&meta_data$Astriction=='n'&meta_data$Antibiotics=='n'&meta_data$Synbiotics=='n',]


## Identify the categorical and continuous variable
 col_str <- 'MetS'
 col_num=colnames(meta_data)[!colnames(meta_data) %in% c('SampleID','MetS','Diarrhea','Astriction','Antibiotics','Synbiotics')]
 col_num

## For regroup of meta data
 re <- meta_regroup(data = meta_data,new_col_id = 'Group',col_str = col_str,
                   col_num = col_num,keep_col = 'SampleID',regroup = T,clust_min = 2,clust_max = 9)

## Show cluster details
 re$regroup_info

## Show regroup result 
 new_group <- re$regroup_data



}
