\name{EMP_META}
\alias{EMP_META}
\title{EMP_META}
\usage{
EMP_META <-function(data,meta,...)
}
\description{
Summary and regroup function for meta data.
}
\arguments{
  \item{data}{Data containing meta data.}
  \item{estimate_group}{Set estimate group for table.}
  \item{missing_plot}{Generate missing plot.[Default:True]}
  \item{keep_col}{Select reserved column.[Default:SampleID]}
  \item{new_col_id}{Set new group id.[Default:Group]}
  \item{col_str}{Select categorical column.}
  \item{col_num}{Select continuous column.}
  \item{regroup}{Generate labeled regroup. }
  \item{clust_min}{Set minimum cluster for continuous variable.Default:2]}
  \item{clust_max}{Set maximum cluster for continuous variable.Default:15]}
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
re <- EMP_META(data = meta_data,estimate_group = 'MetS',missing_plot = F,new_col_id = 'Group',col_str = col_str,
                   col_num = col_num,keep_col = 'SampleID',regroup = T,clust_min = 2,clust_max = 9)

## For table
 re$summary$summary_table

## For detailed summary of meta data
 re$summary$summary_info

## Show cluster details
 re$regroup$regroup_info

## Show regroup result 
 new_group <- re$regroup$regroup_data                

}
