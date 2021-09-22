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
  \item{new_col_id}{Set new group id.}
  \item{col_str}{Select categorical column.}
  \item{col_num}{Select continuous column.}
  \item{keep_col}{Select reserved column.}
  \item{regroup}{Generate labeled regroup. }
  \item{clust_min}{Set minimum cluster for continuous variable.Default:2]}
  \item{clust_max}{Set maximum cluster for continuous variable.Default:15]}
  \item{clust_method}{Set method in clust:ward.D, ward.D2,single,complete,average,mcquitty,median,centroid,kmeans. [Default:kmeans]}
  \item{clust_dis}{Set method in distance:euclidean,maximum,manhattan,canberra,binary,minkowski. [Default:euclidean]}
  \item{silent}{logical: should the report of error messages be suppressed?. [Default:False]}
}
\examples{
## For regroup of meta data
re <- meta_regroup(data = data,new_col_id = 'Group',col_str = c('Leiomyom','EM'),
                   col_num = c('Height','Weight','BMI','Glu'),
                   keep_col = 'SampleID',regroup = T)

}
