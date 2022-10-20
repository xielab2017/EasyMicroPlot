\name{meta_regroup_sort}
\alias{meta_regroup_sort}
\title{meta_regroup_sort}
\usage{
meta_regroup_sort <- function(data,new_col_id = 'Group',regroup_level,keep_col = 'SampleID',regroup_name = 'regroup')
}
\description{
Sort meta data generated from meta_regroup function.
}
\arguments{
  \item{data}{Data containing meta data.}
  \item{new_col_id}{Select the estimate group.[Default:Group]}
  \item{keep_col}{Select reserved column.[Default:SampleID]}  
  \item{regroup_level}{Set new group level for regroup.}
  \item{regroup_name}{Set new group name for regroup.Default:regroup]}
}
\examples{
## Load data
 library(EasyMicroPlot)
 data(EMP)

# Identify the categorical and continuous variable
 col_str <- 'MetS'

 col_num=colnames(EMP$meta)[!colnames(EMP$meta) %in% c('SampleID','MetS','Diarrhea','Astriction','Antibiotics','Synbiotics')]
 col_num

## For regroup of meta data
 re <- meta_regroup(data = data,new_col_id = 'Group',col_str = col_str,
                   col_num = col_num,keep_col = 'SampleID',regroup = T)

## Show cluster details
 re$regroup_info

## Show regroup result 
 new_group <- re$regroup_data


## rearrange the levels of regroup
 new_level <- c('Control_1','Cases_1','Control_2','Cases_2')
 new_group2 <- meta_regroup_sort(data = new_group,new_col_id = 'Group',regroup_level = new_level,
                               keep_col = 'SampleID',regroup_name = 'regroup')



}
