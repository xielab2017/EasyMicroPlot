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
  \item{new_col_id}{Select the estimate group.}
  \item{keep_col}{Select reserved column.}  
  \item{regroup_level}{Set new group level for regroup.}
  \item{regroup_name}{Set new group name for regroup.}
}
\examples{
## For regroup of meta data
re <- meta_regroup(data = data,new_col_id = 'Group',col_str = c('Leiomyom','EM'),
                   keep_col = 'SampleID',regroup = T)

## rearrange the group level
new_level <- c('Control_EM','Control_Control','Leiomyoma_EM','Leiomyoma_Control')
data2=meta_regroup_sort(data = data,new_col_id = 'Group',regroup_level = new_level,
	                    keep_col = 'SampleID',regroup_name = 'regroup')

}
