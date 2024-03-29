\name{beta_plot}
\alias{beta_plot}
\title{beta_plot}
\usage{
beta_plot(dir = dir,min_relative = min_relative,min_ratio = min_ratio,
                      design = Mapping File,adjust = F,pattern = pattern,
                      group_level=group_level,method=method,distance=distance,
                      html_out=html_out,output=output,seed=seed,width=width,height=height)
}
\description{
Filtering data and making beta diversity plot.
}
\arguments{
  \item{data}{An list contain microbial relative abundance at different levels.}
  \item{dir}{An directory contain microbial relative abundance at different levels.}
  \item{group_level}{Set the group order in Mapping File. [Not necessary]}
  \item{min_relative}{Set the min relative abundance for filtering.}
  \item{min_ratio}{Set the min ratio presence for taxonomy.}
  \item{design}{Mapping File.}
  \item{ellipse}{Determine the ellipse for groups. [Default: NULL]}
  \item{adjust}{Set addtional dummy tax in empty data when filtering. [Default:True]}
  \item{pattern}{Set key string character for relative abundance files.}
  \item{palette}{Colour palette.}
  \item{output}{Output filter data at different levels. [Default:False]}
  \item{html_out}{Output interactive html in three axis ay different levels. [Default:False]}
  \item{seed}{Set the random seed to generate reproducible result. [Default:123]}
  \item{width,height}{Set the width and height of interactive html. [Default:15]}
  \item{method}{Set method in multiple comparison: ttest,HSD,LSD(Default),duncan,scheffe,REGW,SNK.}
  \item{distance}{bray(Default),manhattan, euclidean, canberra, clark, kulczynski, jaccard, gower, altGower, morisita, horn, mountford, raup, binomial, chao, cao or mahalanobis.}
  \item{change}{Modify empty taxonomy name.eg d__Bacteria;__;__ will change into d__Bacteria;p__Other;c__Other [Default:False]}
  \item{change_name}{Decide the change name when parameter change is True. [Default:Other]}

}
\examples{
## Load data
library(EasyMicroPlot)
data(EMP)

## Result 
 beta_re <- beta_plot(data = EMP$micro,design = EMP$mapping,min_relative = 0.001,min_ratio = 0.7,distance = 'bray') 

## Plot 
 beta_re$plot$species$pic$p12

## Interactive visualization
 beta_re$plot$species$html$p12

}
