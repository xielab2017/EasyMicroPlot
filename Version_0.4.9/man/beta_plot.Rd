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
  \item{dir}{An directory contain microbial relative abundance at different levels.}
  \item{group_level}{Set the group order in Mapping File. [Not necessary]}
  \item{min_relative}{Set the min relative abundance for filtering.}
  \item{min_ratio}{Set the min ratio presence for taxonomy.}
  \item{design}{Mapping File.}
  \item{adjust}{Set addtional dummy tax in empty data when filtering. [Default:True]}
  \item{pattern}{Set key string character for relative abundance files.}
  \item{palette}{Colour palette.}
  \item{output}{Output filter data at different levels. [Default:False]}
  \item{html_out}{Output interactive html in three axis ay different levels. [Default:False]}
  \item{seed}{Set the random seed to generate reproducible result. [Default:123]}
  \item{width,height}{Set the width and height of interactive html. [Default:15]}
  \item{method}{Set method in multiple comparison: HSD(Default),LSD,duncan,scheffe,REGW,SNK.}
  \item{distance}{bray(Default),manhattan, euclidean, canberra, clark, kulczynski, jaccard, gower, altGower, morisita, horn, mountford, raup, binomial, chao, cao or mahalanobis.}
}
\examples{
beta_result=beta_plot(group_level=c('Ctrl','CtrlD', 'HFD', 'HFDDH'),
                      dir = '.',min_relative = 0.001,min_ratio = 0.85,
                      design = 'mapping.txt',adjust = F,pattern = 'L',html_out = F,output=F,seed = 124)
}
