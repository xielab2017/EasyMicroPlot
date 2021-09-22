\name{alpha_plot}
\alias{alpha_plot}
\title{alpha_plot}
\usage{
alpha_plot(dir,design,...)
}
\description{
Filtering data and making alpha diversity plot.
}
\arguments{
  \item{dir}{An directory contain microbial relative abundance at different levels.}
  \item{group_level}{Set the group order in Mapping File. [Not necessary]}
  \item{min_relative}{Set the min relative abundance for filtering.}
  \item{min_ratio}{Set the min ratio presence for taxonomy.}
  \item{design}{Mapping File.}
  \item{row_panel}{pannel array.}
  \item{pattern}{Set key string character for relative abundance files.}
  \item{palette}{Colour palette.}
  \item{output}{Output filter data at different levels. [Default:False]}
  \item{html_out}{Output interactive html in three axis ay different levels. [Default:False]}
  \item{seed}{Set the random seed to generate reproducible result. [Default:123]}
  \item{width,height}{Set the width and height of interactive html. [Default:10]}
  \item{method}{Set method in multiple comparison: HSD(Default),ttest,LSD,duncan,scheffe,REGW,SNK.}
}
\examples{
## For Student -Test in comparation
 alpha_plot(dir = '.',seed = 123,min_relative = 0.001,min_ratio = 0.7,
 design = 'mapping.txt',pattern = 'L',method = 'ttest',output = F,html_out = F) 
## For one-way ANOVA and Post Hoc test
 alpha_plot(dir = '.',seed = 123,min_relative = 0.001,min_ratio = 0.7,
 design = 'mapping.txt',pattern = 'L',method = 'LSD',output = F,html_out = F)

}
