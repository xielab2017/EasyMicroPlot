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
  \item{data}{An list contain microbial relative abundance at different levels.}
  \item{dir}{An directory contain microbial relative abundance at different levels.}
  \item{group_level}{Set the group order in Mapping File. [Not necessary]}
  \item{min_relative}{Set the min relative abundance for filtering.[Default:0]}
  \item{min_ratio}{Set the min ratio presence for taxonomy.[Default:0]}
  \item{design}{Mapping File.}
  \item{row_panel}{Pannel array.[Default:NULL]}
  \item{pattern}{Set key string character for relative abundance files.}
  \item{palette}{Colour palette.}
  \item{output}{Output filter data at different levels. [Default:False]}
  \item{html_out}{Output interactive html. [Default:False]}
  \item{seed}{Set the random seed to generate reproducible result. [Default:123]}
  \item{width,height}{Set the width and height of interactive html. [Default:10]}
  \item{method}{Set method in multiple comparison: HSD(Default),ttest,LSD,duncan,scheffe,REGW,SNK.}
  \item{mytheme}{Support ggplot theme parameters.}

}
\examples{
## Load data
 library(EasyMicroPlot)
 data(EMP)

## For Student -Test in comparation
 alpha_re <- alpha_plot(data = EMP$micro,design = EMP$mapping,min_relative = 0.001,min_ratio = 0.7,
           method = 'ttest') 
## For one-way ANOVA and Post Hoc test
 alpha_re <- alpha_plot(data = EMP$micro,design = EMP$mapping,min_relative = 0.001,min_ratio = 0.7,
           method = 'LSD')
## Support ggplot theme parameters
 library(ggplot2)
 newtheme_slope=theme(axis.text.x =element_text(angle = 45, hjust = 1,size = 10))
 alpha_re=alpha_plot(data = EMP$micro,design = EMP$mapping,min_relative = 0.001,min_ratio = 0.7,
                    method = 'ttest',mytheme = newtheme_slope) 

## Plot 
 alpha_re$plot$species$pic$Total # for species 
 alpha_re$plot$phylum$pic$Total # for phylum

## Interactive visualization
 alpha_re$plot$species$html$Total

}
