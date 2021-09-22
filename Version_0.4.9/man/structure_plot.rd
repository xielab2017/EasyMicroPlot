\name{structure_plot}
\alias{structure_plot}
\title{structure_plot}
\usage{
structure_plot(dir,design,...)
}
\description{
Filtering data and making structure plot for Top abundance.
}
\arguments{
  \item{dir}{An directory contain microbial relative abundance at different levels.}
  \item{group_level}{Set the group order in Mapping File. [Not necessary]}
  \item{group_level}{Set the tax order in plot. [Not necessary]}
  \item{min_relative}{Set the min relative abundance for filtering.}
  \item{min_ratio}{Set the min ratio presence for taxonomy.}
  \item{design}{Mapping File.}
  \item{row_panel}{pannel array.}
  \item{pattern}{Set key string character for relative abundance files.}
  \item{palette}{Colour palette.}
  \item{output}{Output filter data at different levels. [Default:False]}
  \item{width,height}{Set the width and height of plot. [Default:10]}
  \item{structure_method}{Set select method in top abundance tax: mean(Default),median,max,min.}
  \item{estimate_group}{To choose groups in consideration for top abundance selection.}
}
\examples{
## generate Top abundance barplot (easy running in default parameters)
 barplot=structure_plot(dir = '.',min_relative = 0.001,min_ratio = 0.7,
                    design = 'mapping.txt',num = 10,pattern = 'L',output = F)

## show Top abundance taxonomy
 deposit$result$filter_data$species_ID
 deposit$result$filter_data$phylum_ID 

## multiple ways to select Top abundance tax according to mathematical algorithm and groups
 barplot=structure_plot(dir = '.',min_relative = 0.001,min_ratio = 0.7,
                    design = 'mapping.txt',num = 10,pattern = 'L',
                    output = F,structure_method='mean')
 barplot=structure_plot(dir = '.',min_relative = 0.001,min_ratio = 0.7,
                    design = 'mapping.txt',num = 10,pattern = 'L',
                    output = F,estimate_group=c('Group_A','Group_B')
 deposit$result$filter_data$phylum_ID 

## ```theme()``` is also available in ```structure_plot()```
##For example,change axis_x text in into vertical or slope direction
 newtheme_slope=newtheme_slope=theme(axis.text.x =element_text(angle = 45, hjust = 1))
 newtheme_vertical=newtheme_slope=theme(axis.text.x =element_text(angle = 90, hjust = 0.5))
 deposit=structure_plot(dir = '.',min_relative = 0.001,min_ratio = 0.7,
                        design = 'mapping.txt',num = 10,
                        pattern = 'L',output = F,mytheme = newtheme_slope)
 newtheme_vertical=newtheme_slope=theme(axis.text.x =element_text(angle = 90, hjust = 0.5))
 deposit=structure_plot(dir = '.',min_relative = 0.001,min_ratio = 0.7,
                        design = 'mapping.txt',num = 10,
                        pattern = 'L',output = F,mytheme = newtheme_vertical)         

}
