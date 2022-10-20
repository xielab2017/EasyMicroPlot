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
  \item{data}{An list contain microbial relative abundance at different levels.}
  \item{dir}{An directory contain microbial relative abundance at different levels.}
  \item{group_level}{Set the group order in Mapping File. [Not necessary]}
  \item{tax_level}{Set the tax order in plot. [Not necessary]}
  \item{min_relative}{Set the min relative abundance for filtering. [Default:0]}
  \item{min_ratio}{Set the min ratio presence for taxonomy. [Default:0]}
  \item{num}{Top number of abundance.[Default:10]}
  \item{design}{Mapping File.}
  \item{measure}{Decide the taxa by which samples will be sorted.}
  \item{row_panel}{pannel array. [Default:NULL]}
  \item{pattern}{Set key string character for relative abundance files.}
  \item{palette}{Colour palette.}
  \item{output}{Output filter data at different levels.[Default:False]}
  \item{width,height}{Set the width and height of plot.[Default:10]}
  \item{structure_method}{Set select method in top abundance tax: mean(Default),median,max,min.}
  \item{estimate_group}{To choose groups in consideration for top abundance selection.}
  \item{change}{Modify empty taxonomy name.eg d__Bacteria;__;__ will change into d__Bacteria;p__Other;c__Other [Default:False]}
  \item{change_name}{Decide the change name when parameter change is True. [Default:Other]}

}
\examples{
## Load data
 library(EasyMicroPlot)
 data(EMP)


## Generate Top abundance barplot (easy running in default parameters)
 structure_re <- structure_plot(data = EMP$micro,design = EMP$mapping,
                                 min_relative = 0.001,min_ratio = 0.7) 

## Plot
 structure_re$pic$genus$barplot$Total
 structure_re$pic$genus$barplot$CT


## Show Top abundance taxonomy
 structure_re$result$filter_data$species
 structure_re$result$filter_data$species_ID 


## Support ggplot theme parameters
 library(ggplot2)
 newtheme_slope=theme(axis.text.x =element_text(angle = 45, hjust = 1,size = 10))
 structure_re <- structure_plot(data = EMP$micro,design = EMP$mapping,
                               min_relative = 0.001,min_ratio = 0.7,mytheme =newtheme_slope)
 structure_re$pic$family$barplot$Total
         

}
