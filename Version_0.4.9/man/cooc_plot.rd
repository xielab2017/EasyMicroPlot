\name{cooc_plot}
\alias{cooc_plot}
\title{cooc_plot}
\usage{
cooc_plot <- function(dir,...)
}
\description{
Filtering data and making co-occurrence plot.
}
\arguments{
  \item{dir}{An directory contain microbial relative abundance at different levels.}
  \item{group_level}{Set the group order in Mapping File. [Not necessary]}
  \item{min_relative}{Set the min relative abundance for filtering.}
  \item{min_ratio}{Set the min ratio presence for taxonomy.}
  \item{design}{Mapping File.}
  \item{cooc_method}{"pearson" , "kendall", or "spearman" [default]}
  \item{cooc_p}{Set pvalue forthreshold for correlation test.}
  \item{cooc_r}{Set relation value forthreshold for correlation test.}
  \item{vertex.size}{Vertex size.}
  \item{vertex.label.cex}{Vertex label size.}
  \item{edge.width}{Edge width. [Default:2]}
  \item{edge_color_positive}{Edge colour for positive relation. [Default:darkred]}
  \item{edge_color_negitive}{Edge colour for positive relation. [Default:steelblue]}
  \item{output}{Output filter data at different levels. [Default:False]} 
  \item{cooc_output}{Output cooc plot. [Default:False]}
  \item{set_color_level}{Set colour for vertex.}
  \item{width,height}{Set the width and height of interactive html. [Default:10]}
}
\examples{
 ## For easy mode
 ## ```cooc_output``` determines the output of the cooc plot in your working directory.
 result <- cooc_plot(dir = '.',min_relative = 0.001,min_ratio = 0.7,cooc_output = F,
           design = 'mapping.txt',pattern = '_L',vertex.size = 8,
           vertex.label.cex = 0.5,set_color_level = 'phylum')
 ## retouching mode
 ## When some globe parameters do not works well for specific plot, 
 ## ** cooc_plot_each ** is prepared for this.
 result <- cooc_plot(dir = '.',min_relative = 0.001,min_ratio = 0.7,cooc_output = F,
       design = 'mapping.txt',pattern = '_L',vertex.size = 8,
       vertex.label.cex = 0.5,set_color_level = 'phylum')
 cooc_plot_each(result$plot$Control$species,cooc_output=F,vertex.size = 8,
 	   vertex.label.cex =2 ,edge.width =2 )
 ### net analysis profile
 result <- cooc_plot(dir = '.',min_relative = 0.001,min_ratio = 0.7,cooc_output = F,
           design = 'mapping.txt',pattern = '_L',vertex.size = 8,
           vertex.label.cex = 0.5,set_color_level = 'phylum')
 result$cooc_profile$species # show attributes of net at different levels       

}
