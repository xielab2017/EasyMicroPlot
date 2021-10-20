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
  \item{data}{An list contain microbial relative abundance at different levels.}
  \item{dir}{An directory contain microbial relative abundance at different levels.}
  \item{min_relative}{Set the min relative abundance for filtering.[Default:0]}
  \item{min_ratio}{Set the min ratio presence for taxonomy.[Default:0]}
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
  \item{set_color_level}{Set colour for vertex.(phylum,class,order,family,genus,species)[Default:phylum]}
  \item{width,height}{Set the width and height of plot. [Default:10]}
}
\examples{
## Load data
data(EMP)

## Result 
## When ** cooc_output = T **, cooc picture is generated in working directory.
 cooc_re <- cooc_plot(data = EMP$micro,design = EMP$mapping,min_relative = 0.001,min_ratio = 0.7,cooc_method = 'spearman',cooc_output = T) 
##show attributes of net analysis profile at different levels 
 cooc_re$cooc_profile$species


## When some globe parameters do not works well for specific plot, 
## ** cooc_plot_each ** is prepared for this.
 cooc_plot_each(result$plot$Control$species,cooc_output=T,vertex.size = 8,
 	   vertex.label.cex =2 ,edge.width =2 )
      

}
