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
  \item{data}{An list or dataframe contain microbial relative abundance at different levels.}
  \item{dir}{An directory contain microbial relative abundance at different levels.}
  \item{meta_data}{Meta_data in dataffrmae format whose first column must be SampleID.}
  \item{min_relative}{Set the min relative abundance for filtering.[Default:0]}
  \item{min_ratio}{Set the min ratio presence for taxonomy.[Default:0]}
  \item{design}{Mapping File.}
  \item{group_combie}{Combie the data into cooc analysis regardless of group. [Default:False]}
  \item{cooc_method}{"pearson" , "kendall", or "spearman" [Default:spearman]}
  \item{cooc_p}{Set pvalue forthreshold for correlation test. [Default:0.05]}
  \item{cooc_r}{Set relation value forthreshold for correlation test. [Default:0.3]}
  \item{vertex.size}{Vertex size.}
  \item{vertex.label.cex}{Vertex label size.}
  \item{edge.width}{Edge width. [Default:2]}
  \item{edge.curved}{edge.curved. [Default:False]}
  \item{edge_color_positive}{Edge colour for positive relation. [Default:darkred]}
  \item{edge_color_negitive}{Edge colour for negitive relation. [Default:steelblue]}
  \item{meta_col}{Colour for meta data vertex. [Default:white]}
  \item{clust}{Determin the clust for vertex by fast_greedy algorithm. [Default:False]}
  \item{seed}{Random number for plot layout. [Default:123]}
  \item{output}{Output filter data at different levels. [Default:False]} 
  \item{cooc_output}{Output cooc plot. [Default:False]}
  \item{set_color_level}{Set colour for vertex.(phylum,class,order,family,genus,species)[Default:phylum]}
  \item{width,height}{Set the width and height of plot. [Default:10]}
  \item{heatmap_width,heatmap_height}{Set the width and height of heatmap for vertex importance.}
  \item{change}{Modify empty taxonomy name.eg d__Bacteria;__;__ will change into d__Bacteria;p__Other;c__Other [Default:False]}
  \item{change_name}{Decide the change name when parameter change is True. [Default:Other]}

}
\examples{
## Load data
data(EMP)

## Result 
## When ** cooc_output = T **, cooc picture is generated in working directory.
 cooc_re <- cooc_plot(data = EMP$micro,design = EMP$mapping,min_relative = 0.001,min_ratio = 0.7,cooc_method = 'spearman',cooc_output = T) 
##show attributes of net analysis profile at different levels 
 cooc_re$cooc_profile$species

# Add meta data into cooc analysis
 cooc_re <- cooc_plot(data = EMP$micro,design = EMP$mapping,meta = EMP$iron,min_relative = 0.001,
                      min_ratio = 0.7,cooc_method = 'spearman',cooc_output = T) 



## When some globe parameters do not works well for specific plot, 
## ** cooc_plot_each ** is prepared for this.
 cooc_plot_each(result$plot$Control$species,cooc_output=T,vertex.size = 8,
 	   vertex.label.cex =2 ,edge.width =2 )
      

}
