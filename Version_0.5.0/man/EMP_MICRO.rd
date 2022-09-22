\name{EMP_MICRO}
\alias{EMP_MICRO}
\title{EMP_MICRO}
\usage{
EMP_MICRO <-function(data,meta,...)
}
\description{
Microbial data analysis.
}
\arguments{
  \item{data}{An list contain microbial relative abundance at different levels.}
  \item{design}{Mapping File.[Default:mapping.txt]}
  \item{dir}{An directory contain microbial relative abundance at different levels.}
  \item{min_relative}{et the min relative abundance for filtering.[Default:0.001]}
  \item{min_ratio}{Set the min ratio presence for taxonomy.[Default:0.7]}
  \item{html_out}{Output interactive html.[Default:True]}
  \item{pattern}{Set key string character for relative abundance files.[Default:L]}
  \item{width}{width for plot.[Default:15]}
  \item{height}{height for plot.[Default:15]}
  \item{group_level}{Set the group order for plots. [Not necessary]}  
  \item{method}{Set method in multiple comparison: HSD,ttest,LSD(Default),duncan,scheffe,REGW,SNK.}
  \item{distance}{bray(Default),manhattan, euclidean, canberra, clark, kulczynski, jaccard, gower, altGower, morisita, horn, mountford, raup, binomial, chao, cao or mahalanobis.}
  \item{top_num}{Top number of abundance.[Default:10]}
  \item{tax_level}{Set the tax order in plot. [Not necessary]}
  \item{cooc_r}{Set relation value forthreshold for correlation test.[Default:0.3]}
  \item{cooc_p}{Set pvalue forthreshold for correlation test.[Default:0.05]}
  \item{vertex.size}{Vertex size in cooc plot.}
  \item{vertex.label.cex}{Vertex label size in cooc plot.}
  \item{edge.width}{Edge width. [Default:2]}
  \item{set_color_level}{Set colour for vertex.(phylum,class,order,family,genus,species)[Default:phylum]}
  \item{edge_color_positive}{Edge colour for positive relation. [Default:darkred]}
  \item{edge_color_negitive}{Edge colour for negitive relation. [Default:steelblue]}
  \item{ntree}{Tree parameters in random forest model.[Default:1000]}
  \item{RFCV_estimate}{Microbial data for RFCV.(phylum,class,order,family,genus,species)Default:species]}  
  \item{kfold}{Number of folds in the cross-validation.[Default:5]}
  \item{rep}{Data containing meta data.[Default:10]}
  \item{x_break}{RFCV curve break for x axis.[Default:1] }
  \item{RF_importance}{A vector giving the subscripts which the function will be applied over.c(1, 2) MeanDecreaseAccuracy and MeanDecreaseGini[Default:1]}
  \item{step}{Each step when RFCV model generate.[Default:1] }
  \item{output_folder}{output_folder.[Default:Result]}
}
\examples{
## Load data
 library(EasyMicroPlot)
 data(EMP)

## Analysis and result generate in the output folder
 EMP_MICRO(EMP$micro,EMP$mapping)

## When mapping and microbial relative abundance  filies have already been deposited in working directory         
## Users could activate easy mode without load data.
 EMP_MICRO()

}
