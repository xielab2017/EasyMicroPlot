\name{tax_plot}
\alias{tax_plot}
\title{tax_plot}
\usage{
tax_plot(data=data,tax_select=tax_select,width_total=width_total,height_total=height_total,width=width,height=height,seed=seed,row_panel=row_panel,method=method,mytheme=mytheme,palette=palette)
}
\description{
Perform statistics measurement for bacteria taxa.
}
\arguments{
  \item{data}{A dataframe containg relative abundance and group infomation.}
  \item{tax_select}{Select the tax for comparison.}
  \item{width_total}{Total plot width. [Default:20]}
  \item{height_total}{Total plot height. [Default:20]}
  \item{width}{Each plot width. [Default:10]}
  \item{height}{Each plot height. [Default:8]}
  \item{seed}{Random number of seed. [Default:123]}
  \item{group_level}{Set the group order for plots. [Not necessary]}
  \item{method}{Set method in comparison: HSD(default),ttest,LSD,duncan,scheffe,REGW,SNK}
  \item{mytheme}{Theme design.}
  \item{palette}{Colour palette.}

}
\examples{
## Load data
 library(EasyMicroPlot)
 data(EMP)

## Data preparation 
 core_data <- data_filter(data = EMP$micro,design = EMP$mapping,
                           min_relative = 0.001,min_ratio = 0.7)
 sp <- core_data$filter_data$species


## Taxonomy boxplot
## For Student -Test in comparation
 tax_re <- tax_plot(data = sp,tax_select = c('V14','V15','V16'),method = 'ttest',width = 5,height = 5)
## For one-way ANOVA and Post Hoc test
 tax_re <- tax_plot(data = sp,tax_select = c('V14','V15','V16'),method = 'SNK',width = 5,height = 5)

## Plot
 tax_re$pic$V14

## Interactive visualization
 tax_re$html$V14

## Support ggplot theme parameters
 library(ggplot2)
 newtheme_slope=theme(axis.text.x =element_text(angle = 45, hjust = 1,size = 10))
 tax_re <- tax_plot(data = sp,tax_select = c('V14','V15','V16'),method = 'ttest',mytheme = newtheme_slope,width = 5,height = 5)
 


}
