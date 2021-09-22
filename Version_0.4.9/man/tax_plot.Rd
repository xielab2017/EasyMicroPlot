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
  \item{width_total}{Total plot width.}
  \item{height_total}{Total plot height.}
  \item{width}{Each plot width.}
  \item{height}{Each plot height.}
  \item{seed}{Output filter data at different levels. [Default:False]}
  \item{row_panel}{Each plot height.}
  \item{method}{Set method in comparison: HSD,ttest,LSD,duncan,scheffe,REGW,SNK}
  \item{mytheme}{Theme design.}
  \item{palette}{Colour palette.}

}
\examples{
core_sp <- data_filter(dir = '.',min_relative = 0.001,min_ratio = 0.7,
                   design = mapping,adjust = F,pattern = 'L7')  
sp <- core_sp$filter_data$species
tax_re <- tax_plot(data = sp,tax_select = 'V304',method = 'ttest')
tax_re <- tax_plot(data = sp,tax_select = 'V304',method = 'LSD')
}
