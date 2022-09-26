\docType{package}
\name{EasyMicroPlot-package}
\alias{EMP}
\alias{EMP-package}
\title{EasyMicroPlot: An Efficient and Convenient R package in Microbiome Down-Stream Analysis and Visualization for Clinical Study}

\section{Description}{
\if{html}{\figure{logo.png}{options: align='right' alt='logo' width='120'}}

EasyMicroPlot package aims to provide an easy-to-use microbiome analysis tool on R platform that accomplishes the core tasks of metagenomic downstream analysis from start to finish, specially designed by screening popular microbial analysis and visualization metric in recent clinical studies.
}

\section{Function}{
\code{\link{EMP_META}} : Meta data analysis and visualization.
\itemize{
	\item \code{\link{meta_summary}}  
 	\item \code{\link{meta_regroup}}
}
\code{\link{EMP_MICRO}} : Microbial data analysis and visualization.
\itemize{
	\item \code{\link{data_filter}} 
	\item \code{\link{alpha_plot}}  
 	\item \code{\link{beta_plot}}
 	\item \code{\link{cooc_plot}}
 	\item \code{\link{structure_plot}}
 	\item \code{\link{tax_plot}}
 	\item \code{\link{RFCV}}
 	\item \code{\link{RFCV_data_binary}}
 	\item \code{\link{RFCV_roc}}
}
\code{\link{EMP_COR}} : Correlation analysis and visualization between meta and microbial data.
\itemize{
	\item \code{\link{EMP_COR}}  
	\item \code{\link{EMP_COR_RDA}}
	\item \code{\link{EMP_COR_FIT}}
	\item \code{\link{EMP_COR_SANKEY}}
}
}

\seealso{
Useful links:
\itemize{
  \item \url{https://github.com/xielab2017/EasyMicroPlot/}
}
}


