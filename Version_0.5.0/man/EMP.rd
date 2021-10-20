\docType{package}
\name{EasyMicroPlot-package}
\alias{EMP}
\alias{EMP-package}
\title{EasyMicroPlot: A Standard and Convenient R package in Microbiome Down-Stream Analysis and Visualization for Clinical Study}

\section{Description}{
\if{html}{\figure{logo.png}{options: align='right' alt='logo' width='120'}}

EasyMicroPlot packages aims to provide an easy-to-use microbiome analysis tool on R platform that accomplishes the core tasks of metagenomic down-stream analysis, specially designed by screening popular microbial analysis and visualization metric in recent clinic studies.This package was specially designed to address problems above and perform regular down-stream analysis covering core tasks of metagenomic analysis from start to finish for clinic meta-data and microbiomics computation and visualization.
}

\section{Function}{
EMP META: Meta data analysis and visualization.
\itemize{
	\item \code{\link{EMP_META}} 
	\item \code{\link{meta_summary}}  
 	\item \code{\link{meta_regroup}}
}
EMP MICRO: Microbial data analysis and visualization.
\itemize{
	\item \code{\link{EMP_MICRO}} 
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
EMP COR: Correlation analysis and visualization between meta and microbial data.
\itemize{
	\item \code{\link{EMP_COR}}  
}
}

\seealso{
Useful links:
\itemize{
  \item \url{https://github.com/xielab2017/EasyMicroPlot/wiki}
  \item \url{https://gitee.com/xielab2017/EasyMicroPlot/wiki}
}
}


