# EasyMicroPlot-- A easy R script plot  for Microbiome  analysis

![](https://img.shields.io/badge/R--language->=3.6-brightgreen.svg)
![](https://img.shields.io/badge/Mac--OSX&Windows-Available-brightgreen.svg)
![](https://img.shields.io/badge/Version-0.4.9.9-brightgreen.svg)
[![Awesome](https://cdn.rawgit.com/sindresorhus/awesome/d7305f38d29fed78fa85652e3a63e154dd8e8829/media/badge.svg)](https://github.com/xielab2017/EasyMicroPlot)

EasyMicroPlot packages aims to provide an easy-to-use microbiome analysis tool on R platform that accomplishes the core tasks of metagenomic down-stream analysis, specially designed by screening popular microbial analysis and visualization metric in recent clinic studies.This package was specially designed to address problems above and perform regular down-stream analysis covering core tasks of metagenomic analysis from start to finish for clinic meta-data and microbiomics computation and visualization.


## PACKAGES DEPENDENCIES 
* vegan (>= 2.5-6)
* ape (>= 5.3) 
* grid (>= 3.5.1)
* plyr (>= 1.8.6)
* dplyr (>= 1.0.2)
* multcomp (>= 1.4-14)
* patchwork (>= 1.0.1)
* fs (>= 1.5.0)
* stringr (>= 1.4.0)
* htmlwidgets (>= 1.5.3)
* ggiraph (>= 0.7.0)
* ggpubr (>= 0.4.0)
* ggplot2
* randomForest (>= 4.6-14)
* purrr (>= 0.3.4)
* reshape2 (>= 1.4.4)
* psych (>= 2.0.12)
* IM (>= 5.1.1)
* table1 (>= 1.4.2)
* pROC (>= 1.17.0.1)
* orrplot (>= 0.84)
* pheatmap (>= 1.0.12)
* igraph (>= 1.2.6)

## INSTALLATION

	if(! require("devtools")) install.packages("devtools")
	library(devtools)
	install_github("xielab2017/EasyMicroPlot",subdir='Version_0.4.9')


				
## Main FUNCTION

* data_filter
* pca_boxplot
* alpha_plot
* beta_plot
* alpha_caculate
* structure_plot
* cooc_plot
* tax_plot
* RFCV
* RFCV_roc
* RFCV_data_binary
* cor_plot_heat
* cor_plot_detail

## USAGE and Tutorial
https://github.com/xielab2017/EasyMicroPlot/wiki

