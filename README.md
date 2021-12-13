# EasyMicroPlot: An Efficient and Convenient R package in Microbiome DownStream Analysis and Visualization for Clinical Study
<a href="https://github.com/xielab2017/EasyMicroPlot/wiki"><img src="https://i.loli.net/2021/10/20/u5UfFXvxNyQhWeg.png" width=150 align="right" ></a>
![](https://img.shields.io/badge/R%20language->=3.6-brightgreen.svg)
![](https://img.shields.io/badge/Mac%20OSX%20&%20Windows-Available-brightgreen.svg)
![](https://img.shields.io/badge/Release%20version-0.5.1.9-brightgreen.svg)
[![Awesome](https://cdn.rawgit.com/sindresorhus/awesome/d7305f38d29fed78fa85652e3a63e154dd8e8829/media/badge.svg)](https://github.com/xielab2017/EasyMicroPlot)

EasyMicroPlot package aims to provide an easy-to-use microbiome analysis tool on R platform that accomplishes the core tasks of metagenomic down-stream analysis from start to finish, specially designed by screening popular microbial analysis and visualization metric in recent clinic studies.




## INSTALLATION

	if(! require("devtools")) install.packages("devtools")
	library(devtools)
	install_github("xielab2017/EasyMicroPlot",subdir='Version_0.5.0')
	# When something is wrong, please try this below
	install_github("xielab2017/EasyMicroPlot",subdir='Version_0.5.0',upgrade = 'never')


				
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
* filesstrings (>= 3.2.1)


## AMAZING FEATURE
When mapping and microbial relative abundance  filies have already been deposited in working directory,           
users could activate easy mode to perform a complete workflow with default parameters and generate results in workspace.  
Demo data at this repository: ``` Tutorial data/Easy_mode_data.7z```

```
EMP_MICRO()
``` 

```  
./Result
├── RFCV_result
│   ├── Imprortance
│   ├── model
│   └── taxonomy
├── alpha_result
│   ├── Post-Hoc
│   ├── data
│   ├── html
│   └── pic
├── beta_result
│   ├── bray_0.001_0.7
│   └── jaccard_0.001_0.7
├── cooc_result
│   └── pic
├── core_data
│   ├── species_0.001_70%.txt
│   └── species_0.001_70%_info.txt
└── structure_result
    ├── pic
    ├── taxonomy
    └── top_abundance
```



## USAGE and Tutorial
More feature and tutorial is below :  
https://github.com/xielab2017/EasyMicroPlot/wiki

## PUBLICATION LIST BASED ON EMP PACKAGE
1. (2021) Susu Zhang #, Peili Wu #, Ye Tian#, Bingdong Liu#, Liujing Huang , Zhihong Liu, Nie Lin , Ningning Xu, Yuting Ruan, Zhen Zhang, Ming Wang, Zongbing Cui, HongWei Zhou, Liwei Xie, Hong Chen, Jia SunGut Microbiota Serves a Predictable Outcome of Short-Term Low-Carbohydrate Diet (LCD) Intervention for Patients with Obesity. ***Microbiol. Spectr.***

2. (2021) Liujing Huang#, Bingdong Liu#, Zhihong Liu, Wanqin Feng, Minjuan Liu, Yifeng Wang, Dongxian Peng, Xiafei Fu, Honglei Zhu, Zongbin Cui, Liwei Xie ,Ying Ma Gut Microbiota Exceeds Cervical Microbiota for Early Diagnosis of Endometriosis. ***Frontiers Cellular and Infection Microbiology***
