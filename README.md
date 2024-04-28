# EasyMicroPlot: An Efficient and Convenient R package in Microbiome DownStream Analysis and Visualization for Clinical Study
<a href="https://github.com/xielab2017/EasyMicroPlot/wiki"><img src="https://i.loli.net/2021/10/20/u5UfFXvxNyQhWeg.png" width=150 align="right" ></a>
![](https://img.shields.io/badge/R%20language->=3.6-brightgreen.svg)
![](https://img.shields.io/badge/Mac%20OSX%20&%20Windows-Available-brightgreen.svg)
![](https://img.shields.io/badge/Release%20version-0.5.1.24-brightgreen.svg)
[![Awesome](https://cdn.rawgit.com/sindresorhus/awesome/d7305f38d29fed78fa85652e3a63e154dd8e8829/media/badge.svg)](https://github.com/xielab2017/EasyMicroPlot)

EasyMicroPlot package aims to provide an easy-to-use microbiome analysis tool on R platform that accomplishes the core tasks of metagenomic down-stream analysis from start to finish, specially designed by screening popular microbial analysis and visualization metric in recent clinic studies.




## INSTALLATION
**Github**
```
install.packages('remotes')
remotes::install_github("https://github.com/xielab2017/EasyMicroPlot",subdir='Version_0.5')
```
**Gitee**  
Make sure [git tool](https://git-scm.com/downloads) on your computer installed correctly.
```
install.packages('remotes')
remotes::install_git("https://gitee.com/xielab2017/EasyMicroPlot/",subdir='Version_0.5')
```				

## USAGE and Tutorial

* Chinese version

  <u>https://xielab2017.github.io/EasyMicroPlot_tutorial/</u>

* English version

  In the plan
  
## Version update
* 0.5.1.24

1. Fix the method parameter of adonis2 in the betaplot function.
  
* 0.5.1.23

1. Add Student's t-test and ellipse function for beta_plot moudle.
  
2. Add cex.axis parameter to fix display error for meta_summary

3. Fix some bugs.

## Next update 
The next version will be a giant change, and we will rebuild the EMP package for more exciting features.
- [ ] Introduce tidyverse grammar into analysis process
- [ ] Integate omics data into EMP package 
- [ ] Design more interactive function for new beginers
- [ ] Add more beautiful design for figures


## HOW TO CITE
If you use EMP in published research, please cite this article:
	
**Bingdong Liu**, Liujing Huang, Zhihong Liu, Xiaohan Pan, Zongbing Cui, Jiyang Pan, **Liwei Xie**. EasyMicroPlot : An Efficient and Convenient R Package in Microbiome Downstream Analysis and Visualization for Clinical Study. ***Frontiers in Genetics***. doi: [10.3389/fgene.2021.803627](https://www.frontiersin.org/articles/10.3389/fgene.2021.803627/full)

