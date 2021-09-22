\name{pca_boxplot}
\alias{pca_boxplot}
\title{pca_boxplot}
\usage{
pca_boxplot(data =data ,design = design,group_level=group_level,seed=seed,method=method,distance=distance,palette=palette,html_out=html_out)
}
\description{
Makeing beta diversity plot.
}
\arguments{
  \item{data}{Dataframe generated from data_filter }
  \item{design}{Mapping File}
  \item{group_level}{Set the group order in Mapping File. [Not necessary]}
  \item{seed}{Set the random seed to generate reproducible result. [Default:123]}
  \item{method}{Set method in multiple comparison: HSD(Default),LSD,duncan,scheffe,REGW,SNK.}
  \item{distance}{bray(Default),manhattan, euclidean, canberra, clark, kulczynski, jaccard, gower, altGower, morisita, horn, mountford, raup, binomial, chao, cao or mahalanobis.}
  \item{width}{Set the width of interactive html. [Default:15]}
  \item{height}{Set the height of interactive html. [Default:15]}
  \item{html_out}{Output interactive html in three axis ay different levels. [Default:False]}
}
\examples{
## filtering the data
data=data_filter(dir = '.',design = 'mapping.txt',min_relative = 0.005,min_ratio = 0.7,
                  adjust = T,pattern = 'L4',output=F)
## modifying the data,make SampleID as rowname
rownames(data)<-data[,1]
data<-data[,-1]
data<-subset(data,select=-c(Group))
## plot all
beta=pca_boxplot(data =data ,design = 'mapping.txt',seed=12,method='HSD',distance='bray')

## gereate html
beta=pca_boxplot(data =data ,design = 'mapping.txt',seed=12,
                    method='Tukey',distance='bray')

## generate pdf
## better set seed as the fomer to make pdf as same as html output
pdf('sp_1_2.pdf',height = 15,width = 15)
beta_result$plot$species$p12
dev.off()

}
