\name{meta_regroup}
\alias{meta_regroup}
\title{meta_regroup}
\usage{
meta_regroup <- function(data,new_col_id = 'Group'...)
}
\description{
Regroup for meta data.
}
\arguments{
  \item{data}{Data containing meta data.}
  \item{new_col_id}{Set new group id.[Default:Group]}
  \item{col_str}{Select categorical column.}
  \item{col_num}{Select continuous column.}
  \item{keep_col}{Select reserved column.[Default:SampleID]}
  \item{regroup}{Generate labeled regroup.Default:True] }
  \item{clust_min}{Set minimum cluster for continuous variable.[Default:2]}
  \item{clust_max}{Set maximum cluster for continuous variable.[Default:15]}
  \item{clust_method}{Set method in clust:ward.D, ward.D2,single,complete,average,mcquitty,median,centroid,kmeans. [Default:kmeans]}
  \item{clust_dis}{Set method in distance:euclidean,maximum,manhattan,canberra,binary,minkowski. [Default:euclidean]}
  \item{silent}{logical: should the report of result messages be suppressed?. [Default:False]}
}
\details{

\enumerate{
  \item \bold{Notes on the "clust_dis" argument\cr}  
  The following distance measures are written for two vectors \bold{x} and \bold{y}. They are used when the data is a \strong{d}-dimensional vector arising from measuring \bold{d} characteristics on each of \bold{n} objects or individuals.
  \itemize{
    \item \strong{Euclidean distance} : Usual square distance between the two vectors (2 norm).
    \deqn{d(x,y)=\left(\sum_{j=1}^{d}\left(x_{j}-y_{j}\right)^{2}\right) ^{\frac{1}{2}}}{d(x,y)= (sum_{j=1}^{d} (x_j - y_j)^2)^(1/2)}
    \item \strong{Maximum distance}: Maximum distance between two components of \bold{x} and \bold{y} (supremum norm).
    \deqn{d(x,y)=\sup_{1\leq j\leq d}\left|x_{j}-y_{j}\right|}{d(x,y)= sup|x_j - y_j|, 1\le j \le d}
    \item \strong{Manhattan distance} : Absolute distance between the two vectors (1 norm).
    \deqn{d(x,y)=\sum_{j=1}^{d}\left|x_{j}-y_{j}\right|}{d(x,y)= sum_{j=1}^{d}|x_j - y_j|}
    \item \strong{Canberra distance} : Terms with zero numerator and denominator are omitted from the sum and treated as if the values were missing.
    \deqn{d(x,y)=\sum_{j=1}^{d}\frac{\left|x_{j}-y_{j}\right|}{\left|x_{j}\right|+\left|y_{j}\right|}}{sum_{j=1}^{d}|x_j - y_j|) / (|x_j|+|y_j|)}
    \item \strong{Binary distance} : The vectors are regarded as binary bits, so non-zero elements are "on" and zero elements are "off". The distance is the proportion of bits in which only one is on amongst those in which at least one is on.
    \item \strong{Minkowski distance} : The \bold{p} norm, the \eqn{p^{th}} root of the sum of the \eqn{p^{th}} powers of the differences of the components.
    \deqn{d(x,y)=\left(\sum_{j=1}^{d}\left|x_{j}-y_{j}\right|^{p}\right) ^{\frac{1}{p}}}{d(x,y)= (sum_{j=1}^{d} |x_j - y_j|^p)^(1/p)}
  }
   \item \bold{Notes on the "clust_method" argument\cr}
  The following aggregation methods are available in this package. 
  \itemize{
      \item \bold{Ward} : Ward method minimizes the total within-cluster variance. At each step the pair of clusters with minimum cluster distance are merged. To implement this method, at each step find the pair of clusters that leads to minimum increase in total within-cluster variance after merging.
      Two different algorithms are found in the literature for Ward clustering. The one used by option "ward.D" (equivalent to the only Ward option "ward" in R versions <= 3.0.3) does not implement Ward's (1963) clustering criterion, whereas option "ward.D2" implements that criterion (Murtagh and Legendre 2013). With the latter, the dissimilarities are squared before cluster updating.
\item \bold{Single} : The distance \eqn{D_{ij}}{D_ij} between two clusters \eqn{C_{i}}{C_i} and \eqn{C_{j}}{C_j} is the minimum distance between two points \eqn{x} and \eqn{y}, with \eqn{x \in C_{i}, y \in C_{j}}{x in C_i, y in C_j}. 
\deqn{D_{ij}=min_{x\in C_{i}, y\in C_{j}}d(x,y)}{D_ij= min d(x,y), x in C_i and y in C_j}
    A drawback of this method is the so-called chaining phenomenon: clusters may be forced together due to single elements being close to each other, even though many of the elements in each cluster may be very distant to each other.
      \item \bold{Complete} : The distance \eqn{D_{ij}}{D_ij} between two clusters \eqn{C_{i}}{C_i} and \eqn{C_{j}}{C_j} is the maximum distance between two points \eqn{x} and \eqn{y}, with \eqn{x \in C_{i}, y \in C_{j}}{x in C_i, y in C_j}.
     \deqn{D_{ij}=max_{x\in C_{i}, y\in C_{j}}d(x,y)}{D_ij= max d(x,y), x in C_i, y in C_j}
      \item \bold{Average} : The distance \eqn{D_{ij}}{D_ij} between two clusters \eqn{C_{i}}{C_i} and \eqn{C_{j}}{C_j} is the mean of the distances between the pair of points {x} and {y}, where \eqn{x \in C_{i}, y \in C_{j}}{x in C_i, y in C_j}.
      \deqn{D_{ij}=sum_{x\in C_{i}, y\in C_{j}}\frac{d(x,y)}{n_{i}\times n_{j}}}{D_ij= sum d(x,y)/(n_i * n_j), x in C_i and y in C_j} where \eqn{n_{i}}{n_i} and \eqn{n_{j}}{n_j} are respectively the number of elements in clusters \eqn{C_{i}}{C_i} and \eqn{C_{j}}{C_j}. 
      This method has the tendency to form clusters with the same variance and, in particular, small variance.  
      \item \bold{McQuitty} : The distance between clusters \eqn{C_{i}}{C_i} and \eqn{C_{j}}{C_j} is the weighted mean of the between-cluster dissimilarities:
        \deqn{D_{ij}=\left(D_{ik}+D_{il}\right)/2}{D_ij =(D_ik + D_il)/2}
      where cluster \eqn{C_{j}}{C_j} is formed from the aggregation of clusters \eqn{C_{k}}{C_k} and \eqn{C_{l}}{C_l}.  
     \item \bold{Median} : The distance \eqn{D_{ij}}{D_ij} between two clusters \eqn{C_{i}}{C_i} and \eqn{C_{j}}{C_j} is given by the following formula:  
       \deqn{D_{ij}=\frac{(D_{ik}+D_{il})}{2}-\frac{D_{kl}}{4}}{D_ij=(D_ik + D_il)/2)-(D_kl/4)}  
  where cluster \eqn{C_{j}}{C_j} is formed by the aggregation of clusters \eqn{C_{k}}{C_k} and \eqn{C_{l}}{C_l}.    
  \item \bold{Centroid} : The distance \eqn{D_{ij}}{D_ij} between two clusters \eqn{C_{i}}{C_i} and \eqn{C_{j}}{C_j} is the squared euclidean distance between the gravity centers of the two clusters, i.e. between the mean vectors of the two clusters, \eqn{\bar{x_{i}}}{\bar{x_i}} and \eqn{\bar{x_{j}}}{\bar{x_j}} respectively.
  \deqn{D_{ij}=\left\Vert \bar{x_{i}}-\bar{x_{j}}\right\Vert ^{2}}{ D_ij=||\bar{x_i}-\bar{x_j}||^{2}}
  This method is more robust than others in terms of isolated points.   
 \item \bold{Kmeans} : This method is said to be a reallocation method. Here is the general principle:
      \enumerate{
          \item Select as many points as the number of desired clusters to create initial centers.
          \item Each observation is then associated with the nearest center to create temporary clusters.
          \item The gravity centers of each temporary cluster is calculated and these become the new clusters centers.
          \item Each observation is reallocated to the cluster which has the closest center.
          \item This procedure is iterated until convergence.
        }
  }
  \item \bold{Notes on the "Index" argument\cr\cr} 
  
  The table below summarizes indices implemented in meta_regroup and the criteria used to select the optimal number of clusters. \cr 

\tabular{ll}{
      \bold{Index in NbClust}         \tab      \bold{Optimal number of clusters} \cr 
      
1.  "kl" or "all" or "alllong"        \tab Maximum value of the index \cr
    (Krzanowski and Lai 1988)         \cr
2.  "ch" or "all" or "alllong"        \tab Maximum value of the index \cr
    (Calinski and Harabasz 1974)      \cr
3.  "hartigan" or "all" or "alllong"  \tab Maximum difference between \cr
     (Hartigan 1975)                  \tab hierarchy levels of the index \cr
4.  "ccc" or "all" or "alllong"       \tab Maximum value of the index \cr
    (Sarle 1983)                                                      \cr 
5.  "scott" or "all" or "alllong"     \tab Maximum difference between \cr
    (Scott and Symons 1971)           \tab   hierarchy levels of the index \cr
6.  "marriot" or "all" or "alllong"   \tab Max. value of second differences\cr
    (Marriot 1971)                    \tab  between levels of the index \cr
7.  "trcovw" or "all" or "alllong"    \tab Maximum difference between \cr
    (Milligan and Cooper 1985)        \tab hierarchy levels of the index \cr                            
8.  "tracew" or "all" or "alllong"    \tab Maximum value of absolute second \cr
    (Milligan and Cooper 1985)        \tab differences between levels of the index\cr
9.  "friedman" or "all" or "alllong"  \tab Maximum difference between \cr
    (Friedman and Rubin 1967)         \tab hierarchy levels of the index \cr
10.  "rubin" or "all" or "alllong"    \tab Minimum value of second differences \cr  
    (Friedman and Rubin 1967)         \tab        between levels of the index \cr
11. "cindex" or "all" or "alllong"    \tab Minimum value of the index \cr
    (Hubert and Levin 1976)                                           \cr   
12.  "db" or "all" or "alllong"       \tab Minimum value of the index \cr
     (Davies and Bouldin 1979)                                        \cr    
13.   "silhouette" or "all" or "alllong" \tab Maximum value of the index \cr
    (Rousseeuw 1987)                                                    \cr  
14. "duda" or "all" or "alllong"      \tab Smallest \eqn{n_{c}} such that index > criticalValue \cr
    (Duda and Hart 1973)                                                 \cr    
15. "pseudot2" or "all" or "alllong"  \tab Smallest \eqn{n_{c}} such that index < criticalValue \cr
    (Duda and Hart 1973)                                                   \cr 
16. "beale" or "all" or "alllong"      \tab \eqn{n_{c}} such that critical value of the index >= alpha \cr
    (Beale 1969)                                                           \cr  
17. "ratkowsky" or "all" or "alllong" \tab Maximum value of the index \cr
    (Ratkowsky and Lance 1978)                                          \cr 
18. "ball" or "all" or "alllong"      \tab Maximum difference between hierarchy \cr
    (Ball and Hall 1965)              \tab  levels of the index \cr
19. "ptbiserial" or "all" or "alllong" \tab Maximum value of the index \cr
    (Milligan 1980, 1981)                                              \cr 
20. "gap" or "alllong"                 \tab Smallest \eqn{n_{c}} such that criticalValue >= 0 \cr
    (Tibshirani et al. 2001)                                                                  \cr   
21.  "frey" or "all" or "alllong"   \tab the cluster level before that index value < 1.00 \cr
     (Frey and Van Groenewoud 1972)                                                       \cr
22. "mcclain" or "all" or "alllong"\tab Minimum value of the index \cr
    (McClain and Rao 1975)  \cr
23. "gamma" or "alllong" \tab Maximum value of the index \cr
    (Baker and Hubert 1975)     \cr
24. "gplus" or "alllong"\tab Minimum value of the index \cr
    (Rohlf 1974) (Milligan 1981)  \cr
25. "tau" or "alllong" \tab Maximum value of the index \cr
    (Rohlf 1974) (Milligan 1981)  \cr
26. "dunn" or "all" or "alllong" \tab Maximum value of the index \cr
    (Dunn 1974)       \cr
27. "hubert" or "all" or "alllong" \tab Graphical method \cr
    (Hubert and Arabie 1985)  \cr
28. "sdindex" or "all" or "alllong" \tab Minimum value of the index \cr
    (Halkidi et al. 2000)   \cr
29. "dindex" or "all" or "alllong" \tab Graphical method\cr
    (Lebart et al. 2000)    \cr
30. "sdbw" or "all" or "alllong" \tab Minimum value of the index \cr
    (Halkidi and Vazirgiannis 2001) \cr
}% tabular
  
}%enumerate    

}% details

\examples{
## Load data
 library(EasyMicroPlot)
 data(EMP)

## Data preparation
 meta_data <- EMP$meta
 meta_data <- na.omit(meta_data)
 meta_data <- meta_data[meta_data$Diarrhea=='n'&meta_data$Astriction=='n'&meta_data$Antibiotics=='n'&meta_data$Synbiotics=='n',]


## Identify the categorical and continuous variable
 col_str <- 'MetS'
 col_num=colnames(meta_data)[!colnames(meta_data) \%in\% c('SampleID','MetS','Diarrhea','Astriction','Antibiotics','Synbiotics')]
 col_num

## For regroup of meta data
 re <- meta_regroup(data = meta_data,new_col_id = 'Group',col_str = col_str,
                   col_num = col_num,keep_col = 'SampleID',regroup = T,clust_min = 2,clust_max = 9)

## Show cluster details
 re$regroup_info

## Show regroup result 
 new_group <- re$regroup_data



}
