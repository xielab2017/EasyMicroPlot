#' @import ggplot
#' @importFrom table1 table1
#' @importFrom VIM aggr





meta_regroup_str <- function(data,new_col_id='new_col',col_str,keep_col,regroup=F,silent=F){
  deposit <- list()
  data_unite <- data[col_str]
  data_unite <- na.omit(data_unite)
  col_combie <- as.character(apply(data_unite, 1, function(m){paste(m,collapse = '_')}))
  new_data <- data[rownames(data_unite),]
  new_data  <- new_data[keep_col]
  new_data[new_col_id] <- col_combie
  if (regroup == T) {
    group_name <- sort(unique(col_combie))
    Group_palette <- paste('Group',LETTERS[1:length(group_name)],sep = '_')
    if (length(group_name) > 26) {
          Group_palette <-paste('Group',sprintf(paste0('%0',nchar(123),'d'), 1:length(group_name)),sep = '_')
    }
    new_data[paste0('regroup')] <- factor(new_data[,new_col_id],labels = Group_palette)
  }
  deposit$regroup_data <- new_data
  data_str_clust_best <-  length(unique(new_data[,new_col_id]))
  if (silent == F) {
  cat("***********************************************************************", "\n")
  cat("\n","                  ***** Conclusion *****                           ", "\n", "\n")
  cat("***:  According to the majority rule, the best number of clusters is ",data_str_clust_best , "\n", "\n", "\n")
  cat("***********************************************************************", "\n")
  }
  return(deposit)
}

meta_regroup_sort <- function(data,new_col_id = 'Group',regroup_level,keep_col = 'SampleID',regroup_name = 'regroup'){
  data <- data[,-which(colnames(data) %in% regroup_name)]
  Group_palette <- paste('Group',LETTERS[1:length(unique(data[,new_col_id]))],sep = '_')
  if (length(unique(data[,new_col_id])) > 26) {
          Group_palette <-paste('Group',sprintf(paste0('%0',nchar(123),'d'), 1:length(unique(data[,new_col_id]))),sep = '_')
  }

  data[paste0(regroup_name)] <- factor(data[,new_col_id],levels = regroup_level,labels = Group_palette)
  return(data)
}


meta_summary <- function(data,estimate_group,missing_plot=T,keep_col= 'SampleID',width=20,height=20,cex.axis=0.5){
  deposit <- list()
  if (keep_col == 1) {
    data <- data[,-1]
  }else{
    data <- data[,-which(colnames(data) %in% paste0(keep_col))]
  }
  formula_set <- as.formula(paste(' ~ . | ',estimate_group))
  VIM::aggr(data,prop=F, numbers=T,combined=F,plot=missing_plot)
  deposit[['summary_info']] <- summary(data)
  deposit[['summary_table']] <- table1::table1(formula_set, data=data)
  deposit[['missing_plot']] <- VIM::aggr(data,prop=F, numbers=T,combined=F,plot=missing_plot,cex.axis=cex.axis)
  if (missing_plot==T) {
    pdf(paste0('missing_plot.pdf'),width = width,height = height)
    VIM::aggr(data,prop=F, numbers=T,combined=F,plot=missing_plot,cex.axis=cex.axis)
    dev.off()
  }
  return(deposit)
}



meta_regroup_combie <- function(data,new_col_id = 'Group',col_str,col_num,keep_col = 'SampleID',
                         regroup = T,clust_min=2,clust_max=15,clust_method='kmeans',
                         clust_dis="euclidean",silent=F){
  deposit <- list()
  data <- data[c(keep_col,col_str,col_num)]
  data <- na.omit(data)
  data_str <- data[col_str]
  data_num <- data[col_num]
  # 首先利用nbclust将连续变量转换为等级变量
  # 过滤缺失值，并进行标准化
  data_num <- scale(data_num)
  data_num_clust <- NbClust_modified(data_num, distance=clust_dis, min.nc=clust_min, max.nc=clust_max, method=clust_method)
  # 连续变量聚类后投票示意图
  data_num_clust_vote<-as.data.frame(table(data_num_clust$Best.nc[1,]))
  data_num_clust_plot <- ggplot2::ggplot(data_num_clust_vote, ggplot2::aes(Var1, Freq)) + ggplot2::xlab('Cluster groups for Continuous variable')+
    ggplot2::geom_bar(stat="identity", position="dodge",fill='red')+ggplot2::theme(panel.grid = ggplot2::element_blank(), panel.background = ggplot2::element_rect(color = 'black', fill = 'transparent'))+
    ggplot2::scale_y_continuous(breaks=seq(round(max(data_num_clust_vote$Freq),0)))
  data_temp <- data[c(keep_col,col_str)]
  # 注意这里计算仅为得到等级变量的具体分类数目
  # 注意这里需要关闭多余的输出信息 silent选项
  new_data_str <-meta_regroup_str(data=data_temp,new_col_id = new_col_id,col_str =col_str,
                                  keep_col = keep_col,regroup = F,silent= T) 
  data_str_clust_best <- length(unique(new_data_str$regroup_data[,new_col_id]))
  
  # 增加连续变量的分组信息
  data_temp$num_regroup <- data_num_clust$Best.partition
  new_data_total <- meta_regroup_str(data=data_temp,new_col_id = new_col_id,
                                     col_str =c(col_str,'num_regroup'),
                                     keep_col = keep_col,regroup = regroup,silent= T)
  
  data_num_clust_best <- as.character(data_num_clust_vote$Var1[which.max(data_num_clust_vote$Freq)])
  data_clust_best <- length(unique(new_data_total$regroup_data[,new_col_id]))
  if (silent == F) {
    cat("***********************************************************************", "\n")
    cat(paste ("*** : The best number of clusters for Continuous variable data is ",data_num_clust_best, "\n", "\n"))
    cat(paste ("*** : The best number of clusters for Categorical variable data is ",data_str_clust_best, "\n", "\n"))
    cat("\n","                  ***** Conclusion *****                           ", "\n", "\n")
    cat("***:  According to the majority rule, the best number of clusters is ",data_clust_best , "\n", "\n", "\n")
    cat("***********************************************************************", "\n")
  }
  # 存储数据
  deposit$regroup_data <- new_data_total$regroup_data
  deposit$regroup_info$data_num_clust_best <- data_num_clust_best
  deposit$regroup_info$data_str_clust_best <- data_str_clust_best
  deposit$regroup_info$data_clust_best <- data_clust_best
  deposit$regroup_info$data_num_clust_plot <- data_num_clust_plot
  return(deposit)
}



meta_regroup_num <- function(data,new_col_id = 'Group',col_num,keep_col = 'SampleID',
                             regroup = T,clust_min=2,clust_max=15,clust_method='kmeans',
                             clust_dis="euclidean",silent=F){
  deposit <- list()
  data <- data[c(keep_col,col_num)]
  data <- na.omit(data)
  data_num <- data[col_num]
  # 首先利用nbclust将连续变量转换为等级变量
  # 过滤缺失值，并进行标准化
  data_num <- scale(data_num)
  data_num_clust <- NbClust_modified(data_num, distance=clust_dis, min.nc=clust_min, max.nc=clust_max, method=clust_method)
  # 连续变量聚类后投票示意图
  data_num_clust_vote<-as.data.frame(table(data_num_clust$Best.nc[1,]))
  data_num_clust_plot <- ggplot2::ggplot(data_num_clust_vote, ggplot2::aes(Var1, Freq)) + ggplot2::xlab('Cluster groups for Continuous variable')+
    ggplot2::geom_bar(stat="identity", position="dodge",fill='red')+ggplot2::theme(panel.grid = ggplot2::element_blank(), panel.background = ggplot2::element_rect(color = 'black', fill = 'transparent'))+
    ggplot2::scale_y_continuous(breaks=seq(round(max(data_num_clust_vote$Freq),0)))
  
  
  # 增加连续变量的分组信息
  data$num_regroup <- data_num_clust$Best.partition
  new_data_total <- meta_regroup_str(data=data,new_col_id = new_col_id,
                                     col_str =c('num_regroup'),
                                     keep_col = keep_col,regroup = regroup,silent=T)
  
    data_num_clust_best <- as.character(data_num_clust_vote$Var1[which.max(data_num_clust_vote$Freq)])
  if (silent == F) {
    cat("***********************************************************************", "\n")
    cat("\n","                  ***** Conclusion *****                           ", "\n", "\n")
    cat("***:  According to the majority rule, the best number of clusters is ",data_num_clust_best , "\n", "\n", "\n")
    cat("***********************************************************************", "\n")
  }
  # 存储数据
  deposit$regroup_data <- new_data_total
  deposit$regroup_info$data_num_clust_best <- data_num_clust_best
  deposit$regroup_info$data_num_clust_plot <- data_num_clust_plot
  return(deposit)
}


meta_regroup <- function(data,new_col_id = 'Group',col_str=NULL,col_num=NULL,keep_col = 'SampleID',
                          regroup = T,clust_min=2,clust_max=15,clust_method='kmeans',
                          clust_dis="euclidean",silent=F){
  deposit <- list()
  if( !is.null(col_str) & !is.null(col_num) ){
    
    deposit<-meta_regroup_combie(data = data,new_col_id = new_col_id,col_str = col_str,col_num = col_num,
                 keep_col = keep_col,regroup = regroup,silent = silent,clust_min=clust_min,clust_max=clust_max)
     }else if(!is.null(col_str) & is.null(col_num)){
       deposit<-meta_regroup_str(data = data,new_col_id = new_col_id,col_str = col_str,
                             keep_col = keep_col,regroup = regroup)
     }else if(is.null(col_str) & !is.null(col_num)){
       deposit<-meta_regroup_num(data = data,new_col_id = new_col_id,
                             col_num = col_num,clust_min=clust_min,clust_max=clust_max,clust_method=clust_method,clust_dis=clust_dis,
                             keep_col = keep_col,regroup = regroup,silent = silent)
     }else if ( is.null(col_str) & is.null(col_num)) {
       warning('Please set correct column for Continuous variable or ategorical variable!')
       
     }
    return(deposit)
}  



