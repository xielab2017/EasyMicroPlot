#' @importFrom corrplot corrplot



cor_plot_detail <-function(data,meta_data = NULL,method = 'spearman',width=10,height=10,cor_output=T,file_name='cor_plot'){
  deposit <- list()
  try(data <- subset(data,select = -c(Group)), silent = T)
  if (!is.null(meta_data)) {
    cor_data_combie <- dplyr::inner_join(data,meta_data,by='SampleID')
  }else{
    cor_data_combie=data
  }
  
  # 改第一列 为行名，确保实体数据
  rownames(cor_data_combie) = cor_data_combie[,1]
  cor_data_combie=cor_data_combie[,-1]
  cor_matr = cor(cor_data_combie,method = method)
  res1 <- corrplot::cor.mtest(cor_data_combie, conf.level = .95,method = method,exact=FALSE)
  corrplot::corrplot(cor_matr, p.mat = res1$p, insig = "label_sig",type = "upper",tl.col="black",method = "pie",
                            sig.level = c(.001, .01, .05), pch.cex = .9, pch.col = "black")
  if (cor_output==T) {
    pdf(paste0(file_name,'.pdf'),width = width,height = height)
    corrplot::corrplot(cor_matr, p.mat = res1$p, insig = "label_sig",type = "upper",tl.col="black",method = "pie",
                       sig.level = c(.001, .01, .05), pch.cex = .9, pch.col = "black")
    dev.off()
    }
  deposit$r <- cor_matr
  deposit$p <- res1$p
  return(deposit)
}

getSig <- function(dc) {
  sc <- ''
  if (dc < 0.001) sc <- '***'
  else if (dc < 0.01) sc <- '**'
  else if (dc < 0.05) sc <- '*'
  sc
}
cor_plot_heat <-function(data,meta_data,method = 'spearman',width=10,height=10,cor_output=F,file_name='cor_plot'){
  deposit <- list()
  real_sample <- intersect(meta_data$SampleID,data$SampleID)
  data <- data[data$SampleID%in%real_sample, ]
  meta_data <- meta_data[meta_data$SampleID%in%real_sample, ]
  data=data[order(data$SampleID),]
  meta_data=meta_data[order(meta_data$SampleID),]
  try(data <- subset(data,select = -c(Group)), silent = T)
  data.corr <- psych::corr.test(data[,-1], meta_data[,-1],method = method,adjust='none')
  data.r <- data.corr$r
  data.p <- data.corr$p 
  
  min_p=apply(data.p, 1, min)
  data.r_f=data.r[min_p<0.05,]
  data.p_f <- data.p[min_p<0.05,]
  
  min_p2=apply(data.p_f, 2, min)
  data.r_f2=data.r_f[,min_p2<0.05]
  data.p_f2 <- data.p_f[,min_p2<0.05]
  
  sig.mat <- matrix(sapply(data.p_f2, getSig), nrow=nrow(data.p_f2))
  pheatmap(data.r_f2, clustering_method="average", cluster_rows=F, display_numbers=sig.mat)
  
  if (cor_output==T) {
    pheatmap(data.r_f2, clustering_method="average", cluster_rows=F, display_numbers=sig.mat,filename=paste0(file_name,'.pdf'),width=width,height=height)
    dev.off()
    }
  deposit$r <- data.r
  deposit$p <- data.p
  return(deposit)
}


EMP_COR <-function(data,meta,method = 'spearman',width=10,height=10,cor_output=F,file_name='cor_plot',aes_value=1){
  deposit <- list()
  if (aes_value == 1) {
    deposit <- cor_plot_heat(data=data,meta=meta,method=method,width=width,height=height,cor_output=cor_output,file_name=file_name)
  }else if (aes_value == 2){
    deposit <- cor_plot_detail(data=data,meta=meta,method=method,width=width,height=height,cor_output=cor_output,file_name=file_name)
  }else {
    warning('aes_value must be 1 or 2.')
  }
  return(deposit)
}







