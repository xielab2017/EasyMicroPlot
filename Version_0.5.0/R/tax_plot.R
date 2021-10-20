tax_plot_ttest <- function(data,tax_select=NULL,width_total=20,height_total=20,width=10,height=8,seed=123,row_panel=NULL,mytheme=theme(),palette=c("#E64B35FF","#4DBBD5FF","#00A087FF","#3C5488FF","#F39B7FFF","#8491B4FF",
          "#B2182B","#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7","#CC6666")){
  if (!is.null(tax_select)) {
    data <- data[,c('SampleID','Group',tax_select)]
  }
  deposit <- list()
  # 获得比较组
  group_name=as.character(unique(data$Group))
  group_combn=combn(group_name,2)
  compare=plyr::alply(group_combn,2)
  
  # 获得特征名字
  feature_name=colnames(data)[!colnames(data)%in%c('SampleID','Group')]
  
  # 绘图
  ## 分图
  for (i in feature_name) {
    p1 <- eval(substitute(ggplot(data=data,aes(x=Group,y=data[,i] ))+geom_boxplot(aes(fill=Group),colour="black",notch=F,outlier.colour=NA)+
      ylab(i)+ggpubr::stat_compare_means(comparisons = compare,method="t.test")+scale_fill_manual(values=palette)+
      ggiraph::geom_jitter_interactive(aes(tooltip = paste0(SampleID,' : ',data[,i])),position = position_jitter(height = .00000001))+theme_bw() + mytheme, list( i =i)))
    # 注意html生成时应基于随机种子
    set.seed(seed)
    p1_html <- ggiraph::girafe(code = print(p1),width = width,height = height)
    deposit$pic[[i]] <- p1
    deposit$html[[i]] <- p1_html
  }
  ## 总图
  options(warn=-1)
  data_combie = reshape2::melt(data,id.vars = c('SampleID','Group'))
  options(warn=1)
  p_total <- ggplot(data_combie, aes(x=Group,y=value ,fill = Group)) +
    geom_boxplot(aes(fill=Group),colour="black",notch=F,outlier.colour=NA) +
    facet_wrap(~variable, scales = 'free_y', ncol = row_panel) +
    ggpubr::stat_compare_means(comparisons = compare,method="t.test")+
    scale_fill_manual(values =palette) +
    labs(x = '', y = 'Relative abundance')+ 
    ggiraph::geom_jitter_interactive(aes(tooltip = paste0(SampleID,' : ',data[,i])),position = position_jitter(height = .00000001))+theme_bw() + mytheme
  
  set.seed(seed)
  p_total_html <- ggiraph::girafe(code = print(p_total),width = width_total,height = height_total)
  deposit$pic$total <- p_total
  deposit$html$total <- p_total_html
  return(deposit)
}

tax_plot_mtest <- function(data,tax_select=NULL,width_total=20,height_total=20,width=10,height=8,seed=123,row_panel=NULL,method='HSD',mytheme=theme(),palette=c("#E64B35FF","#4DBBD5FF","#00A087FF","#3C5488FF","#F39B7FFF","#8491B4FF",
          "#B2182B","#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7","#CC6666")){
  if (!is.null(tax_select)) {
  data <- data[,c('SampleID','Group',tax_select)]
  }
  deposit <- list()
  # 获得特征名字
  feature_name <- colnames(data)[!colnames(data)%in%c('SampleID','Group')]
  # 获得mapping 便于后边多组分析
  mapping <- data[c('SampleID','Group')]
  
  # 获得单因素方差分析结果及字母标记法
  fit_model <-list()
  fit_model_info <- list()
  for (i in feature_name) {
    fit_model[[i]] <- aov(data[,paste0(i)]~Group,data = data)
    fit_model_info[[i]] <- summary(fit_model[[i]])
  }  
  
  fit_test <- list()
  for (i in feature_name) {
    fit_test[[i]] <- multi_test(fit_model[[i]],method,mapping)
  }
  
  # 获得各指数的检验字母标记
  fit_test_list <- list()
  for (i in feature_name) {
    data_test <- fit_test[[i]]$letter
    colnames(data_test) <- c(paste0(i),'Group')
    fit_test_list[[i]] <- data_test
  }
  
  # 获得各指数的检验具体信息
  fit_test_info <- list()
  for (i in feature_name) {
    data_test_info <- fit_test[[i]]$comparison
    fit_test_info[[i]] <- data_test_info
  }
  
  tax_result_letter <- fit_test_list %>% purrr::reduce(dplyr::full_join, by ="Group")
  
  # 主要这里使用关闭警告再打开，避免输出过程中产生不必要的警告提示
  suppressWarnings(tax_result_letter_for_total <- reshape2::melt(tax_result_letter,id.vars = c('Group')))

  
  # 计算出各个分面的极值，方便字母标记的定位
  ymax=list()
  for (i in feature_name) {
    ymax[[i]]=max(data[[i]])
  }
  
  for (i in feature_name) {
    id <- with(tax_result_letter_for_total, variable==i)
    tax_result_letter_for_total$ymax[id]=ymax[[i]]
  }
  
  # 绘图
  ## 分图
  for (i in feature_name) {
    p1 <- eval(substitute(ggplot(data=data,aes(x=Group,y=data[,i] ))+geom_boxplot(aes(fill=Group),colour="black",notch=F,outlier.colour=NA)+
      ylab(i)+scale_fill_manual(values=palette)+
      geom_text(data = tax_result_letter,aes(x = Group,y=max(data[,i])*1.1,label = tax_result_letter[,i]),
                size = 7,color = "black") + theme_bw() + mytheme +
      ggiraph::geom_jitter_interactive(aes(tooltip = paste0(SampleID,' : ',data[,i])),position = position_jitter(height = .00000001)),list(i=i))) 
    # 注意html生成时应基于随机种子
    set.seed(seed)
    p1_html <- ggiraph::girafe(code = print(p1),width = width,height = height)
    deposit$pic[[i]] <- p1
    deposit$html[[i]] <- p1_html
  }
  
  ## 总图
  options(warn=-1)
  data_combie = reshape2::melt(data,id.vars = c('SampleID','Group'))
  options(warn=1)
  p_total <- ggplot(data_combie, aes(x=Group,y=value ,fill = Group)) +
    geom_boxplot(aes(fill=Group),colour="black",notch=F,outlier.colour=NA) +
    geom_text(data = tax_result_letter_for_total,aes(x = Group,y=ymax*1.1,label = value),size = 7,color = "black")+
    facet_wrap(~variable, scales = 'free_y', ncol = NULL) +
    scale_fill_manual(values =palette) +
    labs(x = '', y = 'Relative abundance') + theme_bw() + mytheme + 
    ggiraph::geom_jitter_interactive(aes(tooltip = paste0(SampleID,' : ',value)),position = position_jitter(height = .00000001))
  
  set.seed(seed)
  p_total_html <- ggiraph::girafe(code = print(p_total),width = width_total,height = height_total)
  deposit$pic$total <- p_total
  deposit$html$total <- p_total_html
  deposit$Post_Hoc <- fit_test_info
  return(deposit)
}

tax_plot <- function(data,tax_select=NULL,width_total=20,height_total=20,width=10,height=8,seed=123,row_panel=NULL,html_out=F,method='HSD',mytheme=theme(),palette=c("#E64B35FF","#4DBBD5FF","#00A087FF","#3C5488FF","#F39B7FFF","#8491B4FF",
                                                                                                                                                          "#B2182B","#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7","#CC6666")){
  deposit <- list()
  mtest_pool=c('HSD','LSD','duncan','scheffe','REGW','SNK')
  if (method %in% mtest_pool){
    deposit <- tax_plot_mtest(data=data,tax_select=tax_select,width_total=width_total,height_total=height_total,width=width,height=height,seed=seed,row_panel=row_panel,method=method,mytheme=mytheme,palette=palette)
  }else if(method == 'ttest'){
    deposit <- tax_plot_ttest(data=data,tax_select=tax_select,width_total=width_total,height_total=height_total,width=width,height=height,seed=seed,row_panel=row_panel,mytheme=mytheme,palette=palette)
  }else{
    warning('Plz select one of these method : HSD, LSD, duncan, scheffe, REGW, SNK, ttest')
  }
  if (html_out==T) {
    html_name <- names(deposit$html)
    for (i in html_name) {
      htmlwidgets::saveWidget(deposit$html[[i]], paste0(i,'_boxplot.html'))
    }
  }
 return (deposit)
}


