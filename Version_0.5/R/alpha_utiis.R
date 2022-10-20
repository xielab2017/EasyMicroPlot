#' @import ggplot2
#' @import grid
#' @importFrom plyr alply
#' @importFrom dplyr inner_join
#' @importFrom fs file_move
#' @import stringr
#' @importFrom ggiraph girafe
#' @importFrom htmlwidgets saveWidget
#' @importFrom ggpubr stat_compare_means
#' @importFrom vegan diversity
#' @importFrom vegan estimateR
#' @importFrom ggiraph girafe
#' @importFrom ggiraph geom_jitter_interactive
#' @importFrom purrr reduce
#' @importFrom reshape2 melt


options(dplyr.summarise.inform = FALSE)

alpha_caculate <- function(x, tree = NULL, base = exp(1)) {
  est <- vegan::estimateR(x)
  Richness <- est[1, ]
  #Chao1 <- est[2, ]
  Shannon <- vegan::diversity(x, index = 'shannon', base = base)
  Simpson <- vegan::diversity(x, index = 'simpson')    #Gini-Simpson 指数
  InvSimpson <- vegan::diversity(x, index = 'invsimpson')
  Pielou <- Shannon/log(Richness)
  result <- data.frame(Pielou, Shannon, Simpson,InvSimpson)
  return(result)
}

alpha_plot_ttest <- function(data,design,seed=123,width=10,height=10,row_panel=NULL,group_level=c('default'),mytheme=theme(),palette=c("#E64B35FF","#4DBBD5FF","#00A087FF","#3C5488FF","#F39B7FFF","#8491B4FF",
          "#B2182B","#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7","#CC6666")){
  data=data
  mapping=design
  try(mapping<-read.table(paste0(design),header = T),silent = T)
  mapping <- subset(mapping,select = c(SampleID,Group))
  # 检查输入的group 新顺序是否符合要求，来决定是否使用默认字符串顺序
  name_group=unique(mapping$Group)
  group_level_check=all(name_group%in%group_level)&all(group_level%in%name_group)
  if (group_level_check==T) {
    mapping$Group<-factor(mapping$Group,levels = group_level)
  }else{
    if (group_level[1] != 'default') {
      warning('group level can not match, and pipe will follow the default level !')
    }
  }
  real_sample=Reduce(intersect,list(mapping$SampleID,rownames(data)))
  mapping=mapping[mapping$SampleID%in%real_sample,]
  data=data[rownames(data)%in%real_sample,]
  data$SampleID=rownames(data)
  data=dplyr::inner_join(mapping,data,by='SampleID')
  rownames(data) <- data$SampleID

  # 预先保留构建分面表的总数据
  data_combie <- data
  try(data_combie <- subset(data,select = -c(barcode,primer,Description)),silent=T)
  data_combie <- reshape2::melt(data_combie,id.vars = c('SampleID','Group'))

  try(data<-subset(data,select = -c(SampleID,barcode,primer,Description)),silent=T)
  group_name=as.character(unique(data$Group))
  group_combn=combn(group_name,2)
  compare=plyr::alply(group_combn,2)
  

  Pielou <- ggplot(data=data,aes(x=Group,y=data[,'Pielou'] ))+geom_boxplot(aes(fill=Group),colour="black",notch=F,outlier.colour=NA)+
              ylab("Pielou's evenness")+ggpubr::stat_compare_means(comparisons = compare,method="t.test")+scale_fill_manual(values=palette)+
              ggiraph::geom_jitter_interactive(aes(tooltip = paste0(rownames(data),' : ',round(Pielou,2))),position = position_jitter(height = .000001))+theme_bw() + mytheme
  set.seed(seed)
  Pielou_html=ggiraph::girafe(code = print(Pielou),width_svg = width,height_svg = height)


  Shannon <- ggplot(data=data,aes(x=Group,y=data[,'Shannon'] ))+geom_boxplot(aes(fill=Group),colour="black",notch=F,outlier.colour=NA)+
              ylab('Shannon')+ggpubr::stat_compare_means(comparisons = compare,method="t.test")+scale_fill_manual(values=palette)+
              ggiraph::geom_jitter_interactive(aes(tooltip = paste0(rownames(data),' : ',round(Shannon,2))),position = position_jitter(height = .000001))+theme_bw() + mytheme
  set.seed(seed)
  Shannon_html=ggiraph::girafe(code = print(Shannon),width_svg = width,height_svg = height)

  Simpson <- ggplot(data=data,aes(x=Group,y=data[,'Simpson'] ))+geom_boxplot(aes(fill=Group),colour="black",notch=F,outlier.colour=NA)+
              ylab('Simpson')+ggpubr::stat_compare_means(comparisons = compare,method="t.test")+scale_fill_manual(values=palette)+
              ggiraph::geom_jitter_interactive(aes(tooltip = paste0(rownames(data),' : ',round(Simpson,2))),position = position_jitter(height = .000001))+theme_bw() + mytheme
  set.seed(seed)
  Simpson_html=ggiraph::girafe(code = print(Simpson),width_svg = width,height_svg = height)

  InvSimpson <- ggplot(data=data,aes(x=Group,y=data[,'InvSimpson'] ))+geom_boxplot(aes(fill=Group),colour="black",notch=F,outlier.colour=NA)+
              ylab('InvSimpson')+ggpubr::stat_compare_means(comparisons = compare,method="t.test")+scale_fill_manual(values=palette)+
              ggiraph::geom_jitter_interactive(aes(tooltip = paste0(rownames(data),' : ',round(InvSimpson,2))),position = position_jitter(height = .000001))+theme_bw() + mytheme
  set.seed(seed)
  InvSimpson_html=ggiraph::girafe(code = print(InvSimpson),width_svg = width,height_svg = height)

  alpha_pic_total <- ggplot(data_combie, aes(x=Group,y=value ,fill = Group)) +
                      geom_boxplot(aes(fill=Group),colour="black",notch=F,outlier.colour=NA) +
                      facet_wrap(~variable, scales = 'free_y', ncol = row_panel) +
                      ggpubr::stat_compare_means(comparisons = compare,method="t.test")+
                      scale_fill_manual(values =palette) +
                      labs(x = '', y = 'Alpha diversity')+theme_bw() + mytheme +
                      ggiraph::geom_jitter_interactive(aes(tooltip = paste0(SampleID,' : ',round(value,2))),position = position_jitter(height = .000001))
  set.seed(seed)
  alpha_pic_total_html <- ggiraph::girafe(code = print(alpha_pic_total),width_svg = width,height_svg = height)
 
  deposit=list()
  deposit$pic$Pielou=Pielou
  deposit$pic$Shannon=Shannon
  deposit$pic$Simpson=Simpson
  deposit$pic$InvSimpson=InvSimpson
  deposit$pic$Total=alpha_pic_total
  deposit$html$Pielou=Pielou_html
  deposit$html$Shannon=Shannon_html
  deposit$html$Simpson=Simpson_html
  deposit$html$InvSimpson=InvSimpson_html
  deposit$html$Total=alpha_pic_total_html
  return(deposit)
}



alpha_plot_mtest <- function(data,design,seed=123,width=10,height=10,row_panel=NULL,method='LSD',group_level=c('default'),mytheme=theme(),palette=c("#E64B35FF","#4DBBD5FF","#00A087FF","#3C5488FF","#F39B7FFF","#8491B4FF",
          "#B2182B","#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7","#CC6666")){
  data <- data
  mapping <- design
  try(mapping <- read.table(paste0(design),header = T),silent = T)
  mapping <- subset(mapping,select=c(SampleID,Group))
  # 检查输入的group 新顺序是否符合要求，来决定是否使用默认字符串顺序
  name_group=unique(mapping$Group)
  group_level_check=all(name_group%in%group_level)&all(group_level%in%name_group)
  if (group_level_check==T) {
    mapping$Group<-factor(mapping$Group,levels = group_level)
  }else{
    if (group_level[1] != 'default') {
      warning('group level can not match, and pipe will follow the default level !')
    }
  }
  real_sample <- Reduce(intersect,list(mapping$SampleID,rownames(data)))
  mapping <- mapping[mapping$SampleID%in%real_sample,]
  data=data[rownames(data)%in%real_sample,]
  data$SampleID <- rownames(data)
  data <- dplyr::inner_join(mapping,data,by='SampleID')
  rownames(data) <- data$SampleID

  # 预先保留构建分面表的总数据
  data_combie <- data
  data_combie <- reshape2::melt(data_combie,id.vars = c('SampleID','Group'))


  data <- subset(data,select = -c(SampleID))
  # 得到实际特征名称
  feature_name <- colnames(data)[-1]
  # 分别检验各类指数
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
  
  
  # 合并列表中的字母标记
  alpha_result_letter <- fit_test_list %>% purrr::reduce(dplyr::full_join, by ="Group")
  
  # 主要这里使用关闭警告再打开，避免输出过程中产生不必要的警告提示
  suppressWarnings(alpha_result_letter_for_total <- reshape2::melt(alpha_result_letter,id.vars = c('Group')))

  
  # 计算出各个分面的极值，方便字母标记的定位
  ymax=list()
  for (i in feature_name) {
    ymax[[i]]=max(data[[i]])
  }

  for (i in feature_name) {
  id <- with(alpha_result_letter_for_total, variable==i)
  alpha_result_letter_for_total$ymax[id]=ymax[[i]]
  }

  Pielou <- ggplot(data,aes(Group,Pielou)) +
    geom_boxplot(aes(fill = Group),outlier.colour = NA) +scale_fill_manual(values=palette)+
    geom_text(data = alpha_result_letter,aes(x = Group,y=max(data['Pielou'])*1.1,label = Pielou),
              size = 7,color = "black")+theme_bw() + mytheme+
    ggiraph::geom_jitter_interactive(aes(tooltip = paste0(rownames(data),' : ',round(Pielou,2))),position = position_jitter(height = .000001))
  set.seed(seed)
  Pielou_html=ggiraph::girafe(code = print(Pielou),width_svg = width,height_svg = height)
  
  Shannon <- ggplot(data,aes(Group,Shannon)) +
    geom_boxplot(aes(fill = Group),outlier.colour = NA) +scale_fill_manual(values=palette)+
    geom_text(data = alpha_result_letter,aes(x = Group,y=max(data['Shannon'])*1.1,label = Shannon),
              size = 7,color = "black")+theme_bw() + mytheme+
    ggiraph::geom_jitter_interactive(aes(tooltip = paste0(rownames(data),' : ',round(Shannon,2))),position = position_jitter(height = .000001))
  set.seed(seed)
  Shannon_html=ggiraph::girafe(code = print(Shannon),width_svg = width,height_svg = height)
  
  Simpson <- ggplot(data,aes(Group,Simpson)) +
    geom_boxplot(aes(fill = Group),outlier.colour = NA) +scale_fill_manual(values=palette)+
    geom_text(data = alpha_result_letter,aes(x = Group,y=max(data['Simpson'])*1.1,label = Simpson),
              size = 7,color = "black")+theme_bw() + mytheme+
    ggiraph::geom_jitter_interactive(aes(tooltip = paste0(rownames(data),' : ',round(Simpson,2))),position = position_jitter(height = .000001))
  set.seed(seed)
  Simpson_html=ggiraph::girafe(code = print(Simpson),width_svg = width,height_svg = height)
  
  InvSimpson <- ggplot(data,aes(Group,InvSimpson)) +
    geom_boxplot(aes(fill = Group),outlier.colour = NA) +scale_fill_manual(values=palette)+
    geom_text(data = alpha_result_letter,aes(x = Group,y=max(data['InvSimpson'])*1.1,label = InvSimpson),
              size = 7,color = "black")+theme_bw() + mytheme+
    ggiraph::geom_jitter_interactive(aes(tooltip = paste0(rownames(data),' : ',round(InvSimpson,2))),position = position_jitter(height = .000001))
  set.seed(seed)
  InvSimpson_html=ggiraph::girafe(code = print(InvSimpson),width_svg = width,height_svg = height)
  
  alpha_pic_total <- ggplot(data_combie, aes(x=Group,y=value ,fill = Group)) +
                      geom_boxplot(aes(fill=Group),colour="black",notch=F,outlier.colour=NA) +
                      geom_text(data = alpha_result_letter_for_total,aes(x = Group,y=ymax*1.1,label = value),size = 7,color = "black")+
                      facet_wrap(~variable, scales = 'free_y', ncol = row_panel) +
                      scale_fill_manual(values =palette) +
                      labs(x = '', y = 'Alpha diversity') + theme_bw() + mytheme+ 
                      ggiraph::geom_jitter_interactive(aes(tooltip = paste0(SampleID,' : ',round(value,2))),position = position_jitter(height = .000001))

  set.seed(seed)
  alpha_pic_total_html=ggiraph::girafe(code = print(alpha_pic_total),width_svg = width,height_svg = height)


  deposit=list()
  deposit$pic$Pielou=Pielou
  deposit$pic$Shannon=Shannon
  deposit$pic$Simpson=Simpson
  deposit$pic$InvSimpson=InvSimpson
  deposit$pic$Total=alpha_pic_total
  deposit$html$Pielou=Pielou_html
  deposit$html$Shannon=Shannon_html
  deposit$html$Simpson=Simpson_html
  deposit$html$InvSimpson=InvSimpson_html
  deposit$html$Total=alpha_pic_total_html
  deposit$test$ANOVA=fit_model_info
  deposit$test$comparison=fit_test_info
  return(deposit)  
}

alpha_plot=function(data=NULL,design,dir=NULL,group_level=c('default'),seed=123,mytheme=theme(),min_relative = 0,min_ratio = 0,pattern = '',output = F,html_out = F,change=F,change_name='Other',
                    method='HSD',width=10,height=10,row_panel=NULL,palette=c("#E64B35FF","#4DBBD5FF","#00A087FF","#3C5488FF","#F39B7FFF","#8491B4FF",
                                                              "#B2182B","#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7","#CC6666") ){
  deposit=list()
  
  # 注意这里adjust必须为关闭，即出现空行的样本alpha核心物种多样性为0
  deposit$result=data_filter(dir = dir,data = data,min_relative = min_relative,min_ratio = min_ratio,design = design,adjust = F,pattern = pattern,output = output,change=change,change_name=change_name)
  # 提取输入文件数据，注意排除ID文件
  input_file=grep("ID$",names(deposit$result$filter_data),value=T,invert=T)
  for (i in input_file){
    data=data.frame()
    data<-subset(deposit$result$filter_data[[i]],select=-c(Group))
    rownames(data)<-data[,1]
    data<-data[,-1]
    data=round(data*1000000,0)
    alpha_result=alpha_caculate(data)
    if (method == 'ttest') {
      alpha_pic=alpha_plot_ttest(data =alpha_result,design = design,mytheme=mytheme,width = width,height = height,row_panel=row_panel,group_level=group_level,seed = seed,palette = palette)
    }else{
      alpha_pic=alpha_plot_mtest(data =alpha_result,design = design,mytheme=mytheme,width = width,height = height,row_panel=row_panel,group_level=group_level,seed = seed,method = method,palette = palette)
    }
    
    deposit$result$alpha_result[[i]]<-alpha_result
    deposit$plot[[i]]<-alpha_pic
    if (html_out==T) {
      htmlwidgets::saveWidget(deposit$plot[[i]]$html$Pielou, paste0(i,'_',min_relative,'_',min_ratio,'_',method,'_','Pielou.html'))
      htmlwidgets::saveWidget(deposit$plot[[i]]$html$Shannon, paste0(i,'_',min_relative,'_',min_ratio,'_',method,'_','Shannon.html'))
      htmlwidgets::saveWidget(deposit$plot[[i]]$html$Simpson, paste0(i,'_',min_relative,'_',min_ratio,'_',method,'_','Simpson.html'))
      htmlwidgets::saveWidget(deposit$plot[[i]]$html$InvSimpson, paste0(i,'_',min_relative,'_',min_ratio,'_',method,'_','InvSimpson.html'))
      htmlwidgets::saveWidget(deposit$plot[[i]]$html$Total, paste0(i,'_',min_relative,'_',min_ratio,'_',method,'_','Total.html'))
      
    }
    
  }
  return(deposit)
}



























