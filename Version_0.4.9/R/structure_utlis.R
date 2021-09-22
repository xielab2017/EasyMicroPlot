#' @import ggplot2
#' @importFrom reshape2 melt


top_abundance_caculate <- function(data,structure_method = 'mean',num = 10,estimate_group='default'){
  if (estimate_group== 'default') {
    estimate_group=unique(data$Group)
  }
  
  if (!all( estimate_group %in%  data$Group )) {
    warning('estimate_group is wrong, please check !')
  }else{
    data_abundace <- data[data$Group %in% estimate_group, ]
    feature <- ncol(subset(data_abundace,select = -c(SampleID,Group)))
    if (num < feature & 0 < num) {
      if (structure_method=='median') {
        estimate_index=apply(subset(data_abundace,select = -c(SampleID,Group)),2,median)
      }else if (structure_method=='mean') {
        estimate_index=apply(subset(data_abundace,select = -c(SampleID,Group)),2,mean)
      }else if (structure_method=='max'){
        estimate_index=apply(subset(data_abundace,select = -c(SampleID,Group)),2,max)
      }else if (structure_method=='min'){
        estimate_index=apply(subset(data_abundace,select = -c(SampleID,Group)),2,min)
      }else{
        warning('structure_method is wrong, please check !')
      }
      id <- names(sort(estimate_index,decreasing = T)[1:num])
      data_select <- subset(data,select = -c(SampleID,Group))[id]
      data_select$Others <- 1-apply(data_select,1,sum)
      data_select_combie <- data.frame(data[,c('Group','SampleID')],data_select)
      return(data_select_combie)
    }else if ( num >= feature) {
      data_select_combie <- data_abundace
      return(data_select_combie)
    }else{
      warning('num is wrong, please check !')
    }
  } 
}    

structure_top_bar <- function(data,group_level='default',tax_level='default',mytheme=theme(),measure='default',row_panel=NULL,
                              palette=c("#E64B35FF","#4DBBD5FF","#00A087FF","#3C5488FF","#F39B7FFF","#8491B4FF",
                                        "#B2182B","#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7","#CC6666")){
  structure_data <- data
  structure_data_reshape <- reshape2::melt(structure_data,id.vars = c('Group','SampleID'))
  # 检查输入的group_level 新顺序是否符合要求，来决定是否使用默认字符串顺序
  name_group <- unique(structure_data_reshape$Group)
  group_level_check <- all(name_group%in%group_level)&all(group_level%in%name_group)
  if (group_level_check==T) {
    structure_data_reshape$Group<-factor(structure_data_reshape$Group,levels = group_level)
  }else{
    if (group_level[1] != 'default') {
      warning('group level can not match, and pipe will follow the default level !')
    }
  }
  # 检查输入的tax_level 新顺序是否符合要求，来决定是否使用默认字符串顺序
  tax_group=unique(structure_data_reshape$variable)
  tax_level_check <- all(tax_group%in%tax_level)&all(tax_level%in%tax_group)
  if (tax_level_check==T) {
    structure_data_reshape$variable<-factor(structure_data_reshape$variable,levels = tax_level)
  }else{
    if (tax_level[1] != 'default') {
      warning('group level can not match, and pipe will follow the default level !')
    }
  }  
  # 判断色版是否满足tax总数，如不够则需使用彩虹色
  tax_num <- ncol(subset(structure_data,select=-c(Group,SampleID)))
  if (tax_num <= length(palette)) {
    cols <- palette
  }else{
    cols<-rainbow(tax_num)
    warning('palette is less than tax num and ranbow color is used in stead.')
  }
  
  # 判断measure是否满足
  measure_check <- measure[1] %in% tax_group
  if (measure_check==T) {
    measure <- measure
  }else{
    if (measure != 'default') {
      warning(paste0(measure ,' is used as measure paramter'))
    }
    measure <- unique(structure_data_reshape$variable)[1]
    
  }

  #存储
  deposit <- list()
  # 分组图
  for (i in unique(name_group)) {
	  structure_data_reshape_sub=structure_data_reshape[structure_data_reshape$Group==i,]
	  deposit$barplot[[i]] <- ggplot(structure_data_reshape_sub, aes(x=reorder(SampleID, as.numeric(variable==measure)*value),y=100 * value, fill = variable))+  geom_col(position = 'fill', width = 0.8) +
	    scale_fill_manual(values =cols) +
	    labs(x = 'control', y = 'Relative Abundance(%)') +
	    theme(panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'transparent'), strip.text = element_text(size = 12)) +
	    theme(axis.text = element_text(size = 5), axis.title = element_text(size = 13), legend.title = element_blank(), legend.text = element_text(size = 11)) + mytheme
  }
  # 全部组图
  str_group_plot <- ggplot(structure_data_reshape, aes(x=reorder(SampleID, as.numeric(variable==measure)*value),y=100 * value, fill = variable))+  geom_col(position = 'fill', width = 0.8) +
    facet_wrap(~Group, scales = 'free_x', ncol = row_panel) +
    scale_fill_manual(values =cols) +
    labs(x = '', y = 'Relative Abundance(%)') +
    theme(panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'transparent'), strip.text = element_text(size = 12)) +
    theme(axis.text = element_text(size = 5), axis.title = element_text(size = 13), legend.title = element_blank(), legend.text = element_text(size = 11)) + mytheme

  deposit$barplot$Total <- str_group_plot
  return(deposit)
} 


structure_plot <- function(dir,group_level='default',tax_level='default',measure='default',mytheme=theme(),min_relative = 0,min_ratio = 0,design ,pattern = '',output = F,
                           structure_method='mean',num = 10,estimate_group='default',width=10,height=10,row_panel=NULL,
                           palette=c("#E64B35FF","#4DBBD5FF","#00A087FF","#3C5488FF","#F39B7FFF","#8491B4FF","#B2182B","#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7","#CC6666")){
  
  deposit=list()
  deposit$result=data_filter(dir = dir,min_relative = min_relative,min_ratio = min_ratio,design = design,adjust = F,pattern = pattern,output = output)
  # 提取输入文件数据，注意排除ID文件
  input_file=grep("ID$",names(deposit$result$filter_data),value=T,invert=T)
  for (i in input_file){
    structure_data <- top_abundance_caculate(data = deposit$result$filter_data[[i]],structure_method = structure_method,num = num,estimate_group=estimate_group)
    deposit$result$top_abundance[[i]] <- structure_data
    deposit$pic[[i]] <- structure_top_bar(data=structure_data,group_level=group_level,tax_level=tax_level,mytheme=mytheme,measure=measure,row_panel=row_panel,palette=palette)
  }  
  return(deposit)
}


