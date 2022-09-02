EMP_MICRO <- function(data=NULL,design='mapping.txt',dir='.',min_relative=0.001,min_ratio=0.7,html_out=T,method='LSD',beta_method='LSD',distance=c('bray','jaccard'),seed=123,pattern='',group_level='default',top_num=10,cooc_r=0.3,cooc_p=0.05,width=10,
                      height=10,RFCV_estimate='species',x_break=1,vertex.size=15,vertex.label.cex=0.5,set_color_level='phylum',ntree=1000,kfold=5,rep=10,RF_importance=1,step=1,cutoff_colour='red',change=F,change_name='Other',tax_level='default',edge_color_positive='darkred',edge_color_negitive='steelblue',edge.width=2,
                      output_folder='Result/',palette=c("#E64B35FF","#4DBBD5FF","#00A087FF","#3C5488FF","#F39B7FFF","#8491B4FF",
                                        "#B2182B","#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7","#CC6666")){


cat('
--------------------------------------------\n
         EMP_MICRO WORKFLOW START\n
--------------------------------------------\n\n',
'Main Paramters: \n\n',
'Working space :',getwd(),'\n',
'Output folder :',output_folder,'\n',
'File searching pattern :',pattern,'\n',
'Group order for plots :',group_level,'\n',
'Width :',width,'\tHeight :',height,'\n',
'Min relative :',min_relative,'\tMin ratio :',min_ratio,'\n',
'Multiple comparisons :',method,'\n',
'Multiple comparisons for beta diversity :',beta_method,'\n',
'Distance for beta diversity :',distance,'\n',
'Top abundance in structure plot:',top_num,'\n',
'Min correlation index :', cooc_r,'\tP value for correlation :',cooc_p,'\n',
'Positive edge :', edge_color_positive,'\tNegitive edge :',edge_color_negitive,'\n',
'Vertex size :', vertex.size,'\tVertex color :',set_color_level ,'\tLabel size in vertex :',vertex.label.cex,'\n',
'Random numbers :',seed,'\tTaxonomy for RFCV :',RFCV_estimate,'\n'
) 

cat('
--------------------------------------------\n
         ALPHA  ANALYSIS START\n
--------------------------------------------\n\n')  
 alpha_check<-tryCatch({
  ## alpha 专区
  core_data_dic_name <- paste0(output_folder,'/core_data')
  
  alpha_dir_name <- paste0(output_folder,'/alpha_result')
  alpha_dir_pic <- paste0(alpha_dir_name,'/pic')
  alpha_dir_html <- paste0(alpha_dir_name,'/html')
  alpha_dir_test <- paste0(alpha_dir_name,'/Post-Hoc')
  alpha_dir_data <- paste0(alpha_dir_name,'/data')
  
  try(dir.create(core_data_dic_name,recursive = T),silent = T)
  try(dir.create(alpha_dir_pic,recursive = T),silent = T)
  try(dir.create(alpha_dir_html,recursive = T),silent = T)
  try(dir.create(alpha_dir_data,recursive = T),silent = T)
  if (method != 'ttest') {
    try(dir.create(alpha_dir_test,recursive = T),silent = T)
  }
  alpha_result <- alpha_plot(dir = dir,data = data,seed = seed,min_relative = min_relative,min_ratio=min_ratio,palette=palette,
                             method = method,design = design,pattern = pattern,html_out = html_out,output = F,group_level=group_level,change=change,change_name=change_name)
  level_name <- names(alpha_result$plot)
  alpha_index <- names(alpha_result$result$alpha_result[[1]])
  for (k in level_name) {
    set.seed(seed)
    ggplot2::ggsave(paste0(alpha_dir_pic,'/',k,'_alpha.pdf'),alpha_result$plot[[k]]$pic$Total,width = width,height = height)
    suppressMessages(filesstrings::file.move(list.files(path ='.' ,pattern = 'html'),alpha_dir_html))
    sink(paste0(alpha_dir_data,'/',k,'_alpha_value.txt'))
    print(alpha_result$result$alpha_result[[k]])
    sink()
    if (method != 'ttest') {
    sink(paste0(alpha_dir_test,'/',k,'_Post-Hoc.txt'))
    print(alpha_result$plot[[k]]$test)
    sink()
    }
    # core data deposit
    write.table(file=paste0(core_data_dic_name,'/',k,'_',min_relative,'_',min_ratio*100,'%.txt'),alpha_result$result$filter_data[[k]],sep = '\t',quote = F,row.names = F)
    write.table(file=paste0(core_data_dic_name,'/',k,'_',min_relative,'_',min_ratio*100,'%_info.txt'),alpha_result$result$filter_data[[paste0(k,'_ID')]],sep = '\t',quote = F,row.names = F) 
  }
 cat('
--------------------------------------------\n
         ALPHA  ANALYSIS COMPLETE\n
--------------------------------------------')  
 },error=function(e){message("Something is wrong :\n",conditionMessage(e),"\n")
 cat('
--------------------------------------------\n
         ALPHA  ANALYSIS Fail\n
--------------------------------------------')  
   },
 warning=function(a){message("Something is warning :\n",conditionMessage(a),"\n")
cat('
--------------------------------------------\n
         ALPHA  ANALYSIS Fail\n
--------------------------------------------') })


  cat('
--------------------------------------------\n
         BETA ANALYSIS START\n
--------------------------------------------\n\n')
  
beta_check<-tryCatch({
  # beta运行区
  beta_dir_name <- paste0(output_folder,'/beta_result')
  beta_dir_pic <- paste0(beta_dir_name,'/pic')
  beta_dir_html <- paste0(beta_dir_name,'/html')
  beta_dir_test <- paste0(beta_dir_name,'/Post-Hoc')
  
  
  for (i in distance) {
    beta_dir_name=paste0(output_folder,'/beta_result/',i,'_',min_relative,'_',min_ratio,'/')
    beta_dir_pic <- paste0(beta_dir_name,'/pic')
    beta_dir_html <- paste0(beta_dir_name,'/html')
    beta_dir_test <- paste0(beta_dir_name,'/Post-Hoc')
    try(dir.create(beta_dir_pic,recursive = T),silent = T)
    try(dir.create(beta_dir_html,recursive = T),silent = T)
    try(dir.create(beta_dir_test,recursive = T),silent = T)
    
    beta_result <- beta_plot(dir = dir,data = data,min_relative = min_relative,min_ratio = min_ratio,
                             design = design,adjust = T,pattern = pattern,group_level=group_level,change=change,change_name=change_name,
                             html_out = html_out,output=F,seed = seed,method = beta_method,distance = i,palette=palette)
    level_name <- names(beta_result$plot)
    for (j in level_name) {
      set.seed(seed)
      ggplot2::ggsave(paste0(beta_dir_pic,'/',j,'_p12.pdf'),beta_result$plot[[j]]$pic$p12,width = 1.5*width,height = 1.5*height)
      suppressMessages(filesstrings::file.move(paste0(j,'_',min_relative,'_',min_ratio,'_',i,'_',beta_method,'_p1-2.html'), beta_dir_html))
      set.seed(seed)
      ggplot2::ggsave(paste0(beta_dir_pic,'/',j,'_p23.pdf'),beta_result$plot[[j]]$pic$p23,width = 1.5*width,height = 1.5*height)
      suppressMessages(filesstrings::file.move(paste0(j,'_',min_relative,'_',min_ratio,'_',i,'_',beta_method,'_p2-3.html'), beta_dir_html))
      set.seed(seed)
      ggplot2::ggsave(paste0(beta_dir_pic,'/',j,'_p13.pdf'),beta_result$plot[[j]]$pic$p13,width = 1.5*width,height = 1.5*height)
      suppressMessages(filesstrings::file.move(paste0(j,'_',min_relative,'_',min_ratio,'_',i,'_',beta_method,'_p1-3.html'), beta_dir_html))
      sink(paste0(beta_dir_test,'/',j,'_Post-Hoc.txt'))
      print(beta_result$plot[[j]]$test)
      sink()
    }
  }
cat('
--------------------------------------------\n
         BETA  ANALYSIS COMPLETE\n
--------------------------------------------')   
  } ,error=function(e){message("Something is wrong :\n",conditionMessage(e),"\n")
cat('
--------------------------------------------\n
         BETA  ANALYSIS Fail\n
--------------------------------------------')},
 warning=function(a){message("Something is warning :\n",conditionMessage(a),"\n")
cat('
--------------------------------------------\n
         BETA  ANALYSIS Fail\n
--------------------------------------------') })



 

cat('
--------------------------------------------\n
         STRUCTURE ANALYSIS START\n
--------------------------------------------\n\n')  
  
 structure_check<-tryCatch({ 
  ## 结构图
  str_dir_name <- paste0(output_folder,'/structure_result/')
  str_dir_pic <- paste0(str_dir_name,'/pic/')
  str_dir_info <- paste0(str_dir_name,'/taxonomy/')
  str_dir_top <- paste0(str_dir_name,'/top_abundance/')
  
  try(dir.create(str_dir_pic,recursive = T),silent = T)
  try(dir.create(str_dir_info,recursive = T),silent = T)
  try(dir.create(str_dir_top,recursive = T),silent = T)
  barplot_result <- structure_plot(dir = dir,data = data,min_relative = min_relative,min_ratio = min_ratio,tax_level=tax_level,
                                   design = design,num = top_num,pattern = pattern,change=change,change_name=change_name,palette=palette,
                                   output = F,group_level=group_level)
  level_name <- names(barplot_result$pic)
  for (k in level_name) {
    ggplot2::ggsave(paste0(str_dir_pic,k,'_barplot.pdf'),barplot_result$pic[[k]]$barplot$Total,width = 1.5*width,height = 1.5*height)
    write.table(file = paste0(str_dir_info,k,'_taxonomy_info.txt'),barplot_result$result$filter_data[[paste0(k,'_ID')]],sep = '\t',quote = F,row.names = F)
    write.table(file = paste0(str_dir_top,k,'_TOP_',top_num,'.txt'),barplot_result$result$top_abundance[[k]],sep = '\t',quote = F,row.names = F)
    
  }
cat('
--------------------------------------------\n
         STRUCTURE  ANALYSIS COMPLETE\n
--------------------------------------------')   
  },error=function(e){message("Something is wrong :\n",conditionMessage(e),"\n")
cat('
--------------------------------------------\n
         STRUCTURE  ANALYSIS Fail\n
--------------------------------------------')},
 warning=function(a){message("Something is warning :\n",conditionMessage(a),"\n")
cat('
--------------------------------------------\n
         STRUCTURE  ANALYSIS Fail\n
--------------------------------------------') })
   



cat('
--------------------------------------------\n
         COOC ANALYSIS START\n
--------------------------------------------\n\n')  
cooc_check<-tryCatch({   
  ### 共发生网络图
  cooc_result=cooc_plot(dir = dir,data = data,min_relative = min_relative,min_ratio = min_ratio,edge.width=edge.width,width=width,height=height,
                        cooc_output = T,design = design,pattern = pattern,cooc_r = cooc_r,cooc_p =cooc_p,edge_color_positive=edge_color_positive,edge_color_negitive=edge_color_negitive,
                        vertex.size = vertex.size,vertex.label.cex = vertex.label.cex,set_color_level = set_color_level,change=change,change_name=change_name,cooc_output_dir=output_folder)
  cooc_dir_info <- paste0(output_folder,'/cooc_result/cooc_info')
  cooc_dir_net_profile <- paste0(output_folder,'/cooc_result/net_profile')
  
  try(dir.create(cooc_dir_info,recursive = T),silent = T)
  try(dir.create(cooc_dir_net_profile,recursive = T),silent = T)
  
  level_name <- names(cooc_result$cooc_profile)
  group_name <- names(cooc_result$plot)
  for (n in level_name) {
    for (p in group_name) {
      sink(paste0(cooc_dir_info,'/',n,'_',p,'_cor_r.txt'))
      print(cooc_result$plot[[p]][[n]]$cor_result$cor_r)
      sink()
      sink(paste0(cooc_dir_info,'/',n,'_',p,'_cor_p.txt'))
      print(cooc_result$plot[[p]][[n]]$cor_result$cor_p)
      sink()
    }
    sink(paste0(cooc_dir_net_profile,'/',n,'_','cooc_profie.txt'))
    print(cooc_result$cooc_profile[[n]])
    sink()
  }
cat('
--------------------------------------------\n
         COOC  ANALYSIS COMPLETE\n
--------------------------------------------')    
  },error=function(e){message("Something is wrong :\n",conditionMessage(e),"\n")
cat('
--------------------------------------------\n
         COOC  ANALYSIS Fail\n
------------------------------------')},
 warning=function(a){message("Something is warning :\n",conditionMessage(a),"\n")
cat('
--------------------------------------------\n
         COOC  ANALYSIS Fail\n
--------------------------------------------')  })
 



cat('
--------------------------------------------\n
         RFCV ANALYSIS START\n
--------------------------------------------\n\n')   
RFCV_check<-tryCatch({   
  ## RFCV
  RFCV_dir_name <- paste0(output_folder,'/RFCV_result/')
  RFCV_dir_importance <- paste0(output_folder,'/RFCV_result/Imprortance')
  RFCV_dir_tax <- paste0(RFCV_dir_name,'/taxonomy')
  RFCV_dir_model <- paste0(RFCV_dir_name,'/model')
  RFCV_dir_pic <- paste0(RFCV_dir_name,'/taxonomy/pic')
  RFCV_dir_html <- paste0(RFCV_dir_name,'/taxonomy/html')
  RFCV_dir_test <- paste0(RFCV_dir_name,'/taxonomy/Post-Hoc')
  
  
  try(dir.create(RFCV_dir_pic,recursive = T),silent = T)
  try(dir.create(RFCV_dir_html,recursive = T),silent = T)
  try(dir.create(RFCV_dir_importance,recursive = T),silent = T)
  try(dir.create(RFCV_dir_model,recursive = T),silent = T)
  
  
  
  core_sp <- data_filter(dir = dir,data = data,min_relative = min_relative,min_ratio=min_ratio,
                             design = design,pattern = pattern,output = F)
  sp <- core_sp$filter_data[[RFCV_estimate]]
  write.table(file = paste0(RFCV_dir_tax,'/taxonomy_id.txt'),core_sp$filter_data[[paste0(RFCV_estimate,'_ID')]],row.names = F,sep = '\t')
  sp$Group <- factor(sp$Group)
  rf <- RFCV(sp,each_ouput = T,ntree=ntree,kfold=kfold,rep=rep,RF_importance=RF_importance,step=step,cutoff_colour=cutoff_colour,seed_start=seed,x_break=x_break,palette=palette)
  suppressMessages(filesstrings::file.move(list.files(path ='.' ,pattern = 'varimplot.pdf'),RFCV_dir_importance))


  RF_var_dataframe <- data.frame(matrix(unlist(rf$RFCV_result$CV_accuracy),nrow = c(ncol(sp)-2)))
  seed_name=rf$RFCV_result$Seed_num
  colnames(RF_var_dataframe)=paste( "Seed_", seed_name, sep="")
  write.table(RF_var_dataframe,file = paste0(RFCV_dir_model,"/RF_accuracy.txt"),row.names = F,sep = '\t',quote=F)

  ggplot2::ggsave(paste0(RFCV_dir_model,'/RFCV_curve.pdf'),rf$RFCV_result_plot$curve_plot,width = 2*height,height = height)
  
  # 考虑到用户的组名有时过长，这里默认使用斜45度主题
  suppressWarnings(library(ggplot2))
  newtheme_slope=theme(axis.text.x =element_text(angle = 45, hjust = 1,size = 10))

  try(taxa_intersect <- tax_plot(data = sp,tax_select =rf$RFCV_result_plot$intersect_num ,method=method,html_out = F,group_level=group_level,mytheme =newtheme_slope),silent = T)
  taxa_union <- tax_plot(data = sp,tax_select =rf$RFCV_result_plot$union_num ,method=method,html_out = T,group_level=group_level,mytheme =newtheme_slope)
  suppressMessages(filesstrings::file.move(list.files(path ='.' ,pattern = 'boxplot.html'),RFCV_dir_html))
  if (!is.null(taxa_union$Post_Hoc)) {
    dir.create(RFCV_dir_test,recursive = T)
    sink(paste0(RFCV_dir_test,'/Post-Hoc.txt'))
    print(taxa_union$Post_Hoc)
    sink()
  }
  
  # 这里考虑过多分组，导致用户出图展示不佳，进一步调整
  if(length(unique(sp$Group)) > 4){
    width = 2*width
  }
  # 这里关闭警告提示，防止用户交集为空，导致总控停顿报错在这里
  suppressWarnings(try(ggplot2::ggsave(paste0(RFCV_dir_pic,'/RFCV_intersect.pdf'),taxa_intersect$pic$total,width = width,height = height),silent = T))
  ggplot2::ggsave(paste0(RFCV_dir_pic,'/RFCV_union.pdf'),taxa_union$pic$total,width = width,height = height)
  
  if (  length(unique(sp$Group)) == 2  ) {
    pdf(paste0(RFCV_dir_name,'ROC_intersecet.pdf'))
    suppressWarnings(try(RFCV_roc(rf,rf_tax_select='intersect',rf_estimate_group = unique(sp$Group)[1]),silent = T))
    dev.off()
    pdf(paste0(RFCV_dir_name,'ROC_union.pdf'))
    RFCV_roc(rf,rf_tax_select='union',rf_estimate_group = unique(sp$Group)[1])
    dev.off()
  }
cat('
--------------------------------------------\n
         RFCV  ANALYSIS COMPLETE\n
--------------------------------------------')    
  },error=function(e){message("Something is wrong :\n",conditionMessage(e),"\n")
cat('
--------------------------------------------\n
         RFCV  ANALYSIS Fail\n
--------------------------------------------')},
 warning=function(a){message("Something is warning :\n",conditionMessage(a),"\n")
cat('
--------------------------------------------\n
         RFCV  ANALYSIS Fail\n
--------------------------------------------')})

cat('
--------------------------------------------\n
         EMP_MICRO WORKFLOW OVER\n
ALL RESULTS HAVE DEPOSITED AT WORING SPACE\n
--------------------------------------------\n\n')


}