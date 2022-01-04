EMP_COR_RDA <- function(data, meta, seed =123,width = 15, height = 15, ellipse = NULL,zoom = c(1,1,1), arrow_col=c('#F0E442','#CC79A7'),palette=c("#E64B35FF","#4DBBD5FF","#00A087FF","#3C5488FF","#F39B7FFF","#8491B4FF","#B2182B","#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7","#CC6666")){
  deposit <- list()
  data <- data
  env <- meta
  
  mapping <- subset(data,select = c(SampleID,Group))
  real_samples <- intersect(data$SampleID,env$SampleID)
  data <- data[data$SampleID %in% real_samples,]
  env <- env[env$SampleID %in% real_samples,]
  mapping <- mapping[mapping$SampleID %in%real_samples, ]

  deposit$input_data$data <- data
  deposit$input_data$meta <- env
  
  
  # 调整数据排序一致
  data <- subset(data,select = -c(Group))
  data <- data[order(data$SampleID),]
  env <- env[order(env$SampleID),]
  
  #######去除原有列名，使第一列为样本名
  rownames(data)<-data[,1] 
  data<-data[,-1] 
  
  rownames(env)<-env[,1] 
  env<-env[,-1]
  
  #######观察Axis lengths
  #######若上值大于4则选用CCA模型，小于3则选用RDA模型，3-4之间皆可
  #vegan::decorana(data)
  
  ####### 构建模型
  model <- vegan::rda(data,env[,-3],scale=T)
  
  
  model_summary <- summary(model)  #查看分析结果
  sp <- as.data.frame(model_summary$species[,1:2])/zoom[1]
  yz <- as.data.frame(model_summary$biplot[,1:2])/zoom[2]
  st <- as.data.frame(model_summary$constraints[,1:2])/c(zoom[3]+2)
  
  
  # 根据需要填写读取分组信息
  group=mapping$Group
  
  rda_plot <-ggplot() +
    ggiraph::geom_point_interactive(data = st,aes(RDA1,RDA2,shape=group,fill=group),size=4,tooltip=rownames(st))+
    scale_shape_manual(values = rep(21,length(unique(mapping$Group))))+
    scale_fill_manual(values=palette)+
    geom_segment(data = sp,aes(x = 0, y = 0, xend = RDA1, yend = RDA2), 
                 arrow = arrow(angle=22.5,length = unit(0.35,"cm"),
                               type = "closed"),linetype=1, size=0.6,colour = arrow_col[1])+
    ggrepel::geom_text_repel(data = sp,aes(RDA1,RDA2,label=row.names(sp)))+
    geom_segment(data = yz,aes(x = 0, y = 0, xend = RDA1, yend = RDA2), 
                 arrow = arrow(angle=22.5,length = unit(0.35,"cm"),
                               type = "closed"),linetype=1, size=0.6,colour = arrow_col[2])+
    ggrepel::geom_text_repel(data = yz,aes(RDA1,RDA2,label=row.names(yz)))+
    labs(x=paste("RDA 1 (", format(100 *model_summary$cont[[1]][2,1], digits=4), "%)", sep=""),
         y=paste("RDA 2 (", format(100 *model_summary$cont[[1]][2,2], digits=4), "%)", sep=""))+
    geom_hline(yintercept=0,linetype=3,size=1) + 
    geom_vline(xintercept=0,linetype=3,size=1)+
    guides(shape=guide_legend(title=NULL,color="black"),
           fill=guide_legend(title=NULL))+
    theme_bw()+theme(panel.grid=element_blank())
  
  if (!is.null(ellipse)) {
    rda_plot <-rda_plot+ggplot2::stat_ellipse(data=st,aes(RDA1,RDA2,color=group,group=group),level=ellipse)+scale_colour_manual(values=palette)+guides(colour = "none")
  }
  
  rda_plot_html <- ggiraph::girafe(code = print(rda_plot),width_svg = width,height_svg = height)
  #######通过蒙特卡罗置换检验（Monte Carlo permutation test)进行检验环境因子相关显著性
  ######检验整体
  set.seed(seed)
  test=vegan::permutest(model,permu=999)
  ######每个单独的环境因子
  set.seed(seed)
  ef=vegan::envfit(model,env[,-3],permu=999)
  
  ### 膨胀系数
  vif_value <- vegan::vif.cca(model)
  
  
  deposit$model <- model
  deposit$model_information$model_summary <- model_summary
  deposit$model_information$model_permutest<- test
  deposit$model_information$model_envfit<- ef
  deposit$model_information$model_vif<- vif_value
  deposit$plot$pic <- rda_plot
  deposit$plot$html <- rda_plot_html
  
  
  return(deposit)
  
}

