#' @import ggplot2
#' @import grid
#' @importFrom dplyr inner_join
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @import multcomp
#' @import patchwork
#' @importFrom fs file_move
#' @import stringr
#' @importFrom ape pcoa
#' @importFrom vegan vegdist
#' @importFrom vegan adonis
#' @importFrom ggiraph girafe
#' @importFrom ggiraph geom_point_interactive
#' @importFrom htmlwidgets saveWidget




options(dplyr.summarise.inform = FALSE)

pca_boxplot=function(data,design,seed=123,group_level=c('default'),method=c('LSD'),ellipse = NULL,width=15,height=15,distance=c('bray'),palette=c("#E64B35FF","#4DBBD5FF","#00A087FF","#3C5488FF","#F39B7FFF","#8491B4FF",
                                                                                                         "#B2182B","#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7","#CC6666")){
  deposit <- list()
  data=data
  mapping=design
  try(mapping<-read.table(paste0(design),header = T),silent = T)
  mapping$Group=as.factor(mapping$Group)
  real_sample=Reduce(intersect,list(mapping$SampleID,rownames(data)))
  mapping=mapping[mapping$SampleID%in%real_sample,]
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
  groups <- data.frame(mapping$SampleID,mapping$Group)
  colnames(groups) <- c("V1","V2")
  length=length(unique(as.character(groups$V1)))
  times1=length%/%8
  res1=length%%8
  times2=length%/%5
  res2=length%%5
  col1=rep(1:8,times1)
  col=c(col1,1:res1)
  pich1=rep(c(21:24),times2)
  pich=c(pich1,15:(15+res2))
  ###########bray
  ###PCoA分析--轴12
  data <- vegan::vegdist(data,method=distance)
  pcoa<- ape::pcoa(data, correction = "none", rn = NULL)
  PC1 = pcoa$vectors[,1]
  PC2 = pcoa$vectors[,2]
  PC3 = pcoa$vectors[,3]
  plotdata <- data.frame(rownames(pcoa$vectors),PC1,PC2,PC3,groups$V2)
  colnames(plotdata) <-c("sample","PC1","PC2","PC3","Group")
  pc1 <-round(pcoa$values$Relative_eig[1]*100,digits = 2)
  pc2 <-round(pcoa$values$Relative_eig[2]*100,digits = 2)
  pc3 <-round(pcoa$values$Relative_eig[3]*100,digits = 2)
  #plotdata$Group <- factor(plotdata$Group,levels = name_group)
  


  #PC1和PC2的显著性检验(PC1,PC2,PC3进行组间差异检验)
  if (method %in%  c('LSD','HSD','duncan','scheffe','REGW','SNK')) {
    yf <- plotdata
    yd1 <- yf %>% dplyr::group_by(Group) %>% dplyr::summarise(Max = max(PC1))
    yd2 <- yf %>% dplyr::group_by(Group) %>% dplyr::summarise(Max = max(PC2))
    yd3 <- yf %>% dplyr::group_by(Group) %>% dplyr::summarise(Max = max(PC3))
    yd1$Max <- yd1$Max + max(yd1$Max)*0.1
    yd2$Max <- yd2$Max + max(yd2$Max)*0.1
    yd3$Max <- yd3$Max + max(yd2$Max)*0.1
    fit1 <- aov(PC1~Group,data = plotdata)    #ANOVA检验≥3组样本
    fit2 <- aov(PC2~Group,data = plotdata)
    fit3 <- aov(PC3~Group,data = plotdata)

    fit1_test=multi_test(fit1,method,mapping)
    fit2_test=multi_test(fit2,method,mapping)
    fit3_test=multi_test(fit3,method,mapping)


    a=data.frame(groups=fit1_test$model$groups$groups,Gid=rownames(fit1_test$model$groups))
    a$Gid=factor(a$Gid,levels = yd1$Group)
    a=as.data.frame(a[order(a$Gid),])

    b=data.frame(groups=fit2_test$model$groups$groups,Gid=rownames(fit2_test$model$groups))
    b$Gid=factor(b$Gid,levels = yd1$Group)
    b=as.data.frame(b[order(b$Gid),])

    c=data.frame(groups=fit3_test$model$groups$groups,Gid=rownames(fit3_test$model$groups))
    c$Gid=factor(c$Gid,levels = yd1$Group)
    c=as.data.frame(c[order(c$Gid),])


    test <- data.frame(PC1 =a$groups,PC2 = b$groups,PC3 = c$groups,
                        yd1 = yd1$Max,yd2 = yd2$Max,yd3 = yd3$Max,Group = yd1$Group)
    rownames(test)=yd1$Group

    test$Group <- factor(test$Group,levels = name_group)
    #相须图绘制
    p1 <- ggplot(plotdata,aes(Group,PC1)) +
      geom_boxplot(aes(fill = Group),outlier.colour = NA) +scale_fill_manual(values=palette)+
      geom_text(data = test,aes(x = Group,y = yd1,label = PC1),
                size = 7,color = "black",fontface = "bold") +
      coord_flip() +ggiraph::geom_point_interactive(aes(tooltip = paste0(sample,' : ',round(PC1,2))),position = "jitter")+
      theme_bw()+
      theme(axis.ticks.length = unit(0.4,"lines"),
            axis.ticks = element_line(color='black'),
            axis.line = element_line(colour = "black"),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_text(colour='black',size=20,face = "bold"),
            axis.text.x=element_blank(),
            legend.position = "none")
    
    p2 <- ggplot(plotdata,aes(Group,PC2)) +
      geom_boxplot(aes(fill = Group),outlier.colour = NA) +scale_fill_manual(values=palette)+
      geom_text(data = test,aes(x = Group,y = yd2,label = PC2),
                size = 7,color = "black",fontface = "bold")+ggiraph::geom_point_interactive(aes(tooltip = paste0(sample,' : ',round(PC2,2))),position = "jitter")+
      theme_bw()+
      theme(axis.ticks.length = unit(0.4,"lines"),
            axis.ticks = element_line(color='black'),
            axis.line = element_line(colour = "black"),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.x=element_text(colour='black',size=20,angle = 45,
                                     vjust = 1,hjust = 1,face = "bold"),
            axis.text.y=element_blank(),
            legend.position = "none")
    
    p2_r <- ggplot(plotdata,aes(Group,PC2)) +
      geom_boxplot(aes(fill = Group),outlier.colour = NA) +scale_fill_manual(values=palette)+
      geom_text(data = test,aes(x = Group,y = yd2,label = PC2),
                size = 7,color = "black",fontface = "bold")+coord_flip() +ggiraph::geom_point_interactive(aes(tooltip = paste0(sample,' : ',round(PC2,2))),position = "jitter")+
      theme_bw()+
      theme(axis.ticks.length = unit(0.4,"lines"),
            axis.ticks = element_line(color='black'),
            axis.line = element_line(colour = "black"),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_text(colour='black',size=20,face = "bold"),
            axis.text.x=element_blank(),
            legend.position = "none")
    
    
    
    p3 <- ggplot(plotdata,aes(Group,PC3)) + scale_fill_manual(values=palette) +
      geom_boxplot(aes(fill = Group),outlier.colour = NA) +
      geom_text(data = test,aes(x = Group,y = yd3,label = PC3),
                size = 7,color = "black",fontface = "bold") +ggiraph::geom_point_interactive(aes(tooltip = paste0(sample,' : ',round(PC3,2))),position = "jitter")+
      theme_bw()+
      theme(axis.ticks.length = unit(0.4,"lines"),
            axis.ticks = element_line(color='black'),
            axis.line = element_line(colour = "black"),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.x=element_text(colour='black',size=20,angle = 45,
                                     vjust = 1,hjust = 1,face = "bold"),
            axis.text.y=element_blank(),
            legend.position = "none")

    deposit$test$PC1_test=fit1_test$comparison
    deposit$test$PC2_test=fit2_test$comparison
    deposit$test$PC3_test=fit3_test$comparison

  }else if(method == 'ttest'){
    group_combn=combn(as.character(unique(plotdata$Group)),2)
    compare=plyr::alply(group_combn,2)
    
    #相须图绘制
    p1 <- ggplot(plotdata,aes(Group,PC1)) +
      geom_boxplot(aes(fill = Group),outlier.colour = NA) +scale_fill_manual(values=palette)+
      ggpubr::stat_compare_means(comparisons = compare,method="t.test")+
      coord_flip() +ggiraph::geom_point_interactive(aes(tooltip = paste0(sample,' : ',round(PC1,2))),position = "jitter")+
      theme_bw()+
      theme(axis.ticks.length = unit(0.4,"lines"),
            axis.ticks = element_line(color='black'),
            axis.line = element_line(colour = "black"),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_text(colour='black',size=20,face = "bold"),
            axis.text.x=element_blank(),
            legend.position = "none")
    
    p2 <- ggplot(plotdata,aes(Group,PC2)) +
      geom_boxplot(aes(fill = Group),outlier.colour = NA) +scale_fill_manual(values=palette)+
      ggpubr::stat_compare_means(comparisons = compare,method="t.test")+ggiraph::geom_point_interactive(aes(tooltip = paste0(sample,' : ',round(PC2,2))),position = "jitter")+
      theme_bw()+
      theme(axis.ticks.length = unit(0.4,"lines"),
            axis.ticks = element_line(color='black'),
            axis.line = element_line(colour = "black"),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.x=element_text(colour='black',size=20,angle = 45,
                                     vjust = 1,hjust = 1,face = "bold"),
            axis.text.y=element_blank(),
            legend.position = "none")
    
    p2_r <- ggplot(plotdata,aes(Group,PC2)) +
      geom_boxplot(aes(fill = Group),outlier.colour = NA) +scale_fill_manual(values=palette)+
      ggpubr::stat_compare_means(comparisons = compare,method="t.test")+coord_flip() +ggiraph::geom_point_interactive(aes(tooltip = paste0(sample,' : ',round(PC2,2))),position = "jitter")+
      theme_bw()+
      theme(axis.ticks.length = unit(0.4,"lines"),
            axis.ticks = element_line(color='black'),
            axis.line = element_line(colour = "black"),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_text(colour='black',size=20,face = "bold"),
            axis.text.x=element_blank(),
            legend.position = "none")
    
    
    
    p3 <- ggplot(plotdata,aes(Group,PC3)) + scale_fill_manual(values=palette) +
      geom_boxplot(aes(fill = Group),outlier.colour = NA) +
      ggpubr::stat_compare_means(comparisons = compare,method="t.test") +ggiraph::geom_point_interactive(aes(tooltip = paste0(sample,' : ',round(PC3,2))),position = "jitter")+
      theme_bw()+
      theme(axis.ticks.length = unit(0.4,"lines"),
            axis.ticks = element_line(color='black'),
            axis.line = element_line(colour = "black"),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.x=element_text(colour='black',size=20,angle = 45,
                                     vjust = 1,hjust = 1,face = "bold"),
            axis.text.y=element_blank(),
            legend.position = "none")
  }else{
    warning('Please select the correct method !')
  }

  
  #PCoA结果图绘制
  p12<-ggplot(plotdata, aes(PC1, PC2)) +
    ggiraph::geom_point_interactive(aes(fill=Group,tooltip = paste0(sample,'\n','x: ',round(PC1,2),'\n','y: ',round(PC2,2))),size=8,pch = 21)+
    scale_fill_manual(values=palette,name = "Group")+
    xlab(paste("PCoA1 ( ",pc1,"%"," )",sep="")) +
    ylab(paste("PCoA2 ( ",pc2,"%"," )",sep=""))+
    xlim(ggplot_build(p1)$layout$panel_scales_y[[1]]$range$range) +
    ylim(ggplot_build(p2)$layout$panel_scales_y[[1]]$range$range) +
    theme(text=element_text(size=30))+
    geom_vline(aes(xintercept = 0),linetype="dotted")+
    geom_hline(aes(yintercept = 0),linetype="dotted")+
    theme(panel.background = element_rect(fill='white', colour='black'),
          panel.grid=element_blank(),
          axis.title = element_text(color='black',size=34),
          axis.ticks.length = unit(0.4,"lines"), axis.ticks = element_line(color='black'),
          axis.line = element_line(colour = "black"),
          axis.title.x=element_text(colour='black', size=34),
          axis.title.y=element_text(colour='black', size=34),
          axis.text=element_text(colour='black',size=28),
          legend.title=element_text(size = 24,face = "bold"),
          legend.text=element_text(size=20),
          legend.key=element_blank(),legend.position = c('left'),
          legend.background = element_rect(colour = "black"),
          legend.key.height=unit(1,"cm")) +
    guides(fill = guide_legend(ncol = 1))
  
  
  p13<-ggplot(plotdata, aes(PC1, PC3)) +
    ggiraph::geom_point_interactive(aes(fill=Group,tooltip = paste0(sample,'\n','x: ',round(PC1,2),'\n','y: ',round(PC3,2))),size=8,pch = 21)+
    scale_fill_manual(values=palette,name = "Group")+
    xlab(paste("PCoA1 ( ",pc1,"%"," )",sep="")) +
    ylab(paste("PCoA3 ( ",pc3,"%"," )",sep=""))+
    xlim(ggplot_build(p1)$layout$panel_scales_y[[1]]$range$range) +
    ylim(ggplot_build(p3)$layout$panel_scales_y[[1]]$range$range) +
    theme(text=element_text(size=30))+
    geom_vline(aes(xintercept = 0),linetype="dotted")+
    geom_hline(aes(yintercept = 0),linetype="dotted")+
    theme(panel.background = element_rect(fill='white', colour='black'),
          panel.grid=element_blank(),
          axis.title = element_text(color='black',size=34),
          axis.ticks.length = unit(0.4,"lines"), axis.ticks = element_line(color='black'),
          axis.line = element_line(colour = "black"),
          axis.title.x=element_text(colour='black', size=34),
          axis.title.y=element_text(colour='black', size=34),
          axis.text=element_text(colour='black',size=28),
          legend.title=element_text(size = 24,face = "bold"),
          legend.text=element_text(size=20),
          legend.key=element_blank(),legend.position = c('left'),
          legend.background = element_rect(colour = "black"),
          legend.key.height=unit(1,"cm")) +
    guides(fill = guide_legend(ncol = 1))
  
  p23<-ggplot(plotdata, aes(PC2, PC3)) +
    ggiraph::geom_point_interactive(aes(fill=Group,tooltip = paste0(sample,'\n','x: ',round(PC2,2),'\n','y: ',round(PC3,2))),size=8,pch = 21)+
    scale_fill_manual(values=palette,name = "Group")+
    xlab(paste("PCoA2 ( ",pc2,"%"," )",sep="")) +
    ylab(paste("PCoA3 ( ",pc3,"%"," )",sep=""))+
    xlim(ggplot_build(p2)$layout$panel_scales_y[[1]]$range$range) +
    ylim(ggplot_build(p3)$layout$panel_scales_y[[1]]$range$range) +
    theme(text=element_text(size=30))+
    geom_vline(aes(xintercept = 0),linetype="dotted")+
    geom_hline(aes(yintercept = 0),linetype="dotted")+
    theme(panel.background = element_rect(fill='white', colour='black'),
          panel.grid=element_blank(),
          axis.title = element_text(color='black',size=34),
          axis.ticks.length = unit(0.4,"lines"), axis.ticks = element_line(color='black'),
          axis.line = element_line(colour = "black"),
          axis.title.x=element_text(colour='black', size=34),
          axis.title.y=element_text(colour='black', size=34),
          axis.text=element_text(colour='black',size=28),
          legend.title=element_text(size = 24,face = "bold"),
          legend.text=element_text(size=20),
          legend.key=element_blank(),legend.position = c('left'),
          legend.background = element_rect(colour = "black"),
          legend.key.height=unit(1,"cm")) +
    guides(fill = guide_legend(ncol = 1))
  
  if (!is.null(ellipse)) {
    p12 <-p12+ggplot2::stat_ellipse(aes(color=Group,group=Group),level=ellipse)+scale_colour_manual(values=palette)+guides(colour = "none")
    p13 <-p13+ggplot2::stat_ellipse(aes(color=Group,group=Group),level=ellipse)+scale_colour_manual(values=palette)+guides(colour = "none")
    p23 <-p23+ggplot2::stat_ellipse(aes(color=Group,group=Group),level=ellipse)+scale_colour_manual(values=palette)+guides(colour = "none")
  }
  
  
  
  #PERMANOVA分析
  set.seed(seed)
  otu.adonis <- vegan::adonis2(data~V2,data = groups,method = distance)
  p5 <- ggplot() +
    geom_text(aes(x = -0.5,y = 0.6,
                  label = paste(distance,
                                "\nPERMANOVA:\ndf = ",
                                otu.adonis$Df[1],
                                "\nR2 = ",round(otu.adonis$R2[1],4),
                                "\np-value = ",otu.adonis$`Pr(>F)`[1],sep = "")),
              size = 7) +
    theme_bw() +
    xlab("") + ylab("") +
    theme(panel.grid=element_blank(),
          axis.title = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank())
  
  #图像拼接-使用patchwork包将4幅图拼在一起
  p12 <- p1 + p5 + p12 + p2 +
    plot_layout(heights = c(1,4),widths = c(4,1),ncol = 2,nrow = 2)
  p23 <- p2_r + p5 + p23 + p3 +
    plot_layout(heights = c(1,4),widths = c(4,1),ncol = 2,nrow = 2)
  p13 <- p1 + p5 + p13 + p3 +
    plot_layout(heights = c(1,4),widths = c(4,1),ncol = 2,nrow = 2)
  #生成交互式html文件
  #注意这一步生成的html随机过程已经被锁定了，需要设定种子，而ggplot2的pdf每次提取的时候都会产生随机，应在出图时候确定种子
  set.seed(seed)
  p12_html=ggiraph::girafe(code = print(p12),width_svg = width,height_svg = height)
  set.seed(seed)
  p13_html=ggiraph::girafe(code = print(p13),width_svg = width,height_svg = height)
  set.seed(seed)
  p23_html=ggiraph::girafe(code = print(p23),width_svg = width,height_svg = height)
  
  #存储数据
  deposit$pic$p12=p12
  deposit$pic$p23=p23
  deposit$pic$p13=p13
  deposit$html$p12_html=p12_html
  deposit$html$p13_html=p13_html
  deposit$html$p23_html=p23_html
  deposit$pc_data=plotdata
  return(deposit)
}


beta_plot=function(data = NULL,design,dir = NULL,group_level=c('default'),seed=123,min_relative = 0,min_ratio = 0,adjust = T,pattern = '',output = F,html_out = F,change=F,change_name='Other',
                   method='HSD',width=15,height=15,distance = 'bray',ellipse = NULL,palette=c("#E64B35FF","#4DBBD5FF","#00A087FF","#3C5488FF","#F39B7FFF","#8491B4FF",
                                                              "#B2182B","#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7","#CC6666") ){
  deposit=list()
  deposit$result=data_filter(dir = dir,data = data,min_relative = min_relative,min_ratio = min_ratio,design = design,adjust = adjust,pattern = pattern,output = output,change=change,change_name=change_name)
  # 提取输入文件数据，注意排除ID文件
  input_file=grep("ID$",names(deposit$result$filter_data),value=T,invert=T)
  for (i in input_file){
    data=data.frame()
    try(data<-subset(deposit$result$filter_data[[i]],select=-c(Group)),silent=T)
    # 确保数据必须大于三个维度，第一列为样本
    if (ncol(data) < 4) {
      warning('The dimension of data is less than 3 level!')
      }else{
      rownames(data)<-data[,1]
      data<-data[,-1]
      # 当adjust被关闭后，检查空行样本并将其删除
      if (adjust==F){
        id_0=apply(data,1,mean)
        try(samples_elimated<-rownames(data)[id_0==0],silent=T)
        data=data[id_0!=0,]
        if (0%in%id_0){
          warning(paste0('In ',i,' level, ',samples_elimated,' was elimated, due to empty data!') )
        }
      }
      deposit$plot[[i]]<-pca_boxplot(data =data ,design = design,group_level=group_level,ellipse = ellipse,seed=seed,method=method,distance=distance,palette=palette,width=width,height=height)
      if (html_out==T) {
        htmlwidgets::saveWidget(deposit$plot[[i]]$html$p12_html, paste0(i,'_',min_relative,'_',min_ratio,'_',distance,'_',method,'_p1-2.html'))
        htmlwidgets::saveWidget(deposit$plot[[i]]$html$p13_html, paste0(i,'_',min_relative,'_',min_ratio,'_',distance,'_',method,'_p1-3.html'))
        htmlwidgets::saveWidget(deposit$plot[[i]]$html$p23_html, paste0(i,'_',min_relative,'_',min_ratio,'_',distance,'_',method,'_p2-3.html'))
      }
    }
  }
  return(deposit)
}

multi_test=function(fit,method,mapping){
  if (method=='HSD') {
    group_num=as.numeric(table(mapping$Group))
    group_even_check=all(group_num==group_num[1])
    if (group_even_check==T) {
      res<- HSD.test(fit,'Group',unbalanced=F)
      pvalue<- HSD.test(fit,'Group',group = F,unbalanced=F)$comparison
    }else{
      res<- HSD.test(fit,'Group',unbalanced=T)
      pvalue<- HSD.test(fit,'Group',group = F,unbalanced=T)$comparison
      #message('Detected unequal replication, HSD test activated unbalanced mode.')
    }
  }else if(method=='LSD'){
    res<- LSD.test(fit,'Group')
    pvalue<- LSD.test(fit,'Group',group = F)$comparison
  }else if(method=='duncan'){
    res<- duncan.test(fit,'Group')
    pvalue<- duncan.test(fit,'Group',group = F)$comparison
  }else if(method=='scheffe'){
    res<- scheffe.test(fit,'Group')
    pvalue<- scheffe.test(fit,'Group',group = F)$comparison
  }else if(method=='REGW'){
    res<- REGW.test(fit,'Group')
    pvalue<- REGW.test(fit,'Group',group = F)$comparison
  }else if(method=='SNK'){
    res<- SNK.test(fit,'Group')
    pvalue<- SNK.test(fit,'Group',group = F)$comparison
  }else{warning("no method matched in multiple comparisons!")}
  deposit=list()
  deposit$model=res
  deposit$comparison=pvalue
  deposit$letter=data.frame(groups=res$groups$groups,Gid=rownames(res$groups))
  return(deposit)
}


