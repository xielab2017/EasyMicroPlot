#' @importFrom fs file_move
#' @importFrom stringr str_count
#' @importFrom stringr str_split
#' @importFrom psych corr.test
#' @importFrom igraph graph_from_adjacency_matrix
#' @importFrom igraph V
#' @importFrom igraph delete.vertices
#' @importFrom igraph E
#' @importFrom igraph get.vertex.attribute
#' @importFrom igraph plot.igraph
#' @importFrom igraph degree
#' @importFrom igraph layout_in_circle


cooc_plot <- function(data = NULL,design,dir = NULL,min_relative = 0,min_ratio = 0,pattern='',cooc_method='spearman',cooc_p=0.05,cooc_r=0.3,vertex.size=NULL,vertex.label.cex=NULL,edge.width=2,edge_color_positive='darkred',edge_color_negitive='steelblue',output = F,cooc_output = F,cooc_output_dir = NULL,width=10,height=10,set_color_level='phylum',change=F,change_name='Other'){
  deposit=list()
  # 注意这里adjust必须为关闭
  mapping <- design
  filter_result <- data_filter(dir = dir,data = data,min_relative = min_relative,min_ratio = min_ratio,design = design,adjust = F,pattern = pattern,output = output,change=change,change_name=change_name)
  deposit$result <- filter_result
  # 提取总色板和注释
  tax_profile_total <- cooc_color_set(filter_result)
  try(mapping <- read.table(design,header=T),silent=T)
  try(mapping <- subset(mapping,select = c(SampleID,Group)),silent=T)
  if (cooc_output == T ) {
    if (is.null(cooc_output_dir)){
      destination_folder <- paste0('cooc_result/pic')
    }else{
      destination_folder <- paste0(cooc_output_dir,'/cooc_result/pic')
    }
    dir.create(destination_folder,recursive = T)
  }
  for (Group_name in unique(mapping$Group)) {
    # 调整输入数据
    mapping_sub <- mapping[mapping$Group==Group_name,]
    input_list <- data_filter(dir = dir,data = data,min_relative = min_relative,min_ratio = min_ratio,
                              design = mapping_sub,adjust = F,pattern = pattern,output = F,change=change,change_name=change_name)
    
    input_file <- grep("ID$",names(input_list$filter_data),value=T,invert=T)
    for (k in input_file){
      color_ref <- tax_profile_total[[k]]$tax_color[[set_color_level]]
      input_ID <- input_list$filter_data[[paste0(k,'_ID')]]
      input_data <- input_list$filter_data[[k]]
      
      igraph_data <- subset(input_data,select = -c(Group))
      rownames(igraph_data)<-igraph_data$SampleID
      igraph_data <- subset(igraph_data,select = -c(SampleID))
      
      occor  <- psych::corr.test(as.matrix(igraph_data),use="pairwise",method=cooc_method,adjust='none',alpha=.05)
      occor.r <- occor$r
      occor.p <- occor$p
      occor.r[occor.p>cooc_p|abs(occor.r)<cooc_r] = 0 
      igraph <- igraph::graph_from_adjacency_matrix(occor.r,mode="undirected",weighted=TRUE,diag=FALSE)
      bad.vs <- igraph::V(igraph)[igraph::degree(igraph) == 0]
      igraph <- igraph::delete.vertices(igraph, bad.vs)
      # 将igraph weight属性赋值到igraph.weight
      igraph.weight <- igraph::E(igraph)$weight
      
      # 做图前去掉igraph的weight权重，因为做图时某些layout会受到其影响
      igraph::E(igraph)$weight <- NA
      E.color <- igraph.weight
      E.color <- ifelse(E.color>0, edge_color_positive,ifelse(E.color<0, edge_color_negitive,"grey"))
      igraph::E(igraph)$color = as.character(E.color)
      node <- igraph::get.vertex.attribute(igraph)$name
      
      
      tax_color_sub_set <- tax_profile_total[[k]]$tax_color[[k]]
      tax_profile_sub <- tax_profile_total[[k]]$tax_data[tax_profile_total[[k]]$tax_data$ID %in% node,]
      
      # 设定各节点颜色
      each_color <- c()
      for (cc in tax_profile_sub[,set_color_level]) {
        each_color_temp <- as.character(color_ref$color[color_ref$tax %in% cc])
        each_color <- append(each_color,each_color_temp)
      }
      igraph::V(igraph)$color <- as.character(each_color)
      
      # 创建共同legend
      color_legend <-color_ref[color_ref$tax%in% unique(tax_profile_sub[,set_color_level]),] 
      
      deposit$plot[[Group_name]][[k]]$igraph <- igraph
      deposit$plot[[Group_name]][[k]]$color_legend <- color_legend
      deposit$plot[[Group_name]][[k]]$info <- c(Group_name,k)
      deposit$plot[[Group_name]][[k]]$cor_result$cor_r <- occor.r
      deposit$plot[[Group_name]][[k]]$cor_result$cor_p <- occor.p
      
      if (cooc_output ==T ) {
        pdf(paste0(Group_name,'_',k,'.pdf'),width = width,height = height)
        par(mar = c(5, 0, 4, 10) + 0.1)
        sub_title=paste0(k,' level')
        igraph::plot.igraph(igraph,main=paste0("Group_",Group_name,"_Co-occurrence_network.pdf"),sub=sub_title,vertex.size=vertex.size,vertex.label.cex=vertex.label.cex,edge.width=edge.width,
                            edge.lty=1,edge.curved=F,margin=c(0,0,0,0),edge.curved=F,layout=igraph::layout_in_circle,vertex.label.color="black")
        legend(1.1,1,legend = color_legend$tax,text.col=as.character(color_legend$color),cex = 0.8)
        dev.off() 
        fs::file_move(paste0(Group_name,'_',k,'.pdf'),destination_folder)
      }
    }
  }
  cooc_profile <- cooc_info(deposit)
  deposit$cooc_profile <- cooc_profile
  return(deposit)
}



cooc_color_set <- function(data,palette=c("#E64B35FF","#4DBBD5FF","#00A087FF","#3C5488FF","#F39B7FFF","#8491B4FF",
                                           "#B2182B","#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7","#CC6666")){
  deposit =list()
  data=data
  input_file=grep("ID$",names(data$filter_data),value=T,invert=T)
  # 直接获取当前最高级别数据
  for (tax_level in input_file) {
    input_ID_total <- data$filter_data[[paste0(tax_level,'_ID')]]
    input_data_total <- data$filter_data[[tax_level]]
    
    # 筛选规律后的数据表
    tax_total=colnames(input_data_total)[!c(colnames(input_data_total)%in%c('SampleID','Group'))]
    tax_profile_total=input_ID_total[input_ID_total$ID %in% tax_total,]
    anotation <- stringr::str_split(tax_profile_total$tax,';')
    tax_level_total=c('phylum','class','order','family','genus','species')
    sep_num=stringr::str_count(tax_profile_total$tax[1],pattern = ';')
    tax_profile_total_color=list()
    for (m in 1:sep_num) {
      tax_split=c()
      for (n in 1:length(tax_total)) {
        tax_split <- append(tax_split,anotation[[n]][m+1])
      }
      if (length(unique(tax_split)) > length(palette)) {
        tax_split_color <- rainbow(length(unique(tax_split)))
      }else{
        tax_split_color <- palette[1:length(unique(tax_split))]
      }
      tax_profile_total[tax_level_total[m]] <- tax_split
      tax_profile_total_color[[tax_level_total[m]]] <- data.frame(tax=unique(tax_split),color=tax_split_color)
    }
    
    deposit[[tax_level]]$tax_data <- tax_profile_total
    deposit[[tax_level]]$tax_color <- tax_profile_total_color
  }
  return(deposit)
}



cooc_info <- function(data){
  deposit <- list()
  input_file=grep("ID$",names(data$result$filter_data),value=T,invert=T)
  for (tax_level in input_file) {
    table_combie <- data.frame()
    for (Group_name in names(data$plot)) {
      temp <- cooc_info_each(data$plot[[Group_name]][[tax_level]])
      table_combie <- rbind(table_combie,temp)
    }
    deposit[[tax_level]]  <- table_combie  
  }
  return(deposit)
}


cooc_plot_each <- function(data,vertex.size=NULL,vertex.label.cex=NULL,edge.width=2,cooc_output=F,width = 10,height = 10){
  igraph <- data$igraph
  Group_name <- data$info[1]
  color_legend <- data$color_legend
  sub_title=paste0(data$info[2],' level')
  
  igraph::plot.igraph(igraph,main=paste0("Group_",Group_name,"_Co-occurrence_network.pdf"),sub=sub_title,vertex.size=vertex.size,vertex.label.cex=vertex.label.cex,edge.width=edge.width,
                      edge.lty=1,edge.curved=F,margin=c(0,0,0,0),edge.curved=F,layout=igraph::layout_in_circle,vertex.label.color="black")
  legend(1.1,1,legend = color_legend$tax,text.col=as.character(color_legend$color),cex = 0.8)
  
  if (cooc_output ==T ) {
    pdf(paste0(Group_name,'_',data$info[2],'.pdf'),width = width,height = height)
    par(mar = c(5, 0, 4, 10) + 0.1)
    igraph::plot.igraph(igraph,main=paste0("Group_",Group_name,"_Co-occurrence_network.pdf"),sub=sub_title,vertex.size=vertex.size,vertex.label.cex=vertex.label.cex,edge.width=edge.width,
                        edge.lty=1,edge.curved=F,margin=c(0,0,0,0),edge.curved=F,layout=igraph::layout_in_circle,vertex.label.color="black")
    legend(1.1,1,legend = color_legend$tax,text.col=as.character(color_legend$color),cex = 0.8)
    invisible(dev.off())
  }
}

cooc_info_each <- function(data){
  deposit <- list()
  igraph <- data$igraph
  transitivity <- igraph::transitivity(igraph)
  graph_density <- igraph::graph.density(igraph)
  centralization_degree  <- igraph::centralization.degree(igraph)$centralization
  num.vertices <- length(igraph::V(igraph))
  num.edges <- length(igraph::E(igraph)) 
  co_info_combie <- data.frame(num.vertices=num.vertices,num.edges=num.edges,transitivity=transitivity,
                               centralization_degree=centralization_degree,graph_density=graph_density )
  rownames(co_info_combie) <- data$info[1]
  deposit <- co_info_combie
  return(deposit)
}





