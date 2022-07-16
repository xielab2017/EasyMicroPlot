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


cooc_plot <- function(data = NULL,design,dir = NULL,group_combie = F,meta = NULL,min_relative = 0,min_ratio = 0,pattern='',cooc_method='spearman',cooc_p=0.05,cooc_r=0.3,vertex.size=NULL,vertex.label.cex=NULL,edge.width=2,edge.curved = F,edge_color_positive='darkred',edge_color_negitive='steelblue',meta_col = 'white',clust=F,seed=123,output = F,cooc_output = F,cooc_output_dir = NULL,width=10,height=10,heatmap_width = 5,heatmap_height = NULL,set_color_level='phylum',change=F,change_name='Other'){
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
      destination_folder_pic <- paste0('cooc_result/network/')
      destination_folder_vertex <- paste0('cooc_result/vertex/')
    }else{
      destination_folder_pic <- paste0(cooc_output_dir,'/cooc_result/network/')
      destination_folder_vertex <- paste0(cooc_output_dir,'/cooc_result/vertex/')
    }
    dir.create(destination_folder_pic,recursive = T)
    dir.create(destination_folder_vertex,recursive = T)
  }
  for (Group_name in unique(mapping$Group)) {
    # 调整输入数据
    if (group_combie == T) {
      mapping_sub <- mapping
      Group_name <- 'Total'
    }else{
      mapping_sub <- mapping[mapping$Group==Group_name,]
    }
    
    
    input_list <- data_filter(dir = dir,data = data,min_relative = min_relative,min_ratio = min_ratio,
                              design = mapping_sub,adjust = F,pattern = pattern,output = F,change=change,change_name=change_name)
    
    input_file <- grep("ID$",names(input_list$filter_data),value=T,invert=T)
    for (k in input_file){
      color_ref <- tax_profile_total[[k]]$tax_color[[set_color_level]]
      input_ID <- input_list$filter_data[[paste0(k,'_ID')]]
      input_data <- input_list$filter_data[[k]]
      
      igraph_data <- subset(input_data,select = -c(Group))
      if (!is.null(meta)) {
        igraph_data <- dplyr::inner_join(igraph_data,meta,by='SampleID')
      }
      igraph_data <- na.omit(igraph_data)
      rownames(igraph_data)<-igraph_data$SampleID
      igraph_data <- subset(igraph_data,select = -c(SampleID))
      occor  <- psych::corr.test(as.matrix(igraph_data),use="complete",method=cooc_method,adjust='none',alpha=.05)
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
      
      
      #tax_color_sub_set <- tax_profile_total[[k]]$tax_color[[k]]
      tax_profile_sub <- tax_profile_total[[k]]$tax_data[tax_profile_total[[k]]$tax_data$ID %in% node,]
      
      # 将数据与igraph节点排序一致，便于染色
      if (!is.null(meta)) {
        meta_feature_num <- as.numeric(table(node %in% colnames(meta))['TRUE'])
        tax_profile_sub$ID=factor(tax_profile_sub$ID,levels =node[1:c(length(node)-meta_feature_num)])
      }else{
        tax_profile_sub$ID=factor(tax_profile_sub$ID,levels =node)
      }
      tax_profile_sub=tax_profile_sub[order(tax_profile_sub$ID),]
      
      # 设定各节点颜色
      each_color <- c()
      for (cc in tax_profile_sub[,set_color_level]) {
        each_color_temp <- as.character(color_ref$color[color_ref$tax %in% cc])
        each_color <- append(each_color,each_color_temp)
      }
      # meta 数据采用white进行填充
      each_color=append(each_color,rep(meta_col,length(node)-length(each_color)))
      igraph::V(igraph)$color <- as.character(each_color)
      
      
      # 创建共同legend
      color_legend <-color_ref[color_ref$tax%in% unique(tax_profile_sub[,set_color_level]),] 
      if (!is.null(meta)) {
        color_legend <- as.data.frame(rbind(as.matrix(color_legend),c('Meta_data',meta_col)))
      }
      
      
      
      # 计算节点重要性
      page_rank_value=as.data.frame(igraph::page_rank(igraph)$vector,row.names = NULL)
      colnames(page_rank_value)='page_rank_value'
      page_rank_value['vertex']=rownames(page_rank_value)
      
      
      evcent_value=as.data.frame(igraph::evcent(igraph,scale = T)$vector)
      colnames(evcent_value)='evcent_value'
      evcent_value['vertex']=rownames(evcent_value)
      
      
      betweenness_value=as.data.frame(igraph::betweenness(igraph,normalized = T))
      colnames(betweenness_value)='betweenness_value'
      betweenness_value['vertex']=rownames(betweenness_value)
      
      vetex_importance <- dplyr::full_join(evcent_value,betweenness_value,by='vertex')
      vetex_importance <- dplyr::full_join(vetex_importance,page_rank_value,by='vertex')
      
      
      row.names(vetex_importance)=vetex_importance$vertex
      vetex_importance=subset(vetex_importance,select = -c(vertex))
      
      heatmap_palette<- c("#303596", "white","#a8002d") 
      # 这里需要增加一步检验，防止节点数目过少导致节点重要性无法计算，热图计算报错
      heatmap <- NULL
      suppressWarnings(try(heatmap <- pheatmap(vetex_importance,scale ='column', cluster_cols=F,border_color='black',width = 5,height = 10,
                              cluster_rows=T,color = colorRampPalette(heatmap_palette)(1000),silent = T),silent = T))
      
      # 数据存储
      deposit$plot[[Group_name]][[k]]$igraph <- igraph
      deposit$plot[[Group_name]][[k]]$color_legend <- color_legend
      deposit$plot[[Group_name]][[k]]$vertex_attribute$vertex_color <- each_color
      deposit$plot[[Group_name]][[k]]$vertex_attribute$vertex_importance_value <- vetex_importance
      if (!is.null(heatmap)) {
        deposit$plot[[Group_name]][[k]]$vertex_attribute$vertex_importance_plot <- heatmap
      }
      deposit$plot[[Group_name]][[k]]$edge_attribute$edge_color_positive <- edge_color_positive
      deposit$plot[[Group_name]][[k]]$edge_attribute$edge_color_negitive <- edge_color_negitive
      deposit$plot[[Group_name]][[k]]$edge_attribute$edge_color <- igraph::E(igraph)$color
      deposit$plot[[Group_name]][[k]]$info <- c(Group_name,k)
      deposit$plot[[Group_name]][[k]]$cor_result$cor_r <- occor.r
      deposit$plot[[Group_name]][[k]]$cor_result$cor_p <- occor.p
      
      if (cooc_output ==T ) {
        pdf(paste0(Group_name,'_',k,'.pdf'),width = width,height = height)
        par(mar = c(5, 0, 4, 10) + 0.1)
        sub_title=paste0(k,' level')
        if (clust == T) {
          fc = igraph::cluster_fast_greedy(igraph,weights =NULL)
          member.num<-length(table(igraph::membership(fc)))
          member.list<-list()
          for(i in 1:member.num){
            member.list<-c(member.list, list(fc$member==i))
          }
          deposit$plot[[Group_name]][[k]]$cor_result$fc_clust <- igraph::membership(fc)
          set.seed(seed)
          igraph::plot.igraph(igraph,main=paste0("Group_",Group_name,"_Co-occurrence_network.pdf"),sub=sub_title,vertex.size=vertex.size,vertex.label.cex=vertex.label.cex,edge.width=edge.width,
                              edge.lty=1,edge.curved=edge.curved,margin=c(0,0,0,0),layout=igraph::layout.fruchterman.reingold,mark.groups=member.list,mark.border = 'black',vertex.label.color="black")
        }else if (clust == F){
          igraph::plot.igraph(igraph,main=paste0("Group_",Group_name,"_Co-occurrence_network.pdf"),sub=sub_title,vertex.size=vertex.size,vertex.label.cex=vertex.label.cex,edge.width=edge.width,
                              edge.lty=1,edge.curved=edge.curved,margin=c(0,0,0,0),layout=igraph::layout_in_circle,vertex.label.color="black")
        }else{
          warning('Paramter clust should be True or False !')
        }
        legend(1.1,1,legend = color_legend$tax,text.col=as.character(color_legend$color),cex = 0.8)
        dev.off() 
        fs::file_move(paste0(Group_name,'_',k,'.pdf'),destination_folder_pic)
        
        #  输出节点重要性属性值
        vetex_importance_output <- data.frame(vetex=row.names(vetex_importance),evcent_value=vetex_importance$evcent_value,
                                              betweenness_value=vetex_importance$betweenness_value,page_rank_value=vetex_importance$page_rank_value)
        write.table(vetex_importance_output,file = paste0(destination_folder_vertex,Group_name,'_',k,'_vetex_importance.txt'),quote = F,sep = '\t',row.names = F)
        save_pheatmap_pdf(heatmap,filename = paste0(destination_folder_vertex,Group_name,'_',k,'_vetex_importance.pdf'),width = heatmap_width ,height = heatmap_height)
      }
    }
    if (group_combie == T) {
      break
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


cooc_plot_each <- function(data,vertex.size=NULL,meta_col = NULL,edge_color_positive = NULL,edge_color_negitive = NULL,vertex.label.cex=NULL,edge.width=2,edge.curved = F,cooc_output=F,width = 10,height = 10,clust = F,seed =123){
  igraph <- data$igraph
  Group_name <- data$info[1]
  color_legend <- data$color_legend
  vertex_color <- data$vertex_attribute$vertex_color
  edge_color <- data$edge_attribute$edge_color
  if (!is.null(meta_col)) {
    vertex_color=gsub(as.character(color_legend$color[color_legend$tax %in%'Meta_data']),meta_col ,vertex_color)
    igraph::V(igraph)$color <- as.character(vertex_color)
    
    color_legend$color=as.character(color_legend$color)
    color_legend$color[color_legend$tax %in%'Meta_data'] <- meta_col
  }
  
  if (!is.null(edge_color_positive)) {
    edge_color=gsub(as.character(data$edge_attribute$edge_color_positive), edge_color_positive, edge_color)
    igraph::E(igraph)$color <- as.character(edge_color)
  } 

  if (!is.null(edge_color_negitive)) {
    edge_color=gsub(as.character(data$edge_attribute$edge_color_negitive), edge_color_negitive, edge_color)
    igraph::E(igraph)$color <- as.character(edge_color)
  }  
 
  sub_title=paste0(data$info[2],' level')
  
  if (clust == T) {
    fc = igraph::cluster_fast_greedy(igraph,weights =NULL)
    member.num<-length(table(igraph::membership(fc)))
    member.list<-list()
    for(i in 1:member.num){
      member.list<-c(member.list, list(fc$member==i))
    }
    #deposit$plot[[Group_name]][[k]]$cor_result$fc_clust <- igraph::membership(fc)
    set.seed(seed)
    igraph::plot.igraph(igraph,main=paste0("Group_",Group_name,"_Co-occurrence_network.pdf"),sub=sub_title,vertex.size=vertex.size,vertex.label.cex=vertex.label.cex,edge.width=edge.width,
                        edge.lty=1,edge.curved=edge.curved,margin=c(0,0,0,0),layout=igraph::layout.fruchterman.reingold,mark.groups=member.list,mark.border = 'black',vertex.label.color="black")
  }else if (clust == F){
    igraph::plot.igraph(igraph,main=paste0("Group_",Group_name,"_Co-occurrence_network.pdf"),sub=sub_title,vertex.size=vertex.size,vertex.label.cex=vertex.label.cex,edge.width=edge.width,
                        edge.lty=1,edge.curved=edge.curved,margin=c(0,0,0,0),layout=igraph::layout_in_circle,vertex.label.color="black")
  }else{
    warning('Paramter clust should be True or False !')
  }
  legend(1.1,1,legend = color_legend$tax,text.col=as.character(color_legend$color),cex = 0.8)
  
  if (cooc_output ==T ) {
    pdf(paste0(Group_name,'_',data$info[2],'.pdf'),width = width,height = height)
    par(mar = c(5, 0, 4, 10) + 0.1)
    if (clust == T) {
      fc = igraph::cluster_fast_greedy(igraph,weights =NULL)
      member.num<-length(table(igraph::membership(fc)))
      member.list<-list()
      for(i in 1:member.num){
        member.list<-c(member.list, list(fc$member==i))
      }
      #deposit$plot[[Group_name]][[k]]$cor_result$fc_clust <- igraph::membership(fc)
      set.seed(seed)
      igraph::plot.igraph(igraph,main=paste0("Group_",Group_name,"_Co-occurrence_network.pdf"),sub=sub_title,vertex.size=vertex.size,vertex.label.cex=vertex.label.cex,edge.width=edge.width,
                          edge.lty=1,edge.curved=edge.curved,margin=c(0,0,0,0),edge.curved=T,layout=igraph::layout.fruchterman.reingold,mark.groups=member.list,mark.border = 'black',vertex.label.color="black")
    }else if (clust == F){
      igraph::plot.igraph(igraph,main=paste0("Group_",Group_name,"_Co-occurrence_network.pdf"),sub=sub_title,vertex.size=vertex.size,vertex.label.cex=vertex.label.cex,edge.width=edge.width,
                          edge.lty=1,edge.curved=edge.curved,margin=c(0,0,0,0),edge.curved=edge.curved,layout=igraph::layout_in_circle,vertex.label.color="black")
    }else{
      warning('Paramter clust should be True or False !')
    }
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




save_pheatmap_pdf <- function(x, filename, width=NULL, height=NULL) {
  stopifnot(!missing(x))
  stopifnot(!missing(filename))
  pdf(filename, width=width, height=height)
  grid::grid.newpage()
  grid::grid.draw(x$gtable)
  invisible(dev.off())
}


lo = function(rown, coln, nrow, ncol, cellheight = NA, cellwidth = NA, treeheight_col, treeheight_row, legend, annotation_row, annotation_col, annotation_colors, annotation_legend, annotation_names_row, annotation_names_col, main, fontsize, fontsize_row , fontsize_col, angle_col, gaps_row, gaps_col, ...){
    # Get height of colnames and length of rownames
    if(!is.null(coln[1]) | (!is.na2(annotation_row) & annotation_names_row)){
        if(!is.null(coln[1])){
            t = coln
        } else {
            t = ""
        }
        tw = strwidth(t, units = 'in', cex = fontsize_col / fontsize)
        if(annotation_names_row){
            t = c(t, colnames(annotation_row))
            tw = c(tw, strwidth(colnames(annotation_row), units = 'in'))
        }
        longest_coln = which.max(tw)
        gp = list(fontsize = ifelse(longest_coln <= length(coln), fontsize_col, fontsize), ...)
        coln_height = unit(1, "grobheight", textGrob(t[longest_coln], rot = angle_col, gp = do.call(gpar, gp))) + unit(10, "bigpts")
    }
    else{
        coln_height = unit(5, "bigpts")
    }
    
    if(!is.null(rown[1])){
        t = rown
        tw = strwidth(t, units = 'in', cex = fontsize_row / fontsize)
        if(annotation_names_col){
            t = c(t, colnames(annotation_col))
            tw = c(tw, strwidth(colnames(annotation_col), units = 'in'))
        }
        longest_rown = which.max(tw)
        gp = list(fontsize = ifelse(longest_rown <= length(rown), fontsize_row, fontsize), ...)
        rown_width = unit(1, "grobwidth", textGrob(t[longest_rown], rot = 0, gp = do.call(gpar, gp))) + unit(10, "bigpts")
    }
    else{
        rown_width = unit(5, "bigpts")
    }
    
    gp = list(fontsize = fontsize, ...)
    # Legend position
    if(!is.na2(legend)){
        longest_break = which.max(nchar(names(legend)))
        longest_break = unit(1.1, "grobwidth", textGrob(as.character(names(legend))[longest_break], gp = do.call(gpar, gp)))
        title_length = unit(1.1, "grobwidth", textGrob("Scale", gp = gpar(fontface = "bold", ...)))
        legend_width = unit(12, "bigpts") + longest_break * 1.2
        legend_width = max(title_length, legend_width)
    }
    else{
        legend_width = unit(0, "bigpts")
    }
    
    # Set main title height
    if(is.na(main)){
        main_height = unit(0, "npc")
    }
    else{
        main_height = unit(1.5, "grobheight", textGrob(main, gp = gpar(fontsize = 1.3 * fontsize, ...)))
    }
    
    # Column annotations
    textheight = unit(fontsize, "bigpts")
    
    if(!is.na2(annotation_col)){
        # Column annotation height 
        annot_col_height = ncol(annotation_col) * (textheight + unit(2, "bigpts")) + unit(2, "bigpts")
        
        # Width of the correponding legend
        t = c(as.vector(as.matrix(annotation_col)), colnames(annotation_col)) 
        annot_col_legend_width = unit(1.2, "grobwidth", textGrob(t[which.max(nchar(t))], gp = gpar(...))) + unit(12, "bigpts")
        if(!annotation_legend){
            annot_col_legend_width = unit(0, "npc")
        }
    }
    else{
        annot_col_height = unit(0, "bigpts")
        annot_col_legend_width = unit(0, "bigpts")
    }
    
    # Row annotations
    if(!is.na2(annotation_row)){
        # Row annotation width 
        annot_row_width = ncol(annotation_row) * (textheight + unit(2, "bigpts")) + unit(2, "bigpts")
        
        # Width of the correponding legend
        t = c(as.vector(as.matrix(annotation_row)), colnames(annotation_row)) 
        annot_row_legend_width = unit(1.2, "grobwidth", textGrob(t[which.max(nchar(t))], gp = gpar(...))) + unit(12, "bigpts")
        if(!annotation_legend){
            annot_row_legend_width = unit(0, "npc")
        }
    }
    else{
        annot_row_width = unit(0, "bigpts")
        annot_row_legend_width = unit(0, "bigpts")
    }
    
    annot_legend_width = max(annot_row_legend_width, annot_col_legend_width)
    
    # Tree height
    treeheight_col = unit(treeheight_col, "bigpts") + unit(5, "bigpts")
    treeheight_row = unit(treeheight_row, "bigpts") + unit(5, "bigpts") 
    
    # Set cell sizes
    if(is.na(cellwidth)){
        mat_width = unit(1, "npc") - rown_width - legend_width - treeheight_row - annot_row_width - annot_legend_width 
    }
    else{
        mat_width = unit(cellwidth * ncol, "bigpts") + length(gaps_col) * unit(4, "bigpts")
    }
    
    if(is.na(cellheight)){
        mat_height = unit(1, "npc") - main_height - coln_height - treeheight_col - annot_col_height
    }
    else{
        mat_height = unit(cellheight * nrow, "bigpts") + length(gaps_row) * unit(4, "bigpts")
    }    
    
    # Produce gtable
    gt = gtable(widths = unit.c(treeheight_row, annot_row_width, mat_width, rown_width, legend_width, annot_legend_width), heights = unit.c(main_height, treeheight_col, annot_col_height, mat_height, coln_height), vp = viewport(gp = do.call(gpar, gp)))
    
    cw = convertWidth(mat_width - (length(gaps_col) * unit(4, "bigpts")), "bigpts", valueOnly = T) / ncol
    ch = convertHeight(mat_height - (length(gaps_row) * unit(4, "bigpts")), "bigpts", valueOnly = T) / nrow
    
    # Return minimal cell dimension in bigpts to decide if borders are drawn
    mindim = min(cw, ch) 
    
    res = list(gt = gt, mindim = mindim)
    
    return(res)
}

find_coordinates = function(n, gaps, m = 1:n){
    if(length(gaps) == 0){
        return(list(coord = unit(m / n, "npc"), size = unit(1 / n, "npc") ))
    }
    
    if(max(gaps) > n){
        stop("Gaps do not match with matrix size")
    }
    
    size = (1 / n) * (unit(1, "npc") - length(gaps) * unit("4", "bigpts"))
    
    gaps2 = apply(sapply(gaps, function(gap, x){x > gap}, m), 1, sum) 
    coord = m * size + (gaps2 * unit("4", "bigpts"))
    
    return(list(coord = coord, size = size))
}

draw_dendrogram = function(hc, gaps, horizontal = T){
    h = hc$height / max(hc$height) / 1.05
    m = hc$merge
    o = hc$order
    n = length(o)

    m[m > 0] = n + m[m > 0] 
    m[m < 0] = abs(m[m < 0])

    dist = matrix(0, nrow = 2 * n - 1, ncol = 2, dimnames = list(NULL, c("x", "y"))) 
    dist[1:n, 1] = 1 / n / 2 + (1 / n) * (match(1:n, o) - 1)

    for(i in 1:nrow(m)){
        dist[n + i, 1] = (dist[m[i, 1], 1] + dist[m[i, 2], 1]) / 2
        dist[n + i, 2] = h[i]
    }
    
    draw_connection = function(x1, x2, y1, y2, y){
        res = list(
            x = c(x1, x1, x2, x2),
            y = c(y1, y, y, y2)
        )
        
        return(res)
    }
    
    x = rep(NA, nrow(m) * 4)
    y = rep(NA, nrow(m) * 4)
    id = rep(1:nrow(m), rep(4, nrow(m)))
    
    for(i in 1:nrow(m)){
        c = draw_connection(dist[m[i, 1], 1], dist[m[i, 2], 1], dist[m[i, 1], 2], dist[m[i, 2], 2], h[i])
        k = (i - 1) * 4 + 1
        x[k : (k + 3)] = c$x
        y[k : (k + 3)] = c$y
    }
    
    x = find_coordinates(n, gaps, x * n)$coord
    y = unit(y, "npc")
    
    if(!horizontal){
        a = x
        x = unit(1, "npc") - y
        y = unit(1, "npc") - a
    }
    res = polylineGrob(x = x, y = y, id = id)
    
    return(res)
}

draw_matrix = function(matrix, border_color, gaps_rows, gaps_cols, fmat, fontsize_number, number_color){
    n = nrow(matrix)
    m = ncol(matrix)
    
    coord_x = find_coordinates(m, gaps_cols)
    coord_y = find_coordinates(n, gaps_rows)
    
    x = coord_x$coord - 0.5 * coord_x$size
    y = unit(1, "npc") - (coord_y$coord - 0.5 * coord_y$size)
    
    coord = expand.grid(y = y, x = x)
    
    res = gList()
    
    res[["rect"]] = rectGrob(x = coord$x, y = coord$y, width = coord_x$size, height = coord_y$size, gp = gpar(fill = matrix, col = border_color))
    
    if(attr(fmat, "draw")){
        res[["text"]] = textGrob(x = coord$x, y = coord$y, label = fmat, gp = gpar(col = number_color, fontsize = fontsize_number))
    }
    
    res = gTree(children = res)
    
    return(res)
}

draw_colnames = function(coln, gaps, vjust_col, hjust_col, angle_col, ...){
    coord = find_coordinates(length(coln), gaps)
    x = coord$coord - 0.5 * coord$size
    
    res = textGrob(coln, x = x, y = unit(1, "npc") - unit(3, "bigpts"), vjust = vjust_col, hjust = hjust_col, rot = angle_col, gp = gpar(...))
    
    return(res)
}

draw_rownames = function(rown, gaps, ...){
    coord = find_coordinates(length(rown), gaps)
    y = unit(1, "npc") - (coord$coord - 0.5 * coord$size)
    
    res = textGrob(rown, x = unit(3, "bigpts"), y = y, vjust = 0.5, hjust = 0, gp = gpar(...))
    
    return(res)
}

draw_legend = function(color, breaks, legend, ...){
    color = color[!is.infinite(breaks)]
    breaks = breaks[!is.infinite(breaks)]
    
    height = min(unit(1, "npc"), unit(150, "bigpts"))
    
    legend_pos = (legend - min(breaks)) / (max(breaks) - min(breaks))
    legend_pos = height * legend_pos + (unit(1, "npc") - height)
    
    breaks = (breaks - min(breaks)) / (max(breaks) - min(breaks))
    breaks = height * breaks + (unit(1, "npc") - height)
    
    h = breaks[-1] - breaks[-length(breaks)]
    
    rect = rectGrob(x = 0, y = breaks[-length(breaks)], width = unit(10, "bigpts"), height = h, hjust = 0, vjust = 0, gp = gpar(fill = color, col = "#FFFFFF00"))
    text = textGrob(names(legend), x = unit(14, "bigpts"), y = legend_pos, hjust = 0, gp = gpar(...))
    
    res = grobTree(rect, text)
    
    return(res)
}

convert_annotations = function(annotation, annotation_colors){
    new = annotation
    for(i in 1:ncol(annotation)){
        a = annotation[, i]
        b = annotation_colors[[colnames(annotation)[i]]]
        if(is.character(a) | is.factor(a)){
            a = as.character(a)
            
            if(length(setdiff(setdiff(a, NA), names(b))) > 0){
                stop(sprintf("Factor levels on variable %s do not match with annotation_colors", colnames(annotation)[i]))
            }
            new[, i] = b[a]
        }
        else{
            a = cut(a, breaks = 100)
            new[, i] = colorRampPalette(b)(100)[a]
        }
    }
    return(as.matrix(new))
}

draw_annotations = function(converted_annotations, border_color, gaps, fontsize, horizontal){
    n = ncol(converted_annotations)
    m = nrow(converted_annotations)
    
    coord_x = find_coordinates(m, gaps)
    
    x = coord_x$coord - 0.5 * coord_x$size
    
    # y = cumsum(rep(fontsize, n)) - 4 + cumsum(rep(2, n))
    y = cumsum(rep(fontsize, n)) + cumsum(rep(2, n)) - fontsize / 2 + 1 
    y = unit(y, "bigpts")
    
    if(horizontal){
        coord = expand.grid(x = x, y = y)
        res = rectGrob(x = coord$x, y = coord$y, width = coord_x$size, height = unit(fontsize, "bigpts"), gp = gpar(fill = converted_annotations, col = border_color))
    }
    else{
        a = x
        x = unit(1, "npc") - y
        y = unit(1, "npc") - a
        
        coord = expand.grid(y = y, x = x)
        res = rectGrob(x = coord$x, y = coord$y, width = unit(fontsize, "bigpts"), height = coord_x$size, gp = gpar(fill = converted_annotations, col = border_color))
    }
    
    return(res)
}

draw_annotation_names = function(annotations, fontsize, horizontal, hjust_col, vjust_col, angle_col){
    n = ncol(annotations)
    
    x = unit(3, "bigpts")
    
    y = cumsum(rep(fontsize, n)) + cumsum(rep(2, n)) - fontsize / 2 + 1 
    y = unit(y, "bigpts")
    
    if(horizontal){
        res = textGrob(colnames(annotations), x = x, y = y, hjust = 0, gp = gpar(fontsize = fontsize, fontface = 2))
    }
    else{
        a = x
        x = unit(1, "npc") - y
        y = unit(1, "npc") - a
        
        res = textGrob(colnames(annotations), x = x, y = y, vjust = vjust_col, hjust = hjust_col, rot = angle_col, gp = gpar(fontsize = fontsize, fontface = 2))
    }
    
    return(res)
}

draw_annotation_legend = function(annotation, annotation_colors, border_color, ...){
    y = unit(1, "npc")
    text_height = unit(1, "grobheight", textGrob("FGH", gp = gpar(...)))
    
    res = gList()
    for(i in names(annotation)){
        res[[i]] = textGrob(i, x = 0, y = y, vjust = 1, hjust = 0, gp = gpar(fontface = "bold", ...))
        
        y = y - 1.5 * text_height
        if(is.character(annotation[[i]]) | is.factor(annotation[[i]])){
            n = length(annotation_colors[[i]])
            yy = y - (1:n - 1) * 2 * text_height
            
            res[[paste(i, "r")]] = rectGrob(x = unit(0, "npc"), y = yy, hjust = 0, vjust = 1, height = 2 * text_height, width = 2 * text_height, gp = gpar(col = border_color, fill = annotation_colors[[i]]))
            res[[paste(i, "t")]] = textGrob(names(annotation_colors[[i]]), x = text_height * 2.4, y = yy - text_height, hjust = 0, vjust = 0.5, gp = gpar(...))
            
            y = y - n * 2 * text_height
            
        }
        else{
            yy = y - 8 * text_height + seq(0, 1, 0.25)[-1] * 8 * text_height
            h = 8 * text_height * 0.25
            
            res[[paste(i, "r")]] = rectGrob(x = unit(0, "npc"), y = yy, hjust = 0, vjust = 1, height = h, width = 2 * text_height, gp = gpar(col = NA, fill = colorRampPalette(annotation_colors[[i]])(4)))
            res[[paste(i, "r2")]] = rectGrob(x = unit(0, "npc"), y = y, hjust = 0, vjust = 1, height = 8 * text_height, width = 2 * text_height, gp = gpar(col = border_color, fill = NA))
            
            txt = rev(range(grid.pretty(range(annotation[[i]], na.rm = TRUE))))
            yy = y - c(1, 7) * text_height
            res[[paste(i, "t")]]  = textGrob(txt, x = text_height * 2.4, y = yy, hjust = 0, vjust = 0.5, gp = gpar(...))
            y = y - 8 * text_height
        }
        y = y - 1.5 * text_height
    }
    
    res = gTree(children = res)
    
    return(res)
}

draw_main = function(text, ...){
    res = textGrob(text, gp = gpar(fontface = "bold", ...))
    
    return(res)
}

vplayout = function(x, y){
    return(viewport(layout.pos.row = x, layout.pos.col = y))
}

heatmap_motor = function(matrix, border_color, cellwidth, cellheight, tree_col, tree_row, treeheight_col, treeheight_row, filename, width, height, breaks, color, legend, annotation_row, annotation_col, annotation_colors, annotation_legend, annotation_names_row, annotation_names_col, main, fontsize, fontsize_row, fontsize_col, hjust_col, vjust_col, angle_col, fmat, fontsize_number, number_color, gaps_col, gaps_row, labels_row, labels_col, ...){
    # Set layout
    lo = lo(coln = labels_col, rown = labels_row, nrow = nrow(matrix), ncol = ncol(matrix), cellwidth = cellwidth, cellheight = cellheight, treeheight_col = treeheight_col, treeheight_row = treeheight_row, legend = legend, annotation_col = annotation_col, annotation_row = annotation_row, annotation_colors = annotation_colors, annotation_legend = annotation_legend, annotation_names_row = annotation_names_row, annotation_names_col = annotation_names_col, main = main, fontsize = fontsize, fontsize_row = fontsize_row, fontsize_col = fontsize_col, angle_col = angle_col, gaps_row = gaps_row, gaps_col = gaps_col,  ...)
    
    res = lo$gt
    mindim = lo$mindim
    
    if(!is.na(filename)){
        if(is.na(height)){
            height = convertHeight(gtable_height(res), "inches", valueOnly = T)
        }
        if(is.na(width)){
            width = convertWidth(gtable_width(res), "inches", valueOnly = T)
        }
        
        # Get file type
        r = regexpr("\\.[a-zA-Z]*$", filename)
        if(r == -1) stop("Improper filename")
        ending = substr(filename, r + 1, r + attr(r, "match.length"))

        f = switch(ending,
            pdf = function(x, ...) pdf(x, ...),
            png = function(x, ...) png(x, units = "in", res = 300, ...),
            jpeg = function(x, ...) jpeg(x, units = "in", res = 300, ...),
            jpg = function(x, ...) jpeg(x, units = "in", res = 300, ...),
            tiff = function(x, ...) tiff(x, units = "in", res = 300, compression = "lzw", ...),
            bmp = function(x, ...) bmp(x, units = "in", res = 300, ...),
            stop("File type should be: pdf, png, bmp, jpg, tiff")
        )
        
        # print(sprintf("height:%f width:%f", height, width))
        
        # gt = heatmap_motor(matrix, cellwidth = cellwidth, cellheight = cellheight, border_color = border_color, tree_col = tree_col, tree_row = tree_row, treeheight_col = treeheight_col, treeheight_row = treeheight_row, breaks = breaks, color = color, legend = legend, annotation_col = annotation_col, annotation_row = annotation_row, annotation_colors = annotation_colors, annotation_legend = annotation_legend, filename = NA, main = main, fontsize = fontsize, fontsize_row = fontsize_row, fontsize_col = fontsize_col, fmat = fmat, fontsize_number =  fontsize_number, number_color = number_color, labels_row = labels_row, labels_col = labels_col, gaps_col = gaps_col, gaps_row = gaps_row, ...)

        f(filename, height = height, width = width)
        gt = heatmap_motor(matrix, cellwidth = cellwidth, cellheight = cellheight, border_color = border_color, tree_col = tree_col, tree_row = tree_row, treeheight_col = treeheight_col, treeheight_row = treeheight_row, breaks = breaks, color = color, legend = legend, annotation_col = annotation_col, annotation_row = annotation_row, annotation_colors = annotation_colors, annotation_legend = annotation_legend, annotation_names_row = annotation_names_row, annotation_names_col = annotation_names_col, filename = NA, main = main, fontsize = fontsize, fontsize_row = fontsize_row, fontsize_col = fontsize_col, hjust_col = hjust_col, vjust_col = vjust_col, angle_col = angle_col, fmat = fmat, fontsize_number =  fontsize_number, number_color = number_color, labels_row = labels_row, labels_col = labels_col, gaps_col = gaps_col, gaps_row = gaps_row, ...)
        grid.draw(gt)
        dev.off()
        
        return(gt)
    }
    
    # Omit border color if cell size is too small 
    if(mindim < 3) border_color = NA
    
    # Draw title
    if(!is.na(main)){
        elem = draw_main(main, fontsize = 1.3 * fontsize, ...)
        res = gtable_add_grob(res, elem, t = 1, l = 3, name = "main", clip = "off")
    }
    
    # Draw tree for the columns
    if(!is.na2(tree_col) & treeheight_col != 0){
        elem = draw_dendrogram(tree_col, gaps_col, horizontal = T)
        res = gtable_add_grob(res, elem, t = 2, l = 3, name = "col_tree")
    }
    
    # Draw tree for the rows
    if(!is.na2(tree_row) & treeheight_row != 0){
        elem = draw_dendrogram(tree_row, gaps_row, horizontal = F)
        res = gtable_add_grob(res, elem, t = 4, l = 1, name = "row_tree")
    }
    
    # Draw matrix
    elem = draw_matrix(matrix, border_color, gaps_row, gaps_col, fmat, fontsize_number, number_color)
    res = gtable_add_grob(res, elem, t = 4, l = 3, clip = "off", name = "matrix")
    
    # Draw colnames
    if(length(labels_col) != 0){
        pars = list(labels_col, gaps = gaps_col, fontsize = fontsize_col, hjust_col = hjust_col, vjust_col = vjust_col, angle_col = angle_col, ...)
        elem = do.call(draw_colnames, pars)
        res = gtable_add_grob(res, elem, t = 5, l = 3, clip = "off", name = "col_names")
    }
    
    # Draw rownames
    if(length(labels_row) != 0){
        pars = list(labels_row, gaps = gaps_row, fontsize = fontsize_row, ...)
        elem = do.call(draw_rownames, pars)
        res = gtable_add_grob(res, elem, t = 4, l = 4, clip = "off", name = "row_names")
    }
    
    # Draw annotation tracks on cols
    if(!is.na2(annotation_col)){
        # Draw tracks
        converted_annotation = convert_annotations(annotation_col, annotation_colors)
        elem = draw_annotations(converted_annotation, border_color, gaps_col, fontsize, horizontal = T)
        res = gtable_add_grob(res, elem, t = 3, l = 3, clip = "off", name = "col_annotation")
        
        # Draw names
        if(annotation_names_col){
            elem = draw_annotation_names(annotation_col, fontsize, horizontal = T)
            res = gtable_add_grob(res, elem, t = 3, l = 4, clip = "off", name = "col_annotation_names")
        }
    }
    
    # Draw annotation tracks on rows
    if(!is.na2(annotation_row)){
        # Draw tracks
        converted_annotation = convert_annotations(annotation_row, annotation_colors)
        elem = draw_annotations(converted_annotation, border_color, gaps_row, fontsize, horizontal = F)
        res = gtable_add_grob(res, elem, t = 4, l = 2, clip = "off", name = "row_annotation")
        
        # Draw names
        if(annotation_names_row){
            elem = draw_annotation_names(annotation_row, fontsize, horizontal = F, hjust_col = hjust_col, vjust_col = vjust_col, angle_col = angle_col)
            res = gtable_add_grob(res, elem, t = 5, l = 2, clip = "off", name = "row_annotation_names")
        }
    }
    
    # Draw annotation legend
    annotation = c(annotation_col[length(annotation_col):1], annotation_row[length(annotation_row):1])
    annotation = annotation[unlist(lapply(annotation, function(x) !is.na2(x)))]
    
    if(length(annotation) > 0 & annotation_legend){
        elem = draw_annotation_legend(annotation, annotation_colors, border_color, fontsize = fontsize, ...)
        
        t = ifelse(is.null(labels_row), 4, 3)
        res = gtable_add_grob(res, elem, t = t, l = 6, b = 5, clip = "off", name = "annotation_legend")
    }
    
    # Draw legend
    if(!is.na2(legend)){
        elem = draw_legend(color, breaks, legend, fontsize = fontsize, ...)
        
        t = ifelse(is.null(labels_row), 4, 3)
        res = gtable_add_grob(res, elem, t = t, l = 5, b = 5, clip = "off", name = "legend")
    }
    
    return(res)
}

generate_breaks = function(x, n, center = F){
    if(center){
        m = max(abs(c(min(x, na.rm = T), max(x, na.rm = T))))
        res = seq(-m, m, length.out = n + 1)
    }
    else{
        res = seq(min(x, na.rm = T), max(x, na.rm = T), length.out = n + 1)
    }
    
    return(res)
}

scale_vec_colours <- function(x, col = rainbow(10), breaks = NA, na_col){
  res <- col[as.numeric(cut(x, breaks = breaks, include.lowest = T))]
  res[is.na(res)] <- na_col
  return(res)
}

scale_colours = function(mat, col = rainbow(10), breaks = NA, na_col){
    mat = as.matrix(mat)
    return(matrix(scale_vec_colours(as.vector(mat), col = col, breaks = breaks, na_col = na_col), nrow(mat), ncol(mat), dimnames = list(rownames(mat), colnames(mat))))
}

cluster_mat = function(mat, distance, method){
    if(!(method %in% c("ward.D", "ward.D2", "ward", "single", "complete", "average", "mcquitty", "median", "centroid"))){
        stop("clustering method has to one form the list: 'ward', 'ward.D', 'ward.D2', 'single', 'complete', 'average', 'mcquitty', 'median' or 'centroid'.")
    }
    if(!(distance[1] %in% c("correlation", "euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")) & class(distance) != "dist"){
        stop("distance has to be a dissimilarity structure as produced by dist or one measure  form the list: 'correlation', 'euclidean', 'maximum', 'manhattan', 'canberra', 'binary', 'minkowski'")
    }
    if(distance[1] == "correlation"){
        d = as.dist(1 - cor(t(mat)))
    }
    else{
        if(class(distance) == "dist"){
            d = distance
        }
        else{
            d = dist(mat, method = distance)
        }
    }
    
    return(hclust(d, method = method))
}

scale_rows = function(x){
    m = apply(x, 1, mean, na.rm = T)
    s = apply(x, 1, sd, na.rm = T)
    return((x - m) / s)
}

scale_mat = function(mat, scale){
    if(!(scale %in% c("none", "row", "column"))){
        stop("scale argument shoud take values: 'none', 'row' or 'column'")
    }
    mat = switch(scale, none = mat, row = scale_rows(mat), column = t(scale_rows(t(mat))))
    return(mat)
}

generate_annotation_colours = function(annotation, annotation_colors, drop){
    if(is.na2(annotation_colors)){
        annotation_colors = list()
    }
    count = 0
    for(i in 1:length(annotation)){
        annotation[[i]] = annotation[[i]][!is.na(annotation[[i]])]
        if(is.character(annotation[[i]]) | is.factor(annotation[[i]])){
            if (is.factor(annotation[[i]]) & !drop){
                count = count + length(levels(annotation[[i]]))
            }
            else{
                count = count + length(unique(annotation[[i]]))
            }
        }
    }
    
    factor_colors = dscale(factor(1:count), hue_pal(l = 75))
    
    oldseed = NULL 
    if (exists(".Random.seed")) 
    oldseed = get(".Random.seed", pos=.GlobalEnv) 
    
    set.seed(3453)
    
    cont_counter = 2
    for(i in 1:length(annotation)){
        if(!(names(annotation)[i] %in% names(annotation_colors))){
            if(is.character(annotation[[i]]) | is.factor(annotation[[i]])){
                n = length(unique(annotation[[i]]))
                if (is.factor(annotation[[i]]) & !drop){
                    n = length(levels(annotation[[i]]))
                }
                ind = sample(1:length(factor_colors), n)
                annotation_colors[[names(annotation)[i]]] = factor_colors[ind]
                l = levels(as.factor(annotation[[i]]))
                l = l[l %in% unique(annotation[[i]])]
                if (is.factor(annotation[[i]]) & !drop){
                    l = levels(annotation[[i]])
                }
                
                names(annotation_colors[[names(annotation)[i]]]) = l
                factor_colors = factor_colors[-ind]
            }
            else{
                annotation_colors[[names(annotation)[i]]] = brewer_pal("seq", cont_counter)(5)[1:4]
                cont_counter = cont_counter + 1
            }
        }
    }
    
    if(!is.null(oldseed)){ 
        assign(".Random.seed", oldseed, pos=.GlobalEnv) 
    } 
    else{ 
        remove(.Random.seed, pos=.GlobalEnv) 
    }
    
    return(annotation_colors)
}

kmeans_pheatmap = function(mat, k = min(nrow(mat), 150), sd_limit = NA, ...){
    # Filter data
    if(!is.na(sd_limit)){
        s = apply(mat, 1, sd)
        mat = mat[s > sd_limit, ]    
    }
    
    # Cluster data
    set.seed(1245678)
    km = kmeans(mat, k, iter.max = 100)
    mat2 = km$centers
    
    # Compose rownames
    t = table(km$cluster)
    rownames(mat2) = sprintf("cl%s_size_%d", names(t), t)
    
    # Draw heatmap
    pheatmap(mat2, ...)
}

find_gaps = function(tree, cutree_n){
    v = cutree(tree, cutree_n)[tree$order]
    gaps = which((v[-1] - v[-length(v)]) != 0)
    
}

is.na2 = function(x){
    if(is.list(x) | length(x) > 1){
        return(FALSE)
    }
    if(length(x) == 0){
        return(TRUE)
    }
    
    return(is.na(x))
}

identity2 = function(x, ...){
    return(x)
}


#' @export
pheatmap = function(mat, color = colorRampPalette(rev(RColorBrewer::brewer.pal(n = 7, name = "RdYlBu")))(100), kmeans_k = NA, breaks = NA, border_color = "grey60", cellwidth = NA, cellheight = NA, scale = "none", cluster_rows = TRUE, cluster_cols = TRUE, clustering_distance_rows = "euclidean", clustering_distance_cols = "euclidean", clustering_method = "complete", clustering_callback = identity2, cutree_rows = NA, cutree_cols = NA,  treeheight_row = ifelse((class(cluster_rows) == "hclust") || cluster_rows, 50, 0), treeheight_col = ifelse((class(cluster_cols) == "hclust") || cluster_cols, 50, 0), legend = TRUE, legend_breaks = NA, legend_labels = NA, annotation_row = NA, annotation_col = NA, annotation = NA, annotation_colors = NA, annotation_legend = TRUE, annotation_names_row = TRUE, annotation_names_col = TRUE, drop_levels = TRUE, show_rownames = T, show_colnames = T, main = NA, fontsize = 10, fontsize_row = fontsize, fontsize_col = fontsize, angle_col = c("270", "0", "45", "90", "315"), display_numbers = F, number_format = "%.2f", number_color = "grey30", fontsize_number = 0.8 * fontsize, gaps_row = NULL, gaps_col = NULL, labels_row = NULL, labels_col = NULL, filename = NA, width = NA, height = NA, silent = FALSE, na_col = "#DDDDDD", ...){
    
    # Set labels
    if(is.null(labels_row)){
        labels_row = rownames(mat)
    }
    if(is.null(labels_col)){
        labels_col = colnames(mat)
    }
    
    # Preprocess matrix
    mat = as.matrix(mat)
    if(scale != "none"){
        mat = scale_mat(mat, scale)
        if(is.na2(breaks)){
            breaks = generate_breaks(mat, length(color), center = T)
        }
    }
    
    
    # Kmeans
    if(!is.na(kmeans_k)){
        # Cluster data
        km = kmeans(mat, kmeans_k, iter.max = 100)
        mat = km$centers
        
        # Compose rownames
        t = table(km$cluster)
        labels_row = sprintf("Cluster: %s Size: %d", names(t), t)
    }
    else{
        km = NA
    }
    
    # Format numbers to be displayed in cells
    if(is.matrix(display_numbers) | is.data.frame(display_numbers)){
        if(nrow(display_numbers) != nrow(mat) | ncol(display_numbers) != ncol(mat)){
            stop("If display_numbers provided as matrix, its dimensions have to match with mat")
        }
        
        display_numbers = as.matrix(display_numbers)
        fmat = matrix(as.character(display_numbers), nrow = nrow(display_numbers), ncol = ncol(display_numbers))
        fmat_draw = TRUE
        
    }
    else{
        if(display_numbers){
            fmat = matrix(sprintf(number_format, mat), nrow = nrow(mat), ncol = ncol(mat))
            fmat_draw = TRUE
        }
        else{
            fmat = matrix(NA, nrow = nrow(mat), ncol = ncol(mat))
            fmat_draw = FALSE
        }
    }
    
    # Do clustering
    if((class(cluster_rows) == "hclust") || cluster_rows){
        if(class(cluster_rows) == "hclust"){
            tree_row = cluster_rows
        } else {
            tree_row = cluster_mat(mat, distance = clustering_distance_rows, method = clustering_method)
            tree_row = clustering_callback(tree_row, mat)
        }
        mat = mat[tree_row$order, , drop = FALSE]
        fmat = fmat[tree_row$order, , drop = FALSE]
        labels_row = labels_row[tree_row$order]
        if(!is.na(cutree_rows)){
            gaps_row = find_gaps(tree_row, cutree_rows)
        }
        else{
            gaps_row = NULL
        }
    }
    else{
        tree_row = NA
        treeheight_row = 0
    }
    
    if((class(cluster_cols) == "hclust") || cluster_cols){
        if(class(cluster_cols) == "hclust"){
            tree_col = cluster_cols
        } else {
            tree_col = cluster_mat(t(mat), distance = clustering_distance_cols, method = clustering_method)
            tree_col = clustering_callback(tree_col, t(mat))
        }
        mat = mat[, tree_col$order, drop = FALSE]
        fmat = fmat[, tree_col$order, drop = FALSE]
        labels_col = labels_col[tree_col$order]
        if(!is.na(cutree_cols)){
            gaps_col = find_gaps(tree_col, cutree_cols)
        }
        else{
            gaps_col = NULL
        }
    }
    else{
        tree_col = NA
        treeheight_col = 0
    }
    
    attr(fmat, "draw") = fmat_draw
    
    # Colors and scales
    if(!is.na2(legend_breaks) & !is.na2(legend_labels)){
        if(length(legend_breaks) != length(legend_labels)){
            stop("Lengths of legend_breaks and legend_labels must be the same")
        }
    }
    
    
    if(is.na2(breaks)){
        breaks = generate_breaks(as.vector(mat), length(color))
    }
    
    if(!is.infinite(min(breaks))){
        breaks = c(-Inf, breaks)
        color = c(color[1], color)
    }
    if(!is.infinite(max(breaks))){
        breaks = c(breaks, Inf)
        color = c(color, color[length(color)])
    }
    non_inf_breaks = breaks[!is.infinite(breaks)]
    
    if (legend & is.na2(legend_breaks)) {
        legend = grid.pretty(range(as.vector(non_inf_breaks)))
        names(legend) = legend
    }
    else if(legend & !is.na2(legend_breaks)){
        legend = legend_breaks[legend_breaks >= min(non_inf_breaks) & legend_breaks <= max(non_inf_breaks)]
        
        if(!is.na2(legend_labels)){
            legend_labels = legend_labels[legend_breaks >= min(non_inf_breaks) & legend_breaks <= max(non_inf_breaks)]
            names(legend) = legend_labels
        }
        else{
            names(legend) = legend
        }
    }
    else {
        legend = NA
    }
    
    mat = scale_colours(mat, col = color, breaks = breaks, na_col = na_col)
    
    # Preparing annotations
    if(is.na2(annotation_col) & !is.na2(annotation)){
        annotation_col = annotation
    }
    # Select only the ones present in the matrix
    if(!is.na2(annotation_col)){
        annotation_col = annotation_col[colnames(mat), , drop = F]
    }
    
    if(!is.na2(annotation_row)){
        annotation_row = annotation_row[rownames(mat), , drop = F]
    }
    
    annotation = c(annotation_row, annotation_col)
    annotation = annotation[unlist(lapply(annotation, function(x) !is.na2(x)))]
    if(length(annotation) != 0){
        annotation_colors = generate_annotation_colours(annotation, annotation_colors, drop = drop_levels)
    }
    else{
        annotation_colors = NA
    }
    
    if(!show_rownames){
        labels_row = NULL
    }
    
    if(!show_colnames){
        labels_col = NULL
    }
    
    # Set colname rotating parameters
    angle_col = as.character(angle_col)
    angle_col = match.arg(angle_col)
    
    if(angle_col == "0"){
      angle_col = 0
      hjust_col = 0.5
      vjust_col = 1
    }
    if(angle_col == "45"){
      angle_col = 45
      hjust_col = 1
      vjust_col = 1
    }
    if(angle_col == "90"){
      angle_col = 90
      hjust_col = 1
      vjust_col = 0.5
    }
    if(angle_col == "270"){
      angle_col = 270
      hjust_col = 0
      vjust_col = 0.5
    }
    if(angle_col == "315"){
      angle_col = 315
      hjust_col = 0
      vjust_col = 1
    }
    
    # Draw heatmap
    pdf(file = NULL)
    gt = heatmap_motor(mat, border_color = border_color, cellwidth = cellwidth, cellheight = cellheight, treeheight_col = treeheight_col, treeheight_row = treeheight_row, tree_col = tree_col, tree_row = tree_row, filename = filename, width = width, height = height, breaks = breaks, color = color, legend = legend, annotation_row = annotation_row, annotation_col = annotation_col, annotation_colors = annotation_colors, annotation_legend = annotation_legend, annotation_names_row = annotation_names_row, annotation_names_col = annotation_names_col, main = main, fontsize = fontsize, fontsize_row = fontsize_row, fontsize_col = fontsize_col, hjust_col = hjust_col, vjust_col = vjust_col, angle_col = angle_col, fmat = fmat, fontsize_number = fontsize_number, number_color = number_color, gaps_row = gaps_row, gaps_col = gaps_col, labels_row = labels_row, labels_col = labels_col, ...)
    dev.off()
    
    if(is.na(filename) & !silent){
        grid.newpage()
        grid.draw(gt)
    }
    
    invisible(structure(list(tree_row = tree_row, tree_col = tree_col, kmeans = km, gtable = gt), class = "pheatmap"))
}




## 下面为 pheatmp的必须修改，否则在调取图形时会出现覆盖

#' @method grid.draw pheatmap
#' @export
grid.draw.pheatmap <- function(x, recording = TRUE) {
  grid::grid.newpage()
  grid::grid.draw(x$gtable)
}

#' @method print pheatmap
#' @export
print.pheatmap <- function(x, ...) {
  grid::grid.draw(x)
}
