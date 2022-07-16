

EMP_COR_SANKEY<- function(data_list,pvalue=0.05,rvalue=0,cor_method='spearman',sankey_ouput = F,file = 'Sankey.html' ,positive_col = 'steelblue',negtive_col = 'darkred',palette=c("#009E73","#F0E442","#E64B35FF","#CC79A7","#4DBBD5FF","#00A087FF","#3C5488FF","#F39B7FFF","#8491B4FF",
                                                                                         "#B2182B","#E69F00","#56B4E9","#0072B2","#D55E00","#CC6666")){
deposit <- list()
# contruct the relationship
total_data <- data_list
data_length <- length(total_data)
relationship_stock <- list()
count_id <- 1
relationship_stock <- list()
while (data_length>1) {
  if (count_id == 1 ) {
    temp <- rel_cons(data1 = total_data[[1]],data2 = total_data[[2]], 
                     pvalue = pvalue,rvalue = rvalue,cor_method = cor_method)
  }else{
    keep_idx <- relationship_stock[[count_id-1]]$keep
    temp <- rel_cons(data1 = total_data[[1]][,c('SampleID',keep_idx)], data2 = total_data[[2]], 
                     pvalue = pvalue,rvalue = rvalue,cor_method = cor_method)
  }
  if (length(temp$keep)>0) {
    relationship_stock[[count_id]] <- temp
    total_data <- total_data[-1]
    data_length <- data_length-1  
    count_id <- count_id+1
  }else{
    message('No proper relationship was constructed, please reset the parameters! ')
    break
  }
}



# filter the isolated node
kick_id <- kick_check(data = relationship_stock)
relationship_stock_filter <- relationship_stock
if (length(kick_id) != 0) {
while (length(kick_id >0)) {
  for (i in 1:length(relationship_stock_filter)) {
    idx_temp1 <- relationship_stock_filter[[i]]$relation$target %in% kick_id
    idx_temp2 <- relationship_stock_filter[[i]]$keep %in% kick_id
    relationship_stock_filter[[i]]$relation <- relationship_stock_filter[[i]]$relation[!idx_temp1,]
    relationship_stock_filter[[i]]$keep <- relationship_stock_filter[[i]]$keep[!idx_temp2]
  }
  kick_id=kick_check(data = relationship_stock_filter)
}
}

# combine the data
data_long <- c()
count_id <- length(relationship_stock_filter)
relationship_combind <- relationship_stock_filter
if (count_id == 1 ) {
  data_long <- relationship_stock_filter[[1]]$relation
}else{
while ( count_id >=2 ) {
  temp <- rbind(relationship_combind[[1]]$relation,relationship_combind[[2]]$relation)
  count_id=count_id-1
  relationship_combind=relationship_combind[-1]
  data_long <- rbind(data_long,temp)
}
}



deposit$sankey_data <- data_long

#set the edge
idx <- data_long$value<0
data_long$group[idx] <- 'negetive'
data_long$group[!idx] <- 'positive'
data_long$value=abs(data_long$value)*100

data_long$group=as.factor(data_long$group)
colnames(data_long)=c('source','target','value','group')

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(name=c(as.character(data_long$source), as.character(data_long$target)) %>% unique())





length(data_list)
group_name <- paste0("my_unique_group",1:length(data_list))

for (k in 1:length(data_list)) {
  nodes$group[nodes$name %in% colnames(data_list[[k]])[-1]] <- group_name[k]
}
nodes$group=as.factor(nodes$group)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
data_long$IDsource=match(data_long$source, nodes$name)-1 
data_long$IDtarget=match(data_long$target, nodes$name)-1


edge_col <- paste0(paste0('\"',c(positive_col,negtive_col),'\"'),collapse=',')
d3_node_name <- paste0(paste0('\"',group_name,'\"'),collapse=',')
d3_node_col <- paste0(paste0('\"',palette[1:length(group_name)],'\"'),collapse=',')


## origin method
## my_color <- 'd3.scaleOrdinal() .domain(["negitive", "positive","my_unique_group1","my_unique_group2","my_unique_group3","my_unique_group4"]) .range([ "steelblue", "darkred","#00A087FF","#CC79A7","#3C5488FF","#F0E442"])'
## use paste0 to copy this
my_color <- paste0('d3.scaleOrdinal() .domain(["negitive", "positive",',d3_node_name,']) .range([ ',edge_col,',',d3_node_col,'])')




p <- networkD3::sankeyNetwork(Links = data_long, Nodes = nodes, Source = "IDsource", Target = "IDtarget",
                Value = "value", NodeID = "name",sinksRight=F,LinkGroup="group",colourScale=my_color,NodeGroup="group")


if (sankey_ouput == T) {
  htmlwidgets::saveWidget(p, file=file) 
}

deposit$plot <- p

return(deposit)

}


rel_cons <- function(data1,data2,pvalue=0.05,rvalue=0,cor_method='spearman'){
  deposit=list()

  merge_data <- dplyr::inner_join(data1,data2,by='SampleID')
  
  data1_name <- colnames(data1)[-1]
  data2_name <- colnames(data2)[-1]
  
  
  rownames(merge_data)=merge_data[,1]
  merge_data=merge_data[,-1]
  
  
  data.corr <- psych::corr.test(merge_data[,data1_name], merge_data[,data2_name],method = cor_method,adjust='none')
  occor.r <- data.corr$r
  occor.p <- data.corr$p
  occor.r[occor.p>pvalue|abs(occor.r)<rvalue] = 0 
  
  data=as.data.frame(occor.r)
  
  
  data_long <-data %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column(var="source") %>% 
    tidyr::gather(key="target", value="value", -1) %>%
    dplyr::filter(value != 0)
  
  keep=unique(data_long$target)
  deposit$relation=data_long
  deposit$keep=keep
  
  return(deposit)
}


kick_check <- function(data){
  layer_length <- length(data)
  kick_id <- c()
  while( layer_length >= 2 ){
    kick_temp <- data[[1]]$keep[!data[[1]]$keep %in% unique(data[[2]]$relation$source)]
    kick_id <- append(kick_id,kick_temp)
    data <- data[-1]
    layer_length <- layer_length-1
  } 
  return(kick_id)
}


