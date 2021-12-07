modify_data=function(data,design, min_relative,min_odd) {
  otu_origin=data
  mapping=design
  # 将第一列转为行名
  rownames(otu_origin)<-otu_origin[,1]
  otu_origin<-otu_origin[,-1]
  
  # 过滤掉小于千分之一的数据
  otu_origin[otu_origin<min_relative]=0
  
  #先矩阵化，再倒置
  otu_t=as.data.frame(t(as.matrix(otu_origin)))
  
  # 修改第一列
  otu_t=data.frame( SampleID=rownames(otu_t),otu_t)
  rownames(otu_t)=c(1:nrow(otu_t))
  
  #与mapping文件合并，获得数据切分依据
  #注意这里的位置，以mapping为主表进行合并，纠正样本排序
  otu_merge=dplyr::inner_join(mapping,otu_t,by="SampleID")
  otu_group_split=split(otu_merge,otu_merge$Group)
  
  name_group=unique(mapping$Group)
  
  # 创建ID空集
  data_for_filter=list()
  
  for (j in 1:length(name_group)) {
    # 循环读取各组数据
    otu=otu_group_split[[j]]
    # 删除前面合并的不必要的数据
    try(otu<-subset(otu,select = -c(barcode,primer,Group,Description)),silent=T)
    try(otu<-subset(otu,select = -c(Group)),silent=T)
    # 更改数据名称为V，便于下游制图
    #colnames(otu)=c("SampleID",paste("V", 1:c(ncol(otu)-1), sep = ""))
    
    # 将第一列转换为rownames
    rownames(otu)<-otu[,1]
    otu<-otu[,-1]
    
    # 利用mapping文件获得此次分组名称
    id_group=mapping$SampleID%in%rownames(otu)
    
    group_name=mapping$Group[id_group]
    
    name=as.character(group_name[1])
    
    # 统计为0的菌种比例
    no_zero_odd=apply(otu, 2, function(m){sum(m!=0)/nrow(otu)})
    
    # 过滤掉低占比的菌种，可根据情况设置20%，30%
    otu=otu[,no_zero_odd>=min_odd]
    data_for_filter[[j]]=colnames(otu)
  }
  
  ### 根据每个单独过滤组的变量取并集，得到所需的序列
  idx=Reduce(union,data_for_filter)
  
  RF_filter=otu_merge[,idx]
  # 得到所需数据
  RF=data.frame(SampleID=otu_merge$SampleID,Group=otu_merge$Group,RF_filter)
  
  # 变量存储
  deposit=list()
  deposit$mapping_file=mapping
  deposit$otu_group_split=otu_group_split
  deposit$union_id=idx
  deposit$filtered_data=RF
  return(deposit)
}


data_filter=function(dir=NULL,data=NULL,min_relative,min_ratio,design,adjust=F,change=F,change_name='Other',output=F,pattern='L'){
  deposit <- list()
  mapping <- design
  # 消除mapping原有的因子等级，特别是sub_mapping的
  try(mapping$Group <- as.character(mapping$Group),silent=T)
  try(mapping$Group <- as.factor(mapping$Group),silent=T)
  try(mapping<-read.table(paste0(design),header = T,check.names = F,sep = "\t",stringsAsFactors = F),silent = T)
  mapping <- subset(mapping,select = c(SampleID,Group))

  raw_data <- data_input(dir=dir,data=data,pattern=pattern,change=change,change_name=change_name)

  for (tax_level in names(raw_data)){
    data <- raw_data[[tax_level]]
    colnames(data)[1] <- 'ID'
    tax_names <- data.frame(ID=paste0("V", 1:length(data$`ID`)),tax=data$`ID`)
    data$`ID` <- paste0("V", 1:nrow(data))
    
    # 根据mapping文件确认sample的存留与排序
    data2 <- data$`ID`
    for (k in mapping$SampleID) {
        data2 <- cbind(data2,data[paste0(k)])
    }
    colnames(data2)[1] <- c('ID')
    
    filter_result <- modify_data(data=data2,design = mapping,min_relative = min_relative,min_odd =min_ratio )
    sub_data <- filter_result$filtered_data

    
    # check empty data
    sub_data_check <- subset(sub_data,select=-c(SampleID,Group))
    if (ncol(sub_data_check) != 0) {
      id_na<-apply(sub_data_check,1,max)==0
      empty_data_name <- paste0(tax_level,'_empty_data')
      empty_data <- data.frame()
      try(empty_data <- sub_data[id_na,],silent=T)
      if (length(rownames(empty_data))>0) {
        warning('Empty data detected!')
        warning(paste0('input this to acquire more error information: View(<output>$empty_data$',empty_data_name,')'))
        deposit$empty_data[[paste0(empty_data_name)]]=empty_data
      }
      
      # 设置极小值校正特征 使空行可以继续
      if ((adjust==T)&(length(rownames(empty_data))>0)) {
        empty_ID <- sub_data$SampleID[id_na]
        empty_ID_adjust <- paste0(empty_ID,'_adjust')
        for (k in c(1:length(empty_ID_adjust))) {
          sub_data[empty_ID_adjust[k]] <- rep(0,nrow(sub_data))
          idx<-sub_data$SampleID==empty_ID[k]
          sub_data[empty_ID_adjust[k]][idx,] <- 0.00001
        }
      }
      
      if (output==T){
        suppressWarnings(try(dir.create('Filter_result'),silent=T))
        write.csv(sub_data, file =paste0(tax_level,'_',min_relative,"_",min_ratio*100,"%.csv"),row.names = F)
        write.csv(tax_names, file =paste0(tax_level,'_',min_relative,"_",min_ratio*100,"%_info.csv"))
        file_move(paste0(tax_level,'_',min_relative,"_",min_ratio*100,"%.csv"),'Filter_result')
        file_move(paste0(tax_level,'_',min_relative,"_",min_ratio*100,"%_info.csv"),'Filter_result')
      }
      # 取消数据的因子，减少后续分析因为因子水平所导致的问题
      sub_data$SampleID <- as.character(sub_data$SampleID)
      sub_data$Group <- as.character(sub_data$Group)

      deposit$filter_data[[paste0(tax_level)]] <- sub_data
      deposit$filter_data[[paste0(tax_level,'_ID')]] <- tax_names
    }else{
      print(paste0(file_name[i],' has been filtered into empty data, please reset the filter parameter !'))
      deposit$filter_data[[paste0(tax_level)]] <- 'empty data'
    }
  }
  
  return(deposit)
}

data_input=function(dir='.',data = NULL,pattern='',change=F,change_name='Other'){
  deposit <- list()
  sep_num <- c()
  tax_total <- c('phylum','class','order','family','genus','species')
  if (is.null(data)&!is.null(dir)) {
    file_name<-list.files(path =dir ,pattern = pattern)
    file_num <- length(file_name)
    for (i in c(1:file_num)){
      file_data <- data_format_check(paste0(dir,'/',file_name[i]),change = change,change_name = change_name)
      file_data <- file_data$relative
      sep_num[i] <- stringr::str_count(file_data[1,1],pattern = ';')
    }
    if (length(unique(sep_num))<file_num) {
      message('Data level can not match well, plz check files in different microbial level!')
      message('Duplicated level detcted!')
    }else{
      for (i in c(1:file_num)){
        tax_level <- tax_total[sep_num[i]]
        file_data <- data_format_check(paste0(dir,'/',file_name[i]),change = change,change_name = change_name)
        file_data <- file_data$relative
        deposit[[tax_level]] <- file_data
      }
    }
  }else if (inherits(data, "list")) {
    file_num <- length(data)
    for (i in c(1:file_num)){
      file_data <- data_format_check(file = data[[i]],change = change,change_name = change_name)
      file_data <- file_data$relative
      sep_num[i] <- stringr::str_count(file_data[1,1],pattern = ';')
    }
    if (length(unique(sep_num))<file_num) {
      message('Data level can not match well, plz check files in different microbial level!')
      message('Duplicated level detcted!')
    }else{
      for (i in c(1:file_num)){
        tax_level <- tax_total[sep_num[i]]
        file_data <- data_format_check(file = data[[i]],change = change,change_name = change_name)
        file_data <- file_data$relative
        deposit[[tax_level]]=file_data
      }
    }
  }else if (inherits(data, "data.frame")){
    file_data <- data_format_check(file = data,change = change,change_name = change_name)
    file_data <- file_data$relative
    sep_num <- stringr::str_count(file_data[1,1],pattern = ';')
    if (!sep_num%in%c(1:6)) {
      message('Data level can not match well, plz check files in different microbial level!')
    }else{
      tax_level=tax_total[sep_num]
      deposit[[tax_level]]=file_data
    }
  }
  return(deposit)
}  


data_format_check <- function(file,change=F,change_name='Other'){
  deposit <- list()
  # read the data
  if (is.character(file)) {
    if (stringr::str_detect(file,pattern='.csv')) {
      data <- read.csv(file,header = T,stringsAsFactors = F)
    }else if (stringr::str_detect(file,pattern='.txt')) {
      data <- read.table(file,header = T,sep = '\t',stringsAsFactors = F)
    }
  }else if(inherits(file, "data.frame")){
    data <- file
  }
  
  # check the file format 
  format_qiime2 <- stringr::str_detect(colnames(data)[3],pattern='Archaea')|stringr::str_detect(colnames(data)[3],pattern='Bacteria')|stringr::str_detect(colnames(data)[3],pattern='Eukaryota')|stringr::str_detect(colnames(data)[3],pattern='Unassigned')
  if (format_qiime2) {
    format_ab  <- sum(data[1,-1])>1
  }else{
    format_ab <- sum(data[,2])>1
  }
  
  # cacaulation
  if (format_qiime2) {
    colnames(data) <- gsub("[.]p__", ";p__", colnames(data))
    colnames(data) <- gsub("[.]c__", ";c__", colnames(data))
    colnames(data) <- gsub("[.]o__", ";o__", colnames(data))
    colnames(data) <- gsub("[.]f__", ";f__", colnames(data))
    colnames(data) <- gsub("[.]g__", ";g__", colnames(data))
    colnames(data) <- gsub("[.]s__", ";s__", colnames(data))
    colnames(data) <- gsub("[.]__", ";__", colnames(data))
    colnames(data) <- gsub("[.]Other", ";Other", colnames(data))
    
    rownames(data) <- data[,1]
    data <- data[,-1]
    
    data_f <- as.data.frame(t(data))
    if (change==T) {
      level_num <- stringr::str_count(rownames(data_f)[2],pattern = ';')+1
      pattern <- c('[k,d,Unassigned]__','p__','c__','o__','f__','g__','s__')[1:level_num]
      pattern <- c('[k__,d__,Unassigned]','p__','c__','o__','f__','g__','s__')[1:level_num]
      new_ID <- c()
      for (raw_name in rownames(data_f)) {
        level_check <- c()
        for (j in pattern) {
          level_check <- append(level_check,stringr::str_detect(raw_name, j))
        }
        check_num <- sum(rep(1,level_num)[!level_check])
        if (check_num != 0) {
          missing_level <- paste0(';',paste0(rep('__',check_num),collapse = ';'))
          pattern_correct <- paste0(';',paste0(tail(pattern,check_num),change_name,collapse = ';'))
          new_ID <- append(new_ID,gsub(missing_level,pattern_correct,raw_name))
        }else{
          new_ID <- append(new_ID,raw_name)
        }
      }
      rownames(data_f) <-new_ID
    }
    
    data_new <- data.frame(ID=rownames(data_f),data_f)
    rownames(data_new) <- 1:nrow(data_new)  
    if (format_ab) {
      data_f_relab <- as.data.frame(apply(data_f, 2, function(m){m/sum(m)}))
      data_relab <- data.frame(ID=rownames(data_f_relab),data_f_relab)
      rownames(data_relab) <- 1:nrow(data_relab)
      deposit$relative <- data_relab
      deposit$absolute <-  data_new
    }else{deposit$relative <-  data_new}
    return(deposit)
  }else{
    data_f <- data
    rownames(data_f) <- data_f[,1]
    data_f <- data_f[,-1]
    if (change==T) {
      level_num <- stringr::str_count(rownames(data_f)[1],pattern = ';')+1
      pattern <- c('[k,d]__','p__','c__','o__','f__','g__','s__')[1:level_num]
      new_ID <- c()
      for (raw_name in rownames(data_f)) {
        level_check <- c()
        for (j in pattern) {
          level_check <- append(level_check,stringr::str_detect(raw_name, j))
        }
        check_num <- sum(rep(1,level_num)[!level_check])
        if (check_num != 0) {
          missing_level <- paste0(';',paste0(rep('__',check_num),collapse = ';'))
          pattern_correct <- paste0(';',paste0(tail(pattern,check_num),change_name,collapse = ';'))
          new_ID <- append(new_ID,gsub(missing_level,pattern_correct,raw_name))
        }else{
          new_ID <- append(new_ID,raw_name)
        }
      }
      rownames(data_f) <-new_ID
    }
    data_new <- data.frame(ID=rownames(data_f),data_f)
    rownames(data_new) <- 1:nrow(data_new)  
    if (format_ab) {
      data_f_relab <- as.data.frame(apply(data_f, 2, function(m){m/sum(m)}))
      data_relab <- data.frame(ID=rownames(data_f_relab),data_f_relab)
      rownames(data_relab) <- 1:nrow(data_relab)
      deposit$relative <- data_relab
      deposit$absolute <-  data_new
    }else{
      deposit$relative <- data_new
    }
  }
  return(deposit)  
}
