EMP_COR_FIT <- function(data,meta = NULL,var_select,formula = y~poly(x,1,raw = T),eq_size=3,se = F,group = F,width = 5, height = 5,palette=c("#E64B35FF","#4DBBD5FF","#00A087FF","#3C5488FF","#F39B7FFF","#8491B4FF",
                                                                                                                                       "#B2182B","#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7","#CC6666")){
  deposit <- list()
  if (!is.null(meta)) {
    data <- dplyr::inner_join(data,meta,by = 'SampleID')
    data <- na.omit(data)
  }
  
  x <- data[,var_select[1]]
  y <- data[,var_select[2]]
  
  # 确保方程不会被点覆盖
  ymax <- max(data[,var_select[2]])*1.1
  ymin <- min(data[,var_select[2]])*0.9
  
  
  if (group == F) {
    p_fit <- ggplot2::ggplot(data=data,aes(x=data[,var_select[1]],y=data[,var_select[2]]))+ggiraph::geom_jitter_interactive(aes(tooltip = paste0(SampleID,'\n','x: ',data[,var_select[1]],'\n','y: ',data[,var_select[2]])),position = position_jitter(height = .00000001))+
      ggpmisc::stat_poly_line(formula = formula,se=F,colour='red')+
      theme_bw() +  
      xlab(paste0(var_select[1])) + ylab(paste0(var_select[2])) +
      ggpmisc::stat_poly_eq(formula = formula, 
                            aes(label = paste(after_stat(eq.label), "*\", \"*", 
                                              after_stat(rr.label), "*\", \"*", 
                                              after_stat(adj.rr.label), "*\", and \"*",
                                              after_stat(p.value.label), "*\".\"",
                                              sep = "")
                            ),size=eq_size,
                            parse = TRUE) + coord_cartesian(ylim=c(ymin,ymax))
    
  }else if(group == T){
    
    ymax <- ymax*1.3
    p_fit <- ggplot(data=data,aes(x=data[,var_select[1]],y=data[,var_select[2]],colour = Group))+ggiraph::geom_jitter_interactive(aes(tooltip = paste0(SampleID,'\n','x: ',data[,var_select[1]],'\n','y: ',data[,var_select[2]])),position = position_jitter(height = .00000001),size=3)+
      ggpmisc::stat_poly_line(formula = formula,se=F)+  theme_bw() +  
      xlab(paste0(var_select[1])) + ylab(paste0(var_select[2])) +
      ggpmisc::stat_poly_eq(formula = formula, vstep = NULL,
                            aes(label = paste(after_stat(eq.label), "*\", \"*", 
                                              after_stat(rr.label), "*\", \"*", 
                                              after_stat(adj.rr.label), "*\", and \"*",
                                              after_stat(p.value.label), "*\".\"",
                                              sep = "")
                                ),size=eq_size,
                            parse = TRUE) + coord_cartesian(ylim=c(ymin,ymax))+scale_colour_manual(values=palette,name = "Group")
    
  }
  

  
  p_fit_html <- ggiraph::girafe(code = print(p_fit),width = width,height = height)
  
  deposit$pic <- p_fit
  deposit$html <- p_fit_html
  return(deposit)
}
