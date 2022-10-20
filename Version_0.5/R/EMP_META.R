EMP_META <- function (data,estimate_group,missing_plot=T,width=20,height=20,new_col_id = 'Group',col_str=NULL,col_num=NULL,keep_col = 'SampleID',
                      regroup = T,clust_min=2,clust_max=15,clust_method='kmeans',
                      clust_dis="euclidean",silent=F){
	deposit <- list()
cat('
--------------------------------------------\n
         EMP_META WORKFLOW START\n
--------------------------------------------\n\n')	
 

cat('
--------------------------------------------\n
         META SUMMARY START\n
--------------------------------------------\n\n')	

tryCatch({ 
  meta_summary_re<-meta_summary(data=data,estimate_group=estimate_group,missing_plot=missing_plot,
                                  width=width,height=height)

cat('
--------------------------------------------\n
         META SUMMARY COMPLETE\n
--------------------------------------------\n\n')	  
  },error=function(e){message("Something is wrong :\n",conditionMessage(e),"\n")
cat('
--------------------------------------------\n
         META SUMMARY Fail\n
--------------------------------------------')},
 warning=function(a){message("Something is warning :\n",conditionMessage(a),"\n")
cat('
--------------------------------------------\n
         META SUMMARY Fail\n
--------------------------------------------')  })
deposit$summary <- meta_summary_re


cat('
--------------------------------------------\n
         META REGROUP START\n
--------------------------------------------\n\n')

 if (!is.null(col_str)&!is.null(col_num)) {	
tryCatch({ 
 

    meta_regroup_re <- meta_regroup(data = data, new_col_id = new_col_id,col_str = col_str,
                 col_num = col_num,
                 keep_col = keep_col,regroup = regroup,clust_min = clust_min,clust_max = clust_max)
    
cat('
--------------------------------------------\n
         META REGROUP COMPLETE\n
--------------------------------------------\n\n')    
    
  },error=function(e){message("Something is wrong :\n",conditionMessage(e),"\n")
cat('
--------------------------------------------\n
         META REGROUP Fail\n
--------------------------------------------')},
 warning=function(a){message("Something is warning :\n",conditionMessage(a),"\n")
cat('
--------------------------------------------\n
         META REGROUP Fail\n
--------------------------------------------')  })
}
deposit$regroup <- meta_regroup_re
cat('
--------------------------------------------\n
         EMP_META WORKFLOW OVER\n
--------------------------------------------\n\n')

return(deposit)

}