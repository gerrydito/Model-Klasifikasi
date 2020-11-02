PipeOpBinning = R6::R6Class("PipeOpBinning",
                            inherit = PipeOpTaskPreproc,
                            public = list(
                              initialize = function(id = "binning", param_vals = list()) {
                                ps = ParamSet$new(params = list(
                                  ParamUty$new("method" ,default = "freq", tags = c("train", "binning")),
                                  ParamUty$new("breaks_list",default = NULL,tags = c("train", "binning"))
                                ))
                                super$initialize(id = id, param_set = ps, param_vals = param_vals,packages = "scorecard" )
                              }
                            ),
                            private = list(
                              
                              
                              .train_task = function(task) {
              
                                numeric_features = task$feature_types$type %in% c("numeric","integer")
                                cols = private$.select_cols(task)[numeric_features]
                                dt = task$data()#[,c(cols,task$target_names),with=F]
                                binn = scorecard::woebin(dt,y=task$target_names,
                                                         positive = task$positive,x = cols,
                                              method = self$param_set$get_values(tags = "binning")$method,
                                              breaks_list = self$param_set$get_values(tags = "binning")$break_list
                                              )
                                
                                self$state = list(
                                  bins = binn
                                )
                
                                bins_conv=scorecard::woebin_ply(dt,bins = binn,to = "bin") 
                                bins_conv=dplyr::mutate_if(bins_conv,is.character,as.factor)
                                task$cbind(bins_conv)$
                                  select(setdiff(c(task$feature_names,names(bins_conv)),
                                                 c(cols,task$target_names)))
                                task
                              },
                              
                              .predict_task = function(task) {
                                dt = task$data()
                                bins_conv=scorecard::woebin_ply(dt,bins=self$state$bins,to="bin")
                                bins_conv=dplyr::mutate_if(bins_conv,is.character,as.factor)
                                task$cbind(bins_conv)$
                                  select(setdiff(names(bins_conv),task$target_names))
                                task
                                
                              }
                            )
                            
                            
                            
)

mlr_pipeops$add("binning", PipeOpBinning)
