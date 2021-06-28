#mice vignette https://www.gerkovink.com/miceVignettes/Ad_hoc_and_mice/Ad_hoc_methods.html
library(mice)
# for aggr missing plot
library(VIM)
missing_imputation <- function(df, var=""){
  
  numeric_vars <- df %>% select(where(is.numeric)) 
  factor_vars <- df %>% select(where(is.factor)) 
  
  numeric_vars_with_missings <- numeric_vars %>% select(where(has_missings)) 
  cat("Numeric vars ", names(numeric_vars_with_missings), " have missings.\n")
  factor_vars_with_missings <- factor_vars %>% select(where(has_missings)) 
  cat("Factor vars ", names(factor_vars_with_missings), " have missings.\n")
  
  
  #missing pattern
  if( length(names(numeric_vars_with_missings))>0){
    mice_plot_numeric_vars <- aggr(numeric_vars_with_missings, col=c('navyblue','yellow'),
                                   numbers=TRUE, sortVars=TRUE,
                                   labels=names(numeric_vars), cex.lab= 1,
                                   gap=2, ylab=c("Missing data","Pattern"),
                                   oma = c(15,5,5,3))
    
  }
  if(length(names(factor_vars_with_missings))>0){
    mice_plot_factor_vars <- aggr(factor_vars_with_missings, col=c('navyblue','yellow'),
                                  numbers=TRUE, sortVars=TRUE,
                                  labels=names(factor_vars),cex.lab= 1,
                                  gap=2, ylab=c("Missing data","Pattern"),
                                  oma = c(15,5,5,3))
    
  }
  numeric_vars_no_missings <- numeric_vars %>% select(where(no_missings)) 
  
  factor_vars_no_missings <- factor_vars %>% select(where(no_missings)) 

  if(var!=""){
    if(length(factor_vars_no_missings)>0 | length(numeric_vars_no_missings)>0){
      
      if(length(factor_vars_no_missings)>0){
        vars_no_missings <- factor_vars_no_missings
        if(length(numeric_vars_no_missings)>0){
          vars_no_missings <- cbind(vars_no_missings, numeric_vars_no_missings)
        }
      }else if(length(factor_vars_no_missings)>0){
        vars_no_missings <- numeric_vars_no_missings
      }
      
      imputed_var<- mice(cbind(df[var],vars_no_missings), m=5, maxit = 50, method = 'pmm', seed = 500)
      densityplot(df[var], title=paste0(var, " original with missings"))
      densityplot(complete(imputed_var)[1][,1], title=paste0(var, " imputed"))
      summary(df[var])
      summary(complete(imputed_var)[1][,1])
      df[[var]] <- complete(imputed_var)[1][,1]
    }
  }
  return(df)
}