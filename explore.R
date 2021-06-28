library(GGally)

explore <- function(df){
  # df %>%
  #   #mutate( thc_test_ints = cut(drugtests_meth_positive ,breaks =)) %>% 
  #   group_by(drugtests_meth_positive) %>%
  #   summarize(mean(avg_days_per_drugtest))
  print(summary(df))
  numeric_vars <- df %>% select(where(is.numeric)) 
  factor_vars <- df %>% select(where(is.factor)) 
  
  #correlation plots btw all types variables
  print(ggpairs(df, columns=c("percent_days_employed","employment_exempt")))
  
  
  #correlation plot between numeric variables
  print(numeric_vars %>% ggcorr())
  
  #anova tests for numeric variables across factor variables
  # does the analysis of variance
  res_anova <- lapply(names(numeric_vars),
                      function(x){
                        ret <- lapply(names(factor_vars),
                                      function(y){
                                        df <- data.frame(
                                          num = df[[x]],
                                          fac = df[[y]]
                                        )
                                        return(summary(aov(num ~ fac, data = df)))
                                      })
                        names(ret) <- names(factor_vars)
                        
                      })
  names(res_anova)<- names(numeric_vars)
  
}