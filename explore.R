library(GGally)
#mice vignette https://www.gerkovink.com/miceVignettes/Ad_hoc_and_mice/Ad_hoc_methods.html
library(mice)
# for aggr missing plot
library(VIM)
explore <- function(df){
# df %>%
#   #mutate( thc_test_ints = cut(drugtests_meth_positive ,breaks =)) %>% 
#   group_by(drugtests_meth_positive) %>%
#   summarize(mean(avg_days_per_drugtest))

numeric_vars <- df %>% select(where(is.numeric)) 
factor_vars <- df %>% select(where(is.factor)) 

numeric_vars_with_missings <- numeric_vars %>% select(where(has_missings)) 
factor_vars_with_missings <- factor_vars %>% select(where(has_missings)) 

#missing pattern
mice_plot_numeric_vars <- aggr(numeric_vars_with_missings, col=c('navyblue','yellow'),
                               numbers=TRUE, sortVars=TRUE,
                               labels=names(numeric_vars), cex.lab= 1,
                               gap=2, ylab=c("Missing data","Pattern"),
                                               oma = c(15,5,5,3))

mice_plot_factor_vars <- aggr(factor_vars_with_missings, col=c('navyblue','yellow'),
                              numbers=TRUE, sortVars=TRUE,
                              labels=names(factor_vars),cex.lab= 1,
                              gap=2, ylab=c("Missing data","Pattern"),
                                              oma = c(15,5,5,3))

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


numeric_vars_no_missings <- numeric_vars %>% select(where(no_missings)) 
factor_vars_no_missings <- factor_vars %>% select(where(no_missings)) 

vars_no_missings <- cbind(numeric_vars_no_missings, factor_vars_no_missings)
imputed_percent_days_employed <- mice(cbind(df$percent_days_employed,vars_no_missings), m=5, maxit = 50, method = 'pmm', seed = 500)
densityplot(df$percent_days_employed)
densityplot(complete(imputed_percent_days_employed)[1][,1])
summary(df$percent_days_employed)
summary(complete(imputed_percent_days_employed)[1][,1])
df$percent_days_employed <- complete(imputed_percent_days_employed)[1][,1]
return(df)
}