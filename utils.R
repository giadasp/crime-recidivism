dichomotize_perc <- function(x){
  if(is.na(x)){
    return("not tested")
  }else if(x>0){
    return("at least one positive")
  }else{
    return("all negative")
  }
}
dichomotize_perc_col <- Vectorize(dichomotize_perc,vectorize.args = "x")

factorize_var <- function(df, var = "x") {
  return(mutate_at(df, vars(var), factor))
}

no_missings<- function(x){
  if(any(is.na(x))){
    return(FALSE)
  }else{
    return(TRUE)
  }
}

has_missings<- function(x){
  !no_missings(x)
}
