transform_test_data <- function(x){
  N <- nrow(x)
 
  idx_recidive_y1 <-
    which(x$recidivism_arrest_year1 == "true")
  N_recidive_y1 <- length(idx_recidive_y1)
  idx_recidive_y2 <-
    which(x$recidivism_arrest_year2 == "true")
  N_recidive_y2 <- length(idx_recidive_y2)
  idx_recidive_y3 <-
    which(x$recidivism_arrest_year3 == "true")
  N_recidive_y3 <- length(idx_recidive_y3)
  idx_not_recidive <-
    setdiff(1:N, c(idx_recidive_y1, idx_recidive_y2, idx_recidive_y3))
  N_not_recidive <- length(idx_not_recidive)
  
  #transform dataset
  x$recidive <- 0
  
  x_y0 <- x
  x_y0$tstart <- rep(0, N)
  x_y0$tstop <- rep(1, N)
  x_y0$enum <- rep(1, N)
  x_y0$futime <- rep(time_span * 4, N)
  
  x_y1 <- x
  x_y1$tstart <- 1
  x_y1$tstop <- time_span * 2
  x_y1$enum <- 2
  x_y1$futime <- time_span * 4
  x_y1[idx_recidive_y1,]$recidive <- 1
  
  x_y2 <- x
  x_y2$tstart <- 1
  x_y2$tstop <- time_span * 3
  x_y2$enum <- rep(3, N)
  x_y2$futime <- time_span * 4
  x_y2[idx_recidive_y2,]$recidive <- 1
  
  x_y3 <- x
  x_y3$tstart <- 1
  x_y3$tstop <- time_span * 4
  x_y3$enum <- rep(4, N)
  x_y3$futime <- time_span * 4
  x_y3[idx_recidive_y3,]$recidive <- 1
  
  #set the supervision convariates to NA for the first time period (0-1 days),
  #assume you measure those covariates only at time futime.
  
  x_ok <-
    rbind(
      x_y0,
      x_y1,
      x_y2,
      x_y3
    )
  x_ok[1:N, x_supervision_numeric] <- 0
  x_ok[1:N, x_supervision_factor] <- "false"
  
  x_ok <- arrange(x_ok, id, tstart)
  return(x_ok)
}