transform_training_data <- function(x){
  x$recidive <- rep(0, N)
  
  x_recidive_y0 <- x
  x_recidive_y0$tstart <- 0
  x_recidive_y0$tstop <- 1
  x_recidive_y0$recidive <- 0
  x_recidive_y0$enum <- 1
  x_recidive_y0$futime <- time_span * 3
  
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
  
  x_recidive_y0$futime[idx_recidive_y1] <- time_span
  x_recidive_y0$tstop[idx_recidive_y1] <- 1
  x_recidive_y1 <-
    x_recidive_y0[idx_recidive_y1, ]
  x_recidive_y1$tstart <- 1
  x_recidive_y1$tstop <- time_span * 2
  x_recidive_y1$recidive <- 1
  x_recidive_y1$enum <- 2
  x_recidive_y1$futime <- time_span * 2
  
  x_recidive_y0$futime[idx_recidive_y2] <- time_span * 2
  x_recidive_y0$tstop[idx_recidive_y2] <- 1
  x_recidive_y2 <-
    x_recidive_y0[idx_recidive_y2, ]
  x_recidive_y2$tstart <- 1
  x_recidive_y2$tstop <- time_span * 3
  x_recidive_y2$recidive <- 1
  x_recidive_y2$enum <- 2
  x_recidive_y2$futime <- time_span * 3
  
  x_recidive_y0$futime[idx_recidive_y3] <- time_span * 3
  x_recidive_y0$tstop[idx_recidive_y3] <- 1
  x_recidive_y3 <-
    x_recidive_y0[idx_recidive_y3, ]
  x_recidive_y3$tstart <- 1
  x_recidive_y3$tstop <- time_span * 4
  x_recidive_y3$recidive <- 1
  x_recidive_y3$enum <- 2
  x_recidive_y3$futime <- time_span * 4
  
  x_recidive_y0$futime[idx_not_recidive] <- time_span * 3
  x_recidive_y0$tstop[idx_not_recidive] <- 1
  x_not_recidive <-
    x_recidive_y0[idx_not_recidive, ]
  x_not_recidive$tstart <- 1
  x_not_recidive$tstop <- time_span * 4
  x_not_recidive$recidive <- 0
  x_not_recidive$enum <- 2
  x_not_recidive$futime <- time_span * 4
  
  #set the supervision convariates to NA for the first time period (0-1 days),
  #assume you measure those covariates only at time futime.
  
  
  x_ok <-
    rbind(
      x_recidive_y0,
      x_recidive_y1,
      x_recidive_y2,
      x_recidive_y3,
      x_not_recidive
    )
  x_ok[1:N, x_supervision_numeric] <- 0
  x_ok[1:N, x_supervision_factor] <- "false"
  
  x_ok <- arrange(x_ok, id, tstart)
  return(x_ok)
}