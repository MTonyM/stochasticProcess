MM1Simulation <- function(lambda = 2, rho = 1, print_every=2000, time = 5000, verbos = FALSE){
  digi <- 5
  mu <- lambda/rho
  simu_info <- paste("Arrival rate (lambda):",lambda,
                     "jobs/min,\nServer using rate (rho):",rho,
                     ",\nService rate (mu):",mu,
                     "jobs/min.\nSimulation time:",time,"min")
  out_log <- (" ========================== Setup ========================\n")
  out_log <- paste(out_log, simu_info,"\n")
  service_end_time <- -1
  i = 0
  total_wait <- 0
  total_time <- 0
  num_clients <- 0
  num_jobs <- rep(0, time)
  out_log = paste(out_log,"======================Begin Simulation==================\n")
  while ( i < time ){
    num_clients <- num_clients + 1
    if (i == 0 ){
      arrival_time <- round(x = rexp(1, lambda), digits = digi)
      start_time <- arrival_time
    }else{
      arrival_time = arrival_time + round(x = rexp(1, lambda), digits = digi)
      start_time <- max(arrival_time, service_end_time)
    }
    wait <-  start_time - arrival_time
    total_wait <- total_wait + wait
    service_time <- round(x = rexp(1,mu),digits = digi)
    # create clients
    total_time <- total_time + wait + service_time
    service_end_time <- service_time + start_time
    # add up the clients
    jStart = ceiling(arrival_time)
    jEnd = floor(service_end_time)
    if (jEnd >= jStart){
      for (j in seq(ceiling(arrival_time), floor(service_end_time))){
        num_jobs[j] <- num_jobs[j] + 1
      }
    }
    # output log
    i <- arrival_time
    if ( num_clients %% print_every == 0){
      log <- paste("arrival:", format(arrival_time, nsmall = digi),
                   "service:", format(service_time, nsmall = digi),
                   "start time:", format(start_time, nsmall = digi),
                   "end time:", format(service_end_time, nsmall = digi),
                   "wait:", format(wait, nsmall = digi))
      out_log <- paste(out_log, log, "\n")
    }
  }
  num_jobs <- num_jobs[1:time]
  out_log <- paste(out_log,"==========================Summary=======================\n")
  true <- paste("idealwait:",round(rho/(mu-lambda),digits = digi),",idealresp:",round(1/(mu-lambda),digits = digi))
  concl <- paste("mean wait:",round(total_wait/num_clients,digits = digi),",mean resp:",round(total_time/num_clients,digits = digi))
  out_log <- paste(out_log,true,"\n",concl,"\n",
                   "total clients:",num_clients,"\n",
                   "ideal mean of people in system:",round(rho/(1-rho),digits = digi),"\n",
                   "simulation of people in system:",round(mean(num_jobs),digits = digi),"\n",
                   "ideal variance of people in system:",round(rho /(1-rho)^2,digits = digi),"\n",
                   "simulation var of people in system:",round(var(num_jobs),digits = digi))
  if(verbos){
    cat(out_log)
  }
  return(num_jobs)
}

library(ggplot2)
sim04 <- MM1Simulation(lambda = 2, rho = 0.4, verbos = TRUE)
ggplot(data= NULL, aes(x = c(1:length(sim04)), y = sim04)) +
  geom_point(color = "darkred") +
  annotate("text",x = 2500, y = 10, label = "[rho] = 0.4") 