metric_percent_imbalance <- function(times, max=NULL){
    max_util <- max(times)
    mean_util <- mean(times)
    pi <- ((max_util / mean_util) - 1)
    if(!is.null(max)){
      pi <- pi/((max / mean_util))
    }
    return(pi)
}

metric_imbalance_percentage  <- function(times){
    max_util <- max(times)
    mean_util <- mean(times)
    n = length(times)
    ip = ((max_util - mean_util) / max_util ) * (n/(n-1)) 
    return(ip)
}

metric_imbalance_time  <- function(times, max=NULL){
    max_util <- max(times)
    mean_util <- mean(times)
    n = length(times)
    it = max_util - mean_util
    if(!is.null(max)){
      it <- it/max
    }
    return(it)
}

metric_imbalance_std  <- function(times, max=NULL){
    std = sd(times)
    if(!is.null(max)){
      std <- std/max
    }
    return(std)
}

metric_imbalance_norm  <- function(times, max=NULL){
   norm <- mean(abs(times))
   if(!is.null(max)){
      norm <- norm/max
    }
   return(norm)
}
