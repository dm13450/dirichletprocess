source("graph_theme.R")

weibull_censor_generate <- function(){
  #Figure 9
  Likelihood.weibullcens <- function(mdobj, x, theta){
    a = theta[[1]][,,,drop=TRUE]
    b = theta[[2]][,,,drop=TRUE]

    y <- as.numeric(
      b^(-1) * a * x[,1]^(a-1) * exp(-b^(-1) * x[, 1]^a)
    )
    y_cens <- as.numeric(1 - exp(-x[,1]^a / b))

    if(nrow(x) == 1){
      if(x[,2] == 0) return(y)
      if(x[,2] == 1) return(y_cens)
    }
    else{
      y_ret <- y
      y_ret[x[, 2] == 1] <- y_cens[x[, 2]==1]
      return(y_ret)
    }
  }

  mdobjA <- MixingDistribution("weibullcens",
                               c(1,2,0.5), "nonconjugate",
                               mhStepSize=c(0.11,0.11),
                               hyperPriorParameters=c(2.222, 2, 1, 0.05))
  mdobjB <- MixingDistribution("weibullcens",
                               c(1,2,0.5), "nonconjugate",
                               mhStepSize=c(0.11,0.11),
                               hyperPriorParameters=c(2.069, 2, 1, 0.08))

  class(mdobjA) <- c("list", "weibullcens", "weibull", "nonconjugate")
  class(mdobjB) <- c("list", "weibullcens", "weibull", "nonconjugate")

  data_a <- c(1, 3 ,3, 6, 7, 7, 10, 12, 14, 15, 18 ,19, 22 ,26 , 28 , 29 ,34, 40, 48 ,49)
  data_a <- 1 + (data_a / max(data_a))
  data_a <- cbind(data_a, c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,1))
  data_b <- c(1, 1, 2, 2,3,4,5,8,8,9,11,12,14,16,18,21,27,31,38, 44)
  data_b <- 1 + (data_b / max(data_b))
  data_b <- cbind(data_b, c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0))


  dpA <- DirichletProcessCreate(data_a, mdobjA, c(2, 0.9))
  dpA <- Initialise(dpA)

  dpB <- DirichletProcessCreate(data_b, mdobjB, c(2, 0.9))
  dpB <- Initialise(dpB)

  its <- 15000

  dpA <- Fit(dpA, its, TRUE)
  dpB <- Fit(dpB, its, TRUE)



  index_chain <- seq(5000, its, by=1)

  PosteriorFunction_list = lapply(index_chain, function(i) PosteriorClusters(dpA, i))
  PosteriorFunction_list_b = lapply(index_chain, function(i) PosteriorClusters(dpB, i))

  kottas_weibull <- function(x, theta) {
    alpha = theta[[1]][,,,drop=TRUE]
    lambda = theta[[2]][,,,drop=TRUE]

    y <- lambda^(-1) * alpha * x^(alpha-1) * exp(-lambda^(-1) * x^alpha)
    return(y)
  }

  kottas_weibull_survival <- function(x, theta){
    alpha = theta[[1]][,,,drop=TRUE]
    lambda = theta[[2]][,,,drop=TRUE]

    y <-  1-exp(-x^alpha / lambda)
    return(y)
  }


  kottas_weibull_hazard <- function(x, theta){

    y <- kotta_weibull(x, theta)/kottas_weibull_survival(x, theta)
    return(y)
  }

  x_grid = seq(1, 3.5, by=0.001)

  treat_a_mean_density <- rowMeans(data.frame(lapply(PosteriorFunction_list, function(x) weighted_function_generator(kottas_weibull, x$weights, x$params)(x_grid))))
  treat_b_mean_density <- rowMeans(data.frame(lapply(PosteriorFunction_list_b, function(x) weighted_function_generator(kottas_weibull, x$weights, x$params)(x_grid))))

  treat_a_mean_survival <- rowMeans(data.frame(lapply(PosteriorFunction_list, function(x) weighted_function_generator(kottas_weibull_survival, x$weights, x$params)(x_grid))))
  treat_b_mean_survival <- rowMeans(data.frame(lapply(PosteriorFunction_list_b, function(x) weighted_function_generator(kottas_weibull_survival, x$weights, x$params)(x_grid))))

  treat_a_hazard <- treat_a_mean_density / (1-treat_a_mean_survival)
  treat_b_hazard <- treat_b_mean_density / (1-treat_b_mean_survival)

  x_grid_a <- (x_grid - 1)*49
  x_grid_b <- (x_grid - 1)*44

  treatmeant_df_a <- data.frame(A_Density=treat_a_mean_density, A_Survival=1-treat_a_mean_survival, A_Hazard=treat_a_hazard, x=x_grid_a)

  treatmeant_df_b <- data.frame(B_Density=treat_b_mean_density, B_Survival=1-treat_b_mean_survival, B_Hazard=treat_b_hazard, x=x_grid_b)

  treatmeant_df <- bind_rows(gather(treatmeant_df_a, label, value, -x), gather(treatmeant_df_b, label, value, -x))

  treatmeant_df$Group = sapply(treatmeant_df$label, function(x) strsplit(x, "_")[[1]][1])
  treatmeant_df$Function = sapply(treatmeant_df$label, function(x) strsplit(x, "_")[[1]][2])

  treatmeant_df %>% filter(Function == c("Density", "Survival")) %>% ggplot(aes(x=x, y=value, colour=Group, group=Group)) + geom_line() + facet_wrap(~Function, scales = "free") -> weibull_censor_graph

  weibull_censor_graph + xlim(c(0, 60)) -> weibull_censor_graph

  ggsave("weibull_censor_graph.pdf", weibull_censor_graph, width=5, height = 3, units="cm", device="pdf")

  print(weibull_censor_graph)

}
