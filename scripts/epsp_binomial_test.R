# Taisuke Yasuda
#
# This file contains unit tests for epsp_binomial.R.  

setwd("~/Dropbox/carnegie_mellon/research/neuro-summer-2017/")
library(ggplot2)
source("./scripts/epsp_binomial_lib.R")

tasks.to.run = c(2)

# Task 1: Testing the MOME estimates by simulating from the model and then 
# estimating it. 

if (1 %in% tasks.to.run) {
  cat("################################################################\n")
  cat("Task 1: Sample and evaluate MOME estimates.\n")
  cat("################################################################\n\n")
  
  Task1 <- function(theta, N, n, filename) {
    # Tests the MOME estimates by simulating from the model and then estimating
    # from the sample along with a bootstrap estiamte of confidence intervals. 
    # 
    # Args:
    #   theta: Parameter estimates.
    #   N: Assumed number of synaptic contacts.
    #   n: Number of samples to be taken. 
    #   filename: File name for saving the plots. 
    #   
    #  Returns:
    #   theta.hat: MOME estimates.
    #   ci: Confidence intervals. 
    
    cat("N =", N, "\n")
    cat("Model parameters: (mu, sigma, p) = (", theta$mu, ", ", theta$sigma, 
        ", ", theta$p, ")\n", sep="")
    cat("Trials sampled:", n, "\n")
    x <- EPSP.Binomial(theta, N, n)
    theta.hat <- MOME.Binomial(x, N)
    cat("Estimated parameters: (mu, sigma, p) = (", theta.hat$mu, ", ", 
        theta.hat$sigma, ", ", theta.hat$p, ")\n", sep="")
    ci <- Bootstrap.CI.Binomial(theta.hat, N, n, B=100)
    cat("Confidence intervals: (mu, sigma, p) = ([", ci$mu[1], ", ", 
        ci$mu[2], "], [", ci$sigma[1], ", ", ci$sigma[2], "], [", 
        ci$p[1], ", ", ci$p[2], "])\n", sep="")
    # Plot the result if filename is specified
    if (!missing(filename)) {
      df <- data.frame(param=c("mu", "mu", "sigma", "sigma", "p", "p"), 
                       type=c("act", "est", "act", "est", "act", "est"), 
                       val=c(theta$mu, theta.hat$mu, theta$sigma, 
                             theta.hat$sigma, theta$p, theta.hat$p),
                       ymin=c(theta$mu, ci$mu[1], theta$sigma, ci$sigma[1], 
                              theta$p, ci$p[1]), 
                       ymax=c(theta$mu, ci$mu[2], theta$sigma, ci$sigma[2], 
                              theta$p, ci$p[2]))
      plot <- ggplot(df, aes(color=type))
      plot <- plot + geom_point(aes(x=param, y=val))
      plot <- plot + geom_errorbar(aes(x=param, ymin=ymin, ymax=ymax))
      ggsave(filename)
    }
    return(list("theta.hat"=theta.hat, "ci"=ci))
  }
  
  PlotResults1 <- function(results, filename.mu, filename.sigma, filename.p) {
    # Plots the test results. 
    # 
    # Args:
    #   results: List of lists containing the theta.hat and ci. 
    #   filename.mu: Filename to save the mu results.
    #   filename.sigma: Filename to save the sigma results. 
    #   filename.p: Filename to save the p results. 
    
    m <- length(results)
    # Reformat data
    results <- data.frame(do.call(rbind, lapply(results, unlist)))
    # Plot mu results
    df <- data.frame(trials=c(1:m, 1:m), 
                     type=c(rep("act", m), rep("est", m)), 
                     val=c(results$theta.mu, results$theta.hat.mu),
                     ymin=c(results$theta.mu, results$ci.mu1),
                     ymax=c(results$theta.mu, results$ci.mu2))
    plot <- ggplot(df, aes(color=type)) + geom_point(aes(x=trials, y=val))
    plot <- plot + geom_errorbar(aes(x=trials, ymin=ymin, ymax=ymax))
    ggsave(filename.mu)
    # Plot sigma results
    df <- data.frame(trials=c(1:m, 1:m), 
                     type=c(rep("act", m), rep("est", m)), 
                     val=c(results$theta.sigma, results$theta.hat.sigma),
                     ymin=c(results$theta.sigma, results$ci.sigma1),
                     ymax=c(results$theta.sigma, results$ci.sigma2))
    plot <- ggplot(df, aes(color=type)) + geom_point(aes(x=trials, y=val))
    plot <- plot + geom_errorbar(aes(x=trials, ymin=ymin, ymax=ymax))
    ggsave(filename.sigma)
    # Plot p results
    df <- data.frame(trials=c(1:m, 1:m), 
                     type=c(rep("act", m), rep("est", m)), 
                     val=c(results$theta.p, results$theta.hat.p),
                     ymin=c(results$theta.p, results$ci.p1),
                     ymax=c(results$theta.p, results$ci.p2))
    plot <- ggplot(df, aes(color=type)) + geom_point(aes(x=trials, y=val))
    plot <- plot + geom_errorbar(aes(x=trials, ymin=ymin, ymax=ymax))
    ggsave(filename.p)
  }
  
  n <- 1000
  folder <- "./plots/epsp-binomial/tests/task1/"
  results <- list()
  
  cat("Test 1\n")
  theta = list(mu=0.3, sigma=0.2, p=0.2)
  N <- 1
  results[[1]] <- Task1(theta, N, n, paste(folder,"test01.pdf", sep=""))
  results[[1]]$theta <- theta
  cat("\n")
  
  cat("Test 2\n")
  theta = list(mu=0.5, sigma=1.0, p=0.5)
  N <- 1
  results[[2]] <- Task1(theta, N, n, paste(folder,"test02.pdf", sep=""))
  results[[2]]$theta <- theta
  cat("\n")
  
  cat("Test 3\n")
  theta = list(mu=0.3, sigma=0.2, p=0.2)
  N <- 5
  results[[3]] <- Task1(theta, N, n, paste(folder,"test03.pdf", sep=""))
  results[[3]]$theta <- theta
  cat("\n")
  
  cat("Test 4\n")
  theta = list(mu=0.5, sigma=1.0, p=0.5)
  N <- 3
  results[[4]] <- Task1(theta, N, n, paste(folder,"test04.pdf", sep=""))
  results[[4]]$theta <- theta
  cat("\n")
  
  cat("Test 5\n")
  theta = list(mu=0.1, sigma=0.9, p=0.7)
  N <- 2
  results[[5]] <- Task1(theta, N, n, paste(folder,"test05.pdf", sep=""))
  results[[5]]$theta <- theta
  cat("\n")
  
  cat("Test 6\n")
  theta = list(mu=0.7, sigma=0.2, p=0.9)
  N <- 3
  results[[6]] <- Task1(theta, N, n, paste(folder,"test06.pdf", sep=""))
  results[[6]]$theta <- theta
  cat("\n")
  
  cat("Test 7\n")
  theta = list(mu=0.9, sigma=0.9, p=0.02)
  N <- 2
  results[[7]] <- Task1(theta, N, n, paste(folder,"test07.pdf", sep=""))
  results[[7]]$theta <- theta
  cat("\n")
  
  cat("Test 8\n")
  theta = list(mu=0.5, sigma=0.5, p=0.02)
  N <- 3
  results[[8]] <- Task1(theta, N, n, paste(folder,"test08.pdf", sep=""))
  results[[8]]$theta <- theta
  cat("\n")
  
  cat("Test 9\n")
  theta = list(mu=0.5, sigma=0.1, p=0.02)
  N <- 2
  results[[9]] <- Task1(theta, N, n, paste(folder,"test09.pdf", sep=""))
  results[[9]]$theta <- theta
  cat("\n")
  
  cat("Test 10\n")
  theta = list(mu=2, sigma=0.5, p=0.02)
  N <- 3
  results[[10]] <- Task1(theta, N, n, paste(folder,"test10.pdf", sep=""))
  results[[10]]$theta <- theta
  cat("\n")
  
  PlotResults1(results, 
               paste(folder, "mu-1000-samples.pdf", sep=""),
               paste(folder, "sigma-1000-samples.pdf", sep=""),
               paste(folder, "p-1000-samples.pdf", sep=""))
  
  n <- 100
  
  cat("Test 11\n")
  theta = list(mu=0.3, sigma=0.2, p=0.99)
  N <- 1
  results[[1]] <- Task1(theta, N, n, paste(folder,"test11.pdf", sep=""))
  results[[1]]$theta <- theta
  cat("\n")
  
  cat("Test 12\n")
  theta = list(mu=0.3, sigma=0.2, p=0.99)
  N <- 4
  results[[2]] <- Task1(theta, N, n, paste(folder,"test12.pdf", sep=""))
  results[[2]]$theta <- theta
  cat("\n")
  
  cat("Test 13\n")
  theta = list(mu=0.3, sigma=0.2, p=0.2)
  N <- 1
  results[[3]] <- Task1(theta, N, n, paste(folder,"test13.pdf", sep=""))
  results[[3]]$theta <- theta
  cat("\n")
  
  cat("Test 14\n")
  theta = list(mu=0.5, sigma=1.0, p=0.1)
  N <- 1
  results[[4]] <- Task1(theta, N, n, paste(folder,"test14.pdf", sep=""))
  results[[4]]$theta <- theta
  cat("\n")
  
  cat("Test 15\n")
  theta = list(mu=-1.3, sigma=0.2, p=0.15)
  N <- 5
  results[[5]] <- Task1(theta, N, n, paste(folder,"test15.pdf", sep=""))
  results[[5]]$theta <- theta
  cat("\n")
  
  cat("Test 16\n")
  theta = list(mu=0.5, sigma=0.5, p=0.1)
  N <- 3
  results[[6]] <- Task1(theta, N, n, paste(folder,"test16.pdf", sep=""))
  results[[6]]$theta <- theta
  cat("\n")
  
  cat("Test 17\n")
  theta = list(mu=-1.0, sigma=0.2, p=0.15)
  N <- 1
  results[[7]] <- Task1(theta, N, n, paste(folder,"test17.pdf", sep=""))
  results[[7]]$theta <- theta
  cat("\n")
  
  cat("Test 18\n")
  theta = list(mu=0.1, sigma=1.0, p=0.1)
  N <- 2
  results[[8]] <- Task1(theta, N, n, paste(folder,"test18.pdf", sep=""))
  results[[8]]$theta <- theta
  cat("\n")
  
  cat("Test 19\n")
  theta = list(mu=-1.3, sigma=0.2, p=0.15)
  N <- 5
  results[[9]] <- Task1(theta, N, n, paste(folder,"test19.pdf", sep=""))
  results[[9]]$theta <- theta
  cat("\n")
  
  cat("Test 20\n")
  theta = list(mu=-2.0, sigma=0.5, p=0.1)
  N <- 2
  results[[10]] <- Task1(theta, N, n, paste(folder,"test20.pdf", sep=""))
  results[[10]]$theta <- theta
  cat("\n")
  
  PlotResults1(results, 
               paste(folder, "mu-100-samples.pdf", sep=""),
               paste(folder, "sigma-100-samples.pdf", sep=""),
               paste(folder, "p-100-samples.pdf", sep=""))
}
  
# Task 2: Using the MOME estimates to estimate the probability of seeing large
# amplitudes. 

if (2 %in% tasks.to.run) {
  cat("################################################################\n")
  cat("Task 2: Use the MOME estimates to predict the probability of\n")
  cat("large events.\n")
  cat("################################################################\n\n")
  
  Markov <- function(t, theta, N) {
    # Bounds on large amplitudes.
    # 
    # Args:
    #   t: Amplitude to bound. 
    #   theta: Parameters of the model. 
    #   N: Assuemd number of synaptic contacts. 
    #
    # Returns:
    #   p: Upper bound on the probability of events larger than t using Markov.
    
    (N * exp(theta$mu + theta$sigma^2/2) * theta$p) / t
  }
  
  Chebyshev <- function(t, theta, N) {
    # Bounds on large amplitudes.
    # 
    # Args:
    #   t: Amplitude to bound. 
    #   theta: Parameters of the model. 
    #   N: Assuemd number of synaptic contacts. 
    #
    # Returns:
    #   p: Upper bound on the probability of events larger than t using 
    #     Chebyshev.
    
    (N * (exp(2*(theta$mu+theta$sigma^2)) - exp(2*theta$mu+theta$sigma^2)) 
     * theta$p) / t^2
  }
  
  Task2 <- function(theta, N, n, filename) {
    # Tests the MOME estimates by simulating from the model and then the
    # probability of large amplitudes via the Markov and Chebyshev
    # inequalities. 
    # 
    # Args:
    #   theta: Parameter estimates.
    #   N: Assumed number of synaptic contacts.
    #   n: Number of samples to be taken. 
    #   filename: File name for saving the plots. 
    #   
    #  Returns:
    #   theta.hat: MOME estimates.
    #   ci: Confidence intervals. 
    
    cat("N =", N, "\n")
    cat("Model parameters: (mu, sigma, p) = (", theta$mu, ", ", theta$sigma, 
        ", ", theta$p, ")\n", sep="")
    cat("Trials sampled:", n, "\n")
    x <- EPSP.Binomial(theta, N, n)
    t <- max(x)
    theta.hat <- MOME.Binomial(x, N)
    cat("Estimated parameters: (mu, sigma, p) = (", theta.hat$mu, ", ", 
        theta.hat$sigma, ", ", theta.hat$p, ")\n", sep="")
    ci <- Bootstrap.CI.Binomial(theta.hat, N, n, B=100)
    upper <- list(mu=ci$mu[2], sigma=ci$sigma[2], p=ci$p[2])
    cat("Confidence intervals: (mu, sigma, p) = ([", ci$mu[1], ", ", 
        ci$mu[2], "], [", ci$sigma[1], ", ", ci$sigma[2], "], [", 
        ci$p[1], ", ", ci$p[2], "])\n", sep="")
    cat("Maximum amplitude: ", t, "\n", sep="")
    # Sumarry of results
    A = list(prob=1/n, 
             markov.act=Markov(t, theta, N),
             markov.est=Markov(t, theta.hat, N),
             markov.upb=Markov(t, upper, N),
             chebyshev.act=Chebyshev(t, theta, N), 
             chebyshev.est=Chebyshev(t, theta.hat, N),
             chebyshev.upb=Chebyshev(t, upper, N))
    cat("Observed Probability: ", A$prob, "\n", sep="")
    cat("Markov bound (actual value): ", A$markov.act, "\n", sep="")
    cat("Markov bound (estimator): ", A$markov.est, "\n", sep="")
    cat("Markov bound (upper bound): ", A$markov.upb, "\n", sep="")
    cat("Chebyshev bound (actual value): ", A$chebyshev.act, "\n", sep="")
    cat("Chebyshev bound (estimator): ", A$chebyshev.est, "\n", sep="")
    cat("Chebyshev bound (upper bound): ", A$chebyshev.upb, "\n", sep="")
    
    # Plot the result if filename is specified
    if (!missing(filename)) {
      df <- data.frame(cols=c("prob", "markov.act", "markov.est", "markov.upb", 
                              "chebyshev.act", "chebyshev.est", 
                              "chebyshev.upb"), 
                       type=c("emp", "act", "est", "upb", "act", "est", "upb"), 
                       val=unlist(A))
      plot <- ggplot(df, aes(fill=type)) + geom_col(aes(cols,val))
      plot <- plot + labs(title="Bounds on Tail Probabilities", x="Bound", 
                          y="Probability")
      plot <- plot + scale_fill_discrete(name="Parameter Origin", 
                                         breaks=c("emp", "act", "est", "upb"), 
                                         labels=c("Observed", "Actual Value", 
                                                  "MOME Estimate", 
                                                  "CI Upper Bound"))
      plot <- plot + theme(axis.text.x=element_text(angle=45))
      ggsave(filename)
    }
    return(A)
  }
  
  PlotResults2 <- function(results, filename) {
    # Plots the results. 
    # 
    # Args:
    #   results: List of lists containing the bounds.  
    #   filename: Filename to save the plot. 
    
    m <- length(results)
    # Reformat data
    results <- data.frame(do.call(rbind, lapply(results, unlist)))
    # Plot results
    df <- data.frame(trials=c(1:m), 
                     type=factor(c(rep("prob", m), rep("markov.act", m), 
                                   rep("markov.est", m), rep("markov.upb", m), 
                                   rep("chebyshev.act", m), 
                                   rep("chebyshev.est", m),
                                   rep("chebyshev.upb", m)), 
                                 levels=c("prob", "markov.act", "markov.est", 
                                          "markov.upb", "chebyshev.act", 
                                          "chebyshev.est", 
                                          "chebyshev.upb")), 
                     val=c(results$prob, results$markov.act, 
                           results$markov.est, results$markov.upb, 
                           results$chebyshev.act, results$chebyshev.est, 
                           results$chebyshev.upb))
    plot <- ggplot(df, aes(color=type, alpha=type))
    plot <- plot + geom_line(aes(x=trials, y=val))
    plot <- plot + labs(title="Bounds on Tail Probabilities", x="Tests", 
                        y="Probability")
    plot <- plot + scale_color_discrete(name="Bounding Method", 
                                        breaks=c("prob", "markov.act", 
                                                 "markov.est", "markov.upb",
                                                 "chebyshev.act", 
                                                 "chebyshev.est",
                                                 "chebyshev.upb"), 
                                        labels=c("Observed Probability", 
                                                 "Markov Actual", 
                                                 "Markov Estimate", 
                                                 "Markov Upper Bound",
                                                 "Chebyshev Actual",
                                                 "Chebyshev Estimate",
                                                 "Chebyshev Upper Bound"))
    plot <- plot + scale_alpha_manual(values=c(1, rep(0.2,6)), guide=FALSE)
    ggsave(filename)
  }
  
  n <- 1000
  folder <- "./plots/epsp-binomial/tests/task2/"
  results <- list()
  
  cat("Test 1\n")
  theta = list(mu=0.3, sigma=0.2, p=0.2)
  N <- 1
  results[[1]] <- Task2(theta, N, n, paste(folder,"test01.pdf", sep=""))
  cat("\n")
  
  cat("Test 2\n")
  theta = list(mu=0.5, sigma=1.0, p=0.5)
  N <- 1
  results[[2]] <- Task2(theta, N, n, paste(folder,"test02.pdf", sep=""))
  cat("\n")
  
  cat("Test 3\n")
  theta = list(mu=0.3, sigma=0.2, p=0.2)
  N <- 5
  results[[3]] <- Task2(theta, N, n, paste(folder,"test03.pdf", sep=""))
  cat("\n")
  
  cat("Test 4\n")
  theta = list(mu=0.5, sigma=1.0, p=0.5)
  N <- 3
  results[[4]] <- Task2(theta, N, n, paste(folder,"test04.pdf", sep=""))
  cat("\n")
  
  cat("Test 5\n")
  theta = list(mu=0.1, sigma=0.9, p=0.7)
  N <- 2
  results[[5]] <- Task2(theta, N, n, paste(folder,"test05.pdf", sep=""))
  cat("\n")
  
  cat("Test 6\n")
  theta = list(mu=0.7, sigma=0.2, p=0.9)
  N <- 3
  results[[6]] <- Task2(theta, N, n, paste(folder,"test06.pdf", sep=""))
  cat("\n")
  
  cat("Test 7\n")
  theta = list(mu=0.9, sigma=0.9, p=0.02)
  N <- 2
  results[[7]] <- Task2(theta, N, n, paste(folder,"test07.pdf", sep=""))
  cat("\n")
  
  cat("Test 8\n")
  theta = list(mu=0.5, sigma=0.5, p=0.02)
  N <- 3
  results[[8]] <- Task2(theta, N, n, paste(folder,"test08.pdf", sep=""))
  cat("\n")
  
  cat("Test 9\n")
  theta = list(mu=0.5, sigma=0.1, p=0.02)
  N <- 2
  results[[9]] <- Task2(theta, N, n, paste(folder,"test09.pdf", sep=""))
  cat("\n")
  
  cat("Test 10\n")
  theta = list(mu=2, sigma=0.5, p=0.02)
  N <- 3
  results[[10]] <- Task2(theta, N, n, paste(folder,"test10.pdf", sep=""))
  cat("\n")
  
  PlotResults2(results, paste(folder,"max-amp-prob-1000.pdf", sep=""))
  
  n <- 100
  
  cat("Test 11\n")
  theta = list(mu=0.3, sigma=0.2, p=0.99)
  N <- 1
  results[[1]] <- Task2(theta, N, n, paste(folder,"test11.pdf", sep=""))
  cat("\n")
  
  cat("Test 12\n")
  theta = list(mu=0.3, sigma=0.2, p=0.99)
  N <- 4
  results[[2]] <- Task2(theta, N, n, paste(folder,"test12.pdf", sep=""))
  cat("\n")
  
  cat("Test 13\n")
  theta = list(mu=0.3, sigma=0.2, p=0.2)
  N <- 1
  results[[3]] <- Task2(theta, N, n, paste(folder,"test13.pdf", sep=""))
  cat("\n")
  
  cat("Test 14\n")
  theta = list(mu=0.5, sigma=1.0, p=0.1)
  N <- 1
  results[[4]] <- Task2(theta, N, n, paste(folder,"test14.pdf", sep=""))
  cat("\n")
  
  cat("Test 15\n")
  theta = list(mu=-1.3, sigma=0.2, p=0.15)
  N <- 5
  results[[5]] <- Task2(theta, N, n, paste(folder,"test15.pdf", sep=""))
  cat("\n")
  
  cat("Test 16\n")
  theta = list(mu=0.5, sigma=0.5, p=0.1)
  N <- 3
  results[[6]] <- Task2(theta, N, n, paste(folder,"test16.pdf", sep=""))
  cat("\n")
  
  cat("Test 17\n")
  theta = list(mu=-1.0, sigma=0.2, p=0.15)
  N <- 1
  results[[7]] <- Task2(theta, N, n, paste(folder,"test17.pdf", sep=""))
  cat("\n")
  
  cat("Test 18\n")
  theta = list(mu=0.1, sigma=1.0, p=0.1)
  N <- 2
  results[[8]] <- Task2(theta, N, n, paste(folder,"test18.pdf", sep=""))
  cat("\n")
  
  cat("Test 19\n")
  theta = list(mu=-1.3, sigma=0.2, p=0.15)
  N <- 5
  results[[9]] <- Task2(theta, N, n, paste(folder,"test19.pdf", sep=""))
  cat("\n")
  
  cat("Test 20\n")
  theta = list(mu=-2.0, sigma=0.5, p=0.1)
  N <- 2
  results[[10]] <- Task2(theta, N, n, paste(folder,"test20.pdf", sep=""))
  cat("\n")
  
  PlotResults2(results, paste(folder,"max-amp-prob-100.pdf", sep=""))
}
