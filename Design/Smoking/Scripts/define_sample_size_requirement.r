
# STAGE EXPERIMENTS

# Never smokers (ns): Mean and standard deviation of beta
ns_mean <- 0.672
ns_sd <- 0.032

# Current smokers (cs): Mean and standard deviation of beta 
cs_mean <- 0.553
cs_sd <- 0.076

# Number of experiments
nexpr <- 100

# Run a single experiment
run_experiment <- function(size, ns_mean, ns_sd, cs_mean, cs_sd)
{
    ns_data <- rnorm(size, mean = ns_mean, sd = ns_sd)
    cs_data <- rnorm(size, mean = cs_mean, sd = cs_sd)

    test <- t.test(ns_data, cs_data)
    return(test$p.value)
}


# MODEL EXPERIMENTS

# Sizes of test groups to compare
all_results <- list()
for (size in c(2:20))
{
    results <- NULL
    for (experiment in c(1:nexpr))
    {
        results <- c(results, run_experiment(size, ns_mean, ns_sd, cs_mean, cs_sd))
    }

    all_results[[size]] <- -log10(results)
}

pdf("Plots/define_sample_size_requirments.pdf", height = 6, width = 10)
    boxplot(all_results,
            ylab = "-log10(p.value)", 
    	    xlab = "Sizes of groups of participants to compare",
            range = 0.5,
    	    main = paste("Outcomes of ", nexpr, " experiments comparing CpG methylation \n",
                         "between non-smokers and current smokers\n",
                         "in groups from 2 to 20 participants"))
    abline(h = -log10(0.05), col = "darkgreen", lwd = 2)
    abline(h = -log10(0.2), col = "red", lwd = 2)
dev.off()


