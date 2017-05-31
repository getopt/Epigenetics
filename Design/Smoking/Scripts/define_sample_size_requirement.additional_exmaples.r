
# STAGE EXPERIMENTS

# cpgName <- "cg03636183"
cpgName <- "cg03707168"
outfi <- paste("Plots/define_sample_size_requirments.", cpgName, ".pdf", sep = '')

# Never smokers (ns): Mean and standard deviation of beta
# ns_mean <- 0.672
# ns_sd <- 0.032
ns_mean <- 0.300
ns_sd <- 0.054

# Current smokers (cs): Mean and standard deviation of beta 
# cs_mean <- 0.553
# cs_sd <- 0.076
cs_mean <- 0.263
cs_sd <- 0.058

# Number of experiments and participants
nexpr <- 100
ngroup <- 100

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
for (size in c(2:ngroup))
{
    results <- NULL
    for (experiment in c(1:nexpr))
    {
        results <- c(results, run_experiment(size, ns_mean, ns_sd, cs_mean, cs_sd))
    }

    all_results[[size]] <- -log10(results)
}

pdf(outfi, height = 6, width = 10)
    boxplot(all_results,
            ylab = "-log10(p.value)", 
    	    xlab = "Sizes of groups of participants to compare",
            range = 0.5,
    	    main = paste("Outcomes of ", nexpr, " experiments comparing ", cpgName, " methylation \n",
                         "between non-smokers and current smokers\n",
                         "in groups from 2 to ", ngroup, " participants"))
    abline(h = -log10(0.05), col = "darkgreen", lwd = 2)
    abline(h = -log10(0.2), col = "red", lwd = 2)
    legend("topleft", pch = 19, col = "white",
           legend = c(paste("Never-smokers mean methylation: ", 100*ns_mean, "%", sep = ""),
                      paste("Never-smokers standard deviation: ", ns_sd, sep = ""),
                      paste("Current smokers mean methylation: ", 100*cs_mean, "%", sep = ""),
                      paste("Current smokers standard deviation: ", cs_sd, sep = "")),
           bty = "n")

dev.off()


