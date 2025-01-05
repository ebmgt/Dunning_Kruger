# This file is available at https://github.com/ebmgt/Dunning-Kruger/
# Author:rbadgett@kumc.edu
# Permission: GNU GPLv3 https://choosealicense.com/licenses/gpl-3.0/
# Last edited 2025-01-05

#== Startup ======
library(tcltk) # For interactions and troubleshooting, part of base package so no install needed.

#* Set working directory -----
if (Sys.getenv("RSTUDIO") != "1"){
  args <- commandArgs(trailingOnly = FALSE)
  script_path <- sub("--file=", "", args[grep("--file=", args)])  
  script_path <- dirname(script_path)
  setwd(script_path)
}else{
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
}
getwd()

#* Troubleshooting options -----
options(error = NULL)   # Default
options(warn=1) # print error; but dont convert to error. Default is 0
getOption("warn")

## Functions -----
`%notin%` <- Negate(`%in%`)
`%!=na%` <- function(e1, e2) (e1 != e2 | (is.na(e1) & !is.na(e2)) | (is.na(e2) & !is.na(e1))) & !(is.na(e1) & is.na(e2))
`%==na%` <- function(e1, e2) (e1 == e2 | (is.na(e1) & is.na(e2)))

# Call with function_progress(0,'Libraries')
# Call within a loop:
#  function_progress(100*(index)/length(sheet_names), paste('Processing ',sheet_name))
#  if (index+1 == length(sheet_names)){function_progress(100,'Done')}
library(tcltk)  # Ensure Tcl/Tk is loaded

function_progress <- function(progress, titletext = "testing") {
  # Check if Tcl/Tk is available
  if (!capabilities("tcltk")) {
    message("Tcl/Tk not available; cannot display a progress bar.")
    return(invisible(NULL))
  }
  
  # If Pb does not exist or got removed, create a new progress bar at 0
  if (!exists("Pb", envir = .GlobalEnv)) {
    Pb <<- tkProgressBar(title = titletext, label = "", min = 0, max = 100, initial = 0)
  }
  
  info <- sprintf("%d%% done", round(progress))
  
  # Attempt to set the progress bar; if it fails, try recreating it
  tryCatch({
    setTkProgressBar(Pb, value = progress, 
                     title = paste(titletext, sprintf("(%s)", info)), 
                     label = info)
  }, error = function(e) {
    # If there's an error, try to close and remove Pb, then recreate
    if (exists("Pb", envir = .GlobalEnv)) {
      try(close(Pb), silent = TRUE)
      rm(Pb, envir = .GlobalEnv)
    }
    # Recreate the progress bar and update
    Pb <<- tkProgressBar(title = titletext, label = "", min = 0, max = 100, initial = 0)
    setTkProgressBar(Pb, value = progress, 
                     title = paste(titletext, sprintf("(%s)", info)), 
                     label = info)
  })
  
  # If progress reached 100%, close and remove the progress bar
  if (progress == 100) {
    close(Pb)
    rm(Pb, envir = .GlobalEnv)
  }
}


function_libraries_install <- function(packages){
  install.packages(setdiff(packages, rownames(installed.packages())), 
                   repos = "https://cloud.r-project.org/",
                   #type = "binary"
  )
  for(package_name in packages)
  {
    library(package_name, character.only=TRUE, quietly = FALSE);
    cat('Installing package: ', package_name)
  }
  #tk_messageBox(type = "ok", paste(packages, collapse="\n"), title="Packages installed")
}

current.date <- function(){
  # Simply Sys.Date() does the same
  return (as.character(strftime (Sys.time(), format="%Y-%m-%d", tz="", usetz=FALSE)))
}  

# Packages/libraries -----
function_progress(0,'Libraries')
#* Essential -----
packages_essential <- c("tcltk", # Helps troublshooting
                        'rstudioapi', # function_plot_print
                        'stringr', 'openxlsx','readr','foreach')
function_libraries_install(packages_essential)

function_progress(50,'Libraries')

packages_stats <- c('BlandAltmanLeh','reshape2')
function_libraries_install(packages_stats)

function_progress(100,'Libraries')

# Create data --------------
# 1. Generate respondent names (AA, BA, CA, ..., ADZ pattern)
respondents <- c(outer(LETTERS, LETTERS, paste0))[1:100]

# 2. Generate measure_objective with negative kurtosis and quartile means 2, 4, 6, 8
set.seed(42)  # For reproducibility
quartile_means <- c(2, 4, 6, 8)
measure_objective <- unlist(lapply(quartile_means, function(mean) {
  pmax(pmin(rnorm(25, mean, 0.5), 10), 1)  # Ensuring values are between 1 and 10
}))

# 3. Generate measure_self_reported with adjusted quartile means 4, 5, 6, 7
quartile_report_means <- c(4, 5, 6, 7)
measure_self_reported <- unlist(lapply(quartile_report_means, function(mean) {
  pmax(pmin(rnorm(25, mean, 0.5), 10), 1)
}))

# Create dataframe
Dunning_Kruger_example <- data.frame(
  respondent = respondents,
  measure_objective = measure_objective,
  measure_self_reported = measure_self_reported
)

# 4. Create quartiles based on measure_objective
Dunning_Kruger_example$quartile <- cut(Dunning_Kruger_example$measure_objective,
                                       breaks = quantile(Dunning_Kruger_example$measure_objective, probs = seq(0, 1, 0.25)),
                                       include.lowest = TRUE, labels = c("Quartile 1", "Quartile 2", "Quartile 3", "Quartile 4"))

# Calculate means by quartile
quartile_means_actual <- tapply(Dunning_Kruger_example$measure_objective, Dunning_Kruger_example$quartile, mean)
quartile_means_perceived <- tapply(Dunning_Kruger_example$measure_self_reported, Dunning_Kruger_example$quartile, mean)

# Function to perform paired t-test or Wilcoxon test for each quartile and return p-values
get_quartile_pvalues <- function(data) {
  p_values <- numeric(4)
  for (i in 1:4) {
    subset_data <- data[data$quartile == levels(data$quartile)[i], ]
    if (shapiro.test(subset_data$measure_objective - subset_data$measure_self_reported)$p.value > 0.05) {
      test_result <- t.test(subset_data$measure_objective, subset_data$measure_self_reported, paired = TRUE)
    } else {
      test_result <- wilcox.test(subset_data$measure_objective, subset_data$measure_self_reported, paired = TRUE)
    }
    p_values[i] <- test_result$p.value
  }
  return(p_values)
}

# Get p-values for each quartile
p_values <- get_quartile_pvalues(Dunning_Kruger_example)

# Statistics ------
#* Spearman correlation ------
spearman_corr <- cor.test(Dunning_Kruger_example$measure_objective, Dunning_Kruger_example$measure_self_reported, method = "spearman")
R_value <- round(spearman_corr$estimate, 2)
p_value <- format(round(spearman_corr$p.value, 3), nsmall = 3)

#* ANOVA -----
long_data <- melt(Dunning_Kruger_example, 
                  id.vars = c("respondent", "quartile"), 
                  measure.vars = c("measure_objective", "measure_self_reported"), 
                  variable.name = "measure_type", 
                  value.name = "score")
# Perform repeated measures ANOVA
anova_result <- aov(score ~ measure_type + Error(respondent), data = long_data)
anova_summary <- summary(anova_result)
repeated_means_p_value <- anova_summary[[2]][[1]]$`Pr(>F)`[1]

#* Friedman test as non-parametric alternative
# Uswe this if data not normal
friedman_result <- friedman.test(score ~ measure_type | respondent, data = long_data)
#repeated_means_p_value <- friedman_result$p.value

# Adjust plot margins to create space for notes
par(mar = c(5.1 + 3, 4.1, 4.1, 1))  # (bottom, left, top, right)

# Plot using base R ------
plot(1:4, quartile_means_actual, type = "o", pch = 16, col = "blue", xaxt = "n",
     ylim = c(0, 10), xlab = "Quartiles of responses based on objective measures", 
     ylab = "Mean values for\nperformance measures (1-10 scale)",
     main = "Dunning-Kruger Plot Example for 100 subjects")
lines(1:4, quartile_means_perceived, type = "o", pch = 16, col = "red")
axis(1, at = 1:4, labels = c("Quartile 1", "Quartile 2", "Quartile 3", "Quartile 4"))

# Add inset legend
legend(x = "topleft", inset = c(0.02, 0.02), legend = c("Objective measure", "Self-reported measure"), 
       col = c("blue", "red"), pch = 16, box.lwd = 0)


# Add p-values above the highest value for each quartile
for (i in 1:4) {
  y_pos <- max(quartile_means_actual[i], quartile_means_perceived[i]) + 0.5
  col <- ifelse(p_values[i] < 0.05, "red", "black")
  adj_val <- ifelse(i == 1, 0, ifelse(i == 4, 1, 0.5))  # Left align for Quartile 1, right align for Quartile 4
  text(x = i, y = y_pos, labels = paste0("p = ", format(round(p_values[i], 3), nsmall = 3)), col = col, adj = adj_val)
}

# Annotations -----
# Display Spearman correlation and ANOVA p-value under the legend (without overlap)
text(x = 1, y = 7, labels = paste0("Spearman R = ", R_value, ", p = ", p_value, "
", "ANOVA p = ", format(round(repeated_means_p_value, 3), nsmall = 3)), adj = c(0,0))
# Replace ANOVA labeld above with Friedman if you use the Friedman p value

# 7. Add "Notes" section below the plot
mtext("Notes:", line = 4, adj = 0, font = 2,side = 1)
mtext("1. Code and example data are available at https://ebmgt.github.io/Dunning-Kruger/", 
      line = 5, adj = 0, font = 1, cex = 0.8, side = 1)
mtext(paste0("Copyleft: Attribution-NonCommercial 4.0 International (CC BY-NC 4.0). Version: ", Sys.Date(), ". rbadgett@kumc.edu"),
      line = 7, cex=0.8, font=1, adj=1, side=1)
