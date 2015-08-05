#
#
#### Using the 'missForrest' package for multiple imputation.
#
################################################################################
# This material was also discussed in the July issue of RSS Matters, an
# Adobe.pdf version of that article is available at the following URL:
# http://www.unt.edu/rss/class/Jon/Benchmarks/missForest_L_JDS_July2014.pdf
#
# Import the example data file(s).

no.miss <- read.table("http://www.unt.edu/rss/class/Jon/R_SC/Module4/missForest_noMiss.txt",
                      header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
wi.miss <- read.table("http://www.unt.edu/rss/class/Jon/R_SC/Module4/missForest_Miss.txt",
                      header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
summary(no.miss)  # No missing values.
summary(wi.miss)  # Same data, but with missing values (MCAR).

# How much missing? Approximately 5% of the data.

ncol(wi.miss); nrow(wi.miss)
length(which(is.na(wi.miss) == "TRUE")) / (nrow(wi.miss)*ncol(wi.miss))

# Load the necessary package;
# and dependencies: randomForest, foreach, itertools, iterators.

library(missForest)

# Apply the 'missForest' function with all arguments set to default values.
# The function returns a list object with 3 elements: "ximp" which is the
# imputed data, "OOBerror" which is the estimated imputation error, and
# "error" which is the true imputation error. 
# Please note, the function does accept a data frame; the package documentation
# states that the data must be in a matrix (all numeric), however that is not
# the case.

system.time(im.out.1 <- missForest(xmis = wi.miss, maxiter = 10, ntree = 100,
                                   variablewise = FALSE,
                                   decreasing = FALSE, verbose = FALSE,
                                   mtry = floor(sqrt(ncol(wi.miss))), replace = TRUE,
                                   classwt = NULL, cutoff = NULL, strata = NULL,
                                   sampsize = NULL, nodesize = NULL, maxnodes = NULL,
                                   xtrue = NA, parallelize = "no"))

# Extracting only the imputed data from the output list.

im.miss.1 <- im.out.1$ximp
summary(no.miss)
summary(wi.miss)
summary(im.miss.1)

# To extract the OOBerror, simply use the familiar "$" operator.

im.out.1$OOBerror

# Please note: large data sets will require considerable more time.

################################################################################
#
# Now we try an example using the parallel processing argument to utilize
# two cores (on a two core machine). Note, we first need to load an additional
# package (doParallel; and its dependency: parallel) and then register
# the number of processors (i.e. cores).

library(doParallel)
registerDoParallel(cores = 2)

# Now we can apply the 'missForest' function while braking the work down into
# equal numbers of 'variables' or 'forests' for each core to work on (here we
# break the number of variables).

system.time(im.out.2 <- missForest(xmis = wi.miss, maxiter = 10, ntree = 100,
                                   variablewise = FALSE,
                                   decreasing = FALSE, verbose = FALSE,
                                   mtry = floor(sqrt(ncol(wi.miss))), replace = TRUE,
                                   classwt = NULL, cutoff = NULL, strata = NULL,
                                   sampsize = NULL, nodesize = NULL, maxnodes = NULL,
                                   xtrue = NA, parallelize = "variables"))
im.miss.2 <- im.out.2$ximp  # Extracting only the imputed data from the output list.
summary(no.miss)
summary(wi.miss)
summary(im.miss.2)

# Please note: large data sets will require considerable more time.
# Next, we detach all the packages used above and remove the 'im.out' objects.

detach("package:missForest"); detach("package:randomForest")
detach("package:doParallel"); detach("package:parallel")
detach("package:foreach"); detach("package:itertools")
detach("package:iterators")
rm(im.out.1, im.out.2)

################################################################################
# For really big data sets, you might try using the function below 
# to re-sample your data and impute the subsamples repeatedly until 
# all the missing values have been imputed. 

# Read the function into the workspace.

imp.fun <- function(df, n.samp = round(.05*(nrow(df))), p = FALSE,
                    n.core = 1, brk.out = 10000){
  library(missForest)
  para <- "no"
  if(p == TRUE){
    library(doParallel)
    library(parallel)
    registerDoParallel(cores = n.core)
    para <- "forests"
  }
  id <- seq(1:nrow(df))
  count <- length(which(is.na(df) == TRUE))
  print(count / (nrow(df)*ncol(df)))
  i <- 1
  while(count > 0){
    s1 <- sample(id, n.samp, replace = F)
    tmp <- df[s1,]
    count.t <- length(which(is.na(tmp) == TRUE))
    if(count.t > 0){
      imp <-  missForest(xmis = tmp, maxiter = 10, ntree = 100,
                         variablewise = FALSE, decreasing = FALSE, verbose = FALSE,
                         mtry = floor(sqrt(ncol(tmp))), replace = TRUE,
                         classwt = NULL, cutoff = NULL, strata = NULL,
                         sampsize = NULL, nodesize = NULL, maxnodes = NULL,
                         xtrue = NA, parallelize = para)$ximp}
    if(count.t > 0){df[s1,] <- imp}
    count <- length(which(is.na(df) == TRUE))
    print(count / (nrow(df)*ncol(df)))
    i <- i + 1; if(i > brk.out){break}; print(i)
  }; rm(count.t, s1, tmp, imp)
  df.out <- df
  detach("package:missForest")
  detach("package:randomForest")
  if(p == TRUE){detach("package:doParallel")}
  if(p == TRUE){detach("package:parallel")}
  detach("package:foreach")
  detach("package:itertools")
  detach("package:iterators")
  return(df.out)
}

# Now apply the function with the default arguments (not listed). Keep in
# mind, if you have a multicore computer, you can specify p = TRUE and
# the number of cores (e.g., n.core = 2) to utilize the parallel
# function of the 'missForest' function.

im.miss.3 <- imp.fun(df = wi.miss)
summary(no.miss)
summary(wi.miss)
summary(im.miss.3)

################################################################################
#
################ References & Resources:
#
# Stekhoven, D. J. (2013). Package missForest: Nonparametric missing value
#      imputation using random forest. Package documentation available at:
#      http://cran.r-project.org/web/packages/missForest/index.html
#
# Stekhoven, D. J. (2012). MissForest -- Non-parametric missing value imputation
#      for mixed-type data. Bioinformatics, 28(1), 112 -- 118.
# 
################################################################################
#
# Clean up.

rm(im.miss.1, im.miss.2, im.miss.3, no.miss, wi.miss, imp.fun)
ls()





# End script. Last updated: 2014.06.26
