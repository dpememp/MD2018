#=======================================================================

# Rattle is Copyright (c) 2006-2018 Togaware Pty Ltd.
# It is free (as in libre) open source software.
# It is licensed under the GNU General Public License,
# Version 2. Rattle comes with ABSOLUTELY NO WARRANTY.
# Rattle was written by Graham Williams with contributions
# from others as acknowledged in 'library(help=rattle)'.
# Visit https://rattle.togaware.com/ for details.

#=======================================================================
# Rattle timestamp: 2018-12-07 17:13:56 x86_64-redhat-linux-gnu 

# Rattle version 5.2.0 user 'adriano'

# This log captures interactions with Rattle as an R script. 

# For repeatability, export this activity log to a 
# file, like 'model.R' using the Export button or 
# through the Tools menu. Th script can then serve as a 
# starting point for developing your own scripts. 
# After xporting to a file called 'model.R', for exmample, 
# you can type into a new R Console the command 
# "source('model.R')" and so repeat all actions. Generally, 
# you will want to edit the file to suit your own needs. 
# You can also edit this log in place to record additional 
# information before exporting the script. 
 
# Note that saving/loading projects retains this log.

# We begin most scripts by loading the required packages.
# Here are some initial packages to load and others will be
# identified as we proceed through the script. When writing
# our own scripts we often collect together the library
# commands at the beginning of the script here.

library(rattle)   # Access the weather dataset and utilities.
library(magrittr) # Utilise %>% and %<>% pipeline operators.

# This log generally records the process of building a model. 
# However, with very little effort the log can also be used 
# to score a new dataset. The logical variable 'building' 
# is used to toggle between generating transformations, 
# when building a model and using the transformations, 
# when scoring a dataset.

building <- TRUE
scoring  <- ! building

# A pre-defined value is used to reset the random seed 
# so that results are repeatable.

crv$seed <- 42 

#=======================================================================
# Rattle timestamp: 2018-12-07 17:14:25 x86_64-redhat-linux-gnu 

# Load a dataset from file.

fname         <- "file:///home/adriano/Documents/MD/Kaggle Datasets/who-suicide-statistics/who_suicide_statistics.csv" 
crs$dataset <- read.csv(fname,
			na.strings=c(".", "NA", "", "?"),
			strip.white=TRUE, encoding="UTF-8")

#=======================================================================
# Rattle timestamp: 2018-12-07 17:14:26 x86_64-redhat-linux-gnu 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=43776 train=30643 validate=6566 test=6567

set.seed(crv$seed)

crs$nobs <- nrow(crs$dataset)

crs$train <- sample(crs$nobs, 0.7*crs$nobs)

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  sample(0.15*crs$nobs) ->
crs$validate

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  setdiff(crs$validate) ->
crs$test

# The following variable selections have been noted.

crs$input     <- c("country", "year", "age", "suicides_no",
                   "population")

crs$numeric   <- c("year", "suicides_no", "population")

crs$categoric <- c("country", "age")

crs$target    <- "sex"
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- NULL
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2018-12-07 17:14:43 x86_64-redhat-linux-gnu 

# The 'Hmisc' package provides the 'contents' function.

library(Hmisc, quietly=TRUE)

# Obtain a summary of the dataset.

contents(crs$dataset[crs$train, c(crs$input, crs$risk, crs$target)])
summary(crs$dataset[crs$train, c(crs$input, crs$risk, crs$target)])

#=======================================================================
# Rattle timestamp: 2018-12-07 17:17:54 x86_64-redhat-linux-gnu 

# Scatter or Pairs Plot (Plot a Correlation Matrix) 

# The 'Deducer' package provides the 'ggcorplot' function.

library(Deducer, quietly=TRUE)

#=======================================================================
# Rattle timestamp: 2018-12-07 17:25:32 x86_64-redhat-linux-gnu 

# Generate a correlation plot for the variables. 

# The 'corrplot' package provides the 'corrplot' function.

library(corrplot, quietly=TRUE)

# Correlations work for numeric variables only.

crs$cor <- cor(crs$dataset[crs$train, crs$numeric], use="pairwise", method="pearson")

# Order the correlations by their strength.

crs$ord <- order(crs$cor[1,])
crs$cor <- crs$cor[crs$ord, crs$ord]

# Display the actual correlations.

print(crs$cor)

# Graphically display the correlations.

corrplot(crs$cor, mar=c(0,0,1,0))
title(main="Correlation who_suicide_statistics.csv using Pearson",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

#=======================================================================
# Rattle timestamp: 2018-12-07 17:25:59 x86_64-redhat-linux-gnu 

# Principal Components Analysis (on numerics only).

pc <- prcomp(na.omit(crs$dataset[crs$train, crs$numeric]), scale=TRUE, center=TRUE, tol=0)

# Show the output of the analysis.

pc

# Summarise the importance of the components found.

summary(pc)

# Display a plot showing the relative importance of the components.

plot(pc, main="")
title(main="Principal Components Importance who_suicide_statistics.csv",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
axis(1, at=seq(0.7, ncol(pc$rotation)*1.2, 1.2), labels=colnames(pc$rotation), lty=0)

# Display a plot showing the two most principal components.

biplot(pc, main="")
title(main="Principal Components who_suicide_statistics.csv",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

#=======================================================================
# Rattle timestamp: 2018-12-07 17:27:07 x86_64-redhat-linux-gnu 

# KMeans 

# Reset the random number seed to obtain the same results each time.

set.seed(crv$seed)

# The 'reshape' package provides the 'rescaler' function.

library(reshape, quietly=TRUE)

# Generate a kmeans cluster of size 5.

crs$kmeans <- kmeans(sapply(na.omit(crs$dataset[crs$train, crs$numeric]), rescaler, "range"), 5)

#=======================================================================
# Rattle timestamp: 2018-12-07 17:27:07 x86_64-redhat-linux-gnu 

# Report on the cluster characteristics. 

# Cluster sizes:

paste(crs$kmeans$size, collapse=' ')

# Data means:

colMeans(sapply(na.omit(crs$dataset[crs$train, crs$numeric]), rescaler, "range"))

# Cluster centers:

crs$kmeans$centers

# Within cluster sum of squares:

crs$kmeans$withinss

# Time taken: 0.10 secs

#=======================================================================
# Rattle timestamp: 2018-12-07 17:27:36 x86_64-redhat-linux-gnu 

# Decision Tree 

# The 'rpart' package provides the 'rpart' function.

library(rpart, quietly=TRUE)

# Reset the random number seed to obtain the same results each time.

set.seed(crv$seed)

# Build the Decision Tree model.

crs$rpart <- rpart(sex ~ .,
    data=crs$dataset[crs$train, c(crs$input, crs$target)],
    method="class",
    parms=list(split="information"),
    control=rpart.control(usesurrogate=0, 
        maxsurrogate=0),
    model=TRUE)

# Generate a textual view of the Decision Tree model.

print(crs$rpart)
printcp(crs$rpart)
cat("\n")

# Time taken: 0.61 secs
