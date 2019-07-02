# Based on guide by lecturer Bin Peng.
rm(list = ls()) # Clean the memory. Make sure we start from blank.
cat("\014")     # Send ctrl+L to the console and therefore will clear the screen

# getwd() # Give you the current working directory
# setwd() allows you to change the working directory
setwd("/Users/yashvirsurana/Desktop/University of Bath/Financial Economics/")
# NOTE: Place all R code we create and any data we need to call from within R in this R working directory.

df_bank <- read.table("large_bank.csv", header = TRUE, sep = ",")
print(head(df_bank)) # Take a quick look at the data set in R
cat("\n\n")

#Q1
#------------------------------------------------------
# Below we use data of year 1986 
year1986 <- c(1,2,3,4) 
L_tot    <- dim(df_bank)[1] # Return the total number of rows in your data set
ind_tot  <- 1:L_tot
ind_1986 <- ind_tot[ is.element(df_bank$t, year1986) ] # Give the TRUE indices of the data of year 1987
df_bank  <- df_bank[ind_1986,]

# Descriptive Stats for 1986 - Generic Way
print( summary(df_bank) )  
cat("\n\n")

# Descriptive Stats for 1986 - YS way
library(psych) #--Added by YS, psych gives sd, median, range, skew, kurtosis,etc
desc_stats <- describe(df_bank) #--Added by YS
write.csv(desc_stats, file = "desc_stats.csv") 

#Q2
#------------------------------------------------------
# We focus on all quarters in 1986 and all four policies.
Quarter_nos <- c(1,2,3,4) # For year 1986, quarter 3 means t = 3
Policy_nos  <- c(1,2,3,4)

for (q in Quarter_nos) { # loop over quarters
  for (p in Policy_nos) { # loop over policies
    # Get the indices of the rows with "Policy = p" and "Quarter = q",  
    # i.e., identify the data needed for our application
    ind_U <- which(df_bank$POLICY == p & df_bank$t == q)
    cat("The proportion of policy", p, "at Quarter", q, "is",length(ind_U)/466, "\n")
  }
}

#Q3
#------------------------------------------------------
Policy_nos  <- c(1,2,3)
models <- list()

for (p in Policy_nos) {
  
  ind_U <- which(df_bank$POLICY == p) # & df_bank$t == 1)
  # Constructe the variables for OLS regression
  temp <- df_bank[ind_U,]
  Y <- log( temp$COST/temp$P_DEPOSI_w3 )
  X <- matrix(0, nrow = length(Y), ncol = 5)
  X[,1] <- log( temp$P_LABOR_w1/temp$P_DEPOSI_w3 )
  X[,2] <- log( temp$P_FUNDS_w2/temp$P_DEPOSI_w3 )
  X[,3] <- log( temp$CON_LOAN_y1 )
  X[,4] <- log( temp$NONC_LOA_y2 )
  X[,5] <- log( temp$SECURITI_y3 )
  
  OLS_output <- lm(Y ~ X)
  models <- list(models, OLS_output)
  # summary(OLS_output)
  print(summary(OLS_output))#$coefficients[,1:2])
  RTS <- 1/sum( summary(OLS_output)$coefficients[4:6,1] )
  cat("\nRTS is", RTS, "\n")
  
  # Generate Plots for Variables V Y
  coeffs  <- c(1,2,3,4,5)
  for (i in coeffs) {
    jpeg(file = paste('Policy',p,'_Y_X',i,'.jpeg', sep = ''))
    plot(Y, X[,i])
    dev.off()
  }
}

# Print Latex Tables for the Report
library(stargazer)
stargazer(models[[1]][[1]][[2]], models[[1]][[2]], models[[2]],column.labels=c("Policy 1","Policy 2", "Policy 3"), align=TRUE)
models[1]
