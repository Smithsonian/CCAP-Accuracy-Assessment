# Generating Tables to get unbiased esimators of error w/ CI's from CCAP data

aa <- as.matrix(read.csv(paste(getwd(), "/data/CCAP2010AccuracyAssessment.csv", sep=""), row.names = 1))
area <- read.csv(paste(getwd(), "/data/FractionOfClasses.csv", sep=""))
area <- area$Region

propAreaCalc <- function(input_area) {return(input_area / sum(input_area)) }

propAreaMatrix <-function(input_matrix, input_areas) {
  input_pAreas <- propAreaCalc(input_areas)
  n_rowSums <- rowSums(input_matrix)
  areaPropMatrix <- (input_matrix / n_rowSums) * input_pAreas
  return(areaPropMatrix)
}

p_accuracy <- function(input_matrix) { # function for producer's accuracy given a confusion matrix
  pa_output <- c()
  q <- nrow(input_matrix)
  for (j in 1:q) {
    pa_temp <- input_matrix[j,j] / sum(input_matrix[,j])
    pa_output <-c(pa_output, pa_temp)
  }
  out_df <- t(data.frame(producer_accuracy = pa_output))
  colnames(out_df) <- row.names(input_matrix)
  return(out_df)
}

u_accuracy <- function(input_matrix) { # function for user's accuracy given a confusion matrix
  ua_output <- c()
  q <- nrow(input_matrix)
  for (i in 1:q) {
    ua_temp <- input_matrix[i,i] / sum(input_matrix[i,])
    ua_output <-c(ua_output, ua_temp)
  }
  out_df <- t(data.frame(user_accuracy = ua_output))
  colnames(out_df) <- row.names(input_matrix)
  return(out_df)
}

o_agreement <- function(input_matrix) { # function for total agreement given a confusion matrix
  return(overall_agreement = (sum(diag(input_matrix))))
}

areaCorrections <- function(input_matrix, input_areas) {
  
  input_pAreas <- propAreaCalc(input_areas) # calculate the proportional areas
  tempPropAreaMatrix <- propAreaMatrix(input_matrix, input_areas) # calculate confusion matrix as proportional area rather than n
  correctedAreaProps <- colSums(tempPropAreaMatrix) # Eq. 9: the column sums are the estimated area, as opposed to the row sums which are the mapped area
  
  se <-c() # empty vector for se
  for (k in 1:nrow(input_matrix)) { # for each class
    step2s <- c()
    for (i in 1:nrow(input_matrix)) { # for each row
      se_step1 = (input_pAreas[i] * tempPropAreaMatrix[i,k]) - (tempPropAreaMatrix[i,k] ^ 2) # (Wi * pik) - pik^2: Eq. 10 TOP
      se_step2 = se_step1 / (sum(input_matrix[i,]) - 1 ) # step 1 / (ni. - 1): Eq. 10 BOTTOM
      step2s <- c(step2s, se_step2) # store row output
    }
    se_step3 <- sqrt(sum(step2s)) # square root of the sum of the rows: Eq. 10: OUTER
    se <-c(se, se_step3)
  }
  ci = se * 1.96 # CI from SE standard estimation formula
  
  # In Eq. 11 Oloffson et al. apply the estimated area proportions and the CI by multiplying by total map area
  # We do something slightly different that works out to be the same in the end
  # Because we are interested in propegating uncertainty at the pixel level we need to know two things
  # 1. If a pixel is present do we need to scale up or scale down based on inclusion / exclusion probabilities of the mapped class
  # 2. What are the CIs on that 'scaler'
  perPixelScaler <- (correctedAreaProps / rowSums(tempPropAreaMatrix)) # per pixel scaler is the ration of the estimated occurence: mapped occurence
  scalerSE <- se / rowSums(tempPropAreaMatrix) # se is calculated for scaler
  scalerCI <- ci / rowSums(tempPropAreaMatrix) # ci calculated for scaler
  
  estimatedArea <- perPixelScaler * input_areas # calculate estimated area using input class area and pixel based scaler
  estimatedAreaSE <- input_areas * scalerSE # se
  estimatedAreaCI <- input_areas * scalerCI # ci
  
  return(data.frame(perPixelScaler = perPixelScaler, scalerSE = scalerSE, scalerCI = scalerCI, estimatedArea = estimatedArea, estimatedAreaSE = estimatedAreaSE, estimatedAreaCI = estimatedAreaCI))
}

aaPropAreaMatrix <- propAreaMatrix(aa, area)

p_accuracy(aaPropAreaMatrix)
u_accuracy(aaPropAreaMatrix)
o_agreement(aaPropAreaMatrix)

aaAreaCorrections <- areaCorrections(aa, area)
