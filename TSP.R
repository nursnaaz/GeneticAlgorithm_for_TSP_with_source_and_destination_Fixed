###########################################################################################################
#####################################GENETIC ALGORITHM FOR TRAVELLING SALES MAN PROBLEM WITH SOURCE AND DESTINATION FIXED############################################################
###########################################################################################################


######################################################################################################
#To remove previous history and data
rm(list=ls(all=T))


#Set the working directory to fetch data
#setwd("~/Desktop/GNQ4_AI/20170305_Batch25_CSE7318_MCS_GA_Lab")

#Read data into R dataframe
data = read.csv("distanceInfoGNQ.csv",header=T)
str(data)

##Removing the cities column
remove <- c('Cities')
data <- data[,setdiff(names(data),remove)]
str(data)

# generate TSP Route plan to cover
getRoute <- function(){
  return(sample(3:12,10))
}

#####################################################################################################
## :::::::::::::::::::::::::::::::::COMMENT :::::::::::::::::::::::::::::::::::::::::::::::::::::::::
## getMutatedPath will define mutaion policy.
#####################################################################################################
getMutatedPath <- function(initPath, mutateFactor){
  
  temp1 <- initPath[1:mutateFactor]
  temp2 <- initPath[(mutateFactor+1):length(initPath)]
  newPath <- as.vector(c(temp2,temp1))
  return(newPath)
  
}

#####################################################################################################
## :::::::::::::::::::::::::::::::::COMMENT :::::::::::::::::::::::::::::::::::::::::::::::::::::::::
## getCrossoverOffspring :: This funtion will pick 2 parents
## and will facilitate the crossover arrangements using BCRC(BEST COST  ROUTE CROSSOVER OPERATION).
#####################################################################################################
getCrossoverOffspring <- function(selectedParent_1, selectedParent_2){
  ## Selected 2 parent
  parent1 <- selectedParent_1
  parent2 <- selectedParent_2
  ## Fetch out the route traversed by these parents
  parent1path <- parent1[[1]]
  parent2path <- parent2[[1]]
  ## cromosome or city selection for crossover
  crossoverElements1 <- parent1[[1]][2:4]
  crossoverElements2 <- parent2[[1]][2:4]
  ## Initialization of the new paths with without crossover cromosome/cities
  path1 <- parent1path[! (parent1path %in% crossoverElements2)]
  path2 <- parent2path[! (parent2path %in% crossoverElements1)]
  
  ## Crossover arrangements using BCRC(BEST COST  ROUTE CROSSOVER OPERATION).
  subElitePopulationWthCost <- NULL
  subRouteCost <- NULL
  for(i in 1:length(crossoverElements2)){
    subRouteCost <- NULL
    subElitePopulationWthCost <- NULL
    for( j in 1:length(path1)) {
      newPath <- append(path1,crossoverElements2[i],after=j)
      currentRouteMap <- newPath
      if(is.null(subRouteCost)){
        subRouteCost <- list(getRouteCost(currentRouteMap))
      }else{
        subRouteCost <- append(subRouteCost,list(getRouteCost(currentRouteMap)))
      }
      subRouteCost <- unique(subRouteCost, '[[',1)
      subRouteCost <- subRouteCost[order(sapply(subRouteCost, '[[',2))]
    }
    subElitePopulationWthCost <- subRouteCost[[1]]
    path1 <- subElitePopulationWthCost[[1]]
  }
  "routeMap" <- subElitePopulationWthCost[[1]]
  "totalCostDetails" <- subElitePopulationWthCost[[2]]
  depotAndCostDetails <- list(routeMap, totalCostDetails)
  return(depotAndCostDetails)
}

#####################################################################################################
## :::::::::::::::::::::::::::::::::COMMENT :::::::::::::::::::::::::::::::::::::::::::::::::::::::::
## getRouteCost :: Function to compute the route cost using Random path or using the already defined 
## path 
#####################################################################################################
getRouteCost <- function(pvRouteMap){
  traverseData <- data
  sourcePoint <- 1
  stopsCovered <- 0
  routeCost <- 0
  endPoint <- 2
  routeMap <- pvRouteMap
  while (stopsCovered < length(pvRouteMap)) {
    routeCost <- routeCost + traverseData[sourcePoint, routeMap[stopsCovered+1]]
    traverseData[sourcePoint, routeMap[stopsCovered+1]] <- 9999
    sourcePoint = routeMap[stopsCovered+1]
    stopsCovered <- stopsCovered+1
  }
  ## Fixing the destination as 2 the endpoint
  routeCost <- routeCost + traverseData[routeMap[stopsCovered],endPoint]
  
  
  "routeMap" <- routeMap
  "totalCostDetails" <- routeCost
  depotAndCostDetails <- list(routeMap, totalCostDetails)
  return(depotAndCostDetails)
}

############################################################################################################
print("####################################################################################################")
cat(" Starting the process to calculate ELITE set of parents", "\n")
Sys.time()
print("####################################################################################################")
# Calculate elite Population Matrix with Cost
elitePopulationWthCost <- NULL
routeCost <- NULL
for(elitePopColl in 1:10){
  currentRouteMap <- getRoute()
  if(is.null(routeCost)){
    routeCost <- list(getRouteCost(currentRouteMap))
  }else{
    routeCost <- append(routeCost,list(getRouteCost(currentRouteMap)))
  }
  routeCost <- unique(routeCost, '[[',1)
  routeCost <- routeCost[order(sapply(routeCost, '[[',2))]
}
elitePopulationWthCost <- routeCost[1:5]
print(elitePopulationWthCost)

############################################################################################################
print("####################################################################################################")
cat(" Completing  the process to calculate ELITE set of parents using Dynamic programming @ ", "\n")
Sys.time()
print("####################################################################################################")
########################################################################################################################################################################################################################


print("####################################################################################################")
cat(" Startingthe process to calculate solution using GENATIC ALGORITHM @ ",  "\n")
Sys.time()
cat(" BENCHMARK SOLUTION before GA", "\n")
elitePopulationWthCost[1]
print("####################################################################################################")

childPopulationWthCost <- NULL
genrItr = 0   # Number of generations to iterate
gaPickCntr <- 0 
matingFactor = runif(9000,0,1) # Random pick to decide on Mutation / crossover
while(genrItr < 10){  ## Performing n Iteration to test the convergence of the solution
  cat("genrItr", genrItr,"\n")
  routeCost <- NULL
  # Pick the set of 10 new parents after Mutation / crossover
  while(length(routeCost) < 10){
    pickedRouteMap <- elitePopulationWthCost[sample(1:5,1)][[1]]
    gaPickCntr = gaPickCntr + 1
    if(matingFactor[gaPickCntr] < .15 ){ # Mutate if the matingFactor < .1
      print("Mutation")
      mutatedRouteMap <- getMutatedPath(pickedRouteMap[[1]],sample(1:9,1))
      if(is.null(routeCost)){
        routeCost <- list(getRouteCost(mutatedRouteMap))
      }else{
        routeCost <- append(routeCost,list(getRouteCost(mutatedRouteMap)))
      }
    }
    else{# Crossover if the matingFactor > .1
      print("CrossOver")
      selectedParent_1 <- elitePopulationWthCost[sample(1:5,1)][[1]]
      selectedParent_2 <- elitePopulationWthCost[sample(1:5,1)][[1]]
      newOffSpring <- getCrossoverOffspring(selectedParent_1, selectedParent_2)
      if(is.null(routeCost)){
        routeCost<- list(newOffSpring)
      }else{
        routeCost<- append(routeCost,list(newOffSpring))
      }
    }
    routeCost <- unique(routeCost, '[[',1)
    routeCost <- routeCost[order(sapply(routeCost, '[[',2))]
  }
  genrItr <- genrItr + 1 # Increase the iteration constant
  elitePopulationWthCost<- append(elitePopulationWthCost,routeCost[1:3]) # Append the new set of parents to Elite Group
  elitePopulationWthCost <- unique(elitePopulationWthCost, '[[',1) # Verify and remove if there is any duplicate routemap
  elitePopulationWthCost <- elitePopulationWthCost[order(sapply(elitePopulationWthCost, '[[',2))] # Order the Elite Parents w.r.t Cost
  elitePopulationWthCost <- elitePopulationWthCost[1:5] # Pick new top 5 Elite Parents
  print(elitePopulationWthCost) # Print Top 5 Elite parents.
}

print("####################################################################################################")
cat(" Completing the process to calculate solution using GENATIC ALGORITHM @ ",  "\n")
Sys.time()
cat(" FINAL SOLUTION USING GA with Dynamic Programming",  "\n")
elitePopulationWthCost[1]
print("####################################################################################################")

#Best Route 
#[1]  1 2 3  6  8 11  7 12 10  5  9  4
#Cost
# 659

