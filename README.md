# Whale Optimization Algorithm in Data Clustering
Code for data clustering with the whale optimization algorithm and lamarickan learning.

## Setup environment


See in [this blogpost](https://medium.com/analytics-vidhya/how-to-install-roracle-on-windows-10-144b0b923dac) how to setup RStudio with ROracle to get the data out of a database. 

## How to cluster your data

### Scaling of the data

```
datasc <- scale(data)
View(datasc)
```

### Input parameters for WOA

The opjective function of the WOA is the sphere function

![Sphere Function](images/sphere-function.jpg "Sphere Function")


#### Define sphere function as objective function
```
sphere <- function(x){
    return(sum(x^2))
}
```
#### Define parameters
```
numVar <- 5
rangeVar <- matrix(c(-10,10), nrow=2)
```
#### calculate the optimum solution using Whale Optimization Algorithm
```
resultWOA <- WOA(sphere, optimType="MIN", numVar, numPopulation=20, maxIter=100, rangeVar)
```

#### calculate the optimum value using sphere function
```
optimum.value <- sphere(resultWOA)
```