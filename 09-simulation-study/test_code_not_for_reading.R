## deliverable 9 test for the last part


library(dplyr)
N <- 201
shape.set <- 1.4
scale.set<-3

true.norm.med <- qnorm(0.5)
true.norm.min <- mean(apply(array(rnorm(N*10000), dim = c(N, 10000)), 2, min))
true.gamma.med <- qgamma(0.5,shape=shape.set,scale=scale.set)
true.gamma.min <- mean(apply(array(rgamma(N*10000, shape = shape.set, scale = scale.set), dim = c(N, 10000)), 2, min))


## Apply what we have for each different combination
dist1 = c("norm","gamma")
model1=c("MMnorm","MMgamma","KDE","Boot")
par.int1=c("median","min")


cover <- rep(NA)
cover.mean <- rep(NA)
m <- 0
for (i in length(dist1)) {
  for (j in length(model1)){
    for (k in length(par.int1)){
      for(sims in 1:10){
        
        if (dist1 == "norm" & par.int1 == "median"){
          cover[sims] <- generate_data(N,dist = dist1[i], shape.set,scale.set) %>% estimate.ci(mod=model1[j],par.int=par.int1[k],R=1000) %>% capture_par(true.par=true.norm.med)
          print(cover[sims])
        }
        else if (dist1 == "norm" & par.int1 == "min"){
          cover[sims] <- generate_data(N,dist = dist1[i], shape.set,scale.set) %>% estimate.ci(mod=model1[j],par.int=par.int1[k],R=1000) %>% capture_par(true.par=true.norm.min) 
        }
        else if (dist1 == "gamma" & par.int1 == "median"){
          cover[sims] <- generate_data(N,dist = dist1[i], shape.set,scale.set) %>% estimate.ci(mod=model1[j],par.int=par.int1[k],R=1000) %>% capture_par(true.par=true.gamma.med)   
        }
        else {
          cover[sims] <- generate_data(N,dist = dist1[i], shape.set,scale.set) %>% estimate.ci(mod=model1[j],par.int=par.int1[k],R=1000) %>% capture_par(true.par=true.gamma.min)     
        }
      }
      
      for (m in range(16)){
        cover.mean[m] <- mean(cover)
        m = m+1
      }
    }
  }
}
cover.mean

