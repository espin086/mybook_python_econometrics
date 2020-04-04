

library("fbr")
library("plyr")
library("dplyr")
library("ggplot2")
library("Hmisc")
library("caret")
library("forecast")

set.seed(2019)

#read in data
query.d1 <- "
SELECT *
FROM 
doc_aia_insights_final_weekly
WHERE ds BETWEEN '2019-12-06' AND '2020-02-07'
ORDER BY advertiser, ds
"


d1 <- presto(query.d1, namespace = 'ad_metrics')

d2 <- d1
d2$key <- paste0(d2$advertiser,d2$advertiser_sfid,d2$advertiser_country,d2$vertical,d2$sub_vertical,d2$channel)

d3 <- d2 %>% 
  filter(vertical %in% c('Automotive') & weekly_daba_spend > 0 & potential_daba_audience_domestic > 0 )

d3$daba.pct.reach <- d3$weekly_daba_reach_domestic/d3$potential_daba_audience_domestic
d3$daba.cvr <- d3$conversions_number_daba/d3$potential_daba_audience_domestic


upload.data.to.hive(d3,tablename="doc_aia_actual_data",namespace="ad_metrics")

###COMPONENT CURVE SELECTION

#Create a summary table of unique advertisers in data along with min/max percent reach and number of weeks of data
advertiser.list <- d3 %>% 
  group_by(advertiser) %>%
  summarise(min = min(daba.pct.reach), max = max(daba.pct.reach), count = n(), avg = mean(daba.pct.reach))

advertiser.list <- advertiser.list %>%
  filter(count > 2)

advertiser.list$range <- advertiser.list$max - advertiser.list$min

advertiser.list.all <- advertiser.list

#Apply data filters to ensure robust curve with at least 4 data points that are spread out
advertiser.list <- advertiser.list %>%
  filter(max > .1 & count > 4 & range > .005)

advertiser.list <- as.data.frame(advertiser.list)

#write.csv(advertiser.list,"advertiser_list.csv")

#qplot(daba.pct.reach, daba.cvr, data=d3[which(d3$advertiser == "Bed Bath & Beyond Inc"), c("daba.pct.reach", "daba.cvr")], main="")

#for (i in 1:nrow(advertiser.list)) {
#  q <- qplot(daba.pct.reach, daba.cvr, data=d3[which(d3$advertiser == advertiser.list[i,"advertiser"]), c("daba.pct.reach", "daba.cvr")], main=advertiser.list[i,"advertiser"])
#  print(q)
#}

#for (i in 2:20) {
#  q <- qplot(daba.pct.reach, daba.cvr, data=d3[which(d3$advertiser == new.advertiser.list[i,"advertiser"]), c("daba.pct.reach", "daba.cvr")], main=advertiser.list[i,"advertiser"])
#  print(q)
#}

#Split advertisers into training and testing sets
smp.size <- floor(0.9 * nrow(advertiser.list))
train.ind <- sample(seq_len(nrow(advertiser.list)), size = smp.size)
advertiser.train <- advertiser.list[train.ind,]
advertiser.test <- advertiser.list[-train.ind,]

#Filter data set to only include advertisers meeting criteria above & in training set
d3.reg <- d3 %>% 
  filter(advertiser %in% advertiser.train$advertiser)

#Create x^2 for regression fitting
d3.reg$daba.pct.reach.sq <- d3.reg$daba.pct.reach*d3.reg$daba.pct.reach

#Set x break points for fit data and create data set
daba.pct.reach <- seq(.005, .1, by=.005)
fit.data <- as.data.frame(daba.pct.reach)
fit.data$daba.pct.reach.sq <- fit.data$daba.pct.reach*fit.data$daba.pct.reach

#Create data set for slope values
slope.data <- fit.data

#Fit curve equations for all qualifying advertisers; note exculsion for beta2 >=0 in loop
for (i in 1:nrow(advertiser.train)) {
  d3.reg.loop <- d3.reg[which(d3.reg$advertiser == advertiser.train[i,"advertiser"]),]
  glm.obj <- glm(daba.cvr ~ daba.pct.reach + daba.pct.reach.sq -1, data = d3.reg.loop) # inclusion of -1 sets intercept to 0
  glm.obj$coefficients[2] <- ifelse(is.na(glm.obj$coefficients[2]),0,glm.obj$coefficients[2])
  if ( (glm.obj$coefficients[1] > 0) & (glm.obj$coefficients[2] < 0) ) {
    fit.data$y <- predict(glm.obj, newdata = fit.data)
    names(fit.data)[ncol(fit.data)] <- paste(advertiser.train[i,"advertiser"])
  }
}

#Calculate slope values
for (c in 3:ncol(fit.data)) {
  slope.data[1,c] <- NA
  for (r in 2:nrow(fit.data)) {
    slope.data[r,c] <- (fit.data[r,c]/fit.data[r-1,c])-1
  }
}
names(slope.data) <- names(fit.data)

fit.data.t <- as.data.frame(t(fit.data[,3:ncol(fit.data)]))
slope.data.t <- as.data.frame(t(slope.data[,3:ncol(slope.data)]))

#where slope becomes negative, set to previous value
for (c in 3:ncol(slope.data.t)) {
  slope.data.t[,c] <- ifelse(slope.data.t[,c] < 0, slope.data.t[,c-1], slope.data.t[,c])
}

#combine fit & slope data into dataset for clustering
cluster.data <- as.data.frame(cbind(fit.data.t,slope.data.t[,2:ncol(slope.data.t)]))
names(cluster.data) <- c('X1','X2','X3','X4','X5','X6','X7','X8','X9','X10','X11','X12','X13','X14','X15','X16','X17','X18','X19','X20','M2','M3','M4','M5','M6','M7','M8','M9','M10','M11','M12','M13','M14','M15','M16','M17','M18','M19','M20')


###CLUSTERING
#z-score normalization
cluster.data.z <- scale(cluster.data, center = TRUE, scale = TRUE)

wss <- (nrow(cluster.data.z)-1)*sum(apply(cluster.data.z,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(cluster.data.z, centers=i)$withinss)
#plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

fit <- kmeans(cluster.data.z, 4) # number of cluster solution
#table(fit$cluster)

#fviz_cluster(fit, data = cluster.data.z)


cluster.assignments <- as.data.frame(fit$cluster)
cluster.assignments$advertiser <- rownames(cluster.assignments)
#write.csv(cluster.assignments,"cluster_assignments.csv")

#merge cluster assignments onto cluster data sets and get cluster means
temp1 <- cluster.data
temp1$advertiser <- rownames(temp1)
temp2 <- merge(x = temp1, y = cluster.assignments, by = "advertiser")
temp2$cluster.num <- temp2[,"fit$cluster"]
cluster.means <- temp2[,!names(temp2) %in% c("advertiser","fit$cluster")] %>% 
  group_by(cluster.num) %>%
  summarise_all(funs(mean))
#write.csv(cluster.means, "clusters_means.csv")

#find equation of cluster lines
cluster.means.reg <- as.data.frame(t(cluster.means[,2:21]))
names(cluster.means.reg) <- c("c1", "c2", "c3", "c4")
cluster.means.reg <- cbind(daba.pct.reach,cluster.means.reg)
cluster.means.reg$daba.pct.reach.sq <- cluster.means.reg$daba.pct.reach*cluster.means.reg$daba.pct.reach
for (i in 2:(ncol(cluster.means.reg)-1)) {
  assign(paste0("glm.obj.",i),glm(cluster.means.reg[,i] ~ daba.pct.reach + daba.pct.reach.sq -1, data = cluster.means.reg))
}



###VALIDATION 1 - TRAINING DATA
#for each training advertiser, for each x, find y with each of the curves

advertiser.train.data <- d3 %>% 
  filter(advertiser %in% advertiser.train$advertiser)
advertiser.train.data$daba.pct.reach_sq <- advertiser.train.data$daba.pct.reach*advertiser.train.data$daba.pct.reach

curve.predictions <- data.frame()
full.val.act <- as.numeric()
full.val.pred <- as.numeric()

for (i in 1:nrow(advertiser.train)) {
  advertiser.train.data.subset <- advertiser.train.data[which(advertiser.train.data$advertiser == advertiser.train[i,"advertiser"]),]
  sim.data <- data.frame()
  for (j in 1:nrow(advertiser.train.data.subset)) {
    x <- advertiser.train.data.subset[j,"daba.pct.reach"]
    x2 <- advertiser.train.data.subset[j,"daba.pct.reach_sq"]
    sim.data[1,j] <- advertiser.train.data.subset[j,"daba.cvr"]
    sim.data[2,j] <- glm.obj.2$coefficients['daba.pct.reach']*x + glm.obj.2$coefficients['daba.pct.reach.sq']*x2
    sim.data[3,j] <- glm.obj.3$coefficients['daba.pct.reach']*x + glm.obj.3$coefficients['daba.pct.reach.sq']*x2
    sim.data[4,j] <- glm.obj.4$coefficients['daba.pct.reach']*x + glm.obj.4$coefficients['daba.pct.reach.sq']*x2
    sim.data[5,j] <- glm.obj.5$coefficients['daba.pct.reach']*x + glm.obj.5$coefficients['daba.pct.reach.sq']*x2
    sim.data[6,j] <- x
  }
  corr.data <- numeric()
  for (k in 2:nrow(sim.data)-1) {
    #corr.data[k-1] <- cor(as.numeric(sim.data[1,]),as.numeric(sim.data[k,]))
    corr.data[k-1] <- dist(rbind(as.numeric(sim.data[1,]),as.numeric(sim.data[k,])))
  }
  curve.predictions[nrow(curve.predictions)+1,"advertiser"] <- advertiser.train[i,"advertiser"]
  curve.predictions[nrow(curve.predictions),"curve.pred"] <- which(corr.data==min(corr.data))
  #plot.data <- as.data.frame(t(sim.data))
  #names(plot.data) <- c("actual", "c1", "c2", "c3", "c4", "x")
  #plot(plot.data$x, plot.data$actual, ylim = range(c(plot.data$actual, plot.data$c1, plot.data$c2, plot.data$c3, plot.data$c4)), col="green", main=advertiser.train[i,"advertiser"], pch=17)
  #points(plot.data$x, plot.data$c1, col='orange', pch=16)
  #points(plot.data$x, plot.data$c2, col='blue', pch=16)
  #points(plot.data$x, plot.data$c3, col='purple', pch=16)
  #points(plot.data$x, plot.data$c4, col='yellow', pch=16)
  #points(plot.data$x, plot.data[,paste0("c",which(corr.data==min(corr.data)))], col='red', pch=16)
  
  #create data set for validation
  full.val.act <- c(full.val.act, t(sim.data[1,]))
  full.val.pred <- c(full.val.pred, t(sim.data[1+which(corr.data==min(corr.data)),]))
  
}

#MAE(full.val.act,full.val.pred)

curve.predictions.train <- curve.predictions


###VALIDATION 2 - TESTING DATA
#for each testing advertiser, for each x, find y with each of the curves

advertiser.test.data <- d3 %>% 
  filter(advertiser %in% advertiser.test$advertiser)
advertiser.test.data$daba.pct.reach_sq <- advertiser.test.data$daba.pct.reach*advertiser.test.data$daba.pct.reach

curve.predictions <- data.frame()
full.val.act <- as.numeric()
full.val.pred <- as.numeric()

for (i in 1:nrow(advertiser.test)) {
  advertiser.test.data.subset <- advertiser.test.data[which(advertiser.test.data$advertiser == advertiser.test[i,"advertiser"]),]
  sim.data <- data.frame()
  for (j in 1:nrow(advertiser.test.data.subset)) {
    x <- advertiser.test.data.subset[j,"daba.pct.reach"]
    x2 <- advertiser.test.data.subset[j,"daba.pct.reach_sq"]
    sim.data[1,j] <- advertiser.test.data.subset[j,"daba.cvr"]
    sim.data[2,j] <- glm.obj.2$coefficients['daba.pct.reach']*x + glm.obj.2$coefficients['daba.pct.reach.sq']*x2
    sim.data[3,j] <- glm.obj.3$coefficients['daba.pct.reach']*x + glm.obj.3$coefficients['daba.pct.reach.sq']*x2
    sim.data[4,j] <- glm.obj.4$coefficients['daba.pct.reach']*x + glm.obj.4$coefficients['daba.pct.reach.sq']*x2
    sim.data[5,j] <- glm.obj.5$coefficients['daba.pct.reach']*x + glm.obj.5$coefficients['daba.pct.reach.sq']*x2
    sim.data[6,j] <- x
  }
  corr.data <- numeric()
  for (k in 2:nrow(sim.data)-1) {
    #corr.data[k-1] <- cor(as.numeric(sim.data[1,]),as.numeric(sim.data[k,]))
    corr.data[k-1] <- dist(rbind(as.numeric(sim.data[1,]),as.numeric(sim.data[k,])))
  }
  curve.predictions[nrow(curve.predictions)+1,"advertiser"] <- advertiser.test[i,"advertiser"]
  curve.predictions[nrow(curve.predictions),"curve.pred"] <- which(corr.data==min(corr.data))
  #plot.data <- as.data.frame(t(sim.data))
  #names(plot.data) <- c("actual", "c1", "c2", "c3", "c4", "x")
  #plot(plot.data$x, plot.data$actual, ylim = range(c(plot.data$actual, plot.data$c1, plot.data$c2, plot.data$c3, plot.data$c4)), col="green", main=advertiser.test[i,"advertiser"], pch=17)
  #points(plot.data$x, plot.data$c1, col='orange', pch=16)
  #points(plot.data$x, plot.data$c2, col='blue', pch=16)
  #points(plot.data$x, plot.data$c3, col='purple', pch=16)
  #points(plot.data$x, plot.data$c4, col='yellow', pch=16)
  #points(plot.data$x, plot.data[,paste0("c",which(corr.data==min(corr.data)))], col='red', pch=16)
  
  #create data set for validation
  full.val.act <- c(full.val.act, t(sim.data[1,]))
  full.val.pred <- c(full.val.pred, t(sim.data[1+which(corr.data==min(corr.data)),]))
  
}

#MAE(full.val.act,full.val.pred)

curve.predictions.test <- curve.predictions

###CURVE FITTING

#find most similar cluster
#for each advertiser, for each x, find y with each of the curves
new.advertiser.list <- advertiser.list.all[!(advertiser.list.all$advertiser %in% advertiser.list$advertiser),]
new.advertiser.list <- as.data.frame(new.advertiser.list)
new.data <- d3 %>% 
  filter(advertiser %in% new.advertiser.list$advertiser)
new.data$daba.pct.reach_sq <- new.data$daba.pct.reach*new.data$daba.pct.reach

curve.predictions <- data.frame()
new.val.act <- as.numeric()
new.val.pred <- as.numeric()
for (i in 1:nrow(new.advertiser.list)) {
  new.data.subset <- new.data[which(new.data$advertiser == new.advertiser.list[i,"advertiser"]),]
  sim.data <- data.frame()
  for (j in 1:nrow(new.data.subset)) {
    x <- new.data.subset[j,"daba.pct.reach"]
    x2 <- new.data.subset[j,"daba.pct.reach_sq"]
    sim.data[1,j] <- new.data.subset[j,"daba.cvr"]
    sim.data[2,j] <- glm.obj.2$coefficients['daba.pct.reach']*x + glm.obj.2$coefficients['daba.pct.reach.sq']*x2
    sim.data[3,j] <- glm.obj.3$coefficients['daba.pct.reach']*x + glm.obj.3$coefficients['daba.pct.reach.sq']*x2
    sim.data[4,j] <- glm.obj.4$coefficients['daba.pct.reach']*x + glm.obj.4$coefficients['daba.pct.reach.sq']*x2
    sim.data[5,j] <- glm.obj.5$coefficients['daba.pct.reach']*x + glm.obj.5$coefficients['daba.pct.reach.sq']*x2
    sim.data[6,j] <- x
  }
  corr.data <- numeric()
  for (k in 2:nrow(sim.data)-1) {
    #corr.data[k-1] <- cor(as.numeric(sim.data[1,]),as.numeric(sim.data[k,]))
    corr.data[k-1] <- dist(rbind(as.numeric(sim.data[1,]),as.numeric(sim.data[k,])))
  }
  curve.predictions[nrow(curve.predictions)+1,"advertiser"] <- new.advertiser.list[i,"advertiser"]
  curve.predictions[nrow(curve.predictions),"curve.pred"] <- which(corr.data==min(corr.data))
  #plot.data <- as.data.frame(t(sim.data))
  #names(plot.data) <- c("actual", "c1", "c2", "c3", "c4", "x")
  #plot(plot.data$x, plot.data$actual, ylim = range(c(plot.data$actual, plot.data$c1, plot.data$c2, plot.data$c3, plot.data$c4)), col="black", main=new.advertiser.list[i,"advertiser"], pch=17)
  #points(plot.data$x, plot.data$c1, col='orange', pch=16)
  #points(plot.data$x, plot.data$c2, col='blue', pch=16)
  #points(plot.data$x, plot.data$c3, col='purple', pch=16)
  #points(plot.data$x, plot.data$c4, col='brown', pch=16)
  #points(plot.data$x, plot.data[,paste0("c",which(corr.data==min(corr.data)))], col='red', pch=16)
  
  #create data set for validation
  new.val.act <- c(new.val.act, t(sim.data[1,]))
  new.val.pred <- c(new.val.pred, t(sim.data[1+which(corr.data==min(corr.data)),]))
  
}

###VALIDATION 3 (setup in loop above)
#MAE(new.val.act,new.val.pred)

curve.predictions.new <- curve.predictions

###EXTRAPOLATION

#extraploate curves and find ceilings
extrap.x <- seq(.005, 1, by=.005)
extrap.data <- as.data.frame(extrap.x)
names(extrap.data) <- c("x")
for (i in 2:5) {
  for (j in 1:nrow(extrap.data)) {
    x <- extrap.data[j,"x"]
    x2 <- x*x
    extrap.data[j,i] <- get(paste0("glm.obj.",i))$coefficients['daba.pct.reach']*x + get(paste0("glm.obj.",i))$coefficients['daba.pct.reach.sq']*x2
  }
}

#write.csv(extrap.data,"extrap.data.csv")

curve.ceilings <- as.data.frame(c(1,2,3,4))
names(curve.ceilings) <- c("curve.pred")

for (i in 2:ncol(extrap.data)) {
  curve.ceilings[i-1,"curve.ceiling"] <- extrap.data[which.max(extrap.data[,i]),"x"]
  curve.ceilings[i-1,"ceiling.cvr"] <- extrap.data[which.max(extrap.data[,i]),i]
}

#sales headroom
new.advertiser.max.actual <- d3 %>% 
  filter(weekly_daba_spend > 0) %>%
  group_by(advertiser, vertical, sub_vertical) %>%
  summarise(max.actual = max(daba.pct.reach), max.spend = max(weekly_daba_spend))

new.advertiser.max.actual.curve <- merge(curve.predictions,new.advertiser.max.actual,by="advertiser")
new.advertiser.max.actual.pred <- merge(new.advertiser.max.actual.curve, curve.ceilings, by="curve.pred")
new.advertiser.max.actual.pred$headroom <- new.advertiser.max.actual.pred$curve.ceiling/new.advertiser.max.actual.pred$max.actual
new.advertiser.max.actual.pred$scale <- (new.advertiser.max.actual.pred$headroom*new.advertiser.max.actual.pred$max.spend) - new.advertiser.max.actual.pred$max.spend

#sum(new.advertiser.max.actual.pred$scale)

retail.fcst <- new.advertiser.max.actual.pred %>% 
  filter(vertical == 'Retail')
#sum(retail.fcst$scale)

superstores <- c("Toy & Hobby", "Home Improvement", "Home & Office", "Grocery, Drug & Convenience")
superstores.fcst <- new.advertiser.max.actual.pred %>% 
  filter(vertical == 'Retail' 
         & sub_vertical %in% superstores)
#sum(superstores.fcst$scale)

deptmall.fcst <- new.advertiser.max.actual.pred %>% 
  filter(vertical == 'Retail' 
         & !sub_vertical %in% superstores)
#sum(deptmall.fcst$scale)


#Final Data

extrap.data.ceiladj <- extrap.data
names(extrap.data.ceiladj) <- c("x","c1","c2","c3","c4")
for (i in 2:ncol(extrap.data.ceiladj)) {
  for (j in 1:nrow(extrap.data.ceiladj)) {
    extrap.data.ceiladj[j,i] <- ifelse(extrap.data.ceiladj[j,1] > curve.ceilings[i-1,"curve.ceiling"], curve.ceilings[i-1,"ceiling.cvr"], extrap.data.ceiladj[j,i])
  }
}

#write.csv(extrap.data.ceiladj,"extrap_data_ceiladj.csv")

curve.predictions.all <- rbind(curve.predictions.train, curve.predictions.test, curve.predictions.new)

final.data <- as.data.frame(matrix(ncol = 3))
names(final.data) <- c("advertiser", "pct_reach_daba_potential", "cvr_daba_potential")
for (i in 1:nrow(curve.predictions.all)) {
  loop.advertiser <- curve.predictions.all[i,"advertiser"]
  loop.curve <- paste0("c",as.numeric(curve.predictions.all[i,"curve.pred"]))
  loop.curve.data <- extrap.data.ceiladj[,names(extrap.data.ceiladj) %in% c("x",loop.curve)]
  loop.data <- cbind(loop.advertiser, loop.curve.data)
  names(loop.data) <- c("advertiser", "pct_reach_daba_potential", "cvr_daba_potential")
  final.data <- rbind(final.data, loop.data)
}    

final.data <- final.data[2:nrow(final.data),]

meta <- d3 %>% 
  group_by(advertiser, advertiser_sfid, advertiser_country, vertical, sub_vertical, channel) %>%
  summarise(count = n())
meta$count <- NULL
meta.dups <- meta %>% 
  group_by(advertiser) %>% 
  summarise(n.adv.rcds= n())
duplicates <- meta.dups %>%
  filter(n.adv.rcds > 1)

final.data.meta <- merge(meta[,names(meta) %in% c("advertiser", "advertiser_sfid")], final.data[which(!final.data$advertiser %in% duplicates$advertiser),], by = "advertiser")
final.data.dups <- final.data[which(final.data$advertiser %in% duplicates$advertiser),]
final.data.all <- rbind.fill(final.data.meta,final.data.dups)

#Final output tables #1 - this is the bulk of the output - can plot the expected curve for advertiser data
upload.data.to.hive(final.data.all,tablename="doc_aia_curves_out",namespace="ad_metrics")

#doc_da_summary
final.data.summary <- merge(curve.predictions.all, curve.ceilings, by = "curve.pred")
names(final.data.summary) <- c("curve_predicted", "advertiser", "pct_reach_daba_max_potential", "cvr_daba_max_potential")

final.data.summary.meta <- merge(meta[,names(meta) %in% c("advertiser", "advertiser_sfid")], final.data.summary[which(!final.data.summary$advertiser %in% duplicates$advertiser),], by = "advertiser")
final.data.summary.dups <- final.data.summary[which(final.data.summary$advertiser %in% duplicates$advertiser),]
final.data.summary.all <- rbind.fill(final.data.summary.meta,final.data.summary.dups)
#Final output tables #2
upload.data.to.hive(final.data.summary.all,tablename="doc_aia_summary",namespace="ad_metrics")

#table(final.data.summary$curve_predicted)


#pilot account selection
final.data.summary.2 <- merge(final.data.summary, advertiser.list.all, by = "advertiser")
colnames(final.data.summary.2)[which(names(final.data.summary.2) == "max")] <- "pct_reach_daba_max_actual"
final.data.summary.2$headroom <- final.data.summary.2$pct_reach_daba_max_potential - final.data.summary.2$pct_reach_daba_max_actual
final.data.summary.2$headroom_pct <- (1 - (final.data.summary.2$pct_reach_daba_max_actual/final.data.summary.2$pct_reach_daba_max_potential))
final.data.summary.2 <- final.data.summary.2[,!names(final.data.summary.2) %in% c("min", "advertiser.y")]

final.data.summary.2.meta <- merge(meta, final.data.summary.2[which(!final.data.summary.2$advertiser %in% duplicates$advertiser),], by = "advertiser")
final.data.summary.2.dups <- final.data.summary.2[which(final.data.summary.2$advertiser %in% duplicates$advertiser),]
final.data.summary.2.final <- rbind.fill(final.data.summary.2.meta,final.data.summary.2.dups)
final.data.summary.2.final.us <- final.data.summary.2.final %>% filter(advertiser_country == "US")
#write.csv(final.data.summary.2.final.us,"doc_summary_data.csv")

#tableau.data <- d3[which(d3$advertiser_country=="US" & d3$channel!="SMB Unmanaged"), !names(d3) %in% c("accounts")]
#colnames(tableau.data)[which(names(tableau.data) == "daba.pct.reach")] <- "daba_pct_reach"
#colnames(tableau.data)[which(names(tableau.data) == "daba.cvr")] <- "daba_cvr"


upload.data.to.hive(tableau.data,tablename="af_tableau_aia_data",namespace="ad_metrics")


#write.csv(tableau.data,"tableau_data.csv")




