
library(readxl)
BD<- read_excel("C:/Users/Sevas/Desktop/PI 2/BD_Rubik_V1_transformadaf.xlsx")
s<-read_excel("C:/Users/Sevas/Desktop/PI 2/BD_Rubik_V1_transformadaf.xlsx")
BD<-BD[,-c(11,12,13)]


getPkg <-function(pkg, url = "http://cran.r-project.org"){
  to.do <- setdiff(pkg, installed.packages()[,1])
  for(package in to.do) invisible(install.packages(package, repos = url))
}
pkgs <- c("glmnet","dplyr","ggplot2","lattice","outliers","caret","ROCR","nortest","nnet","gridExtra","Rtsne",
          "MASS","lme4","SparseM","car","leaps","easypackages","pscl","MASS","tidyr","reticulate","reshape2",
          "gvlma","olsrr","RcmdrMisc","kableExtra","clusterSim","stringr","summarytools","formattable",
          "cluster","factoextra","pracma","clValid","NbClust","frbs","e1017")
getPkg(pkgs)
library(easypackages)
libraries(pkgs)
library(janitor)

BD%>%clean_names()->bd

#data<-bd[,-c(2)]


# wssplot <- function(data, nc=15, seed=1152189889){
#   wss <- (nrow(data)-1)*sum(apply(data,2,var))
#   for (i in 2:nc){
#     set.seed(seed)
#     wss[i] <- sum(kmeans(data, centers=i)$withinss)}
#   plot(1:nc, wss, type="b", xlab="Número de clusters",
#        ylab="suma de cuadrados",main="Número óptimo de clusters")}
# ```

bd$genero<-ifelse(bd$genero=='Masculino',1,0)

library(dummies)
library(fastDummies)

#bd<-dummy.data.frame(bd,names = c('estado_civil'),sep='_')

#bd=data.frame()




bd$estado_civil<-ifelse(bd$estado_civil=="Casado/a",'Casado',ifelse(bd$estado_civil== "Soltero/a",'Soltero',
                                                                    ifelse(bd$estado_civil=="Unión marital de hecho",'Union marital',
                                                                           'Otros')))

bd$estado_civil<-ifelse(is.na(bd$estado_civil),'Otros',bd$estado_civil)
unique(bd$estado_civil)





#summary(bd2)

bd2<-dummy_cols(bd[,-c(1,6,9,10)],select_columns = c('estado_civil'))

bd2<-bd2[,-c(1,2,6)]

library(mice)

bd2$prop_sob<-ifelse(is.na(bd2$prop_sob),0,bd2$prop_sob)
bd2$prop_obj<-ifelse(is.na(bd2$prop_obj),0,bd2$prop_obj)
bd2$prob_fo<-ifelse(is.na(bd2$prob_fo),0,bd2$prob_fo)
bd2$prob_def<-ifelse(is.na(bd2$prob_def),0,bd2$prob_def)





columns<-c('devengado_mean','devengado_sd','devengado_median','devengado_min',
          'devengado_max','variable_mean','variable_sd','variable_median',
          'variable_min','variable_max','deduccion_mean','deduccion_sd','deduccion_median',
          'deduccion_min','deduccion_max','conteo_meses_de_garantizado','conteo_meses_de_desempeno')

imputed_data<-mice(bd2[,names(bd2)%in% columns],m=1,
                   maxit = 1,method = 'mean',seed = 2018,print=F)
complete.data<-mice::complete(imputed_data)

bd3<-subset(bd2,select = -c(devengado_mean,devengado_sd,devengado_min,devengado_median,
                  devengado_max,variable_mean,variable_sd,variable_median,
                  variable_min,variable_max,deduccion_mean,deduccion_sd,deduccion_median,
                  deduccion_min,deduccion_max,conteo_meses_de_garantizado,conteo_meses_de_desempeno))

bd4<-data.frame(bd3,complete.data)

# colSums(is.na(data_stand))
# 
# data_stand<-bd4 %>% mutate_if(is.numeric,scale)
# data_stand<-data.frame(bd[,1],data_stand)

#res.dist<-get_dist(data_stand[,-1],stand = TRUE,method = 'kendall')
#fviz_dist(res.dist,gradient = list(low='#FC4E07',mid='white',high='#069926'))

###### Determinacion optima de Cluster ###########################
########## bajas dimensiones #######################
set.seed(1152)
Tsne <- Rtsne(bd4, dims = 3, perplexity = 100, verbose = TRUE, max_iter = 500,
              eta = 200, check_duplicates = FALSE)
tsne <- data.frame(bd$cedula,Tsne[["Y"]])
names(Tsne) <- c("Ceduda","X1", "X2", "X3")
# train_tsne<-tsne[index,] test_tsne<-tsne[-index,]
my_data <- scale(tsne[,-1])

res.dist<-get_dist(my_data,stand = TRUE,method = 'kendall')
fviz_dist(res.dist,gradient = list(low='#FC4E07',mid='white',high='#069926'))

##########################3
library(NbClust)
par(mfrow = c(1, 1))
#my_data <- scale(df)
set.seed(1152)
res.nbclust <- NbClust(my_data, distance = "manhattan", min.nc = 2, max.nc = 10,
                       method = "ward.D2", index = "all")
n_clus_Ele <- max(res.nbclust$Best.partition)
factoextra::fviz_nbclust(res.nbclust)


fviz_nbclust(my_data, kmeans, method = "silhouette")

##### Cluster Kmeans #############
d<-dist(my_data, method = "manhattan")
km.res<-kmeans(d,n_clus_Ele+1,nstart = 25)
fviz_cluster(km.res,data = my_data,frame.type="confidence")+
  theme_bw() + labs(title = "k-means")



###########################33
pam.res<-pam(my_data,n_clus_Ele+1, metric = "manhattan")
fviz_cluster(pam.res, data = my_data, stand = T, frame.type = "confidence") +
  theme_bw() + labs(title = "k Medoids")
fviz_silhouette(pam.res)+ labs(title = "k Medoids: Silhouette plot")
########## Clara ########333
clarz<-clara(my_data,n_clus_Ele+1, metric = "manhattan")
fviz_cluster(clarz,data = my_data, stand = T, frame.type = "confidence") +
  theme_bw() + labs(title = "Clara")
fviz_silhouette(clarz)+ labs(title = "Clara: Silhouette plot")
################# Fuzzy ####################

fuzzy_cluster <- fanny(x = my_data, k = n_clus_Ele+1, metric = "manhattan")
fviz_cluster(object = fuzzy_cluster, frame.type = "confidence", pallete = "jco") +
  theme_bw() + labs(title = "Fuzzy Cluster plot")
fviz_silhouette(fuzzy_cluster)+ labs(title = "Fuzzy Cluster: Silhouette plot")
# Subtractive y Mountain
# method.type <- "SBC"
# range.data <- matrix(apply(my_data, 2, range), nrow = 2)
# control.SBC <- list(r.a = 0.5, eps.high = 0.5, eps.low = 0.15, name = "Sim-0")
# object.SBC_Subtravive <- frbs.learn(my_data, range.data, method.type, control.SBC)

library("factoextra")
# Compute hierarchical clustering and cut into 4 clusters
res <- hcut(my_data, k = n_clus_Ele+1, stand = TRUE,hc_metric = "manhattan")
# Visualize
fviz_dend(res, rect = TRUE, cex = 0.5,
          k_colors = c("#cf043d", "#c206bf", "#069926"))

##############################################################################################3
###################### validacion ######################################

intern <- clValid(my_data, nClust = 2:n_clus_Ele+1, maxitems = 10000, clMethods = c("hierarchical",
                                                                                  "kmeans", "fanny", "clara", "pam"), metric = "manhattan", validation = "internal")
par(mfrow = c(1, 1))
summary(intern)
plot(intern)


################################ Analisis de supervivecia #####################################

#########################################
# library(survival)
# 
# aaa<-data.frame(id=bd$cedula,duration=s$xt_month,delta=1,grupo=km.res$cluster)
# 
# 
# library("flexsurv")
# 
# Dist <- c("exp", "weibull", "llogis", "gompertz", "genf")
# data.Surv <- Surv(aaa$duration, aaa$delta)
# # Inicia SCRIPT#
# 
# #Riesgo relativo
# par(mfrow=c(1,1))
# model <- sapply(Dist, function(x) flexsurvreg(data.Surv ~ 1, dist = x), USE.NAMES = T, 
#                 simplify = F)
# plot(model[[1]], ci = F, conf.int = F, lty = 2, main = "Ajuste Paramétrico",xlim=c(0,30) ,
#      xlab = "Tiempo", ylab = "Probabilidad de Supervivencia")
# for (i in 2:length(Dist)) plot(model[[i]], ci = F, conf.int = F, add = T, col = i + 
#                                  1, lty = i)
# legend("topright", c("KM", Dist), lty = 1:(length(Dist) + 1), col = 1:(length(Dist) +  1))
# 
# ##Riesgo acumulado
# plot(model[[1]], ci = F, conf.int = F, lty = 2, main = "Ajuste Paramétrico", xlim=c(0,30) , ylim=c(0,1),
#      xlab = "Tiempo", ylab = "Riesgo Acumulado", type = "cumhaz")
# for (i in 2:length(Dist)) plot(model[[i]], ci = F, conf.int = F, add = T, col = i + 
#                                  1, lty = i, type = "cumhaz")
# 
# legend("topleft", c("KM", Dist), lty = 1:(length(Dist) + 1), col = 1:(length(Dist) + 
#                                                                          1))
# 
# ## distancia genf fue la mejor
# 
# model <- aaa[,-1] %>% group_by(grupo) %>% do(flex = flexsurvreg(Surv(duration, delta) ~ 
#                                                                   1, data = ., dist = "genf"))
# 
# KM <- survfit(Surv(duration, delta) ~ grupo, data = aaa[,-1])
# 
# 
# # plot(KM, col = 1:length(model$flex), main = "Comparación Paramétrica", xlab = "Tiempo", 
# #      ylab = "Probabilidad de Supervivencia")
# # for (i in 1:length(model$flex)) lines(model$flex[[i]], ci = F, col = i)
# # legend("topright", c("1","2","3"), lty = 1:(length(model$flex) + 1), col = 1:(length(model$flex) +1))
# # 
# # 
# # 
# # plot(KM, col = 1:length(model$flex), main = "Comparación Paramétrica", xlab = "Tiempo", 
# #      ylab = "Riesgo Acumulado", fun = "cumhaz")
# # for (i in 1:length(model$flex)) lines(model$flex[[i]], ci = F, col = i, type = "cumhaz")
# 
# 
# 
# #########


################################ Analisis de supervivecia #####################################

#########################################
library(survival)

aaa<-data.frame(id=bd$cedula,duration=s$xt_month,delta=1,grupo=km.res$cluster)
aaa2<-data.frame(id=bd$cedula,duration=s$xt_month,delta=1,grupo=res$cluster)
aaa3<-data.frame(id=bd$cedula,duration=s$xt_month,delta=1,grupo=fuzzy_cluster$clustering)

library("flexsurv")

Dist <- c("exp", "weibull", "llogis", "gompertz", "genf")
data.Surv <- Surv(aaa$duration, aaa$delta)
# Inicia SCRIPT#

#Riesgo relativo
par(mfrow=c(1,1))
model <- sapply(Dist, function(x) flexsurvreg(data.Surv ~ 1, dist = x), USE.NAMES = T, 
                simplify = F)
plot(model[[1]], ci = F, conf.int = F, lty = 2, main = "Ajuste Paramétrico",xlim=c(0,30) ,
     xlab = "Tiempo", ylab = "Probabilidad de Supervivencia")
for (i in 2:length(Dist)) plot(model[[i]], ci = F, conf.int = F, add = T, col = i + 
                                 1, lty = i)
legend("topright", c("KM", Dist), lty = 1:(length(Dist) + 1), col = 1:(length(Dist) +  1))

##Riesgo acumulado
plot(model[[1]], ci = F, conf.int = F, lty = 2, main = "Ajuste Paramétrico", xlim=c(0,30) , ylim=c(0,1),
     xlab = "Tiempo", ylab = "Riesgo Acumulado", type = "cumhaz")
for (i in 2:length(Dist)) plot(model[[i]], ci = F, conf.int = F, add = T, col = i + 
                                 1, lty = i, type = "cumhaz")

legend("topright", c("KM", Dist), lty = 1:(length(Dist) + 1), col = 1:(length(Dist) + 
                                                                         1))

## distancia genf fue la mejor

### Kmeans
model <- aaa[,-1] %>% group_by(grupo) %>% do(flex = flexsurvreg(Surv(duration, delta) ~ 
                                                                  1, data = ., dist = "genf"))
KM <- survfit(Surv(duration, delta) ~ grupo, data = aaa[,-1])


plot(KM, col = 1:length(model$flex), main = "K-means: Comparación de clusters", xlab = "Tiempo", 
     ylab = "Probabilidad de Supervivencia", xlim=c(0,30))
for (i in 1:length(model$flex)) lines(model$flex[[i]], ci = F, col = i)
legend("topright", c("1","2","3"), lty = 1:(length(model$flex) + 1), col = 1:(length(model$flex) +1))

#plot(KM, col = 1:length(model$flex), main = "K-means: Comparación de clusters", xlab = "Tiempo", 
#     ylab = "Riesgo Acumulado", fun = "cumhaz")
#for (i in 1:length(model$flex)) lines(model$flex[[i]], ci = F, col = i, type = "cumhaz")

### Hierarchical
model <- aaa2[,-1] %>% group_by(grupo) %>% do(flex = flexsurvreg(Surv(duration, delta) ~ 
                                                                   1, data = ., dist = "genf"))
KM <- survfit(Surv(duration, delta) ~ grupo, data = aaa2[,-1])

plot(KM, col = 1:length(model$flex), main = "Hierarchical: Comparación de clusters", xlab = "Tiempo", 
     ylab = "Probabilidad de Supervivencia", xlim=c(0,30))
for (i in 1:length(model$flex)) lines(model$flex[[i]], ci = F, col = i)
legend("topright", c("1","2","3"), lty = 1:(length(model$flex)), col = 1:(length(model$flex)))


### Fuzzy
model <- aaa3[,-1] %>% group_by(grupo) %>% do(flex = flexsurvreg(Surv(duration, delta) ~ 
                                                                   1, data = ., dist = "genf"))
KM <- survfit(Surv(duration, delta) ~ grupo, data = aaa3[,-1])

plot(KM, col = 1:length(model$flex), main = "Fuzzy: Comparación de clusters", xlab = "Tiempo", 
     ylab = "Probabilidad de Supervivencia", xlim=c(0,30))
for (i in 1:length(model$flex)) lines(model$flex[[i]], ci = F, col = i)
legend("topright", c("1","2","3"), lty = 1:(length(model$flex) ), col = 1:(length(model$flex)))

############################
library(dplyr)
# df<-data.frame(s,grupo=km.res$cluster)
# df %>% group_by(grupo) %>% summarise(Meses_Promedio=mean(xt_month,na.rm = TRUE),Edad_Prom=mean(edad_,na.rm=TRUE),Meses_Mediana=median(xt_month,na.rm = TRUE),Devengado_Medio=mean(devengado_mean,na.rm = TRUE),
#                                      Devengado_min=mean(devengado_min,na.rm = TRUE),Devengado_max=mean(devengado_max,na.rm = TRUE),
#                                      Devengado_sd=mean(devengado_sd,na.rm = TRUE),Desempeño_Sob=mean(`PROP.SOB`,na.rm = TRUE),
#                                      Desempeño_Def=mean(`PROB.DEF`,na.rm = TRUE),Cantidad=n())->tablaResumen
#   write.csv2(tablaResumen,file = '../Desktop/Desc.csv')
#   
# summarize(df)
