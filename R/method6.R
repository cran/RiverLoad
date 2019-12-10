method6 <-
function(db, ncomp, period) {
  if(requireNamespace("imputeTS")){
n<-nrow(db)
interpolation<-data.frame(imputeTS::na_interpolation(db, "linear"))
load<-data.frame(interpolation[,2]*interpolation[,-c(1:2)])
difference<-matrix(nrow=(nrow(db)-1), ncol=1)
for (i in 1:(nrow(db)-1)){
difference[i]<-difftime(db[i+1,1], db[i,1], units="days")
  }}


flux<-(load[-(nrow(load)),]*(difference)*86400)
if(missing(period)){
method6<-apply(flux,2, sum)
return(method6)
}

else if (period=="month") {
loadtot<-cbind.data.frame(interpolation$datetime[-nrow(interpolation)], flux)
colnames(loadtot)[1]<-c("datetime")
loadtot[,1]<-format(as.POSIXct(loadtot[,1]), format="%Y-%m")
forrow<-aggregate(loadtot[,2]~datetime, loadtot, sum)
agg.dataC<-matrix(nrow=nrow(forrow), ncol=(ncomp))
for (i in 1:ncomp) {
agg.data<-aggregate(loadtot[,i+1]~datetime, loadtot, sum)
agg.dataC[,i]<-as.matrix(agg.data[,2])}
colnames(agg.dataC)<-c(names(db)[3:(ncomp+2)])
rownames(agg.dataC)<-forrow$datetime
return(agg.dataC)
}


else if (period=="year") {
loadtot<-cbind.data.frame(interpolation$datetime[-nrow(interpolation)], flux)
colnames(loadtot)[1]<-c("datetime")
loadtot[,1]<-format(as.POSIXct(loadtot[,1]), format="%Y")
forrow<-aggregate(loadtot[,2]~datetime, loadtot, sum)
agg.dataC<-matrix(nrow=nrow(forrow), ncol=(ncomp))
for (i in 1:ncomp) {
agg.data<-aggregate(loadtot[,i+1]~datetime, loadtot, sum)
agg.dataC[,i]<-as.matrix(agg.data[,2])}
colnames(agg.dataC)<-c(names(db)[3:(ncomp+2)])
rownames(agg.dataC)<-forrow$datetime
return(agg.dataC)
}}
