suppressPackageStartupMessages(library(travelr))
library(foreign)

# load prepared assignment sets and run them through some assignment algorithms.

load(file="SiouxFallsAset.Rdata")

options(width=120)
cat("SiouxFalls test assignment\n")
# cat(paste(rep("=",80),collapse=""),"\n")
# cat("Assignment with default method (AON)\n")
# ntwk=aset.sf$network$Links
# # print(ntwk)
# aresult<-highway.assign(aset.sf)
# cat("Assignment result structure:\n")
# # str(aresult)
# cat(paste(rep("=",80),collapse=""),"\n")
# target<-read.dbf("Network-Source/SiouxFalls_LoadTarget.dbf")
# target<-target[,c("A","B","V_1")]
# names(target)<-c("A","B","V.1")
# cat("Comparing results...\n")
# test.vol<-data.frame(A=aset.sf$network$Links$From,B=aset.sf$network$Links$To,aresult$volumes)
# test.vol<-merge(target,test.vol,by=c("A","B"),all=TRUE)
# write.dbf(test.vol,file="SiouxFalls_TestOutput.dbf")
# right.answer=with(test.vol,all(V.1==All))
# if (!right.answer) {
# 	cat("Wrong Loaded Volumes:\n")
# 	with(test.vol, {
# 		tv<-test.vol[V.1!=All,]
# 		print(tv[order(tv$A,tv$B),])
# 		} )
# 	costs<-aset.sf$ff.cost
# 	right.costs<-all(costs==aset.sf$network$Links$FFTime)
# 	cat("Any costs not equal to FFTime? ",!right.costs,"\n")
# 	if (!right.costs) {
# 		bad.costs<-cbind(ntwk,Costs.Built=costs)
# 		print(with(bad.costs,print(bad.costs[FFTime!=Costs.Built,c("From","To","FFTime","Costs.Built")])))
# 	}
# #	cat("Path traces:\n")
# 	trace<-.walk.paths(aresult$paths,1:aset.sf$network$numZones,1:aset.sf$network$numZones)
#  	trace.mtx<-matrix(trace,nrow=aset.sf$network$numZones,ncol=aset.sf$network$numZones,byrow=TRUE)
#  	tr.out<-data.frame(I=integer(0),J=integer(0),A=integer(0),B=integer(0),Link=integer(0),FFTime=numeric(0))
# 	for (i in 1:aset.sf$network$numZones) {
# 		for (j in 1:1:aset.sf$network$numZones) {
# 			tm<-unlist(trace.mtx[i,j])
# #			cat(i,"->",j,": ",paste(tm),"\n")
#   			if (length(tm)>0) {
#   				for (lnk in tm)	{
#  					tr.out<-rbind(tr.out,data.frame(I=i,J=j,A=ntwk$From[lnk],B=ntwk$To[lnk],Link=lnk,FFTime=ntwk$FFTime[lnk]))
#  				}
#  			}
# 		}
# 	}
# 	path.traces<-read.csv("Network-Source/SiouxFalls_PathTrace.csv",header=FALSE)
# 	names(path.traces)<-c("I","J","A","B","FFTime","CumTime")
# 	tr.out<-merge(path.traces,tr.out,by=c("I","J","A","B"),all=TRUE,suffixes=c(".IN",".OUT"))
# 	skim.differences<-aggregate(tr.out[,c("FFTime.IN","FFTime.OUT")],by=tr.out[,c("I","J")],FUN=sum)
# 	skim.differences<-skim.differences[with(skim.differences,which(round(FFTime.IN)!=round(FFTime.OUT))),]
# 	if (nrow(skim.differences)>0) {
# 		cat("Skim differences:\n")
# 		print(skim.differences)
# 	} else {
# 		cat("There are no skim differences\n")
# 	}
# 	write.csv(tr.out,file="SiouxFalls_AON_traces.csv",row.names=FALSE)
# 	tr.differences <- unique(tr.out[which(is.na(tr.out$FFTime.IN)|is.na(tr.out$FFTime.OUT)),c("A","B")])
# 	if (nrow(tr.differences)>0) {
# 		cat("A/B links that are not the same on both paths:\n")
# 		print(tr.differences)
# 	} else {
# 		cat("All paths have the same links\n")
# 	}
# } else {
# 	cat("Right Answer.\n")
# }
# 
# cat(paste(rep("=",80),collapse=""),"\n")
# 
# cat("Assignment with explicit AON method, and select link\n")
# all.links <- aset.sf$network$Links$.LinkID
# intercept.links <- sample(all.links,1) # Pick a single random link for intercept
# selected.links<-all.links%in%intercept.links
# show.selected.links<-paste("(1-based)",all.links[selected.links],collapse=",")
# cat("Finding intercepts of links (",show.selected.links,")\n")
# aresult<-highway.assign(aset.sf,method="AON",control=list(intercept=new.intercept.set(selected.links)))
# select.link<-aresult$intercept
# cat("Structure of select.link after assignment:\n")
# str(select.link)
# cat("OD pairs with paths through selected links (",show.selected.links,"):\n")
# od.pairs<-which( select.link$od[[1]]>0, arr.ind=TRUE )
# if (nrow(od.pairs)>0) {
# 	select.paths<-.walk.paths( aresult$paths[[1]], od.pairs[,1], od.pairs[,2], permute=FALSE )
# 	select.paths<-sapply(select.paths,function(x)paste(x,collapse=","))
# 	path.names<-strsplit(names(select.paths),"\\.")
# 	path.names<-sapply(path.names,function(x) paste(gsub("D","Dest ",gsub("O","Origin ",x)),collapse="->"))
# 	cat(paste(path.names,select.paths,sep=": "),sep="\n")
# } else {
# 	cat("No paths selected for links: ",paste(all.links[selected.links],collapse=","),"\n")
# }
# cat("Select Link Demand Matrices:\n")
# print(select.link$od[[1]])
# cat("Select Link Flow Volumes:\n")
# print(data.frame(.LinkID=all.links[select.link$volumes[[1]]>0],Volume=select.link$volumes[[1]][select.link$volumes[[1]]>0]))
#  
# cat(paste(rep("=",80),collapse=""),"\n")
# cat("Assignment with MSA method\n")
# aresult<-highway.assign(aset.sf,method="MSA",control=list(verbose=1,log=TRUE))
# print(class(aresult$log))
# print(aresult$log)
# save(aresult,file="MSA_Assignment_Results.Rdata")

cat(paste(rep("=",80),collapse=""),"\n")
cat("Assignment with Frank-Wolfe method\n")
aresult<-highway.assign(aset.sf,method="Frank.Wolfe",control=list(verbose=1,log=TRUE,max.iter=100))
print(aresult$log)
save(aresult,file="Frank-Wolfe_Assignment_Results.Rdata")
