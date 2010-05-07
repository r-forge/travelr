####################################################################################################
#   travelr\r\Highway_Network.R by Jeremy Raw  Copyright (C) 2010
# 
#   This program is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; either version 2 of the License, or
#   (at your option) any later version.
# 
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
# 
#   A copy of the GNU General Public License is available at
#   http://www.r-project.org/Licenses/
#   and included in the R distribution (in directory ‘share/licenses’).
####################################################################################################

# Load a Transportation Network Test Problem network
# See http://www.bgu.ac.il/~bargera/tntp for the files themselves

tntp.network<-function(linkfile) {
	# get lines from file
	linkfile<-file(linkfile)
	lines<-readLines(linkfile,-1)
	close(linkfile)

	# Clean up the lines
	lines<-gsub("^[[:space:]]*","",lines)
	lines<-gsub("[[:space:]]*$","",lines)

	# Process header lines
	header.lines<-grep("^<(NUMBER|FIRST)",lines)
	headers<-lines[header.lines]

	# Clean up the metadata names into R vector
	headers<-gsub("<NUMBER OF ","num",headers)
	headers<-gsub("<FIRST THRU NODE","firstThruNode",headers)
	headers<-gsub("NODES","Nodes",headers)
	headers<-gsub("ZONES","Zones",headers)
	headers<-gsub("LINKS","Links",headers)
	headers<-strsplit(headers,">\\s*")
	headers<-sapply(headers,function(x){y=as.integer(x[[2]]);names(y)=x[[1]];return(y)})

	# Extract header elements into individual variables
	missing.headers<-setdiff(c("numLinks","numNodes","numZones"),names(headers))
	if ( length(missing.headers)>0 )
		stop("Missing headers:",paste(missing.headers,collapse=","),"\n")

	# Drop header- and comment-like lines
	lines<-lines[-grep("^[<~]",lines)]

	# Make the data lines into a data.frame, with standard field names
	data.lines<-grep("^\\d",lines)
	data<-lines[data.lines]
	data<-gsub("\\s*;","",data)
	data<-strsplit(data,"\\t")
	testNumLinks<-length(data)
	numFields<-length(data[[1]])
	link.frame<-data.frame(matrix(as.numeric(unlist(data)),nrow=testNumLinks,ncol=numFields,byrow=TRUE))
 	data.names<-c("From","To","Capacity","Length","FFTime","B","Power","Speed","Toll","Type")
 	if (length(link.frame)>length(data.names))
 		data.names<-c(data.names,paste("Field.",seq(1:(length(link.frame)-length(data.names))),sep=""))
 	else
 		data.names<-data.names[1:length(link.frame)]
 	names(link.frame)<-data.names

	# Do some general error checking
	if ( nrow(link.frame)!=headers["numLinks"] )
		cat("Mismatch in number of links:  Metadata=",headers["numLinks"]," and Data=",nrow(link.frame),"\n",sep="")
	allNodes<-unique(with(link.frame,c(From,To)))
	if (length(allNodes)!=max(allNodes)||headers["numNodes"]!=max(allNodes))
		cat("Mismatch in node numbering: Metadata=",headers["numNodes"],
			", Data=",length(allNodes),", Largest=",max(allNodes),"\n",sep="")

	# Make a travelr highway network
	return(as.highway.net(links=link.frame,
	                      numNodes=headers["numNodes"],
						  numZones=headers["numZones"],
						  firstThruNode=headers["firstThruNode"])
			)
}

# Load demand matrix corresponding to network file
tntp.od<-function(odfile) {
	# get lines from file
	odfile<-file(odfile)
	lines<-readLines(odfile,-1)
	close(odfile)

	# Clean up the lines
	lines<-gsub("^[[:space:]]*","",lines)
	lines<-gsub("[[:space:]]*$","",lines)

	# Process header lines
	header.lines<-grep("^<(NUMBER|TOTAL)",lines)
	headers<-lines[header.lines]

	# Clean up the metadata names into R vector
	headers<-gsub("<NUMBER OF ZONES","numZones",headers)
	headers<-gsub("<TOTAL OD FLOW","totalFlow",headers)
	headers<-strsplit(headers,">\\s*")
	headers<-sapply(headers,function(x){y=as.numeric(x[[2]]);names(y)=x[[1]];return(y)})

	# Extract header elements into individual variables
	missing.headers<-setdiff(c("numZones"),names(headers))
	if ( length(missing.headers)>0 )
		stop("Missing headers:",paste(missing.headers,collapse=","),"\n")

	# Drop header- and comment-like lines
	lines<-lines[-grep("^[<~]",lines)]
	lines<-lines[-grep("^$",lines)]

	# Extract the data
  	origins<-grep("origin",lines,ignore.case=TRUE)
  	origin.data<-data.frame(Origins=1:length(origins),From=origins+1,To=c(origins[2:length(origins)]-1,length(lines)))
	origin.data<-origin.data[-which(origin.data$To<origin.data$From),]
  	lines<-strsplit(lines,"[;:[:space:]]+")
 
 	extract.data <- function(o,f,t) {
 		pairs <- matrix(as.numeric(unlist(lines[f:t])),ncol=2,byrow=TRUE)
		if ( any(is.na(pairs)) ) cat("Nulls in origin:",o,"\n")
 		dimnames(pairs)<-list(NULL,c("To","Value"))
 		return( cbind(From=o,pairs) )
 	}
 	raw.data<-with(origin.data,mapply(Origins,From,To,FUN=extract.data) )
	
	od.matrix<-matrix(0,nrow=headers["numZones"],ncol=headers["numZones"])
	for ( o in raw.data ) od.matrix[o[,c("From","To")]]<-o[,"Value"]
#	od.matrix[raw.data[,c("From","To")]]<-raw.data$Data

	attr(od.matrix,"numZones")<-headers["numZones"]
 	return(od.matrix)
}
