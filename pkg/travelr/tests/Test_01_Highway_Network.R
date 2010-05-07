# This test will only run if library(travelr) has been implicitly executed

suppressPackageStartupMessages(library(travelr))

options(width=200)

# The script expects to find subdirectories called "Network-<source>" where
# the source is ignored, but should indicate where the network came from
# Inside that folder, we'll expect to find networks and demand matrices in a broadly
# general format described below.
dirs<-dir(pattern="Network-",full.names=TRUE)
for (d in rev(dirs)) {
	# Inside the network folder, call up all the visible files that have a "." in their name
	# Conventionally, the files are stored:
	#     as Excel-like .csv (Network Links, Nodes, OD), or
	#     as .txt (Metadata, Penalties)
	# The penalty file is just reported in "Cube" format (straightforward -- see travelr::as.highway.net docs
	files<-dir(path=d,pattern="^.*[^_]+[_].*\\.(txt|csv)$",full.names=TRUE)
	networks<-unique(gsub("^.*/([^_]+)[_].*\\.(txt|csv)$","\\1",files))
	cat(paste(rep("=",80),collapse=""),"\n")
	cat("Networks in",d,":\n")
	cat(paste(networks,collapse=","),"\n")
	cat(paste(rep("-",80),collapse=""),"\n")
	for (n in networks) {
		# Locate network files
		net.files <- files[grep(n,files)]
		meta.file <- net.files[grep("Meta",net.files)]
		link.file <- net.files[grep("_Network",net.files)]
		node.file <- net.files[grep("_Nodes",net.files)]
		od.file   <- net.files[grep("_OD\\.",net.files)]
		pen.file  <- net.files[grep("_Penalt",net.files)]
		cat(paste(rep("=",80),collapse=""),"\n")
		cat(sep="","Network '",n,"':\n")
		cat(sep="","Meta (",meta.file,")\nOD (",od.file,")\nLinks (",link.file,")")
		if (length(node.file)>0) cat("\nNodes (",node.file,")")
		if (length(pen.file)>0)  cat("\nPenalties (",pen.file,")")
		cat("\n")

		# Process file contents
		numLinks <- numNodes <- 0
		cat("Processing metadata...\n")
		if (length(meta.file)>0) {
			# metadata is a text table with two whitespace-separated columns, the first of which
			# is the name of the metadata element, and the second is its value
			# Since the metadata ends up in a single numeric vector, the types should support that
			#  (Specifically, don't create metadata that can't be converted to a number)
			m<-read.table(meta.file,row.names=1)
			meta<-m[,1]
			names(meta)<-row.names(m)
			cat("Raw metadata:\n")
			print(meta)
			cat("Interesting metadata:\n")
			print(meta)
			meta<-meta[names(meta)%in%c("numZones","numNodes","numLinks","firstThruNode","Flow")]
			rm(m)
		} else stop("Meta file is missing for the network:",n,"\nMust have at least 'numZones'")

		cat("Links...\n")
		if (length(link.file)>0) {
			# Links are very similar to the TNTP format, with these fields:
			# "From","To","Capacity","Length","FFTime","Speed","Toll","B","Power","Type"
			# There is a python script to convert raw TNTP nodes, links and OD pairs
			# into formats that are easily readable by this script (see elsewhere)
			link <- read.csv(link.file)
			numLinks = nrow(link)
			if ( is.na(meta["numLinks"]) || meta["numLinks"]!=numLinks ) meta["numLinks"]<-numLinks
			cat("numLinks:",numLinks,"\n")
			print(names(link))
			linkNodes <- unique(c(link$From,link$To)) # this is the list of "actual" node numbers
			if ( is.na(meta["numNodes"]) ) meta["numNodes"]<-max(linkNodes)
			linkFields<-c(From="From",To="To",Capacity="Capacity",Time="FFTime")
		} else stop("Network (link) file is missing for the network:",n)

		cat("Nodes... [",meta["numNodes"],"]\n",sep="")
		if (length(node.file)>0) {
			# The node file is optional
			# If present, it contains X and Y projected coordinates for displaying the network
			# If absent, the node numbers are those actually in use in the network links
			node <- read.csv(node.file)
			if (names(node)[1]!="Node") names(node)[1]<-"Node" # Many TNTP just say "node"
			numNodes<-nrow(node)
			if ( !is.na(meta["numNodes"]) ) meta["numNodes.raw"]<-meta["numNodes"]
			else meta["numNodes.raw"]<-meta["numNodes"]<-numNodes

			if ( meta["numNodes"]<numNodes ) meta["numNodes"]<-numNodes
			else if ( meta["numNodes.raw"]>numNodes ) {
				meta["numNodes"]<-numNodes
			}
		} else {
			node<-data.frame(Node=linkNodes)
			meta["numNodes.raw"]<-max(node$Node)
			meta["numNodes"]<-length(node$Node)
			# Note: the "raw" number of nodes reflects the highest node number in use, and there
			# may be gaps (unused nodes) in the coding.  The actual number of nodes is the number
			# of distinct node numbers.  The highway.net structure uses (and builds) that information
			# including re-mapping the nodes to a compact set.
		}
		if ( !all(c(1:meta["numZones"]) %in% linkNodes) ) {
			warning("Zone numbers must be continuous from 1 to",meta["numZones"])
			warning("You will get warnings when you build paths from this network, because the missing zones can't be reached.")
			warning("This has implications for the OD tables, if those include zones not present in the network.")
			warning("An easy solution would be to add dummy links into some harmless network location for the missing zones.")
			warning("Otherwise, some initial zone number pre-processing should happen.")
			warning("NET RESULT: The network R data file was NOT created for",n)
			continue # Abandon ship on this network and just carry on.
		}
		cat("numNodes:",meta["numNodes"],"(Raw:",meta["numNodes.raw"],") ")
		cat(paste("[",paste(names(node),collapse=","),"]",sep=""),"\n")

		cat("OD...")
		if (length(od.file)>0) {
			# Using a pre-processed version of the OD matrix, that resembles a
			# literal matrix dump into a square array with row and column numbers			od <- read.csv(od.file)
			od<-read.csv(od.file)
			od<-data.matrix(od[,-1])
			numZones<-nrow(od)
			if ( is.na(meta["numZones"]) ) meta["numZones"]<-numZones
			if ( is.na(meta["Flow"]) ) meta["Flow"]<-sum(od)
			cat(" [",numZones," Zones]\n",sep="")
		}
		cat("Penalties...")
		penaltyNodes<-integer(0)
		if (length(pen.file)>0) {
			# Remove penalties where the nodes are not in linkNodes
			penalty<-read.table(pen.file,col.names=c("From","Thru","To","Set","Penalty"))
			penFields<-c(Thru="Thru",From="From",To="To",Penalty.value="Penalty")
		} else {
			penalty<-NULL
			penFields<-NULL
		}
		cat("\n")

		# Finally, fix up "firstThruNode", which bars path-building through nodes with numbers
		# smaller than this -- sketch networks typically have firstThruNode set to 1, and real networks
		# usually don't allow routing through the terminal nodes (which are nodes with raw numbers 1:numZones)
		print(meta[names(meta) %in% c("numNodes","numNodes.raw","numZones","firstThruNode","numLinks","Flow")])
			# Only the key elements of meta are printed; but all get saved

		# Now build the highway network
		# First step is to make sure the node numbers are correct, and fix firstThruNode
		if (meta["numNodes"]!=meta["numNodes.raw"]) {
			nodeMap<-map.highway.nodes(unique(c(node$Node,linkNodes,penaltyNodes)),numZones=numZones)
			cat("Mapping link nodes...\n")
			print(nodeMap)
			node$Node.raw<-node$Node; node$Node<-nodeMap$map(node$Node.raw)
			link$From.raw<-link$From; link$From<-nodeMap$map(link$From.raw)
			link$To.raw<-link$To;     link$To<-nodeMap$map(link$To.raw)
			if (length(penaltyNodes)>0) {
				cat("Mapping penality nodes...\n")
				penalty$From.raw<-penalty$From; penalty$From<-nodeMap$map(node$penalty$From.raw)
				penalty$To.raw<-penalty$To;     penalty$To<-nodeMap$map(node$penalty$To.raw)
				penalty$Thru.raw<-penalty$Thru; penalty$Thru<-nodeMap$map(node$penalty$Thru.raw)
			}
			if ( is.na(meta["firstThruNode"]) ) meta["firstThruNode"]<-meta["numZones"]+1
			else { # in case the first through node has a gap after the actual zones...
				ftn<-nodeMap$map(meta["firstThruNode"])
				if ( ftn==0 ) meta["firstThruNode"]<-meta["numZones"]+1
				else if (ftn!=meta["firstThruNode"]) meta["firstThruNode"]<-ftn
			}
		} else {
			nodeMap=NULL
		}

		# Now do some network testing
		temp.hwy<-as.highway.net(links=link,numNodes=meta["numNodes"],
					numZones=meta["numZones"],Penalty=penalty,nodes=node,
			        nodeMap=nodeMap,firstThruNode=meta["firstThruNode"],
					Link.fields=linkFields,Penalty.fields=penFields)
		cat("Highway network print:\n")
		print(temp.hwy)
		cat("Highway network structure:\n")
		str(temp.hwy)
		if ( !is.null(temp.hwy$nodeMap) ) {
			cat("Node map functions:\n")
			nodeMap = temp.hwy$nodeMap
			print(nodeMap)
			test.vec <- round(runif(10,min=1,max=attr(nodeMap,"maxRawNode")))
				# runif will sometimes generate nodes that are not in the raw set
				# since the raw set is hidden in the function environment, it's too hard to get at...
				# TODO: elevate the nodeMap vector itself to a full member, and use "with" to process
				# it, rather than an attached environment (or keep the environment itself as a
				# structure member).
			mapped <- nodeMap$map(test.vec)
			print(mapped)
			unmapped <- nodeMap$unmap(mapped[mapped!=0])
			print(data.frame(Node=test.vec[mapped!=0],Map=mapped[mapped!=0],UnMap=unmapped))
			rm(test.vec,mapped,unmapped,nodeMap)
		}

		# Then save the files
		n.out<-paste("data/",n,".rda",sep="")
		out<-paste(n,c(".net",".od"),sep="")
		cat("Saving",n,"as",n.out)
		assign(out[1],temp.hwy)
		assign(out[2],od)
		cat(" [",paste(out,collapse=","),"]\n",sep=" ")
		save(list=out,file=n.out,compress="xz")
		rm(temp.hwy)
		rm(list=c("out")) # These networks can be big; allow garbage collection once processed
	}
}
cat(paste(rep("=",80),collapse=""),"\n")
