library("graph")
  library("Rgraphviz")
  library("RBGL")

 getBoundary <- function(subgraph, graph) {
     bdy <- unique(unlist(graph::boundary(subgraph, graph)))
     return(bdy)
     ## returns  boundary of subgraph in graph as  R char vector
     ## in future, add some error checking for argument types
 }

 getSarpyPrecincts <- function(precList) {
     sarpyBoundary <- precList[!(precList %in% c("lanc", "doug"))]
     return(sarpyBoundary)
     ## returns list (char vector) of precincts stricly in Sarpy Cty
     ## in future, add some error checking for argument types
 }

makeNewCongDist <-
    function(sarpyPrecincts, vertexDelete, vertexAdd, county) {
        newSarpyPrecincts <-
            c(sarpyPrecincts[-match(vertexDelete, sarpyPrecincts)],
                            vertexAdd, county)
     newCongDist <- graph::subGraph(newSarpyPrecincts, gSarpy)
     return(newCongDist)
     ## return a
 }

 vEccentricity <- function(graph, vertex) {
     ve <-  max(RBGL::dijkstra.sp(graph, start = vertex)$distances)
     return(ve)
     ## returns integer, longest shortest-path from vertex to
     ## every other vertex in graph        
     ## in future, add some error checking for argument types
 }

 gDiameter <- function(graph) {
     d <- max(RBGL::johnson.all.pairs.sp(graph))
     return(d)
     ## returns integer, greatest distance between any pair
     ## of vertices in  graph 
     ## in future, add some error checking for argument types
 }

 contiguous <- function(graph) {
     isconnected <- length(RBGL::connectedComp(graph)) == 1
     return(isconnected)
     ## returns logical (boolean), TRUE if single connected component
     ## in future, add some error checking for argument types
 }

## The following are compactness measures for the originally
## constituted congressional districts
## My goal is to have trial districts to have no
## greater eccentricities or diameters.
 BASEVECCENCD1 <- 7
 BASEGDIAMCD1 <- 7

 BASEVECCENCD2 <- 4
 BASEGDIAMCD2 <- 6

 sarpyAdj <- as.matrix(read.table("sarpy_adj.txt")) 
 #n x 2 edgelist matrix, not adjacency matrix!
 gSarpy <- ftM2graphNEL(sarpyAdj, edgemode = "undirected")

 cd1Prec <- c("lanc", "p1", "p2", "p3", "p4", "p5", "p6", "p7", "p8",
"p9", "p10", "p11", "p12", "p13", "p16", "p17",  "p18", "p20", "p21",
"p22",  "p24", "p25", "p26")
 cd2Prec <- setdiff(nodes(gSarpy), cd1Prec)
 congDist1 <- subGraph(cd1Prec, gSarpy)
 congDist2 <- subGraph(cd2Prec, gSarpy)

for (mcstep in 1:10) {

    trialStepCount <- 0

 bdyCongDist1 <-
     getSarpyPrecincts(getBoundary(congDist1, gSarpy)) # in CongDist2
 bdyCongDist2 <-
     getSarpyPrecincts(getBoundary(congDist2, gSarpy)) # in CongDist1

  #randomly select a boundary precinct for each Congressional District
  precFromCongDist1 <- sample(bdyCongDist2, 1)
  precFromCongDist2 <- sample(bdyCongDist1, 1)


 newCongDist1 <- makeNewCongDist(getSarpyPrecincts(cd1Prec),
                                 precFromCongDist1, precFromCongDist2,
                                 "lanc")
  newCongDist2 <- makeNewCongDist(getSarpyPrecincts(cd2Prec),
                                  precFromCongDist2, precFromCongDist1,
                                  "doug")

  if (vEccentricity(newCongDist1, "lanc") <= BASEVECCENCD1 &&
      gDiameter(newCongDist1) <= BASEGDIAMCD1 &&
      contiguous(newCongDist1) &&
      vEccentricity(newCongDist2, "doug") <= BASEVECCENCD2 &&
      gDiameter(newCongDist2) <= BASEGDIAMCD2 &&
      contiguous(newCongDist2)
     ) {
      CongDist1 <- newCongDist1
      CongDist2 <- newCongDist2
      }

 trialStepCount <- trialStepCount + 1
}


