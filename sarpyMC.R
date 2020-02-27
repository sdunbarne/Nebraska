library("graph")
library("Rgraphviz")
library("RBGL")
library("dplyr")

getBoundary <- function(subgraph, graph) {
    bdy <- unique(unlist(graph::boundary(subgraph, graph)))
    return(bdy)
    ## returns boundary of subgraph in graph as R char vector in
    ## future, add some error checking for argument types
}

getSarpyPrecincts <- function(precList) {
    sarpyBoundary <- precList[!(precList %in% c("lanc", "doug"))]
    return(sarpyBoundary)
    ## returns list (char vector) of precincts stricly in Sarpy Cty in
    ## future, add some error checking for argument types
}

makeNewCongDist <- function(sarpyPrecincts, vertexDelete, vertexAdd, county) {
    newSarpyPrecincts <- c(sarpyPrecincts[-match(vertexDelete, sarpyPrecincts)], 
        vertexAdd, county)
    newCongDist <- graph::subGraph(newSarpyPrecincts, gSarpy)
    return(newCongDist)
}

vEccentricity <- function(graph, vertex) {
    ve <- max(RBGL::dijkstra.sp(graph, start = vertex)$distances)
    return(ve)
    ## returns integer, longest shortest-path from vertex to every
    ## other vertex in graph in future, add some error checking for
    ## argument types
}

gDiameter <- function(graph) {
    d <- max(RBGL::johnson.all.pairs.sp(graph))
    return(d)
    ## returns integer, greatest distance between any pair of vertices
    ## in graph in future, add some error checking for argument types
}

contiguous <- function(graph) {
    isconnected <- length(RBGL::connectedComp(graph)) == 1
    return(isconnected)
    ## returns logical (boolean), TRUE if single connected component
    ## in future, add some error checking for argument types
}

## The following are compactness measures for the originally
## constituted congressional districts My goal is to have trial
## districts to have no greater eccentricities or diameters.
BASEVECCENCD1 <- 7
BASEGDIAMCD1 <- 7

BASEVECCENCD2 <- 4
BASEGDIAMCD2 <- 6

sarpyAdj <- as.matrix(read.table("sarpy_adj.txt"))
# n x 2 edgelist matrix, not adjacency matrix!
gSarpy <- ftM2graphNEL(sarpyAdj, edgemode = "undirected")

cd1Prec <- c("p1", "p2", "p3", "p4", "p5", "p6", "p7", "p8",
             "p9", "p10", "p11", "p12", "p13", "p16", "p17",
             "p18", "p20", "p21", "p22", "p24", "p25", "p26", 
             "lanc")
cd2Prec <- setdiff(nodes(gSarpy), cd1Prec)
congDist1 <- subGraph(cd1Prec, gSarpy)
congDist2 <- subGraph(cd2Prec, gSarpy)

Elec2018 <- read.csv("Elec2018.csv")

mcData <- data.frame(mcStep = 0, rePartionTrial = 0,
                     EccenCD1 = 7, DiamCD1 = 7, CompCD1 = 1, 
                     EccenCD2 = 4, DiamCD2 = 6, CompCD2 = 1,
                     bdyCD1 = 9, BdyCD2 = 9)
## districtsSeen <- c(1)
## names(districtsSeen) <- c(str_c(sort(nodes(congDist1)), collapse = ""))

elecOutcomes <- data.frame(mcStep = 0, cd2DEM = 121770, cd2REP = 126715)

for (mcStep in 1:10) {
rePartitionTrial <- 1
    
bdyCongDist1 <-
    getSarpyPrecincts(getBoundary(congDist1, gSarpy))  # in CongDist2
bdyCongDist2 <-
    getSarpyPrecincts(getBoundary(congDist2, gSarpy))  # in CongDist1

repeat {
        # rePartition Trial
        
        # randomly select a boundary precinct for each Congressional District
    precFromCongDist1 <- sample(bdyCongDist2, 1)
    precFromCongDist2 <- sample(bdyCongDist1, 1)
        
        
    newCongDist1 <-
        makeNewCongDist(getSarpyPrecincts(nodes(congDist1)),
                        precFromCongDist1, precFromCongDist2,
                        "lanc")
    newCongDist2 <-
        makeNewCongDist(getSarpyPrecincts(nodes(congDist2)),
                        precFromCongDist2, precFromCongDist1,
                        "doug")
        
    testvEccenCD1 <- vEccentricity(newCongDist1, "lanc")
    testgDiamCD1 <- gDiameter(newCongDist1)
    testCompsCD1 <- length(RBGL::connectedComp(newCongDist1))
    testvEccenCD2 <- vEccentricity(newCongDist2, "doug")
    testgDiamCD2 <- gDiameter(newCongDist2)
    testCompsCD2 <- length(RBGL::connectedComp(newCongDist1))
        
    trialData <- c(mcStep, rePartitionTrial,
                   testvEccenCD1, testgDiamCD1, testCompsCD1, 
                   testvEccenCD2, testgDiamCD2, testCompsCD2,
                   length(bdyCongDist1), length(bdyCongDist2))
    mcData <- rbind(mcData, setNames(trialData, names(mcData)))
    
    allConditions <- testvEccenCD1 <= BASEVECCENCD1 &&
        testgDiamCD1 <= BASEGDIAMCD1 && 
        contiguous(newCongDist1) &&
        testvEccenCD2 <= BASEVECCENCD2 &&
        testgDiamCD2 <= BASEGDIAMCD2 && contiguous(newCongDist2)

    rePartitionTrial <- rePartitionTrial + 1

    if (allConditions) 
        break
}        
# From here on down, is now as of Fri, Feb 21.This need serious checking and
# vetting.  also needs improvement, and simplification , now it is clumsy!
        if (allConditions) {
            congDist1 <- newCongDist1
            congDist2 <- newCongDist2
            
            ## h <- str_c(sort(nodes(congDist1)), collapse = "")
            ## if (h %in% names(districtsSeen)) {
            ##     districtsSeen[[h]] <- districtsSeen[[h]] + 1
            ## } else {
            ##     districtsSeen[[h]] <- 1
            ## }
            
            outcome <- filter(Elec2018, prec %in% nodes(congDist2)) %>%
                group_by(party) %>% 
                summarise(newelec = sum(as.integer(votes)))
            
            elecOutcomes <-
                rbind(elecOutcomes,
                      setNames(c(mcStep, as.integer(outcome[1, 2]),
                                 as.integer(outcome[2, 2])),
                               names(elecOutcomes)))
        }
        
}
