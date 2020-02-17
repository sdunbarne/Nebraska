getSarpyPrecs <- function(date) {
read.csv(paste0(date, "__ne__general__precinct.csv"), as.is="precinct") %>%
    filter(county=="Sarpy") %>%
    filter(str_detect(office,  "Representative") || office=="House" ) %>%
    filter(votes!=0) %>%
    select("precinct", "district", "candidate", "votes") %>%
    mutate( prec= str_replace( precinct, "Precinct ", "p"))
}
