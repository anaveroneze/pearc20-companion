#!/usr/bin/Rscript
library(tidyverse)    
options(crayon.enabled=FALSE)

topology <- expand.grid(X = seq(from = 7, by = -1, l = 8),
                    Y = seq(from = 7, by = -1, l = 8)) %>%
        mutate(PX = 8, PY = 8, Rank = seq(from = 63, by = -1, l = 64))

# Read .csv files and name columns properly
read_one_traces_csv <- function(FILE) {
    read_csv(FILE, col_names=FALSE, col_types=cols(), progress=FALSE) %>%
        rename(Rank = X1, 
               Start = X2,
               End = X3, 
               Imbrication = X4,
               Operation = X5,
               PAPI_TOT_INS = X6, 
               PAPI_TOT_CYC = X7) %>%
        mutate(Rank = as.integer(Rank)) %>%
        mutate(Case = ifelse(grepl("chuetsuoki", FILE), "chuetsuoki", "sishuan"),
               Case = ifelse(grepl("ligurian", FILE), "ligurian", Case)) %>%
        # Create the Iteration (per-rank)
        mutate(Iteration = ifelse(Operation == "iteration", 1, 0)) %>%
        group_by(Rank) %>%
        arrange(Start) %>%
        mutate(Iteration = cumsum(Iteration)) %>%
        ungroup %>%
        filter(Operation != "iteration") %>%
        filter(Iteration != 0) %>%
	# Define new ZERO
        mutate(End = End - min(Start)) %>%
        mutate(Start = Start - min(Start)) %>%
        mutate(Duration = End - Start ) %>%
        mutate(Type = ifelse(grepl("MPI", Operation), "MPI", "Application")) %>%
	# Add topology
	left_join(topology, by="Rank")
}

args = commandArgs(trailingOnly=TRUE)

# Get csv outputs
csv_files <- list.files(path = "./../data", pattern = "*.csv$", recursive = TRUE, all.files = TRUE, full.names = TRUE)
if(length(csv_files) != 0) {
	lapply(csv_files, read_one_traces_csv) %>%
		bind_rows -> df;

	write.csv(df, file = "./../data/all_traces.csv")
}
