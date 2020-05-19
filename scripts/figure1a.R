#!/usr/bin/Rscript
library(ggplot2)
library(dplyr)
library(wesanderson)
require(gridExtra)
options(crayon.enabled=FALSE)

df  <-  read.csv(file = "./../data/all_traces.csv")

df %>%
    filter(Case == "ligurian") %>%
    filter(Type == "Application") %>%
    filter(Operation != "seisMoment") %>%
    group_by(Rank, Iteration) %>%
    summarize(Load = sum(Duration)) %>%
    filter(Iteration == 90) %>%
    group_by(Iteration) %>%
    summarize(maxload = max(Load), minload = min(Load)) %>%
    print -> size;

mid = ((size$maxload + size$minload) / 2)

df %>%
    filter(Case == "ligurian") %>%
    filter(Type == "Application") %>%
    filter(Operation != "seisMoment") %>%
    filter(Iteration == 90) %>%
    group_by(Case, X, Y) %>% 
    summarize(Load = sum(Duration)) %>%
    ggplot(aes(x = X, y = Y, fill=Load)) + geom_tile() +
    scale_fill_gradient2(name="Duration (s)", midpoint=mid, low="blue", mid="white", 
                         high="red", limits=c(size$minload, size$maxload)) +
    ylab("Y Domain decomposition") + 
    xlab("X Domain decomposition") + theme_bw(base_size=22) + 
    theme(legend.position="top", legend.justification="left", 
          legend.key.size = unit(1, "cm"),
          legend.key.width = unit(2, "cm"),
          legend.text = element_text(color = "black", size = 22))

ggsave('./../img/PEARC_8x8_spatial_ligurian.png', device="png", width=200, height=250, units="mm")
