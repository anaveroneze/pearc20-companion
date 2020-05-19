#!/usr/bin/Rscript
library(ggplot2)
library(dplyr)
library(wesanderson)
require(gridExtra)

df  <-  read.csv(file = "./../data/all_traces.csv")

df %>%
    filter(Case == "ligurian") %>%
    filter(Type == "Application") %>%
    filter(Operation != "seisMoment") %>%
    group_by(Rank, Iteration) %>%
    summarize(Load = sum(Duration)) %>%
    print -> df.ligurian.temp0;

df %>%
    filter(Case == "ligurian") %>%
    filter(Type == "Application") %>%
    filter(Operation != "seisMoment") %>%
    group_by(Rank, Iteration) %>%
    summarize(Iter.Duration = max(End) - min(Start)) %>%
    print -> df.ligurian.temp1;

max <- max(df.ligurian.temp0$Load)
min <- min(df.ligurian.temp0$Load)
mid = ((max+min)/2)

df.ligurian.temp0 %>%
    left_join(df.ligurian.temp1) %>%
    ggplot(aes(y = Rank, x = Iteration)) +
    geom_raster(aes(fill = Load)) +
    scale_fill_gradient2(name="Duration (s)", midpoint=mid, low="blue", mid="white", high="red", space="Lab", limits=c(min, max)) +
    theme_bw(base_size=22) +
    theme(
        plot.margin = unit(c(0,0,0,0), "cm"),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(color = "black", size = 22),
        panel.grid = element_blank(),
        legend.key.width = unit(2, "cm"),
        legend.position="top",
        legend.spacing = unit(0, "mm"),
        legend.box.spacing = unit(0, "pt"),
        legend.box.margin = margin(0,0,0,0),
        legend.justification = "left") +
    scale_x_continuous("Iteration", limits=c(0, 300),  breaks=seq(0, 300, 25), expand = c(0, 0)) +
    scale_y_continuous("Process", limits=c(0, 63),  breaks=seq(0, 63, 3), expand = c(0, 0))

ggsave('./../img/PEARC_8x8_heatmap_ligurian.png', device="png", width=350, height=150, units="mm")
