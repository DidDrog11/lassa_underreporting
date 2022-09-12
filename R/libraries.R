if (!require("pacman")) install.packages("pacman")
pkgs <- c("cowplot",
          "ggdist",
          "here",
          "Hmisc",
          "lemon",
          "readxl",
          "tidyverse")
pacman::p_load(pkgs, character.only = T)
