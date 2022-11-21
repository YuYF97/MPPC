source("simulation_function.R")
source("MPPC.R")

load("arth.RData")
load("arth_know.RData")
mppc <- mpbn(data = arth, M.whitelist = know$knowledge.white, 
             M.blacklist = know$knowledge.black)

