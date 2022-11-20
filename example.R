library(bnlearn)
source("simulation_function.R")
source("MPPC.R")

load('arth150.rda')
arth150 <- rbn(bn, n = 10000)

bnn <- tranbn(bn)
know <- knowledge(bnn, p.d = 0.3, p.w = 0.3, p.b = 0.3)
mppc <- mpbn(data = magic, M.whitelist = know$knowledge.white, 
             M.blacklist = know$knowledge.black)
