knowledge <- function(bn, p.d=0, p.w=0.5, p.b=0.3){
  add.d <- data.frame(bn$arcs)
  add.m <- data.frame(NULL)
  i <- 0
  while (i<nrow(add.d)) {
    i <- i+1
    if (add.d[i,2] %in% add.d[,1]) {
      add.m <- rbind(add.m, MW.deal(add.d, from = add.d[i,1], to = add.d[i,2]))
    }
  }
  add.m <- unique(add.m)
  # print(2)
  if (nrow(add.d)>0) {
    sele <- sample(c(0,1,2), nrow(add.d), replace = T, prob = c(p.w,p.b,(1-p.w-p.b)))
    knowledge.wd <- add.d[which(sele==0),]
    if (is.null(knowledge.wd)) {
      knowledge.wd <- data.frame(from=NULL,to=NULL)
    }
    knowledge.bd <- data.frame(from=add.d[which(sele==1),2], to=add.d[which(sele==1),1])
    if (is.null(knowledge.bd)) {
      knowledge.bd <- data.frame(from=NULL,to=NULL)
    }
  } else {
    knowledge.wd <- data.frame(from=NULL,to=NULL)
    knowledge.bd <- data.frame(from=NULL,to=NULL)
  }
  # print(3)
  if (nrow(add.m)>1) {
    sele <- sample(c(0,1,2), nrow(add.m), replace = T, prob = c(p.w,p.b,(1-p.w-p.b)))
    knowledge.wm <- add.m[which(sele==0),]
    if (is.null(knowledge.wm)) {
      knowledge.wm <- data.frame(from=NULL,to=NULL)
    }
    knowledge.bm <- data.frame(from=add.m[which(sele==1),2], to=add.m[which(sele==1),1])
    if (is.null(knowledge.bm)) {
      knowledge.bm <- data.frame(from=NULL,to=NULL)
    }
  } else {
    knowledge.wm <- data.frame(from=NULL,to=NULL)
    knowledge.bm <- data.frame(from=NULL,to=NULL)
  }
  # print(4)
  if (p.d>0) {
    knowledge.w <- rbind(knowledge.wd[sample(x = c(1:nrow(knowledge.wd)), size = p.d*nrow(knowledge.wd)),],
                         knowledge.wm[sample(x = c(1:nrow(knowledge.wm)), size = (1-p.d)*nrow(knowledge.wm)),])
    knowledge.b <- rbind(knowledge.bd[sample(x = c(1:nrow(knowledge.bd)), size = p.d*nrow(knowledge.bd)),],
                         knowledge.bm[sample(x = c(1:nrow(knowledge.bm)), size = (1-p.d)*nrow(knowledge.bm)),])
  } else {
    knowledge.w <- knowledge.wm
    knowledge.b <- knowledge.bm
  }
  
  res <- list(knowledge.white=knowledge.w, knowledge.black=knowledge.b)
  return(res)
}

simdata <- function(n, nsample, prob=NULL, p.d=0, p.w=0.5, p.b=0.3){
  if (!require('bnlearn')) {
    stop('The package bnlearn was not installed')
  }
  if (!require('rlist')) {
    stop('The package rlist was not installed')
  }
  nodes <- paste0("X",c(1:n))
  randombn <- random.graph(nodes, num = 1, prob = prob, method = "ordered")
  arcs <- data.frame(randombn$arcs)
  rbn.arcs <- data.frame(NULL)
  data.g <- list(NULL)
  length(data.g) <- n
  names(data.g) <- nodes
  for (i in 1:n) {
    arc.t <- subset(arcs, to==paste0("X",i))
    if (nrow(arc.t)==0) {
      data.g[[i]] <- rnorm(nsample)
    }else{
      eff <- runif(n = nrow(arc.t), min = 0.8, max = 1.6)
      arc.t <- cbind(arc.t, eff)
      rbn.arcs <- rbind(rbn.arcs, arc.t)
      data.g[[i]] <- rep(0, nsample)
      for (j in 1:nrow(arc.t)) {
        data.g[[i]] <- data.g[[i]] + eff[j] * data.g[[arc.t[j,1]]]
      }
      data.g[[i]] <- data.g[[i]] + rnorm(nsample, 0, 1)
    }
  }
  data <- data.frame(list.cbind(data.g))
  colnames(data) <- paste0("X",c(n:1))
  nodes(randombn) <- paste0("X",c(n:1))
  # print(1)
  knowledge.sele <- knowledge(bn = randombn, p.d=p.d, p.w = p.w, p.b = p.b)
  result <- list(bn = randombn, data = data, knowledge = knowledge.sele, effect = rbn.arcs)
  return(result)
}

main <- function(n, nnode, nsample, prob=NULL, p.d=0, p.w=0.5, p.b=0.3, 
                 data.record=T, time.record = F) {
  if (!require('parallel')) {
    stop('The package parallel was not installed')
  }
  res <- list(NULL)
  length(res) <- n
  i <- 0
  t <- txtProgressBar(1, n, style=3)
  while (i < n) {
    i <- i+1
    testdata <- simdata(n = nnode, nsample = nsample, prob = prob, p.d=p.d,
                        p.w = p.w, p.b = p.b)
    cl = makeCluster(4, type = "SOCK")
    tmp <- list(NULL)
    length(tmp) <- 9
    # print(0)
    # print(head(data.frame(testdata$knowledge$knowledge.white)))
    tmp[[1]] <- testdata$bn
    time1 <- system.time(tmp[[2]] <- pc.stable(testdata$data, whitelist = data.frame(testdata$knowledge$knowledge.white), 
                                               blacklist = data.frame(testdata$knowledge$knowledge.black)))
    
    time2 <- system.time(tmp[[3]] <- pc.stable(testdata$data))
    # print(1)
    time3 <- system.time(tmp[[4]] <- gs(testdata$data, whitelist = data.frame(testdata$knowledge$knowledge.white),
                                        blacklist = data.frame(testdata$knowledge$knowledge.black), cluster = cl))

    time4 <- system.time(tmp[[5]] <- gs(testdata$data, cluster = cl))
    # print(2)
    # # tmp[[4]] <- iamb(testdata$data, cluster = cl)
    # # tmp[[5]] <- fast.iamb(testdata$data, cluster = cl)
    time5 <- system.time(tmp[[6]] <- iamb(testdata$data, whitelist = data.frame(testdata$knowledge$knowledge.white),
                                          blacklist = data.frame(testdata$knowledge$knowledge.black)))
    
    time6 <- system.time(tmp[[7]] <- iamb(testdata$data))
    
    # print(2)
    time7 <- system.time(tmp[[8]] <- mpbn(data = testdata$data, M.whitelist = data.frame(testdata$knowledge$knowledge.white),
                                          M.blacklist = data.frame(testdata$knowledge$knowledge.black)))
    # time7 <- system.time(tmp[[8]] <- mpbn(data = testdata$data, whitelist = data.frame(testdata$knowledge$knowledge.white),
    #                                       blacklist = data.frame(testdata$knowledge$knowledge.black)))
    # time7 <- system.time(tmp[[8]] <- mpbn(data = testdata$data))
    # print(3)
    if (time.record) {
      tmp[[2]] <- append(tmp[[2]], time1)
      tmp[[3]] <- append(tmp[[3]], time2)
      tmp[[4]] <- append(tmp[[4]], time3)
      tmp[[5]] <- append(tmp[[5]], time4)
      tmp[[6]] <- append(tmp[[6]], time5)
      tmp[[7]] <- append(tmp[[7]], time6)
      tmp[[8]] <- append(tmp[[8]], time7)
    }
    if (data.record) {
      tmp[[9]] <- testdata
    } else {
      testdata[[2]] <- NULL
      tmp[[9]] <- testdata
    }
    names(tmp) <- c("true","pc(kw)","pc","gs(kw)","gs","iamb(kw)","iamb","mpbn","simdata")
    res[[i]] <- tmp
    setTxtProgressBar(t, i)
  }
  return(res)
}


evalbn <- function(mainres){
  evalres <- list(NULL)
  # mainres <- result
  length(evalres) <- length(mainres)
  tmp <- list(NULL)
  length(tmp) <- length(mainres[[1]])-2
  for (i in 1:length(mainres)) {
    for (j in 2:(length(mainres[[i]])-1)) {
      if (j==(length(mainres[[i]])-1)) {
        tmp[[j-1]] <- data.frame(mainres[[i]][[j]]$elapsed, mainres[[i]][[j]]$res$n.test)
        colnames(tmp[[j-1]]) <- paste0(c("time","ntest"), "_", names(mainres[[i]])[j])
      } else {
        tmp[[j-1]] <- data.frame(mainres[[i]][[j]]$elapsed, mainres[[i]][[j]]$learning$ntests)
        colnames(tmp[[j-1]]) <-  paste0(c("time","ntest"), "_", names(mainres[[i]])[j])
      }
    }
    evalres[[i]] <- list.cbind(tmp)
  }
  evalres <- list.rbind(evalres)
  return(evalres)
}


evalbn2 <- function(mainres){
  evalres <- list(NULL)
  # mainres <- result
  length(evalres) <- length(mainres)
  tmp <- list(NULL)
  length(tmp) <- length(mainres[[1]])-2
  t <- txtProgressBar(1, length(mainres), style=3)
  for (i in 1:length(mainres)) {
    for (j in 2:(length(mainres[[i]])-1)) {
      # print(i)
      # print(j)
      tmp[[j-1]] <- data.frame(0,0)
      if (j==(length(mainres[[i]])-1)) {
        # tmp[[j-1]] <- c(0,0)
        tmp[[j-1]][1,1] <- adjacency_precision(as.cgraph(mainres[[i]]$true),
                                               as.cgraph(mainres[[i]][[j]]$bn))
        tmp[[j-1]][1,2] <- adjacency_recall(as.cgraph(mainres[[i]]$true),
                                            as.cgraph(mainres[[i]][[j]]$bn))
        tmp[[j-1]][1,3] <- shd(as.cgraph(mainres[[i]]$true),
                               as.cgraph(mainres[[i]][[j]]$bn))
        tmp[[j-1]][1,4] <- 2*tmp[[j-1]][1,1]*tmp[[j-1]][1,2]/(tmp[[j-1]][1,1]+tmp[[j-1]][1,2])
        names(tmp[[j-1]]) <- paste0(c("precision","recall","shd","F1"), "_", names(mainres[[i]])[j])
      } else {
        tmpbn <- empty.graph(nodes = nodes(mainres[[i]][[length(mainres[[i]])-1]]$bn))
        arcs(tmpbn, check.cycles = F) <- mainres[[i]][[j]][[3]]
        # tmp[[j-1]] <- c(0,0)
        tmp[[j-1]][1,1] <- adjacency_precision(as.cgraph(mainres[[i]]$true),
                                               as.cgraph(tmpbn))
        tmp[[j-1]][1,2] <- adjacency_recall(as.cgraph(mainres[[i]]$true),
                                            as.cgraph(tmpbn))
        tmp[[j-1]][1,3] <- shd(as.cgraph(mainres[[i]]$true),
                               as.cgraph(tmpbn))
        tmp[[j-1]][1,4] <- 2*tmp[[j-1]][1,1]*tmp[[j-1]][1,2]/(tmp[[j-1]][1,1]+tmp[[j-1]][1,2])
        names(tmp[[j-1]]) <- paste0(c("precision","recall","shd","F1"), "_", names(mainres[[i]])[j])
      }
      # print(tmp)
    }
    evalres[[i]] <- list.cbind(tmp)
    setTxtProgressBar(t, i)
  }
  evalres <- list.rbind(evalres)
  return(evalres)
}

###################################
# evalbn3 <- function(mainres){
#   evalres <- list(NULL)
#   # mainres <- result
#   length(evalres) <- length(mainres)
#   tmp <- list(NULL)
#   length(tmp) <- length(mainres[[1]])-2
#   for (i in 1:length(mainres)) {
#     for (j in 2:(length(mainres[[i]])-1)) {
#       # print(i)
#       # print(j)
#       tmp[[j-1]] <- data.frame(0,0)
#       if (j==(length(mainres[[i]])-1)) {
#         # tmp[[j-1]] <- c(0,0)
#         tmp[[j-1]][1,1] <- arrowhead_precision(as.cgraph(mainres[[i]]$true),
#                                                as.cgraph(mainres[[i]][[j]]$bn))
#         tmp[[j-1]][1,2] <- arrowhead_recall(as.cgraph(mainres[[i]]$true),
#                                             as.cgraph(mainres[[i]][[j]]$bn))
#         names(tmp[[j-1]]) <- paste0(c("precision","recall"), "_", names(mainres[[i]])[j])
#       } else {
#         tmpbn <- empty.graph(nodes = nodes(mainres[[i]][[length(mainres[[i]])-1]]$bn))
#         arcs(tmpbn) <- mainres[[i]][[j]][[3]]
#         # tmp[[j-1]] <- c(0,0)
#         tmp[[j-1]][1,1] <- arrowhead_precision(as.cgraph(mainres[[i]]$true),
#                                                as.cgraph(tmpbn))
#         tmp[[j-1]][1,2] <- arrowhead_recall(as.cgraph(mainres[[i]]$true),
#                                             as.cgraph(tmpbn))
#         names(tmp[[j-1]]) <- paste0(c("precision","recall"), "_", names(mainres[[i]])[j])
#       }
#       # print(tmp)
#     }
#     evalres[[i]] <- list.cbind(tmp)
#   }
#   evalres <- list.rbind(evalres)
#   return(evalres)
# }
# 
# evalbn4 <- function(mainres){
#   evalres <- list(NULL)
#   # mainres <- result
#   length(evalres) <- length(mainres)
#   tmp <- list(NULL)
#   length(tmp) <- length(mainres[[1]])-2
#   for (i in 1:length(mainres)) {
#     for (j in 2:(length(mainres[[i]])-1)) {
#       # print(i)
#       # print(j)
#       tmp[[j-1]] <- data.frame(0,0)
#       if (j==(length(mainres[[i]])-1)) {
#         # tmp[[j-1]] <- c(0,0)
#         tmp[[j-1]][1,1] <- shd(as.cgraph(mainres[[i]]$true),
#                                                as.cgraph(mainres[[i]][[j]]$bn))
#         names(tmp[[j-1]]) <- paste0("shd_", names(mainres[[i]])[j])
#       } else {
#         tmpbn <- empty.graph(nodes = nodes(mainres[[i]][[length(mainres[[i]])-1]]$bn))
#         arcs(tmpbn) <- mainres[[i]][[j]][[3]]
#         # tmp[[j-1]] <- c(0,0)
#         tmp[[j-1]][1,1] <- shd(as.cgraph(mainres[[i]]$true),
#                                                as.cgraph(tmpbn))
#         names(tmp[[j-1]]) <- paste0("shd_", names(mainres[[i]])[j])
#       }
#       # print(tmp)
#     }
#     evalres[[i]] <- list.cbind(tmp)
#   }
#   evalres <- list.rbind(evalres)
#   return(evalres)
# }
##############################

pfunction <- function(resultlist, index, level){
  evalres <- list()
  length(evalres) <- length(resultlist)
  for (i in 1:length(resultlist)) {
    evalres[[i]] <- evalbn2(resultlist[[i]])
    print(i)
  }
  methoduse <- c("pc(kw)", "pc", "gs(kw)", "gs", "iamb(kw)", "iamb", "mppc")
  evalplot.pre <- data.frame(NULL)
  evalplot.rec <- data.frame(NULL)
  evalplot.f1 <- data.frame(NULL)
  evalplot.shd <- data.frame(NULL)
  for (i in 1:length(evalres)) {
    evalplot.pre <- rbind(evalplot.pre,
                          cbind(evalres[[i]]$`precision_pc(kw)`,level[i],"pc(kw)"),
                          cbind(evalres[[i]]$precision_pc,level[i],"pc"),
                          cbind(evalres[[i]]$`precision_gs(kw)`,level[i],"gs(kw)"),
                          cbind(evalres[[i]]$precision_gs,level[i],"gs"),
                          cbind(evalres[[i]]$`precision_iamb(kw)`,level[i],"iamb(kw)"),
                          cbind(evalres[[i]]$precision_iamb,level[i],"iamb"),
                          cbind(evalres[[i]]$precision_mpbn,level[i],"mppc"))
    evalplot.rec <- rbind(evalplot.rec,
                          cbind(evalres[[i]]$`recall_pc(kw)`,level[i],"pc(kw)"),
                          cbind(evalres[[i]]$recall_pc,level[i],"pc"),
                          cbind(evalres[[i]]$`recall_gs(kw)`,level[i],"gs(kw)"),
                          cbind(evalres[[i]]$recall_gs,level[i],"gs"),
                          cbind(evalres[[i]]$`recall_iamb(kw)`,level[i],"iamb(kw)"),
                          cbind(evalres[[i]]$recall_iamb,level[i],"iamb"),
                          cbind(evalres[[i]]$recall_mpbn,level[i],"mppc"))
    evalplot.f1 <- rbind(evalplot.f1,
                         cbind(evalres[[i]]$`F1_pc(kw)`,level[i],"pc(kw)"),
                         cbind(evalres[[i]]$F1_pc,level[i],"pc"),
                         cbind(evalres[[i]]$`F1_gs(kw)`,level[i],"gs(kw)"),
                         cbind(evalres[[i]]$F1_gs,level[i],"gs"),
                         cbind(evalres[[i]]$`F1_iamb(kw)`,level[i],"iamb(kw)"),
                         cbind(evalres[[i]]$F1_iamb,level[i],"iamb"),
                         cbind(evalres[[i]]$F1_mpbn,level[i],"mppc"))
    evalplot.shd <- rbind(evalplot.shd,
                          cbind(evalres[[i]]$`shd_pc(kw)`,level[i],"pc(kw)"),
                          cbind(evalres[[i]]$shd_pc,level[i],"pc"),
                          cbind(evalres[[i]]$`shd_gs(kw)`,level[i],"gs(kw)"),
                          cbind(evalres[[i]]$shd_gs,level[i],"gs"),
                          cbind(evalres[[i]]$`shd_iamb(kw)`,level[i],"iamb(kw)"),
                          cbind(evalres[[i]]$shd_iamb,level[i],"iamb"),
                          cbind(evalres[[i]]$shd_mpbn,level[i],"mppc"))
  }
  colnames(evalplot.pre) <- c("precision",index,"method")
  colnames(evalplot.rec) <- c("recall",index,"method")
  colnames(evalplot.f1) <- c("F1",index,"method")
  colnames(evalplot.shd) <- c("SHD",index,"method")
  evalplot.pre$precision <- as.numeric(evalplot.pre$precision)
  evalplot.rec$recall <- as.numeric(evalplot.rec$recall)
  evalplot.f1$F1 <- as.numeric(evalplot.f1$F1)
  evalplot.shd$SHD <- as.numeric(evalplot.shd$SHD)
  
  p.precision <- ggplot(evalplot.pre, aes(x = factor(evalplot.pre[,2], levels = level),
                                          y = precision, fill = factor(method,levels = c("pc","pc(kw)","gs","gs(kw)","iamb","iamb(kw)","mppc")))) +
    geom_boxplot(alpha=0.7) + labs(fill = "method") +
    ylim(0,1) + xlab(index) +
    theme_bw() +
    theme(plot.title = element_text(size = 14, face =  "bold"),
          text = element_text(size = 16),
          axis.title = element_text(face="bold"),
          axis.text.x=element_text(size = 14),
          axis.text.y=element_text(size = 14),
          legend.title = element_text(size = 16), 
          legend.text = element_text(size = 14))
  p.recall <- ggplot(evalplot.rec, aes(x = factor(evalplot.rec[,2], levels = level),
                                       y = recall, fill = factor(method,levels = c("pc","pc(kw)","gs","gs(kw)","iamb","iamb(kw)","mppc")))) +
    geom_boxplot(alpha=0.7) + labs(fill = "method") +
    ylim(0,1) + xlab(index) +
    theme_bw() +
    theme(plot.title = element_text(size = 14, face =  "bold"),
          text = element_text(size = 16),
          axis.title = element_text(face="bold"),
          axis.text.x=element_text(size = 14),
          axis.text.y=element_text(size = 14),
          legend.title = element_text(size = 16), 
          legend.text = element_text(size = 14))
  p.f1 <- ggplot(evalplot.f1, aes(x = factor(evalplot.f1[,2], levels = level),
                                  y = F1, fill = factor(method,levels = c("pc","pc(kw)","gs","gs(kw)","iamb","iamb(kw)","mppc")))) +
    geom_boxplot(alpha=0.7) + labs(fill = "method") +
    ylim(0,1) + xlab(index) +
    theme_bw() +
    theme(plot.title = element_text(size = 14, face =  "bold"),
          text = element_text(size = 16),
          axis.title = element_text(face="bold"),
          axis.text.x=element_text(size = 14),
          axis.text.y=element_text(size = 14),
          legend.title = element_text(size = 16), 
          legend.text = element_text(size = 14))
  p.shd <- ggplot(evalplot.shd, aes(x = factor(evalplot.shd[,2], levels = level),
                                    y = SHD, fill = factor(method,levels = c("pc","pc(kw)","gs","gs(kw)","iamb","iamb(kw)","mppc")))) +
    geom_boxplot(alpha=0.7) + labs(fill = "method") +
    ylim(0,max(evalplot.shd$SHD)) + xlab(index) +
    theme_bw() +
    theme(plot.title = element_text(size = 14, face =  "bold"),
          text = element_text(size = 16),
          axis.title = element_text(face="bold"),
          axis.text.x=element_text(size = 14),
          axis.text.y=element_text(size = 14),
          legend.title = element_text(size = 16), 
          legend.text = element_text(size = 14))
  return(list(list(evalplot.pre,evalplot.rec,evalplot.f1,evalplot.shd),
              list(p.precision, p.recall, p.f1, p.shd)))
}

grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, 
                                       position = c("bottom", "right")) {
  require(gridExtra)
  require(grid)
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position="none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  grid.newpage()
  grid.draw(combined)
  # return gtable invisibly
  invisible(combined)
}

tranbn <- function(bn) {
  node <- names(bn)
  bnn <- empty.graph(nodes = node)
  for (i in 1:length(bn)) {
    if (length(bn[[i]]$parents)>0) {
      for (j in 1:length(bn[[i]]$parents)) {
        bnn <- set.arc(bnn, from = bn[[i]]$parents[j], to = node[i])
      }
    }
  }
  return(bnn)
}
