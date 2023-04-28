library(Rcpp)
library(RcppArmadillo)
sourceCpp("MW.deal.single.cpp")
sourceCpp("MW.deal.all.cpp")
sourceCpp("MB.deal.all.cpp")
sourceCpp("MB.deal.single.cpp")
sourceCpp("conflict.cpp")
sourceCpp("search.cpp")
sourceCpp("rev_search.cpp")
sourceCpp("ci0.cpp")
sourceCpp("cin.cpp")
sourceCpp("innerconflict.cpp")
sourceCpp("unshield_triple.cpp")
sourceCpp("v_stru.cpp")
###########################
########## 预处理 #########
###########################
#处理白名单
MW.deal <- function(M.whitelist, from, to){
  M.whitelist <- unique(M.whitelist)
  add <- data.frame(from=from, to=M.whitelist[which(M.whitelist[,1]==to),2])
  i <- 0
  while (i<nrow(add)) {
    i <- i+1
    if (add[i,2] %in% M.whitelist[,1]) {
      add <- rbind(add, c(add[i,1], M.whitelist[which(M.whitelist[,1]==add[i,2]),2]))
    }
  }
  add <- suppressMessages(dplyr::anti_join(add, M.whitelist))
  return(add)
}


#处理黑名单
MB.deal <- function(M.whitelist, from, to){
  M.whitelist <- unique(M.whitelist)
  add <- data.frame(from=from, to=M.whitelist[which(M.whitelist[,2]==to),1])
  i <- 0
  while (i<nrow(add)) {
    i <- i+1
    if (add[i,2] %in% M.whitelist[,2]) {
      add <- rbind(add, c(add[i,1], M.whitelist[which(M.whitelist[,2]==add[i,2]),1]))
    }
  }
  return(add)
}

############################
#######主函数邻接阶段#######
############################
MPBN.adj <- function(data, M.whitelist=data.frame(from=NULL,to=NULL),
                     M.blacklist=data.frame(from=NULL,to=NULL),
                     whitelist=data.frame(from=NULL,to=NULL),
                     blacklist=data.frame(from=NULL,to=NULL),max.cond=5,
                     breaksize=1, alpha=0.05, beta=0.6, record=F){
  #加载R包
  if (!require('bnlearn')) {
    stop('The package bnlearn was not installed')
  }
  if (!require('dplyr')) {
    stop('The package dplyr was not installed')
  }
  
  #记录输入数据
  if (record) {
    input <- list(data=data, M.whitelist=M.whitelist, M.blacklist=M.blacklist, whitelist=whitelist,
                  blacklist=blacklist, max.cond=max.cond, breaksize=breaksize, alpha = alpha, beta = beta)
  } else {
    input <- list(M.whitelist=M.whitelist, M.blacklist=M.blacklist, whitelist=whitelist,
                  blacklist=blacklist, max.cond=max.cond, breaksize=breaksize, alpha = alpha, beta = beta)
  }
  
  #读取边际黑白名单
  Marg.white <- M.whitelist
  Marg.black <- M.blacklist
  
  #读取节点
  nodes <- colnames(data)
  #设置maxcond
  if (is.null(max.cond)) {
    max.cond <- (length(nodes))/4
  }
  #设置alpha
  if (is.null(alpha)) {
    alpha <- max(0.05/length(nodes), 0.0001)
  }
  #设置beta
  if (is.null(beta)) {
    beta <- alpha
  }
  #判断beta
  if (alpha>beta) {
    return("ERROR: alpha < beta")
  }
  #设置无向边判断顺序
  order <- as.data.frame(t(combn(nodes, 2)))
  colnames(order) <- c("from","to")
  # #生成全图
  mpbn <- matrix(1,nrow = length(nodes), ncol = length(nodes))
  colnames(mpbn) <- nodes
  rownames(mpbn) <- nodes
  for (i in 1:length(nodes)) {
    mpbn[i,i] <- 0
  }
  # # plot(comDAG)
  # mpbn <- comDAG
  
  #黑白名单处理
  #处理白名单
  Marg.white <- unique(Marg.white)
  if (nrow(whitelist)>0) {
    Marg.white <- rbind(Marg.white, whitelist)
  }
  if (nrow(Marg.white)>0) {
    Marg.white <- data.frame(MW_all(MWr = Marg.white))
  }
  Marg.white <- unique(Marg.white)
  
  #处理黑名单
  Marg.black <- unique(Marg.black)
  if (nrow(Marg.black)>0) {
    Marg.black <- data.frame(MB_all(MBr = Marg.black, MWr = Marg.white))
  }
  Marg.black <- unique(Marg.black)
  
  #CI判断顺序
  record.order <- c(1:nrow(order))
  if (nrow(Marg.black)>0) {
    revblack <- rev_search(Marg.black,Marg.black)
    revblack <- unique(revblack)
    revorder <- search(order, Marg.black[revblack,])
    revorder <- unique(revorder)
    if (length(revorder)>0) {
      Marg.black <- Marg.black[-revblack,]
      record.order <- record.order[-revorder]
      for (i in revorder) {
        mpbn[order[i,1],order[i,2]] <- 0
        mpbn[order[i,2],order[i,1]] <- 0
      }
    }
  }
  
  # print(length(revorder))
  # print(order[revorder,])
  
  #精简边际黑白名单，判断是否矛盾
  if (nrow(Marg.white)>0 & nrow(Marg.black)>0) {
    tmp <- conflict(MWr = Marg.white, MBr = Marg.black)
    Marg.white <- data.frame(tmp[[1]])
    Marg.black <- data.frame(tmp[[2]])
    rm(tmp)
  }
  
  #白名单
  if (nrow(whitelist)>0) {
    W.order <- search(orderr = order, kwr = whitelist)
  } else {
    W.order <- c(NULL)
  }
  #黑名单
  if (nrow(blacklist)>0) {
    B.order <- search(orderr = order, kwr = blacklist)
  } else {
    B.order <- c(NULL)
  }
  #边际白名单
  if (nrow(Marg.white)>0) {
    MW.order <- search(orderr = order, kwr = Marg.white)
  } else {
    MW.order <- c(NULL)
  }
  ini.Marg.white <- Marg.white
  ini.MW.order <- MW.order
  
  #边际黑名单
  if (nrow(Marg.black)>0) {
    MB.order <- search(orderr = order, kwr = Marg.black)
  } else {
    MB.order <- c(NULL)
  }
  ini.Marg.black <- Marg.black
  ini.MB.order <- MB.order
  
  
  
  #去除因果白名单直接边和判断
  if (length(W.order)>0) {
    for (i in 1:length(W.order)) {
      mpbn[whitelist[i,2],whitelist[i,1]] <- 0
    }
    record.order <- record.order[-match(W.order, record.order)]
  }
  
  #非父节点记录(黑名单)
  NPaset <- list(NULL)
  length(NPaset) <- length(nodes)
  names(NPaset) <- nodes
  if (nrow(blacklist)>0) {
    for (i in 1:nrow(blacklist)) {
      NPaset[[blacklist[i,2]]] <- c(NPaset[[blacklist[i,2]]], whitelist[i,1])
    }
  }
  if (nrow(Marg.black)>0) {
    for (i in 1:nrow(Marg.black)) {
      NPaset[[Marg.black[i, 2]]] <- c(NPaset[[Marg.black[i, 2]]], Marg.black[i,1])
    }
  }
  if (nrow(whitelist)>0) {
    for (i in 1:nrow(whitelist)) {
      NPaset[[whitelist[i,1]]] <- c(NPaset[[whitelist[i,1]]], whitelist[i,2])
    }
  }
  if (nrow(Marg.white)>0) {
    for (i in 1:nrow(Marg.white)) {
      NPaset[[Marg.white[i, 1]]] <- c(NPaset[[Marg.white[i, 1]]], Marg.white[i,2])
    }
  }
  
  #记录分隔集Sepset
  Sepset <- list(NULL)
  length(Sepset) <- nrow(order)
  
  # print("a")
  #记录测试数量
  n.test <- 0
  #记录分隔集数量
  n.condition <- 0
  #循环去除无向边
  while (n.condition<=length(nodes)-2 & n.condition<=max.cond) {
    delete.record <- c()  #记录删除直接边
    causal.record <- c()  #记录因果知识边
    if (n.condition==0) {
      for (i in record.order) {
        # p.value <- ci.test(x = order[i,1], order[i,2], data = data)
        p.value <- ci0(x = data[,order[i,1]], y = data[,order[i,2]])
        n.test <- n.test + 1
        if (p.value >= alpha) {
          delete.record[length(delete.record)+1] <- i
          if (p.value >= beta) {
            causal.record[length(causal.record)+1] <- i
          }
          Sepset[[i]] <- list("0")
        }
      }
      # print("b")
    } else {
      for (i in record.order) {
        if (i %in% MW.order) { #判断边具有边际白先验
          # print(1)
          # print(Marg.white[which(MW.order==i)[1],2])
          # print(mpbn[,Marg.white[which(MW.order==i)[1],2]])
          node.cond <- nodes[which(mpbn[,Marg.white[which(MW.order==i)[1],2]]==1)] #选择果节点的相邻节点
          node.cond <- node.cond[-which(node.cond==order[i,1] | node.cond==order[i,2])] #去除因节点
          #去除子节点
          if (length(intersect(NPaset[[Marg.white[which(MW.order==i)[1],2]]], node.cond))>0) { #判断选择的节点里有没有非父节点
            node.cond <- node.cond[-match(intersect(NPaset[[Marg.white[which(MW.order==i)[1],2]]], node.cond), node.cond)]
          }
          if (length(node.cond)<n.condition) {
            next
          } else {
            cond.order <- as.data.frame(t(combn(node.cond, n.condition)))
          }
        } else if (i %in% MB.order) { #判断边具有边际黑先验
          # print(2)
          # print(Marg.black[which(MB.order==i)[1],1])
          node.cond <- nodes[which(mpbn[,Marg.black[which(MB.order==i)[1],1]]==1)] #选择果节点的相邻节点
          node.cond <- node.cond[-which(node.cond==order[i,1] | node.cond==order[i,2])] #去除因节点
          if (length(intersect(NPaset[[Marg.black[which(MB.order==i)[1],1]]], node.cond))>0) { #判断选择的节点里有没有非父节点
            node.cond <- node.cond[-match(intersect(NPaset[[Marg.black[which(MB.order==i)[1],1]]], node.cond), node.cond)]
          }
          if (length(node.cond)<n.condition) {
            next
          } else {
            cond.order <- as.data.frame(t(combn(node.cond, n.condition)))
          }
        } else {
          # print(3)
          cond.order <- data.frame(NULL)
          for (t in 1:2) {
            node.cond <- nodes[which(mpbn[,order[i,t]]==1)] #节点的相邻节点
            node.cond <- node.cond[-which(node.cond==order[i,1] | node.cond==order[i,2])]  #去除另一节点
            if (length(intersect(NPaset[[order[i,t]]], node.cond))>0) {
              node.cond <- node.cond[-match(intersect(NPaset[[order[i,t]]], node.cond), node.cond)]
            }
            if (length(node.cond)<n.condition) {
              next
            } else {
              cond.order <- rbind(cond.order, as.data.frame(t(combn(node.cond, n.condition))))
            }
          }
          if (nrow(cond.order)<1) {
            next
          }
        }
        #按照顺序进行CItest
        for (j in 1:nrow(cond.order)) {
          # p.value <- ci.test(x = order[i,1], y = order[i,2], 
          #                    z = as.character(cond.order[j,]), data = data)
          p.value <- cin(x = data[,order[i,1]], y = data[,order[i,2]], 
                         z = as.matrix(data[,as.character(cond.order[j,])]))
          n.test <- n.test + 1
          if (is.na(p.value))
            next
          if (p.value >= alpha) {
            delete.record[length(delete.record)+1] <- i
            Sepset[[i]][[length(Sepset[[i]])+1]] <- as.character(cond.order[j,])
            if (p.value >= beta) {
              causal.record[length(causal.record)+1] <- i
            }
            if (n.condition>=breaksize) {
              break
            }
          }
        }
      }
      # print("c")
      #边际白名单扩增
      if (length(intersect(causal.record, MW.order))>=1) {
        spe <- intersect(causal.record, MW.order)
        if (is.list(spe)) {
          spe <- spe[[1]]
        }
        for (k in spe) {
          for (m in 1:length(Sepset[[k]])) {
            if (length(Sepset[[k]][[m]])==1) {
              #判断新加入的因果关系是否会导致矛盾
              if (Sepset[[k]][[m]]=="0") {
                break
              }
              if (innerconflict(Marg.white, c(Marg.white[which(MW.order==k)[1],1], Sepset[[k]][[m]]))==0 & 
                  innerconflict(Marg.white, c(Sepset[[k]][[m]], Marg.white[which(MW.order==k)[1],2]))==0) {
                add <- MW_single(MWr = Marg.white, 
                                 newMW = data.frame(from=Marg.white[which(MW.order==k),1], to=Sepset[[k]][[m]]))
                add <- data.frame(add)
                Marg.white <- rbind(Marg.white, add)
                tmp <- search(orderr = order, kwr = add)
                MW.order <- append(MW.order, tmp)
                add <- MW_single(MWr = Marg.white, 
                                 newMW = data.frame(from=Sepset[[k]][[m]][1], to=Marg.white[which(MW.order==k)[1],2]))
                add <- data.frame(add)
                Marg.white <- rbind(Marg.white, add)
                tmp <- search(orderr = order, kwr = add)
                MW.order <- append(MW.order, tmp)
              } else {
                next
              }
            } else {
              for (num in 1:length(Sepset[[k]][[m]])) {
                if (innerconflict(Marg.white, c(Sepset[[k]][[m]][num], Marg.white[which(MW.order==k)[1],2]))==0) {
                  add <- MW_single(MWr = Marg.white, 
                                   newMW = data.frame(from=Sepset[[k]][[m]][num], to=Marg.white[which(MW.order==k)[1],2]))
                  add <- data.frame(add)
                  Marg.white <- rbind(Marg.white, add)
                  tmp <- search(orderr = order, kwr = add)
                  MW.order <- append(MW.order, tmp)
                } else {
                  next
                }
              }
            }
          }
        }
        Marg.white <- unique(Marg.white)
        MW.order <- unique(MW.order)
      }
      # print("C1")
      #边际黑名单扩增
      if (length(intersect(causal.record, MB.order))>=1) {
        spe <- intersect(causal.record, MB.order)
        if (is.list(spe)) {
          spe <- spe[[1]]
        }
        for (k in spe) {
          for (m in 1:length(Sepset[[k]])) {
            for (num in 1:length(Sepset[[k]][[m]])) {
              if (Sepset[[k]][[m]][1]=="0") {
                break
              }
              if (innerconflict(Marg.black, c(Marg.black[which(MB.order==k)[1],1], Sepset[[k]][[m]][num]))==0) {
                if (nrow(Marg.white)>0) {
                  add <- MB_single(MBr = Marg.black, MWr = Marg.white, 
                                 newMB = data.frame(from=Marg.black[which(MB.order==k)[1],1], to=Sepset[[k]][[m]][num]))
                  add <- data.frame(add)
                  Marg.black <- rbind(Marg.black, add)
                  tmp <- search(orderr = order, kwr = add)
                  MB.order <- append(MB.order, tmp)
                } else {
                  add <- data.frame(from=Marg.black[which(MB.order==k)[1],1], to=Sepset[[k]][[m]][num])
                  Marg.black <- rbind(Marg.black, add)
                  tmp <- search(orderr = order, kwr = add)
                  MB.order <- append(MB.order, tmp)
                }
              } else {
                next
              }
            }
          }
        }
        Marg.black <- unique(Marg.black)
        MB.order <- unique(MB.order)
      }
      # print("C2")
    }
    #精简边际黑白名单，判断是否矛盾
    if (nrow(Marg.white)>0 & nrow(Marg.black)>0) {
      tmp <- conflict(MWr = Marg.white, MBr = Marg.black)
      Marg.white <- data.frame(tmp[[1]])
      Marg.black <- data.frame(tmp[[2]])
      #######################
      MW.order <- search(orderr = order, kwr = Marg.white)
      MB.order <- search(orderr = order, kwr = Marg.black)
      #######################
      rm(tmp)
    }
    # print("d")
    #更新非父节点集合
    if (nrow(Marg.black)>0) {
      for (i in 1:nrow(Marg.black)) {
        NPaset[[Marg.black[i, 2]]] <- c(NPaset[[Marg.black[i, 2]]], Marg.black[i,1])
      }
    }
    if (nrow(Marg.white)>0) {
      for (i in 1:nrow(Marg.white)) {
        NPaset[[Marg.white[i, 1]]] <- c(NPaset[[Marg.white[i, 1]]], Marg.white[i,2])
      }
    }
    for (i in 1:length(NPaset)) { #去重
      if (length(NPaset[[i]])>0) {
        NPaset[[i]] <- unique(NPaset[[i]])
      }
    }
    #更新图结构
    if (length(delete.record)>=1) {
      record.order <- record.order[-match(delete.record, record.order)]
      for (i in delete.record) {
        mpbn[order[i,1],order[i,2]] <- 0
        mpbn[order[i,2],order[i,1]] <- 0
      }
    }
    # print("d1")
    n.condition <- n.condition+1
    if (length(record.order)==0) {
      break
    }
  }
  
  #精简边际黑白名单，判断是否矛盾
  if (nrow(Marg.white)>0 & nrow(Marg.black)>0) {
    tmp <- conflict(MWr = Marg.white, MBr = Marg.black)
    Marg.white <- data.frame(tmp[[1]])
    Marg.black <- data.frame(tmp[[2]])
    #######################
    MW.order <- search(orderr = order, kwr = Marg.white)
    MB.order <- search(orderr = order, kwr = Marg.black)
    #######################
    rm(tmp)
  }
  
  #判断初始边际白名单
  if (length(intersect(record.order, ini.MW.order))>=1) {
    MWtoW.order <- intersect(record.order, ini.MW.order)
    for (i in MWtoW.order) {
      mpbn[ini.Marg.white[which(ini.MW.order==i),2],ini.Marg.white[which(ini.MW.order==i),1]] <- 0
    }
  }
  # #判断初始边际黑名单
  # if (length(intersect(record.order, ini.MB.order))>=1) {
  #   MBtoB.order <- intersect(record.order, ini.MB.order)
  #   for (i in MBtoB.order) {
  #     mpbn[ini.Marg.black[which(ini.MB.order==i),1],ini.Marg.black[which(ini.MB.order==i),2]] <- 0
  #   }
  # }
  mpbn1 <- mpbn
  # #判断剩余边际白名单
  # if (length(intersect(record.order, setdiff(MW.order,ini.MW.order)))>=1) {
  #   MWtoW.order <- intersect(record.order, setdiff(MW.order,ini.MW.order))
  #   for (i in MWtoW.order) {
  #     mpbn[Marg.white[Marg.white[which(MW.order==i)[1],2],which(MW.order==i)[1],1]] <- 0
  #   }
  # }
  # # print("dd")
  # #判断剩余边际黑名单
  # if (length(intersect(record.order, setdiff(MB.order,ini.MB.order)))>=1) {
  #   MBtoB.order <- intersect(record.order, setdiff(MB.order,ini.MB.order))
  #   for (i in MBtoB.order) {
  #     mpbn[Marg.black[which(MB.order==i)[1],1],Marg.black[which(MB.order==i)[1],2]] <- 0
  #   }
  # }
  for (i in 1:length(Sepset)) {
    names(Sepset)[i] <- paste0("from=",order[i,1],", to=",order[i,2])
  }
  
  arct <- data.frame(from=NULL, to=NULL)
  for (i in 1:nrow(mpbn)) {
    if (length(nodes[which(mpbn[i,]==1)])>0) {
      arct <- rbind(arct, data.frame(from=nodes[i], to=nodes[which(mpbn[i,]==1)]))
    }
  }
  mpbn <- empty.graph(nodes)
  arcs(mpbn, check.cycles = F) <- arct
  if (!acyclic(mpbn, directed = T)) {
    arct <- data.frame(from=NULL, to=NULL)
    for (i in 1:nrow(mpbn1)) {
      if (length(nodes[which(mpbn1[i,]==1)])>0) {
        arct <- rbind(arct, data.frame(from=nodes[i], to=nodes[which(mpbn1[i,]==1)]))
      }
    }
    arcs(mpbn, check.cycles = F) <- arct
  }
  # print("e")
  
  #判断初始边际黑名单
  if (length(intersect(record.order, ini.MB.order))>=1) {
    MBtoB.order <- intersect(record.order, ini.MB.order)
    for (i in MBtoB.order) {
      mpbn <- set.arc(mpbn, from = ini.Marg.black[which(ini.MB.order==i),2], to = ini.Marg.black[which(ini.MB.order==i),1], 
                      check.cycles = F)
      tmpbn <- empty.graph(nodes = bnlearn::nodes(mpbn))
      tmpbn$arcs <- directed.arcs(mpbn)
      if (!acyclic(tmpbn)) {
        mpbn <- drop.arc(mpbn, from = ini.Marg.black[which(ini.MB.order==i),2], to =  ini.Marg.black[which(ini.MB.order==i),1])
      }
    }
  }
  # print("e")
  #判断剩余边际白名单
  if (length(intersect(record.order, setdiff(MW.order,ini.MW.order)))>=1) {
    MWtoW.order <- intersect(record.order, setdiff(MW.order,ini.MW.order))
    for (i in MWtoW.order) {
      # print(i)
      mpbn <- set.arc(mpbn, from = Marg.white[which(MW.order==i),1], to = Marg.white[which(MW.order==i),2], check.cycles = F)
      tmpbn <- empty.graph(nodes = bnlearn::nodes(mpbn))
      tmpbn$arcs <- directed.arcs(mpbn)
      if (!acyclic(tmpbn)) {
        mpbn <- set.edge(mpbn, from = Marg.white[which(MW.order==i),1], to = Marg.white[which(MW.order==i),2])
      }
    }
  }
  # print("e")
  #判断剩余边际黑名单
  if (length(intersect(record.order, setdiff(MB.order,ini.MB.order)))>=1) {
    MBtoB.order <- intersect(record.order, setdiff(MB.order,ini.MB.order))
    for (i in MBtoB.order) {
      # print(i)
      # print(Marg.black[which(MB.order==i),2])
      # print(Marg.black[which(MB.order==i),1])
      mpbn <- set.arc(mpbn, from = Marg.black[which(MB.order==i),2], to = Marg.black[which(MB.order==i),1], check.cycles = F)
      tmpbn <- empty.graph(nodes = bnlearn::nodes(mpbn))
      tmpbn$arcs <- directed.arcs(mpbn)
      if (!acyclic(tmpbn)) {
        mpbn <- drop.arc(mpbn, from = Marg.black[which(MB.order==i),2], to = Marg.black[which(MB.order==i),1])
      }
    }
  }
  # print("e")
  # dirarc <- as.data.frame(directed.arcs(mpbn))
  # confarc <- search(dirarc, ini.Marg.black)
  # confarc <- unique(confarc)
  # if (length(confarc)>0) {
  #   for (i in confarc) {
  #     mpbn <- drop.arc(mpbn, from = dirarc[i,1], to = dirarc[i,2])
  #   }
  # }
  
  
  # print("f")
  #记录结果
  #合并中间结果
  med_result <- list(Sepset=Sepset, M.whitelist=Marg.white, M.blacklist=Marg.black, 
                     whitelist=whitelist, blacklist=blacklist, order_use=order, 
                     remainedge=record.order,  NonParentSet=NPaset)
  #返回结果
  res <- list(bn=mpbn, n.test=n.test, result=med_result, input=input)
  return(res)
}

############################
#######    预处理    #######
############################
unshielded_triple <- function(all.arcs, und.arcs, from, to){
  #选取无向边
  tmp <- und.arcs[-which(apply(und.arcs, 1, function(x) all(x==rev(c(from, to))))), ]
  tmp <- tmp[tmp[,1]==to,]
  res <- data.frame(NULL)
  if (nrow(tmp)>0) {
    for (i in 1:nrow(tmp)) {
      a <- which(apply(all.arcs, 1, function(x) (all(x==c(from, tmp[i,2])) | all(x==rev(c(from, tmp[i,2]))))))
      if (length(a)>0) {
        next
      } else {
        res <- rbind(res, data.frame(X=from, Y=to, Z=tmp[i,2]))
      }
    }
  }
  return(res)
}

############################
#######判断vstructure#######
############################
v_struct <- function(adj.data, from, to, med){
  #判断order
  sel.order <- which(apply(adj.data$result$order_use, 1, function(x) (all(x==c(from, to)) | all(x==rev(c(from, to))))))
  if (is.null(adj.data$result$Sepset[[sel.order]])) {
    v_stru <- T
  } else {
    sepset.use <- adj.data$result$Sepset[[sel.order]]
    num <- 0
    for (i in 1:length(sepset.use)) {
      if (med %in% sepset.use[[i]]) {
        num <- num+1
      }
    }
    v_stru <- NULL
    if (num > length(sepset.use)/2) {
      v_stru <- F
    } else {
      v_stru <- T
    }
  }
  return(v_stru)
}

############################
#######   类别转换   #######
############################
conv_cg_to_bn <- function(edges){
  edges <- data.frame(edges)
  colnames(edges) <- c("from", "to", "ori")
  dir.arc.m <- edges[edges$ori=="-->",]
  und.arc.m <- edges[edges$ori=="---",]
  res <- list(dirarc=dir.arc.m, undarc=und.arc.m)
  return(res)
}

############################
#######主函数定向阶段#######
############################
MPBN.ori <- function(adj.data){
  if (!require('rlist')) {
    stop('The package rlist was not installed')
  }
  if (!require('causality')) {
    stop('The package causality was not installed')
  }
  #抽取unshielded triple
  # print("g")
  all.arcs <- data.frame(arcs(adj.data$bn))
  und.arcs <- data.frame(undirected.arcs(adj.data$bn))
  dir.arcs <- data.frame(directed.arcs(adj.data$bn))
  unsh_tri <- triple(und_arc = und.arcs, arc = all.arcs, dir_arc = dir.arcs)
  # unsh_tri <- list.rbind(apply(und.arcs, 1, function(x) unshielded_triple(all.arcs = all.arcs, und.arcs = und.arcs, from = x[1], to = x[2])))
  # unsh_tri <- unsh_tri[which(!duplicated(data.frame(t(apply(unsh_tri, 1, function(x) sort(x)))))),] 
  #判断v结构
  if (nrow(unsh_tri)>0) {
    vtru <- v_stru(order = adj.data$result$order_use, triple = unsh_tri, Sepset = adj.data$result$Sepset)
    # vtru <- apply(unsh_tri, 1, function(x) v_struct(adj.data = adj.data, from = x[1], to = x[3], med = x[2]))
    #定向v结构
    for (i in 1:nrow(unsh_tri)) {
      if (vtru[i]==1) {
        adj.data$bn <- set.arc(adj.data$bn, from = unsh_tri[i,1], to = unsh_tri[i,2], check.cycles = F)
        tmpbn <- empty.graph(nodes = bnlearn::nodes(adj.data$bn))
        arcs(tmpbn, check.cycles = F) <- directed.arcs(adj.data$bn)
        if (!acyclic(tmpbn)) {
          adj.data$bn <- set.edge(adj.data$bn, from = unsh_tri[i,1], to = unsh_tri[i,2])
        }
        adj.data$bn <- set.arc(adj.data$bn, from = unsh_tri[i,3], to = unsh_tri[i,2], check.cycles = F)
        tmpbn <- empty.graph(nodes = bnlearn::nodes(adj.data$bn))
        arcs(tmpbn, check.cycles = F) <- directed.arcs(adj.data$bn)
        if (!acyclic(tmpbn)) {
          adj.data$bn <- set.edge(adj.data$bn, from = unsh_tri[i,3], to = unsh_tri[i,2])
        }
      } else {
        next
      }
    }
    #记录结果
    adj.data$result[[length(adj.data$result)+1]] <- data.frame(unsh_tri,vtru)
    names(adj.data$result)[length(adj.data$result)] <- "unshielded_triple"
  }
  #MEEK RULES
  bn.meek <- as.cgraph(adj.data$bn)
  bn.meek <- meek(bn.meek)
  # print("h")
  #convert cgraph to bn class
  edges.meek <- bn.meek$edges
  edges.meek <- conv_cg_to_bn(edges = edges.meek)
  bn.res <- adj.data$bn
  dirarc1 <- directed.arcs(bn.res)
  if (nrow(edges.meek$dirarc)>0) {
    for (i in 1:nrow(edges.meek$dirarc)) {
      if (length(which(dirarc1[,1]==as.character(edges.meek$dirarc[i,1]) & 
                       dirarc1[,2]==as.character(edges.meek$dirarc[i,2])))==0) {
        bn.res <- set.arc(bn.res, from = as.character(edges.meek$dirarc[i,1]), 
                          to = as.character(edges.meek$dirarc[i,2]), check.cycles = F)
        tmpbn <- empty.graph(nodes = bnlearn::nodes(bn.res))
        arcs(tmpbn, check.cycles = F) <- directed.arcs(bn.res)
        if (!acyclic(tmpbn)) {
          bn.res <- set.edge(bn.res, from = as.character(edges.meek$dirarc[i,1]),
                             to = as.character(edges.meek$dirarc[i,2]))
        }
      }
    }
  }
  result <- list(bn=bn.res, res=adj.data)
  return(result)
}

############################
#######    主函数    #######
############################
mpbn <- function(data, M.whitelist=data.frame(from=NULL,to=NULL),
                 M.blacklist=data.frame(from=NULL,to=NULL),
                 whitelist=data.frame(from=NULL,to=NULL),
                 blacklist=data.frame(from=NULL,to=NULL),max.cond=5,
                 breaksize=1, alpha=NULL,beta=NULL){
  adj <- MPBN.adj(data = data, M.whitelist = M.whitelist, M.blacklist = M.blacklist,
                  whitelist = whitelist, blacklist = blacklist,
                  max.cond = max.cond, breaksize = breaksize, alpha = alpha, beta = beta)
  mpbn <- MPBN.ori(adj)
  return(mpbn)
}


###########################
######   定向PCDAG   ######
###########################

hympbn <- function(res,data){
  pcdag <- res
  allarcs <- as.data.frame(t(combn(nodes(pcdag), 2)))
  colnames(allarcs) <- c("from","to")
  allarcs <- rbind(allarcs, data.frame(from=allarcs[,2],to=allarcs[,1]))
  WL <- data.frame(directed.arcs(pcdag))
  tmparc <- rbind(WL, data.frame(from=WL[,2],to=WL[,1]), undirected.arcs(pcdag))
  pos <- search(orderr = allarcs, kwr = tmparc)
  BL <- allarcs[-pos,]
  DAG_hc <- hc(x = data, whitelist = WL, blacklist = BL)
  result <- list(DAG = DAG_hc, result = res)
  return(result)
}




