rm(list = ls())
library(vars)
library(expm)
library(plyr)
library(xts)
library(network)
library(tseries)
library(imputeTS)
library(boot)
library(igraph)
setwd("C:/Users/pb3661/Dropbox/R_no2")
source("functions/f_VD_intra.R")
source("functions/f_boot_1.R")
source("functions/f_eval.R")

#rename a as by (it is asw all the same):
by14<-a14
byGr<-aGr
by4<-a4
byG<-aG

f.plot_mat<-function(m,vertexes,q,arrow_scale,vertex_size,label_size){#q are the quantiles to scale with
  weight<-c(2,8,16)
  #q<-quantile(m,probs = prob)
  mat<-m  
  mat[mat<q[[1]]]<-0
  mat[q[[1]]<=mat & mat<q[[2]]]<-weight[1]
  mat[q[[2]]<=mat & mat<q[[3]]]<-weight[2]
  mat[mat>=q[[3]] & mat<1]<-weight[3]
  
  
  net<-graph_from_adjacency_matrix(mat,mode = "directed",weighted = T)
  net_simplify<- simplify(net, remove.multiple = F, remove.loops = T)
  l<- layout.circle(net_simplify)
  edge.lty<-rep(1, ecount(net_simplify))
  edge.arrow.size<-rep(1, ecount(net_simplify))
  
  edge.curved<-autocurve.edges(net_simplify)
  #curves<-autocurve.edges2(net_simplify)
  E(net_simplify)$color[E(net_simplify)$weight==weight[1]]<-"#e9a42c"
  E(net_simplify)$color[E(net_simplify)$weight==weight[2]]<-"#d55b20"
  E(net_simplify)$color[E(net_simplify)$weight==weight[3]]<-"#ad3c39"
  V(net_simplify)$label.cex<-label_size
  V(net_simplify)$color<-"#b8b8b8"
  plot(net_simplify,
       vertex.label=vertexes,vertex.label.family = "sans",edge.width=E(net_simplify)$weight,layout=l,edge.curved=0.1,
       vertex.size=vertex_size,vertex.label.size=label_size,vertex.label.color="black", label.cex=1,edge.arrow.size=3.5)
  
  
  }


f.paste_pdf<-function(matrix, name, scale, vertexes,q){
  pdf(file=paste("C:/Users/pb3661/Dropbox/BIS_KIT/tex3/images/networksfin/",
                 name, ".pdf", sep=""),width=10,height=10)
  old.par <- par( no.readonly = TRUE )
  par( oma = c( 0, 0, 3, 0 ) )
  f.plot_mat(matrix,vertexes,q,scale,35,5)
  par( old.par )
  dev.off()
}

set.seed(3)

#Get all names:
event_names<-names(all.list.d)
#plot matrix for events
f.plot_event<-function(event,c.orig_data,b.origdata,cds_quantiles,by_quantiles){
  event.data<-lapply(all.list.d[[event]],f_boot_5)#f_boot_rel
  event.ul<-unlist(event.data,recursive=F)
  event.cds<-event.ul[sapply(names(event.ul),function(x){grepl("cds",x,fixed=T)})]
  event.cds.vertexes <- colnames(c.orig_data)
  event.cds.vertexes <- sapply(event.cds.vertexes,function(x){strsplit(x,"_")[[1]][1]})
  #Hier nochmals aufspaltung moeglich
  for(i in 1:length(event.cds)){
    name<-names(event.cds[i])
    f.paste_pdf(matrix = event.cds[[i]], name = gsub("\\.","_",name),scale=c(2.2,2.2), vertexes = event.cds.vertexes, q=cds_quantiles)
  }
  
}
# same function for final paper plots
f.plot_event_fin<-function(event,c.orig_data,b.origdata,cds_quantiles,by_quantiles){
  event.data<-lapply(all.list.fin.d[[event]],f_boot_5)#f_boot_rel
  event.ul<-unlist(event.data,recursive=F)
  event.cds<-event.ul[sapply(names(event.ul),function(x){grepl("cds",x,fixed=T)})]
  event.cds.vertexes <- colnames(c.orig_data)
  event.cds.vertexes <- sapply(event.cds.vertexes,function(x){strsplit(x,"_")[[1]][1]})
  for(i in 1:length(event.cds)){
    name<-names(event.cds[i])
    f.paste_pdf(matrix = event.cds[[i]], name = gsub("\\.","_",name),scale=c(2.2,2.2), vertexes = event.cds.vertexes, q=cds_quantiles)
  }
}



f.get_scaling<-function(reference_event="Gr2"){
  reference<-lapply(all.list.d[[reference_event]], f_boot_5)#f_boot_rel
  reference.ul<-unlist(reference,recursive = F)
  reference.cds<-reference.ul[sapply(names(reference.ul),function(x){grepl("cds",x,fixed=T)})]
  reference.cds.ul<-unlist(reference.cds,recursive = F)
  reference.cds.q<-quantile(reference.cds.ul[reference.cds.ul>0],probs = c(.25,.5,.75))
  reference.by<-reference.ul[sapply(names(reference.ul),function(x){grepl("by",x,fixed=T)})]
  reference.by.ul<-unlist(reference.by,recursive = F)
  reference.by.q<-quantile(reference.by.ul[reference.by.ul>0],probs = c(.25,.5,.75))
  return(list(reference.cds.q,reference.by.q))
  
}

#get Scaling
set.seed(42)
q<-f.get_scaling("Gr2")
q.cds<-q[[1]]
q.by<-q[[2]]


# events for paper
f.plot_event_fin(event="banAB", c.orig_data=c14, b.origdata = by14, cds_quantiles = q.cds, by_quantiles = q.by)
f.plot_event_fin(event="isoGr", c.orig_data=cGr, b.origdata = byGr, cds_quantiles = q.cds, by_quantiles = q.by)
f.plot_event_fin(event="intGS1", c.orig_data=cGr, b.origdata = byGr, cds_quantiles = q.cds, by_quantiles = q.by)

# all events
f.plot_event(event="Gr1", c.orig_data=cGr, b.origdata = byGr, cds_quantiles = q.cds, by_quantiles = q.by)
f.plot_event(event="Ie", c.orig_data=cGr, b.origdata = byGr, cds_quantiles = q.cds, by_quantiles = q.by)
f.plot_event(event="Pt", c.orig_data=cGr, b.origdata = byGr, cds_quantiles = q.cds, by_quantiles = q.by)
f.plot_event(event="Gr2", c.orig_data=cGr, b.origdata = by14, cds_quantiles = q.cds , by_quantiles = q.by) # !asw pre w/ Gr
f.plot_event(event="SMP1", c.orig_data=cGr, b.origdata = byGr, cds_quantiles = q.cds, by_quantiles = q.by)
f.plot_event(event="SMP2", c.orig_data=cGr, b.origdata = by14, cds_quantiles = q.cds, by_quantiles = q.by)
f.plot_event(event="Es", c.orig_data=c14, b.origdata = by14, cds_quantiles = q.cds, by_quantiles = q.by)
f.plot_event(event="trd", c.orig_data=c14, b.origdata = by14, cds_quantiles = q.cds, by_quantiles = q.by)
f.plot_event(event="ban", c.orig_data=c14, b.origdata = by14, cds_quantiles = q.cds, by_quantiles = q.by)
f.plot_event(event="GD", c.orig_data=c14, b.origdata = by14, cds_quantiles = q.cds, by_quantiles = q.by)
