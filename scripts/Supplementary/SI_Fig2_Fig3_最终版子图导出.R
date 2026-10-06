suppressPackageStartupMessages({library(xml2);library(rsvg)})
args <- grep("^--file=",commandArgs(FALSE),value=TRUE)
folder <- if(length(args)) dirname(normalizePath(sub("^--file=","",args[1]))) else getwd()
# Preserve the exact final artwork, including each paired image and its frame.
for(i in 1:4) {
  d <- read_xml(file.path(folder,"source_data/SI_Fig2_final_layout.svg"))
  groups <- xml_find_all(d,"/*[local-name()='svg']/*[local-name()='g']/*[local-name()='g']")
  stopifnot(length(groups)==17)
  keep <- ((i-1)*4+1):(i*4)
  xml_remove(groups[-keep])
  x <- (2+((i-1)%%2)*91)*72/25.4-0.5
  y <- (1.5+((i-1)%/%2)*51)*72/25.4-0.5
  w <- 87.5*72/25.4+1; h <- 49*72/25.4+1
  xml_set_attr(d,"viewBox",paste(x,y,w,h))
  xml_set_attr(d,"width",paste0(w,"pt"));xml_set_attr(d,"height",paste0(h,"pt"))
  bg <- read_xml(sprintf('<rect xmlns="http://www.w3.org/2000/svg" x="%s" y="%s" width="%s" height="%s" fill="white"/>',x,y,w,h))
  xml_add_child(d,bg,.where=0)
  base <- file.path(folder,paste0("SI_Fig2_",letters[i]))
  write_xml(d,paste0(base,".svg"));rsvg_pdf(paste0(base,".svg"),paste0(base,".pdf"))
}
doc <- read_xml(file.path(folder,"source_data/SI_Fig3_final_layout.svg"))
nodes <- xml_find_all(doc,"/*[local-name()='svg']/*[local-name()='svg']")
stopifnot(length(nodes)==2)
for(i in 1:2) {
  # Keep the SI panel viewBox and scaling, not an older map version.
  n <- nodes[[i]]
  d <- read_xml('<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink"/>')
  w<-as.numeric(xml_attr(n,"width"));h<-as.numeric(xml_attr(n,"height"))
  xml_set_attr(d,"width",paste0(w,"pt"));xml_set_attr(d,"height",paste0(h,"pt"))
  xml_set_attr(d,"viewBox",paste(0,0,w,h))
  xml_add_child(d,read_xml('<rect xmlns="http://www.w3.org/2000/svg" width="100%" height="100%" fill="white"/>'))
  xml_add_child(d,n,.copy=TRUE)
  child<-xml_find_first(d,"./*[local-name()='svg']")
  xml_set_attr(child,"x","0");xml_set_attr(child,"y","0")
  base<-file.path(folder,paste0("SI_Fig3_",letters[i]))
  write_xml(d,paste0(base,".svg"));rsvg_pdf(paste0(base,".svg"),paste0(base,".pdf"))
}
