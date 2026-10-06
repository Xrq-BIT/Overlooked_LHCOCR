suppressPackageStartupMessages({library(sf);library(terra);library(grid)})
args <- grep("^--file=",commandArgs(FALSE),value=TRUE)
root <- if(length(args)) dirname(normalizePath(sub("^--file=","",args[1]))) else getwd()
out <- file.path(root,'reproduced_Fig2')
dir.create(out,showWarnings=FALSE)
ids <- c('L100000100223','L100000100555','L100000100200','L100000100144')
sites <- c('Huainan Fengtai','Handan Eastern Outskirt','Anhui Bengbu','Maritsa Iztok-3')
boundary_file <- Sys.getenv("SI_BOUNDARY_FILE")
if(!nzchar(boundary_file) || !file.exists(boundary_file)) stop("Set SI_BOUNDARY_FILE to your boundary SHP/GPKG; boundary data are not included in this package.")
polys <- st_read(boundary_file,quiet=TRUE)
if(!"gem_id" %in% names(polys)) polys$gem_id <- paste0("L",polys$id)
cases <- list(); provenance <- list()
for(i in seq_along(ids)) {
  p <- polys[polys$gem_id==ids[i],]
  stopifnot(nrow(p)==1,all(st_is_valid(p)))
  centre <- st_coordinates(st_centroid(st_geometry(st_transform(p,4326))))[1,]
  epsg <- ifelse(centre[2]>=0,32600,32700)+floor((centre[1]+180)/6)+1
  p <- st_transform(p,epsg)
  tif <- file.path(root,'source_data',paste0(ids[i],'.tif'))
  r <- project(rast(tif),paste0('EPSG:',epsg),method='bilinear',res=2)
  b <- st_bbox(p); side <- max(b$xmax-b$xmin,b$ymax-b$ymin)+400
  cx <- (b$xmin+b$xmax)/2;cy <- (b$ymin+b$ymax)/2
  r <- crop(r,ext(cx-side/2,cx+side/2,cy-side/2,cy+side/2))
  v <- values(r);stopifnot(ncol(v)==3)
  missing <- !complete.cases(v);v[missing,]<-255
  v[] <- pmin(255,pmax(0,v))
  raster <- as.raster(matrix(rgb(v[,1],v[,2],v[,3],maxColorValue=255),nrow=nrow(r),byrow=TRUE))
  cases[[i]] <- list(image=raster,p=p,extent=as.vector(ext(r)))
  provenance[[i]] <- data.frame(gem_id=ids[i],site=sites[i],image_md5=unname(tools::md5sum(tif)),
    xmin=xmin(r),xmax=xmax(r),ymin=ymin(r),ymax=ymax(r),display_epsg=epsg,
    display_resolution_m=2,display_resampling='bilinear',RGB_scale=255)
}
# Paired images show inventory footprints, not prediction-versus-reference accuracy.
W <- 183;H <- 103; panel <- 41
label_pt <- 27*(183/25.4*72)/1200
draw <- function() {
  grid.newpage()
  pushViewport(viewport(xscale=c(0,W),yscale=c(H,0)))
  label <- function(s,x,y,size=8,face='plain',colour='black',just='left') {
    grid.text(s,x=unit(x,'native'),y=unit(y,'native'),just=just,
              gp=gpar(fontfamily='Arial',fontsize=size,fontface=face,col=colour))
  }
  for(i in seq_along(cases)) for(j in 1:2) {
    c <- cases[[i]]
    column <- (i-1) %% 2; row <- (i-1) %/% 2
    origin <- 2 + column*91
    x <- origin+1.5+(j-1)*43.5;top <- 8+row*51
    if(j==1) {
      label(letters[i],x,top-3.5,label_pt,'bold')
      label(sites[i],x+6,top-3.5,7)
      grid.rect(x=unit(origin,'native'),y=unit(top-6.5,'native'),
                width=unit(87.5,'mm'),height=unit(49,'mm'),just=c('left','top'),
                gp=gpar(fill=NA,col='#777777',lwd=.5))
    }
    pushViewport(viewport(x=unit(x,'native'),y=unit(top,'native'),width=unit(panel,'mm'),
                          height=unit(panel,'mm'),just=c('left','top'),clip='on'))
    grid.raster(c$image,width=1,height=1,interpolate=FALSE)
    if(j==2) {
      xy <- st_coordinates(c$p)
      groups <- interaction(as.data.frame(xy[,setdiff(colnames(xy),c('X','Y')),drop=FALSE]),drop=TRUE)
      for(g in levels(groups)) {
        a <- xy[groups==g,,drop=FALSE]
        xx <- (a[,'X']-c$extent[1])/diff(c$extent[1:2])
        yy <- (a[,'Y']-c$extent[3])/diff(c$extent[3:4])
        grid.lines(xx,yy,gp=gpar(col='white',lwd=1.65,linejoin='round'))
        grid.lines(xx,yy,gp=gpar(col='#F3A32C',lwd=1.05,linejoin='round'))
      }
    }
    grid.text('N',x=.94,y=.93,gp=gpar(fontfamily='Arial',fontsize=6,col='white',fontface='bold'))
    grid.lines(x=c(.94,.94),y=c(.80,.88),arrow=arrow(length=unit(1.1,'mm'),type='closed'),
               gp=gpar(col='white',fill='white',lwd=.65))
    popViewport()
  }
  popViewport()
}
base <- file.path(out,'SI_Fig2_abcd')
svglite::svglite(paste0(base,'.svg'),width=W/25.4,height=H/25.4);draw();dev.off()
rsvg::rsvg_pdf(paste0(base,'.svg'),paste0(base,'.pdf'))
ragg::agg_png(paste0(base,'_preview.png'),width=W,height=H,units='mm',res=300);draw();dev.off()
write.csv(do.call(rbind,provenance),file.path(out,'display_provenance.csv'),row.names=FALSE)
writeLines(c('Image plate: four paired RGB/footprint cases; inventory illustration only.',
 'Same imagery, UTM projection, 2-m resampling and crop rule as the preceding version.',
 'No contrast enhancement, image generation, boundary modification or case replacement.',
 'Scale bars removed at author request. Different cases do not share a common ground scale.',
 'Two-by-two case layout, 183 x 103 mm. Each case contains two proportional image panels.',
 'Panel letters retain exactly the same font size as the four-row version.',
 'Arial bold black panel letters a-d; one thin neutral frame around each paired case.',
 'SVG/PDF retain raster imagery and vector boundary strokes and editable labels.'),file.path(out,'figure_notes.txt'))
cat(base,'\n')
