#' Draw a cleveland dot plot
#'@param data a data.frame
#'@param mapping Set of aesthetic mappings created by aes or aes_.
#'@param reorderByX If true, the data is reordered by x variable
#'@param no Number of data be drawn in plot
#'@param start start point of x axis as ratio to minimum x variable
#'@param interactive A logical value. If TRUE, an interactive plot will be returned
#'@param decreasing Should the sort order be increasing or decreasing?
#'@param ... other arguments passed on to geom_point_interactive
#'@importFrom ggplot2 theme theme_bw facet_grid
#'@importFrom ggiraph geom_segment_interactive geom_point_interactive
#'@export
#'@examples
#' require(ggplot2)
#' require(ggiraph)
#' mtcars$name=rownames(mtcars)
#' ggCLE(data=mtcars,aes(x=mpg,y=name))
#' ggCLE(data=mtcars,aes(x=mpg,y=name,color=am,facet=am),interactive=TRUE)
#' if(requireNamespace("gcookbook",quietly=TRUE)){
#'    require(gcookbook)
#'    ggCLE(data=tophitters2001,aes(x=avg,y=name),no=20,interactive=TRUE)
#'    ggCLE(data=tophitters2001,aes(x=avg,y=name,color=lg,facet=lg),no=20)
#' }
ggCLE=function(data,mapping,
               reorderByX=TRUE,no=NULL,start=0.99,interactive=FALSE,decreasing=TRUE,...){

        # data=tophitters2001
        # mapping=aes(x=avg,y=name,color=lg,facet=lg)
        # reorderByX=TRUE;no=NULL;start=0.99;interactive=FALSE;decreasing=TRUE

        xvar=paste(mapping[["x"]])
        yvar=paste(mapping[["y"]])
        facetvar=NULL
        if("facet" %in% names(mapping)) facetvar<-paste(mapping[["facet"]])
        colorvar=NULL
        if("colour" %in% names(mapping)) colorvar<-paste(mapping[["colour"]])


     # data=tophitters2001;xvar="avg";yvar="name";colorvar="lg";facetvar="lg";
     # reorderByX=TRUE;no=NULL;start=0.95;interactive=TRUE;decreasing=FALSE
    if(!is.null(no)) data<-data[order(data[[xvar]],decreasing=decreasing)[1:no],]
    data$id=1:nrow(data)
    if(is.null(yvar)) yvar="id"
    if(reorderByX) {
        nameorder=data[[yvar]][order(data[[xvar]],decreasing=(!decreasing))]
        data$yvar1=factor(data[[yvar]],levels=nameorder)
        #str(data)
        # data$yvar1=reorder(data[[yvar]],data[[xvar]])
        # if(decreasing==FALSE) data$yvar1=factor(data$yvar1,levels=rev(data$yvar1))
    } else {
        data$yvar1=data[[yvar]]
    }

    data$tooltip=paste0(data$id,"<br>",yvar,"=",data[[yvar]],"<br>",xvar,"=",data[[xvar]])
    xend<-min(data[[xvar]])*start
    xend<-as.character(xend)

    data
    p<-ggplot(data,aes_string(x=xvar,y="yvar1",colour=colorvar,
                              data_id="id",tooltip="tooltip"))

    p<-p+geom_segment_interactive(aes_string(xend=xend,yend="yvar1"))+
        geom_point_interactive(size=3,...)+
        ylab("")+
        theme_bw()+theme(legend.position="none")


    if(!is.null(facetvar)) {
        myformula=as.formula(paste0(facetvar,"~."))
        p<-p+facet_grid(myformula,scales="free_y",space="free_y")
    }



    if(interactive){
        tooltip_css <- "background-color:white;font-style:italic;padding:10px;border-radius:10px 20px 10px 20px;"
        #hover_css="fill-opacity=.3;cursor:pointer;stroke:gold;"
        hover_css="r:4px;cursor:pointer;stroke-width:6px;"
        selected_css = "fill:#FF3333;stroke:black;"
        p<-ggiraph(code=print(p),tooltip_extra_css=tooltip_css,tooltip_opacity=.75,
                   zoom_max=10,hover_css=hover_css,selected_css=selected_css)
    }
    p

}

