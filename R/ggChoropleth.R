#' Draw an interactive choropleth map
#' @param data a data.frame
#' @param mapping Set of aesthetic mappings created by aes or aes_. Passed on geom_map_interactive. Required mappings are map_id and fill. Possible mapping is facet.
#' @param map a map maybe a result of map_data()
#' @param colors A vector of colours used as a parameter of scale_fill_gradientn()
#' @param palette A palette name used for discrete fill var
#' @param title A title
#' @param digits An integer indicating the number of decimal places
#' @param interactive Logical. If positive an interactive map will be made
#' @param ... other arguments passed on to geom_map_interactive
#' @importFrom ggplot2 scale_fill_brewer scale_colour_gradientn facet_wrap scale_fill_gradientn coord_map
#' @importFrom ggiraph geom_map_interactive
#' @export
#'@examples
#'#crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)
#'#require(ggplot2)
#'#require(ggiraph)
#'#require(maps)
#'#require(mapproj)
#'#states_map <- map_data("state")
#'#ggChoropleth(crimes,aes(fill=Murder,map_id=state),map=states_map,interactive=TRUE)
#'#ggChoropleth(crimes,aes(fill=c(Murder,Rape),map_id=state),map=states_map,interactive=TRUE)
#'#ggChoropleth(crimes,aes(map_id=state),map=states_map,interactive=TRUE)
ggChoropleth=function(data,mapping,map,
                      colors=c('white','orange','red'),palette=NULL,
                      title="",digits=1,interactive=FALSE,...){

        mapidvar<-fillvar<-facetvar<-tooltip<-NULL
        if("map_id" %in% names(mapping)) mapidvar<-paste(mapping[["map_id"]])
        if("fill" %in% names(mapping)) fillvar<-paste(mapping[["fill"]])
        if("facet" %in% names(mapping)) facetvar<-paste(mapping[["facet"]])
        if("tooltip" %in% names(mapping)) tooltip<-paste(mapping[["tooltip"]])
        #if(is.null(fillvar)) warning("Aestheic mapping to 'fill' is required.")
        if(is.null(mapidvar)) warning("Aestheic mapping to 'map_id' is required.")

        longdata=FALSE
        fillvar
        if(is.null(fillvar)) {
                (select=sapply(data,is.numeric))
                (fillvar=colnames(data)[select])
                longdata=TRUE
        } else {
                fillvar=paste0(mapping[["fill"]])
                if(length(fillvar)>1) {
                        fillvar<-fillvar[-1]
                        longdata=TRUE
                }
        }
        if(longdata){
                data<-data[c(mapidvar,tooltip,fillvar)]
                longdf<-melt(data,id.vars=c(mapidvar,tooltip))
                data<-longdf
        }



    data$data_id=data[[mapidvar]]
    if(is.null(tooltip)) {
            if(longdata){
                    data$tooltip=paste0(data[[mapidvar]],"<br>",
                                        data[["variable"]],"<br>",data[["value"]])
            } else{
                if(is.numeric(data[[fillvar]])) data[[fillvar]]=round(data[[fillvar]],digits)
                data$tooltip=paste0(data[[mapidvar]],"<br>",
                            fillvar,"<br>",data[[fillvar]])
            }
    } else {
            if(longdata){
                    data$tooltip=paste0(data[[tooltip]],"<br>",
                                        data[["variable"]],"<br>",data[["value"]])
            } else{
                if(is.numeric(data[[fillvar]])) data[[fillvar]]=round(data[[fillvar]],digits)
                data$tooltip=paste0(data[[tooltip]],"<br>",fillvar,"<br>",data[[fillvar]])
            }
    }
    mycolors=colors
    tooltip_css <- "background-color:white;font-style:italic;padding:10px;border-radius:20px 20px 20px 20px;"

    if(longdata) {
            p<-ggplot(data=data,aes_string(map_id=mapidvar,fill="value"))
            p<-p+scale_fill_gradientn(colours=mycolors)
    } else{
            p<-ggplot(data=data,mapping)
            if(is.numeric(data[[fillvar]])) {
                    p<-p+scale_fill_gradientn(colours=mycolors)
            } else {
                    if(is.null(palette)) {
                            p<-p+scale_colour_gradientn(colours=mycolors)
                    } else {
                            p<-p+scale_fill_brewer(palette)
                    }
            }
    }
    p<-p+ expand_limits(x=map$long,y=map$lat)+coord_map()
    p<-p+geom_map_interactive(aes_string(data_id="data_id",tooltip="tooltip"),
                              map=map,colour='black',size=0.1,...)
    p<-p+labs(y="",x="")

    if(longdata) p<-p+facet_wrap("variable")
    if(!is.null(facetvar)) p<-p+facet_wrap(facetvar)
    if(title!="") p<-p+ ggtitle(title)

    if(interactive) p<-ggiraph(code=print(p),tooltip_extra_css = tooltip_css,zoom_max=10)
    p
}


