#' Draw a heatmap of correlation test
#'
#' @param data A data.frame
#' @param label if 0, no label(default), if 1, use r value as label, if 2, use r value with significant mark as label
#' @param colors colors for low, mid and high correlation values
#' @param title if true, add title to the heatmap
#'@param interactive A logical value. If TRUE, an interactive plot will be returned
#' @param ... further arguments to be passed to cor.test
#' @importFrom mycor mycor
#' @importFrom ggplot2 scale_fill_gradient2 coord_equal geom_text
#' @importFrom ggiraph geom_tile_interactive
#' @export
#' @examples
#' require(mycor)
#' require(ggplot2)
#' require(ggiraph)
#' ggCor(iris)
#' ggCor(iris,label=2,interactive=TRUE)
#' ggCor(mtcars,interactive=TRUE)
#' ggCor(iris,method="pearson",interactive=TRUE)
ggCor=function(data,label=0,colors=NULL,title=FALSE,interactive=FALSE,...){
    # df=iris;
    # result=mycor(iris)
    result=mycor::mycor(data,...)

    if(is.null(colors)) colors=c("#6D9EC1","white","#E46726")
    cor_mat<-result$r
    p_mat<-result$p
    diag( cor_mat ) <- NA
    diag( p_mat ) <- NA
    var1 <- rep( row.names(cor_mat), ncol(cor_mat) )
    var2 <- rep( colnames(cor_mat), each = nrow(cor_mat) )
    cor <- as.numeric(cor_mat)
    cor_mat <- data.frame( var1 = var1, var2 = var2,
                           cor = cor, stringsAsFactors = FALSE )
    pval=as.numeric(p_mat)
    cor_mat$label=ifelse(is.na(cor_mat$cor),"",sprintf("%0.2f",cor_mat$cor))
    if(label==2) cor_mat$label=paste0(cor_mat$label,ifelse(is.na(pval),"",ifelse(pval<0.01,"**",ifelse(pval<0.05,"*",""))))
    cor_mat$p=ifelse(is.na(pval),"",ifelse(pval<0.001,"< 0.001",sprintf(" = %0.3f",pval)))
    cor_mat[["tooltip"]] <-
        sprintf("<i>%s</i> vs <i>%s</i>:</br><i>r</i> = %s</br><i>p</i> %s",
                var1, var2, cor_mat$label,cor_mat$p)

    # ggplot creation and ggiraph printing ----
    p <- ggplot(data = cor_mat, aes_string(x = "var1", y = "var2",tooltip="tooltip") ) +
        geom_tile_interactive(aes(fill = cor), colour = "white") +
        scale_fill_gradient2(low = colors[1], mid = colors[2], high = colors[3], limits = c(-1, 1)) +
        coord_equal()+
        xlab("")+ylab("")
    if(title) {
        title=paste0(result$out$method,",",result$out$alternative)
        p<-p+ggtitle(title)
    }
    if(label>0) p<-p+geom_text(aes(label=label))
    if(interactive){
            tooltip_css <- "background-color:white;font-style:italic;padding:10px;border-radius:10px 20px 10px 20px;"
            hover_css="r:4px;cursor:pointer;stroke-width:6px;"
            selected_css = "fill:#FF3333;stroke:black;"
            p<-ggiraph(code=print(p),tooltip_extra_css=tooltip_css,tooltip_opacity=.75,
                       zoom_max=10,hover_css=hover_css,selected_css=selected_css)
    }
    p
}
