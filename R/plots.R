
#' @title This function returns a plot.
#' @param dataf Data frame containing information about the experiment.
#' @param poly Use large polygons (TRUE or FALSE).
#' @param y_rev Reverse the y axis.
#' @param x_rev Reverse the x axis.
#' @param poly If TRUE set the polygon pattern.
#' @param rep_unrep Replicated or unreplicated selection design.

#' @description It prints a graphic.
#'
#' @return A dataframe.
#' @keywords internal
#'  






#Hexagon patterns at lines 65 79 



#Data needed for this example (replicated R7 )



plot_convert<-function(dataf,poly=TRUE,y_rev=TRUE,x_rev=FALSE,rep_unrep=NULL){
  
    #print(dataf)
    x_rev=FALSE
    #set.seed(150)#set for the random numbers used in graphic
    column<-dataf$Entry
    x_pos<-unlist(dataf$XPos)
    y_pos<-unlist(dataf$YPos) #reverses the range of y axis
    maxxp<-max(dataf$XPos)
    minxp<-min(dataf$XPos)
    maxyp<-max(dataf$YPos)
    minyp<-min(dataf$YPos)
    dataf[is.na(dataf)] = 0 #if there is NaN replace it with 0
    lim<-minxp
    #match colours to entries
    #by default the y axis is reverse
    y_range<-c(maxyp+lim,0) 
    x_range<-c(0,maxxp+lim)   
    if(x_rev==TRUE) {x_range<-c(maxxp+lim,0)}  
    if(y_rev==FALSE){y_range<-c(0,maxyp+lim)}

    #Change this since for some reason does not eval y_lim 
    #if(y_rev==TRUE) {y_range<-c(maxyp+lim,0)}else if(y_rev==FALSE){y_range<-c(0,maxyp+lim)}
    #if(x_rev==TRUE) {x_range<-c(maxxp+lim,0)}else if(x_rev==FALSE){x_range<-c(0,maxxp+lim)}
    #y_range<-c(maxyp+lim,0)
    #print(c(maxyp+lim,0))
    #print(maxyp)
    #print(lim)
    #print(y_range)
    #set the window dimensions
    device_width<-max(dataf$Plant)
    device_heigh<-max(dataf$Row)
    if (device_width>18){device_width=18}else if (device_width<4){device_width=4}
    if (device_heigh>11){device_heigh=11}else if (device_heigh<4){device_heigh=4}

    #grDevices::dev.new(width=device_width, height=device_heigh)
    
    #This is test (it should be removed) 
    #par( mfrow= c(3,2) )
 

    graphics::par(mai=c(0.82,1.2,1.2,0.82)) # set the margins




    graphics::plot(x_pos , y_pos , axes=FALSE ,frame.plot=TRUE , # ylab(el),  # yaxt , xaxt create no axis by default
        ylim = y_range , xlim=x_range,
        xaxs ="i" , yaxs="i" ,#i means start from zero
    #Remove or add plotting symbol by chang the pch variable
    #
    #    pch=26 ,cex=5  ,col=color_vec_full,
    #
        col = grDevices::rgb(red = 1, green = 0, blue = 0, alpha = 0),
        ylab="" ,xlab=""#set notitle for y axis and use title function instead 
    )#plot

    #set the hexagon pattern and create polygon where R is 
    d<-dataf$XPos[1]
    a<-d*sqrt(3)/3
    b<-d*sqrt(3)/6
    x<-x_pos
    y<-y_pos
    #Show big polygons or small ones
    if(poly==TRUE){
        graphics::segments( 
            c(-d/2 +x,0 +x , d/2+x , d/2+x  ,0+x  , -d/2+x ),
            c(b+y  ,a+y  , b+y , -b+y ,-a+y  , -b+y ),

            c(0+x   ,d/2+x , d/2+x ,  0+x , -d/2+x , -d/2+x  ),
            c(a+y   ,b+y , -b+y,  -a+y, -b+y , b+y )
        )
    


        #Create a polygon where the control entry (if unreplicated) or R entry is (if replicated)
        for(i in 1:length(dataf$Entry)){
            #For some reason this does not work (i removed the if(dataf$Entry[i]... ) 
            #if(dataf$Entry[i] %in% c("Control1","Control2","Control3")|dataf$Entry[i]==max(dataf$Entry) && !(rep_unrep=="unrep")){
            if(1){
                
                x<-dataf$XPos[i]
                y<-dataf$YPos[i]
                #Select different colours for each control
                if(dataf$Entry[i]=="Control1"){
                    graphics::polygon(c(-d/2 +x,0 +x , d/2+x , d/2+x  ,0+x  , -d/2+x ),
                            c(b+y  ,a+y  , b+y , -b+y ,-a+y  , -b+y ),
                            col = "#6BD7AF"
                    )
                }else if (dataf$Entry[i]=="Control2"){
                            graphics::polygon(c(-d/2 +x,0 +x , d/2+x , d/2+x  ,0+x  , -d/2+x ),
                                c(b+y  ,a+y  , b+y , -b+y ,-a+y  , -b+y ),
                                col = "orange"
                            )
                }else if (dataf$Entry[i]=="Control3"){
                            graphics::polygon(c(-d/2 +x,0 +x , d/2+x , d/2+x  ,0+x  , -d/2+x ),
                                c(b+y  ,a+y  , b+y , -b+y ,-a+y  , -b+y ),
                                col = "grey"
                            )
                }else if (dataf$Entry[i]==max(dataf$Entry) && rep_unrep=="rep")  {

                    graphics::polygon(c(-d/2 +x,0 +x , d/2+x , d/2+x  ,0+x  , -d/2+x ),
                            c(b+y  ,a+y  , b+y , -b+y ,-a+y  , -b+y ),
                            col = "#6BD7AF"
                    )

                }   
            }
        }
    
    


        #set the labels 
        labels_<-dataf$Entry

            #Removed this (adjust the text size)
            #graphics::text(
            #    x_pos ,y_pos , labels=labels_ , #,pos=3, offset=0.9   #change the position of labels with this comment
            #    
            #    cex=70/(max(as.numeric(dataf$Row))*max(as.numeric(dataf$Plant)))
            #) 
        #Drow each point seperately    
        for (i in 1:nrow(dataf)){
            if(dataf$Entry[i]=="Control1" | dataf$Entry[i]=="Control2"|dataf$Entry[i]=="Control3"){
                #Change the Control to Ctrl
                if(dataf$Entry[i]=="Control1"){lab<-"Ctrl1"}
                else if(dataf$Entry[i]=="Control2"){lab<-"Ctrl2"}
                else if(dataf$Entry[i]=="Control3"){lab<-"Ctrl3"}
                
                graphics::text(
                    dataf$XPos[i],
                    dataf$YPos[i],
                    labels=lab
                    
                    #,cex=20/(max(as.numeric(dataf$Row))*max(as.numeric(dataf$Plant)))
                )
            }else{
                graphics::text(
                    dataf$XPos[i],
                    dataf$YPos[i],
                    labels=dataf$Entry[i]
                )    

            }
        }


        graphics::title(ylab="YPos", line=4)      
        graphics::mtext("XPos", side=3, line=4 )    
    }#if poly == true
    else if(poly==FALSE){
        #generate random numbers for coloring polygons if replicated
        if(rep_unrep=="rep"){
            color_vec<-sample(1:100,3*max(dataf$Entry),replace=TRUE)
            color_vec<-color_vec/100
            color_r<-head(color_vec,max(dataf$Entry))
            #Rotate the vector
            color_vec<-c( tail(color_vec,-max(dataf$Entry)),head(color_vec,(max(dataf$Entry))) )
            color_g<-head(color_vec,max(dataf$Entry))
            color_vec<-c( tail(color_vec,-max(dataf$Entry)),head(color_vec,(max(dataf$Entry))) )
            color_b<-head(color_vec,max(dataf$Entry))

        }
        for(i in 1:length(dataf$Entry)){
              
            x<-dataf$XPos[i]
            y<-dataf$YPos[i]

            #polygon_dimensions_x<-c(-d/2/3 +x,0 +x , d/2/3+x , d/2/3+x  ,0+x  , -d/2/3+x )
            #polygon_dimensions_y<-c(b/3+y  ,a/3+y  , b/3+y , -b/3+y ,-a/3+y  , -b/3+y )
            #Select different colours for each control
            if(dataf$Entry[i]=="Control1"){
                        graphics::points(x,
                                y,
                                pch=19,
                                col = "#6BD7AF",
                                cex=200/(max(as.numeric(dataf$Row))*max(as.numeric(dataf$Plant)))
                        )
            }else if (dataf$Entry[i]=="Control2"){
                        graphics::points(x,
                                y,
                                pch=19,
                                col = "orange",
                                cex=200/(max(as.numeric(dataf$Row))*max(as.numeric(dataf$Plant)))

                        )
            }else if (dataf$Entry[i]=="Control3"){
                        graphics::points(x,
                                y,
                                pch=19,
                                col = "blue",
                                cex=200/(max(as.numeric(dataf$Row))*max(as.numeric(dataf$Plant)))
                        )
            }else if (rep_unrep %in% c("unrep","unrep1","unrep3") )  {
                    
                        graphics::points(x,
                                y,
                                pch=19,
                                col = grDevices::rgb(0.5,0.5,0.5,alpha=0.3),
                                cex=200/(max(as.numeric(dataf$Row))*max(as.numeric(dataf$Plant)))
                                
                        )
            }else if (rep_unrep=="rep")  {
                graphics::points(
                    x,
                    y,
                    pch=19,
                    col = grDevices::rgb(
                        color_r[dataf$Entry[i]],
                        color_g[dataf$Entry[i]],
                        color_b[dataf$Entry[i]]
                    ),
                    cex=200/(max(as.numeric(dataf$Row))*max(as.numeric(dataf$Plant)))
                                
                )
                        
            }
            
            
             
        }
        #set the labels and labelsize
        labels_<-dataf$Entry
        text_size<-200/(max(as.numeric(dataf$Row))*max(as.numeric(dataf$Plant)))
        if(text_size<0.2){text_size<-0.2}else if(text_size>0.8){text_size<-0.8}
        
        graphics::text(x_pos ,y_pos , labels=labels_   ,pos=3, offset=max(10/(max(as.numeric(dataf$Row))),0.1),   #change the position of labels with this comment
                        cex=text_size #set a limit for the size of text
         ) 
        graphics::title(ylab="YPos", line=4)      
        graphics::mtext("XPos", side=3, line=4 )    
    }
        
        
        #deactivate or activate axis with this lines
        graphics::axis(3,at= append((unlist(dataf$XPos)),0)    ,   las=2   ) #las - vertical
        graphics::axis(2,at=append(round(unlist(dataf$YPos),3),0) , las=2) #las - vertical

}