#' @title Replicated honeycomb selection designs.
#' 
#' @description This function creates a data frame of a replicated honeycomb selection design.
#'
#' @param R The R design.
#' @param K The K parameter.
#' @param rows The number of rows. 
#' @param plpr The number of plants per row.
#' @param distance Distance between plants in meters.
#'  
#' @param poly If TRUE the polygon pattern is displayed.

#' @return A dataframe.
#' @references 
#' Fasoula V. (2013). Prognostic Breeding: A New Paradigm for Crop Improvement. Plant Breeding Reviews 37: 297-347. 10.1002/9781118497869.ch6. \doi{10.1002/9781118497869.ch6}
#'
#' Fasoula V.A., and Tokatlidis I.S. (2012). Development of crop cultivars by honeycomb breeding. Agronomy for Sustainable Development 32:161–180. 10.1007/s13593-011-0034-0 \doi{10.1007/s13593-011-0034-0}
#'
#' Fasoulas A.C., and Fasoula V.A. (1995). Honeycomb selection designs. In J. Janick (ed.). Plant Breeding Reviews 13: 87-139.  \doi{10.1002/9780470650059.ch3}
#' 
#' Tokatlidis I. (2016). Sampling the spatial heterogeneity of the honeycomb model in maize and wheat breeding trials: Analysis of secondary data compared to popular classical designs. Experimental Agriculture, 52(3), 371-390. \doi{10.1017/S0014479715000150}
#' 
#' Tokatlidis I., and Vlachostergios D. (2016). Sustainable Stewardship of the Landrace Diversity. Diversity 8(4):29. \doi{10.3390/d8040029}
#' 
#' 

#' @examples HDR(7,2,10,10,1)

#' @export




HDR<-function(R,K,rows,plpr,distance,poly=TRUE ){
    #Added these two lines as suggested so the old adjustments will be restored 
    oldpar <- graphics::par(no.readonly = TRUE) 
    on.exit(graphics::par(oldpar))  
    
    ring<-6
   
    #Don't check here if this R exists
    replicated_info<-generate(R)
    #Test if the given k is available
     
        if( !(K %in% unlist(replicated_info[,c("K1","K2","K3","K4","K5","K6")])) ) {
            
            warning("Not supported K value. Supported K values for this design can be found using generate function.") #This has to be modified
            #print(replicated_info[,c("Type","K1","K2","K3","K4","K5","K6")])
            return()
        }    
    #Get the number of groups from  the line that k belongs
    Groups<-replicated_info[replicated_info$K1==K | replicated_info$K2==K |replicated_info$K3==K |replicated_info$K4==K |replicated_info$K5==K |replicated_info$K6==K  ,"Groups"]
    
    #set the variables globally
    R_global<-R
    #R_global<<-R
    K_global<-K
    #K_global<<-K
    rows_global<-rows
    #rows_global<<-rows
    plpr_global<-plpr
    #plpr_global<<-plpr
    space_global<-distance
    #space_global<<-distance
    ring_global<-ring
    #ring_global<<-ring
    rep_unrep<-"rep"
    #rep_unrep<<-"rep"
    



    #proceed to the creation of dataframe and file 
     
    #print(main_data)
    #create the datafraime containing the entries
    #Groups<-3
    #R<-9
    #rows<-20
    #K<-2
    #create a data frame with correct entries in each line
    df_with_entries<- as.data.frame( split(R:1 ,rep(1:(R/Groups),Groups))) 
    #fill the correct ammount of rows 
    df_with_entries<-head(df_with_entries[rep(1:Groups,rows ),],rows)
    #Restore the order of data frame lines
    rownames(df_with_entries) = seq(length=nrow(df_with_entries))

    #inverse the order of data frame 
    df_with_entries=df_with_entries[,order(ncol(df_with_entries):1)]
    group_size<-R/Groups #variable for the size of groups 

####print(df_with_entries)



    #Set the first line of the vector seperately
    pick_value<-c(tail(unlist( df_with_entries[1,]),1),head(unlist(df_with_entries[1,]),-1))
    df_with_entries[1,]<-pick_value
    counter<-1



    #Run the vector and bring entries in the right sequence
    for(i in 2:nrow(df_with_entries)){
        
        if(i%%2==0){counter<-(counter+K)%%group_size} else if (i%%2==1){counter<-(counter+K+1)%%group_size}
        
        
        if(counter==0){counter=group_size}
########print(counter)
        
         
        pick_value<-c(tail(unlist(df_with_entries[i,]), counter),head(unlist(df_with_entries[i,]),-counter))
        df_with_entries[i,]<-pick_value
        
    }
    #print(df_with_entries)

    
    #append columns in the data frame
    df_with_entries<-df_with_entries[,rep((1:group_size),plpr)]
    #Take only as many columns as plants per row
    df_with_entries<-df_with_entries[,1:plpr]
    #Restore the names of dataframe
    colnames(df_with_entries) = seq(length=ncol(df_with_entries))
    #Pass the values to main data frame
    



    main_data<-data.frame( 
        "Entry"  =as.vector(t(df_with_entries)),
        "Row"    =rep(1:rows,each=plpr),
        "Plant"  =rep(1:plpr,rows),
        
        "XPos"   =head(rep(c((1:plpr)*distance,((1:plpr)*(distance)+(distance/2))),rows),rows*plpr),
        "YPos"   =rep((distance*(3)**(1/2)/2)*(1:rows),each=plpr),
        "Data"    =rep(NA,rows*plpr)
    ) 
    Main_Data_Frame<-main_data
    #Main_Data_Frame<<-main_data
    #cat("Analysis table is saved in \"Main_Data_Frame\"\n\n\n")
    #print(main_data)
    plot_convert(main_data,poly,rep_unrep=rep_unrep)
    return(main_data)
}


#' @title Non-replicated honeycomb selection design.
#' 
#' @description This function creates a data frame of an unreplicated honeycomb selection design.
#'
#' @param  rows The number of rows. 
#' @param  plpr The number of plants per row.
#' @param  distance Distance between plants in meters.
#'  
#' @param  poly If TRUE set polygon pattern is displayed.
#' @references 
#' Fasoula V. (2013). Prognostic Breeding: A New Paradigm for Crop Improvement. Plant Breeding Reviews 37: 297-347. 10.1002/9781118497869.ch6. \doi{10.1002/9781118497869.ch6}
#'
#' Fasoula V.A., and Tokatlidis I.S. (2012). Development of crop cultivars by honeycomb breeding. Agronomy for Sustainable Development 32:161–180. 10.1007/s13593-011-0034-0 \doi{10.1007/s13593-011-0034-0}
#'
#' Fasoulas A.C., and Fasoula V.A. (1995). Honeycomb selection designs. In J. Janick (ed.). Plant Breeding Reviews 13: 87-139.  \doi{10.1002/9780470650059.ch3}
#' 
#' Tokatlidis I. (2016). Sampling the spatial heterogeneity of the honeycomb model in maize and wheat breeding trials: Analysis of secondary data compared to popular classical designs. Experimental Agriculture, 52(3), 371-390. \doi{10.1017/S0014479715000150}
#' 
#' Tokatlidis I., and Vlachostergios D. (2016). Sustainable Stewardship of the Landrace Diversity. Diversity 8(4):29. \doi{10.3390/d8040029}
#' @return A dataframe.
#' @examples HDNR0(10,10,1)
#' @export



HDNR0<-function(rows,plpr,distance,poly=TRUE){
    #Added these two lines as suggested so the old adjustments will be restored 
    oldpar <- graphics::par(no.readonly = TRUE) 
    on.exit(graphics::par(oldpar))  
    
    ring<-6
    R_global<-""
    #R_global<<-""
    K_global<-""
    #K_global<<-""
    rows_global<-rows
    #rows_global<<-rows
    plpr_global<-plpr
    #plpr_global<<-plpr
    space_global<-distance
    #space_global<<-distance
    ring_global<-ring
    #ring_global<<-ring
    rep_unrep<-"unrep"
    #rep_unrep<<-"unrep"


    main_data<-data.frame( 
        "Entry"  =1:(plpr*rows),
        "Row"    =rep(1:rows,each=plpr),
        "Plant"  =rep(1:plpr,rows),
        
        "XPos"   =head(rep(c((1:plpr)*distance,((1:plpr)*(distance)+(distance/2))),rows),rows*plpr),
        "YPos"   =rep((distance*(3)**(1/2)/2)*(1:rows),each=plpr),
        "Data"    =rep(NA,rows*plpr)
    ) 
    
    
    
    
    
    
    Main_Data_Frame<-main_data
    #Main_Data_Frame<<-main_data
    #cat("Analysis table is saved in \"Main_Data_Frame\"\n\n\n")
    plot_convert(Main_Data_Frame,poly,rep_unrep=rep_unrep)
    return(Main_Data_Frame)
}

#' @title Non-replicated honeycomb selection design with one control.
#' 
#' @description This function creates a data frame of an unreplicated honeycomb selection design with one control.
#' @param  K The K parameter.  
#' @param  rows The number of rows. 
#' @param  plpr The number of plants per row.
#' @param  distance Distance between plants in meters.
#'   
#' @param poly If TRUE  the polygon pattern is displayed.
#' @references 
#' Fasoula V. (2013). Prognostic Breeding: A New Paradigm for Crop Improvement. Plant Breeding Reviews 37: 297-347. 10.1002/9781118497869.ch6. \doi{10.1002/9781118497869.ch6}
#'
#' Fasoula V.A., and Tokatlidis I.S. (2012). Development of crop cultivars by honeycomb breeding. Agronomy for Sustainable Development 32:161–180. 10.1007/s13593-011-0034-0 \doi{10.1007/s13593-011-0034-0}
#'
#' Fasoulas A.C., and Fasoula V.A. (1995). Honeycomb selection designs. In J. Janick (ed.). Plant Breeding Reviews 13: 87-139.  \doi{10.1002/9780470650059.ch3}
#' 
#' Tokatlidis I. (2016). Sampling the spatial heterogeneity of the honeycomb model in maize and wheat breeding trials: Analysis of secondary data compared to popular classical designs. Experimental Agriculture, 52(3), 371-390. \doi{10.1017/S0014479715000150}
#' 
#' Tokatlidis I., and Vlachostergios D. (2016). Sustainable Stewardship of the Landrace Diversity. Diversity 8(4):29. \doi{10.3390/d8040029}
#' 
#' 
#' @return A dataframe.
#' @examples HDNR1(1,10,10,1) 
#' @export


HDNR1<-function(K,rows,plpr,distance,poly=TRUE){
    #Added these two lines as suggested so the old adjustments will be restored 
    oldpar <- graphics::par(no.readonly = TRUE) 
    on.exit(graphics::par(oldpar)) 


    ring<-6
    if(!(K %in% c(1,3,5,7))){warning("\nAvailable K values: 1,3,5,7\n") ; return()} #This has to be deleted 
    R_global<-""
    #R_global<<-""
    K_global<-K
    #K_global<<-K
    rows_global<-rows
    #rows_global<<-rows
    plpr_global<-plpr
    #plpr_global<<-plpr
    space_global<-distance
    #space_global<<-distance
    ring_global<-ring
    #ring_global<<-ring
    rep_unrep<-"unrep1"
    #rep_unrep<<-"unrep1"

    
    main_data<-data.frame( 
        "Entry"  =1:(rows*plpr),
        "Row"    =rep(1:rows,each=plpr),
        "Plant"  =rep(1:plpr,rows),
        
        "XPos"   =head(rep(c((1:plpr)*distance,((1:plpr)*(distance)+(distance/2))),rows),rows*plpr),
        "YPos"   =rep((distance*(3)**(1/2)/2)*(1:rows),each=plpr),
        "Data"    =rep(NA,rows*plpr)
    ) 
    
        
    #Set the control plants vector to be added

    vector_with_entries<-c(1,NA,NA,NA,NA,NA,NA)
    #Use K to set the first row
    vector_with_entries<-c(tail(vector_with_entries,K),head(vector_with_entries,-K))
    #print(vector_with_entries)
    #This is the first row of the plot
    data_frame_with_entries<-data.frame(vector_with_entries)
    current_vector<-vector_with_entries
    #start from the second row until the final row
    for(i in 2:rows){
        #set the current row each time
        if(i%%2==0){
            current_vector<-c(tail(current_vector,2),head(current_vector,-2))
            #print(current_vector)
            data_frame_with_entries<-cbind(data_frame_with_entries,current_vector)
            
            }
        if(i%%2==1){
            current_vector<-c(tail(current_vector,3),head(current_vector,-3))
            data_frame_with_entries<-cbind(data_frame_with_entries,current_vector)
            
            }
    }
    #Add plants at the end of the data frame 
    while(nrow(data_frame_with_entries)<plpr){
        data_frame_with_entries<-rbind(data_frame_with_entries,data_frame_with_entries)
    }
    #Select only the rows until the number of plants per row
    data_frame_with_entries<-head(data_frame_with_entries,plpr)
    #print(data_frame_with_entries)
    #convert the data frame to vector and store it in previously defined vector_with_entries vector
    vector_with_entries<-c()
    for (i in 1:ncol(data_frame_with_entries)){
        vector_with_entries<-append(vector_with_entries,data_frame_with_entries[,i])
    }
    

    #Set a counter to identify the rest of the entries  
    counter<-2
    for(i in 1:length(vector_with_entries)){
        if(is.na(vector_with_entries[i])){vector_with_entries[i]<-counter ; counter<-counter+1}
    }

    main_data$Entry<- vector_with_entries
   


    #Set  the name of Entry 1 as control1
    main_data$Entry[main_data$Entry==1]<-"Control1"
    Main_Data_Frame<-main_data
    #Main_Data_Frame<<-main_data
    #cat("Analysis table is saved in \"Main_Data_Frame\"\n\n")
    plot_convert(Main_Data_Frame,poly,rep_unrep=rep_unrep)
    return(Main_Data_Frame)

}


#' @title Non-replicated honeycomb selection design with three controls.
#' 
#' @description This function creates a data frame of an unreplicated honeycomb selection design with three controls.
#'
#' 
#' @param K The K parameter.
#' @param rows The number of rows. 
#' @param plpr The number of plants per row.
#' @param distance Distance between plants in meters.
#' 
#' @param poly If TRUE the polygon pattern is displayed.
#' @return A dataframe
#' @importFrom stats sd
#' @importFrom utils head
#' @importFrom utils tail
#' @references 
#' Fasoula V. (2013). Prognostic Breeding: A New Paradigm for Crop Improvement. Plant Breeding Reviews 37: 297-347. 10.1002/9781118497869.ch6. \doi{10.1002/9781118497869.ch6}
#'
#' Fasoula V.A., and Tokatlidis I.S. (2012). Development of crop cultivars by honeycomb breeding. Agronomy for Sustainable Development 32:161–180. 10.1007/s13593-011-0034-0 \doi{10.1007/s13593-011-0034-0}
#'
#' Fasoulas A.C., and Fasoula V.A. (1995). Honeycomb selection designs. In J. Janick (ed.). Plant Breeding Reviews 13: 87-139.  \doi{10.1002/9780470650059.ch3}
#' 
#' Tokatlidis I. (2016). Sampling the spatial heterogeneity of the honeycomb model in maize and wheat breeding trials: Analysis of secondary data compared to popular classical designs. Experimental Agriculture, 52(3), 371-390. \doi{10.1017/S0014479715000150}
#' 
#' Tokatlidis I., and Vlachostergios D. (2016). Sustainable Stewardship of the Landrace Diversity. Diversity 8(4):29. \doi{10.3390/d8040029}
#' 
#' 
#' @examples HDNR3(1,10,10,1)
#' @export



HDNR3<-function(K,rows,plpr,distance,poly=TRUE){
    #Added these two lines as suggested so the old adjustments will be restored 
    oldpar <- graphics::par(no.readonly = TRUE) 
    on.exit(graphics::par(oldpar))  

    ring<-6
    R_global<-""
    #R_global<<-""
    K_global<-K
    #K_global<<-K
    rows_global<-rows
    #rows_global<<-rows
    plpr_global<-plpr
    #plpr_global<<-plpr
    space_global<-distance
    #space_global<<-distance
    ring_global<-ring
    #ring_global<<-ring
    rep_unrep<-"unrep3"
    #rep_unrep<<-"unrep3"

    #if(!(K %in% c(1,3,5,7))){cat("\nAvailable K values: 1,3,5,7\n") ; return()}

    
    main_data<-data.frame( 
        "Entry"  =1:(rows*plpr),
        "Row"    =rep(1:rows,each=plpr),
        "Plant"  =rep(1:plpr,rows),
        
        "XPos"   =head(rep(c((1:plpr)*distance,((1:plpr)*(distance)+(distance/2))),rows),rows*plpr),
        "YPos"   =rep((distance*(3)**(1/2)/2)*(1:rows),each=plpr),
        "Data"    =rep(NA,rows*plpr)
    ) 
    
    
    
    #Set the first line of entries
    #cat("\nOnly one design at the moment\n")
    vector_with_entries<-c(3,rep(NA,6),1,rep(NA,6),2,rep(NA,6))
    current_vector<-vector_with_entries
    #Set the data frame with the entries
    data_frame_with_entries<-data.frame(current_vector)
    for(i in 2:rows){
        if(i%%2==0){
            
            current_vector<-c(tail(current_vector,-5),head(current_vector,5))
            #print(current_vector)
            data_frame_with_entries<-cbind(data_frame_with_entries,current_vector)
        }
        if(i%%2==1){
            current_vector<-c(tail(current_vector,-4),head(current_vector,4))
            #print(current_vector)
            data_frame_with_entries<-cbind(data_frame_with_entries,current_vector)
        }
    }
    #print(data_frame_with_entries)
    #Add rows if necessary
    while(nrow(data_frame_with_entries)<plpr){
        data_frame_with_entries<-rbind(data_frame_with_entries,data_frame_with_entries)
    }
    #Keep only the necessary rows of the data frame corresponding to the plant per row value 
    data_frame_with_entries<-head(data_frame_with_entries,plpr)
    #Change the format to a vector
    vector_with_entries<-c()
    for(i in  1:ncol(data_frame_with_entries)){
        vector_with_entries<-append(vector_with_entries,data_frame_with_entries[,i])
    }
    #print(vector_with_entries)
    #Set the rest of the entries
    counter<-4
    for(i in 1:length(vector_with_entries)){
        if(is.na(vector_with_entries[i])){
            vector_with_entries[i]<-counter
            counter<-counter+1
        }
    }
    main_data$Entry<-vector_with_entries
    #Set the names of Entries 1, 2,  3 as control1, Control2, Control3
    main_data$Entry[main_data$Entry==1]<-"Control1"
    main_data$Entry[main_data$Entry==2]<-"Control2"
    main_data$Entry[main_data$Entry==3]<-"Control3"
    
    Main_Data_Frame<-main_data
    #Main_Data_Frame<<-main_data
    #cat("Analysis table is saved in \"Main_Data_Frame\"\n\n")
    plot_convert(Main_Data_Frame,poly,rep_unrep=rep_unrep)
    return(Main_Data_Frame)
}







#' @title Analysis of the honeycomb selection design. 
#' 
#' @description This function analyzes the response variable of the data frame.
#' @param Response_Vector A vector containing the response variable data.
#' @param ring The number of plants per moving ring.
#' @param blocks The complete moving replicate.
#' @param row_element The position of the plant (number of row) in the center of a complete moving replicate/ring.
#' @param plant_element The position of the plant (number of plant) in the center of a complete moving replicate/ring.  
#' @param CRS The number of selected plants used for the CRS index.
#' @param Main_Data_Frame A data frame containing the design's information

#' @return A list.
#' @references 
#' Fasoula V. (2013). Prognostic Breeding: A New Paradigm for Crop Improvement. Plant Breeding Reviews 37: 297-347. 10.1002/9781118497869.ch6. \doi{10.1002/9781118497869.ch6}
#'
#' Fasoula V.A., and Tokatlidis I.S. (2012). Development of crop cultivars by honeycomb breeding. Agronomy for Sustainable Development 32:161–180. 10.1007/s13593-011-0034-0 \doi{10.1007/s13593-011-0034-0}
#'
#' Fasoulas A.C., and Fasoula V.A. (1995). Honeycomb selection designs. In J. Janick (ed.). Plant Breeding Reviews 13: 87-139.  \doi{10.1002/9780470650059.ch3}
#' 
#' Tokatlidis I. (2016). Sampling the spatial heterogeneity of the honeycomb model in maize and wheat breeding trials: Analysis of secondary data compared to popular classical designs. Experimental Agriculture, 52(3), 371-390. \doi{10.1017/S0014479715000150}
#' 
#' Tokatlidis I., and Vlachostergios D. (2016). Sustainable Stewardship of the Landrace Diversity. Diversity 8(4):29. \doi{10.3390/d8040029}
#' 
#' @examples main_data<-HDR(7,2,10,10,1)
#' main_data$Data<-wheat_data$total_yield
#' 
#' analysis(main_data,"Data",6)
#' @export
analysis<-function(Main_Data_Frame=NULL,Response_Vector=NULL,ring=6,blocks=FALSE,row_element=NULL,plant_element=NULL,CRS=5){
    #Added these two lines as suggested so the old adjustments will be restored 
    oldpar <- graphics::par(no.readonly = TRUE) 
    on.exit(graphics::par(oldpar))  


    #Check for NA (because there is an issue when inserted from excell)
    for(i in 1:nrow(Main_Data_Frame)){
        if(is.na(Main_Data_Frame$Data[i])){
            Main_Data_Frame$Data[i]<-NaN
        }
    }
    
    
    #Convert Main_Data_Frame to data frame (in case data is inserted from readxl..)
    Main_Data_Frame<-as.data.frame(Main_Data_Frame)
    #Get the name of the column of the table and use it as obsservation vector.
    Observation_vec_for_blocks<-Response_Vector #Use thi s in case of block analysis because we change the value of Response_Vector
    Response_Vector<-Main_Data_Frame[,Response_Vector]

    #Check if there is a column wich contains characters ()
    for(i in 2:ncol(Main_Data_Frame)){
        if(is.character(Main_Data_Frame[,i])    ){  
            warning("One of the vectors contains characters instead of numeric values. Possibly there was an error because of comma instead of period.")
            return()
        }

    }
    #Check for the first row to be entry in case it is inserted from excel
    if(colnames(Main_Data_Frame[1])!="Entry") {Main_Data_Frame<-Main_Data_Frame[,2:ncol(Main_Data_Frame)]}
    #Renamee it as data frame just in case
    Main_Data_Frame<-as.data.frame(Main_Data_Frame)

 


    #Now the rep_unrep is needed (check if the values are unique)
    test<-1:nrow(Main_Data_Frame)==Main_Data_Frame$Entry
    test<-factor(test)
     
    test<-levels(test)
     
    if(("TRUE" %in%   test)&& ("FALSE" %in%   test) ){test<-"FALSE"} 
    if( test=="TRUE" | "Control1" %in% Main_Data_Frame$Entry){
        rep_unrep="unrep"} else {rep_unrep="rep"}

    
    #Do the block analysis and return
    if(blocks==TRUE){ 
        if(!rep_unrep=="rep"){warning("Can only do the analysis for replicated designs."); return()}  #This has to be modified
        Main_Data_Frame<-analize_blocks(Main_Data_Frame=Main_Data_Frame,observation=Observation_vec_for_blocks,row_element=row_element,plant_element=plant_element,CRS=CRS,rep_unrep=rep_unrep) 
        #Here main data frame contains two data frames
        #Show aggain converted values ie convert NaN to NA
        for(i in 1:nrow(Main_Data_Frame[[1]])){
            if(is.na(Main_Data_Frame[[1]]$Data[i])){
                Main_Data_Frame[[1]]$Data[i]<-NA
            }
        }

        #Replace CRS of the control with NA
        for (i in 1:nrow(Main_Data_Frame[[2]])){
            if(Main_Data_Frame[[2]]$Entry[i]==Main_Data_Frame[[2]]$Entry[nrow(Main_Data_Frame[[2]])]){
                Main_Data_Frame[[2]]$CRS[i]<-NA
            }
        }
        return(Main_Data_Frame)
    }
 
    #Delete all the other rows in Main_Data_Frame and clear the whole summary_table_global 
    Main_Data_Frame<-Main_Data_Frame[,1:6]
    #Main_Data_Frame<<-Main_Data_Frame[,1:6]
    summary_table_global<-c()
    #summary_table_global<<-c()

    ring_global<-ring
    #ring_global<<-ring

    #Rebuild plot if row_element and plant_element not null rep_unrep is defined above

    if(!is.null(row_element) && !is.null(plant_element)){plot_convert(Main_Data_Frame,rep_unrep=rep_unrep)}
    



    #Generate radius table and pair it with the number of plants corresponding to it
    total<-c()
    for(x in 0:10){
        for(y in 0:10){
            total<-append(total,(x**2+y**2+x*y)**(1/2))
        }
    }
    total<-factor(total)
    total<-levels(total)
    total<-head(total , 37)#includes 0 as first element 
    total<-as.numeric(total)
    radius_table<-tail(total,36)#remove the first 0
    
    ring_plants<-c(6,12,18,30,36,42,54,60,72,84,90,96,108,120,126,138,150,162,168,186,198,210,222,234,240,252,264,270,282,294,300,312,336,348,360,366)
    

    #Test if the ring is the correct this has to be deleted 
    if(!(ring %in% ring_plants)) {
        warning("\n\nChoose one of those ring sizes:\n
        6,12,18,30,36,42,54,60,72,84,90,96,108,120,126,138,150,162,168,
        186,198,210,222,234,240,252,264,270,282,294,300,312,336,348,360,366
        ")
        #print(ring_plants)
        return()
    }

    ring_info<-data.frame("Plants"=ring_plants,"Radius"=radius_table)
    
    #print(radius_table)
    
    #If there is a vector provided, add it to the Observation column of the global data frame 
    if(!is.null(Response_Vector)){
        Main_Data_Frame$Data<-Response_Vector
        #Main_Data_Frame$Data<<-Response_Vector
    }

    main_data<-Main_Data_Frame



    #store the distance between plants
    d<-main_data$XPos[1]
    #if(is.na(main_data$Data[1])){cat("\nAdd data in the observation column to run the analysis\n");return()}
    

    #Get the ring average for each entry
    #add one more column in the global data frame
    Main_Data_Frame$NumR<-cbind(rep(NA,nrow(Main_Data_Frame)))
    Main_Data_Frame$MeanR<-cbind(rep(NA,nrow(Main_Data_Frame)))

    ring<-ring_global
    #print(ring)
    range<-as.numeric(ring_info[ring_info$Plants==ring,"Radius"])*d
    #print(range)

    #create moving ring vectorMeanRing
    for(i in 1:nrow(main_data)){
        #set a vector to store the ring of Data for each 
        ring_coun_vec<-c()
        for(j in 1:nrow(main_data)){
            #print(c(main_data[i,"XPos"],main_data[j,"XPos"],main_data[i,"YPos"],main_data[j,"YPos"]))

            range_check<-((main_data[i,"XPos"]-main_data[j,"XPos"])**2+(main_data[i,"YPos"]-main_data[j,"YPos"])**2)**(1/2)
             
            


            if( (range_check<=range+0.00000000001 && range_check!=0)){#add some small number to range  

                #Add this to draw points of ring
                if(!is.null(row_element)  &&!is.null(plant_element) ){
                    if(Main_Data_Frame$Row[i]==row_element && Main_Data_Frame$Plant[i]==plant_element){
                            if(!is.nan(Main_Data_Frame$Data[j])){
                                graphics::points(Main_Data_Frame$XPos[j],Main_Data_Frame$YPos[j],
                                cex= 3
                                )
                                if(!is.nan(Main_Data_Frame$Data[i])){
                                    graphics::points(Main_Data_Frame$XPos[i],Main_Data_Frame$YPos[i],
                                    cex= 3,
                                    col="red")
                                }else{
                                    graphics::points(Main_Data_Frame$XPos[i],Main_Data_Frame$YPos[i],pch=4,cex=4,col="red")
                                }
                            }else{
                                graphics::points(Main_Data_Frame$XPos[j],Main_Data_Frame$YPos[j],
                                cex=3,
                                pch=4)                            
                            }
                    } 
                }
                
                if(!is.nan(main_data[j,"Data"]) ){
                    ring_coun_vec<-append(ring_coun_vec,main_data[j,"Data"])



                }
            }
             
        }
        #add ring values to global table
        Main_Data_Frame$NumR[i]<-length(ring_coun_vec)
        #Main_Data_Frame$NumR[i]<<-length(ring_coun_vec)

        Main_Data_Frame$MeanR[i]<- mean(ring_coun_vec)
        #Main_Data_Frame$MeanR[i]<<- mean(ring_coun_vec)
        #print(length(ring_coun_vec))


       


    }


    #Calculate the plant index (it is both for replicated and unreplicated)

    #Add another column to dataframe (localy and globally)
    Main_Data_Frame$PYI<-rep(NA,nrow(Main_Data_Frame))
    #Main_Data_Frame$PYI<<-rep(NA,nrow(Main_Data_Frame))
    


    for(i in 1:nrow(Main_Data_Frame)){
         if(!is.nan(Main_Data_Frame$Data[i])){#check if the plant exists
            Main_Data_Frame$PYI[i]<- (Main_Data_Frame$Data[i]/Main_Data_Frame$MeanR[i])**2
            #Main_Data_Frame$PYI[i]<<-(Main_Data_Frame$Data[i]/Main_Data_Frame$MeanR[i])**2
        }
    }




    #Calculate the mean of the whole plot exclude na
    MeanTrial<-mean(Main_Data_Frame$Data, na.rm=TRUE)






    #Add these if  the design is replicated 
    if(rep_unrep=="rep"){
        Main_Data_Frame$Mean<-rep(NA,nrow(Main_Data_Frame))

        Main_Data_Frame$N<-rep(NA,nrow(Main_Data_Frame))

        Main_Data_Frame$sd<-rep(NA,nrow(Main_Data_Frame))

        #Main_Data_Frame$LPl<-rep(NA,nrow(Main_Data_Frame))

        for(i in 1:nrow(Main_Data_Frame)){
            obs_vec<-c()
            for(j in 1:nrow(Main_Data_Frame)){
                if(Main_Data_Frame$Entry[i]==Main_Data_Frame$Entry[j] && !is.nan(Main_Data_Frame$Data[j])){
                    obs_vec<-append(obs_vec,Main_Data_Frame$Data[j])
                }
            }
            
            #add values to data table global
            Main_Data_Frame$Mean[i]<-mean(obs_vec)
            #Main_Data_Frame$Mean[i]<<-mean(obs_vec)
            Main_Data_Frame$N[i]<-length(obs_vec)
            #Main_Data_Frame$N[i]<<-length(obs_vec)
            Main_Data_Frame$sd[i]<-sd(obs_vec)   
            #Main_Data_Frame$sd[i]<<-sd(obs_vec)   
        }
        #Add stability index in Main_Data_Frame
        Main_Data_Frame$HI<-(Main_Data_Frame$Mean/Main_Data_Frame$sd)**2
        #Main_Data_Frame$HI<<-(Main_Data_Frame$Mean/Main_Data_Frame$sd)**2

        #Add line index 
        Main_Data_Frame$GYI<-(Main_Data_Frame$Mean/MeanTrial)**2
        #Main_Data_Frame$GYI<<-(Main_Data_Frame$Mean/MeanTrial)**2

        #Add Plant prognostic equation
        Main_Data_Frame$PPE<-Main_Data_Frame$PYI*Main_Data_Frame$HI
        #Main_Data_Frame$PPE<<-Main_Data_Frame$PYI*Main_Data_Frame$HI

        #Add line prognostic equation
        Main_Data_Frame$GPE<-Main_Data_Frame$HI*Main_Data_Frame$GYI
        #Main_Data_Frame$GPE<<-Main_Data_Frame$HI*Main_Data_Frame$GYI


        #Add meanPYI and meanPPE
         
        Main_Data_Frame$mPYI<-rep(NA,nrow(Main_Data_Frame))
        #Main_Data_Frame$mPYI<<-rep(NA,nrow(Main_Data_Frame))

        Main_Data_Frame$mPPE<-rep(NA,nrow(Main_Data_Frame))
        #Main_Data_Frame$mPPE<<-rep(NA,nrow(Main_Data_Frame))


        entry_elements<-as.numeric(levels(as.factor(Main_Data_Frame$Entry)))
        for(j in entry_elements){
            PYI_elements<-c()
            PPE_elements<-c()
            for(i in 1:nrow(Main_Data_Frame)){
                    if(Main_Data_Frame$Entry[i]==entry_elements[j]){
                        PYI_elements<-append(PYI_elements,Main_Data_Frame$PYI[i])
                        PPE_elements<-append(PPE_elements,Main_Data_Frame$PPE[i])
                    }
            }        
          

            #fill the table with the current values
            for(k in 1:nrow(Main_Data_Frame)){
                
                if(Main_Data_Frame$Entry[k]==entry_elements[j]){
                        Main_Data_Frame$mPYI[k]<-mean(PYI_elements,na.rm = TRUE)
                        #Main_Data_Frame$mPYI[k]<<-mean(PYI_elements,na.rm = TRUE)
                        Main_Data_Frame$mPPE[k]<-mean(PPE_elements,na.rm = TRUE)
                        #Main_Data_Frame$mPPE[k]<<-mean(PPE_elements,na.rm = TRUE)

                }
            }        

        }



    }

    if(rep_unrep=="rep"){
    #New index Coefficient of  Response to Selection (CRS).
    #Create a data frame with the max values 
    Entry_for_CRS<-factor(Main_Data_Frame$Entry)
    Entry_for_CRS<-levels(Entry_for_CRS)

     
    table_for_CRS <-data.frame("Entry"=Entry_for_CRS)
    
    for(i in Entry_for_CRS){
        #Convert Entry to numeric
        i<-as.numeric(i)
        obs_for_CRS<-c()
        #Select only rows with the same Entry
        obs_for_CRS<-Main_Data_Frame[Main_Data_Frame$Entry==i,"Data"]
        #Chose the  5 greatest plants and estimate mean
         
        obs_for_CRS<-sort(obs_for_CRS,na.last=FALSE)
         
        selection<-tail(obs_for_CRS,CRS )
         
        table_for_CRS$Mean_of_Max[i]<-mean(selection,na.rm=TRUE)
        

    }
    #print(table_for_CRS)
    #Insert crs in analysis table global.
    Main_Data_Frame$CRS <-rep(NA,nrow(Main_Data_Frame))
    #Main_Data_Frame$CRS<<-rep(NA,nrow(Main_Data_Frame))
    for (i in 1:nrow(Main_Data_Frame)){
        Main_Data_Frame$CRS[i] <- (table_for_CRS[table_for_CRS$Entry==Main_Data_Frame$Entry[i],"Mean_of_Max"]-Main_Data_Frame$Mean[1])/Main_Data_Frame$sd[i]
        #Main_Data_Frame$CRS[i]<<- (table_for_CRS[table_for_CRS$Entry==Main_Data_Frame$Entry[i],"Mean_of_Max"]-Main_Data_Frame$Mean[1])/Main_Data_Frame$sd[i]
    
    }
}



    #cat("Variables \n\"Main_Data_Frame\" and \"summary_table_global\" have been modified.\n\n")
    #Create a summary table if the design is replicated
    summary_table_global<-data.frame()
    #summary_table_global<<-data.frame()
    if(rep_unrep=="rep"){
        for(i in 1:max(unlist(Main_Data_Frame$Entry))){
            for(j in 1:nrow(Main_Data_Frame)){
                if(i==Main_Data_Frame$Entry[j]){
                    summary_table_global<-rbind(summary_table_global,
                        Main_Data_Frame[j , c("Entry","N","Mean","sd","HI","GYI","GPE","mPYI","mPPE","CRS")]
                    ) 
                    #summary_table_global<<-rbind(summary_table_global,
                    #    Main_Data_Frame[j , c("Entry","N","Mean","sd","HI","GYI","GPE","mPYI","mPPE","CRS")]
                    #) 

                    break
                }
            }
        }
    #Last addition CV
    #print(summary_table_global)
    summary_table_global<-cbind(summary_table_global, ((summary_table_global[,"sd"])/(summary_table_global[,"Mean"])*100) )
    #summary_table_global<<-cbind(summary_table_global, ((summary_table_global[,"sd"])/(summary_table_global[,"Mean"])*100) )
    colnames(summary_table_global)<-c("Entry","N","Mean","sd","HI","GYI","GPE","mPYI","mPPE","CRS","CV")
    #print(summary_table_global)
    #colnames(summary_table_global)<<-c("Entry","N","Mean","sd","HI","GYI","GPE","mPYI","mPPE","CRS","CV")

    summary_table_global<-summary_table_global[,c("Entry","N","Mean","CV","sd","HI","GYI","GPE","mPYI","mPPE","CRS")]
    #summary_table_global<<-summary_table_global[,c("Entry","N","Mean","CV","sd","HI","GYI","GPE","mPYI","mPPE","CRS")]
    
    }
    



    

    
    #Print the results
    #print(Main_Data_Frame)
    #cat("\n\n\n")
    if(nrow(summary_table_global)!=0)  {
        #Write this here because the data frame appears wrong
        rownames(summary_table_global)<-1:nrow(summary_table_global)
        #print(summary_table_global)
         
        
        #summary_table_global<<-summary_table_global

    }


    #Show aggain converted values ie convert NaN to NA
    for(i in 1:nrow(Main_Data_Frame)){
        if(is.na(Main_Data_Frame$Data[i])){
            Main_Data_Frame$Data[i]<-NA
        }
    }
     
    #Replace the CRS of the control entry with NA. This only if the experiment is unreplicated
    if(rep_unrep=="rep"){
        for (i in 1:nrow(summary_table_global)){
            if(summary_table_global$Entry[i]==summary_table_global$Entry[nrow(summary_table_global)]){
                summary_table_global$CRS[i]<-NA
            }
        }
    }
    return_value<-list(Main_Data_Frame[,!names(Main_Data_Frame) %in% c("mPPE","mPYI","GPE","GYI","CRS")],summary_table_global)
    

    return(return_value)
}







