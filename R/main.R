#' @title Tests if a selection design exists and returns its X and Y values.
#' 
#' @description It is used to return the X and Y values of a replicated selection design if it exists. 
#'
#' @param R_gen A single number or vector containing the replicated selection designs for testing.
#' @return A dataframe.
#' @keywords internal
#'  

#The function starts from X and for this specific X whenever it meets an X estimates the rest variables
#Because it gets only one R value there may be exist more than one X and Y combinations that lead to this R
#
test_for_R<-function(R_gen){

    #options("width"=201)#set the terminal size

    if(R_gen<3){R_gen<-3}
    #create vectors for each variable since there maybe exist more than one of these for an R design
 
    R_final<-c()
    X_final<-c()
    Y_final<-c()
    grouped_ungroup_final<-c()
    max_common_divisor_final<-c()
    K1_final<-c()
    K2_final<-c()
    K3_final<-c()
    K4_final<-c()
    K5_final<-c()
    K6_final<-c()     
    
    Rs_final<-c()
    Xs_final<-c()
    Ys_final<-c()
    #test for null provided by the generate function 
    #if(is.null(R_gen)){R_gen<-t0.int(readline("Give an R nmber: "))}
    #by using floor and ceiling we get only integers
    X_min<-floor  ( (R_gen/3)**(1/2) )
    X_max<-ceiling( (R_gen  )**(1/2) )
    

    
    for(X in X_min:X_max){

        Y_estimated<-(((4*R_gen-3*X**2))**(1/2)-X)   /  2               # Y should be equal to this by solving the eq for R X and X
        #print(Y_estimated)
        if(Y_estimated%%1==0    &&  Y_estimated>=0  &&  Y_estimated<=X  &&  !is.nan(Y_estimated)){ # Y should be less than X and Natural number  
           Y<-Y_estimated #if Al the above are true then the R design exists and so does Y
           R<-R_gen 
           Y_final<-append(Y_final,Y)
           X_final<-append(X_final,X)
           R_final<-append(R_final,R)
           #cat("\n\nx=",X,"y=",Y,"R=",R,"\n")


            if(min(X,Y)==0){max_common_divisor<-max(X,Y)} 
            else{ 

                for(divisor in 1:min(c(X,Y))){#Now check here whether the design is replicated or unreplicated and calculate K values
                    if((X%%divisor==0) && (Y%%divisor==0)){
                        #print(divisor)
                        max_common_divisor<-divisor
                        #cat()
                    }

                }
            }

            if(max_common_divisor>1){
                grouped_ungroup<-"Grouped"

            }else{
                grouped_ungroup<-"Ungrouped"
            }



            #cat("max common div=",max_common_divisor,grouped_ungroup,"\n")
            max_common_divisor_final<-append(max_common_divisor_final,max_common_divisor)

            grouped_ungroup_final<-append(grouped_ungroup_final,grouped_ungroup)
             

            K_plus_minus_un<-c()
            #print("set")
            Xs_temp<-c()
            Ys_temp<-c()
            Rs_temp<-c()
            if(X%%2==1){max_X1<-floor((X-1)/2)}else{max_X1<-floor(X/2)}
               
            for(X1_temp in 1:max_X1){
                #print(K_plus_minus_un)
                #print(c(X,Y))
                Y1_plus_check<-(X1_temp*Y+1)/X
                #cat("X1_temp ",X1_temp,"\n")
                #print(Y1_plus_check)
                if(Y1_plus_check%%1==0){X1_plus<-X1_temp    ;   Y1_plus<-Y1_plus_check}
                
                Y1_minus_check<-(X1_temp*Y-1)/X
                #print(Y1_minus_check)
                if(Y1_minus_check%%1==0){X1_minus<-X1_temp    ;   Y1_minus<-Y1_minus_check}
                if(exists("X1_plus")){
                    Y2_plus<-Y-Y1_plus
                    X2_plus<-X-X1_plus
                    R2_plus<-Y2_plus**2+Y2_plus*X2_plus+X2_plus**2
                    R1_plus<-Y1_plus**2+Y1_plus*X1_plus+X1_plus**2
                    #cat("condition -->",Y2_plus*X1_plus-Y1_plus*X2_plus,"\n")
                    #print(R)
                    #print(c(R1_plus,R2_plus))
                    if((Y2_plus*X1_plus-Y1_plus*X2_plus) %in% c(1,-1)){
                        K1_plus_check<-((4*R*R1_plus-3)**(1/2)-1)/2
                        K2_plus_check<-((4*R*R2_plus-3)**(1/2)-1)/2


                          
                            if(K1_plus_check%%1==0 &&  K2_plus_check%%1==0  && K1_plus_check+K2_plus_check==R_gen-1){
                                K1_plus<-K1_plus_check
                                #cat("K1_plus=",K1_plus,"\n")
                                if(!(K1_plus %in% K_plus_minus_un)){K_plus_minus_un<-append(K_plus_minus_un,K1_plus)}
                           #}
                           #if(K2_plus_check%%1==0 ){
                                K2_plus<-K2_plus_check
                                #cat("K2_plus=",K2_plus,"\n")
                                if(!(K2_plus %in% K_plus_minus_un)){K_plus_minus_un<-append(K_plus_minus_un,K2_plus)}
                            }
                           



                    }    
                    #print("Plus")
                    
                    #print(c(K1_plus,K2_plus))
                    #if(K1_plus+K2_plus==R_gen-1){print("ok")}
                     
                }
                if(exists("X1_minus")){
                    
                    Y2_minus<-Y-Y1_minus
                    X2_minus<-X-X1_minus
                    R2_minus<-Y2_minus**2+Y2_minus*X2_minus+X2_minus**2
                    R1_minus<-Y1_minus**2+Y1_minus*X1_minus+X1_minus**2
                    #cat("condition -->",Y2_minus*X1_minus-Y1_minus*X2_minus,"\n")
                    #print(R)
                    #print(c(R1_minus,R2_minus))
                    if((Y2_minus*X1_minus-Y1_minus*X2_minus) %in% c(1,-1) ){
                        K1_minus_check<-((4*R*R2_minus-3)**(1/2)-1)/2
                        K2_minus_check<-((4*R*R1_minus-3)**(1/2)-1)/2


                         
                            if(K1_minus_check%%1==0  && K2_minus_check%%1==0 && K1_minus_check+K2_minus_check==R_gen-1){
                                K1_minus<- K1_minus_check
                                #cat("K1_minus=", K1_minus, "\n")
                                if(!(K1_minus %in% K_plus_minus_un)){K_plus_minus_un<-append(K_plus_minus_un,K1_minus)}
                            #}
                            #if(K2_minus_check%%1==0){
                                K2_minus<- K2_minus_check
                                #cat("K2_minus=", K2_minus, "\n")
                                if(!(K2_minus %in% K_plus_minus_un)){K_plus_minus_un<-append(K_plus_minus_un,K2_minus)}
                            }
                           

                        #print(K_plus_minus_un)    
                    } 
                    #print("minus")   
                    
                    #print(c(K1_minus,K2_minus))
                    #if(K1_minus+K2_minus==R_gen-1){print("ok")}
                
                }
                

             
            }
                
                #######cat("K_plus_minus_un",K_plus_minus_un,"\n")
                if(length(K_plus_minus_un)>0){
                    #print(K_plus_minus_un)
                    K1_final<-append(K1_final,K_plus_minus_un[1])
                    K2_final<-append(K2_final,K_plus_minus_un[2])
                    K3_final<-append(K3_final,NA)
                    K4_final<-append(K4_final,NA)
                    K5_final<-append(K5_final,NA)
                    K6_final<-append(K6_final,NA)
                } else{
                    K1_final<-append(K1_final,NA)
                    K2_final<-append(K2_final,NA)
                    K3_final<-append(K3_final,NA)
                    K4_final<-append(K4_final,NA)
                    K5_final<-append(K5_final,NA)
                    K6_final<-append(K6_final,NA)
                }
                #assign here the Rs, Xs, Ys
                if(grouped_ungroup=="Grouped"){
                    Xs_temp<-append(Xs_temp,X/max_common_divisor)
                    Ys_temp<-append(Ys_temp,Y/max_common_divisor)
                    Rs_temp<-append(Rs_temp,Xs_temp**2+Ys_temp*Xs_temp+Ys_temp**2)
####################cat("\nXs_temp",Xs_temp,"Ys_temp",Ys_temp,"Rs_temp",Rs_temp,"\n")
                    Rs_final<-append(Rs_final,Rs_temp)
                    Xs_final<-append(Xs_final,Xs_temp)
                    Ys_final<-append(Ys_final,Ys_temp)
                }
                else if (grouped_ungroup=="Ungrouped"){
                    Xs_temp<-append(Xs_temp,NA)
                    Ys_temp<-append(Ys_temp,NA)
                    Rs_temp<-append(Rs_temp,NA) 
####################cat("Xs_temp",Xs_temp,"Ys_temp",Ys_temp,"Rs_temp",Rs_temp,"\n")  
                    Rs_final<-append(Rs_final,Rs_temp)
                    Xs_final<-append(Xs_final,Xs_temp)
                    Ys_final<-append(Ys_final,Ys_temp)

                    

                }

        }
    }
    #print(Rs_final)
    #print(Xs_final)
    #print(Ys_final)
    


    #if there is not an R design return nothing (NULL)
    if(length(R_final)==0){return()}  
    
    for(i in 1:length(K2_final)){
        if( is.na(K2_final[i])  ){
            K2_final[i]<-K1_final[i]
        }
    }
    #check for the Grouped ones and add more K values
    return_value<-data.frame("R"=R_final,"X"=X_final,"Y"=Y_final,"Type"=grouped_ungroup_final,"Groups"=max_common_divisor_final,"GroupSize"=R_final/max_common_divisor_final,"K1"=K1_final,"K2"=K2_final,"Rs"=Rs_final,"Xs"=Xs_final,"Ys"=Ys_final)
    
    
    return(return_value)
}

#' @title This function returns only the grouped replicated selection designs.
#' 
#' @description It calls the check for R function and keeps only the grouped selection designs.
#' @param R_gen A single number or vector containing the replicated selection designs for testing.
#' @keywords internal
#' @return A dataframe.
#'  
return_grouped<-function(R_gen){

    a<-test_for_R(R_gen)
    #check if the data frame contains zero elements
    if(is.na(a[a$Type=="Grouped",][1,1]) || is.null(a[a$Type=="Grouped",][1,1])){return()}
    main_data<-as.data.frame(a[a$Type=="Grouped",])
    
    main_data$K3<-rep(NA,nrow(main_data))
    main_data$K4<-rep(NA,nrow(main_data))
    main_data$K5<-rep(NA,nrow(main_data))
    main_data$K6<-rep(NA,nrow(main_data))




    #test if the row contains Y=0 then X-1 = K3 ,  X = K4 , R-1-K4 = K5 , R-1 - K3 = K6
    if(0 %in% main_data$Y){
       main_data[main_data$Y==0,"K3"]=main_data[main_data$Y==0,"X"]-1
       main_data[main_data$Y==0,"K4"]=main_data[main_data$Y==0,"X"]
       main_data[main_data$Y==0,"K5"]=main_data[main_data$Y==0,"R"]-1-main_data[main_data$Y==0,"X"]
       main_data[main_data$Y==0,"K6"]=main_data[main_data$Y==0,"R"]-1-main_data[main_data$Y==0,"X"]+1
    }
    #Get to the rows that dont contain Y = 0
    for(i in 1:nrow(main_data)){
        if(main_data[i,"Y"]!=0){
            data_to_get_k1k2<-as.data.frame(return_ungrouped(main_data[i,"Rs"]))
            #get only the rows which have the specific Xs and Ys
            #print("Getting values")
            
            K1_calculations<-data_to_get_k1k2[1,"K1"]
            K2_calculations<-data_to_get_k1k2[1,"K2"]
 
            #print(K1_calculations)
            #print(K2_calculations)
            main_data$K3<-(main_data[i,"Rs"]*main_data[i,"R"])**(1/2)-K1_calculations-1
            main_data$K4<-(main_data[i,"Rs"]*main_data[i,"R"])**(1/2)+K1_calculations 
            main_data$K5<-(main_data[i,"Rs"]*main_data[i,"R"])**(1/2)-K2_calculations-1
            main_data$K6<-(main_data[i,"Rs"]*main_data[i,"R"])**(1/2)+K2_calculations

            
        }
    }
    #Test if X=Y. Then given K3,K4, K5=R-1-K3 K6=R-1-K4. Designs with the same X and Y are appear to be grouped
    for(i in 1:nrow(main_data)) {
        if(main_data[i,"X"]==main_data[i,"Y"]){
            main_data[i,"K5"]=R_gen-1-main_data[i,"K4"]
            main_data[i,"K6"]=R_gen-1-main_data[i,"K3"]
            
        }
    }
   

    return_value<-main_data
    return( return_value)

}          


#' @title This function returns only the ungrouped replicated selection designs.
#' @param R_gen A single number or vector containing the replicated selection designs for testing.
#' @description It calls the check for R function and keeps only the Ungrouped selection designs.
#'
#' @return A dataframe.
#' @keywords internal
#'  

#R27 only has issues
return_ungrouped<-function(R_gen){

    a<-test_for_R(R_gen)
    #check if the data frame contains zero elements
    if(is.na(a[a$Type=="Ungrouped",][1,1]) || is.null(a[a$Type=="Ungrouped",][1,1])){return()}
    main_data<-as.data.frame(a[a$Type=="Ungrouped",])
    main_data$K3<-rep(NA,nrow(main_data))
    main_data$K4<-rep(NA,nrow(main_data))
    main_data$K5<-rep(NA,nrow(main_data))
    main_data$K6<-rep(NA,nrow(main_data))
                                                                                                                                                                                                
    return_value<-main_data

    return(return_value)

}   


#' @title Available honeycomb selection designs.
#' @param E_gen A single number or a vector of entries.
#' @description This function is used to generate the available  honeycomb selection designs including k parameters.
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
#' @examples generate(1:50)
#' @export
generate<-function(E_gen=NULL){
    R_gen<-E_gen
    if(is.null(R_gen)){R_gen<-as.integer(readline("Give an R value: "))}
   
    main_data<-data.frame("R"=c(),"X"=c(),"Y"=c(),"Type"=c(),"Groups"=c(),
    "K1"=c(),"K2"=c(),"Rs"=c(),"Xs"=c(),"Ys"=c(),"K3"=c(),"K4"=c(),"K5"=c(),"K6"=c())
    
    #If a number below 3 is given set the lower R το 3 
    if(min(R_gen)<3){R_gen<-3:max(R_gen)}


    
    
    #append designs to a dataframe
    for(i in R_gen){
        if(!is.null(return_ungrouped(i)) ){main_data<-rbind(main_data,as.data.frame(return_ungrouped(i)))}
        if(!is.null(return_grouped(i)) ){
            
            to_append_on_main_data<-as.data.frame(return_grouped(i))
            #print(to_append_on_main_data$K5)
            #print(to_append_on_main_data$K3)
            #print(to_append_on_main_data$K6)
            #print(to_append_on_main_data$K4)


            #Clear the double K values (error the condition has length>1 so iterate through elements)
            for(i in 1:length(to_append_on_main_data$K5)){
                if(to_append_on_main_data$K5[i]==to_append_on_main_data$K3[i]){to_append_on_main_data$K5[i]=NA}
            }
            for(i in 1:length(to_append_on_main_data$K6)){
                if(to_append_on_main_data$K6[i]==to_append_on_main_data$K4[i]){to_append_on_main_data$K6[i]=NA}
            }

            main_data<-rbind(main_data,to_append_on_main_data)

            }

    }

    return_value<-main_data
    #Set the order of rows in data frame
    rownames(return_value) = seq(length=nrow(return_value))
    #Test if the number provided gives any results
    if(length(return_value)==0){
        warning("\nNot an available R design. Provide a vector to generate function to see which designs are available. E.g. generate(1:40) ")
        #return_value<-generate((R_gen-10):(R_gen+10))
        ##print(return_value)
        #return(return_value)
        return()
        
    }
    #writexl::write_xlsx(return_value,"honeycomb_designs.xlsx")
    #shell.exec("honeycomb_designs.xlsx")
    #Store the data frame in a global variable
    #cat("\nDesigns info are saved in \"Generated_designs_global\" data frame.\n\n")
    Generated_designs_global<-return_value
    #Generated_designs_global<<-return_value
    #drop the Rs,Xs, Xs columns
    return_value<-return_value[!(names(return_value) %in% c("Rs","Xs","Ys"))]
    #calculate the greatest common divisor of plants per row and minimum K value. It is needed to estimate rows per section
     
    gcd_vector<-c()
    for(i in 1:nrow(return_value) ){
        minK<-return_value[i, c("K1","K2","K3","K4","K5","K6")  ]
        minK<-min(minK,na.rm=TRUE)
         
        two_K_1<-2*minK+1
        for(j in 1:(min(c(return_value[i,"GroupSize"],two_K_1)))){
            if( (return_value[i,"GroupSize"]%%j==0)  & (two_K_1%%j==0)   ){
                 gcd_for_rps<-j
                 #print(two_K_1)
                 #print(gcd_for_rps)

            }
        }

        gcd_vector<-append(gcd_vector,gcd_for_rps)
    }

    #Move Groups and plants per group at the end 
    return_value<-data.frame(
        "R"=return_value$R,"X"=return_value$X,"Y"=return_value$Y,"K1"=return_value$K1,"K2"=return_value$K2,"K3"=return_value$K3,
        "K4"=return_value$K4,"K5"=return_value$K5,"K6"=return_value$K6,"Type"=return_value$Type,
        "Groups"=return_value$Groups,"GroupSize"=return_value$GroupSize , "SetRows"=2*return_value$GroupSize/gcd_vector
    )
    #Replace NA with "-"
    return_value[is.na(return_value)] <- "-"
    #Add plants per group and row in one section columns 

    #Fix the R27 issue
    return_value[return_value$R==27,"K5"]<-16
    return_value[return_value$R==27,"K6"]<-19
    

    #Clear the double or triple R3 designs
    if(nrow(Generated_designs_global)<=3){
        if(Generated_designs_global[nrow(Generated_designs_global),"R"]==3){
            Generated_designs_global<-head(Generated_designs_global,1)
            #Generated_designs_global<<-head(Generated_designs_global,1)
        }
    }

    Generated_designs_global<-return_value
    #Generated_designs_global<<-return_value




    return(Generated_designs_global)#return_value)

}



