#' @title Analysis of honeycomb selection design based on blocks of unique nearby entries.
#' 
#' @description  A Function to analyze blocks of entries. The "central" plant in each position is not calculated.
#' @param observation A vector containing the observations.
#' @param row_element The row of the element which the block it belongs to will be displayed.
#' @param plant_element The position of the element in the row which the block it belongs to will be displayed.
#' @param CRS Number of top plants used for the CRS index.
#' @param rep_unrep Replicated of unreplicated design.
#' @return A dataframe.
#' @keywords internal

analize_blocks<-function(Main_Data_Frame=NULL,observation=NULL,row_element=NULL,plant_element=NULL,CRS,rep_unrep){
 
 

#The global variable is deleted so  
observation<-Main_Data_Frame[,observation]

#Insert values in the observation column
Main_Data_Frame$Data <-observation
#Main_Data_Frame$Data<<-observation


#Delete all the other rows in Main_Data_Frame and clear the whole summary_table_global 
Main_Data_Frame<-Main_Data_Frame[,1:6]
#Main_Data_Frame<<-Main_Data_Frame[,1:6]
summary_table_global<-c()
#summary_table_global<<-c()



#
#Rebuild plot if row_element and plant_element is provided 
if(!is.null(row_element) && !is.null(plant_element)){plot_convert(Main_Data_Frame,rep_unrep=rep_unrep)}



#Return if the obs is na in the first element
if(is.na(Main_Data_Frame$Data[1])){return()}
#Return if the design is unreplicated
if(rep_unrep=="unrep"){return()}

#Find the value of R here (because we no longer have R global)
R_global<-max(Main_Data_Frame$Entry)
control_radious<-c()
for(x in 0:(2*R_global)){
    for(y in 0:(2*R_global)){
       
        number<-x**2+y**2+x*y
         if(number<=2*R_global){
            control_radious<-append(control_radious,number)
            
            if(number==2*R_global){break}
        }
    }
    if(x==y){break}
}
 
control_radious<-factor(control_radious)
control_radious<-levels(control_radious)
control_radious<-as.numeric(control_radious)
control_radious<-control_radious**(1/2)
#Discard the first  value (0)
control_radious<-tail(control_radious,length(control_radious)-1)
#Multiply the initial   radius with the interplot distance
control_radious<-Main_Data_Frame$XPos[1]*control_radious


#Add columns to analysis table global 
#NumB    MeanB       PYI     Mean N sd       HI       GYI        PPE       GPE      mPYI     mPPE
#NumB not  needed (always the same number of entries) 


#Add a size b column in the table
Main_Data_Frame$SizeB<- rep(NA,nrow(Main_Data_Frame))
#Main_Data_Frame$SizeB<<-rep(NA,nrow(Main_Data_Frame))
#Add the meanB column
Main_Data_Frame$MeanB<- rep(NA,nrow(Main_Data_Frame))
#Main_Data_Frame$MeanB<<-rep(NA,nrow(Main_Data_Frame))


for (i in 1:nrow(Main_Data_Frame)){
    mean_vector<-c()
    coordx<-c()
    coordy<-c()
    current_x<-Main_Data_Frame$XPos[i]
    current_y<-Main_Data_Frame$YPos[i]
    #Set the vector containing the entries to spot 
    entries_to_delete<-1:R_global
    #remove the current one
    entries_to_delete<-entries_to_delete[-Main_Data_Frame$Entry[i]]
    
    for(j in control_radious){ #Check for the plants gradually for each radious
        for(k in 1:nrow(Main_Data_Frame)){#Chech the distance for each plant
            testing_x<-Main_Data_Frame$XPos[k]
            testing_y<-Main_Data_Frame$YPos[k]
            distance<-((current_x-testing_x)**2+(current_y-testing_y)**2)**(1/2)
            if( distance >= j-0.00001 && distance<=j+0.0000001 && !(distance==0)  
               ) #It counts NaN values also  
               {  
               
               
                if(Main_Data_Frame$Entry[k] %in% entries_to_delete){
                 
                    #Plot the position of the elements for a specific element 
                    

                    if(!is.null(row_element) &&!is.null(plant_element) ){
                        if(Main_Data_Frame$Row[i]==row_element && Main_Data_Frame$Plant[i]==plant_element){
                            if(!is.nan(Main_Data_Frame$Data[k])){
                                graphics::points(Main_Data_Frame$XPos[k],Main_Data_Frame$YPos[k],
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
                                graphics::points(Main_Data_Frame$XPos[k],Main_Data_Frame$YPos[k],
                                cex=3,
                                pch=4)                            
                            }
                        } 

                    }
                    
                    

                    
                    #Remove the element if it is found
                    a<-Main_Data_Frame$Entry[k]
                    entries_to_delete<-entries_to_delete[! entries_to_delete==a]
                    #Append the entrty to the vector 
                    mean_vector<-append(mean_vector,Main_Data_Frame$Data[k])
                   
                   
                }
            } #j is the control radious
            
        }
    } 




    #Calculate the sizeof block without NaNs
    Main_Data_Frame$SizeB[i]<-length( stats::na.omit(mean_vector))
    #Main_Data_Frame$SizeB[i]<<-length( stats::na.omit(mean_vector))



    #Calculate the mean and add it in  the main table
    Main_Data_Frame$MeanB[i]<-mean(stats::na.omit(mean_vector))
    #Main_Data_Frame$MeanB[i]<<-mean(stats::na.omit(mean_vector))
    
} 


#Calculate the mean of the whole plot excluding na
MeanTrial<-mean(stats::na.omit(Main_Data_Frame$Data))

#Add PYI column to dataframe (localy and globally)
Main_Data_Frame$PYI<-rep(NA,nrow(Main_Data_Frame))
#Main_Data_Frame$PYI<<-rep(NA,nrow(Main_Data_Frame))

#Fill the PYI column
for(i in 1:nrow(Main_Data_Frame)){
    Main_Data_Frame$PYI[i] <-(Main_Data_Frame$Data[i]/Main_Data_Frame$MeanB[i])**2
    #Main_Data_Frame$PYI[i]<<-(Main_Data_Frame$Data[i]/Main_Data_Frame$MeanB[i])**2
}


#Add these if  the design is replicated (this is for rings)
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
    #Set the vector containing the entries
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
    #New index Coefficient of  Data to Selection (CRS).
    #Create a data frame with the max values 
    Entry_for_CRS<-factor(Main_Data_Frame$Entry)
    Entry_for_CRS<-levels(Entry_for_CRS)

     
    table_for_CRS <-data.frame("Entry"=Entry_for_CRS)
    
    for(i in Entry_for_CRS){
        #Convert Entry to numeric
        i<-as.numeric(i)
        obs_for_CRS_Data<-c()
        obs_for_CRS_PYI<-c()

        #Select only rows with the same Entry.  
        obs_for_CRS_Data<-Main_Data_Frame[Main_Data_Frame$Entry==i,"Data"] 

        #This one was added (we need the data of the best  plants according to PYI )
        obs_for_CRS_PYI<-Main_Data_Frame[Main_Data_Frame$Entry==i,"PYI"] 
        dframe_obs_for_CRS<-data.frame(obs_for_CRS_Data, obs_for_CRS_PYI) #This one contains both data and PYI

        #This was added (order the data frame according to PYI)

        #Order the dframe according to PYI but chose the data 
        dframe_obs_for_CRS<-dframe_obs_for_CRS[order(dframe_obs_for_CRS$obs_for_CRS_PYI,na.last=FALSE),]

        #This 
        #Chose the  5 greatest plants and estimate mean
        #obs_for_CRS<-sort(obs_for_CRS,na.last=FALSE)
         
        #select the data from the plants of same entry , having the best PYI 
        selection<-tail(dframe_obs_for_CRS$obs_for_CRS_Data,CRS ) #obs_for_CRS includes the Data ()
         


        #insert the mean of the above selected best plants
        table_for_CRS$Mean_of_Max[i]<-mean(selection, na.rm=TRUE)
        

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

#Create a summay table if the design is replicated
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
#Add a CV in the table (same as for the analysis without blocks)
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


 
 
#This function returns its result to analysis function
return_value<-list(Main_Data_Frame[,!names(Main_Data_Frame) %in% c("mPPE","mPYE","GPE","GYI","CRS")],summary_table_global)
#cat("\n\"Main_Data_Frame\" and \"summary_table_global\" have been modified\n\n")

return(return_value)

}
 