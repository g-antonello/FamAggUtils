
# 2 utility functions to make the code easier downstream 
is.father <- function(ped.df, id, id2){
  return(any(ped.df$id == id & ped.df$father == id2))
}

is.mother <- function(ped.df, id, id2){
  return(any(ped.df$id == id & ped.df$mother == id2))
}

#! example run
#! --------------------
#! library(FamAgg)
#! data(minnbreast)
#! 
#! minnbreast.ped <- minnbreast[, c("famid", "id", "fatherid", "motherid", "sex")]
#! 
#! ## Renaming column names.
#! colnames(minnbreast.ped) <- c("family", "id", "father", "mother", "sex")
#! endage <- minnbreast$endage
#! names(endage) <- minnbreast.ped$id
#! ## Create the object.
#! fad <- FAData(pedigree = minnbreast.ped, age = endage)
#! 
#! plotPed(fad, family = 243, filename = "~/tmp_data_to_share/pedigree-test.png", device = "png")
#! 
#! ancestors <- getAncestors(fad, "9508")
#! 
#! source("~/bin/FamAgg.Utils.R")
#! get_id_ancestors_relationship(fad, id = 9508, ancestors_id = ancestors)
#! --------------------
#! end of example run

get_id_ancestors_relationship <- function(ped, id, ancestors_id){
 
  # check class of the input object
  
  if(class(ped) != "FAData"){
    stop("Only FamAgg format allowed")
  }
  
  if((length(ancestors_id) == 0)| is.null(ancestors_id)| is_empty(ancestors_id)){
    stop(return(NULL))
  }
  
 
  
  # create a data frame
  ped.df <- pedigree(ped)
  
  
  ancestors_renamed <- character(length = length(ancestors_id))
  
  for(i in 1:length(ancestors_id)){
    name_step1 <- ifelse(
      test = is.father(ped.df, id, ancestors_id[i]), 
      yes = "father",
      no = ifelse(
        test = is.mother(ped.df, id, ancestors_id[i]),
        yes = "mother",
        no = "Other"
          )
      )
    
    if(name_step1 == "Other"){
      
      father <- ped.df[ped.df$id == id, "father"]
      mother <- ped.df[ped.df$id == id, "mother"]
      
      name_step2 <- ifelse(
        test = is.father(ped.df, father, ancestors_id[i]),
        yes = "fa_grandpa",
        no = ifelse(
          test = is.mother(ped.df, father, ancestors_id[i]),
          yes = "fa_grandma",
          no = ifelse(
            test = is.father(ped.df, mother, ancestors_id[i]),
            yes = "mo_grandpa",
            no = ifelse(
              test = is.mother(ped.df, mother, ancestors_id[i]),
              yes = "mo_grandma",
              no = "Older_ancestor"
          )
        )
      )
    )
      ancestors_renamed[i] <- name_step2
    } else {
      ancestors_renamed[i] <- name_step1
    }
  }
  names(ancestors_id) <- ancestors_renamed
  return(ancestors_id)
}

