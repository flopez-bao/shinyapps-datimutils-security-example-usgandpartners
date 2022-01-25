#these functions are meant to return developer friendly objects
#these functions will eventually be added to datimutils

#populate three dataframes for an end user developer to join this with their dataframe
#MechsByCOCUID
#MechsByMechNumber
#MechsByName



#' @export
#' @title getMechs(username=NULL, password = NULL)
#' @description Allows developer to derive datim user type and apply security restrictions accordingly. 
#' @param username DHIS 2 username.
#' @param password DHIS 2 password for the username.
#' @param base_url base_url The URL of the server, e.g. https://www.datim.org/. 
#' @param by the id mechanisms should be pulled by, e.g. cocuid, mech_id, mech_name

getMechs <- function(username, password, base_url) {
  
  #stop if username and password are not entered
  if((!(is.null(username)) && is.null(password)) || (is.null(username) && !(is.null(password)))){
    stop("If directly providing function credentials you must specify both username and password")
  }
  
  # fetch api, report error if unable to resolve
  url <- paste0(base_url,"api/categoryOptions?filter=categories.id:eq:SH885jaRe0o&fields=name,id,categoryOptionCombos[name,id,code]&paging=false")
  tryCatch(
    expr = {
      req <- httr::GET(url, httr::authenticate(username, password))
    },
    error = function(e){ 
      print(e)
    }
  )
  
  # process api content
  json <- httr::content(req)
  col_names <- c("mech_code", "category_option_combos_id", "name")
  my_cat_ops <- unstack(data.frame(d<-unlist(json),names(d)))[,c(1,2,3)]
  names(my_cat_ops) <- col_names
  
  return(my_cat_ops)  
}

#' @export
#' @title getStreams(username=NULL, password = NULL)
#' @description Allows developer to derive datim user streams and apply security restrictions accordingly. 
#' @param username DHIS 2 username.
#' @param password DHIS 2 password for the username.
#' @param base_url base_url The URL of the server, e.g. https://www.datim.org/. 

getStreams <- function(username, password, base_url) {
  
  #stop if username and password are not entered
  if((!(is.null(username)) && is.null(password)) || (is.null(username) && !(is.null(password)))){
    stop("If directly providing function credentials you must specify both username and password")
  }
  
  # fetch api, report error if unable to resolve
  url <- paste0(base_url,"api/me?fields=userGroups%5Bname,id%5D")
  tryCatch(
    expr = {
      req <- httr::GET(url, httr::authenticate(username, password))
    },
    error = function(e){ 
      print(e)
    }
  )
  
  #process group ids
  json <- httr::content(req, "text")
  groups_id <- jsonlite::fromJSON(json)$userGroups[,"name"]
  
  #return the relevant streams for classification
  user_groups <- groups_id[grepl("Data (.+?) access|^Global|^OU",groups_id)]
  
  #remove Data Access
  user_groups_e <- gsub("Data | access", "", user_groups)
  
  #return data frame
  groups_id_df <- data.frame(stream = user_groups_e, stringsAsFactors = F)
  
  return(groups_id_df)
}

#' @export
#' @title getUserType(username=NULL, password = NULL)
#' @description Allows developer to classify a user based on their data streams. 
#' @streams data streams as a vector.

getUserType <- function(streams) {
  
  if( length( regmatches(streams ,regexpr("OU (.+?) MOH users", streams)) )  > 0 ) {
    return("moh")
  } else if ( length(regmatches(streams ,regexpr("Global users", streams)) ) > 0 ) {
    return("global only")
  } else if ( length( regmatches(streams, regexpr("OU (.+?) Partner (.+?) users - (.+?)",streams)) ) > 0 ) {
    return("partner only")
  } else if ( length( regmatches(streams, regexpr("OU (.+?) Agency (.+?) users",streams)) ) > 0 ) {
    return("agency only")
  } else if ( length( regmatches(streams, regexpr("OU (.+?) Interagency users",streams)) ) > 0 ) {
    return("interagency only")
  } else if ( length( regmatches(streams, regexpr("Global Agency (.+?) users",streams)) ) > 0 ) {
    return("global agency")
  } else if ( length( regmatches(streams, regexpr("Global Partner (.+?) users - (.+?)",streams)) ) > 0 ) {
    return("global partner")
  } else {
    return("unclassified user")
  }
}

# #MOH
# z = getUserType("moh_zaf","ipod-invent-Pots-8")
# regmatches(z ,regexpr("OU (.+?) MOH users", z))
# 
# #Global Only
# z = getUserType("flopez","Artemis9185!")
# regmatches(z ,regexpr("Global users", z))
# 
# #partner only
# z = getUserType("p_nga_msh","ipod-invent-Pots-8")
# regmatches(z, regexpr("OU (.+?) Partner (.+?) users - (.+?)",z))
# 
# #agency only
# z = getUserType("a_nga_usaid","ipod-invent-Pots-8")
# regmatches(z, regexpr("OU (.+?) Agency (.+?) users",z))
# 
# #interagency
# z = getUserType("ia_zaf","ipod-invent-Pots-8")
# regmatches(z, regexpr("OU (.+?) Interagency users",z))
# 
# #global agency
# z = getUserType("smokeGA","ipod-invent-Pots-8")
# regmatches(z, regexpr("Global Agency (.+?) users",z))
# 
# #global partner
# z = getUserType("smokeGP","ipod-invent-Pots-8")
# regmatches(z, regexpr("Global Partner (.+?) users - (.+?)",z))
# 
# 
# classifyUser(z)



#user name examples
#smokeGP - Global Partner
#smokeGA - Global Agency
#flopez - Global
#smokeIA - Interagency
#smokeAgency - Agency
#smokeIP - Partner
#smokeMOH - MOH




