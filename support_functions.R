read_survey_data <- function(path, sheet, skip=3) {
  headers <- read_xlsx(path,sheet=sheet)
  colnames(headers) <- tolower(colnames(headers))
  non_question_columns <- c('school_name','school_type','number of eligible responses','number of parent responses')
  data <- read_xlsx(path,sheet=sheet,skip=skip,col_names = colnames(headers))
  headers <- headers %>% dplyr::select(-one_of(non_question_columns))
  data <- data %>% dplyr::select(-one_of(non_question_columns))
  response_cats <- headers[1,]
  data <- as.data.frame(data, stringsAsFactors = FALSE, row.names = NULL)
  return(list(data,response_cats))
}


get_summarized_scores <- function(df, response_cats,used_ratings,rating_values){
  non_likert <- colnames(df)[is.na(match(response_cats,used_ratings))]
  rating_multipler <- rating_values[match(response_cats,used_ratings)]
  rating_multipler <- rating_multipler[!is.na(match(response_cats,used_ratings))]
  rating_multipler <- rating_multipler[!is.na(rating_multipler)]
  
  questions <- colnames(df)[!grepl("dbn|x__[0-9]{1,}",colnames(df))]
  excluded_questions <- non_likert[!grepl("dbn|x__[0-9]{1,}",non_likert)]
  included_questions <- questions[!(questions %in% excluded_questions)]
  df_questions <- df[,!(colnames(df) %in% non_likert[-1])]
  rating_reduced <- c(NA,rating_multipler[!is.na(rating_multipler)])
  for(i in c(2:ncol(df_questions))){
    #print(i)
    df_questions[,i] <- as.numeric(df_questions[,i])
  }
  df_questions[is.na(df_questions)] <- 0
  #missing_responses <- apply(df_questions,1,FUN=function(x) sum(is.na(x)))
  #df_questions <- df_questions[missing_responses == 0,]
  
  rating_multipler_mat <- matrix(rep(rating_multipler,nrow(df_questions)),nrow=nrow(df_questions),byrow=TRUE)
  identity <- matrix(rep(rep(1,length(rating_multipler)),nrow(df_questions)),nrow=nrow(df_questions),byrow=TRUE)
  prods <- as.matrix(df_questions[,-1])*rating_multipler_mat
  counts <- as.matrix(df_questions[,-1])*identity
  col_nums <- c(match(included_questions,colnames(prods)),ncol(prods)+1)
  df_summarized <- data.frame(matrix(NA,nrow=nrow(df_questions),ncol=length(included_questions)),row.names = NULL)
  for(i in c(1:length(included_questions))){
    df_summarized[,i] <- rowSums(prods[,col_nums[i]:(col_nums[i+1]-1)],na.rm = TRUE)/rowSums(counts[,col_nums[i]:(col_nums[i+1]-1)],na.rm = TRUE) 
  }
  colnames(df_summarized) <- included_questions
  df_summarized$dbn <- df_questions$dbn
  
  ## exclude missing rows and columns
  cols_to_keep <- !(apply(df_summarized,2,FUN= function(x) sum(is.na(x))) == nrow(df_summarized))
  rows_to_keep <- !(apply(df_summarized,1,FUN= function(x) sum(is.na(x))) == (ncol(df_summarized)-1))
  df_summarized <- df_summarized[rows_to_keep,cols_to_keep]
  colnames(df_summarized)[1:(ncol(df_summarized)-1)] <- str_extract(colnames(df_summarized)[1:(ncol(df_summarized)-1)],"[0-9]{1,}[a-z]{0,}.")
  
  return(df_summarized)
}

load_perf_data <- function(){
  charter_ela <- read_xlsx("data/charter-school-results-2013-2019-(public).xlsx",sheet="ELA")
  charter_ela$subject <- "ELA"
  charter_math <- read_xlsx("data/charter-school-results-2013-2019-(public).xlsx",sheet="Math")
  charter_math$subject <- "Math"
  district_ela <- read_xlsx('data/school-ela-results-2013-2019-(public).xlsx',sheet='All')
  district_ela$subject <- "ELA"
  district_math <- read_xlsx('data/school-math-results-2013-2019-(public).xlsx',sheet='All')
  district_math$subject <- "Math"
  
  perf <- rbind(charter_ela,charter_math,district_ela,district_math)
  return(perf)
}