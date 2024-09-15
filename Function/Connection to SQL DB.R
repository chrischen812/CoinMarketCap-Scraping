

query_db <- function(con, query, nrow_to_fetch = 100, nrow_to_print = 10000) {
  start_time <- proc.time()
  print('Connecting to Database')
  res <- DBI::dbSendQuery (con, query)
  df <- NULL
  print('Running')
  cat("\r", 'Row 0')
  repeat {
    df_to_append <- DBI::dbFetch (res, n = nrow_to_fetch)
    df <- bind_rows(df, df_to_append)
    
    if ((nrow(df) %% nrow_to_print) == 0) {cat("\r", paste('Row', format(nrow(df), big.mark = ",")))}
    if (nrow(df_to_append) == 0) {
      cat("\n\n") #this resets printing
      break
    }
  }
  dbClearResult(res)
  code_timer(start_time)
  return(df)
}


code_timer <- function(start_time, end_time = proc.time()) {
  
  time_diff <- (end_time - start_time)[1:3]
  
  format_seconds <- function(x) {
    abs_x <- seconds_to_period(abs(x))
    x <- seconds_to_period(x)
    if (minute(abs_x) == 0) {
      val <- paste(sprintf('%.2f', second(x)), 'seconds')
    } else if (hour(abs_x) == 0) {
      val <- sprintf('%01d:%05.2f', minute(x), second(abs_x))
      return(val)
    } else if (day(abs_x) == 0) {
      val <- sprintf('%01d:%02d:%05.2f', hour(x), minute(abs_x), second(abs_x))
      return(val)
    } else {
      return(x)
    }
  }
  
  
  check_db_tables <- function(db) {
    
    pattern <- "\\w+"
    first_word_from <- db %>% str_extract(pattern) %>% toupper() == "FROM"
    
    if (!first_word_from) {
      db <- paste('FROM', db)
    }
    
    db_tables <- get_table_df(db)
    
    # Add a column with the table names from the database
    pattern <- "((\\w+)\\.(\\w+))"
    db_tables <- db_tables %>%
      select(table_string, table_name, alias, join_tables) %>%
      rename(db_table = table_name)
    
    return(db_tables)
    
  }
  
  
  time_diff_formatted <- sapply(time_diff, format_seconds)
  
  writeLines(
    c(
      paste('real time:         ', getElement(time_diff_formatted, "elapsed")),
      paste('user cpu time:     ', getElement(time_diff_formatted, "user.self")),
      paste('system cpu time:   ', getElement(time_diff_formatted, "sys.self"))
    )
  )
  
  invisible(time_diff)
  
}

connect_to_db <- function(dsn = "Teradata") {
  start_time <- proc.time()
  con <- dbConnect(odbc::odbc(),
                   dsn,
                   UID = rstudioapi::askForPassword("MS ID"),
                   PWD = rstudioapi::askForPassword("MS Password"),
                   timeout = 90)
  code_timer(start_time)
  return(con)
}


library(sqldf)
library(tidyverse)
library(lubridate)

Level2 <- connect_to_db(dsn="LEV_PROD")
query <- paste(
"SELECT TOP 1000 *",
"FROM [dbo].[Members_202306_20230718]")

df2 <- query_db(Level2, query)



