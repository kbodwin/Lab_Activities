teach_me <- function(.source, input){
  
  #### Find names of shiny user inputs ####
  
  # all input names
  
  input_names <- .source %>% 
    str_extract_all("\\{[^\\}]*\\}")%>%
    unlist() %>%
    str_remove(fixed("{")) %>%
    str_remove(fixed("}")) %>%
    intersect(names(input))
  
  #### Sanitize inputs, then insert into code string and evaluate ####
  
  # Make a list of !!sym wrapped user inputs
  sym_inputs <- sub_syms(input_names)
  names(sym_inputs) <- input_names
  
  # Create expression  (glue in the non-user-input things)
  sym_code <- glue_data(as.list(sym_inputs), .source)
  sym_code <- glue(sym_code)
  raw_code <- eval(parse(text = paste0("rlang::expr(",sym_code,")")))
  
  #outputs <- evaluate::evaluate(raw_code)
  
  #### Now make the formatted one ####
  
  # inputs_new <- inputs %>%
  #   wrap_inputs()
  
  html_code <- .source #%>%
  #hlt_regexp("\\{[^\\}]*\\}") #%>%
  #glue_data(.x = input, .) %>%
  #glue() %>%
  #demoR::txt_tocode()
  
  #html_code <- paste0("<pre><code class = 'language-r'>", html_code, "</code></pre>")
  
  return(list(raw_code = raw_code, html_code = html_code))
  
}



sub_syms <- function(input_names){
  
  paste0("(!!sym(input[['", input_names, "']]))")
  
}
