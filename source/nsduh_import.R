drug_import= function(drug, tablenum,html){
  drug_df = 
    html |> 
    html_table() |> 
    nth(tablenum) |>
    slice(-1) %>% 
    mutate(drug= drug)
  return(drug_df)
}
