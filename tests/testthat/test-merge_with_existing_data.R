test_that("merge_with_existing_data met à jour correctement les données existantes", {
  # Load the openxlsx2 package if not already loaded
  library(openxlsx2)
  
  file_path <- "C:/Users/hmaire.VIGNEVIN/OneDrive - IFV/Documents/startbox/tests/testthat/testdata/biovimed_teissonniere_2024.xlsx"
  
  wb <- wb_load(file_path)
  
  # Create a combined dataset to merge
  combined <- read.csv2("C:/Users/hmaire.VIGNEVIN/OneDrive - IFV/Documents/startbox/tests/testthat/testdata/teissonniere_dataF1_2024.csv", sep = ";")
  
  # Add required columns for merge (prov_name and prov_date)
  combined$prov_name <- "teissonniere_dataF1_2024.csv"
  combined$prov_date <- format(Sys.Date(), "%d/%m/%Y")
  
  expected_rows <- nrow(combined)
  
  # Call the function
  result <- merge_with_existing_data(wb, combined)
  
  # 5. Check the result
  expect_s3_class(result, "data.frame")
  
  # Check that the new prov_name is included
  expect_true("teissonniere_dataF1_2024.csv" %in% result$prov_name)
  
  # Check that the number of new rows matches the CSV
  expect_equal(nrow(result[result$prov_name == "teissonniere_dataF1_2024.csv", ]), expected_rows)
  
  # Check that key columns are preserved
  expect_true(all(c("plot_id", "PM_LEAF_PC", "bbch_stage", "observation_date") %in% names(result)))
})