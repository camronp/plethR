% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/export_as_clean_df.R
\name{export_as_clean_df}
\alias{export_as_clean_df}
\title{Export Data Frames as Clean Excel File}
\usage{
export_as_clean_df(data_frames, file_name)
}
\arguments{
\item{data_frames}{A named list of data frames to be exported. This can be the output of either
`rename_grouped_dataframes()` or `merge_group_avg()`.}

\item{file_name}{A character string specifying the base file name for the Excel file.
The file will be saved with the `.xlsx` extension in the current working directory.}
}
\value{
NULL (invisible). The function is used for its side effect of exporting the Excel file.
}
\description{
This function exports either cleaned renamed data frames or merged renamed data frames
to an Excel file, saved in the current working directory. Each data frame is saved
as a separate sheet in the Excel file.
}
\details{
Each data frame in the input list will be written to a separate sheet in the Excel file.
The sheet names will be derived from the names of the data frames in the list.
}
\examples{
# Example usage with renamed cleaned data frames
renamed_dfs <- rename_grouped_dataframes(df_list, group_mapping, group_names)
export_as_clean_df(renamed_dfs, file_name = "cleaned_data")

# Example usage with merged renamed data frames
merged_dfs <- merge_group_avg(df_list_clean, group_identities, group_names)
export_as_clean_df(merged_dfs, file_name = "merged_data")

}
