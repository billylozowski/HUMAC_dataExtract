# HUMAC_dataExtract

### Overview
The R scripts within the HUMAC_dataExtract folder scrape data from downloaded .pdf reports.
Scripts can extract data from multiple .pdf files at once, providing these are organised in a single file folder.
The data is then saved as a .csv file, with one row corresponding to one report.

### Long format, position vs. time
Currently, this is the only script that has been completed.

### Calling the Function
When calling the function, "sport" and "session" need to be defined.
- This assumes the folder structure of data > sport > session (i.e., data/baseball/2026_09)

However, folder structure can be defined as the user chooses. If the structure is changed, ensure that the .csv file
is saved to the desired location by updating these variables.

For example: 

data > subfolder_1 > ... > subfolder_n (data/subfolder_1/.../subfolder_n)

then:

write_csv(session.data, file.path(folder_OUT, paste0("Extracted HUMAC Data for ", subfolder_1, " - ", subforlder_n, ".csv")))


