# ms-qc-report
visualisation tools for reporting QC for targeted pipeline


The following are instructions on how to generate a report for any of the targeted assays, inclusing of SCFA, BA, AA, TRY and ddhN. The function works with either a dataElement or with TSV, xml and TXT files. 

The function looks like this

generateReport(folder = ,fileType = ,assay = ,title = ,plateOrder = ,output_file = ,scale = )


folder argument
The lab team will supply a path to a folder that contain all the files they wish to have included in their report. They must all be the same type of file (all xml, TSV or TXT). Create a folder on your computer and save the path to your file, on a mac you will use "~/Desktop/filename" on a microsoft "C::\\Users\\username\\Desktop\\filename". For a Windows, you may need to confirm your username/path. Use home_dir <- Sys.getenv("R_USER") to see the correct start to your path.

MAC:
folder = "~/Desktop/filename"

wInDoWs

folder = "C::\\Users\\username\\Desktop\\filename"

For those using a dataElement, load the dae into your environment and supply it to folder
folder = daename

output_file argument
All users need to define where the html report will be saved. It is reccommended that you use projectName cohortName sampleMatrixType runID and assay. Ensure that you use .html at the end of your path

MAC
output_file = "~/Desktop/covid19_biogune_PLA_COVr01_MS_AA.html"

wInDoWs

folder = "C::\\Users\\username\\Desktop\\covid19_biogune_PLA_COVr01_MS_AA.html"

fileType argument
Those supplying a folder with a file in it, please specify the fileType "xml", "tsv" or "txt". Those using a dae do not need to use this argument. 

assay argument
All users must supply then assay argument, "SCFA", "AA", "BA", "TRY" or "ddhN" are accepted. 

title argument
You can supply the title that will appear inside the report as a character. It's reccomended that you use projectName cohortName sampleMatrixType and assay. So, for example, supply:

title = "Covid 19 Biogine PLA MS AA"

scale argument
This is a TRUE or FALSE that determines in the scales are log10 or not. The default is TRUE so if you would like to change that simply supply

scale = FALSE

plateOrder
If your data has acquisition dates available for all plates, it will automatically order them using this. If not, the plates are ordered by alphanumeric order (COVp01, COVp02, COVp03 etc). In any case, you can supply what order you want the plates to appear:

plateOrder = c("COVp02", "COVp01", "COVp03")


At a bare minimum your line of code should look like:

generateReport(folder = "C::\\Users\\username\\Desktop\\covidbiogune",
              fileType = "xml", 
              assay = "AA" ,
              title = "Covid 19 Biogune PLA MS AA",
              output_file = "C::\\Users\\username\\Desktop\\covid19_biogune_PLA_COVr01_MS_AA.html")


or, with a dae covid19_biogune_PLA_COVr01_MS_AA

generateReport(folder = biogune,
              assay = "AA" ,
              title = "Covid 19 Biogune PLA MS AA",
              output_file = "C::\\Users\\username\\Desktop\\covid19_biogune_PLA_COVr01_MS_AA.html")
