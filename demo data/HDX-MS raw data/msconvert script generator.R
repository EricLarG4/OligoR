library(tidyverse)

#indicate demo data time filters file
convert.generator <- readxl::read_excel(
  "demo data/HDX-MS raw data/mzML convert generator.xlsx",
  sheet = "23TAG-PhenDC3"
)

#specify input file template, ouput file path, and output file name template
#use double backslashes \\
input.file.template <- 'msconvert C:\\Users\\Eric\\Desktop\\23TAG-Phen-DC3\\180209-Exac-5617-VG-EL-23TAG-Phen-DC3-'
#must contain final \\
output.path <- 'C:\\Users\\Eric\\Desktop\\23TAG-Phen-DC3\\filtered\\'
output.file.template <- '23TAG-Phen-DC3-'

#generate command line for each file
convert.generator <- convert.generator %>%
  mutate(
    commands = paste0(
      input.file.template,
      tube,
      '.RAW --mzML -g --filter "mzWindow [',
      start.mz,
      ',',
      end.mz,
      ']" -o ',
      output.path,
      ' --outfile ',
      output.file.template,
      pt,
      ' --filter "scanTime [',
      start.s,
      ',',
      end.s,
      ']"'
    )
  )

#generate concatenated command line without starting and ending quotes and double backslashes
writeLines(paste(convert.generator$commands, collapse = ' && '))




#############################
# application to other data #
#############################

#23TAG----

#indicate demo data time filters file
convert.generator <- readxl::read_excel(
  "demo data/HDX-MS raw data/mzML convert generator.xlsx",
  sheet = "23TAG"
)

#specify input file template, ouput file path, and output file name template
#use double backslashes \\
input.file.template <- 'msconvert C:\\Users\\Eric\\Desktop\\23TAG\\1802114-Exac-5621-VG-EL-23TAG-'
#must contain final \\
output.path <- 'C:\\Users\\Eric\\Desktop\\23TAG\\filtered\\'
output.file.template <- '23TAG-Phen-DC3-'

#generate command line for each file
convert.generator <- convert.generator %>%
  mutate(
    commands = paste0(
      input.file.template,
      tube,
      '.RAW --mzML -g --filter "mzWindow [',
      start.mz,
      ',',
      end.mz,
      ']" -o ',
      output.path,
      ' --outfile ',
      output.file.template,
      pt,
      ' --filter "scanTime [',
      start.s,
      ',',
      end.s,
      ']"'
    )
  )

#generate concatenated command line without starting and ending quotes and double backslashes
writeLines(paste(convert.generator$commands, collapse = ' && '))

#T30177-TT----

#indicate demo data time filters file
convert.generator <- readxl::read_excel(
  "demo data/HDX-MS raw data/mzML convert generator.xlsx",
  sheet = "T30177-TT"
)

#specify input file template, ouput file path, and output file name template
#use double backslashes \\
input.file.template <- 'msconvert C:\\Users\\Eric\\Desktop\\T30177TT\\190109-Exac-6356-VG-EL-T30-'
#must contain final \\
output.path <- 'C:\\Users\\Eric\\Desktop\\T30177TT\\filtered\\'
output.file.template <- 'T30177TT-'

#generate command line for each file
convert.generator <- convert.generator %>%
  mutate(
    commands = paste0(
      input.file.template,
      tube,
      '.RAW --mzML -g --filter "mzWindow [',
      start.mz,
      ',',
      end.mz,
      ']" -o ',
      output.path,
      ' --outfile ',
      output.file.template,
      pt,
      ' --filter "scanTime [',
      start.s,
      ',',
      end.s,
      ']"'
    )
  )

#generate concatenated command line without starting and ending quotes and double backslashes
writeLines(paste(convert.generator$commands, collapse = ' && '))
