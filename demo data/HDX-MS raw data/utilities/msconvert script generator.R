library(tidyverse)

# ms convert is there: cd C:\Program Files\ProteoWizard\ProteoWizard 3.0.22163.4cd43c9


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



#VEGF----

#indicate demo data time filters file
convert.generator <- readxl::read_excel(
  "demo data/HDX-MS raw data/mzML convert generator.xlsx",
  sheet = "VEGF"
)

#specify input file template, ouput file path, and output file name template
#use double backslashes \\
input.file.template <- 'msconvert C:\\Users\\Eric\\Desktop\\VEGF\\190110-Exac-6358-VG-EL-VEGF-'
#must contain final \\
output.path <- 'C:\\Users\\Eric\\Desktop\\VEGF\\filtered\\'
output.file.template <- 'VEGF-'

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




#5YEY IMS----

#indicate demo data time filters file
convert.generator <- readxl::read_excel(
  "demo data/HDX-MS raw data/utilities/mzML convert generator.xlsx",
  sheet = "5YEY"
)

#specify input file template, ouput file path, and output file name template
#use double backslashes \\
input.file.template <- 'msconvert C:\\Users\\Eric\\Desktop\\cache\\5YEY\\220811-RG-5YEY-1KCl-'
#must contain final \\
output.path <- 'C:\\Users\\Eric\\Desktop\\cache\\5YEY\\filtered\\'
output.file.template <- '5YEY-'

#generate command line for each file
convert.generator <- convert.generator %>%
  mutate(
    commands = paste0(
      input.file.template,
      tube,
      '.d --mzML -g --filter "mzWindow [',
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


#5YEY IMS - zone 1----

#indicate demo data time filters file
convert.generator <- readxl::read_excel(
  "demo data/HDX-MS raw data/utilities/mzML convert generator.xlsx",
  sheet = "5YEY-zone1"
)

#specify input file template, ouput file path, and output file name template
#use double backslashes \\
input.file.template <- 'msconvert C:\\Users\\Eric\\Desktop\\cache\\5YEY-zone1\\220811-RG-5YEY-1KCl-'
#must contain final \\
output.path <- 'C:\\Users\\Eric\\Desktop\\cache\\5YEY-zone1\\filtered\\'
output.file.template <- '5YEY-zone1-'

#generate command line for each file
convert.generator <- convert.generator %>%
  mutate(
    commands = paste0(
      input.file.template,
      tube,
      '_filtered[1].d --mzML -g --filter "mzWindow [',
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


#2KPR----

#indicate demo data time filters file
#use double backslashes \\
convert.generator <- readxl::read_excel(
  "C:\\Users\\Eric\\OneDrive - u-bordeaux.fr\\Backups\\Raw Data IMS\\HDX\\RG\\mzML convert generator.xlsx",
  sheet = "2KPR"
)

#specify input file template, ouput file path, and output file name template
#use double backslashes \\
input.file.template <- 'msconvert C:\\Users\\Eric\\Desktop\\cache\\2KPR\\220823-RG-2kpr-1KCl-'
#must contain final \\
output.path <- 'C:\\Users\\Eric\\Desktop\\cache\\2KPR\\converted\\'
output.file.template <- '2KPR'

#generate command line for each file
convert.generator <- convert.generator %>%
  mutate(
    commands = paste0(
      input.file.template,
      tube,
      '.d --mzML -g --filter "mzWindow [',
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

