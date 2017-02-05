#' RobsRUtils: Routines I have found or created along the way
#'
#' @section RobsRUtils functions:
#' The RobsRUtils functions ...
#'    \itemize{
#'      \item \code{\link{ErrorBar_LL_UL}}
#'      \item \code{\link{FormatJSON_String}}
#'      \item \code{\link{GetColourBlindPalette}}
#'      \item \code{\link{GetColourBlindPalette}}
#'      \item \code{\link{GetPaletteSize}}
#'      \item \code{\link{MD5_thisFile}}
#'      \item \code{\link{MakePie}}
#'      \item \code{\link{ProgressDot}}
#'      \item \code{\link{PlotPDF}}
#'      \item \code{\link{ProducePlot}}
#'      \item \code{\link{SHA1_thisFile}}
#'      \item \code{\link{SHA256_thisFile}}
#'      \item \code{\link{TrimLeading}}
#'      \item \code{\link{TrimTrailing}}
#'      \item \code{\link{Trim}}
#'    }
#' @docType package
#' @name RobsRUtils
NULL

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste0('RobsRUtils version: ',packageVersion('RobsRUtils')))
}

#' Utility to provide Lower and Upper Limit values for Error Bars.
#'
#' @param x - the data
#' @param type - either SD, SE, or CI
#' @param CI - confidence interval, defaults to 95\%
#' @return results with ymin and ymax
#' @export
#' @examples
#' \dontrun{
#' library(ggplot)
#' p<-ggplot(data=plot.data,aes(x=measured.value,y=outcome))
#' p<-p+geom_point(size=2,col='blue')
#' p<-p+stat_summary(fun.y='mean', geom="point", size = 5, shape=15,col='red')
#' This style is from the old ggplot v1 code
#' p<-p+stat_summary(fun.data = 'ErrorBar_LL_UL',type='CI',CI=0.95, geom = "errorbar", width = 10)
#' This style is from the newer ggplot v2 code ... hopefully you are at this level or above
#' p<-p+stat_summary(fun.data = 'ErrorBar_LL_UL',fun.args=list(type='CI',CI=0.95), geom = "errorbar", width = 10)
#' p
#' }
ErrorBar_LL_UL<-function(x,type='SD',CI=0.95)
{
  #print(type)
  #print(CI)
  if(type=='SD')
  {
    result = c(mean(x) - sd(x), mean(x) + sd(x))
  }
  else if(type=='SE')
  {
    se<-sd(x)/sqrt(length(x))
    result = c(mean(x) - se, mean(x) + se)
  }
  else if(type=='CI')
  {
    # Need SE first
    N<-length(x)
    se<-sd(x)/sqrt(N)
    # Confidence interval multiplier for SE
    # Calculate t-statistic for confidence interval:
    # i.e if conf.interval = 0.95, use 0.975 (-/+), assumes df=N-1
    ciMult <- qt(CI/2 + 0.5, N-1)
    conf.int <- se * ciMult
    result = c(mean(x) - conf.int, mean(x) + conf.int)
  }
  else
  {
    message(paste0('Unknown type: ', type))
    message('Using type=SD')
  }

  names(result) = c("ymin", "ymax")
  return(result)
}

#' A text based progress bar
#'
#' @param count - the current count
#' @param total - the final count
#' @export
#' @examples
#' \dontrun{
#' ProgressDot(count,my.total.loop.size)
#' }
ProgressDot<-function(count,total)
{
  if(total < 500)
  {
    dotBlock=10
    lineBlock=100

    if(total < 50)
    {
      dotBlock=1
      lineBlock=10
    }
  }
  else if (total < 5000)
  {
    dotBlock=100
    lineBlock=1000
  }
  else
  {
    dotBlock=100
    lineBlock=2000
  }

  if (count%%dotBlock == 0)
  {
    cat('.')

    if (count%%lineBlock == 0)
    {
      cat(paste0(' ',count,'/',total,'\n'))
    }
  }
}

#' Utility to wrap the standard pdf() driver to A4 or A3 format
#'
#' Remember to use dev.off() after you have done the plot.
#'
#' @param plot.filename - the filename for the plot
#' @param page.size='A4' - used to specify page size, can be A3
#' @param landscape=TRUE - used to specify page orientation
#'
#' @export
#' @examples
#' \dontrun{
#' filename<-'A_good_plot.pdf'
#' PlotPDF(filename)
#'  ... some plot commands
#' dev.off()
#'
#' PlotPDF(filename,page.size='A3',landscape=FALSE)
#'  ... some plot commands
#' dev.off()
#' }
PlotPDF<-function(plot.filename,page.size='A4',landscape=TRUE)
{

  if(page.size=='A3')
  {
    if(landscape)
    {
      # A3 Landscape
      width<-420/25.4
      height<-297/25.4
    }
    else
    {
      # A3 Portrait
      height<-420/25.4
      width<-297/25.4
    }
  }else{
    if (landscape)
    {
      # A4 Landscape
      width<-297/25.4
      height<-210/25.4
    }
    else
    {
      # A4 Portrait
      height<-297/25.4
      width<-210/25.4
    }
  }

  pdf(plot.filename, width=width, height=height,pointsize=10)
}


#' Utility to return string w/o leading whitespace
#'
#' @param x - the string to process
#' @return string w/o leading whitespace
#' @export
#' @examples
#' \dontrun{
#' filename<-'  File_1234.json'
#' filename.clean<-TrimLeading(filename)
#' }
# returns string w/o leading whitespace
TrimLeading <- function (x)  sub("^\\s+", "", x)

#' Utility to return string w/o trailing whitespace
#'
#' @param x - the string to process
#' @return string w/o trailing whitespace
#' @export
#' @examples
#' \dontrun{
#' filename<-'File_1234.json   '
#' filename.clean<-TrimTrailing(filename)
#' }
TrimTrailing <- function (x) sub("\\s+$", "", x)

#' Utility to return string w/o leading or trailing whitespace
#'
#' @param x - the string to process
#' @return string w/o trailing whitespace
#' @export
#' @examples
#' \dontrun{
#' filename<-'  File_1234.json   '
#' filename.clean<-Trim(filename)
#' }
Trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#' Utility to return a colour blind sensitive colour palette
#'
#' @param size = NULL - provide a size to enable colour recycling
#' @return a character vector with the colour blind palette codes
#' @export
#' @examples
#' \dontrun{
#' cbp<-GetColourBlindPalette()
#' cbp<-GetColourBlindPalette(size=16)
#' cbp<-GetColourBlindPalette(size=33)
#' }
#'
GetColourBlindPalette<-function(size=NULL)
{
  # See http://jfly.iam.u-tokyo.ac.jp/color/

  cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7","#F0E442")
  num.base.colours<-length(cbPalette)

  if(is.null(size) == FALSE)
  {
    if(size > num.base.colours )
    {
      # We need to recycle colours
      # how many repeat groups? ... use integer division
      repeat.colour.groups<-size%/%num.base.colours

      if(size%%num.base.colours == 0)
      {
        cbPalette<-rep(cbPalette,repeat.colour.groups)
      }
      else
      {
        # We need one more group
        cbPalette<-rep(cbPalette,repeat.colour.groups+1)
      }
    }
  }

  return(cbPalette)
}

#' Utility to return the palette size for GetColourBlindPalette()
#'
#' @param num.groups - provide a size to enable colour recycling
#' @param palette.block.size = 8 - size of the block of colours used by GetColourBlindPalette()
#' @return the palette size - this is a multiple of palette.block.size
#' @export
#' @examples
#' \dontrun{
#' num.process.runs <- 22
#' palette.size <- GetPaletteSize(num.process.runs)
#' cbp<-GetColourBlindPalette(size=palette.size)
#' }
#'
GetPaletteSize<-function(num.groups,palette.block.size=8)
{
  palette.size<-palette.block.size

  pal.rem<-num.groups%%palette.block.size
  if(pal.rem > 0)
  {
    palette.size<-((num.groups%/%palette.block.size) + 1) * palette.block.size
  }else{
    palette.size<-(num.groups%/%palette.block.size) * palette.block.size
  }

  return(palette.size)
}

#' Utility to return MD5 signature of a file
#'
#' @param filename
#' @param writeMD5File
#' @return an uppercase MD5 signature
#' @export
#' @examples
#' \dontrun{
#' MD5_thisFile(filename)
#' }
#'
MD5_thisFile<-function(filename,writeMD5File = TRUE)
{
  md5<-toupper(as.character(md5sum(filename)))

  if(writeMD5File)
  {
    signature.filename<-paste0(removeFileExtension(filename),'.md5')
    cat(file = signature.filename,paste0(md5,' ',basename(filename)))
  }

  return(md5)
}


#' Utility to return SHA1 signature of a file
#'
#' @param filename
#' @param writeSHA1File = TRUE - will write a file with the same
#' base name and extension .sha1 to the same dir as the file
#' @return an uppercase SHA1 signature
#' @export
#' @examples
#' \dontrun{
#' SHA1_thisFile(filename)
#' }
#'
SHA1_thisFile<-function(filename,writeSHA1File = TRUE)
{
  sha1<-toupper(as.character(digest(filename, algo = "sha1", file = TRUE)))

  if(writeSHA1File)
  {
    signature.filename<-paste0(removeFileExtension(filename),'.sha1')
    cat(file = signature.filename,paste0(sha1,' ',basename(filename)))
  }

  return(sha1)
}

#' Utility to return SHA256 signature of a file
#'
#' @param filename
#' @param writeSHA256File = TRUE - will write a file with the same
#' base name and extension .sha256 to the same dir as the file
#' @return an uppercase SHA256 signature
#' @export
#' @examples
#' \dontrun{
#' SHA256_thisFile(filename)
#' }
#'
SHA256_thisFile<-function(filename,writeSHA256File = TRUE)
{
  sha256<-toupper(as.character(digest(filename, algo = "sha256", file = TRUE)))

  if(writeSHA256File)
  {
    signature.filename<-paste0(removeFileExtension(filename),'.sha256')
    cat(file = signature.filename,paste0(sha256,' ',basename(filename)))
  }

  return(sha256)
}


#' Utility to return a formatted JSON string
#'
#' @param json.text - the JSON string
#' @param filename  = NULL, if you provide a string it will be written to that file
#' in the current folder. Provide a full path if you want it to go somewhere else
#' @return  a formatted version of the json.text
#' @export
#' @examples
#' \dontrun{
#' json.text<-SomeJSON_string_from_SomeWhere
#' FormatJSON_String(json.text)
#' FormatJSON_String(json.text,filename='MyJSON_File.json')
#' }
#'
FormatJSON_String<-function(json.text,filename=NULL)
{
  dfl <- jsonlite::fromJSON(json.text)
  json.text<-jsonlite::toJSON(dfl, pretty = TRUE)
  if(!is.null(filename)) # Watch the ! for NOT
  {
    writeLines(json.text,con=filename)
  }
  return(json.text)
}


#' Utility to generate encryption keys
#'
#' @return  list with two file names to private and public key files (.Rdata format)
#' @export
#' @examples
#' \dontrun{
#' my.keys<-GenAndSaveKeys()
#' }
#'
GenAndSaveKeys<-function()
{
  key <- PKI.genRSAkey(2048)

  fileTime<-format(Sys.time(), '%Y%m%d%H%M')

  RSAPrvKeyFilename<-paste0('RSAPrvKey-DT-',fileTime,'.Rdata')
  priv.pem<-PKI.save.key(key, format = "PEM", private=TRUE)
  save(priv.pem,file=RSAPrvKeyFilename)

  RSAPubKeyFilename<-paste0('RSAPubKey-DT-',fileTime,'.Rdata')
  pub.pem<-PKI.save.key(key, format = "PEM", private=FALSE)
  save(pub.pem,file=RSAPubKeyFilename)

  res<-list()

  res$RSAPrvKeyFilename<-RSAPrvKeyFilename
  res$RSAPubKeyFilename<-RSAPubKeyFilename

  return(res)
}

#' Utility to load previously generated cryptographic key files
#'
#' @param RSAPrvKeyFilename - a file name
#' @param RSAPubKeyFilename - a file name
#' @return  a list with two encrypted key objects
#' @export
#' @examples
#' \dontrun{
#' Fill these in with the timestamped file names
#'RSAPrvKeyFilename<-my.keys$RSAPrvKeyFilename
#'RSAPubKeyFilename<-my.keys$RSAPubKeyFilename
#'
# Load the keys into R objects
#'
#' key.ring<-LoadKeys(RSAPrvKeyFilename,RSAPubKeyFilename)
#' private.key<-key.ring$priv.k
#' public.key<-key.ring$pub.k
#' }
#'
LoadKeys<-function(RSAPrvKeyFilename,RSAPubKeyFilename)
{
  # This loads an R object called priv.pem
  load(RSAPrvKeyFilename)

  # This loads an R object called pub.pem
  load(RSAPubKeyFilename)

  # load back the public key separately
  priv.k <- PKI.load.key(priv.pem)
  pub.k <- PKI.load.key(pub.pem)

  key.ring<-list()
  key.ring$priv.k<-priv.k
  key.ring$pub.k<-pub.k

  return(key.ring)
}

#' Utility to encrypt a clear text with a public key
#' from previously generated cryptographic key files
#'
#' @param clear.pw - the clear text
#' @param pub.k - a public encrypted key
#' @param pwRoot ='SSpw'- the root you want for the encrypted file name
#' @return  the encrypted.pw.filename
#' @export
#' @examples
#' \dontrun{
#' Fill these in with the timestamped file names
#'RSAPrvKeyFilename<-my.keys$RSAPrvKeyFilename
#'RSAPubKeyFilename<-my.keys$RSAPubKeyFilename
#'
# Load the keys into R objects
#'
#' key.ring<-LoadKeys(RSAPrvKeyFilename,RSAPubKeyFilename)
#'
#' clear.pw<-"A secret password"
#'
#' encrypted.pw.filename<-EncryptAndSave(clear.pw,key.ring$pub.k)
#' }
#'
EncryptAndSave<-function(clear.pw,pub.k,pwRoot='SSpw')
{
  raw.clear.pw <- charToRaw(clear.pw)

  # encrypt with the public key
  encrypted.pw <- PKI.encrypt(raw.clear.pw, pub.k)
  # write to a binary file

  fileTime<-format(Sys.time(), '%Y%m%d%H%M')
  encrypted.pw.filename<-paste0(pwRoot,'-DT-',fileTime,'.bin')
  writeBin(encrypted.pw,encrypted.pw.filename)

  return(encrypted.pw.filename)
}

#' Utility to get the clear text from an encrypted file
#'
#' @param clear.pw - the clear text
#' @param priv.k - a private encrypted key
#' @return  the clear text
#' @export
#' @examples
#' \dontrun{
#' Fill these in with the timestamped file names
#'
#' encrypted.pw.filename<-'SSpw-DT-201607131414.bin'
#' clear.text<-GetClearText(encrypted.pw.filename,key.ring$priv.k)
#' clear.text
#' }
#'
GetClearText<-function(encrypted.pw.filename,priv.k)
{
  to.read = file(encrypted.pw.filename, "rb")
  encrypted.pw.from.file<-readBin(to.read,raw(),n=5000)
  close(to.read)

  clear.text<-rawToChar(PKI.decrypt(encrypted.pw.from.file,priv.k))

  return(clear.text)
}


#' Utility to make a ggplot pie chart
#'
#' @param type - a character vector of pie segments names
#' @param percent - a numeric vector of pie segments values (make them add to 100)
#' @param type.title.str = 'Title String Here' - a title for the plot
#' @param segment.name = 'Type' - the legend title
#' @param segment.cols = NULL - used to control colours of the segments
#' @param segment.breaks = NULL - used to control colours of the segments
#' @return  a ggplot object with your pie chart
#' @export
#' @examples
#' \dontrun{
#' # Get these two vectors in the order you need and make sure
#' # the percent add up to 100
#' type<-c('Comp1','Comp2','Comp3','Other')
#' percent<-c(65,20,10,5)
#'
#' #Set up the pie chart characteristics
#' pie.trace.cols<-c('Comp1'='cornflowerblue','Comp2'='forestgreen','Comp3'='orange','Other'='indianred2')
#' pie.breaks<-c('Comp1','Comp2','Comp3','Other')
#' pie.segment.name<-'Variation Type'
#' type.title.str<-'Three Variation Components'
#'
#' p<-MakePie(type,percent,type.title.str
#'            ,segment.name=pie.segment.name
#'            ,segment.cols=pie.trace.cols
#'            ,segment.breaks=pie.breaks)
#' print(p)
#' }
#'
MakePie<-function(type,percent,type.title.str='Title String Here',segment.name='Type',segment.cols=NULL,segment.breaks=NULL)
{
  # Pie charts, looks tricky but just modify the type and percent lines below
  # You also might need to mess with the geom_text(colour = ???) if you change
  # the colours in the scale_fill_manual() line

  df.pie<-data_frame(type,percent)
  df.pie$pos<-cumsum(percent) - (0.5 * percent)

  p<-ggplot(data=df.pie,aes(x=factor(1),y=percent,fill=type))
  p<-p+geom_bar(stat = 'identity',width = 1)
  p<-p+geom_text(aes(y=pos,label = paste0(percent,"%")), colour = 'black', size=4)

  p<-p+coord_polar(theta="y")
  p<-p+theme(axis.text = element_blank()
             ,axis.ticks = element_blank()
             ,panel.grid  = element_blank())

  p<-p+labs(title = type.title.str, x='',y='')

  if(is.null(segment.cols) == FALSE)
  {
    p<-p + scale_fill_manual(name=segment.name, breaks=segment.breaks,values = segment.cols )
  }

  return(p)
}

#' Wrapper function for plotting ggplot2 PDFs
#' @param plot.filename - the filename for the plot
#' @param plot.obj - a ggplot2 object
#' @param page.size='A4' - used to specify page size, can be A3
#' @param landscape=TRUE - used to specify page orientation
#' @export
#' @examples
#' \dontrun{

#'
#'x.vals<-c(1,2,3)
#'y.vals<-c(2,4,6)
#'plot.data<-data_frame(x.vals,y.vals)
#'p<-ggplot(data = plot.data,aes(x=x.vals,y=y.vals))
#'p<-p+geom_line(size=0.5)
#'p<-p+geom_point(size=2)
#'titleStr<-'A simple plot'
#'p<-p+labs(title = titleStr)
#'
#'print(p)
#'
#'file.name<-'A good file name.pdf'
#'ProducePlot(file.name,plot.obj=p)
#' }
ProducePlot<-function(filename,plot.obj,page.size='A4',landscape=TRUE)
{
  suppressMessages(
    {suppressWarnings({
      PlotPDF(plot.filename = filename,page.size=page.size,landscape=landscape)
      print(plot.obj)
      dev.off()
    })
    })

  msg<-paste0('PDF plot written to: ',filename)
  return(msg)
}

# internal function
removeFileExtension<-function(filename)
{
  last.dot.posn<-regexpr("\\.[^\\.]*$", filename)
  return(substr(filename,1,last.dot.posn-1))
}
