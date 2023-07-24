make_spreadsheet <- function(p, r, m, h_list, fontsize) {

  # Make m an integer
  m = round(m,0)

  #Instructions
  row = list(
    randomise_blocks = NA,
    randomise_trials = NA,
    display = "Instructions",
    L1H = NA,
    L1M = NA,
    L1L = NA,
    L1PrH = NA,
    L1PrM = NA,
    L1PrL = NA,
    L2H = NA,
    L2M = NA,
    L2L = NA,
    L2PrH = NA,
    L2PrM = NA,
    L2PrL = NA
  )
  spreadsheet <- data.frame(row)

  #Practice
  row = data.frame(list(
    randomise_blocks = NA,
    randomise_trials = 1,
    display = "Practice",
    L1H = "",
    L1M = "$10",
    L1L = "",
    L1PrH = "",
    L1PrM = "100%",
    L1PrL = "",
    L2H = "$20",
    L2M = "$10",
    L2L = "$0",
    L2PrH = "25%",
    L2PrM = "50%",
    L2PrL = "25%"
  ))
  spreadsheet = rbind(spreadsheet, row)
  row = data.frame(list(
    randomise_blocks = NA,
    randomise_trials = 1,
    display = "Practice",
    L1H = "$25",
    L1M = "$20",
    L1L = "$0",
    L1PrH = "20%",
    L1PrM = "50%",
    L1PrL = "30%",
    L2H = "",
    L2M = "$10",
    L2L = "",
    L2PrH = "",
    L2PrM = "100%",
    L2PrL = ""
  ))
  spreadsheet = rbind(spreadsheet, row)
  row = data.frame(list(
    randomise_blocks = NA,
    randomise_trials = 1,
    display = "Practice",
    L1H = "",
    L1M = "$10",
    L1L = "",
    L1PrH = "",
    L1PrM = "100%",
    L1PrL = "",
    L2H = "$30",
    L2M = "$10",
    L2L = "$0",
    L2PrH = "10%",
    L2PrM = "20%",
    L2PrL = "70%"
  ))
  spreadsheet = rbind(spreadsheet, row)

  #Start
  row = data.frame(list(
    randomise_blocks = NA,
    randomise_trials = NA,
    display = "StartExperiment",
    L1H = NA,
    L1M = NA,
    L1L = NA,
    L1PrH = NA,
    L1PrM = NA,
    L1PrL = NA,
    L2H = NA,
    L2M = NA,
    L2L = NA,
    L2PrH = NA,
    L2PrM = NA,
    L2PrL = NA
  ))
  spreadsheet = rbind(spreadsheet, row)

  #Trials Block 1
  for (i in c(1:length(h_list))) {
    a = runif(1,0,1)
    if (a>.5) {
      L1H = ""
      L1PrH = ""
      L1M = paste0("$", toString(m))
      L1PrM = "100%"
      L1L = ""
      L1PrL = ""
      L2H = paste0("$", toString(h_list[i]))
      L2PrH = paste0(toString(p*r*100), "%")
      L2M = paste0("$", toString(m))
      L2PrM = paste0(toString((1-r)*100), "%")
      L2L = "$0"
      L2PrL = paste0(toString(r*(1-p)*100), "%")
    } else {
      L1H = paste0("$", toString(h_list[i]))
      L1PrH = paste0(toString(p*r*100), "%")
      L1M = paste0("$", toString(m))
      L1PrM = paste0(toString((1-r)*100), "%")
      L1L = "$0"
      L1PrL = paste0(toString(r*(1-p)*100), "%")
      L2H = ""
      L2PrH = ""
      L2M = paste0("$", toString(m))
      L2PrM = "100%"
      L2L = ""
      L2PrL = ""
    }

    row = data.frame(list(
      randomise_blocks = NA,
      randomise_trials = 2,
      display = "Trial",
      L1H = L1H,
      L1M = L1M,
      L1L = L1L,
      L1PrH = L1PrH,
      L1PrM = L1PrM,
      L1PrL = L1PrL,
      L2H = L2H,
      L2M = L2M,
      L2L = L2L,
      L2PrH = L2PrH,
      L2PrM = L2PrM,
      L2PrL = L2PrL
    ))
    spreadsheet = rbind(spreadsheet, row)
  }

  #Block
  row = data.frame(list(
    randomise_blocks = NA,
    randomise_trials = NA,
    display = "BlockStart",
    L1H = NA,
    L1M = NA,
    L1L = NA,
    L1PrH = NA,
    L1PrM = NA,
    L1PrL = NA,
    L2H = NA,
    L2M = NA,
    L2L = NA,
    L2PrH = NA,
    L2PrM = NA,
    L2PrL = NA
  ))
  spreadsheet = rbind(spreadsheet, row)

  #Trials Block 2
  for (i in c(1:length(h_list))) {
    a = runif(1,0,1)
    if (a>.5) {
      L1H = ""
      L1PrH = ""
      L1M = paste0("$", toString(m))
      L1PrM = paste0(toString(r*100), "%")
      L1L = "$0"
      L1PrL = paste0(toString((1-r)*100), "%")
      L2H = paste0("$", toString(h_list[i]))
      L2PrH = paste0(toString(p*r*100), "%")
      L2M = ""
      L2PrM = ""
      L2L = "$0"
      L2PrL = paste0(toString((1-p*r)*100), "%")
    } else {
      L1H = paste0("$", toString(h_list[i]))
      L1PrH = paste0(toString(p*r*100), "%")
      L1M = ""
      L1PrM = ""
      L1L = "$0"
      L1PrL = paste0(toString((1-p*r)*100), "%")
      L2H = ""
      L2PrH = ""
      L2M = paste0("$", toString(m))
      L2PrM = paste0(toString(r*100), "%")
      L2L = "$0"
      L2PrL = paste0(toString((1-r)*100), "%")
    }

    row = data.frame(list(
      randomise_blocks = NA,
      randomise_trials = 3,
      display = "Trial",
      L1H = L1H,
      L1M = L1M,
      L1L = L1L,
      L1PrH = L1PrH,
      L1PrM = L1PrM,
      L1PrL = L1PrL,
      L2H = L2H,
      L2M = L2M,
      L2L = L2L,
      L2PrH = L2PrH,
      L2PrM = L2PrM,
      L2PrL = L2PrL
    ))
    spreadsheet = rbind(spreadsheet, row)
  }

  #End
  row = data.frame(list(
    randomise_blocks = NA,
    randomise_trials = NA,
    display = "End",
    L1H = NA,
    L1M = NA,
    L1L = NA,
    L1PrH = NA,
    L1PrM = NA,
    L1PrL = NA,
    L2H = NA,
    L2M = NA,
    L2L = NA,
    L2PrH = NA,
    L2PrM = NA,
    L2PrL = NA
  ))
  spreadsheet = rbind(spreadsheet, row)

  #Fontsize
  pretext = paste0('<h3 style="font-size: ', toString(fontsize), 'px; color: white;">')
  spreadsheet$L1H_text   = paste0(pretext, spreadsheet$L1H, '<h3/>')
  spreadsheet$L1M_text   = paste0(pretext, spreadsheet$L1M, '<h3/>')
  spreadsheet$L1L_text   = paste0(pretext, spreadsheet$L1L, '<h3/>')
  spreadsheet$L1PrH_text = paste0(pretext, spreadsheet$L1PrH, '<h3/>')
  spreadsheet$L1PrM_text = paste0(pretext, spreadsheet$L1PrM, '<h3/>')
  spreadsheet$L1PrL_text = paste0(pretext, spreadsheet$L1PrL, '<h3/>')
  spreadsheet$L2H_text   = paste0(pretext, spreadsheet$L2H, '<h3/>')
  spreadsheet$L2M_text   = paste0(pretext, spreadsheet$L2M, '<h3/>')
  spreadsheet$L2L_text   = paste0(pretext, spreadsheet$L2L, '<h3/>')
  spreadsheet$L2PrH_text = paste0(pretext, spreadsheet$L2PrH, '<h3/>')
  spreadsheet$L2PrM_text = paste0(pretext, spreadsheet$L2PrM, '<h3/>')
  spreadsheet$L2PrL_text = paste0(pretext, spreadsheet$L2PrL, '<h3/>')

  #Return
  return(spreadsheet)
}


outdir = "D:/OneDrive - California Institute of Technology/PhD/Rangel Lab/2023-common-consequence/experiment/code"
fontsize = 40
m = 30
h_list = seq(5,100,5)
r_list = c(.3, .7)
p_list = c(.3, .5, .7)

for (r in r_list) {
  for (p in p_list) {
    spreadsheet = make_spreadsheet(p=p, r=r, m=m, h_list=h_list, fontsize=fontsize)
    filename = paste0("spreadsheet", toString(r*10), toString(p*10), ".csv")
    write.csv(spreadsheet, file=file.path(outdir,filename), na="", row.names=F)
  }
}