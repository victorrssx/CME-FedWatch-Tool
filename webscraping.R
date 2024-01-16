

 ## 1ª Abordagem ----

  install.packages("KeyboardSimulator")
  library("KeyboardSimulator")

 
 keybd.type_string("https://www.cmegroup.com/markets/interest-rates/cme-fedwatch-tool.html")
 
 
 browseURL("https://www.cmegroup.com/markets/interest-rates/cme-fedwatch-tool.html", browser = getOption("browser"),
           encodeIfNeeded = FALSE)
 Sys.sleep(5)
 mouse.move(1355, 718)
 replicate(10, mouse.click(button = "left"), simplify = FALSE)
 
 
 mouse.move(192, 275)
 Sys.sleep(1)
 mouse.move(184, 275)
 mouse.click(button = "left")
 Sys.sleep(2)
 
 mouse.move(377, 342)
 mouse.click(button = "right")
 Sys.sleep(2)
 
 mouse.move(462, 523)
 mouse.click(button = "left")

 
 ## 2ª Abordagem ----
 
 install.packages("RSelenium")
 library(RSelenium)
 

 ## 3ª Abordagem ----
 
 library(rvest)
 
 x = read.csv("https://cmegroup-tools.quikstrike.net/User/Export/FedWatch/AllMeetings.aspx?insid=107151757&qsid=eb6822bc-3d4f-47e5-ab68-89938a22e323.csv")
 x1 = read.csv("https://cmegroup-tools.quikstrike.net/User/Export/FedWatch/AllMeetings.aspx?insid=107234268&qsid=bd43f0ed-3dc9-499f-b576-4ff439857a72")
 
 url = "https://www.cmegroup.com/markets/interest-rates/cme-fedwatch-tool.html"
 pg = read_html(url)
 html_elements(pg, "a id = center")
 
 
# <a id="ctl00_MainContent_ucViewControl_IntegratedFedWatchTool_ucd_hlTestAllMeetings" title="Download history for all upcoming Federal Reserve meetings." href="Export/FedWatch/AllMeetings.aspx?insid=107151757&amp;qsid=eb6822bc-3d4f-47e5-ab68-89938a22e323" target="_blank">            
  # <div class="center"><img id="ctl00_MainContent_ucViewControl_IntegratedFedWatchTool_ucd_imgExcel" src="../Images/Icons/Excel256.png" style="width:32px;"></div>
   #<div class="center" style="margin-top:0.5em;">All upcoming<br>meetings</div>
  # </a>
   
   
   
   html = minimal_html("
                        <h1>This is a heading</h1>
                        <p id='first'>This is a paragraph</p>
                        <p class='important'>This is an important paragraph</p>
                       ")

   html %>% html_element("h1")
   html %>% html_elements("p")
   html %>% html_elements(".important")
   html %>% html_elements("#first")
 