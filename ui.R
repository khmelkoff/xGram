# Define UI for application that draws a histogram
shinyUI(fluidPage(
    div(
    # Application title 
    fluidRow(
        column(width = 1),
        column(width = 10,
               h3("xGram")#,
               #hr()
        )
    ),
    fluidRow(
        column(width = 1),
        column(width = 10,
            uiOutput("box"),
            hr()
        )
    ),
    fluidRow(
        column(width = 1),
        column(width = 10,
               uiOutput("ui"),
               hr()
        )
    ), 
    fluidRow(
           column(width = 1),
           column(width = 10,
                  p(
                  actionButton("qButton", "q"),
                  actionButton("wButton", "w"),
                  actionButton("eButton", "e"),
                  actionButton("rButton", "r"),
                  actionButton("tButton", "t"),
                  actionButton("yButton", "y"),
                  actionButton("uButton", "u"),
                  actionButton("iButton", "i"),
                  actionButton("oButton", "o"),
                  actionButton("pButton", "p")
                  ),
                  p(
                  actionButton("aButton", "a"),
                  actionButton("sButton", "s"),
                  actionButton("dButton", "d"),
                  actionButton("fButton", "f"),
                  actionButton("gButton", "g"),
                  actionButton("hButton", "h"),
                  actionButton("jButton", "j"),
                  actionButton("kButton", "k"),
                  actionButton("lButton", "l")
                  ),
                  p(
                  actionButton("zButton", "z"),
                  actionButton("xButton", "x"),
                  actionButton("cButton", "c"),
                  actionButton("vButton", "v"),
                  actionButton("bButton", "b"),
                  actionButton("nButton", "n"),
                  actionButton("mButton", "m"),
                  actionButton("backButton", HTML("&#8592;"))
                  ),
                  p(
                  actionButton("apoButton", "'"),
                  actionButton("spaceButton", HTML("space/submit")),
                  actionButton("dotButton", "."),
                  actionButton("clearButton", "clear")
                  ),
                  HTML("<small><small>xGram 0.6.2, khmelkoff &copy; 2015. Source code: 
                       <a href=https://github.com/khmelkoff/xGram>https://github.com/khmelkoff/xGram</a><br>
                       Slide Deck: <a href=http://rpubs.com/khmelkoff/100726>http://rpubs.com/khmelkoff/100726</a>
                       </small></small>")
           )
        ),
    align="center")
))