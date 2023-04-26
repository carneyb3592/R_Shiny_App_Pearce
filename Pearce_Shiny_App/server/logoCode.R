library(hexSticker)
library(ggplot2)
imgurl <- file.path("rankrate.png")
sticker(imgurl, package="rankrate",
        s_x=1,s_y=1.15,s_width=.85,
        p_x=1,p_y=.42,p_size=20,p_color="white",
        h_fill="black",h_color="gold",
        filename="~/R_Shiny_App_Pearce/Pearce_Shiny_App/ui/rankrate_logo.png")
