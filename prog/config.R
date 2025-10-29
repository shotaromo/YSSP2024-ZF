
# plot theme --------------------------------------------------------------

plot_theme_white <- theme_bw() +
  theme(
    panel.border=element_blank(),
    plot.background=element_rect(fill='white',colour=NA),
    panel.grid.minor = element_line(color = NA), 
    axis.line=element_line(colour='black',linewidth=0.25),
    # axis.line=element_blank(),
    panel.background=element_rect(fill='white'),
    panel.grid.major=element_line(colour=NA),
    strip.background=element_rect(fill=NA, colour=NA),
    strip.text.x = element_text(size=6, colour = 'black', angle = 0,face='plain',margin=margin(1,0,2,0)),
    strip.text.y = element_text(size=6, colour = 'black', angle = 270,face='plain',margin=margin(0,1,0,2)),
    axis.text.x=element_text(size=6,angle=45, vjust=1, hjust=1, margin = margin(t =3 , r = 0, b = 0, l = 0)),
    axis.text.y=element_text(size=6,margin = margin(t = 0, r =3, b = 0, l = 0)),
    axis.title.x = element_text(size=6),
    axis.title.y = element_text(size=6),
    legend.text = element_text(size=6),
    legend.title = element_blank(),
    legend.key.height = unit(8.5,'pt'),
    legend.key.width = unit(7,'pt'),
    axis.ticks=element_line(colour='black',linewidth=0.25),
    axis.ticks.length=unit(-0.075,'cm'),
    axis.text = element_blank(),
    plot.title=element_text(size=6),
    plot.tag=element_text(size=7,face='bold'),
    plot.margin = margin(1,1,1,1)
  )

plot_theme_white_rect <- theme_bw() +
  theme(
    panel.border=element_rect(color='black',linewidth=0.25,fill=NA),
    plot.background=element_rect(fill='white',colour=NA),
    panel.grid.minor = element_line(color = NA), 
    axis.line=element_line(colour='black',linewidth=0.25),
    panel.background=element_rect(fill='white'),
    panel.grid.major=element_line(colour=NA),
    strip.background=element_rect(fill=NA, colour=NA),
    strip.text.x = element_text(size=5.5, colour = 'black', angle = 0,face='plain',margin=margin(1,0,2,0)),
    strip.text.y = element_text(size=5.5, colour = 'black', angle = 270,face='plain',margin=margin(0,1,0,2)),
    axis.text.x=element_text(size=5.5,angle=0, margin = margin(t =3 , r = 0, b = 0, l = 0)),
    axis.text.y=element_text(size=5.5,margin = margin(t = 0, r =1, b = 0, l = 0)),
    axis.title.x = element_text(size=5.5),
    axis.title.y = element_text(size=5.5),
    legend.text = element_text(size=5.5),
    legend.title = element_blank(),
    legend.key.height = unit(8.5,'pt'),
    legend.key.width = unit(3,'pt'),
    axis.ticks=element_line(colour='black',linewidth=0.25),
    axis.ticks.length=unit(-0.075,'cm'),
    axis.text = element_blank(),
    plot.title=element_text(size=5.5,hjust=0,vjust=-3),
    plot.tag=element_text(size=7,face='bold'),
    plot.tag.position = c(-.03,0.85),
    legend.position='none',
    plot.margin=margin(0,-5,-5,20)
  )

plot_theme_ternary <-  theme_bw() +
  theme(
  panel.border=element_blank(),
  plot.background=element_rect(fill='white',colour=NA),
  panel.grid.major=element_line(colour='grey20',linewidth=0.13,linetype='dotted'),
  panel.grid.minor=element_line(colour='grey20',linewidth=0.13,linetype='dotted'),
  axis.line=element_line(colour='black',linewidth=0.25),
  panel.background=element_rect(fill='white'),
  strip.background=element_rect(fill=NA, colour=NA),
  strip.text.x = element_text(size=6, colour = 'black', angle = 0,face='plain',margin=margin(1,0,2,0)),
  strip.text.y = element_text(size=6, colour = 'black', angle = 270,face='plain',margin=margin(0,1,0,2)),
  axis.text.x=element_text(size=6,angle=45, vjust=1, hjust=1, margin = margin(t =3 , r = 0, b = 0, l = 0)),
  axis.text.y=element_text(size=6,margin = margin(t = 0, r =3, b = 0, l = 0)),
  axis.title.x = element_text(size=6),
  axis.title.y = element_text(size=6),
  legend.text = element_text(size=6),
  legend.title = element_blank(),
  legend.key.height = unit(8.5,'pt'),
  legend.key.width = unit(7,'pt'),
  axis.ticks=element_line(colour='black',linewidth=0.25),
  plot.title=element_blank(),
  plot.tag=element_text(size=7,face='bold'),
  tern.axis.text=element_text(size=6,margin=margin(0,0,0,0),colour='black'),
  tern.axis.title=element_text(margin=margin(0,0,0,0)),
  tern.axis.arrow=element_line(color=NA,linewidth=0.75,arrow=arrow(length=unit(0,'cm'))),
  tern.axis.ticks=element_line(colour='black',linewidth=0.75),
  tern.axis.ticks.length.major=unit(0.2,'cm'),
  tern.axis.ticks.length.minor=unit(0,'cm'),
  tern.axis.title.show=FALSE,
  legend.position='none',
  plot.margin = margin(1,1,1,1)
)


# plot function -----------------------------------------------------------

set_plot <- function(var){
  plt <- list()
  plt$Color <- as.character(var$Color); names(plt$Color) <- as.character(var$Variable)
  plt$Legend <- as.character(var$Legend); names(plt$Legend) <- as.character(var$Variable)
  return(plt)
}

bezier_tern <- function(p0, pc, p1, n = 100, colnames){
  t <- seq(0, 1, length.out = n)
  df <- data.frame(
    (1 - t)^2 * p0[1] + 2*(1 - t)*t*pc[1] + t^2*p1[1],
    (1 - t)^2 * p0[2] + 2*(1 - t)*t*pc[2] + t^2*p1[2],
    (1 - t)^2 * p0[3] + 2*(1 - t)*t*pc[3] + t^2*p1[3],
    check.names = FALSE
  )
  names(df) <- colnames
  return(df)
}

# variables ---------------------------------------------------------------

var_fos <- tribble(
  ~Variable,~Legend,~Color,
  'Primary Energy|Coal|w/o CCS','Coal w/o CCS','grey60',
  'Primary Energy|Coal|w/ CCS','Coal w/ CCS','grey30',
  'Primary Energy|Oil|w/o CCS','Oil w/o CCS','sandybrown',
  'Primary Energy|Oil|w/ CCS','Oil w/ CCS','tan3',
  'Primary Energy|Gas|w/o CCS','Gas w/o CCS','lightgoldenrod',
  'Primary Energy|Gas|w/ CCS','Gas w/ CCS','lightgoldenrod3',
)

var_prm <- tribble(
  ~Variable,~Legend,~Color,
  'Primary Energy|Coal|w/o CCS','Coal w/o CCS','grey60',
  'Primary Energy|Coal|w/ CCS','Coal w/ CCS','grey30',
  'Primary Energy|Oil|w/o CCS','Oil w/o CCS','sandybrown',
  'Primary Energy|Oil|w/ CCS','Oil w/ CCS','tan3',
  'Primary Energy|Gas|w/o CCS','Gas w/o CCS','lightgoldenrod',
  'Primary Energy|Gas|w/ CCS','Gas w/ CCS','lightgoldenrod3'
)

var_ele <- tribble(
  ~Variable,~Legend,~Color,
  'Secondary Energy|Electricity|Fossil','Fossil','grey60',
  'Secondary Energy|Electricity|Nuclear','Nuclear','moccasin',
  'Secondary Energy|Electricity|Hydro','Hydro','lightsteelblue',
  'Secondary Energy|Electricity|Biomass','Biomass','darkolivegreen2',
  'Secondary Energy|Electricity|Geothermal','Geothermal','peru',
  'Secondary Energy|Electricity|Solar','Solar','lightsalmon',
  'Secondary Energy|Electricity|Wind','Wind','lightskyblue3'
)

var_ele2 <- tribble(
  ~Variable,~Legend,~Color,
  'Secondary Energy|Electricity|Coal|w/o CCS','Coal w/o CCS','grey60',
  'Secondary Energy|Electricity|Coal|w/ CCS','Coal w/ CCS','grey30',
  'Secondary Energy|Electricity|Oil|w/o CCS','Oil w/o CCS','sandybrown',
  'Secondary Energy|Electricity|Oil|w/ CCS','Oil w/ CCS','tan3',
  'Secondary Energy|Electricity|Gas|w/o CCS','Gas w/o CCS','lightgoldenrod',
  'Secondary Energy|Electricity|Gas|w/ CCS','Gas w/ CCS','lightgoldenrod3',
  'Secondary Energy|Electricity|Nuclear','Nuclear','moccasin',
  'Secondary Energy|Electricity|Hydro','Hydro','lightsteelblue',
  'Secondary Energy|Electricity|Biomass|w/o CCS','Biomass w/o CCS','darkolivegreen2',
  'Secondary Energy|Electricity|Biomass|w/ CCS','Biomass w/ CCS','darkolivegreen4',
  'Secondary Energy|Electricity|Geothermal','Geothermal','peru',
  'Secondary Energy|Electricity|Solar','Solar','lightsalmon',
  'Secondary Energy|Electricity|Wind','Wind','lightskyblue3'
)

var_hyd <- tribble(
  ~Variable,~Legend,~Color,
  'Secondary Energy|Hydrogen|Fossil','Fossil','grey50',
  'Secondary Energy|Hydrogen|Biomass','Biomass','darkolivegreen2',
  'Secondary Energy|Hydrogen|Electricity','Electricity','lightsteelblue'
)

var_hyd2 <- tribble(
  ~Variable,~Legend,~Color,
  'Secondary Energy|Hydrogen|Coal|w/o CCS','Coal w/o CCS','grey60',
  'Secondary Energy|Hydrogen|Coal|w/ CCS','Coal w/ CCS','grey30',
  'Secondary Energy|Hydrogen|Gas|w/o CCS','Gas w/o CCS','lightgoldenrod',
  'Secondary Energy|Hydrogen|Gas|w/ CCS','Gas w/ CCS','lightgoldenrod3',
  'Secondary Energy|Hydrogen|Biomass|w/o CCS','Biomass w/o CCS','darkolivegreen2',
  'Secondary Energy|Hydrogen|Biomass|w/ CCS','Biomass w/ CCS','darkolivegreen4',
  'Secondary Energy|Hydrogen|Electricity','Electricity','lightsteelblue'
)

var_fin <- tribble(
  ~Variable,~Legend,~Color,
  'Final Energy|Solids|Coal','Coal','grey60',
  'Final Energy|Liquids|Fossil','Liquids-fossil','sandybrown',
  'Final Energy|Gases|Fossil','Gases-fossil','lightgoldenrod2',
  'Final Energy|Biomass','Biomass','darkolivegreen2',
  'Final Energy|Solar','Solar','lightsalmon',
  'Final Energy|Electricity','Electricity','lightsteelblue',
  'Final Energy|Heat','Heat','salmon',
  'Final Energy|Hydrogen','Hydrogen','thistle2',
  'Final Energy|Liquids|Hydrogen synfuel','Liquids-synfuel','orchid',
  'Final Energy|Gases|Hydrogen synfuel','Gases-synfuel','orchid1'
)

var_fin_agg <- tribble(
  ~Variable,~Legend,~Color,
  'Final Energy|Solids|Coal','Coal','grey60',
  'Final Energy|Liquids|Fossil','Liquids-fossil','sandybrown',
  'Final Energy|Gases|Fossil','Gases-fossil','lightgoldenrod2',
  'Final Energy|Biomass','Biomass','darkolivegreen2',
  'Final Energy|Solar','Solar','lightsalmon',
  'Final Energy|Electricity','Electricity','lightsteelblue',
  'Final Energy|Heat','Heat','salmon',
  'Final Energy|Hydrogen','Hydrogen','thistle2',
  'Final Energy|Hydrogen synfuel','Synfuel','orchid'
)

var_emi <- tribble(
  ~Variable,~Legend,~Color,
  'Emissions|CO2|Industrial Processes','Industrial process','grey50',
  'Emissions|CO2|Energy|Demand|Other','Other energy demand','sandybrown',
  'Emissions|CO2|Energy|Demand|Industry','Industry','salmon',
  'Emissions|CO2|Energy|Demand|Residential and Commercial','Buildings','lightskyblue3',
  'Emissions|CO2|Energy|Demand|Transportation','Transport','darkolivegreen2',
  'Emissions|CO2|Energy|Supply|Other','Energy supply (pos.)','moccasin',
  'Emissions|CO2|BECCS','Energy supply (neg.)','thistle2',
  'Emissions|CO2|DACCS','DACCS','orchid',
  'Emissions|CO2|AFOLU','AFOLU','aquamarine3'
)
