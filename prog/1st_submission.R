
# load packages -----------------------------------------------------------

library(tidyverse)
library(imputeTS)
library(ggtern)
library(ggalluvial)
library(patchwork)
library(cowplot)
library(lemon)


# plot theme --------------------------------------------------------------

source('prog/config.R')


# import data -------------------------------------------------------------

# load scenario data
df_load <- read_csv('data/scenario_data.csv') %>% 
  pivot_longer(cols=-c(Model,Region,Scenario,Variable,Unit),
               names_to='Year',values_to='value',names_transform=as.numeric) %>% 
  mutate(Model=recode(Model,'AIM-Technology'='AIM','MESSAGEix-GLOBIOM'='MESSAGEix')) %>% 
  drop_na(value)

# load AR6 scenario metadata
ls_Category <- read_xlsx(paste0('data/AR6_Scenarios_Database_metadata_indicators_v1.1.xlsx'),'meta_Ch3vetted_withclimate') %>% 
  transmute(Case=paste0(Model,'_',Scenario),Model,Scenario,Category)

# load AR6 scenario data
# ! please download AR6 Scenarios Database world v1.1 & place it in the data directory
df_load_AR6 <- read_csv(paste0('data/AR6_Scenarios_Database_World_v1.1.csv')) %>% 
  mutate(Case=paste0(Model,'_',Scenario)) %>% 
  full_join(ls_Category) %>% 
  mutate(Case=paste0(Model,'-',Scenario)) %>% 
  filter(Category%in%c('C1','C2')) %>% 
  select(-Unit,-Model,-Scenario) %>%
  pivot_longer(cols=-c(Case,Category,Region,Variable),
               names_to='Year',values_to='value',names_transform=as.numeric)


# fig. 1a -----------------------------------------------------------------

df_fig_1a <- df_load %>% 
  filter(Variable%in%c('Primary Energy|Coal',
                       'Primary Energy|Oil',
                       'Primary Energy|Gas',
                       'Primary Energy|Non-Biomass Renewables',
                       'Primary Energy|Nuclear',
                       'Primary Energy|Biomass')) %>% 
  mutate(Variable=case_when(Variable%in%c('Primary Energy|Coal',
                                          'Primary Energy|Oil',
                                          'Primary Energy|Gas')~'Fossil',
                            Variable%in%c('Primary Energy|Biomass')~'Biomass',
                            Variable%in%c('Primary Energy|Nuclear','Primary Energy|Non-Biomass Renewables')~'Nuclear + Non-biomass renewables')) %>% 
  group_by(Model,Scenario,Region,Variable,Year) %>% 
  reframe(value=sum(value)) %>% 
  pivot_wider(names_from=Variable,values_from=value) %>% 
  mutate(Total=`Fossil`+`Biomass`+`Nuclear + Non-biomass renewables`) %>% 
  group_by(Model,Scenario,Region,Year) %>% 
  mutate(across(c(`Fossil`,`Biomass`,`Nuclear + Non-biomass renewables`),~./sum(`Fossil`,`Biomass`,`Nuclear + Non-biomass renewables`)))

df_fig_1a_AR6 <- df_load_AR6 %>% 
  filter(Category%in%c('C1','C2'),Year%in%c(2030,2050,2070,2100),
         Variable%in%c('Primary Energy|Coal',
                       'Primary Energy|Oil',
                       'Primary Energy|Gas',
                       'Primary Energy|Non-Biomass Renewables',
                       'Primary Energy|Nuclear',
                       'Primary Energy|Biomass')) %>%
  mutate(Variable=case_when(Variable%in%c('Primary Energy|Coal',
                                          'Primary Energy|Oil',
                                          'Primary Energy|Gas')~'Fossil',
                            Variable%in%c('Primary Energy|Biomass')~'Biomass',
                            Variable%in%c('Primary Energy|Nuclear','Primary Energy|Non-Biomass Renewables')~'Nuclear + Non-biomass renewables')) %>% 
  group_by(Region,Variable,Case,Category,Year) %>% 
  reframe(value=sum(value)) %>% 
  pivot_wider(names_from=Variable,values_from=value) %>% 
  drop_na() %>% 
  mutate(Total=`Fossil`+`Biomass`+`Nuclear + Non-biomass renewables`) %>% 
  group_by(Case,Region,Year) %>% 
  mutate(across(c(`Fossil`,`Biomass`,`Nuclear + Non-biomass renewables`),~./sum(`Fossil`,`Biomass`,`Nuclear + Non-biomass renewables`)))

g_fig_1a <- df_fig_1a %>% 
  filter(Year==2050) %>% 
  mutate(Scenario=factor(Scenario,levels=c('Opt1.5C','ZF2100','ZF2090','ZF2080','ZF2070','ZF2060','ZF2050'))) %>% 
  arrange(Model,Scenario) %>%
  ggtern(aes(Fossil,`Biomass`,`Nuclear + Non-biomass renewables`)) +
  geom_mask() +
  geom_point(data=df_fig_1a_AR6 %>% filter(Year==2050,Category=='C2'),color='grey70',size=0.7,shape=1,stroke=0.4) +
  geom_point(data=df_fig_1a_AR6 %>% filter(Year==2050,Category=='C1'),color='grey70',size=0.7,shape=4,stroke=0.4) +
  geom_point(aes(color=Model,shape=Scenario,size=Scenario),stroke=0.5) +
  annotate("text", x = 0.85, y = 0.075, z = 0.075, hjust=0.1,
           label = paste0('C1: n=', df_fig_1a_AR6 %>% filter(Year == 2050, Category%in%c('C1')) %>% nrow()), 
           size = 1.75) +
  annotate("text", x = 0.675, y = 0.075, z = 0.25, hjust=0.1,
           label = paste0('C2: n=', df_fig_1a_AR6 %>% filter(Year == 2050, Category%in%c('C2')) %>% nrow()), 
           size = 1.75) +
  geom_path(aes(color=Model),linewidth=0.25) +
  scale_color_manual(values=c('AIM'='#349b8d','MESSAGEix'='#ff7f0e')) +
  scale_shape_manual(values=c('Opt1.5C'=4,'ZF2100'=2,'ZF2050'=21,
                              'ZF2060'=0,'ZF2070'=8,'ZF2080'=5,'ZF2090'=3)) +
  scale_size_manual(values=c('Opt1.5C'=1.5,'ZF2100'=1.5,'ZF2050'=1.5,
                             'ZF2060'=1.5,'ZF2070'=1.5,'ZF2080'=1.5,'ZF2090'=1.5)) +
  labs(
    x='',y='',z='',
    Tarrow='Biomass(%)',
    Larrow='Fossil (%)',
    Rarrow='Nuclear & non-biomass renewables (%)'
  ) +
  plot_theme_ternary +
  theme(
    plot.margin=margin(-15,-30,-10,-30)
  ) +
  theme_showarrows() +
  theme_clockwise() +
  theme_ticksinside() 
plot(g_fig_1a)


# fig. 1b -----------------------------------------------------------------

df_fig_1b <- df_load %>% 
  filter(Variable%in%c('Secondary Energy|Electricity',
                       'Secondary Energy|Electricity|Coal',
                       'Secondary Energy|Electricity|Oil',
                       'Secondary Energy|Electricity|Gas',
                       'Secondary Energy|Electricity|Nuclear',
                       'Secondary Energy|Electricity|Biomass')) %>% 
  pivot_wider(names_from=Variable,values_from=value) %>% 
  transmute(Model,Scenario,Region,Year,
            Fossil=(`Secondary Energy|Electricity|Coal`+`Secondary Energy|Electricity|Oil`+`Secondary Energy|Electricity|Gas`)/`Secondary Energy|Electricity`,
            `Biomass`=(`Secondary Energy|Electricity|Biomass`)/`Secondary Energy|Electricity`,
            `Nuclear + Non-biomass renewables`=(`Secondary Energy|Electricity`-(`Secondary Energy|Electricity|Coal`+`Secondary Energy|Electricity|Oil`+`Secondary Energy|Electricity|Gas`+`Secondary Energy|Electricity|Biomass`))/`Secondary Energy|Electricity`)

df_fig_1b_AR6 <- df_load_AR6 %>% 
  filter(Category%in%c('C1','C2'),Year%in%c(2030,2050,2070,2100),
         Variable%in%c('Secondary Energy|Electricity',
                       'Secondary Energy|Electricity|Coal',
                       'Secondary Energy|Electricity|Oil',
                       'Secondary Energy|Electricity|Gas',
                       'Secondary Energy|Electricity|Nuclear',
                       'Secondary Energy|Electricity|Biomass')) %>%
  pivot_wider(names_from=Variable,values_from=value) %>% 
  drop_na() %>% 
  transmute(Region,Case,Category,Year,
            Fossil=(`Secondary Energy|Electricity|Coal`+`Secondary Energy|Electricity|Oil`+`Secondary Energy|Electricity|Gas`)/`Secondary Energy|Electricity`,
            `Biomass`=(`Secondary Energy|Electricity|Biomass`)/`Secondary Energy|Electricity`,
            `Nuclear + Non-biomass renewables`=(`Secondary Energy|Electricity`-(`Secondary Energy|Electricity|Coal`+`Secondary Energy|Electricity|Oil`+`Secondary Energy|Electricity|Gas`+`Secondary Energy|Electricity|Biomass`))/`Secondary Energy|Electricity`)

g_fig_1b <- df_fig_1b %>% 
  filter(Year==2050) %>% 
  mutate(Scenario=factor(Scenario,levels=c('Opt1.5C','ZF2100','ZF2090','ZF2080','ZF2070','ZF2060','ZF2050'))) %>% 
  arrange(Model,Scenario) %>% 
  ggtern(aes(Fossil,`Biomass`,`Nuclear + Non-biomass renewables`)) +
  geom_mask() +
  geom_point(data=df_fig_1b_AR6 %>% filter(Year==2050,Category%in%c('C2')),color='grey70',size=0.7,shape=1,stroke=0.4) +
  geom_point(data=df_fig_1b_AR6 %>% filter(Year==2050,Category%in%c('C1')),color='grey70',size=0.7,shape=4,stroke=0.4) +
  geom_point(aes(color=Model,shape=Scenario,size=Scenario),stroke=0.5) +
  annotate("text", x = 0.85, y = 0.075, z = 0.075, hjust=0.1,
           label = paste0('C1: n=', df_fig_1b_AR6 %>% filter(Year == 2050, Category%in%c('C1')) %>% nrow()), 
           size = 1.75) +
  annotate("text", x = 0.675, y = 0.075, z = 0.25, hjust=0.1,
           label = paste0('C2: n=', df_fig_1b_AR6 %>% filter(Year == 2050, Category%in%c('C2')) %>% nrow()), 
           size = 1.75) +
  geom_path(aes(color=Model),linewidth=0.25) +
  scale_color_manual(values=c('AIM'='#349b8d','MESSAGEix'='#ff7f0e')) +
  scale_shape_manual(values=c('Opt1.5C'=4,'ZF2100'=2,'ZF2050'=21,
                              'ZF2060'=0,'ZF2070'=8,'ZF2080'=5,'ZF2090'=3)) +
  scale_size_manual(values=c('Opt1.5C'=1.5,'ZF2100'=1.5,'ZF2050'=1.5,
                             'ZF2060'=1.5,'ZF2070'=1.5,'ZF2080'=1.5,'ZF2090'=1.5)) +
  labs(
    x='',y='',z='',
    Tarrow='Biomass (%)',
    Larrow='Fossil (%)',
    Rarrow='Nuclear & non-biomass renewables (%)'
  ) +
  plot_theme_ternary +
  theme(
    plot.margin=margin(-15,-30,-10,-30)
  ) +
  theme_showarrows() +
  theme_clockwise() +
  theme_ticksinside()
plot(g_fig_1b)


# fig. 1c -----------------------------------------------------------------

df_fig_1c <- df_load %>%
  filter(Variable%in%c('Final Energy',
                       'Final Energy|Solids',
                       'Final Energy|Liquids',
                       'Final Energy|Gases')) %>%
  pivot_wider(names_from=Variable,values_from=value) %>%
  transmute(Model,Scenario,Region,Year,
            Solids=`Final Energy|Solids`/`Final Energy`,
            `Liquid & gases`=(`Final Energy|Liquids`+`Final Energy|Gases`)/`Final Energy`,
            `Electricity, hydrogen & others`=(`Final Energy`-(`Final Energy|Solids`+`Final Energy|Liquids`+`Final Energy|Gases`))/`Final Energy`)

df_fig_1c_AR6 <- df_load_AR6 %>%
  filter(Category%in%c('C1','C2'),Year%in%c(2030,2050,2070,2100),
         Variable%in%c('Final Energy',
                       'Final Energy|Solids',
                       'Final Energy|Liquids',
                       'Final Energy|Gases')) %>%
  pivot_wider(names_from=Variable,values_from=value) %>%
  drop_na() %>% 
  transmute(Region,Case,Category,Year,
            Solids=`Final Energy|Solids`/`Final Energy`,
            `Liquid & gases`=(`Final Energy|Liquids`+`Final Energy|Gases`)/`Final Energy`,
            `Electricity, hydrogen & others`=(`Final Energy`-(`Final Energy|Solids`+`Final Energy|Liquids`+`Final Energy|Gases`))/`Final Energy`)

g_fig_1c <- df_fig_1c %>%
  filter(Year==2050) %>%
  mutate(Scenario=factor(Scenario,levels=c('Opt1.5C','ZF2100','ZF2090','ZF2080','ZF2070','ZF2060','ZF2050'))) %>%
  arrange(Model,Scenario) %>%
  ggtern(aes(Solids,`Liquid & gases`,`Electricity, hydrogen & others`)) +
  geom_mask() +
  geom_point(data=df_fig_1c_AR6 %>% filter(Year==2050,Category%in%c('C2')),color='grey70',size=0.7,shape=1,stroke=0.4) +
  geom_point(data=df_fig_1c_AR6 %>% filter(Year==2050,Category%in%c('C1')),color='grey70',size=0.7,shape=4,stroke=0.4) +
  geom_point(aes(color=Model,shape=Scenario,size=Scenario),stroke=0.5) +
  annotate("text", x = 0.85, y = 0.075, z = 0.075, hjust=0.1,
           label = paste0('C1: n=', df_fig_1c_AR6 %>% filter(Year == 2050, Category%in%c('C1')) %>% nrow()), 
           size = 1.75) +
  annotate("text", x = 0.675, y = 0.075, z = 0.25, hjust=0.1,
           label = paste0('C2: n=', df_fig_1c_AR6 %>% filter(Year == 2050, Category%in%c('C2')) %>% nrow()), 
           size = 1.75) +
  geom_path(aes(color=Model),linewidth=0.25) +
  scale_color_manual(values=c('AIM'='#349b8d','MESSAGEix'='#ff7f0e')) +
  scale_shape_manual(values=c('Opt1.5C'=4,'ZF2100'=2,'ZF2050'=21,
                              'ZF2060'=0,'ZF2070'=8,'ZF2080'=5,'ZF2090'=3)) +
  scale_size_manual(values=c('Opt1.5C'=1.5,'ZF2100'=1.5,'ZF2050'=1.5,
                             'ZF2060'=1.5,'ZF2070'=1.5,'ZF2080'=1.5,'ZF2090'=1.5)) +
  labs(
    x='',y='',z='',
    Tarrow='Liquid & gases (%)',
    Larrow='Solids (%)',
    Rarrow='Electricity, hydrogen & others (%)'
  ) +
  plot_theme_ternary +
  theme(
    plot.margin=margin(-15,-30,-10,-30)
  ) +
  theme_showarrows() +
  theme_clockwise() +
  theme_ticksinside()
plot(g_fig_1c)


# fig. 1d -----------------------------------------------------------------

df_fig_1d <- df_load %>% 
  filter(Variable%in%c('Primary Energy|Coal','Primary Energy|Oil','Primary Energy|Gas')) %>%
  group_by(Model,Scenario,Region,Unit,Year) %>%
  reframe(value=sum(value))

df_fig_1d_AR6 <- df_load_AR6 %>% 
  filter(Year%in%c(2030,2050,2070,2100),Variable%in%c('Primary Energy|Coal','Primary Energy|Oil','Primary Energy|Gas')) %>% 
  group_by(Region,Category,Case,Year) %>% 
  reframe(value=sum(value)) %>% 
  group_by(Region,Year) %>% 
  reframe(p50=median(value),p90=quantile(value,.9),p10=quantile(value,.1),p0=min(value),p100=max(value),n=n())

g_fig_1d <- df_fig_1d %>% 
  filter(Scenario%in%c('Opt1.5C','ZF2100','ZF2050')) %>% 
  arrange(Scenario) %>% 
  ggplot() +
  geom_hline(yintercept=0,linewidth=0.1,color='grey15',linetype='dashed') +
  geom_crossbar(data=df_fig_1d_AR6,
                aes(x=Year,ymin=p10,ymax=p90,y=p50),width=4.5,color='white',fill='grey80',linewidth=0.25)+
  geom_crossbar(data=df_fig_1d_AR6,
                aes(x=Year,ymin=p0,ymax=p100,y=p0),width=4.5,color='grey80',fill='transparent',fatten=0,linewidth=0.25)+
  geom_line(aes(x=Year,y=value,color=Model,group=interaction(Model,Scenario)),linewidth=0.2) +
  geom_point(data=. %>% filter(Year%in%c(seq(2020,2055,5),seq(2060,2100,10))&Model=='MESSAGEix'|Year%in%seq(2020,2100,5)&Model=='AIM'),
             aes(x=Year,y=value,shape=Scenario,color=Model,size=Scenario),stroke=0.4) +
  annotate("text", x=2025, y=0, hjust=0,vjust=0,
           label = paste0('n=',df_fig_1d_AR6 %>% filter(Year == 2050) %>% pull(n)),
           size = 1.75) +
  labs(x='',y=expression(paste('Primary energy (EJ ',{yr^{-1}},')'))) +
  scale_color_manual(values=c('AIM'='#349b8d','MESSAGEix'='#ff7f0e')) +
  scale_size_manual(values=c('Opt1.5C'=1,'ZF2100'=.8,'ZF2050'=.8)) +
  scale_shape_manual(values=c('Opt1.5C'=4,'ZF2100'=2,'ZF2050'=21)) +
  plot_theme_white +
  theme(legend.position='none')
plot(g_fig_1d)


# fig. 1e -----------------------------------------------------------------

df_fig_1e <- df_fig_1d %>% 
  group_by(Model,Scenario,Region,Unit) %>% 
  complete(Year=2020:2100) %>% 
  mutate(value=na_locf(value,option='nocb')) %>% 
  mutate(value=cumsum(value))

df_fig_1e_AR6 <- df_load_AR6 %>% 
  filter(Variable%in%c('Primary Energy|Coal','Primary Energy|Oil','Primary Energy|Gas'),Year>=2020) %>% 
  group_by(Region,Category,Case,Year) %>% 
  reframe(value=sum(value)) %>% 
  group_by(Region,Category,Case) %>% 
  mutate(value=na_interpolation(value)) %>% 
  group_by(Region,Category,Case) %>% 
  mutate(value=cumsum(value)) %>% 
  filter(Year%in%c(2030,2050,2070,2100)) %>% 
  group_by(Region,Year) %>% 
  reframe(p50=median(value),p90=quantile(value,.9),p10=quantile(value,.1),p0=min(value),p100=max(value),n=n())

g_fig_1e <- df_fig_1e %>% 
  filter(Scenario%in%c('Opt1.5C','ZF2100','ZF2050')) %>% 
  arrange(Scenario) %>% 
  ggplot() +
  geom_hline(yintercept=0,linewidth=0.1,color='grey15',linetype='dashed') +
  geom_crossbar(data=df_fig_1e_AR6,
                aes(x=Year,ymin=p10/1000,ymax=p90/1000,y=p50/1000),width=4.5,color='white',fill='grey80',linewidth=0.25)+
  geom_crossbar(data=df_fig_1e_AR6,
                aes(x=Year,ymin=p0/1000,ymax=p100/1000,y=p0/1000),width=4.5,color='grey80',fill='transparent',fatten=0,linewidth=0.25)+
  geom_line(aes(x=Year,y=value/1000,color=Model,group=interaction(Model,Scenario)),linewidth=0.2) +
  geom_point(data=. %>% filter(Year%in%c(seq(2020,2055,5),seq(2060,2100,10))&Model=='MESSAGEix'|Year%in%seq(2020,2100,5)&Model=='AIM'),
             aes(x=Year,y=value/1000,shape=Scenario,color=Model,size=Scenario),stroke=0.4) +
  annotate("text", x=2025, y=0, hjust=0,vjust=0,
           label = paste0('n=', df_fig_1e_AR6 %>% filter(Year == 2050) %>% pull(n)),
           size = 1.75) +
  labs(x='',y=expression(paste('Cumulatitve primary energy (ZJ)'))) +
  scale_color_manual(values=c('AIM'='#349b8d','MESSAGEix'='#ff7f0e')) +
  scale_size_manual(values=c('Opt1.5C'=1,'ZF2100'=.8,'ZF2050'=.8)) +
  scale_shape_manual(values=c('Opt1.5C'=4,'ZF2100'=2,'ZF2050'=21)) +
  plot_theme_white +
  theme(legend.position='none')
plot(g_fig_1e)


# fig. 1f -----------------------------------------------------------------

plt <- set_plot(var_prm)

df_fig_1f <- df_load %>%
  filter(Variable%in%c('Primary Energy|Coal','Primary Energy|Coal|w/o CCS',
                       'Primary Energy|Oil','Primary Energy|Oil|w/o CCS',
                       'Primary Energy|Gas','Primary Energy|Gas|w/o CCS')) %>%
  pivot_wider(names_from=Variable,values_from=value) %>%
  transmute(Model,Scenario,Region,Unit,Year,
            `Primary Energy|Coal|w/ CCS`=`Primary Energy|Coal`-`Primary Energy|Coal|w/o CCS`,
            `Primary Energy|Oil|w/ CCS`=`Primary Energy|Oil`-`Primary Energy|Oil|w/o CCS`,
            `Primary Energy|Gas|w/ CCS`=`Primary Energy|Gas`-`Primary Energy|Gas|w/o CCS`,
            `Primary Energy|Coal|w/o CCS`,
            `Primary Energy|Oil|w/o CCS`,
            `Primary Energy|Gas|w/o CCS`) %>% 
  pivot_longer(cols=-c(Model,Scenario,Region,Unit,Year),
               names_to='Variable',
               values_to='value') %>% 
  group_by(Model,Scenario,Region,Variable,Unit) %>% 
  complete(Year=2020:2100) %>% 
  mutate(value=na_locf(value,option='nocb')) %>% 
  reframe(value=sum(value))

g_fig_1f <- df_fig_1f %>% 
  filter(Scenario%in%c('Opt1.5C','ZF2100','ZF2050')) %>% 
  mutate(Variable=factor(Variable,levels=rev(var_prm$Variable)),
         Scenario=factor(Scenario,levels=c('Opt1.5C','ZF2100','ZF2050'))) %>% 
  arrange(Scenario) %>% 
  ggplot() +
  geom_hline(yintercept=0,linewidth=0.1,color='grey15',linetype='dashed') +
  geom_bar(aes(x=Scenario,y=value/1000,fill=Variable),stat='identity') +
  scale_fill_manual(values=plt$Color,labels=plt$Legend) +
  labs(x='',y=expression(paste('Cumulative primary energy (ZJ)'))) +
  facet_wrap(vars(Model)) +
  plot_theme_white +
  theme(legend.position='none')
plot(g_fig_1f)


# fig. 1 ------------------------------------------------------------------

l_fig_1abc_1 <- g_legend(g_fig_1a+theme(legend.position='right'))

l_fig_1abc_2 <- tribble(~y,~y2,~label,
                      0,2,'AR6 C1',
                      0,1,'AR6 C2',
                      0,0,'Historical (2020)') %>% 
  mutate(x=0) %>% 
  ggplot() +
  geom_point(aes(y=y2,color=label,size=label,shape=label),x=.18)+
  geom_text(aes(y=y2,label=label),x=.5,hjust=0,size=2)+
  scale_color_manual(values=c('grey70','grey70','black')) +
  scale_size_manual(values=c(1,1,1.5)) +
  scale_shape_manual(values=c(4,1,13)) +
  xlim(-.2,1.6)+ylim(-.4,2.3)+
  theme_void()+theme(plot.margin=unit(c(0,0,0,0),'mm'),legend.position='none')

l_fig_1de <- tribble(~y,~y2,~label,
                 -.1,0,'Min',
                 1.1,1,'Max',
                 .5,.5,'Median',
                 .2,.2,'10th',
                 .8,.8,'90th') %>% 
  mutate(x=0) %>% 
  ggplot()+
  geom_crossbar(aes(x=x),ymin=.2,ymax=.8,y=.5,width=.15,linewidth=.15,color='white',fill='grey80')+
  geom_crossbar(aes(x=x),ymin=0,ymax=1,y=0,width=.15,linewidth=.15,color='grey80',fill='transparent',fatten=0)+
  geom_point(aes(y=y2),x=.18,shape=95,color='grey70',size=1.5)+
  geom_text(aes(y=y,label=label),x=.35,hjust=0,size=2)+
  annotate('text',x=0.75,y=1.3,label='AR6 C1&C2',size=2) +
  xlim(-.4,1.6)+ylim(-.4,1.3)+
  theme_void()+theme(plot.margin=unit(c(0,0,0,0),'mm'))

l_fig_1f <- g_legend(g_fig_1f+theme(legend.position='right'))

g_fig_1 <-ggdraw() +
  draw_plot(g_fig_1c,x=.65,y=.475,width=.35,height=.525) +
  draw_plot(g_fig_1b,x=.3125,y=.475,width=.35,height=.525) +
  draw_plot(g_fig_1a,x=-.025,y=.475,width=.35,height=.525) +
  draw_plot(g_fig_1d,x=.0,y=0,width=.185,height=.45) +
  draw_plot(g_fig_1e,x=.2,y=0,width=.185,height=.45) +
  draw_plot(g_fig_1f,x=.4,y=0,width=.2,height=.45) +
  draw_plot(l_fig_1abc_1,x=.65,y=0.025,width=.075,height=.45) +
  draw_plot(l_fig_1abc_2,x=.71,y=0.08,width=.2,height=.075) +
  draw_plot(l_fig_1de,x=.74,y=0.14,width=.075,height=.275) +
  draw_plot(l_fig_1f,x=.85,y=0.2,width=.075,height=.2) +
  draw_text('Primary energy mix',x=0,y=.94,size=7,hjust=-0.1) +
  draw_text('Power generation mix',x=.315,y=.94,size=7,hjust=-0.1) +
  draw_text('Final energy mix',x=.675,y=.94,size=7,hjust=-0.1) +
  draw_plot_label(
    label=c('a','b','c','d','e','f'),
    x=c(0,.315,.675,0,.2,0.4),
    y=c(1,1,1,.475,.475,.475),
    size=8
  )
plot(g_fig_1)

ggsave('output/1st_submission/fig_1.png',g_fig_1,width=180,height=105,units='mm',dpi=500)


# fig. 2a -----------------------------------------------------------------

plt <- set_plot(var_fin_agg)

df_fig_2a <- df_load %>% 
  filter(Variable%in%c('Final Energy|Solids|Coal',
                       'Final Energy|Liquids|Fossil',
                       'Final Energy|Solids|Biomass',
                       'Final Energy|Liquids|Biomass',
                       'Final Energy|Solar',
                       'Final Energy|Electricity',
                       'Final Energy|Heat',
                       'Final Energy|Hydrogen',
                       'Final Energy|Gases|Fossil',
                       'Final Energy|Gases|Biomass',
                       'Final Energy|Gases|Hydrogen synfuel',
                       'Final Energy|Liquids|Hydrogen synfuel')) %>% 
  mutate(Variable=case_when(str_detect(Variable,'Biomass')~'Final Energy|Biomass',
                            TRUE~Variable)) %>% 
  group_by(Model,Scenario,Region,Variable,Unit,Year) %>% 
  reframe(value=sum(value))

g_fig_2a <-df_fig_2a %>%
  filter(Scenario%in%c('Opt1.5C','ZF2050')) %>% 
  mutate(Variable=recode(Variable,
                         'Final Energy|Liquids|Hydrogen synfuel'='Final Energy|Hydrogen synfuel',
                         'Final Energy|Gases|Hydrogen synfuel'='Final Energy|Hydrogen synfuel'),
         Variable=factor(Variable,levels=rev(var_fin_agg$Variable))) %>%
  group_by(Model,Scenario,Region,Variable,Unit,Year) %>% 
  reframe(value=sum(value)) %>% 
  ggplot() +
  geom_area(aes(x=Year,y=value,fill=Variable)) +
  scale_fill_manual(values=plt$Color,labels=plt$Legend) +
  guides(fill=guide_legend(ncol=1)) +
  labs(x='',y=expression(paste('Final energy (EJ ',{yr^{-1}},')'))) +
  facet_grid(rows=vars(Model),cols=vars(Scenario),
             axes='all',axis.labels='margins') +
  plot_theme_white
plot(g_fig_2a)


# fig. 2b -----------------------------------------------------------------

df_fig_2b <- df_fig_2a %>% 
  group_by(Model,Scenario,Region,Unit,Year) %>% 
  mutate(value=value/sum(value)) %>% 
  ungroup()

df_fig_2b_AR6_hydcar <- df_load_AR6 %>%
  filter(Category%in%c('C1','C2'),Year%in%c(2030,2050,2070,2100),
         Variable%in%c('Final Energy',
                       'Final Energy|Solids|Coal',
                       'Final Energy|Solids|Biomass',
                       'Final Energy|Liquids',
                       'Secondary Energy|Liquids',
                       'Secondary Energy|Liquids|Biomass',
                       'Final Energy|Gases',
                       'Secondary Energy|Gases',
                       'Secondary Energy|Gases|Biomass')) %>%
  pivot_wider(names_from=Variable,values_from=value) %>% 
  drop_na(`Secondary Energy|Liquids`,`Secondary Energy|Gases`,`Secondary Energy|Liquids|Biomass`) %>% 
  replace_na(list(`Secondary Energy|Gases|Biomass`=0)) %>%
  transmute(Region,Case,Category,Year,
            Fossil=(`Final Energy|Solids|Coal`+
                      (`Secondary Energy|Liquids`-`Secondary Energy|Liquids|Biomass`)/`Secondary Energy|Liquids`*`Final Energy|Liquids`+
                      (`Secondary Energy|Gases`-`Secondary Energy|Gases|Biomass`)/`Secondary Energy|Gases`*`Final Energy|Gases`)/`Final Energy`,
            Biomass=(`Final Energy|Solids|Biomass`+
                       `Secondary Energy|Liquids|Biomass`/`Secondary Energy|Liquids`*`Final Energy|Liquids`+
                       `Secondary Energy|Gases|Biomass`/`Secondary Energy|Gases`*`Final Energy|Gases`)/`Final Energy`) %>% 
  pivot_longer(cols=-c(Region,Case,Category,Year),
               names_to='Variable',values_to='value') %>% 
  filter(Year%in%c(2030,2050,2070,2100)) %>% 
  group_by(Region,Variable,Year) %>% 
  reframe(p50=median(value),p90=quantile(value,.9),p10=quantile(value,.1),p0=min(value),p100=max(value),n=n())

df_fig_2b_AR6_ele <- df_load_AR6 %>%
  filter(Category%in%c('C1','C2'),Year%in%c(2030,2050,2070,2100),
         Variable%in%c('Final Energy',
                       'Final Energy|Electricity')) %>%
  pivot_wider(names_from=Variable,values_from=value) %>% 
  drop_na(`Final Energy|Electricity`) %>%
  transmute(Region,Case,Category,Year,
            Electricity=`Final Energy|Electricity`/`Final Energy`) %>% 
  pivot_longer(cols=-c(Region,Case,Category,Year),
               names_to='Variable',values_to='value') %>% 
  filter(Year%in%c(2030,2050,2070,2100)) %>% 
  group_by(Region,Variable,Year) %>% 
  reframe(p50=median(value),p90=quantile(value,.9),p10=quantile(value,.1),p0=min(value),p100=max(value),n=n())

df_fig_2b_AR6_hyd <- df_load_AR6 %>%
  filter(Category%in%c('C1','C2'),Year%in%c(2030,2050,2070,2100),
         Variable%in%c('Final Energy',
                       'Final Energy|Hydrogen')) %>%
  pivot_wider(names_from=Variable,values_from=value) %>% 
  drop_na(`Final Energy|Hydrogen`) %>%
  transmute(Region,Case,Category,Year,
            Hydrogen=`Final Energy|Hydrogen`/`Final Energy`) %>% 
  pivot_longer(cols=-c(Region,Case,Category,Year),
               names_to='Variable',values_to='value') %>% 
  filter(Year%in%c(2030,2050,2070,2100)) %>% 
  group_by(Region,Variable,Year) %>% 
  reframe(p50=median(value),p90=quantile(value,.9),p10=quantile(value,.1),p0=min(value),p100=max(value),n=n())

df_fig_2b_AR6 <-  bind_rows(df_fig_2b_AR6_hydcar,df_fig_2b_AR6_ele,df_fig_2b_AR6_hyd) %>% 
  bind_rows(expand_grid(Region='World',Variable='Synfuel',Year=c(2030,2050,2070,2100),
                        p50=0,p90=0,p10=0,p0=0,p100=0,n=0)) %>%
  expand_grid(Model=c('AIM','MESSAGEix')) %>% 
  mutate(Variable=factor(Variable,levels=c('Fossil','Electricity','Biomass','Hydrogen','Synfuel'))) 

g_fig_2b <- df_fig_2b %>%
  filter(Scenario%in%c('Opt1.5C','ZF2100','ZF2050')) %>%
  mutate(Variable=case_when(Variable%in%c('Final Energy|Solids|Coal','Final Energy|Liquids|Fossil','Final Energy|Gases|Fossil')~'Fossil',
                            Variable%in%c('Final Energy|Biomass')~'Biomass',
                            Variable%in%c('Final Energy|Electricity')~'Electricity',
                            Variable%in%c('Final Energy|Hydrogen')~'Hydrogen',
                            Variable%in%c('Final Energy|Liquids|Hydrogen synfuel','Final Energy|Gases|Hydrogen synfuel')~'Synfuel'),
         Variable=factor(Variable,levels=c('Fossil','Electricity','Biomass','Hydrogen','Synfuel'))) %>%
  drop_na(Variable) %>%
  group_by(Model,Scenario,Region,Unit,Year,Variable) %>%
  reframe(value=sum(value)) %>%
  ggplot() +
  geom_hline(yintercept=0,linewidth=0.1,color='grey15',linetype='dashed') +
  geom_crossbar(data=df_fig_2b_AR6,
                aes(x=Year,ymin=p10,ymax=p90,y=p50),width=4,color='white',fill='grey80',linewidth=0.25)+
  geom_crossbar(data=df_fig_2b_AR6,
                aes(x=Year,ymin=p0,ymax=p100,y=p0),width=4,color='grey80',fill='transparent',fatten=0,linewidth=0.25)+
  geom_line(aes(x=Year,y=value,color=Scenario,linetype=Scenario),linewidth=0.2) +
  geom_point(aes(x=Year,y=value,color=Scenario,shape=Scenario),size=0.7,stroke=0.4,fill=NA) +
  geom_text(data=df_fig_2b_AR6 %>% filter(Year==2050),
            aes(label=paste0('n=',n)),x=2025,y=0.825,hjust=0,vjust=0,size=1.75) +
  scale_color_manual(values=c('Opt1.5C'='grey35','ZF2100'='#4DBBD5FF','ZF2050'='#E64B35FF')) +
  scale_linetype_manual(values=c('Opt1.5C'='dotdash','ZF2100'='solid','ZF2050'='solid')) +
  scale_shape_manual(values=c('Opt1.5C'=4,'ZF2100'=2,'ZF2050'=21)) +
  scale_y_continuous(labels=scales::percent) +
  labs(x='',y='Share in final energy (%)') +
  guides(color=guide_legend(nrow=1)) +
  facet_grid(rows=vars(Model),cols=vars(Variable),axes='all',axis.labels='margins') +
  plot_theme_white +
  theme(legend.position='bottom')
plot(g_fig_2b)


# fig. 2 ------------------------------------------------------------------

l_fig_2a <- g_legend(g_fig_2a+guides(fill=guide_legend(nrow=1)))
l_fig_2b <- g_legend(g_fig_2b+guides(color=guide_legend(ncol=1))+theme(legend.position='right'))

g_fig_2 <-ggdraw() +
  draw_plot(g_fig_2a+theme(legend.position='none'),x=0,y=0.05,width=.25,height=.935) +
  draw_plot(g_fig_2b+theme(legend.position='none'),x=0.265,y=0.05,width=.635,height=.935) +
  draw_plot(l_fig_1de,x=.91,y=.15,width=.075,height=.4) +
  draw_plot(l_fig_2a,x=0,y=0,width=1,height=.05) +
  draw_plot(l_fig_2b,x=.915,y=.6,width=.075,height=.3) +
  draw_plot_label(
    label=c('a','b'),
    x=c(0,0.265),
    y=c(1,1),
    size=8
  )
plot(g_fig_2)

ggsave('output/1st_submission/fig_2.png',g_fig_2,width=180,height=70,units='mm',dpi=1000)


# fig. 3a & 3b ------------------------------------------------------------

plt <- set_plot(var_ele)

var_sec_flow <- c('Secondary Energy|Hydrogen|Fossil','Secondary Energy|Hydrogen|Biomass',
                  'Secondary Energy|Electricity','Secondary Energy|Hydrogen|Electricity','Final Energy|Electricity','Secondary Energy|Electricity|Own Use|DAC',
                  'Secondary Energy|Hydrogen','Final Energy|Liquids|Hydrogen synfuel','Final Energy|Gases|Hydrogen synfuel','Final Energy|Hydrogen')

var_sec_flow2 <- c('Secondary Energy|Hydrogen|Fossil','Secondary Energy|Hydrogen|Biomass','Secondary Energy|Electricity|Own Use|DAC',
                   'Final Energy|Electricity','Final Energy|Liquids|Hydrogen synfuel','Final Energy|Gases|Hydrogen synfuel','Final Energy|Hydrogen',
                   'Loss|Electricity','Loss|Hydrogen')

df_sec_loss <- df_load %>% 
  filter(Year%in%c(2050,2100),Scenario%in%c('Opt1.5C','ZF2050')) %>% 
  filter(Variable%in%c(var_sec_flow)) %>% 
  group_by(Model,Scenario,Region,Unit,Year) %>% 
  complete(Variable=var_sec_flow,
           fill=list(value=0)) %>% 
  ungroup() %>% 
  pivot_wider(names_from=Variable,values_from=value) %>% 
  transmute(Model,Scenario,Region,Unit,Year,
            `Loss|Electricity`=`Secondary Energy|Electricity`-(`Secondary Energy|Hydrogen|Electricity`+`Secondary Energy|Hydrogen|Fossil`+`Secondary Energy|Hydrogen|Biomass`+`Final Energy|Electricity`+`Secondary Energy|Electricity|Own Use|DAC`),
            `Loss|Hydrogen`=`Secondary Energy|Hydrogen`-(`Final Energy|Liquids|Hydrogen synfuel`+`Final Energy|Gases|Hydrogen synfuel`+`Final Energy|Hydrogen`)
  ) %>% 
  pivot_longer(cols=c('Loss|Electricity','Loss|Hydrogen'),names_to='Variable',values_to='value')

for (i in c(2050,2100)) {
  for (j in c('AIM','MESSAGEix')) {
    
    total <- df_load %>% 
      filter(Year==i,
             Model==j,
             Scenario%in%c('Opt1.5C','ZF2050'),
             Variable%in%c('Secondary Energy|Hydrogen|Fossil',
                           'Secondary Energy|Hydrogen|Biomass',
                           'Secondary Energy|Electricity')) %>% 
      group_by(Scenario) %>% 
      reframe(value=sum(value)) %>% 
      mutate(value=ceiling(value/50)*50)
    
    df_ele_snapshot <- df_load %>% 
      filter(Variable%in%c('Secondary Energy|Electricity|Fossil',
                           'Secondary Energy|Electricity|Nuclear',
                           'Secondary Energy|Electricity|Biomass',
                           'Secondary Energy|Electricity|Geothermal',
                           'Secondary Energy|Electricity|Hydro',
                           'Secondary Energy|Electricity|Solar',
                           'Secondary Energy|Electricity|Wind'),
             Year==i,
             Model==j,
             Scenario%in%c('Opt1.5C','ZF2050')) %>% 
      mutate(Variable=factor(Variable,levels=rev(var_ele$Variable)),
             Scenario=factor(Scenario,levels=c('Opt1.5C','ZF2100','ZF2050')))
    
    g_ele_snapshot <- df_ele_snapshot %>% 
      ggplot() +
      geom_bar(aes(x=Scenario,y=value,fill=Variable),stat='identity',width=0.8) +
      scale_fill_manual(values=plt$Color,labels=plt$Legend) +
      ylim(0,max(total$value))+
      labs(x='',y=expression(paste('Secondary energy (EJ ',{yr^{-1}},')'))) +
      plot_theme_white +
      theme(
        panel.grid.major.x=element_blank(),
        legend.position='right',
        plot.margin=unit(c(0,0,0,0),unit='mm')
      )
    plot(g_ele_snapshot)
    
    obj_name <- paste0("g_ele_snapshot_", i, "_", j)
    assign(obj_name, g_ele_snapshot)
    
    g_sec_flow <- tribble(~Variable,~Source,~Hydrogen,~Synfuel,~Final,
                          'Secondary Energy|Hydrogen|Fossil','Fossil','Hydrogen','Loss2','Loss',
                          'Secondary Energy|Hydrogen|Biomass','Biomass','Hydrogen','Loss2','Loss',
                          'Loss|Electricity','Electricity','Loss','Loss','Loss',
                          'Secondary Energy|Electricity|Own Use|DAC','Electricity','DAC','Loss2','Loss',
                          'Final Energy|Electricity','Electricity','Electricity','Electricity','Electricity',
                          'Loss|Hydrogen','Electricity','Hydrogen','Loss2','Loss',
                          'Final Energy|Liquids|Hydrogen synfuel','Electricity','Hydrogen','Synfuel','Synfuel',
                          'Final Energy|Gases|Hydrogen synfuel','Electricity','Hydrogen','Synfuel','Synfuel',
                          'Final Energy|Hydrogen','Electricity','Hydrogen','Hydrogen','Hydrogen') %>% 
      inner_join(bind_rows(df_load,df_sec_loss) %>% 
                   filter(Variable%in%var_sec_flow2,Year==i,Model==j,Scenario%in%c('Opt1.5C','ZF2050')) %>% 
                   group_by(Model,Scenario,Region,Unit,Year) %>% 
                   complete(Variable=var_sec_flow2,fill=list(value=0)) %>% 
                   ungroup()) %>% 
      select(-Year,-Unit,-Region) %>% 
      pivot_longer(cols=-c(Model,Scenario,Variable,value),names_to='x',values_to='Carrier') %>% 
      mutate(Label=Carrier,Alpha=1,Positionh=.5,Positionv=.5) %>% 
      mutate(Label=ifelse(x=='Hydrogen'&Carrier%in%c('Electricity'),' ',Label)) %>%
      mutate(Label=ifelse(x=='Synfuel'&Carrier%in%c('Electricity','Hydrogen','Loss'),' ',Label)) %>%
      mutate(Label=ifelse(x=='Synfuel'&Carrier=='Loss2','Loss',Label)) %>%
      mutate(Label=ifelse(x=='Final'&Carrier=='Synfuel',' ',Label)) %>%
      mutate(Alpha=ifelse(x=='Hydrogen'&Carrier%in%c('Electricity'),.5,Alpha)) %>%
      mutate(Alpha=ifelse(x=='Synfuel'&Carrier%in%c('Electricity','Hydrogen','Loss'),.5,Alpha)) %>%
      mutate(Alpha=ifelse(x=='Final'&Carrier=='Loss',.5,Alpha)) %>%
      mutate(Positionh=ifelse(x=='Source',.2,ifelse(x=='Final',.8,.5))) %>%
      mutate(Positionv=ifelse(x=='Source'&Carrier=='Fossil',-.5,Positionv)) %>% 
      mutate(Variable=factor(Variable,levels=rev(c('Secondary Energy|Hydrogen|Fossil',
                                                   'Secondary Energy|Hydrogen|Biomass',
                                                   'Loss|Electricity',
                                                   'Secondary Energy|Electricity|Own Use|DAC',
                                                   'Final Energy|Electricity',
                                                   'Loss|Hydrogen',
                                                   'Final Energy|Liquids|Hydrogen synfuel',
                                                   'Final Energy|Gases|Hydrogen synfuel',
                                                   'Final Energy|Hydrogen'))),
             Scenario=factor(Scenario,levels=c('Opt1.5C','ZF2050')),
             Carrier=factor(Carrier,levels=rev(c('Synfuel',
                                                 'Hydrogen',
                                                 'Electricity',
                                                 'Fossil',
                                                 'Biomass',
                                                 'DAC',
                                                 'Loss2',
                                                 'Loss')))) %>%
      ggplot(aes(x=x,y=value,alluvium=Variable,stratum=Carrier,label=Carrier))+
      geom_flow(aes(fill=Carrier),show.legend=F)+
      geom_stratum(aes(fill=Carrier,alpha=Alpha),color='transparent',show.legend=F)+
      geom_text(aes(label=Label,hjust=Positionh,vjust=Positionv),stat='stratum',size=6.5*0.282)+
      labs(title=NULL,x=NULL,y='')+
      ylim(0,max(total$value))+
      scale_x_discrete(limits=c('Source','Hydrogen','Synfuel','Final'),labels=c('Source','','','Demand'),expand=c(.05,.05))+
      scale_fill_manual(values=c('Synfuel'='orchid',
                                 'Hydrogen'='thistle2',
                                 'Electricity'='lightsteelblue',
                                 'Fossil'='sandybrown',
                                 'Biomass'='darkolivegreen2',
                                 'DAC'='darkgoldenrod2',
                                 'Loss'='grey',
                                 'Loss2'='grey'))+
      scale_alpha_continuous(limits=c(0,1),range=c(0,1))+
      facet_grid(rows=vars(Model),cols=vars(Scenario)) +
      plot_theme_white +
      theme(legend.position='bottom',
            strip.background=element_blank(),
            axis.text.y=element_blank(),
            plot.margin=unit(c(0,0,0,0),unit='mm'))+
      guides(fill=guide_legend(title=NULL))
    plot(g_sec_flow)
    
    obj_name <- paste0("g_sec_flow_", i, "_", j)
    assign(obj_name, g_sec_flow)
  }
}


# fig. 3c -----------------------------------------------------------------

df_fig_3c_1 <- df_load %>% 
  filter(Variable%in%c('Secondary Energy|Electricity|Fossil',
                       'Secondary Energy|Electricity|Nuclear',
                       'Secondary Energy|Electricity|Biomass',
                       'Secondary Energy|Electricity|Geothermal',
                       'Secondary Energy|Electricity|Hydro',
                       'Secondary Energy|Electricity|Solar',
                       'Secondary Energy|Electricity|Wind')) %>% 
  group_by(Model,Scenario,Region,Year) %>% 
  reframe(value=sum(value),
          Variable='Electricity\n(total)')

df_fig_3c_2 <- df_load %>% 
  filter(Variable%in%c('Secondary Energy|Hydrogen')) %>% 
  group_by(Model,Scenario,Region,Year) %>% 
  reframe(value=sum(value),
          Variable='Hydrogen\n(total)')

df_fig_3c <- bind_rows(
  df_fig_3c_1,
  df_fig_3c_2
) %>% 
  mutate(Variable=factor(Variable,levels=c(
    'Electricity\n(total)',
    'Hydrogen\n(total)'
  )))

df_fig_3c_AR6_1 <- df_load_AR6 %>% 
  filter(Variable%in%c('Secondary Energy|Electricity'),Year%in%c(2030,2050,2070,2100)) %>% 
  group_by(Region,Category,Case,Year) %>% 
  reframe(value=sum(value)) %>% 
  group_by(Region,Year) %>% 
  reframe(p50=median(value),p90=quantile(value,.9),p10=quantile(value,.1),p0=min(value),p100=max(value),n=n(),
          Variable='Electricity\n(total)')

df_fig_3c_AR6_2 <- df_load_AR6 %>% 
  filter(Variable%in%c('Secondary Energy|Hydrogen'),Year%in%c(2030,2050,2070,2100)) %>% 
  group_by(Region,Category,Case,Year) %>% 
  reframe(value=sum(value)) %>% 
  group_by(Region,Year) %>% 
  reframe(p50=median(value),p90=quantile(value,.9),p10=quantile(value,.1),p0=min(value),p100=max(value),n=n(),
          Variable='Hydrogen\n(total)')

df_fig_3c_AR6 <- bind_rows(
  df_fig_3c_AR6_1,
  df_fig_3c_AR6_2
) %>% 
  mutate(Variable=factor(Variable,levels=c(
    'Electricity\n(total)',
    'Hydrogen\n(total)'
  )))

g_fig_3c <- df_fig_3c %>% 
  filter(Year%in%c(2030,2050,2070,2100)) %>% 
  mutate(Year=as.character(Year),
         Scenario=factor(Scenario,levels=c('Opt1.5C','ZF2100','ZF2090','ZF2080','ZF2070','ZF2060','ZF2050'))) %>%
  arrange(Scenario) %>% 
  ggplot() +
  geom_hline(yintercept=0,linewidth=0.1,color='grey15',linetype='dashed') +
  geom_crossbar(data=df_fig_3c_AR6 %>% mutate(Year=as.character(Year)),
                aes(x=Year,ymin=p10,ymax=p90,y=p50),width=.6,color='white',fill='grey80',linewidth=0.3)+
  geom_crossbar(data=df_fig_3c_AR6 %>% mutate(Year=as.character(Year)),
                aes(x=Year,ymin=p0,ymax=p100,y=p0),width=.6,color='grey80',fill='transparent',fatten=0,linewidth=0.3)+
  geom_jitter(aes(x=Year,y=value,shape=Scenario,color=Model,size=Scenario),stroke=0.4,width=.1) +
  labs(x='',y=expression(paste('Secondary energy (EJ ',{yr^{-1}},')'))) +
  geom_text(data=df_fig_3c_AR6 %>% mutate(Year=as.character(Year)),
            aes(x='2070',y=-20,label=paste0('n=',n)),size=1.75) +
  scale_color_manual(values=c('AIM'='#349b8d','MESSAGEix'='#ff7f0e')) +
  scale_size_manual(values=c('Opt1.5C'=1,'ZF2100'=1,'ZF2050'=1,
                             'ZF2060'=1,'ZF2070'=1,'ZF2080'=1,'ZF2090'=1)) +
  scale_shape_manual(values=c('Opt1.5C'=4,'ZF2100'=2,'ZF2050'=21,
                              'ZF2060'=0,'ZF2070'=8,'ZF2080'=5,'ZF2090'=3)) +
  facet_wrap(vars(Variable),nrow=1,axes='all_y',scales='free') +
  plot_theme_white
plot(g_fig_3c)


# fig. 3d -----------------------------------------------------------------

df_fig_3d <- df_load %>%
  filter(Variable%in%c('Capacity|Electricity|Solar|PV',
                       'Capacity|Electricity|Wind',
                       'Capacity|Hydrogen|Electricity',
                       'Capacity|Electricity|Storage')) %>%
  mutate(Variable=recode(Variable,
                         'Capacity|Electricity|Solar|PV'='Solar PV',
                         'Capacity|Electricity|Wind'='Wind',
                         'Capacity|Electricity|Storage'='Storage',
                         'Capacity|Hydrogen|Electricity'='Electrolyzer'),
         Variable=factor(Variable,levels=c('Solar PV','Wind','Storage','Electrolyzer'))) %>% 
  filter(Scenario%in%c('Opt1.5C','ZF2050','ZF2100'),
         Year%in%seq(2020,2100,10)) %>% 
  pivot_wider(names_from=Year,values_from=value) %>% 
  transmute(Model,Scenario,Region,Variable,Unit,
            `2020-30`=(`2030`-`2020`)/10,
            `2030-40`=(`2040`-`2030`)/10,
            `2040-50`=(`2050`-`2040`)/10,
            `2050-60`=(`2060`-`2050`)/10,
            `2060-70`=(`2070`-`2060`)/10,
            `2070-80`=(`2080`-`2070`)/10,
            `2080-90`=(`2090`-`2080`)/10,
            `2090-100`=(`2100`-`2090`)/10) %>% 
  pivot_longer(cols=-c(Model,Scenario,Region,Variable,Unit),
               names_to='Year',values_to='value')

g_fig_3d <- df_fig_3d %>% 
  ggplot() +
  geom_hline(yintercept=0,linewidth=0.1,color='grey15',linetype='dashed') +
  geom_line(aes(x=Year,y=value/1000,color=Scenario,linetype=Scenario,group=Scenario),linewidth=0.2) +
  geom_area(aes(x=Year,y=value/1000,fill=Scenario,group=Scenario),alpha=0.1,position='identity') +
  geom_point(aes(x=Year,y=value/1000,color=Scenario,shape=Scenario,group=Scenario),size=1,stroke=0.5,fill=NA) +
  scale_color_manual(values=c('Opt1.5C'='grey35','ZF2100'='#4DBBD5FF','ZF2050'='#E64B35FF')) +
  scale_fill_manual(values=c('Opt1.5C'='grey35','ZF2100'='#4DBBD5FF','ZF2050'='#E64B35FF')) +
  scale_linetype_manual(values=c('Opt1.5C'='dotdash','ZF2100'='solid','ZF2050'='solid')) +
  scale_shape_manual(values=c('Opt1.5C'=4,'ZF2100'=2,'ZF2050'=21)) +
  scale_x_discrete(labels=c('2020-30',
                            '',
                            '2040-50',
                            '',
                            '2060-70',
                            '',
                            '2080-90',
                            '')) +
  labs(x='',y=expression(paste('Net capacity increase (TW ',{yr^{-1}},')'))) +
  facet_wrap(vars(Model,Variable),scales='free_y',axes='all',axis.labels='all_y',nrow=2) +
  plot_theme_white +
  theme(legend.position='right',
        axis.text.x=element_text(vjust=1,hjust=1))
plot(g_fig_3d)


# fig. 3 ------------------------------------------------------------------

l_fig_3ab <- g_legend(`g_ele_snapshot_2050_AIM`)
l_fig_3c_1 <- g_legend(g_fig_3c+guides(color=guide_legend(ncol=1),shape='none',size='none')+theme(legend.position='right'))
l_fig_3c_2 <- g_legend(g_fig_3c+guides(color='none')+theme(legend.position='right'))
l_fig_3d <- g_legend(g_fig_3d)

g_fig_3 <-ggdraw() +
  draw_plot(g_fig_3c+theme(legend.position='none'),x=0,y=0,width=.23,height=.55) +
  draw_plot(`g_ele_snapshot_2050_AIM`+theme(legend.position='none')+`g_sec_flow_2050_AIM`+plot_layout(widths=c(1,8)),
            x=0,y=0.55,width=.45,height=.45) +
  draw_plot(`g_ele_snapshot_2050_MESSAGEix`+theme(legend.position='none')+`g_sec_flow_2050_MESSAGEix`+plot_layout(widths=c(1,8)),,
            x=.45,y=0.55,width=.45,height=.45) +
  draw_plot(l_fig_3ab,x=.9,y=0.55,width=.1,height=.45) +
  draw_plot(g_fig_3d+theme(legend.position='bottom',legend.margin=margin(-15,0,0,0)),x=.34,y=0.02,width=.66,height=.53) +
  draw_plot(l_fig_3c_1,x=.238,y=0.425,width=.1,height=.1) +
  draw_plot(l_fig_3c_2,x=.229,y=0.275,width=.1,height=.1) +
  draw_plot(l_fig_1de,x=.238,y=0,width=.075,height=.2) +
  draw_plot_label(
    label=c('a','b','c','d'),
    x=c(0,.45,0,0.36),
    y=c(1,1,.55,.55),
    size=8
  )

ggsave('output/1st_submission/fig_3.png',g_fig_3,width=180,height=120,units='mm',dpi=500)


# fig. 4a -----------------------------------------------------------------

plt <- set_plot(var_emi)

df_fig_4a <- df_load %>% 
  filter(Variable=='Emissions|CO2|Energy and Industrial Processes including Direct Air Capture')

df_fig_4a_AR6 <- df_load_AR6 %>% 
  filter(Category%in%c('C1','C2'),
         Variable%in%c('Emissions|CO2|Energy and Industrial Processes','Carbon Sequestration|Direct Air Capture'),
         Year%in%c(2030,2050,2070,2100)) %>% 
  group_by(Region,Case,Category,Year) %>% 
  reframe(Variable='Emissions|CO2|Energy and Industrial Processes including Direct Air Capture',value=sum(value)) %>% 
  group_by(Region,Variable,Year) %>% 
  reframe(p50=median(value),p90=quantile(value,.9),p10=quantile(value,.1),p0=min(value),p100=max(value),n=n())

g_fig_4a <- df_fig_4a %>%
  mutate(Scenario=factor(Scenario,levels=c('Opt1.5C','ZF2100','ZF2090','ZF2080','ZF2070','ZF2060','ZF2050'))) %>% 
  arrange(rev(Scenario)) %>% 
  ggplot() +
  geom_hline(yintercept=0,linewidth=0.1,color='grey15',linetype='dashed') +
  geom_crossbar(data=df_fig_4a_AR6,
                aes(x=Year,ymin=p10/1000,ymax=p90/1000,y=p50/1000),width=3,color='white',fill='grey80',linewidth=0.25)+
  geom_crossbar(data=df_fig_4a_AR6,
                aes(x=Year,ymin=p0/1000,ymax=p100/1000,y=p0/1000),width=3,color='grey80',fill='transparent',fatten=0,linewidth=0.25)+
  geom_line(aes(x=Year,y=value/1000,color=Scenario,group=interaction(Model,Scenario),linetype=Scenario),linewidth=0.2) +
  geom_point(aes(x=Year,y=value/1000,color=Scenario,shape=Scenario),size=0.8,stroke=0.4) +
  geom_text(data=df_fig_4a_AR6 %>% filter(Year==2050),
            aes(label=paste0('n=',n)),x=2025,y=-20,hjust=0,vjust=0,size=1.75) +
  scale_color_manual(values=c('Opt1.5C'='grey30',
                              'ZF2050'="#2621A8FF",
                              'ZF2060'="#5D03AEFF",
                              'ZF2070'="#9C179EFF",
                              'ZF2080'="#CC4678FF",
                              'ZF2090'="#ED7953FF",
                              'ZF2100'="#FDB32FFF")) +
  scale_linetype_manual(values=c('Opt1.5C'='dashed',
                                 'ZF2050'="solid",
                                 'ZF2060'="solid",
                                 'ZF2070'="solid",
                                 'ZF2080'="solid",
                                 'ZF2090'="solid",
                                 'ZF2100'="solid"
  )) +
  scale_shape_manual(values=c('Opt1.5C'=4,
                              'ZF2100'=21,
                              'ZF2050'=2,
                              'ZF2060'=0,
                              'ZF2070'=8,
                              'ZF2080'=5,
                              'ZF2090'=3)) +
  labs(x='',y=expression(paste({CO[2]},' emission (Gt-',{CO[2]},' ',{yr^{-1}},')'))) +
  facet_wrap(vars(Model),nrow=1,axes='all',axis.labels='margins') +
  plot_theme_white
plot(g_fig_4a)


# fig. 4b -----------------------------------------------------------------

df_fig_4b <- df_load %>% 
  filter(Variable%in%c('Emissions|CO2|Energy|Demand|Industry',
                       'Emissions|CO2|Energy|Demand|Residential and Commercial',
                       'Emissions|CO2|Energy|Demand|Transportation',
                       'Emissions|CO2|Energy|Demand|AFOFI',
                       'Emissions|CO2|Energy|Demand|Other Sector',
                       'Emissions|CO2|Energy|Demand',
                       'Emissions|CO2|Energy|Supply',
                       'Emissions|CO2|Industrial Processes',
                       'Emissions|CO2|BECCS',
                       'Emissions|CO2|DACCS',
                       'Emissions|CO2|AFOLU')) %>% 
  group_by(Model,Scenario,Region,Variable,Unit) %>% 
  complete(Year=seq(2020,2100)) %>%
  mutate(value=na_locf(value,option='nocb')) %>% 
  filter(Year>=2020) %>% 
  pivot_wider(names_from=Variable,values_from=value) %>% 
  transmute(Model,Scenario,Region,Unit,Year,
            `Emissions|CO2|Energy|Demand|Industry`,
            `Emissions|CO2|Energy|Demand|Residential and Commercial`,
            `Emissions|CO2|Energy|Demand|Transportation`,
            `Emissions|CO2|Energy|Demand|Other`=`Emissions|CO2|Energy|Demand|AFOFI`+`Emissions|CO2|Energy|Demand|Other Sector`,
            `Emissions|CO2|Energy|Supply|Other`=`Emissions|CO2|Energy|Supply`-`Emissions|CO2|BECCS`,
            `Emissions|CO2|Industrial Processes`,
            `Emissions|CO2|DACCS`,
            `Emissions|CO2|BECCS`,
            `Emissions|CO2|AFOLU`) %>% 
  pivot_longer(cols=-c(Model,Scenario,Region,Unit,Year),names_to='Variable',values_to='value')

g_fig_4b <- df_fig_4b %>% 
  filter(Scenario%in%c('Opt1.5C','ZF2100','ZF2050'),Year==2050) %>% 
  mutate(Variable=factor(Variable,levels=rev(var_emi$Variable)),
         Scenario=factor(Scenario,levels=c('Opt1.5C','ZF2100','ZF2050'))) %>% 
  ggplot() +
  geom_hline(yintercept=0,linewidth=0.1,color='grey15',linetype='dashed') +
  geom_bar(aes(x=Scenario,y=value/1000,fill=Variable),stat='identity') +
  geom_point(data=. %>% 
               filter(Scenario%in%c('Opt1.5C','ZF2100','ZF2050'),Year==2050) %>% 
               mutate(Variable='Net CO2 (Energy + AFOLU)') %>% 
               replace_na(list(value=0)) %>% 
               group_by(Model,Scenario,Variable,Region,Unit) %>% 
               reframe(value=sum(value)),
             aes(x=Scenario,y=value/1000,shape=Variable),size=.9,color='black',fill='white',stroke=0.4) +
  geom_point(data=. %>% 
               filter(Scenario%in%c('Opt1.5C','ZF2100','ZF2050'),
                      Variable!='Emissions|CO2|AFOLU',Year==2050) %>% 
               mutate(Variable='Net CO2 (Energy)') %>% 
               replace_na(list(value=0)) %>% 
               group_by(Model,Scenario,Variable,Region,Unit) %>% 
               reframe(value=sum(value)),
             aes(x=Scenario,y=value/1000,shape=Variable),size=.9,color='black',fill='white',stroke=0.4) +
  scale_shape_manual(values=c('Net CO2 (Energy + AFOLU)'=21,'Net CO2 (Energy)'=25),
                     labels=c('Net CO2 (Energy + AFOLU)'=expression(paste('Net ',{CO[2]},' (Energy + AFOLU)')),
                              'Net CO2 (Energy)'=expression(paste('Net ',{CO[2]},' (Energy)')))) +
  scale_fill_manual(values=plt$Color,labels=plt$Legend) +
  labs(x='',y=expression(paste({CO[2]},' emission (Gt-',{CO[2]},' ',{yr^{-1}},')'))) +
  facet_wrap(vars(Model),nrow=1) +
  plot_theme_white
plot(g_fig_4b)


# fig. 4c -----------------------------------------------------------------

df_fig_4c <- df_fig_4b %>% 
  group_by(Model,Scenario,Region,Unit,Variable) %>% 
  reframe(value=sum(value))

g_fig_4c <- df_fig_4c %>% 
  filter(Scenario%in%c('Opt1.5C','ZF2100','ZF2050')) %>% 
  mutate(Variable=factor(Variable,levels=rev(var_emi$Variable)),
         Scenario=factor(Scenario,levels=c('Opt1.5C','ZF2100','ZF2050'))) %>%
  replace_na(list(value=0)) %>% 
  ggplot() +
  geom_hline(yintercept=0,linewidth=0.1,color='grey15',linetype='dashed') +
  geom_bar(aes(x=Scenario,y=value/1000,fill=Variable),stat='identity') +
  geom_point(data=. %>% 
               filter(Scenario%in%c('Opt1.5C','ZF2100','ZF2050')) %>% 
               mutate(Variable='Net CO2 (Energy + AFOLU)') %>% 
               replace_na(list(value=0)) %>% 
               group_by(Model,Scenario,Variable,Region,Unit) %>% 
               reframe(value=sum(value)),
             aes(x=Scenario,y=value/1000,shape=Variable),size=.9,color='black',fill='white',stroke=0.4) +
  geom_point(data=. %>% 
               filter(Scenario%in%c('Opt1.5C','ZF2100','ZF2050'),
                      Variable!='Emissions|CO2|AFOLU') %>% 
               mutate(Variable='Net CO2 (Energy)') %>% 
               replace_na(list(value=0)) %>% 
               group_by(Model,Scenario,Variable,Region,Unit) %>% 
               reframe(value=sum(value)),
             aes(x=Scenario,y=value/1000,shape=Variable),size=.9,color='black',fill='white',stroke=0.4) +
  scale_fill_manual(values=plt$Color,labels=plt$Legend) +
  scale_shape_manual(values=c('Net CO2 (Energy + AFOLU)'=21,'Net CO2 (Energy)'=25),
                     labels=c('Net CO2 (Energy + AFOLU)'=expression(paste('Net ',{CO[2]},' (Energy + AFOLU)')),
                              'Net CO2 (Energy)'=expression(paste('Net ',{CO[2]},' (Energy)')))) +
  guides(
    fill=guide_legend(order=1),
    shape=guide_legend(order=2)
  ) +
  labs(x='',y=expression(paste('Cumulative ',{CO[2]},' emission (Gt-',{CO[2]},')'))) +
  facet_wrap(vars(Model)) +
  plot_theme_white
plot(g_fig_4c)


# fig. 4 ------------------------------------------------------------------

l_fig_4a <- g_legend(g_fig_4a)
l_fig_4bc_1 <- g_legend(g_fig_4b+guides(fill=guide_legend(nrow=2),shape='none'))
l_fig_4bc_2 <- g_legend(g_fig_4b+guides(fill='none',shape=guide_legend(nrow=2)))

g_fig_4 <-ggdraw() +
  draw_plot(g_fig_4a+theme(legend.position='none'),x=0,y=.15,width=.45,height=.8) +
  draw_plot(l_fig_4a,x=.425,y=.55,width=.15,height=.4) +
  draw_plot(l_fig_1de,x=.4575,y=.2,width=.075,height=.35) +
  draw_plot(g_fig_4b+theme(legend.position='none'),x=.575,y=.15,width=.2,height=.8) +
  draw_plot(g_fig_4c+theme(legend.position='none'),x=.785,y=.15,width=.2,height=.8) +
  draw_plot(l_fig_4bc_1,x=0,y=0,width=.8,height=.15) +
  draw_plot(l_fig_4bc_2,x=.75,y=0,width=.1,height=.15) +
  draw_plot_label(
    label=c('a','b','c'),
    x=c(0,.575,.785),
    y=c(1,1,1),
    size=8
  )
plot(g_fig_4)

ggsave('output/1st_submission/fig_4.png',g_fig_4,width=180,height=65,units='mm',dpi=500)



# fig. 5a -----------------------------------------------------------------

df_fig_5a <- df_load %>% 
  mutate(Model=factor(Model,levels=c('MESSAGEix','AIM'))) %>% 
  filter(Variable%in%c('Capacity Additions|Electricity|Solar',
                       'Capacity Additions|Electricity|Wind')) %>% 
  group_by(Model,Scenario,Region,Variable,Unit) %>% 
  complete(Year=seq(2020,2100)) %>%
  mutate(value=na_locf(value,option='nocb')) %>% 
  filter(Year>=2021,Year<=2050) %>% 
  group_by(Model,Scenario,Region,Unit) %>% 
  reframe(value=sum(value)) %>% 
  pivot_wider(names_from=Scenario,values_from=value) %>% 
  mutate(across(-c(Model,Region,Unit),~(.-`Opt1.5C`)/`Opt1.5C`)) %>% 
  pivot_longer(cols=-c(Model,Region,Unit),
               names_to='Scenario',values_to='value')

df_fig_5a_minmax <- df_fig_5a %>% 
  filter(Scenario!='Opt1.5C') %>% 
  group_by(Model,Region,Unit) %>% 
  reframe(vmax=max(value),vmin=min(value))

fig_5a_max <- df_fig_5a_minmax %>% 
  pivot_longer(cols=-c(Model,Region,Unit),names_to='Variable',values_to='value') %>% 
  reframe(value=max(abs(value)))

g_fig_5a  <- df_fig_5a %>% 
  ggplot() +
  geom_hline(yintercept=0,linewidth=0.1,color='grey15',linetype='dashed') +
  geom_crossbar(data=df_fig_5a_minmax,
                aes(x=Model,ymin=vmin,ymax=vmax,y=vmax,fill=Model),width=.7,color='black',fatten=0,linewidth=0.15,alpha=0.7)+
  geom_point(aes(x=Model,y=value,shape=Scenario,size=Scenario),stroke=0.3,size=1,color='black') +
  labs(x='',y='',title='Solar and Wind power capacity addition in the 2020–2050 period (% change from Opt1.5C)') +
  scale_fill_manual(values=c('AIM'='#349b8d','MESSAGEix'='#ff7f0e')) +
  scale_shape_manual(values=c('Opt1.5C'=4,'ZF2100'=2,'ZF2050'=21,
                              'ZF2060'=0,'ZF2070'=8,'ZF2080'=5,'ZF2090'=3)) +
  scale_x_discrete(labels=c('M','A'),position='top') +
  scale_y_continuous(labels=scales::percent,limits=c(-fig_5a_max$value,fig_5a_max$value)) +
  coord_flip() +
  plot_theme_white_rect +
  theme(legend.position='none',
        panel.background=element_rect(color=NA,fill='white'),
        plot.background=element_rect(color=NA,fill='#f2efe6'))
plot(g_fig_5a)


# fig. 5b -----------------------------------------------------------------

df_fig_5b <- df_load %>% 
  mutate(Model=factor(Model,levels=c('MESSAGEix','AIM'))) %>% 
  filter(Variable%in%c('Investment|Energy Supply')) %>% 
  group_by(Model,Scenario,Region,Variable,Unit) %>% 
  complete(Year=seq(2020,2100)) %>%
  mutate(value=na_locf(value,option='nocb')) %>% 
  filter(Year>=2021) %>% 
  mutate(discount=exp(-(Year-2021)*log(1+0.05)),
         value_discounted=value*discount) %>% 
  group_by(Model,Scenario,Region,Unit) %>% 
  reframe(value=sum(value_discounted)) %>% 
  pivot_wider(names_from=Scenario,values_from=value) %>% 
  mutate(across(-c(Model,Region,Unit),~(.-`Opt1.5C`)/`Opt1.5C`)) %>% 
  pivot_longer(cols=-c(Model,Region,Unit),
               names_to='Scenario',values_to='value')

df_fig_5b_minmax <- df_fig_5b %>% 
  filter(Scenario!='Opt1.5C') %>% 
  group_by(Model,Region,Unit) %>% 
  reframe(vmax=max(value),vmin=min(value))

fig_5b_max <- df_fig_5b_minmax %>% 
  pivot_longer(cols=-c(Model,Region,Unit),names_to='Variable',values_to='value') %>% 
  reframe(value=max(abs(value)))

g_fig_5b <- df_fig_5b %>% 
  ggplot() +
  geom_hline(yintercept=0,linewidth=0.1,color='grey15',linetype='dashed') +
  geom_crossbar(data=df_fig_5b_minmax,
                aes(x=Model,ymin=vmin,ymax=vmax,y=vmax,fill=Model),width=.7,color='black',fatten=0,linewidth=0.15,alpha=0.7)+
  geom_point(aes(x=Model,y=value,shape=Scenario,size=Scenario),stroke=0.3,size=1,color='black') +
  labs(x='',y='',title='Investment in energy supply sectors in the 2020–2100 period (% change from Opt1.5C)') +
  scale_fill_manual(values=c('AIM'='#349b8d','MESSAGEix'='#ff7f0e')) +
  scale_shape_manual(values=c('Opt1.5C'=4,'ZF2100'=2,'ZF2050'=21,
                              'ZF2060'=0,'ZF2070'=8,'ZF2080'=5,'ZF2090'=3)) +
  scale_x_discrete(labels=c('M','A'),position='top') +
  scale_y_continuous(labels=scales::percent,limits=c(-fig_5b_max$value,fig_5b_max$value)) +
  coord_flip() +
  plot_theme_white_rect +
  theme(legend.position='none',
        panel.background=element_rect(color=NA,fill='white'),
        plot.background=element_rect(color=NA,fill='#f2efe6'))
plot(g_fig_5b)


# fig. 5c -----------------------------------------------------------------

df_fig_5c <- df_load %>%
  mutate(Model=factor(Model,levels=c('MESSAGEix','AIM'))) %>% 
  filter(Variable%in%c('Stranded Capacity|Electricity|Coal')) %>% 
  group_by(Model,Scenario,Region,Variable,Unit) %>% 
  complete(Year=seq(2020,2100)) %>%
  mutate(value=na_locf(value,'nocb')) %>% 
  filter(Year>=2021,Year<=2050) %>% 
  group_by(Model,Scenario,Region,Unit) %>% 
  reframe(value=sum(value)) %>% 
  pivot_wider(names_from=Scenario,values_from=value) %>% 
  mutate(across(-c(Model,Region,Unit),~(.-`Opt1.5C`)/`Opt1.5C`)) %>% 
  pivot_longer(cols=-c(Model,Region,Unit),
               names_to='Scenario',values_to='value') 

df_fig_5c_minmax <- df_fig_5c %>% 
  filter(Scenario!='Opt1.5C') %>% 
  group_by(Model,Region,Unit) %>% 
  reframe(vmax=max(value),vmin=min(value))

fig_5c_max <- df_fig_5c_minmax %>% 
  pivot_longer(cols=-c(Model,Region,Unit),names_to='Variable',values_to='value') %>% 
  reframe(value=max(abs(value)))

g_fig_5c <- df_fig_5c %>% 
  arrange(Model) %>% 
  ggplot() +
  geom_hline(yintercept=0,linewidth=0.1,color='grey15',linetype='dashed') +
  geom_crossbar(data=df_fig_5c_minmax,
                aes(x=Model,ymin=vmin,ymax=vmax,y=vmax,fill=Model),width=.7,color='black',fatten=0,linewidth=0.15,alpha=0.7)+
  geom_point(aes(x=Model,y=value,shape=Scenario,size=Scenario),stroke=0.3,size=1,color='black') +
  labs(x='',y='',title='Stranded capacity of coal power plants in the 2020–2050 period (% change from Opt1.5C)') +
  scale_fill_manual(values=c('AIM'='#349b8d','MESSAGEix'='#ff7f0e')) +
  scale_shape_manual(values=c('Opt1.5C'=4,'ZF2100'=2,'ZF2050'=21,
                              'ZF2060'=0,'ZF2070'=8,'ZF2080'=5,'ZF2090'=3)) +
  scale_x_discrete(labels=c('M','A'),position='top') +
  scale_y_continuous(labels=scales::percent,limits=c(-fig_5c_max$value,fig_5c_max$value)) +
  coord_flip() +
  plot_theme_white_rect +
  theme(legend.position='none',
        panel.background=element_rect(color=NA,fill='white'),
        plot.background=element_rect(color=NA,fill='#f2efe6'))
plot(g_fig_5c)


# fig. 5d -----------------------------------------------------------------

df_fig_5d <- df_load %>% 
  mutate(Model=factor(Model,levels=c('MESSAGEix','AIM'))) %>% 
  filter(Variable%in%c('Investment|Energy Demand|Efficiency and Decarbonization')) %>% 
  group_by(Model,Scenario,Region,Variable,Unit) %>% 
  complete(Year=seq(2020,2100)) %>%
  mutate(value=na_interpolation(value)) %>% 
  filter(Year>=2021) %>% 
  mutate(discount=exp(-(Year-2021)*log(1+0.05)),
         value_discounted=value*discount) %>% 
  group_by(Model,Scenario,Region,Unit) %>% 
  reframe(value=sum(value_discounted)) %>% 
  pivot_wider(names_from=Scenario,values_from=value) %>% 
  mutate(across(-c(Model,Region,Unit),~(.-`Opt1.5C`)/`Opt1.5C`)) %>% 
  pivot_longer(cols=-c(Model,Region,Unit),
               names_to='Scenario',values_to='value')

df_fig_5d_minmax <- df_fig_5d %>%
  filter(Scenario!='Opt1.5C') %>% 
  group_by(Model,Region,Unit) %>% 
  reframe(vmax=max(value),vmin=min(value))

fig_5d_max <- df_fig_5d_minmax %>% 
  pivot_longer(cols=-c(Model,Region,Unit),names_to='Variable',values_to='value') %>% 
  reframe(value=max(abs(value)))

g_fig_5d <- df_fig_5d %>% 
  ggplot() +
  geom_hline(yintercept=0,linewidth=0.1,color='grey15',linetype='dashed') +
  geom_crossbar(data=df_fig_5d_minmax,
                aes(x=Model,ymin=vmin,ymax=vmax,y=vmax,fill=Model),width=.7,color='black',fatten=0,linewidth=0.15,alpha=0.7)+
  geom_point(aes(x=Model,y=value,shape=Scenario,size=Scenario),stroke=0.3,size=1,color='black') +
  labs(x='',y='',title='Investment in energy demand sectors in the 2020–2100 period (% change from Opt1.5C)') +
  scale_fill_manual(values=c('AIM'='#349b8d','MESSAGEix'='#ff7f0e')) +
  scale_shape_manual(values=c('Opt1.5C'=4,'ZF2100'=2,'ZF2050'=21,
                              'ZF2060'=0,'ZF2070'=8,'ZF2080'=5,'ZF2090'=3)) +
  scale_x_discrete(labels=c('A'),position='top') +
  scale_y_continuous(labels=scales::percent,limits=c(-fig_5d_max$value,fig_5d_max$value)) +
  coord_flip() +
  plot_theme_white_rect +
  theme(legend.position='none',
        panel.background=element_rect(color=NA,fill='white'),
        plot.background=element_rect(color=NA,fill='#f2efe6'))
plot(g_fig_5d)


# fig. 5e -----------------------------------------------------------------

df_fig_5e <- df_load %>%
  mutate(Model=factor(Model,levels=c('MESSAGEix','AIM'))) %>%
  filter(Variable%in%c('Final Energy|Electricity',
                       'Final Energy|Hydrogen',
                       'Final Energy|Heat',
                       'Final Energy|Solar')) %>%
  group_by(Model,Scenario,Region,Unit,Year) %>%
  reframe(value=sum(value),Variable='Final Energy|Non-Hydrocarbon') %>%
  group_by(Model,Scenario,Region,Variable,Unit) %>%
  complete(Year=seq(2020,2100)) %>%
  mutate(value=na_locf(value,'nocb')) %>%
  filter(Year>=2021,Year<=2050) %>%
  group_by(Model,Scenario,Region,Unit) %>%
  reframe(value=mean(value)) %>%
  pivot_wider(names_from=Scenario,values_from=value) %>%
  mutate(across(-c(Model,Region,Unit),~(.-`Opt1.5C`)/`Opt1.5C`)) %>%
  pivot_longer(cols=-c(Model,Region,Unit),
               names_to='Scenario',values_to='value')

df_fig_5e_minmax <- df_fig_5e %>%
  filter(Scenario!='Opt1.5C') %>%
  group_by(Model,Region,Unit) %>%
  reframe(vmax=max(value),vmin=min(value))

fig_5e_max <- df_fig_5e_minmax %>%
  pivot_longer(cols=-c(Model,Region,Unit),names_to='Variable',values_to='value') %>%
  reframe(value=max(abs(value)))

g_fig_5e <- df_fig_5e %>% 
  ggplot() +
  geom_hline(yintercept=0,linewidth=0.1,color='grey15',linetype='dashed') + 
  geom_crossbar(data=df_fig_5e_minmax,
                aes(x=Model,ymin=vmin,ymax=vmax,y=vmax,fill=Model),width=.7,color='black',fatten=0,linewidth=0.15,alpha=0.7)+
  geom_point(aes(x=Model,y=value,shape=Scenario),stroke=0.3,size=1,color='black') +
  labs(x='',y='',title='Non-hydrocarbon final energy in the 2020–2050 period (% change from Opt1.5C)') +
  scale_fill_manual(values=c('AIM'='#349b8d','MESSAGEix'='#ff7f0e')) +
  scale_shape_manual(values=c('Opt1.5C'=4,'ZF2100'=2,'ZF2050'=21,
                              'ZF2060'=0,'ZF2070'=8,'ZF2080'=5,'ZF2090'=3)) +
  scale_x_discrete(labels=c('M','A'),position='top') +
  scale_y_continuous(labels=scales::percent,limits=c(-fig_5e_max$value,fig_5e_max$value)) +
  coord_flip() +
  plot_theme_white_rect +
  theme(legend.position='none',
        panel.background=element_rect(color=NA,fill='white'),
        plot.background=element_rect(color=NA,fill='#f2efe6'))
plot(g_fig_5e)


# fig. 5f -----------------------------------------------------------------

df_fig_5f <- df_load %>% 
  mutate(Model=factor(Model,levels=c('MESSAGEix','AIM'))) %>% 
  filter(Variable=='CCUS|Geological Storage') %>% 
  group_by(Model,Scenario,Region,Variable,Unit) %>% 
  complete(Year=seq(2020,2100)) %>%
  mutate(value=na_locf(value,'nocb')) %>% 
  filter(Year>=2021) %>% 
  group_by(Model,Scenario,Region,Unit) %>% 
  reframe(value=sum(value)) %>% 
  pivot_wider(names_from=Scenario,values_from=value) %>% 
  mutate(across(-c(Model,Region,Unit),~(.-`Opt1.5C`)/`Opt1.5C`)) %>% 
  pivot_longer(cols=-c(Model,Region,Unit),
               names_to='Scenario',values_to='value')

df_fig_5f_minmax <- df_fig_5f %>% 
  filter(Scenario!='Opt1.5C') %>% 
  group_by(Model,Region,Unit) %>% 
  reframe(vmax=max(value),vmin=min(value))

fig_5f_max <- df_fig_5f_minmax %>% 
  pivot_longer(cols=-c(Model,Region,Unit),names_to='Variable',values_to='value') %>% 
  reframe(value=max(abs(value)))

g_fig_5f <- df_fig_5f %>% 
  ggplot() +
  geom_hline(yintercept=0,linewidth=0.1,color='grey15',linetype='dashed') +
  geom_crossbar(data=df_fig_5f_minmax,
                aes(x=Model,ymin=vmin,ymax=vmax,y=vmax,fill=Model),width=.7,color='black',fatten=0,linewidth=0.15,alpha=0.7)+
  geom_point(aes(x=Model,y=value,shape=Scenario),stroke=0.3,size=1,color='black') +
  labs(x='',y='',title=expression(paste('Geological ',{CO[2]},' storage in the 2020–2100 period (% change from Opt1.5C)'))) +
  scale_fill_manual(values=c('AIM'='#349b8d','MESSAGEix'='#ff7f0e')) +
  scale_shape_manual(values=c('Opt1.5C'=4,'ZF2100'=2,'ZF2050'=21,
                              'ZF2060'=0,'ZF2070'=8,'ZF2080'=5,'ZF2090'=3)) +
  scale_x_discrete(labels=c('M','A'),position='top') +
  scale_y_continuous(labels=scales::percent,limits=c(-fig_5f_max$value,fig_5f_max$value)) +
  coord_flip() +
  plot_theme_white_rect +
  theme(legend.position='none',
        panel.background=element_rect(color=NA,fill='white'),
        plot.background=element_rect(color=NA,fill='#e7e6f2'))
plot(g_fig_5f)


# fig. 5g -----------------------------------------------------------------

df_fig_5g <- df_load %>% 
  mutate(Model=factor(Model,levels=c('MESSAGEix','AIM'))) %>% 
  filter(Variable%in%c('CCUS|Geological Storage|Biomass','CCUS|Geological Storage|Direct Air Capture')) %>% 
  group_by(Model,Scenario,Region,Variable,Unit) %>% 
  complete(Year=seq(2020,2100)) %>%
  mutate(value=na_locf(value,'nocb')) %>% 
  filter(Year>=2021) %>% 
  group_by(Model,Scenario,Region,Unit) %>% 
  reframe(value=sum(value)) %>% 
  pivot_wider(names_from=Scenario,values_from=value) %>% 
  mutate(across(-c(Model,Region,Unit),~(.-`Opt1.5C`)/`Opt1.5C`)) %>% 
  pivot_longer(cols=-c(Model,Region,Unit),
               names_to='Scenario',values_to='value')

df_fig_5g_minmax <- df_fig_5g %>% 
  filter(Scenario!='Opt1.5C') %>% 
  group_by(Model,Region,Unit) %>% 
  reframe(vmax=max(value),vmin=min(value))

fig_5g_max <- df_fig_5g_minmax %>% 
  pivot_longer(cols=-c(Model,Region,Unit),names_to='Variable',values_to='value') %>% 
  reframe(value=max(abs(value)))

g_fig_5g <- df_fig_5g %>% 
  ggplot() +
  geom_hline(yintercept=0,linewidth=0.1,color='grey15',linetype='dashed') +
  geom_crossbar(data=df_fig_5g_minmax,
                aes(x=Model,ymin=vmin,ymax=vmax,y=vmax,fill=Model),width=.7,color='black',fatten=0,linewidth=0.15,alpha=0.7)+
  geom_point(aes(x=Model,y=value,shape=Scenario),stroke=0.3,size=1,color='black') +
  labs(x='',y='',title=expression(paste('CDR in the 2020–2100 period (% change from Opt1.5C)'))) +
  scale_fill_manual(values=c('AIM'='#349b8d','MESSAGEix'='#ff7f0e')) +
  scale_shape_manual(values=c('Opt1.5C'=4,'ZF2100'=2,'ZF2050'=21,
                              'ZF2060'=0,'ZF2070'=8,'ZF2080'=5,'ZF2090'=3)) +
  scale_x_discrete(labels=c('M','A'),position='top') +
  scale_y_continuous(labels=scales::percent,limits=c(-fig_5g_max$value,fig_5g_max$value)) +
  coord_flip() +
  plot_theme_white_rect +
  theme(legend.position='none',
        panel.background=element_rect(color=NA,fill='white'),
        plot.background=element_rect(color=NA,fill='#e7e6f2'))
plot(g_fig_5g)


# fig. 5h -----------------------------------------------------------------

df_fig_5h <- df_load %>% 
  mutate(Model=factor(Model,levels=c('MESSAGEix','AIM'))) %>% 
  filter(Variable%in%c('Primary Energy|Biomass')) %>% 
  group_by(Model,Scenario,Region,Variable,Unit) %>% 
  complete(Year=seq(2020,2100)) %>%
  mutate(value=na_locf(value,'nocb')) %>% 
  filter(Year>=2021) %>% 
  group_by(Model,Scenario,Region,Unit) %>% 
  reframe(value=mean(value)) %>% 
  pivot_wider(names_from=Scenario,values_from=value) %>% 
  mutate(across(-c(Model,Region,Unit),~(.-`Opt1.5C`)/`Opt1.5C`)) %>% 
  pivot_longer(cols=-c(Model,Region,Unit),
               names_to='Scenario',values_to='value')

df_fig_5h_minmax <- df_fig_5h %>% 
  filter(Scenario!='Opt1.5C') %>% 
  group_by(Model,Region,Unit) %>% 
  reframe(vmax=max(value),vmin=min(value))

fig_5h_max <- df_fig_5h_minmax %>% 
  pivot_longer(cols=-c(Model,Region,Unit),names_to='Variable',values_to='value') %>% 
  reframe(value=max(abs(value)))

g_fig_5h <- df_fig_5h %>% 
  ggplot() +
  geom_hline(yintercept=0,linewidth=0.1,color='grey15',linetype='dashed') + 
  geom_crossbar(data=df_fig_5h_minmax,
                aes(x=Model,ymin=vmin,ymax=vmax,y=vmax,fill=Model),width=.7,color='black',fatten=0,linewidth=0.15,alpha=0.7)+
  geom_point(aes(x=Model,y=value,shape=Scenario),stroke=0.3,size=1,color='black') +
  labs(x='',y='',title=expression(paste('Biomass primary energy in the 2020–2100 period (% change from Opt1.5C)'))) +
  scale_fill_manual(values=c('AIM'='#349b8d','MESSAGEix'='#ff7f0e')) +
  scale_shape_manual(values=c('Opt1.5C'=4,'ZF2100'=2,'ZF2050'=21,
                              'ZF2060'=0,'ZF2070'=8,'ZF2080'=5,'ZF2090'=3)) +
  scale_x_discrete(labels=c('M','A'),position='top') +
  scale_y_continuous(labels=scales::percent,limits=c(-fig_5h_max$value,fig_5h_max$value)) +
  coord_flip() +
  plot_theme_white_rect +
  theme(legend.position='none',
        panel.background=element_rect(color=NA,fill='white'),
        plot.background=element_rect(color=NA,fill='#ebf2e6'))
plot(g_fig_5h)


# fig. 5i -----------------------------------------------------------------

df_fig_5i <- df_fig_4c %>% 
  mutate(Model=factor(Model,levels=c('MESSAGEix','AIM'))) %>% 
  filter(Variable%in%c('Emissions|CO2|AFOLU')) %>%
  select(-Variable) %>% 
  pivot_wider(names_from=Scenario,values_from=value) %>% 
  mutate(across(-c(Model,Region,Unit),~(-.+`Opt1.5C`)/-`Opt1.5C`)) %>% 
  pivot_longer(cols=-c(Model,Region,Unit),
               names_to='Scenario',values_to='value')

df_fig_5i_minmax <- df_fig_5i %>% 
  filter(Scenario!='Opt1.5C') %>% 
  group_by(Model,Region,Unit) %>% 
  reframe(vmax=max(value),vmin=min(value))

fig_5i_max <- df_fig_5i_minmax %>% 
  pivot_longer(cols=-c(Model,Region,Unit),names_to='Variable',values_to='value') %>% 
  reframe(value=max(abs(value)))

g_fig_5i <- df_fig_5i %>% 
  ggplot() +
  geom_hline(yintercept=0,linewidth=0.1,color='grey15',linetype='dashed') +
  geom_crossbar(data=df_fig_5i_minmax,
                aes(x=Model,ymin=vmin,ymax=vmax,y=vmax,fill=Model),width=.7,color='black',fatten=0,linewidth=0.15,alpha=0.7)+
  geom_point(aes(x=Model,y=value,shape=Scenario),stroke=0.3,size=1,color='black') +
  labs(x='',y='',title=expression(paste('Negative ',{CO[2]},' emission from AFOLU sector in the 2020–2100 period (% change from Opt1.5C)'))) +
  scale_fill_manual(values=c('AIM'='#349b8d','MESSAGEix'='#ff7f0e')) +
  scale_shape_manual(values=c('Opt1.5C'=4,'ZF2100'=2,'ZF2050'=21,
                              'ZF2060'=0,'ZF2070'=8,'ZF2080'=5,'ZF2090'=3)) +
  scale_x_discrete(labels=c('M','A'),position='top') +
  scale_y_continuous(labels=scales::percent,limits=c(-fig_5i_max$value,fig_5i_max$value)) +
  coord_flip() +
  plot_theme_white_rect +
  theme(panel.background=element_rect(color=NA,fill='white'),
        plot.background=element_rect(color=NA,fill='#ebf2e6'))
plot(g_fig_5i)


# fig. 5 ------------------------------------------------------------------

l_fig_5_1 <- g_legend(g_fig_5a+
                     theme(legend.position='bottom',legend.margin=margin(0,0,0,-5))+
                     guides(fill='none',shape=guide_legend(nrow=1)))
plot(l_fig_5_1)

l_fig_5_2 <- ggplot() +
  geom_crossbar(aes(x=1,xmin=1,xmax=2,y=0),fill='#349b8d',width=0.4,color='black',fatten=0,linewidth=0.15,alpha=0.7)+
  geom_crossbar(aes(x=5,xmin=5,xmax=6,y=0),fill='#ff7f0e',width=0.4,color='black',fatten=0,linewidth=0.15,alpha=0.7)+
  annotate("text",x=2,y=0,label="AIM (A)",size=1.75,hjust=-.2) +
  annotate("text",x=6,y=0,label = "MESSAGEix (M)",size=1.75,hjust=-.1) +
  xlim(-2,10)+
  ylim(-1,1)+
  theme_void()
plot(l_fig_5_2)

header_fig_5 <- ggplot() +
  geom_segment(
    aes(x=1,xend=5,y=1,yend=1),
    arrow = arrow(length = unit(0.15, "cm")),
    linewidth=0.25,
    color = "black"
  ) +
  annotate("text",x=1,y=1,label="Low challenges",size=2.25,hjust=1.2,fontface=2) +
  annotate("text",x=5,y=1,label = "High challenges",size=2.25,hjust=-0.2,fontface=2) +
  xlim(-1,7) +
  theme_void() 
plot(header_fig_5)

g_fig_5 <-ggdraw() +
  draw_plot(g_fig_5a+plot_theme_white_rect+theme(panel.background=element_rect(color=NA,fill='white'),plot.background=element_rect(color=NA,fill='#f2efe6')),
            x=0,y=0.78,width=1,height=.09) +
  draw_plot(g_fig_5b+plot_theme_white_rect+theme(panel.background=element_rect(color=NA,fill='white'),plot.background=element_rect(color=NA,fill='#f2efe6')),
            x=0,y=0.69,width=1,height=.09) +
  draw_plot(g_fig_5c+plot_theme_white_rect+theme(panel.background=element_rect(color=NA,fill='white'),plot.background=element_rect(color=NA,fill='#f2efe6')),
            x=0,y=0.6,width=1,height=.09) +
  draw_plot(g_fig_5d+plot_theme_white_rect+theme(panel.background=element_rect(color=NA,fill='white'),plot.background=element_rect(color=NA,fill='#f2daef')),
            x=0,y=0.53,width=1,height=.073) +
  draw_plot(g_fig_5e+plot_theme_white_rect+theme(panel.background=element_rect(color=NA,fill='white'),plot.background=element_rect(color=NA,fill='#f2daef')),
            x=0,y=0.44,width=1,height=.09) +
  draw_plot(g_fig_5f+plot_theme_white_rect+theme(panel.background=element_rect(color=NA,fill='white'),plot.background=element_rect(color=NA,fill='#e7e6f2')),
            x=0,y=0.35,width=1,height=.09) +
  draw_plot(g_fig_5g+plot_theme_white_rect+theme(panel.background=element_rect(color=NA,fill='white'),plot.background=element_rect(color=NA,fill='#e7e6f2')),
            x=0,y=0.26,width=1,height=.09) +
  draw_plot(g_fig_5h+plot_theme_white_rect+theme(panel.background=element_rect(color=NA,fill='white'),plot.background=element_rect(color=NA,fill='#ebf2e6')),
            x=0,y=0.17,width=1,height=.09) +
  draw_plot(g_fig_5i+plot_theme_white_rect+theme(panel.background=element_rect(color=NA,fill='white'),plot.background=element_rect(color=NA,fill='#ebf2e6')),
            x=0,y=0.08,width=1,height=.09) +
  draw_plot(l_fig_5_1,
            x=0,y=0.04,width=1,height=.04) +
  draw_plot(l_fig_5_2,
            x=0.1,y=0.01,width=0.8,height=.04) +
  draw_plot(header_fig_5,
            x=0.005,y=0.87,width=1,height=.04) +
  draw_text('Energy supply',angle=90,size=6.5,x=0.015,y=0.745) +
  draw_text('Energy demand',angle=90,size=6.5,x=0.015,y=0.525) +
  draw_text('CCS',angle=90,size=6.5,x=0.015,y=0.35) +
  draw_text('Land use',angle=90,size=6.5,x=0.015,y=0.17) +
  draw_plot_label(
    label=c('a','b','c','d','e','f','g','h','i'),
    x=c(0.045,0.045,0.045,0.045,0.045,0.045,0.045,0.045,0.045),
    y=c(0.87,0.78,0.69,0.6,0.53,0.44,0.35,0.26,0.17),
    size=7
  )
plot(g_fig_5)

ggsave('output/1st_submission/fig_5.png',g_fig_5,width=90,height=150,units='mm',dpi=500)


# fig. S1a ----------------------------------------------------------------

g_fig_S1a <- df_fig_1a %>% 
  filter(Year==2100) %>% 
  mutate(Scenario=factor(Scenario,levels=c('Opt1.5C','ZF2100','ZF2090','ZF2080','ZF2070','ZF2060','ZF2050'))) %>% 
  arrange(Model,Scenario) %>%
  ggtern(aes(Fossil,`Biomass`,`Nuclear + Non-biomass renewables`)) +
  geom_mask() +
  geom_point(data=df_fig_1a_AR6 %>% filter(Year==2100,Category=='C2'),color='grey70',size=0.7,shape=1,stroke=0.4) +
  geom_point(data=df_fig_1a_AR6 %>% filter(Year==2100,Category=='C1'),color='grey70',size=0.7,shape=4,stroke=0.4) +
  geom_point(aes(color=Model,shape=Scenario,size=Scenario),stroke=0.5) +
  annotate("text", x = 0.85, y = 0.075, z = 0.075, hjust=0.1,
           label = paste0('C1: n=', df_fig_1a_AR6 %>% filter(Year == 2100, Category%in%c('C1')) %>% nrow()), 
           size = 1.75) +
  annotate("text", x = 0.675, y = 0.075, z = 0.25, hjust=0.1,
           label = paste0('C2: n=', df_fig_1a_AR6 %>% filter(Year == 2100, Category%in%c('C2')) %>% nrow()), 
           size = 1.75) +
  geom_path(aes(color=Model),linewidth=0.25) +
  scale_color_manual(values=c('AIM'='#349b8d','MESSAGEix'='#ff7f0e')) +
  scale_shape_manual(values=c('Opt1.5C'=4,'ZF2100'=2,'ZF2050'=21,
                              'ZF2060'=0,'ZF2070'=8,'ZF2080'=5,'ZF2090'=3)) +
  scale_size_manual(values=c('Opt1.5C'=1.5,'ZF2100'=1.5,'ZF2050'=1.5,
                             'ZF2060'=1.5,'ZF2070'=1.5,'ZF2080'=1.5,'ZF2090'=1.5)) +
  labs(
    x='',y='',z='',
    Tarrow='Biomass(%)',
    Larrow='Fossil (%)',
    Rarrow='Nuclear & non-biomass renewables (%)'
  ) +
  plot_theme_ternary +
  theme(
    plot.margin=margin(-15,-30,-10,-30)
  ) +
  theme_showarrows() +
  theme_clockwise() +
  theme_ticksinside() 
plot(g_fig_S1a)


# fig. S1b ----------------------------------------------------------------

g_fig_S1b <- df_fig_1b %>% 
  filter(Year==2100) %>% 
  mutate(Scenario=factor(Scenario,levels=c('Opt1.5C','ZF2100','ZF2090','ZF2080','ZF2070','ZF2060','ZF2050'))) %>% 
  arrange(Model,Scenario) %>% 
  ggtern(aes(Fossil,`Biomass`,`Nuclear + Non-biomass renewables`)) +
  geom_mask() +
  geom_point(data=df_fig_1b_AR6 %>% filter(Year==2100,Category%in%c('C2')),color='grey70',size=0.7,shape=1,stroke=0.4) +
  geom_point(data=df_fig_1b_AR6 %>% filter(Year==2100,Category%in%c('C1')),color='grey70',size=0.7,shape=4,stroke=0.4) +
  geom_point(aes(color=Model,shape=Scenario,size=Scenario),stroke=0.5) +
  annotate("text", x = 0.85, y = 0.075, z = 0.075, hjust=0.1,
           label = paste0('C1: n=', df_fig_1b_AR6 %>% filter(Year == 2100, Category%in%c('C1')) %>% nrow()), 
           size = 1.75) +
  annotate("text", x = 0.675, y = 0.075, z = 0.25, hjust=0.1,
           label = paste0('C2: n=', df_fig_1b_AR6 %>% filter(Year == 2100, Category%in%c('C2')) %>% nrow()), 
           size = 1.75) +
  geom_path(aes(color=Model),linewidth=0.25) +
  scale_color_manual(values=c('AIM'='#349b8d','MESSAGEix'='#ff7f0e')) +
  scale_shape_manual(values=c('Opt1.5C'=4,'ZF2100'=2,'ZF2050'=21,
                              'ZF2060'=0,'ZF2070'=8,'ZF2080'=5,'ZF2090'=3)) +
  scale_size_manual(values=c('Opt1.5C'=1.5,'ZF2100'=1.5,'ZF2050'=1.5,
                             'ZF2060'=1.5,'ZF2070'=1.5,'ZF2080'=1.5,'ZF2090'=1.5)) +
  labs(
    x='',y='',z='',
    Tarrow='Biomass (%)',
    Larrow='Fossil (%)',
    Rarrow='Nuclear & non-biomass renewables (%)'
  ) +
  plot_theme_ternary +
  theme(
    plot.margin=margin(-15,-30,-10,-30)
  ) +
  theme_showarrows() +
  theme_clockwise() +
  theme_ticksinside()
plot(g_fig_S1b)


# fig. S1c ----------------------------------------------------------------

g_fig_S1c <- df_fig_1c %>%
  filter(Year==2100) %>%
  mutate(Scenario=factor(Scenario,levels=c('Opt1.5C','ZF2100','ZF2090','ZF2080','ZF2070','ZF2060','ZF2050'))) %>%
  arrange(Model,Scenario) %>%
  ggtern(aes(Solids,`Liquid & gases`,`Electricity, hydrogen & others`)) +
  geom_mask() +
  geom_point(data=df_fig_1c_AR6 %>% filter(Year==2100,Category%in%c('C2')),color='grey70',size=0.7,shape=1,stroke=0.4) +
  geom_point(data=df_fig_1c_AR6 %>% filter(Year==2100,Category%in%c('C1')),color='grey70',size=0.7,shape=4,stroke=0.4) +
  geom_point(aes(color=Model,shape=Scenario,size=Scenario),stroke=0.5) +
  annotate("text", x = 0.85, y = 0.075, z = 0.075, hjust=0.1,
           label = paste0('C1: n=', df_fig_1c_AR6 %>% filter(Year == 2100, Category%in%c('C1')) %>% nrow()), 
           size = 1.75) +
  annotate("text", x = 0.675, y = 0.075, z = 0.25, hjust=0.1,
           label = paste0('C2: n=', df_fig_1c_AR6 %>% filter(Year == 2100, Category%in%c('C2')) %>% nrow()), 
           size = 1.75) +
  geom_path(aes(color=Model),linewidth=0.25) +
  scale_color_manual(values=c('AIM'='#349b8d','MESSAGEix'='#ff7f0e')) +
  scale_shape_manual(values=c('Opt1.5C'=4,'ZF2100'=2,'ZF2050'=21,
                              'ZF2060'=0,'ZF2070'=8,'ZF2080'=5,'ZF2090'=3)) +
  scale_size_manual(values=c('Opt1.5C'=1.5,'ZF2100'=1.5,'ZF2050'=1.5,
                             'ZF2060'=1.5,'ZF2070'=1.5,'ZF2080'=1.5,'ZF2090'=1.5)) +
  labs(
    x='',y='',z='',
    Tarrow='Liquid & gases (%)',
    Larrow='Solids (%)',
    Rarrow='Electricity, hydrogen & others (%)'
  ) +
  plot_theme_ternary +
  theme(
    plot.margin=margin(-15,-30,-10,-30)
  ) +
  theme_showarrows() +
  theme_clockwise() +
  theme_ticksinside()
plot(g_fig_S1c)


# fig. S1d ----------------------------------------------------------------

df_fig_S1d <- df_load %>%
  filter(Variable%in%c('Primary Energy|Coal')) %>% 
  group_by(Model,Scenario,Region,Unit,Year) %>%
  reframe(value=sum(value))

df_fig_S1d_AR6 <- df_load_AR6 %>% 
  filter(Year%in%c(2030,2050,2070,2100),Variable%in%c('Primary Energy|Coal')) %>% 
  group_by(Region,Category,Case,Year) %>% 
  reframe(value=sum(value)) %>% 
  group_by(Region,Year) %>% 
  reframe(p50=median(value),p90=quantile(value,.9),p10=quantile(value,.1),p0=min(value),p100=max(value),n=n())

g_fig_S1d <- df_fig_S1d %>% 
  filter(Scenario%in%c('Opt1.5C','ZF2100','ZF2050')) %>% 
  arrange(Scenario) %>% 
  ggplot() +
  geom_hline(yintercept=0,linewidth=0.1,color='grey15',linetype='dashed') +
  geom_crossbar(data=df_fig_S1d_AR6,
                aes(x=Year,ymin=p10,ymax=p90,y=p50),width=4.5,color='white',fill='grey80',linewidth=0.25)+
  geom_crossbar(data=df_fig_S1d_AR6,
                aes(x=Year,ymin=p0,ymax=p100,y=p0),width=4.5,color='grey80',fill='transparent',fatten=0,linewidth=0.25)+
  geom_line(aes(x=Year,y=value,color=Model,group=interaction(Model,Scenario)),linewidth=0.2) +
  geom_point(data=. %>% filter(Year%in%c(seq(2020,2055,5),seq(2060,2100,10))&Model=='MESSAGEix'|Year%in%seq(2020,2100,5)&Model=='AIM'),
             aes(x=Year,y=value,shape=Scenario,color=Model,size=Scenario),stroke=0.4) +
  annotate("text", x=2020, y=0, hjust=0,vjust=0,
           label = paste0('n=', df_fig_S1d_AR6 %>% filter(Year == 2050) %>% pull(n)),
           size = 1.75) +
  labs(x='',y=expression(paste('Primary energy (EJ ',{yr^{-1}},')'))) +
  scale_color_manual(values=c('AIM'='#349b8d','MESSAGEix'='#ff7f0e')) +
  scale_size_manual(values=c('Opt1.5C'=1,'ZF2100'=.8,'ZF2050'=.8)) +
  scale_shape_manual(values=c('Opt1.5C'=4,'ZF2100'=2,'ZF2050'=21)) +
  plot_theme_white +
  theme(legend.position='none')
plot(g_fig_S1d)


# fig. S1e ----------------------------------------------------------------

df_fig_S1e <- df_load %>%
  filter(Variable%in%c('Primary Energy|Oil')) %>% 
  group_by(Model,Scenario,Region,Unit,Year) %>%
  reframe(value=sum(value))

df_fig_S1e_AR6 <- df_load_AR6 %>% 
  filter(Year%in%c(2030,2050,2070,2100),Variable%in%c('Primary Energy|Oil')) %>% 
  group_by(Region,Category,Case,Year) %>% 
  reframe(value=sum(value)) %>% 
  group_by(Region,Year) %>% 
  reframe(p50=median(value),p90=quantile(value,.9),p10=quantile(value,.1),p0=min(value),p100=max(value),n=n())

g_fig_S1e <- df_fig_S1e %>% 
  filter(Scenario%in%c('Opt1.5C','ZF2100','ZF2050')) %>% 
  arrange(Scenario) %>% 
  ggplot() +
  geom_hline(yintercept=0,linewidth=0.1,color='grey15',linetype='dashed') +
  geom_crossbar(data=df_fig_S1e_AR6,
                aes(x=Year,ymin=p10,ymax=p90,y=p50),width=4.5,color='white',fill='grey80',linewidth=0.25)+
  geom_crossbar(data=df_fig_S1e_AR6,
                aes(x=Year,ymin=p0,ymax=p100,y=p0),width=4.5,color='grey80',fill='transparent',fatten=0,linewidth=0.25)+
  geom_line(aes(x=Year,y=value,color=Model,group=interaction(Model,Scenario)),linewidth=0.2) +
  geom_point(data=. %>% filter(Year%in%c(seq(2020,2055,5),seq(2060,2100,10))&Model=='MESSAGEix'|Year%in%seq(2020,2100,5)&Model=='AIM'),
             aes(x=Year,y=value,shape=Scenario,color=Model,size=Scenario),stroke=0.4) +
  annotate("text", x=2020, y=0, hjust=0,vjust=0,
           label = paste0('n=', df_fig_S1e_AR6 %>% filter(Year == 2050) %>% pull(n)),
           size = 1.75) +
  labs(x='',y=expression(paste('Primary energy (EJ ',{yr^{-1}},')'))) +
  scale_color_manual(values=c('AIM'='#349b8d','MESSAGEix'='#ff7f0e')) +
  scale_size_manual(values=c('Opt1.5C'=1,'ZF2100'=.8,'ZF2050'=.8)) +
  scale_shape_manual(values=c('Opt1.5C'=4,'ZF2100'=2,'ZF2050'=21)) +
  plot_theme_white +
  theme(legend.position='none')
plot(g_fig_S1e)


# fig. S1f ----------------------------------------------------------------

df_fig_S1f <- df_load %>%
  filter(Variable%in%c('Primary Energy|Gas')) %>% 
  group_by(Model,Scenario,Region,Unit,Year) %>%
  reframe(value=sum(value))

df_fig_S1f_AR6 <- df_load_AR6 %>% 
  filter(Year%in%c(2030,2050,2070,2100),Variable%in%c('Primary Energy|Gas')) %>% 
  group_by(Region,Category,Case,Year) %>% 
  reframe(value=sum(value)) %>% 
  group_by(Region,Year) %>% 
  reframe(p50=median(value),p90=quantile(value,.9),p10=quantile(value,.1),p0=min(value),p100=max(value),n=n())

g_fig_S1f <- df_fig_S1f %>% 
  filter(Scenario%in%c('Opt1.5C','ZF2100','ZF2050')) %>% 
  arrange(Scenario) %>% 
  ggplot() +
  geom_hline(yintercept=0,linewidth=0.1,color='grey15',linetype='dashed') +
  geom_crossbar(data=df_fig_S1f_AR6,
                aes(x=Year,ymin=p10,ymax=p90,y=p50),width=4.5,color='white',fill='grey80',linewidth=0.25)+
  geom_crossbar(data=df_fig_S1f_AR6,
                aes(x=Year,ymin=p0,ymax=p100,y=p0),width=4.5,color='grey80',fill='transparent',fatten=0,linewidth=0.25)+
  geom_line(aes(x=Year,y=value,color=Model,group=interaction(Model,Scenario)),linewidth=0.2) +
  geom_point(data=. %>% filter(Year%in%c(seq(2020,2055,5),seq(2060,2100,10))&Model=='MESSAGEix'|Year%in%seq(2020,2100,5)&Model=='AIM'),
             aes(x=Year,y=value,shape=Scenario,color=Model,size=Scenario),stroke=0.4) +
  annotate("text", x=2020, y=0, hjust=0,vjust=0,
           label = paste0('n=', df_fig_S1f_AR6 %>% filter(Year == 2050) %>% pull(n)),
           size = 1.75) +
  labs(x='',y=expression(paste('Primary energy (EJ ',{yr^{-1}},')'))) +
  scale_color_manual(values=c('AIM'='#349b8d','MESSAGEix'='#ff7f0e')) +
  scale_size_manual(values=c('Opt1.5C'=1,'ZF2100'=.8,'ZF2050'=.8)) +
  scale_shape_manual(values=c('Opt1.5C'=4,'ZF2100'=2,'ZF2050'=21)) +
  plot_theme_white +
  theme(legend.position='none')
plot(g_fig_S1f)


# fig. S1 -----------------------------------------------------------------

g_fig_S1 <-ggdraw() +
  draw_plot(g_fig_S1c,x=.65,y=.475,width=.35,height=.525) +
  draw_plot(g_fig_S1b,x=.3125,y=.475,width=.35,height=.525) +
  draw_plot(g_fig_S1a,x=-.025,y=.475,width=.35,height=.525) +
  draw_plot(g_fig_S1d,x=.0,y=0,width=.175,height=.45) +
  draw_plot(g_fig_S1e,x=.2,y=0,width=.175,height=.45) +
  draw_plot(g_fig_S1f,x=.4,y=0,width=.175,height=.45) +
  draw_plot(l_fig_1abc_1,x=.7,y=0.025,width=.075,height=.45) +
  draw_plot(l_fig_1de,x=.79,y=0.14,width=.075,height=.275) +
  draw_plot(l_fig_1abc_2,x=.76,y=0.08,width=.2,height=.075) +
  draw_text('Primary energy mix',x=0,y=.94,size=7,hjust=-0.1) +
  draw_text('Power generation mix',x=.315,y=.94,size=7,hjust=-0.1) +
  draw_text('Final energy mix',x=.675,y=.94,size=7,hjust=-0.1) +
  draw_plot_label(
    label=c('a','b','c','d','e','f'),
    x=c(0,.315,.675,0,.2,0.4),
    y=c(1,1,1,.475,.475,.475),
    size=8
  )
plot(g_fig_S1)

ggsave('output/1st_submission/fig_S1.png',g_fig_S1,width=180,height=105,units='mm',dpi=500)


# fig. S2a ----------------------------------------------------------------

plt <- set_plot(var_fin_agg)

g_fig_S2a <-df_fig_2a %>%
  mutate(Variable=recode(Variable,
                         'Final Energy|Liquids|Hydrogen synfuel'='Final Energy|Hydrogen synfuel',
                         'Final Energy|Gases|Hydrogen synfuel'='Final Energy|Hydrogen synfuel'),
         Variable=factor(Variable,levels=rev(var_fin_agg$Variable)),
         Scenario=factor(Scenario,levels=c(
           'Opt1.5C','ZF2100','ZF2090','ZF2080','ZF2070','ZF2060','ZF2050'
         ))) %>%
  group_by(Model,Scenario,Region,Variable,Unit,Year) %>% 
  reframe(value=sum(value)) %>% 
  ggplot() +
  geom_area(aes(x=Year,y=value,fill=Variable)) +
  scale_fill_manual(values=plt$Color,labels=plt$Legend) +
  guides(fill=guide_legend(ncol=1)) +
  labs(x='',y=expression(paste('Final energy (EJ ',{yr^{-1}},')')),tag='a') +
  facet_grid(rows=vars(Model),cols=vars(Scenario),
             axes='all',axis.labels='margins') +
  plot_theme_white
plot(g_fig_S2a)


# fig. S2b ----------------------------------------------------------------

df_fig_S2b <- df_load %>% 
  filter(Variable%in%c('Final Energy|Industry|Solids|Coal',
                       'Final Energy|Industry|Liquids|Fossil',
                       'Final Energy|Industry|Gases|Fossil',
                       'Final Energy|Industry|Solids|Biomass',
                       'Final Energy|Industry|Liquids|Biomass',
                       'Final Energy|Industry|Gases|Biomass',
                       'Final Energy|Industry|Solar',
                       'Final Energy|Industry|Electricity',
                       'Final Energy|Industry|Heat',
                       'Final Energy|Industry|Hydrogen',
                       'Final Energy|Industry|Gases|Hydrogen synfuel',
                       'Final Energy|Industry|Liquids|Hydrogen synfuel')) %>% 
  mutate(Variable=case_when(Variable%in%c('Final Energy|Industry|Solids|Coal')~'Final Energy|Solids|Coal',
                            Variable%in%c('Final Energy|Industry|Liquids|Fossil')~'Final Energy|Liquids|Fossil',
                            Variable%in%c('Final Energy|Industry|Gases|Fossil')~'Final Energy|Gases|Fossil',
                            Variable%in%c('Final Energy|Industry|Solids|Biomass',
                                          'Final Energy|Industry|Liquids|Biomass',
                                          'Final Energy|Industry|Gases|Biomass')~'Final Energy|Biomass',
                            Variable=='Final Energy|Industry|Solar'~'Final Energy|Solar',
                            Variable=='Final Energy|Industry|Electricity'~'Final Energy|Electricity',
                            Variable=='Final Energy|Industry|Heat'~'Final Energy|Heat',
                            Variable=='Final Energy|Industry|Hydrogen'~'Final Energy|Hydrogen',
                            Variable%in%c('Final Energy|Industry|Gases|Hydrogen synfuel',
                                          'Final Energy|Industry|Liquids|Hydrogen synfuel')~'Final Energy|Hydrogen synfuel'))

g_fig_S2b <-df_fig_S2b %>%
  filter(Scenario%in%c('Opt1.5C','ZF2100','ZF2050')) %>% 
  mutate(Variable=factor(Variable,levels=rev(var_fin_agg$Variable)),
         Scenario=factor(Scenario,levels=c(
           'Opt1.5C','ZF2100','ZF2090','ZF2080','ZF2070','ZF2060','ZF2050'
         ))) %>%
  group_by(Model,Scenario,Region,Variable,Unit,Year) %>% 
  reframe(value=sum(value)) %>% 
  ggplot() +
  geom_area(aes(x=Year,y=value,fill=Variable)) +
  scale_fill_manual(values=plt$Color,labels=plt$Legend) +
  guides(fill=guide_legend(ncol=1)) +
  labs(x='',y=expression(paste('Final energy (EJ ',{yr^{-1}},')'))) +
  facet_grid(rows=vars(Model),cols=vars(Scenario),
             axes='all',axis.labels='margins') +
  plot_theme_white
plot(g_fig_S2b)


# fig. S2c ----------------------------------------------------------------

df_fig_S2c <- df_load %>% 
  filter(Variable%in%c('Final Energy|Residential and Commercial|Solids|Coal',
                       'Final Energy|Residential and Commercial|Liquids|Fossil',
                       'Final Energy|Residential and Commercial|Gases|Fossil',
                       'Final Energy|Residential and Commercial|Solids|Biomass',
                       'Final Energy|Residential and Commercial|Liquids|Biomass',
                       'Final Energy|Residential and Commercial|Gases|Biomass',
                       'Final Energy|Residential and Commercial|Solar',
                       'Final Energy|Residential and Commercial|Electricity',
                       'Final Energy|Residential and Commercial|Heat',
                       'Final Energy|Residential and Commercial|Hydrogen',
                       'Final Energy|Residential and Commercial|Gases|Hydrogen synfuel',
                       'Final Energy|Residential and Commercial|Liquids|Hydrogen synfuel')) %>% 
  mutate(Variable=case_when(Variable%in%c('Final Energy|Residential and Commercial|Solids|Coal')~'Final Energy|Solids|Coal',
                            Variable%in%c('Final Energy|Residential and Commercial|Liquids|Fossil')~'Final Energy|Liquids|Fossil',
                            Variable%in%c('Final Energy|Residential and Commercial|Gases|Fossil')~'Final Energy|Gases|Fossil',
                            Variable%in%c('Final Energy|Residential and Commercial|Solids|Biomass',
                                          'Final Energy|Residential and Commercial|Liquids|Biomass',
                                          'Final Energy|Residential and Commercial|Gases|Biomass')~'Final Energy|Biomass',
                            Variable=='Final Energy|Residential and Commercial|Solar'~'Final Energy|Solar',
                            Variable=='Final Energy|Residential and Commercial|Electricity'~'Final Energy|Electricity',
                            Variable=='Final Energy|Residential and Commercial|Heat'~'Final Energy|Heat',
                            Variable=='Final Energy|Residential and Commercial|Hydrogen'~'Final Energy|Hydrogen',
                            Variable%in%c('Final Energy|Residential and Commercial|Gases|Hydrogen synfuel',
                                          'Final Energy|Residential and Commercial|Liquids|Hydrogen synfuel')~'Final Energy|Hydrogen synfuel'))

g_fig_S2c <-df_fig_S2c %>%
  filter(Scenario%in%c('Opt1.5C','ZF2100','ZF2050')) %>% 
  mutate(Variable=factor(Variable,levels=rev(var_fin_agg$Variable)),
         Scenario=factor(Scenario,levels=c(
           'Opt1.5C','ZF2100','ZF2090','ZF2080','ZF2070','ZF2060','ZF2050'
         ))) %>%
  group_by(Model,Scenario,Region,Variable,Unit,Year) %>% 
  reframe(value=sum(value)) %>% 
  ggplot() +
  geom_area(aes(x=Year,y=value,fill=Variable)) +
  scale_fill_manual(values=plt$Color,labels=plt$Legend) +
  guides(fill=guide_legend(ncol=1)) +
  labs(x='',y=expression(paste('Final energy (EJ ',{yr^{-1}},')'))) +
  facet_grid(rows=vars(Model),cols=vars(Scenario),
             axes='all',axis.labels='margins') +
  plot_theme_white
plot(g_fig_S2c)


# fig. S2d ----------------------------------------------------------------

df_fig_S2d <- df_load %>% 
  filter(Variable%in%c('Final Energy|Transportation|Solids|Coal',
                       'Final Energy|Transportation|Liquids|Fossil',
                       'Final Energy|Transportation|Gases|Fossil',
                       'Final Energy|Transportation|Solids|Biomass',
                       'Final Energy|Transportation|Liquids|Biomass',
                       'Final Energy|Transportation|Gases|Biomass',
                       'Final Energy|Transportation|Solar',
                       'Final Energy|Transportation|Electricity',
                       'Final Energy|Transportation|Heat',
                       'Final Energy|Transportation|Hydrogen',
                       'Final Energy|Transportation|Gases|Hydrogen synfuel',
                       'Final Energy|Transportation|Liquids|Hydrogen synfuel')) %>% 
  mutate(Variable=case_when(Variable%in%c('Final Energy|Transportation|Solids|Coal')~'Final Energy|Solids|Coal',
                            Variable%in%c('Final Energy|Transportation|Liquids|Fossil')~'Final Energy|Liquids|Fossil',
                            Variable%in%c('Final Energy|Transportation|Gases|Fossil')~'Final Energy|Gases|Fossil',
                            Variable%in%c('Final Energy|Transportation|Solids|Biomass',
                                          'Final Energy|Transportation|Liquids|Biomass',
                                          'Final Energy|Transportation|Gases|Biomass')~'Final Energy|Biomass',
                            Variable=='Final Energy|Transportation|Solar'~'Final Energy|Solar',
                            Variable=='Final Energy|Transportation|Electricity'~'Final Energy|Electricity',
                            Variable=='Final Energy|Transportation|Heat'~'Final Energy|Heat',
                            Variable=='Final Energy|Transportation|Hydrogen'~'Final Energy|Hydrogen',
                            Variable%in%c('Final Energy|Transportation|Gases|Hydrogen synfuel',
                                          'Final Energy|Transportation|Liquids|Hydrogen synfuel')~'Final Energy|Hydrogen synfuel'))

g_fig_S2d <-df_fig_S2d %>%
  filter(Scenario%in%c('Opt1.5C','ZF2100','ZF2050')) %>% 
  mutate(Variable=factor(Variable,levels=rev(var_fin_agg$Variable)),
         Scenario=factor(Scenario,levels=c(
           'Opt1.5C','ZF2100','ZF2090','ZF2080','ZF2070','ZF2060','ZF2050'
         ))) %>%
  group_by(Model,Scenario,Region,Variable,Unit,Year) %>% 
  reframe(value=sum(value)) %>% 
  ggplot() +
  geom_area(aes(x=Year,y=value,fill=Variable)) +
  scale_fill_manual(values=plt$Color,labels=plt$Legend) +
  guides(fill=guide_legend(ncol=1)) +
  labs(x='',y=expression(paste('Final energy (EJ ',{yr^{-1}},')'))) +
  facet_grid(rows=vars(Model),cols=vars(Scenario),
             axes='all',axis.labels='margins') +
  plot_theme_white
plot(g_fig_S2d)


# fig. S2 -----------------------------------------------------------------

l_fig_S2 <- g_legend(g_fig_S2a)

g_fig_S2<- (((g_fig_S2a + labs(title='Total',tag='a')+theme(legend.position='none')+guides(fill=guide_legend(nrow=1)))+
               plot_spacer() + l_fig_S2 + plot_layout(widths=c(9,0,1))) /
               ((g_fig_S2b + labs(title='Industry',tag='b')+theme(legend.position='none')+guides(fill=guide_legend(nrow=1))) +
                (g_fig_S2c + labs(title='Buidings',tag='c')+theme(legend.position='none')+guides(fill=guide_legend(nrow=1))) +
                (g_fig_S2d + labs(title='Transport',tag='d')+theme(legend.position='none')+guides(fill=guide_legend(nrow=1))))) +
  plot_layout(heights=c(1,1))&theme(plot.margin=margin(0,5,0,0))

ggsave('output/1st_submission/fig_S2.png',g_fig_S2,width=180,height=110,units='mm',dpi=500)


# fig. S3 -----------------------------------------------------------------

g_fig_S3 <-ggdraw() +
  draw_plot(`g_ele_snapshot_2100_AIM`+theme(legend.position='none')+`g_sec_flow_2100_AIM`+plot_layout(widths=c(1,8)),
            x=0,y=0,width=.45,height=.95) +
  draw_plot(`g_ele_snapshot_2100_MESSAGEix`+theme(legend.position='none')+`g_sec_flow_2100_MESSAGEix`+plot_layout(widths=c(1,8)),,
            x=.45,y=0,width=.45,height=.95) +
  draw_plot(l_fig_3ab,x=.9,y=0,width=.1,height=.95) +
  draw_plot_label(
    label=c('a','b'),
    x=c(0,.45),
    y=c(1,1),
    size=8
  )
plot(g_fig_S3)
ggsave('output/1st_submission/fig_S3.png',g_fig_S3,width=180,height=60,units='mm',dpi=500)


# fig. S4a ----------------------------------------------------------------

plt <- set_plot(var_ele2)

df_fig_S4a <- df_load %>% 
  filter(Variable%in%c('Secondary Energy|Electricity|Coal|w/o CCS',
                       'Secondary Energy|Electricity|Coal|w/ CCS',
                       'Secondary Energy|Electricity|Oil|w/o CCS',
                       'Secondary Energy|Electricity|Oil|w/ CCS',
                       'Secondary Energy|Electricity|Gas|w/o CCS',
                       'Secondary Energy|Electricity|Gas|w/ CCS',
                       'Secondary Energy|Electricity|Nuclear',
                       'Secondary Energy|Electricity|Biomass|w/o CCS',
                       'Secondary Energy|Electricity|Biomass|w/ CCS',
                       'Secondary Energy|Electricity|Geothermal',
                       'Secondary Energy|Electricity|Hydro',
                       'Secondary Energy|Electricity|Solar',
                       'Secondary Energy|Electricity|Wind'))

g_fig_S4a <-df_fig_S4a %>%
  mutate(Variable=factor(Variable,levels=rev(var_ele2$Variable)),
         Scenario=factor(Scenario,levels=c(
           'Opt1.5C','ZF2100','ZF2090','ZF2080','ZF2070','ZF2060','ZF2050'
         ))) %>%
  group_by(Model,Scenario,Region,Variable,Unit,Year) %>% 
  reframe(value=sum(value)) %>% 
  ggplot() +
  geom_area(aes(x=Year,y=value,fill=Variable)) +
  scale_fill_manual(values=plt$Color,labels=plt$Legend) +
  guides(fill=guide_legend(ncol=1)) +
  labs(x='',y=expression(paste('Secondary energy (EJ ',{yr^{-1}},')')),tag='a') +
  facet_grid(rows=vars(Model),cols=vars(Scenario),
             axes='all',axis.labels='margins') +
  plot_theme_white
plot(g_fig_S4a)


# fig. S4b ----------------------------------------------------------------

plt <- set_plot(var_hyd2)

df_fig_S4b <- df_load %>% 
  filter(Variable%in%c('Secondary Energy|Hydrogen|Coal|w/o CCS',
                       'Secondary Energy|Hydrogen|Coal|w/ CCS',
                       'Secondary Energy|Hydrogen|Gas|w/o CCS',
                       'Secondary Energy|Hydrogen|Gas|w/ CCS',
                       'Secondary Energy|Hydrogen|Biomass|w/o CCS',
                       'Secondary Energy|Hydrogen|Biomass|w/ CCS',
                       'Secondary Energy|Hydrogen|Electricity'))

g_fig_S4b <-df_fig_S4b %>%
  mutate(Variable=factor(Variable,levels=rev(var_hyd2$Variable)),
         Scenario=factor(Scenario,levels=c(
           'Opt1.5C','ZF2100','ZF2090','ZF2080','ZF2070','ZF2060','ZF2050'
         ))) %>%
  group_by(Model,Scenario,Region,Variable,Unit,Year) %>% 
  reframe(value=sum(value)) %>% 
  ggplot() +
  geom_area(aes(x=Year,y=value,fill=Variable)) +
  scale_fill_manual(values=plt$Color,labels=plt$Legend) +
  guides(fill=guide_legend(ncol=1)) +
  labs(x='',y=expression(paste('Secondary energy (EJ ',{yr^{-1}},')')),tag='b') +
  facet_grid(rows=vars(Model),cols=vars(Scenario),
             axes='all',axis.labels='margins') +
  plot_theme_white
plot(g_fig_S4b)


# fig. S4 -----------------------------------------------------------------

g_fig_S4 <- g_fig_S4a + g_fig_S4b + plot_layout(ncol=1)

ggsave('output/1st_submission/fig_S4.png',g_fig_S4,width=180,height=120,units='mm',dpi=500)


# fig. S5a ----------------------------------------------------------------

plt <- set_plot(var_emi)

g_fig_S5a <- df_fig_4b %>% 
  filter(Model=='AIM'&Year%in%seq(2020,2100,5)|Model=='MESSAGEix'&Year%in%c(seq(2020,2055,5),seq(2060,2100,10))) %>% 
  mutate(Variable=factor(Variable,levels=rev(var_emi$Variable)),
         Scenario=factor(Scenario,levels=c('Opt1.5C','ZF2100','ZF2090','ZF2080','ZF2070','ZF2060','ZF2050'))) %>% 
  replace_na(list(value=0)) %>% 
  ggplot() +
  geom_hline(yintercept=0,linewidth=0.1,color='grey15',linetype='dashed') +
  geom_area(aes(x=Year,y=value/1000,fill=Variable)) +
  geom_point(data=. %>% group_by(Model,Scenario,Region,Unit,Year) %>% reframe(value=sum(value)),
             aes(x=Year,y=value/1000),size=0.5) +
  geom_line(data=. %>% group_by(Model,Scenario,Region,Unit,Year) %>% reframe(value=sum(value)),
            aes(x=Year,y=value/1000),linewidth=0.2) +
  scale_fill_manual(values=plt$Color,labels=plt$Legend) +
  labs(x='',y=expression(paste({CO[2]},' emission (Gt-',{CO[2]},' ',{yr^{-1}},')'))) +
  facet_grid(rows=vars(Model),cols=vars(Scenario),
             axes='all',axis.labels='margins') +
  plot_theme_white +
  theme(legend.position='none')
plot(g_fig_S5a)


# fig. S5b ----------------------------------------------------------------

df_fig_S5b_1 <- df_load %>%
  filter(Variable%in%c('CCUS|Geological Storage|Biomass',
                       'CCUS|Geological Storage|Fossil',
                       'CCUS|Geological Storage|Industrial Processes',
                       'CCUS|Geological Storage|Direct Air Capture')) %>%
  mutate(Variable=paste0(str_remove_all(Variable,'CCUS\\|Geological Storage\\|'),' (CCS)'))

df_fig_S5b_2 <- df_load %>%
  filter(Variable%in%c('CCUS|Utilization|Energy|Biomass',
                       'CCUS|Utilization|Energy|Fossil',
                       'CCUS|Utilization|Energy|Industrial Processes',
                       'CCUS|Utilization|Energy|Direct Air Capture')) %>%
  mutate(Variable=paste0(str_remove_all(Variable,'CCUS\\|Utilization\\|Energy\\|'),' (CCU)'))

df_fig_S5b <- bind_rows(df_fig_S5b_1,df_fig_S5b_2)

g_fig_S5b <- df_fig_S5b %>%
  filter(Model=='AIM'&Year%in%seq(2020,2100,5)|Model=='MESSAGEix'&Year%in%c(seq(2020,2055,5),seq(2060,2100,10))) %>% 
  pivot_wider(names_from=Variable,values_from=value,values_fill=0) %>% 
  pivot_longer(cols=-c(Model,Region,Scenario,Unit,Year),
               names_to='Variable',values_to='value') %>% 
  mutate(Scenario=factor(Scenario,levels=c('Opt1.5C','ZF2100','ZF2090','ZF2080','ZF2070','ZF2060','ZF2050')),
         Variable=factor(Variable,levels=c('Direct Air Capture (CCU)',
                                           'Direct Air Capture (CCS)',
                                           'Biomass (CCU)',
                                           'Biomass (CCS)',
                                           'Fossil (CCU)',
                                           'Fossil (CCS)',
                                           'Industrial Processes (CCU)',
                                           'Industrial Processes (CCS)'))) %>%
  ggplot() +
  geom_hline(yintercept=0,linewidth=0.1,color='grey15',linetype='dashed') +
  geom_area(aes(x=Year,y=value/1000,fill=Variable)) +
  scale_fill_manual(values=c('orchid',
                             'thistle2',
                             'darkolivegreen3',
                             '#e4ed68',
                             'tan3',
                             'sandybrown',
                             'grey50',
                             'grey70')) +
  facet_grid(rows=vars(Model),cols=vars(Scenario),
             axes='all',axis.labels='margins') +
  labs(x='',y=expression(paste('Carbon Capture (Gt-',{CO[2]},' ',{yr^{-1}},')'))) +
  plot_theme_white
plot(g_fig_S5b)

l_fig_S5a <- g_legend(g_fig_S5a+theme(legend.position='bottom')+guides(fill=guide_legend(nrow=1)))
l_fig_S5b <- g_legend(g_fig_S5b+theme(legend.position='bottom')+guides(fill=guide_legend(nrow=2)))

g_figS5 <- g_fig_S5a + theme(legend.position='none') + labs(tag='a') + 
  l_fig_S5a + 
  plot_spacer() +
  g_fig_S5b + theme(legend.position='none') + labs(tag='b') + 
  l_fig_S5b +
  plot_layout(ncol=1,guides='collect',heights=c(5,.2,0,3,.4))&theme(legend.position='none')
ggsave('output/1st_submission/fig_S5.png',g_figS5,width=180,height=150,units='mm',dpi=500)


# fig. S6 -----------------------------------------------------------------

df_fig_S6 <- df_load %>% 
  filter(Variable%in%c('Trade|Primary Energy|Biomass|Volume',
                       'Trade|Primary Energy|Coal|Volume',
                       'Trade|Primary Energy|Gas|Volume',
                       'Trade|Primary Energy|Oil|Volume',
                       'Trade|Secondary Energy|Electricity|Volume',
                       'Trade|Secondary Energy|Hydrogen|Volume',
                       'Trade|Secondary Energy|Ammonia|Volume',
                       'Trade|Secondary Energy|Synfuel|Volume',
                       'Trade|Secondary Energy|Liquids|Biomass|Volume',
                       'Trade|Secondary Energy|Liquids|Oil|Volume',
                       'Trade|Secondary Energy|Liquids|Coal|Volume')) %>% 
  mutate(Variable=str_remove_all(Variable,'Trade\\||\\|Volume'),
         Variable=str_remove_all(Variable,'.*\\|'),
         Variable=case_when(Variable%in%c('Hydrogen','Ammonia')~'Hydrogen',TRUE~Variable)) %>% 
  group_by(Model,Scenario,Variable,Unit,Year) %>% 
  reframe(value=sum(value))

g_fig_S6 <- df_fig_S6 %>% 
  mutate(Variable=factor(Variable,levels=rev(c('Coal','Oil','Gas','Biomass','Electricity','Hydrogen','Synfuel'))),
  Scenario=factor(Scenario,levels=c('Opt1.5C','ZF2100','ZF2090','ZF2080','ZF2070','ZF2060','ZF2050'))) %>%
  group_by(Model,Scenario,Unit,Year) %>% 
  complete(Variable,fill=list(value=0)) %>% 
  group_by(Model,Scenario,Variable,Unit,Year) %>% 
  reframe(value=sum(value)) %>% 
  ggplot() +
  geom_area(aes(x=Year,y=value,fill=Variable)) +
  scale_fill_manual(values=c(
    'Coal'='grey60',
    'Oil'='sandybrown',
    'Gas'='lightgoldenrod2',
    'Biomass'='darkolivegreen2',
    'Electricity'='lightsteelblue',
    'Hydrogen'='thistle2',
    'Synfuel'='orchid'
  )) +
  guides(fill=guide_legend(ncol=1)) +
  labs(x='',y=expression(paste('Energy trade (EJ ',{yr^{-1}},')'))) +
  facet_grid(rows=vars(Model),cols=vars(Scenario),
             axes='all',axis.labels='margins') +
  plot_theme_white +
  theme(legend.position='right',
        legend.margin=margin(0,0,0,0))
plot(g_fig_S6)

ggsave('output/1st_submission/fig_S6.png',g_fig_S6,width=180,height=75,units='mm',dpi=500)
