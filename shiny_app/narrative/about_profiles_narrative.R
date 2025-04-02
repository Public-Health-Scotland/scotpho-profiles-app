###########################################.
# Narrative text for 'about profiles' tab 
###########################################.

###########################################.
### Care and Wellbeing ----

about_cwb_text <- navset_pill_list(
  widths = c(3, 9),
  nav_panel("Overview",
  h3("Overview"),
  p("The Care & Wellbeing Profile is one source of data and intelligence to support the ambitions of the ",
    tags$a("Care & Wellbeing Portfolio (CWP)", href = "https://www.gov.scot/groups/care-and-wellbeing-portfolio-board/", target = "_blank"),
    "to improve population health, address health inequalities and improve
     the health and care system."),
  p("Many of the influences on health outcomes lie outwith health and social care.
     Collective action across government, with health boards, local government
     partners and the wider public sector, is fundamental to improving population
     health and reducing health inequalities."),
  p("A range of indicators are included in this Profile structured around the
     evidence-based Marmot framework which looks at the social determinants of
     health, the conditions in which people are born, grow, live, work and age
     which can lead to health inequalities."),
  br()
  ),
  nav_panel("Care & Wellbeing Portfolio (CWP)",
  h3("Care & Wellbeing Portfolio (CWP)"),
  p("The CWP is the principal strategic reform vehicle in the Scottish Government.
     It brings oversight and coherence to the major health and care reform programmes designed to improve
     population health, address health inequalities and improve health and care system sustainability."),
  p("The underpinning framework of the Portfolio is the evidence-based Marmot
     domains – \"policy objectives\" that create health and reduce inequalities.
     This globally recognised framework was first set out in",
    tags$a("Fair Society, Healthy Lives", href = "https://www.instituteofhealthequity.org/resources-reports/fair-society-healthy-lives-the-marmot-review", target = "_blank"),
    "in 2010, updated in a",
    tags$a("10 year on report.", href = "https://www.health.org.uk/publications/reports/the-marmot-review-10-years-on", target = "_blank"),
    "The domains cover early years, education, work, living standards, healthy places, ill health prevention,
     discrimination and racism, and environmental sustainability and equity."),
  br()
  ),
  nav_panel("Other resources",
  h3("Other resources"),
  p("The Care & Wellbeing Profile is one source of data and intelligence to support
     the ambitions of CWP. A number of other sources of helpful information is also available:"),
  tags$li("The ", tags$b("Improvement Service (IS)"), "is the 'go-to' organisation for local government improvement
     in Scotland. It provides performance management and benchmarking products and services
     to help councils assess and improve their own performance, and support decision-making
     in councils and community planning partnerships. More information can be found at:",
     tags$a("Improvement Service.", href = "https://www.improvementservice.org.uk/", target = "_blank")),
  tags$li("The ", tags$b("National Performance Framework (NPF)"), "provides a measure of national
     wellbeing and keeps track of how Scotland is performing. It aims to reduce
     inequalities and gives equal importance to economic, environmental and social
     progress. More information can be found at:",
     tags$a("National Outcomes | National Performance Framework", href = "https://nationalperformance.gov.scot/national-outcomes", target = "_blank")),
  br()
  )
)
  
###########################################.
### Health and Wellbeing ----

about_hwb_text <- navset_pill_list(
  widths = c(3, 9),
nav_panel("Overview",
h3("Overview"),
p("The ScotPHO Health and Wellbeing profiles are designed to provide a broad picture of health in Scotland, highlight health and social inequalities and aim
to enable resources to be appropriately targeted to reduce inequalities."),
br()
),
nav_panel("Target audience",
h3("Target audience"),
p("We expect that the following professional groups will find the information contained here of particular
interest: public health and health improvement staff; health promotion officers; public health practitioners;
planners and other health professionals within NHS Boards. It will also be of interest to those in government,
local authorities, third sector and academia including planners and policy makers; community planning;
improvement services; researchers and voluntary sector organisations.
Additionally, we hope that a range of national and local organisations will find the profiles useful to
understand local health variations and identify areas in which to target health improvement efforts."),
br()
)
)


###########################################.
### Children and young people ----

about_cyp_text <- navset_pill_list(
  widths = c(3, 9),
  nav_panel("Overview",
  h5("Overview"),
  p("The Scottish Public Health Observatory (ScotPHO) Children and Young People’s Profile presents a wide range of data at national, local authority, NHS board, health & social care partnership and, where available, intermediate zone level.
    These data can be used by a variety of partners across the public and third sectors to highlight inequalities, plan services, target resources effectively and monitor outcomes for children and young
    people at a population level."),
  p("The Children and Young People’s Profile comprises 52 indicators across a wide range of health outcomes and social determinants.
    The indicators within the profile are organised by the eight wellbeing domains, as defined in",
    tags$a("Getting It Right For Every Child (GIRFEC).", href = "https://www.gov.scot/policies/girfec/wellbeing-indicators-shanarri/", target = "_blank"),
    "which is the national approach to improving outcomes for all children and young people.The rights of children and their wellbeing sit at the
    heart of GIRFEC. In recognition that the wellbeing of children and young people is influenced by all of their experiences and everything
    around them, a broad definition of wellbeing has been developed."),
  p("Organisation of the indicators by the SHANARRI domains received overwhelming support during a stakeholder consultation in the
  summer of 2016. In response to this, the SHANARRI approach has been adopted in the profile. In developing the profile it was
  recognised that one indicator may fit into more than one domain.Equally, for some domains, it was difficult to find suitable indicators.
  For presentation within the profile each indicator has been placed into a single wellbeing domain. Users of the profiles may choose
  to use the indicators differently to how they have been presented.Unfortunately, none of the indicators identified for inclusion in the
  profile was a good fit for the ‘respected’ domain."),
  br()
  ),
  nav_panel("Further information",
  h3("Further information"),
  p("For more information on GIRFEC and the wellbeing domains please see: ",
  tags$a("https:://www.gov.scot/Topics/People/Young-People/gettingitright/wellbeing", href = "https:://www.gov.scot/Topics/People/Young-People/gettingitright/wellbeing", target = "_blank")),
  br()
  )
)



###########################################.
### alcohol ----

about_alc_text <- navset_pill_list(
  widths = c(3, 9),
  nav_panel("Overview",
  h3("Overview"),
  p("The Scottish Public Health Observatory (ScotPHO) Alcohol Profile presents a range of data at national, local authority, NHS board, health & social care partnership and, where available, intermediate zone level.
    These data can be used by a variety of partners across the public and third sectors to highlight inequalities, plan services, target resources effectively and monitor outcomes at a population level."),
  br()
  ),
  nav_panel("Further information",
  h3("Futher information"),
  p("Public Health Scotland (PHS) produce an ",
  tags$a("Alcohol consumption and harms dashabord ",href="https://scotland.shinyapps.io/phs-health-achd/", target = "_blank"),
  "which provides further information relating to alcohol."),
  br()
  )
  )


###########################################.
### Adult Mental Health ----

about_men_text <-  navset_pill_list(
  widths = c(3, 9),
  nav_panel("Overview",
  h3("Overview"),
  p("The Adult Mental Health Profile is the result of Public Health Scotland's ",
    tags$a("Mental Health Indicators", href = "https://publichealthscotland.scot/our-areas-of-work/health-and-wellbeing/prevention-of-mental-ill-health-and-improved-wellbeing/mental-health-indicators/overview/", target="_blank"),
    "project. "),
  p("The aim of the project is to make relevant data more accessible to local and national users. 
    Better access to timely data will support efforts to improve population mental health, 
    through more effective planning and policies, and more efficient resource allocation. 
    Further information on development of the mental health profile can be found at the link above."),
  p("The mental health indicators include measures of mental health outcomes, as well as of 
    a wide range of interconnected determinants (risk factors and protective factors) of these outcomes. 
    A separate mental health profile for children and young people is available elsewhere on this site. "),
  br()
  ),
  nav_panel("Mental Health Profile for Adults",
  h3("Mental Health Profile for Adults"),
  p("The ",
    tags$a("adult mental health indicator set", href = "https://publichealthscotland.scot/our-areas-of-work/health-and-wellbeing/prevention-of-mental-ill-health-and-improved-wellbeing/mental-health-indicators/adult-mental-health-indicators/", target="_blank"),
    "includes 55 indicators grouped into four high-level \"domains\":"),
    tags$li("10 ", tags$b("mental health outcomes")), 
    tags$li("9 ", tags$b("individual-level determinants")), 
    tags$li("11 ", tags$b("community-level determinants")), 
    tags$li("25 ", tags$b("structural-level determinants")), 
  br(),
  p("Some indicators identified as being important currently have no data, either because a suitable source has not been identified, or a suitable definition has not been developed. These are:"),
  tags$li(tags$b("Mental health outcomes:"), " Adult drug use disorders."), 
  tags$li(tags$b("Individual-level determinants:"), " Sleep behaviour, Supportive family unit/relationships, Social media, Spirituality."), 
  tags$li(tags$b("Community-level determinants:"), " Institutional trust."), 
  tags$li(tags$b("Structural-level determinants:"), " Racism, Stigma around mental health, Climate change."),
  br(),
  p("Adults are defined as individuals aged 16 years and over, unless otherwise stated."),
  br()
  )
  )

###########################################.
### CYP Mental Health ----

about_cmh_text <-  navset_pill_list(
  widths = c(3, 9),
  nav_panel("Overview",
            h3("Overview"),
            p("The Mental Health Profile is the result of Public Health Scotland's ",
              tags$a("Mental Health Indicators", href = "https://publichealthscotland.scot/our-areas-of-work/health-and-wellbeing/prevention-of-mental-ill-health-and-improved-wellbeing/mental-health-indicators/overview/", target="_blank"),
              "project. "),
            p("The aim of the project is to make relevant data more accessible to local and national users. 
    Better access to timely data will support efforts to improve population mental health, 
    through more effective planning and policies, and more efficient resource allocation. 
    Further information on development of the mental health profile can be found at the link above."),
            p("The mental health indicators include measures of mental health outcomes, as well as of 
    a wide range of interconnected determinants (risk factors and protective factors) of these outcomes. 
    A separate mental health profile for adults is available elsewhere on this site. "),
            br()
  ),
  nav_panel("Mental Health Profile for Children and Young People",
            h3("Mental Health Profile for Children and Young People"),
            p("The ",
              tags$a("children and young people's mental health indicator set", href = "https://publichealthscotland.scot/our-areas-of-work/health-and-wellbeing/prevention-of-mental-ill-health-and-improved-wellbeing/mental-health-indicators/children-and-young-people-mental-health-indicators/", target="_blank"),
              "includes 70 indicators grouped into six high-level \"domains\":"),
            tags$li("11 ", tags$b("mental health outcomes")), 
            tags$li("12 ", tags$b("individual-level determinants")), 
            tags$li("17 ", tags$b("determinants related to family and friends")), 
            tags$li("7 ", tags$b("determinants related to the learning environment")), 
            tags$li("5 ", tags$b("community-level determinants")), 
            tags$li("18 ", tags$b("structural-level determinants")), 
            p("Some indicators identified as being important currently have no data, either because a suitable source has not been identified, or a suitable definition has not been developed. "),
            p("The indicators cover different periods of childhood, from pre-birth to early adulthood."),
            br()
  )
) 
  