###########################################.
# Narrative text for 'about profiles' tab 
###########################################.

###########################################.
### Population Health (formerly Care and Wellbeing) ----

about_cwb_text <- navset_pill_list(
  widths = c(3, 9),
  nav_panel("Population Health dashboard",
  h3("Population Health dashboard"),
  p("The Population Health dashboard is a source of data and intelligence that supports the vision of Scotland’s Population Health Framework: to improve population health 
    and address longstanding health inequalities. The dashboard includes data on both life expectancy and healthy life expectancy, as well as a range of other indicators to 
    monitor population health outcomes."),
  p("The dashboard is also designed to support the Collaboration for Health Equity in Scotland (CHES). Public Health Scotland (PHS) have joined with the Institute for Health
    Equity (IHE) to strengthen and accelerate the action underway to improve Scotland’s health, increase wellbeing, and reduce health inequalities. IHE highlight that reducing 
    health inequity requires action on the “Marmot Eight” principles as laid out in their work on ", 
    tags$a("Marmot Places.", href = "https://www.instituteofhealthequity.org/taking-action/marmot-places", target = "_blank")),
  p("The indicators on the dashboard have been developed through consultation with stakeholders to provide a strategic focus on a shared set of outcomes. The indicator set remains
    under development, and changes will be made in future as new data becomes available, and in response to feedback from users."),
  br()
  ),
  
  nav_panel("Population Health Framework",
            h3("Population Health Framework"),
            p("The Population Health dashboard is one source of data designed to monitor progress against the aims of the Scottish Government’s ten-year Population Health Framework (PHF)."),
            p("As part of the PHF, a high level aim has been set:"),
            p("By 2035 we will improve Scottish life expectancy whilst reducing the life expectancy gap between the most deprived 20% of local areas and the national average."),
            p("The Scottish Government will provide updates on a regular basis offering an assessment of progress towards meeting this aim. This will include discussion of the 
              headline life expectancy measure, along with other measures such as healthy life expectancy."),
            p("The PHF sets out the Scottish Government’s commitment to build a clear and collaborative approach to improving the nation’s health, underpinned by evidence. It 
              is deliberately long-term and focused on prevention, equity, and sustainability by addressing the root causes of poor health."),
            p("The focus of the Framework is prevention. In health, prevention is about keeping people healthy and reducing the risk of poor health, illness, injury, 
            and early death. A key lesson from the pandemic is that our resilience to future health threats is strengthened by creating and sustaining health and preventing 
            avoidable poor health. This can be achieved by strengthening the building blocks referenced above, and creating environments that make it easier to live healthy lives."),
            p("In particular, the Framework focuses on ‘primary prevention’ – action designed to stop problems from emerging in the first instance. This has been a priority in Scotland
            since the Christie Commission on Public Services in 2011. The evidence on forecasted burden of disease, rising service demand, and financial sustainability is clear that 
            prevention of poor health is required now more than ever."),
            p("The Framework is structured around four key ‘primary prevention drivers’ of health and wellbeing which align with the ",
              tags$a("King’s Fund population health pillars.", href = "https://www.kingsfund.org.uk/insight-and-analysis/reports/vision-population-health", target = "_blank"),
              "These population health pillars link to Sir Michael Marmot’s eight principles, which are the “building blocks” that form the structure of the dashboard. The indicators
              included on the dashboard are designed to reflect progress against the aims and priorities of the PHF."),
              br()
  ),
  nav_panel("The Marmot Eight principles",
            h3("The Marmot Eight principles"),
            p("The indicators in the Population Health dashboard are structured around Sir Michael Marmot’s eight principles for health equity – these are policy objectives that create 
              health and reduce inequalities. This globally recognised “Marmot Eight” evidence-based framework was first set out by the Institute of Health Equity in ",
              tags$a("Fair Society, Healthy Lives", href = "https://www.instituteofhealthequity.org/resources-reports/fair-society-healthy-lives-the-marmot-review", target = "_blank"),
              "in 2010, and updated in 2020 as part of the ",
              tags$a("10 years on review report", href = "https://www.health.org.uk/publications/reports/the-marmot-review-10-years-on", target = "_blank"),  
              ". The eight Marmot principles are:",
              tags$ol(
                tags$li(tags$b("Early years")," – Give every child the best start in life."), 
                tags$li(tags$b("Education")," – Enable all children, young people and adults to maximise their capabilities and have control over their lives."), 
                tags$li(tags$b("Work")," – Create fair employment and good work for all."),
                tags$li(tags$b("Living standards")," – Ensure a healthy standard of living for all."),
                tags$li(tags$b("Healthy places")," – Create and develop healthy and sustainable places and communities."),
                tags$li(tags$b("Impact of ill-health prevention")," – Strengthen the role and impact of ill health prevention."),
                tags$li(tags$b("Discrimination and racism")," – Tackle racism, discrimination and their outcomes."),
                tags$li(tags$b("Environmental sustainability and health equity")," – Pursue environmental sustainability and health equity together.")
                )),
              br()
            ),
  nav_panel("Collaboration for Health Equity in Scotland",
            h3("Collaboration for Health Equity in Scotland"),
            p("Public Health Scotland (PHS) has joined with the University College London (UCL) Institute of Health Equity (IHE) for a two-year ",
              tags$a("Collaboration for Health Equity in Scotland (CHES)", href = "https://publichealthscotland.scot/population-health/environmental-health-impacts/collaboration-for-health-equity-in-scotland/", target = "_blank"),
              ". Working with Professor Sir Michael Marmot, the director of the Institute, this collaboration will strengthen and accelerate the action already underway to improve Scotland’s health, promote wellbeing, and address 
              health inequalities. The partnership between PHS and IHE is to support public service reform and will cover two key areas: ",
              tags$li("Work at a national level to provide new insights into the most effective ways to progress health equity in Scotland through Marmot’s eight principles."), 
              tags$li("Work in partnership with local authorities and NHS boards across Aberdeen City, North Ayrshire, and South Lanarkshire to develop and implement strategies to enhance health equity.")), 
              p("The ultimate goal is to enable people to live longer, healthier lives by addressing the root causes of health inequalities and preventing illness before it starts. By creating conditions in which communities 
                can thrive, the initiative seeks to drive lasting, positive change."),
              p("The Population Health dashboard provides a set of indicators structured around Marmot’s eight principles to inform progress towards this goal."),
            br()),

  nav_panel("Other resources",
  h3("Other resources"),
  p("The Population Health dashboard draws from existing sources of Population Health information to support policy-making at a national
    and local level. The indicators aim to provide a strategic focus around policy priorities as set out in the Population Health Framework.
    A wider range of indicators can also be accessed through the ScotPHO profiles tool and other existing sources of data. Other sources of data and evidence include:"),

  tags$li(tags$a("Public Health Scotland (PHS).", href = "https://www.publichealthscotland.scot/", target = "_blank"),
          "publishes a wide range of statistics related to public health outcomes, as well as the health and social care system."),
  br(),
  tags$li(tags$a("ScotPHO", href = "https://www.scotpho.org.uk/", target = "_blank"),
          ", as well as the Online Profiles Tool, publish a wide range of reports and analysis on their website. These cover
          a broad range of public health themes, including mental health, substance use, cardiovascular disease, children and young people’s health,
          environmental health, and health inequalities, among others. Together, these resources support evidence-informed decision-making and help
          monitor progress toward improving population health across Scotland."),
  br(),
  tags$li("The ",tags$a("PHS Health & wellbeing Metadata Catalogue ", href = "https://scotland.shinyapps.io/phs-metadata-catalogue/", target = "_blank"),
          "tool provides metadata on easily accessible, publicly available health and wellbeing indicators for Scotland."),
  br(),
  tags$li("The ", tags$a("Improvement Service (IS)",href = "https://www.improvementservice.org.uk/", target = "_blank"),
          " support local government improvement in Scotland providing performance management and benchmarking products and
          services to help councils assess and improve their own performance, and support decision-making in councils and community planning partnerships. This includes the ",
          tags$a("Improvement Service (IS)",href = "https://www.improvementservice.org.uk/benchmarking", target = "_blank"),
          " and ",
          tags$a("Community Planning Outcomes Profile.",href = "https://www.improvementservice.org.uk/products-and-services/data-intelligence-and-benchmarking/community-planning-outcomes-profile", target = "_blank")),
  br()
  ),
  
  nav_panel("Additional links",
            h3("Additional links"),
            p("While the Population Health dashboard provides a strategic focus on a selection of indicators, a wealth of information is already available to provide Scottish 
              insights relating to each of the eight Marmot principles. The following section highlights some key publications:"),
            tags$ol(
              tags$li(tags$b("Early years"),
                      "PHS produce early ",
                      tags$a("child development statistics",href = "https://publichealthscotland.scot/publications/early-child-development/early-child-development-statistics-scotland-2022-to-2023/", target = "_blank"),
                      " annually. This provides data on the development of children aged 0-5 years, including the percentage of children presenting with a developmental concern.",
                      "PHS also publish the ",
                      tags$a("Health in the early years (HEYS) dashboard",href = "https://publichealthscotland.scot/publications/health-in-the-early-years-heys-dashboard/", target = "_blank"),
                      ". This presents data on breastfeeding and early child development, including breakdowns for local areas.",
                      "The Scottish Government regularly gather data on the funded early learning and childcare (ELC) entitlement through the ELC census. This collects information on ELC provisions
                      from services that deliver this entitlement."),
              br(),
              tags$li(tags$b("Education"),
                      "The Scottish Government publishes ",tags$a("school education statistics",href = "https://www.gov.scot/collections/school-education-statistics/", target = "_blank"),
                      " annually. These statistics provide detail on the annual including pupil and teacher characteristics, as well has attainment and qualification 
                      results. This is accompanied by various interactive dashboards that provide breakdowns for local areas."),
              br(),
              tags$li(tags$b("Work"),
                      "The Scottish Government regularly publishes ",
                      tags$a("labour market statistics",href = "https://www.gov.scot/collections/labour-market-statistics/", target = "_blank"),
                      ". This includes Labour Force Survey data relating to employment, unemployment, and economic inactivity trends."),
              br(),
              tags$li(tags$b("Living standards"),
                      "The Scottish Government regularly publish ",tags$a("poverty and income inequality",href = "https://www.gov.scot/collections/poverty-and-income-inequality-statistics/", target = "_blank")," statistics.",
                      "The Scottish Government also regularly publish data on ",tags$a("primary income account and Gross National Income",href = "https://data.gov.scot/primary-income-account/", target = "_blank"),".",
                      "The annual ",tags$a("Scottish Household Survey",href = "https://www.gov.scot/publications/scottish-household-survey-2022-key-findings/pages/5/", target = "_blank")," collects data on household finances.",
                      "The ",tags$a("Scottish House Condition Survey",href = "https://www.gov.scot/collections/scottish-house-condition-survey/", target = "_blank")," collects data on the the physical condition of Scotland’s
                      homes and the experiences of householders."),
              br(),
              tags$li(tags$b("Healthy places"),
                      "The ",tags$a("Scottish Index of Multiple Deprivation",href = "https://simd.scot/#/simd2020/BTTTFTT/7.49077135520893/-4.0132/55.9875/", target = "_blank"),
                      " measures the relative levels of deprivation in specific geographical areas across Scotland. This is based on information collected by the Scottish Government.",
                      "The ",tags$a("Place Standard tool",href = "https://simd.scot/#/simd2020/BTTTFTT/7.49077135520893/-4.0132/55.9875/", target = "_blank"),
                      " was developed by the Place Standard partners, which include the Scottish Government, Public Health Scotland, Architecture and Design Scotland,
                      the Improvement Service and Glasgow City Council. The tool provides a framework for assessing places to inform local planning decisions. The",
                      tags$a("Place and Wellbeing Outcomes",href = "https://www.improvementservice.org.uk/products-and-services/planning-and-place-based-approaches/planning-for-place-programme/place-and-wellbeing-outcomes", target = "_blank"),
                      " were developed alongside the Place Standard tool, with relevant indicators identified to inform against the outcomes. ",
                      tags$a("Understanding Scotland’s Places (USP)",href = "https://www.usp.scot/", target = "_blank"),
                      "helps users to better understand the local areas in which people work and live. Deliberately designed to avoid a simplistic ranking of places as better or worse, USP focuses on the shared characteristics of towns."),
              br(),
              tags$li(tags$b("Impact of ill-health prevention"),
                      "PHS regularly publish ",
                      tags$a("disease prevalence data and statistics",href = "https://publichealthscotland.scot/publications/general-practice-disease-prevalence-data-visualisation/general-practice-disease-prevalence-visualisation-27-june-2023/", target = "_blank"),
                      ". Vaccination is one of the most effective public health interventions to prevent disease and protect against severe disease. PHS’ ",
                      tags$a("Vaccination Surveillance Dashboard",href = "https://scotland.shinyapps.io/phs-vaccination-surveillance/", target = "_blank"),
                      "presents data to monitor trends in vaccination uptake, including equality breakdowns.",
                      "National screening programmes have an important role to play in reducing health inequalities. PHS publish screening statistics for programmes including ",
                      tags$a("bowel",href = "https://publichealthscotland.scot/publications/scottish-bowel-screening-programme-statistics/", target = "_blank"),",",
                      tags$a("breast",href = "https://publichealthscotland.scot/publications/scottish-bowel-screening-programme-statistics/", target = "_blank"),"and ",
                      tags$a("cervical",href = "https://publichealthscotland.scot/publications/scottish-bowel-screening-programme-statistics/", target = "_blank")," screening.",
                      "The ", tags$a("Scottish Health Survey provides",href = "https://www.gov.scot/collections/scottish-health-survey/", target = "_blank"),
                      " a detailed picture of the health of the Scottish population, and is designed to make a major contribution to the monitoring of health in Scotland."),
              br(),
              tags$li(tags$b("Discrimination and racism"),
                      "The Scottish Government’s ",
                      tags$a("Equality Evidence Finder",href = "https://publichealthscotland.scot/publications/scottish-bowel-screening-programme-statistics/", target = "_blank"),
                      " brings together the latest statistics and research for Scotland across different equality characteristics. This includes age, disability, ethnicity, 
                      gender, religion, sexual orientation, socio-economic status, and transgender status."),
              br(),
              tags$li(tags$b("Environmental sustainability and health equity"),
                      "The Scottish Government regularly produces statistics on ",
                      tags$a("greenhouse gas emissions",href = "https://www.gov.scot/publications/scottish-greenhouse-gas-statistics-2022/", target = "_blank"),
                      " and ",
                      tags$a("Scotland's carbon footprint",href = "https://www.gov.scot/publications/scotlands-carbon-footprint-1998-2018/", target = "_blank"),
                      ". The ",
                      tags$a("Scottish Climate Survey",href = "https://www.gov.scot/collections/scottish-climate-survey/", target = "_blank"),
                      "is a nationally representative survey of Scottish adults’ awareness, understanding, and experiences of climate change-related issues. The Scottish Government also produces ",
                      tags$a("energy statistics for Scotland",href = "https://www.gov.scot/collections/quarterly-energy-statistics-scotland/", target = "_blank"),
                      ", which outlines trends in energy usage and energy targets each quarter. ",
                      "The annual ",
                      tags$a("climate change monitoring report",href = "https://www.gov.scot/publications/climate-change-monitoring-report-2024/", target = "_blank"),
                      "is also produced by the Scottish Government, which measures progress against the Scottish Government’s ",
                      tags$a("Scottish Climate Survey",href = "https://www.gov.scot/collections/scottish-climate-survey/", target = "_blank"),
                      tags$a("climate change plan",href = "https://www.gov.scot/publications/securing-green-recovery-path-net-zero-update-climate-change-plan-20182032/", target = "_blank"),
                      ".")
            ), #close ordered list
            br()
  ),#close nav panel
  
  nav_panel("Feedback",
            h3("Feedback"),
            p("The content of the Population Health dashboard will remain under review. We welcome feedback on all aspects of the dashboard including the indicator content and presentation. Please send any feedback to ",
              tags$a("phs.scotpho@phs.scot", href = "mailto:phs.scotpho@phs.scot", target = "_blank")),
            br())
) #close pop health navset

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
p("We expect that the following professional groups will find the infmation contained here of particular
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
  