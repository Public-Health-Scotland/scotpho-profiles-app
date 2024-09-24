
####################################################
# About SIMD
####################################################
about_simd_quintiles <- tagList(
      tags$p("To prepare the charts shown we divide geographical areas into five groups (also known as quintiles) based on their relative levels of deprivation, as measured by the ",
        tags$a(href="https://www2.gov.scot/simd",  "Scottish Index of Multiple Deprivation (SIMD).", target = "_blank")),
      tags$p("Those living in areas assigned to quintile 1 experience the highest levels of relative deprivation and those living in quintile 5 the lowest relative deprivation."),
      tags$p("Geographical areas are assigned to a within Scotland quintile or a local quintile (e.g. within NHS board or within local authority quintile) based on the SIMD ranking."),
      tags$p("Indicator data for an NHS board or council area is presented by local deprivation quintiles by default. This tool allows users to switch from the default local quintiles to view the same data according to Scotland quintiles.")
    )





####################################################
# About Relative index of inequality (RII) measure 
####################################################
about_rii <- tagList(
  tags$div(
  tags$h6("What does the chart show?", class = "chart-header"),
  tags$p("The chart below shows how relative inequality (the gap between the least disadvantaged group
     and the average of all groups) has changed over time."),
  tags$p("The values in the chart are known as the 'Relative Index of Inequality (RII)' for each year this 
          is calculated using a linear regression model of the social variable (in this case the SIMD quintiles) 
          and the selected indicator measure (e.g. rate of hospitalisations/deaths/etc)."),
  tags$p("The RII represents the inequality gap between the most disadvantaged and the overall average. ScotPHO use a
          linear regression model and have converted the RII so that the value in the chart represents the percentage
          difference of the rate in the most disadvantaged group relative to the rate in the overall population.")
  ),
  tags$div(
  tags$h6("How do I interpret the chart?", class = "chart-header"),
  tags$p("If there were no difference between indicator values in the most disadvantaged area and the overall
          average the RII would be zero. The larger the RII the greater the inequity between the most disadvantaged
          areas and the overall average. It is possible for relative inequality to reduce but absolute inequality
          to increase which why it is important to consider trends in both the SII and RII.")
  )
  )



#########################################################
# About Slope index of inequality (SII)
########################################################

about_sii <- tagList(
  tags$div(
    tags$h6("What does the chart show?", class = "chart-header"),
    tags$p("The chart shows the absolute inequality has changed over time. Absolute inequality is
            measured using a value called the 'Slope Index of Inequality (SII)'. This is a measure of
            the gap between the most and least disadvantaged populations.")
    ),
  tags$div(
    tags$h6("How do I interpret the chart?", class = "chart-header"),
    tags$p("The larger the SII, the greater the disparity between the most and least deprived areas.
            An increasing trend suggests the gap between the most and least deprived areas is growing.")
    )
)

###########################################################
# About population attributable risk trends (PAR)
##########################################################


about_par <- tagList(
  tags$div(
    tags$h6("What does the chart show?", class = "chart-header"),
    tags$p("The bar chart shows indicator values split by the deprivation quintiles. The area shaded
       in purple is the same across all 5 quintiles, it shows the rate observed in the least deprived
       quintile. The area shaded in blue represents the additional activity the 4 remaining quintiles
       have over and above that seen in the least deprived quintile."),
    tags$p("Looking at data in this way illustrates the potential impact of removing deprivation (i.e. 
    in the hypothetical situation that all deprivation quintiles experienced the same rates).")
    ),
  tags$div(
    tags$h6("Related resources", class = "chart-header"),
    tags$p("You can read more about the Population Attributable Risk in the", tags$a("Measuring inequalities 
           section", href = "https://www.scotpho.org.uk/methods-and-data/measuring-health-inequalities/", target = "_blank"),  "of the ScotPHO website.")
    
  )
)


about_par_trend <- tagList(
  tags$div(
  tags$h6("What does the chart show?", class = "chart-header"),
  tags$p("The line chart shows the Population Attributable Risk (PAR) also known as Population Attributable Fraction (PAF).
          The PAR is presented as a percentage, and describes by how much the overall rate of an indicator would increase or
          decrease if all areas were to experience the rates observed in the most favourable area. ")
  ),
  tags$div(
  tags$h6("How do I interpret the chart?", class = "chart-header"),
  tags$p("The higher the PAR values the greater the impact of inequality on that indicator and the greater the potential for
          improvement if this inequality could be removed."),
  tags$p("The PAF describes a hypothetical situation and makes the assumption that all of the association between the risk 
  factor and indicator is causal. In reality there could a number of other factors influencing the trends observed.")
  ),
  tags$div(
  tags$h6("Related resources", class = "chart-header"),
  tags$p("You can read more about the PAR in the", tags$a("Measuring inequalities 
         section", href = "https://www.scotpho.org.uk/methods-and-data/measuring-health-inequalities/", target = "_blank")," of the ScotPHO website.")
  )
  )

###########################################################
# About SIMD trend chart
###########################################################

about_simd <- tags$div(
  tags$h6("What does the chart show?", class = "chart-header"),
  tags$p("The bar chart shows how the measure for a particular indicator varies according to the relative deprivation 
  of the area people live in. The chart illustrate how rates in the most and least deprived areas 
  compare and also whether there is a simple relationship between relative deprivation and a particular indicator.")
)



about_simd_trend <- tags$div(
  tags$h6("What does the chart show?", class = "chart-header"),
  tags$p("The trend chart shows how the measure for a particular indicator varies according to the relative deprivation 
  of the area people live in, over a period of time. The chart illustrate how rates in the most and least deprived areas 
  compare over time and also whether there is a simple relationship between relative deprivation and a particular indicator.")
)


