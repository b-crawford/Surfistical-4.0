spot_names = function(vec){
  for(i in 1:length(vec)){
    if(grepl("Gold Coast",x = vec[i], fixed = T)==1){
      vec[i] = "GoldCoast"
    }
    if(grepl("Margaret River",x = vec[i], fixed = T)==1){
      vec[i] = "MargaretRiver"
    }
    if(grepl("Bells Beach",x = vec[i], fixed = T)==1){
      vec[i] = "BellsBeach"
    }
    if(grepl("Rio",x = vec[i], fixed = T)==1){
      vec[i] = "Rio"
    }
    if(grepl("Fiji",x = vec[i], fixed = T)==1){
      vec[i] = "Fiji"
    }
    if(grepl("J-Bay",x = vec[i], fixed = T)==1){
      vec[i] = "Jbay"
    }
    if(grepl("Tahiti",x = vec[i], fixed = T)==1){
      vec[i] = "Tahiti"
    }
    if(grepl("Teahupoo",x = vec[i], fixed = T)==1){
      vec[i] = "Tahiti"
    }
    if(grepl("France",x = vec[i], fixed = T)==1){
      vec[i] = "France"
    }
    if(grepl("Portugal",x = vec[i], fixed = T)==1){
      vec[i] = "Portugal"
    }
    if(grepl("Pipe",x = vec[i], fixed = T)==1){
      vec[i] = "Pipeline"
    }
    if(grepl("Bali",x = vec[i], fixed = T)==1){
      vec[i] = "Bali"
    }
    if(grepl("Trestles",x = vec[i], fixed = T)==1){
      vec[i] = "Trestles"
    }
    if(grepl("Catarina",x = vec[i], fixed = T)==1){
      vec[i] = "SantaCatarina"
    }
    if(grepl("New York",x = vec[i], fixed = T)==1){
      vec[i] = "NewYork"
    }
    if(grepl("Cruz",x = vec[i], fixed = T)==1){
      vec[i] = "SantaCruz"
    }
    if(grepl("US Open",x = vec[i], fixed = T)==1){
      vec[i] = "USOpen"
    }
    if(grepl("Cascais",x = vec[i], fixed = T)==1){
      vec[i] = "Cascais"
    }
    if(grepl("Maui",x = vec[i], fixed = T)==1){
      vec[i] = "Maui"
    }
    if(grepl("Swatch",x = vec[i], fixed = T)==1){
      vec[i] = "Trestles"
    }
    if(grepl("NZ",x = vec[i], fixed = T)==1){
      vec[i] = "NewZealand"
    }
    if(grepl("Subaru",x = vec[i], fixed = T)==1){
      vec[i] = "NewZealand"
    }
    if(grepl("Beachley",x = vec[i], fixed = T)==1){
      vec[i] = "Beachley"
    }
    if(grepl("Peru",x = vec[i], fixed = T)==1){
      vec[i] = "Peru"
    }
    if(grepl("World Cup",x = vec[i], fixed = T)==1){
      vec[i] = "Oahu"
    }
    if(grepl("Rip Curl Search",x = vec[i], fixed = T)==1){
      vec[i] = "Search"
    }
  }
  return(vec)
} 

