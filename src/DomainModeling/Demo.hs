{-#LANGUAGE OverloadedStrings#-}

module DomainModeling.Demo where

import           DomainModeling.Project

someProject :: Project () ProjectId
someProject = ProjectGroup "Sweden" () [stockholm, gothenburg, malmo]
  where
    stockholm  = Project  "Stocklm" 1
    gothenburg = Project  "Gothenburg" 2
    malmo      = ProjectGroup "Malo" () [city, limhamn]
    city       = Project  "malo city" 3
    limhamn     = Project  "Limhamn" 4


