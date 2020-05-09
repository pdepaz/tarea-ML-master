set3 <- c("`marital-status.Married-civ-spouse`", "`occupation.Blue-Collar`", "`capital-gain`", 
          "`hours-per-week`", "occupation.Services", "age", "`education.HS-Graduate`", "`education.High-School`",
          "`education.Elem-Middle-School`", "education.Associates", "`relationship.Own-child`",
          "education.Bachelors", "`occupation.Exec-managerial`", "education.Masters", "`relationship.Other-relative`",
          "`occupation.Adm-clerical`", "relationship.Wife", "sex.Female", "`workclass.Self-employed`",
          "`marital-status.Never-married`", "fnlwgt", "`marital-status.Married-AF-spouse`", "`native-country.United-States`",
          "occupation.Sales", "`relationship.Not-in-family`", "`native-country.Western-Europe`", "`capital-loss`", "race.Black",
          "`native-country.South-America`", "`race.Amer-Indian-Eskimo`", "`race.Asian-Pac-Islander`",
          "`native-country.N-Asia`", "`native-country.North-America`") # 33. From lista_AIC[[2]][[1]]

set4 <- c("`marital-status.Married-civ-spouse`", "`occupation.Blue-Collar`", "`capital-gain`", 
          "`hours-per-week`", "occupation.Services", "age", "`education.HS-Graduate`", "`education.High-School`",
          "`education.Elem-Middle-School`", "education.Associates", "`relationship.Own-child`",
          "education.Bachelors", "`occupation.Exec-managerial`", "education.Masters", "`relationship.Other-relative`",
          "`occupation.Adm-clerical`", "relationship.Wife", "sex.Female", "`workclass.Self-employed`",
          "`marital-status.Never-married`", "fnlwgt", "`marital-status.Married-AF-spouse`", "`native-country.United-States`",
          "occupation.Sales", "`relationship.Not-in-family`", "`native-country.Western-Europe`", "`capital-loss`", "race.Black",
          "`native-country.South-America`", "`race.Amer-Indian-Eskimo`", "`native-country.SE-Asia`", 
          "`native-country.Eastern-Europe`", "`workclass.Not-Working`") # 33. From lista_AIC[[2]][[2]]

set5 <- c(set4, "`occupation.Prof-specialty`") # 34. From lista_AIC[[2]][[3]]

set6 <- c("`marital-status.Married-civ-spouse`", "`occupation.Blue-Collar`", "`capital-gain`", 
          "`hours-per-week`", "occupation.Services", "age", "education.Bachelors", "education.Masters",
          "`education.Prof-school`", "education.Doctorate", "education.Associates", "`education.High-School`",
          "`education.Elem-Middle-School`", "`relationship.Own-child`", "`occupation.Exec-managerial`",
          "`relationship.Other-relative`", "`occupation.Adm-clerical`", "relationship.Wife", "sex.Female", 
          "`workclass.Self-employed`", "`marital-status.Never-married`", "race.White", "fnlwgt",
          "`native-country.South-America`", "occupation.Sales", "`native-country.North-America`",
          "`capital-loss`", "`marital-status.Married-AF-spouse`", "`relationship.Not-in-family`",
          "`native-country.N-Asia`", "`race.Asian-Pac-Islander`", "`native-country.Central-America`",
          "race.Black", "`native-country.Middle-East`") #34. From lista_AIC[[2]][[4]]

set7 <- c("`marital-status.Married-civ-spouse`", "`occupation.Blue-Collar`", "`capital-gain`", 
          "`hours-per-week`", "occupation.Services", "age", "education.Bachelors", "education.Masters",
          "`education.Prof-school`", "education.Doctorate", "education.Associates", "`education.High-School`",
          "`education.Elem-Middle-School`", "`relationship.Own-child`", "`occupation.Exec-managerial`",
          "`relationship.Other-relative`", "`occupation.Adm-clerical`", "relationship.Wife", "sex.Female", 
          "`workclass.Self-employed`", "`marital-status.Never-married`", "race.White", "fnlwgt",
          "`native-country.South-America`", "occupation.Sales", "`native-country.North-America`",
          "`capital-loss`", "`marital-status.Married-AF-spouse`", "`relationship.Not-in-family`",
          "`native-country.N-Asia`", "`race.Asian-Pac-Islander`", "`native-country.Central-America`",
          "`workclass.Not-Working`") # 33. From lista_AIC[[2]][[5]]

set8 <- c("`marital-status.Married-civ-spouse`", "`occupation.Blue-Collar`", "`capital-gain`", 
          "`hours-per-week`", "occupation.Services", "age", "education.Bachelors", "education.Masters",
          "`education.Prof-school`", "education.Doctorate", "`education.High-School`", "`education.Elem-Middle-School`",
          "`relationship.Own-child`", "`occupation.Exec-managerial`", "`education.HS-Graduate`",
          "`relationship.Other-relative`", "`occupation.Adm-clerical`", "relationship.Wife", "sex.Female", 
          "`workclass.Self-employed`", "`marital-status.Never-married`", "race.White", "fnlwgt",
          "`native-country.South-America`", "occupation.Sales", "`native-country.North-America`",
          "`capital-loss`", "`marital-status.Married-AF-spouse`", "`relationship.Not-in-family`",
          "`native-country.N-Asia`", "`race.Asian-Pac-Islander`", "`native-country.Central-America`") # 32. From lista_AIC[[2]][[6]]

set9 <- c("`marital-status.Married-civ-spouse`", "`occupation.Blue-Collar`", "`capital-gain`", 
          "`hours-per-week`", "occupation.Services", "age", "`education.HS-Graduate`", "`education.High-School`",
          "`education.Elem-Middle-School`", "education.Associates", "education.Bachelors", "education.Masters",
          "`relationship.Own-child`", "`occupation.Exec-managerial`", "`relationship.Other-relative`", 
          "`occupation.Adm-clerical`", "relationship.Wife", "sex.Female", "`workclass.Self-employed`", 
          "`marital-status.Never-married`", "`native-country.United-States`",
          "occupation.Sales", "`relationship.Not-in-family`", "`native-country.Western-Europe`", 
          "`capital-loss`", "race.Black", "fnlwgt", "`marital-status.Married-AF-spouse`") # 28. From lista_BIC[[2]][[1]]

set10 <- c("`marital-status.Married-civ-spouse`", "`occupation.Blue-Collar`", "`capital-gain`", 
           "`hours-per-week`", "occupation.Services", "age", "education.Bachelors", "education.Masters",
           "`education.Prof-school`", "education.Doctorate", "`education.High-School`", "`education.Elem-Middle-School`",
           "`relationship.Own-child`", "`occupation.Exec-managerial`", "education.Associates",
           "`occupation.Adm-clerical`", "relationship.Wife", "sex.Female", "`workclass.Self-employed`", 
           "`marital-status.Never-married`", "fnlwgt", "`relationship.Other-relative`",
           "`native-country.North-America`", "`native-country.South-America`", "occupation.Sales",
           "`marital-status.Married-AF-spouse`", "`relationship.Not-in-family`", "`capital-loss`", "race.White") # 29. From lista_BIC[[2]][[2]]

set11 <- c("`marital-status.Married-civ-spouse`", "`occupation.Blue-Collar`", "`capital-gain`", 
           "`hours-per-week`", "occupation.Services", "age", "education.Bachelors", "education.Masters",
           "education.Associates", "`education.Elem-Middle-School`", "`education.HS-Graduate`",
           "`education.High-School`", "`relationship.Own-child`", 
           "`occupation.Exec-managerial`", "`relationship.Other-relative`", "`occupation.Adm-clerical`", 
           "relationship.Wife", "sex.Female", "`workclass.Self-employed`", "`marital-status.Never-married`", 
           "fnlwgt", "`marital-status.Married-AF-spouse`", "occupation.Sales", "`relationship.Not-in-family`",
           "`native-country.United-States`", "`native-country.Western-Europe`") # 26. From lista_BIC[[2]][[3]]

set12 <- c("`marital-status.Married-civ-spouse`", "`occupation.Blue-Collar`", "`capital-gain`", 
           "`hours-per-week`", "occupation.Services", "age", "education.Bachelors", "education.Masters",
           "`education.Prof-school`", "education.Doctorate", "`education.High-School`", "`education.Elem-Middle-School`",
           "`relationship.Own-child`", "`occupation.Exec-managerial`", "`education.HS-Graduate`",
           "`relationship.Other-relative`", "relationship.Wife", "sex.Female", "`workclass.Self-employed`", 
           "`marital-status.Never-married`", "`occupation.Adm-clerical`", "fnlwgt",
           "`native-country.South-America`", "occupation.Sales", "`native-country.North-America`",
           "`marital-status.Married-AF-spouse`", "`relationship.Not-in-family`", "`capital-loss`", "race.White") # 29. From lista_BIC[[2]][[4]]