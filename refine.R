#Assignment 1 - Clean up refine_original.csv

# Task 1 - Correct brand names, the way the company names are 
# spelt and make them uniform.

func1<-function(y){if(substr(y, 1, 1)=="a"|substr(y, 1, 1)=="A"){print("Akzo")} else if(substr(y, 1, 1)=="p"|substr(y, 1, 1)=="f"|substr(y, 1, 1)=="P"){print("Phillips")} else if(substr(y, 1, 1)=="u"|substr(y, 1, 1)=="U"){print("Unilever")} else print("Van Houten")}
r_f1<-mutate(refine_original, company_new=lapply(as.character(company), func1))
r_f11 <- arrange(r_f1, company)
r_f2 <- bind_cols(select(r_f11, company_new), select(r_f11, -company))

# Task 2 - Separate the Product.code...number column into two columns.
r_f3 <- separate(r_f2, Product.code...number, c("Product.Code", "Product.Number"), sep="-", remove=TRUE, convert=FALSE)

#Task 3 - Label the products with the respective product names.
func2 <- function(y){if(y=="p"){print("Smartphone")} else if(y=="v"){print("TV")} else if(y=="x"){print("Laptop")} else if(y=="q"){print("Tablet")}}
r_f4 <- mutate(test1, Product.Name = lapply(r_f3$Product.Code, func2))
r_f5 <- bind_cols(select(r_f4, company_new:Product.Number, Product.Name), select(r_f4, address:name))

#Task 4 - Create a column with the full adress by uniting address:country.
r_f6 <- unite(r_f5, "full_address", address:country, sep=", ")

#Task 5 - Create binary columns for all the companies and their products.
r_f7 <- mutate(r_f6, company_phillips=as.numeric(r_f6$company_new=="Phillips"))
r_f7_1 <- mutate(r_f7, company_akzo=as.numeric(r_f7$company_new=="Akzo"))
r_f7_2 <- mutate(r_f7_1, company_unilever=as.numeric(r_f7_1$company_new=="Unilever"))
r_f7_3 <- mutate(r_f7_2, company_van_houten=as.numeric(r_f7_2$company_new=="Van Houten"))
r_f7_4 <- mutate(r_f7_3, product_smartphone=as.numeric(r_f7_3$Product.Name=="Smartphone"))
r_f7_5 <- mutate(r_f7_4, product_TV=as.numeric(r_f7_4$Product.Name=="TV"))
r_f7_6 <- mutate(r_f7_5, product_Laptop=as.numeric(r_f7_5$Product.Name=="Laptop"))
r_f8 <- mutate(r_f7_6, product_Tablet=as.numeric(r_f7_6$Product.Name=="Tablet"))

#Task 6 - Save the cleaned up data in a file.
write.csv(as.character(r_f8), file="refine_clean.csv")

