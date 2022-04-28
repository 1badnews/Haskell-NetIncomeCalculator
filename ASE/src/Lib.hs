module Lib where

defaultPersonalAllowance :: Double
defaultPersonalAllowance = 12570

personalAllowance :: Double -> Double 
personalAllowance income = if income > 150000
  then 0
  else defaultPersonalAllowance

studentLoan :: Double -> Double
studentLoan income = 
  if income < 27288
    then 0
    else ((income-27288)*0.09)*1e2/1e2

incomeTax :: Double -> Double
incomeTax income =
  if income < personalAllowance income
    then 0
  else if income <= 37700+personalAllowance income
    then ((income-personalAllowance income)*0.2 )*1e2/1e2
  else if income > 37700+personalAllowance income && income < 150000+personalAllowance income+37700
    then (((income-personalAllowance income-37700)*0.4)+7540)*1e2/1e2
  else 60000+7540+((income-150000-37700-personalAllowance income)*0.45)*1e2/1e2

nationalInsurance :: Double -> Double
nationalInsurance income = 
  if income < 9564
    then 0
  else if income > 9564 && income < 50268
    then ((income-9564)*0.12)*1e2/1e2
  else (((income-50268-9564)*0.02)+6032.16)*1e2/1e2 

totalIncome :: Double -> Double
totalIncome income = (income - incomeTax income - nationalInsurance income)*1e2/1e2

totalStudentIncome :: Double -> Double
totalStudentIncome income = ((income -studentLoan income - incomeTax income - nationalInsurance income)*1e2/1e2)

