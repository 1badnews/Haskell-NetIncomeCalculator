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
    else (income-27288)*0.09

incomeTax :: Double -> Double
incomeTax income =
  if income < personalAllowance income
    then 0
  else if income <= 37700+personalAllowance income
    then (income-personalAllowance income)*0.2
  else if income > 37700+personalAllowance income && income < 150000+personalAllowance income+37700
    then (((income-personalAllowance income-37700)*0.4)+7540)
  else 60000+7540+((income-150000-37700-personalAllowance income)*0.45)

