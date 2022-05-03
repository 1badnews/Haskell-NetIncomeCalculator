module Lib where

decimalPoint :: Double -> Double
decimalPoint x = (fromIntegral (floor (x*t)))/t
  where t= 10^2 

defaultPersonalAllowance :: Double
defaultPersonalAllowance = 12570

personalAllowance :: Double -> Double 
personalAllowance income = if income > 150000
  then 0
  else if income > 100000 && income < 150000
    then defaultPersonalAllowance-((income-100000)/2)
  else defaultPersonalAllowance

studentLoan :: Double -> Double
studentLoan income = 
  if income < 27288
    then 0
    else decimalPoint ((income-27288)*0.09)

incomeTax :: Double -> Double
incomeTax income =
  if income < personalAllowance income
    then 0
  else if income <= 37700+personalAllowance income
    then decimalPoint ((income-personalAllowance income)*0.2 )
  else if income > 37700+personalAllowance income && income < 150000+personalAllowance income+37700
    then decimalPoint (((income-personalAllowance income-37700)*0.4)+7540)
  else decimalPoint (60000+7540+((income-150000-37700-personalAllowance income)*0.45))

nationalInsurance :: Double -> Double
nationalInsurance income = 
  if income < 9564
    then 0
  else if income > 9564 && income < 50268
    then decimalPoint ((income-9564)*0.12)
  else decimalPoint (((income-50268-9564)*0.02)+6032.16)

totalIncome :: Double -> Double
totalIncome income = decimalPoint (income - incomeTax income - nationalInsurance income)

totalStudentIncome :: Double -> Double
totalStudentIncome income = decimalPoint (income -studentLoan income - incomeTax income - nationalInsurance income)

