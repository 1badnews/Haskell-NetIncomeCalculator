import Test.HUnit
import Test.QuickCheck
import Lib

main :: IO ()
main = do
    result <- runTestTT allTests
    quickCheck prop_studentLoan
    quickCheck prop_incomeTax
    quickCheck prop_nationalInsurance
    quickCheck prop_totalIncome
    quickCheck prop_totalStudentIncome
    quickCheck prop_personalAllowance
    return ()

allTests :: Test 
allTests = TestList [

-- personal allowance --
  testEqualDouble "Normal personal allowance" defaultPersonalAllowance (personalAllowance 20000),
  testEqualDouble "Personal allowance with deduction" 7570 (personalAllowance 110000),
  testEqualDouble "Normal personal allowance" defaultPersonalAllowance (personalAllowance 70000),
  testEqualDouble "No personal allowance" 0 (personalAllowance 200000),
-- personal allowance --

  -- income tax -- 
  testEqualDouble "Basic tax rate" 1486 (incomeTax 20000),
  testEqualDouble "Higher tax rate" 15432 (incomeTax 70000),
  testEqualDouble "Additional tax rate" 73075 (incomeTax 200000),
  -- income tax -- 

  -- student loan repayments --
  testEqualDouble "No student loan repayment" 0 (studentLoan 20000),
  testEqualDouble "Student loan repayment" 3844.08 (studentLoan 70000),
  testEqualDouble "Student loan repayment" 15544.08 (studentLoan 200000),
  -- student loan repayments --

  -- national insurance contributions --
  testEqualDouble "National insurance contribution" 1252.32 (nationalInsurance 20000),
  testEqualDouble "National insurance contribution" 6235.52 (nationalInsurance 70000),
  testEqualDouble "National insurance contribution" 8835.52 (nationalInsurance 200000),
  -- national insurance contributions -- 

  -- net income --
  testEqualDouble "Total income after tax" 17261.68 (totalIncome 20000),
  testEqualDouble "Total income after tax" 48332.48 (totalIncome 70000),
  testEqualDouble "Total income after tax" 118089.48 (totalIncome 200000),
  -- net income --

  -- net income with student loan --
  testEqualDouble "Total income after tax (including student loan)" 17261.68 (totalStudentIncome 20000),
  testEqualDouble "Total income after tax (including student loan)" 44488.39 (totalStudentIncome 70000),
  testEqualDouble "Total income after tax (including student loan)" 102545.40 (totalStudentIncome 200000)
  -- net income with student loan --
  ]

testEqualDouble :: String -> Double -> Double -> Test 
testEqualDouble name result input = TestCase (assertEqual name result input)


-- Property Tests --

prop_studentLoan :: Double -> Bool
prop_studentLoan income =
  let calcStudentLoan = if (income < 27288) then 0  else decimalPoint ((income-27288)*0.09) in
    studentLoan income == calcStudentLoan

prop_incomeTax :: Double -> Bool
prop_incomeTax income =
  let calcIncomeTax = if income < personalAllowance income then 0 else if income <= 37700+personalAllowance income then decimalPoint ((income-personalAllowance income)*0.2 ) else if income > 37700+personalAllowance income && income < 150000+personalAllowance income+37700 then decimalPoint (((income-personalAllowance income-37700)*0.4)+7540) else decimalPoint (60000+7540+((income-150000-37700-personalAllowance income)*0.45)) in 
    incomeTax income == calcIncomeTax


prop_nationalInsurance :: Double -> Bool
prop_nationalInsurance income = 
  let calcNationalInsurance =  if income < 9564 then 0 else if income > 9564 && income < 50268 then decimalPoint ((income-9564)*0.12) else decimalPoint (((income-50268-9564)*0.02)+6032.16) in
    nationalInsurance income == calcNationalInsurance

prop_totalIncome :: Double -> Bool
prop_totalIncome income =
  let calctotalIncome = decimalPoint (income - incomeTax income - nationalInsurance income) in
    totalIncome income == calctotalIncome

prop_totalStudentIncome :: Double -> Bool
prop_totalStudentIncome income =
  let calctotalStudentIncome = decimalPoint (income -studentLoan income - incomeTax income - nationalInsurance income) in
    totalStudentIncome income == calctotalStudentIncome

prop_personalAllowance :: Double -> Bool
prop_personalAllowance income =
  let calcpersonalAllowance = if income > 150000 then 0 else if income > 100000 && income < 150000 then defaultPersonalAllowance-((income-100000)/2) else defaultPersonalAllowance in
    personalAllowance income == calcpersonalAllowance