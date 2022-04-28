import Test.HUnit

import Lib

main :: IO ()
main = do
    result <- runTestTT allTests
    return ()

allTests :: Test 
allTests = TestList [

-- personal allowance --
-- testEqual "No tax to pay" 0 (incomeTax 10000),
  testEqualDouble "Normal personal allowance" defaultPersonalAllowance (personalAllowance 20000),
  testEqualDouble "Normal personal allowance" defaultPersonalAllowance (personalAllowance 70000),
  testEqualDouble "Normal personal allowance" 0 (personalAllowance 200000),
-- personal allowance --

  -- income tax -- 
  testEqualDouble "Basic tax rate" 1486 (incomeTax 20000),
  testEqualDouble "Higher tax rate" 15432 (incomeTax 70000),
  testEqualDouble "Additional tax rate" 73075 (incomeTax 200000),
  -- income tax -- 

  -- student loan repayments --
  testEqualDouble "Student loan repayment" 0 (studentLoan 20000),
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
  testEqualDouble "Total income after tax (including student loan)" 44488.40 (totalStudentIncome 70000),
  testEqualDouble "Total income after tax (including student loan)" 102545.40 (totalStudentIncome 200000)
  -- net income with student loan --
  ]

testEqual :: String -> Int -> Int -> Test 
testEqual name result input = TestCase (assertEqual name result input)

testEqualDouble :: String -> Double -> Double -> Test 
testEqualDouble name result input = TestCase (assertEqual name result input)




