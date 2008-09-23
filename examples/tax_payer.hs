import HCL
import Text.Regex (mkRegex, matchRegex)
import Data.Maybe (isNothing)

data Taxpayer = Taxpayer { name :: String, age :: Int, ssn :: String }
 deriving (Read, Show)

reqTaxpayer :: Request Taxpayer
reqTaxpayer = prompt "Please enter tax payer information: " (reqRead reqResp)

reqTaxpayerEasy :: Request Taxpayer
reqTaxpayerEasy =
  do
    name <- prompt "Please enter the tax payer's name: " reqResp
    age <- prompt "Please enter their age: " reqInt
    ssn <- prompt "What is their SSN/ASN: " reqResp
    return (Taxpayer name age ssn)

reqTaxpayerValidate :: Request Taxpayer
reqTaxpayerValidate =
  do
    name <- prompt "Please enter the tax payer's name: " reqResp
    age <- prompt "Please enter their age: " reqInt
    ssn <- reqSSN (prompt "What is their SSN/ASN: " reqResp)
    return (Taxpayer name age ssn)

getTaxpayer which =
  execReq $
    do
      taxPayer <- which
      reqIO $ putStrLn $ "You entered: " ++ show taxPayer

reqSSN :: Request String -> Request String
reqSSN req =
  do
    -- very simple validation
    let
      matchSSN = matchRegex (mkRegex "^...-..-....$")
      invalidSSN ssn = return $ isNothing (matchSSN ssn)
    ssn <- reqWhile invalidSSN req
    return ssn
