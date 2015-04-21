module BigDataAggregator where
import qualified Data.Map as Map
import qualified Data.List.Split as S

-- Domain

type Partner = String
type Currency = String
type Amount = Double
type ExchangeRates = Map.Map (Currency, Currency) Double
data Money = Money { amount :: Amount, currency :: Currency } deriving (Show)  
data Transaction = Transaction { money :: Money, partner :: Partner }
type Transactions = [Transaction]

convert :: Money -> Currency -> ExchangeRates -> Money
convert (Money amount currency) targetCurrency exchangeRates 
    | currency == targetCurrency = (Money amount currency)
    | otherwise = Money (amount * rate) targetCurrency
        where rate = Map.findWithDefault 0 (currency, targetCurrency) exchangeRates

plus :: Money -> Money -> Money
plus (Money amount1 c) (Money amount2 _) = Money (amount1 + amount2) c

aggregateTransactionsOfPartner :: Transactions -> Partner -> Currency -> ExchangeRates -> Money
aggregateTransactionsOfPartner transactions p targetCurrency exchangeRates = 
    foldl plus (Money 0 targetCurrency) 
    $ map (\tr -> convert (money tr) targetCurrency exchangeRates) 
    $ filter (\tr -> p == partner tr) transactions    


aggregateTransactionsByPartner :: Transactions -> Currency -> ExchangeRates -> Map.Map Partner Money
aggregateTransactionsByPartner trs targetCurrency exchangeRates = foldl accumulate Map.empty trs where        
    {accumulate :: Map.Map Partner Money -> Transaction -> Map.Map Partner Money ;
     accumulate acc tr = 
        let p = (partner tr)
            currentValue = Map.findWithDefault (Money 0 targetCurrency) p acc
            convertedValue = convert (money tr) targetCurrency exchangeRates
            updatedValue = plus currentValue convertedValue
        in Map.insert p updatedValue acc }   

-- Infrastructure
lineToTransaction :: [Char] -> Transaction
lineToTransaction line = 
    let parts = S.splitOn "," line
        partner = parts !! 0
        currency = parts !! 1
        amount = read (parts !! 2) :: Amount
    in Transaction (Money amount currency) partner    

main = do
    let transactions = map lineToTransaction ["XY,GBP,10", "ZZ,USD,20", "ZZ,GBP,30"]
        exchangeRates = Map.fromList [(("USD","GBP"),1.5),(("GBP","USD"),0.7)]
        aggregatedForPartner = aggregateTransactionsOfPartner transactions "ZZ" "USD" exchangeRates
        aggregatedByPartner = aggregateTransactionsByPartner transactions "USD" exchangeRates
    putStrLn $ show $ aggregatedForPartner 
    putStrLn $ show $ aggregatedByPartner 
