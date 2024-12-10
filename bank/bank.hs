module Bank where

newtype BankOp a = BankOp { runBankOpWith :: Float -> (a, Float) }

runBankOp :: BankOp a -> a
runBankOp bankOp = fst (runBankOpWith bankOp 0.0)

deposit :: Float -> BankOp ()
deposit amount = BankOp $ \currentBalance -> ((), currentBalance + amount)

withdraw :: Float -> BankOp Float
withdraw amount = BankOp $ \currentBalance ->
    let overdraftLimit = -100  -- Maximum overdraft allowed
        projectedBalance = currentBalance - amount
        actualWithdrawal = if projectedBalance < overdraftLimit
                           then currentBalance - overdraftLimit  -- Restrict withdrawal
                           else amount
        finalBalance = max projectedBalance overdraftLimit
    in (actualWithdrawal, finalBalance)

getBalance :: BankOp Float
getBalance = BankOp $ \currentBalance -> (currentBalance, currentBalance)

instance Monad BankOp where
    return = pure  -- same as pure
    (BankOp g) >>= f = BankOp $ \balance -> 
        let (result, newBalance) = g balance
            BankOp h = f result
        in h newBalance

instance Functor BankOp where
    fmap f (BankOp g) = BankOp $ \balance -> 
        let (result, newBalance) = g balance
        in (f result, newBalance)

instance Applicative BankOp where
    pure x = BankOp $ \balance -> (x, balance)
    (BankOp f) <*> (BankOp g) = BankOp $ \balance -> 
        let (func, balance1) = f balance
            (result, balance2) = g balance1
        in (func result, balance2)
