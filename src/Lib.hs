module Lib ( tax )
          where

tax :: Float -> Float
tax income = taxed
  where

    taxable -- Taxable Income (Income - Personal Allowance)
     | income > 100000 && income < 125000 = income - (12500 - (income-100000)/2)
     | income >= 125000   = income
     | income < 12500     = 0
     | otherwise          = income - 12500


    taxed -- End Income (Taxed)
     | income >= 0      = income - tax
     | otherwise        = 0

    tax = tax1 + tax2 + tax3 -- Overall Tax

    tax1 -- First Tax Bracket
     | taxable > 37500    = 37500 * 0.2
     | otherwise          = taxable * 0.2
    tax2 -- Second Tax Bracket
     | taxable <= 37500   = 0
     | taxable > 150000   = 112500 * 0.4
     | otherwise          = (taxable-37500) * 0.4
    tax3 -- Final Tax Bracket
     | taxable <= 150000  = 0
     | otherwise          = (taxable-150000) * 0.45
