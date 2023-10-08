{-# LANGUAGE OverloadedStrings #-}

module OrderByTest where

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Database.Orville.PostgreSQL as O
import qualified Database.Orville.PostgreSQL.Select as S
import qualified TestDB as TestDB

import Control.Monad (void)
import Data.Int (Int64)
import Database.Orville.PostgreSQL.Expr (aliased, qualified)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

test_order_by :: TestTree
test_order_by =
  TestDB.withOrvilleRun $ \run ->
    testGroup
      "Order by queries"
      [ testCase "Sort Ascending" $ do
          run (TestDB.reset schema)
          void $ run (O.insertRecord orderTable foobarOrder)
          void $ run (O.insertRecord orderTable anotherFoobarOrder)
          void $ run (O.insertRecord orderTable orderNamedAlice)
          result <- run (O.findRecordsBy orderTable customerFkIdField $
                          O.order orderNameField O.Ascending
                        )
          let lookupResult =
                case M.lookup (CustomerId 2) result of
                  Just res -> fmap orderName res
                  Nothing -> []

          assertEqual
            "Order returned didn't match expected result"
            [ orderName orderNamedAlice
            , orderName anotherFoobarOrder
            ]
            lookupResult
      , testCase "Sort Descending" $ do
          run (TestDB.reset schema)
          void $ run (O.insertRecord orderTable foobarOrder)
          void $ run (O.insertRecord orderTable anotherFoobarOrder)
          void $ run (O.insertRecord orderTable orderNamedAlice)
          result <- run (O.findRecordsBy orderTable customerFkIdField $
                          O.order orderNameField O.Descending
                        )
          let lookupResult =
                case M.lookup (CustomerId 2) result of
                  Just res -> fmap orderName res
                  Nothing -> []

          assertEqual
            "Order returned didn't match expected result"
            [ orderName anotherFoobarOrder
            , orderName orderNamedAlice
            ]
            lookupResult
      ]

data CompleteOrder = CompleteOrder
  { order :: T.Text
  , customer :: T.Text
  } deriving (Eq, Show)

completeOrderSelect :: O.SelectOptions -> S.Select CompleteOrder
completeOrderSelect = S.selectQuery buildCompleteOrder orderCustomerFrom

buildCompleteOrder :: O.FromSql CompleteOrder
buildCompleteOrder =
  CompleteOrder <$>
  O.col
    (S.selectField orderNameField `qualified` "order" `aliased` "order_name") <*>
  O.col
    (S.selectField customerNameField `qualified` "customer" `aliased`
     "customer_name")

orderCustomerFrom :: S.FromClause
orderCustomerFrom =
  S.fromClauseRaw
    "FROM \"order\" INNER JOIN \"customer\" ON \"order\".\"customer_id\" = \"customer\".\"id\""

schema :: O.SchemaDefinition
schema = [O.Table orderTable, O.Table customerTable]

-- Order definitions
orderTable :: O.TableDefinition Order Order OrderId
orderTable =
  O.mkTableDefinition $
  O.TableParams
    { O.tblName = "order"
    , O.tblPrimaryKey = O.primaryKey orderIdField
    , O.tblMapper =
        Order <$> O.attrField orderId orderIdField <*>
        O.attrField customerFkId customerFkIdField <*>
        O.attrField orderName orderNameField
    , O.tblGetKey = orderId
    , O.tblSafeToDelete = []
    , O.tblComments = O.noComments
    }

orderIdField :: O.FieldDefinition O.NotNull OrderId
orderIdField =
  O.int64Field "id" `O.withConversion`
  O.convertSqlType unOrderId OrderId

customerFkIdField :: O.FieldDefinition O.NotNull CustomerId
customerFkIdField =
  O.int64Field "customer_id" `O.withConversion`
  O.convertSqlType unCustomerId CustomerId

orderNameField :: O.FieldDefinition O.NotNull OrderName
orderNameField =
  O.textField "name" 255 `O.withConversion`
  O.convertSqlType unOrderName OrderName

data Order = Order
  { orderId :: OrderId
  , customerFkId :: CustomerId
  , orderName :: OrderName
  } deriving (Show, Eq)

newtype OrderId = OrderId
  { unOrderId :: Int64
  } deriving (Show, Eq)

newtype OrderName = OrderName
  { unOrderName :: T.Text
  } deriving (Show, Eq)

foobarOrder :: Order
foobarOrder =
  Order
    { orderId = OrderId 1
    , customerFkId = CustomerId 1
    , orderName = OrderName "foobar"
    }

anotherFoobarOrder :: Order
anotherFoobarOrder =
  Order
    { orderId = OrderId 3
    , customerFkId = CustomerId 2
    , orderName = OrderName "Foobar"
    }

orderNamedAlice :: Order
orderNamedAlice =
  Order
    { orderId = OrderId 2
    , customerFkId = CustomerId 2
    , orderName = OrderName "Alice"
    }

orderNameSelect :: O.SelectOptions -> S.Select OrderName
orderNameSelect = S.selectQuery buildOrderName orderNameFrom

buildOrderName :: O.FromSql OrderName
buildOrderName = OrderName <$> O.col (S.selectField orderNameField)

orderNameFrom :: S.FromClause
orderNameFrom = S.fromClauseTable orderTable

-- Customer definitions
customerTable :: O.TableDefinition Customer Customer CustomerId
customerTable =
  O.mkTableDefinition $
  O.TableParams
    { O.tblName = "customer"
    , O.tblPrimaryKey = O.primaryKey customerIdField
    , O.tblMapper =
        Customer <$> O.attrField customerId customerIdField <*>
        O.attrField customerName customerNameField
    , O.tblGetKey = customerId
    , O.tblSafeToDelete = []
    , O.tblComments = O.noComments
    }

customerIdField :: O.FieldDefinition O.NotNull CustomerId
customerIdField =
  O.int64Field "id" `O.withConversion`
  O.convertSqlType unCustomerId CustomerId

customerNameField :: O.FieldDefinition O.NotNull CustomerName
customerNameField =
  O.textField "name" 255 `O.withConversion`
  O.convertSqlType unCustomerName CustomerName

data Customer = Customer
  { customerId :: CustomerId
  , customerName :: CustomerName
  } deriving (Show, Eq)

newtype CustomerId = CustomerId
  { unCustomerId :: Int64
  } deriving (Show, Eq, Ord)

newtype CustomerName = CustomerName
  { unCustomerName :: T.Text
  } deriving (Show, Eq)
