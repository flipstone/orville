{-# LANGUAGE OverloadedStrings #-}

module WhereConditionTest where

import qualified Data.Text as T
import qualified Database.Orville.Oracle as O
import qualified Database.Orville.Oracle.Select as S
import qualified TestDB as TestDB

import Control.Monad (void)
import Data.Int (Int64)
import Database.HDBC (toSql)
import Database.Orville.Oracle ((.==))
import Database.Orville.Oracle.Expr (aliased, qualified)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

test_where_condition :: TestTree
test_where_condition =
  TestDB.withOrvilleRun $ \run ->
    testGroup
      "Where condition queries"
      [ testCase "Where distinct" $ do
          run (TestDB.reset schema)
          void $ run (O.insertRecord orderTable foobarOrder)
          void $ run (O.insertRecord orderTable anotherFoobarOrder)
          let opts = O.distinct
          result <- run (S.runSelect $ orderNameSelect opts)
          assertEqual
            "Order returned didn't match expected result"
            [orderName foobarOrder]
            result
      , testCase "Where like" $ do
          run (TestDB.reset schema)
          void $ run (O.insertRecord orderTable foobarOrder)
          void $ run (O.insertRecord orderTable orderNamedAlice)
          let opts = O.where_ $ O.whereLike orderNameField "%li%"
          result <- run (O.selectAll orderTable opts)
          assertEqual
            "Order returned didn't match expected result"
            [orderNamedAlice]
            result
      , testCase "Where like insensitive" $ do
          run (TestDB.reset schema)
          void $ run (O.insertRecord orderTable foobarOrder)
          void $ run (O.insertRecord orderTable orderNamedAlice)
          let opts = O.where_ $ O.whereLikeInsensitive orderNameField "%LI%"
          result <- run (O.selectAll orderTable opts)
          assertEqual
            "Order returned didn't match expected result"
            [orderNamedAlice]
            result
      , testCase "Qualified where" $ do
          run (TestDB.reset schema)
          void $ run (O.insertRecord orderTable foobarOrder)
          void $ run (O.insertRecord orderTable orderNamedAlice)
          void $ run (O.insertRecord customerTable aliceCustomer)
          void $ run (O.insertRecord customerTable bobCustomer)
          let opts =
                O.where_ $
                O.whereQualified customerTable $
                (customerNameField .== customerName aliceCustomer)
          result <- run (S.runSelect $ completeOrderSelect opts)
          assertEqual
            "Order returned didn't match expected result"
            [CompleteOrder {order = "foobar", customer = "Alice"}]
            result
      , testCase "Raw where" $ do
          run (TestDB.reset schema)
          void $ run (O.insertRecord orderTable foobarOrder)
          void $ run (O.insertRecord orderTable orderNamedAlice)
          void $ run (O.insertRecord customerTable aliceCustomer)
          void $ run (O.insertRecord customerTable bobCustomer)
          let opts =
                O.where_ $
                O.whereRaw "customer.name = ?" [toSql ("Alice" :: String)]
          result <- run (S.runSelect $ completeOrderSelect opts)
          assertEqual
            "Order returned didn't match expected result"
            [CompleteOrder {order = "foobar", customer = "Alice"}]
            result
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
    , O.tblPrimaryKey = orderIdField
    , O.tblMapper =
        Order <$> O.attrField orderId orderIdField <*>
        O.attrField customerFkId customerFkIdField <*>
        O.attrField orderName orderNameField
    , O.tblGetKey = orderId
    , O.tblSafeToDelete = []
    , O.tblComments = O.noComments
    }

orderIdField :: O.FieldDefinition OrderId
orderIdField =
  O.int64Field "id" `O.withFlag` O.PrimaryKey `O.withConversion`
  O.convertSqlType unOrderId OrderId

customerFkIdField :: O.FieldDefinition CustomerId
customerFkIdField =
  O.int64Field "customer_id" `O.withConversion`
  O.convertSqlType unCustomerId CustomerId

orderNameField :: O.FieldDefinition OrderName
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
    , orderName = OrderName "foobar"
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
    , O.tblPrimaryKey = customerIdField
    , O.tblMapper =
        Customer <$> O.attrField customerId customerIdField <*>
        O.attrField customerName customerNameField
    , O.tblGetKey = customerId
    , O.tblSafeToDelete = []
    , O.tblComments = O.noComments
    }

customerIdField :: O.FieldDefinition CustomerId
customerIdField =
  O.int64Field "id" `O.withFlag` O.PrimaryKey `O.withConversion`
  O.convertSqlType unCustomerId CustomerId

customerNameField :: O.FieldDefinition CustomerName
customerNameField =
  O.textField "name" 255 `O.withConversion`
  O.convertSqlType unCustomerName CustomerName

data Customer = Customer
  { customerId :: CustomerId
  , customerName :: CustomerName
  } deriving (Show, Eq)

newtype CustomerId = CustomerId
  { unCustomerId :: Int64
  } deriving (Show, Eq)

newtype CustomerName = CustomerName
  { unCustomerName :: T.Text
  } deriving (Show, Eq)

aliceCustomer :: Customer
aliceCustomer =
  Customer {customerId = CustomerId 1, customerName = CustomerName "Alice"}

bobCustomer :: Customer
bobCustomer =
  Customer {customerId = CustomerId 2, customerName = CustomerName "Bob"}
