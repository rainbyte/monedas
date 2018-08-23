# monedas-core

## How to build

1. Install `stack` to handle project deps automatically.
1. Build the project running `stack build` command.
1. Execute provided server using `stack exec monedas-core-exe`
1. Service should be available via port 5000.

## REST API: Main Operations

1. API usage examples are provided in this section.
1. Install `httpie` to test them.
1. Run the server before testing the API.

### Create a new currency

```bash
http POST localhost:5000/createCurrency 'name=ARS'
```

### Send money between users

```bash
echo '{"from": "foo", "to": "bar", "currency": "ARS", "amount": 1000}' | http POST localhost:5000/sendMoney
```

### Inspect transaction registry

```bash
http GET localhost:5000/inspectLedger
```

### View users balances

```bash
http GET localhost:5000/userBalances
```

## REST API: Other Operations

### Create a new user

```bash
http POST localhost:5000/createUser 'name=foo'
```

### Deposit money to an user

This operation debits funds from `#Income` account to preserve consistency.

```bash
echo '{"to": "bar", "currency": "ARS", "amount": 1000}' | http POST localhost:5000/deposit
```

### Extract money from an user

This operation credits funds to `#Expense` account to preserve consistency.

```bash
echo '{"from": "foo", "currency": "ARS", "amount": 1000}' | http POST localhost:5000/extract
```

### List users

```bash
http GET localhost:5000/users
```


### List currencies

```bash
http GET localhost:5000/currencies
```