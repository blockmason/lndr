# lndr-backend

## Settlement Flow

The settlement endpoints are the same as the normal non-settlement credit
endpoints, /lend and /borrow. However, a settlement is distinguished from
a non-settlement by the inclusion of a json key / value pair in the request
body of the form "settlementCurrency" : "{Currency name}". Currently, the
server only supports settlements in eth, so { "settlementCurrency" : "ETH" } is
the only thing you should be adding to the json body of your /lend or /borrow
requests. Once a unilateral settlement credit is submitted, it assumed the
"amount" field is given in USD and a corresponding ETH settlement amount is
determined using the current ETH/USD exchange rate on coinbase. Pending
settlements will not be returned in /pending calls. Rather, there is a separate
/pending_settlements endpoint that returns unilateral pending settlement
credits along with bilateral pending settlements (signed by both parties but
not yet associated with a ether transfer). Once a unilateral pending settlement
credit is approved by a counterparty, becoming a bilateral pending settlement,
the server requires that a txHash of an ether transfer between the two parties
for the exact eth amount indicated in the /pending_settlements record be
submitted to the /verify_settlement endpoint.

## LNDR Server

Web service API

## POST /add_friends/:user

#### Authentication



Clients must supply the following data


#### Captures:

- *user*: the address of the user whose friends will be returned

#### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (): `application/json;charset=utf-8`

```javascript
[]
```

- Example (): `application/json`

```javascript
[]
```

- Example (): `application/json;charset=utf-8`

```javascript
["0x11edd217a875063583dd1b638d16810c5d34d54b"]
```

- Example (): `application/json`

```javascript
["0x11edd217a875063583dd1b638d16810c5d34d54b"]
```

- Example (): `application/json;charset=utf-8`

```javascript
["0x11edd217a875063583dd1b638d16810c5d34d54b","0x11edd217a875063583dd1b638d16810c5d34d54b"]
```

#### Response:

- Status code 204
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

-

```javascript

```

-

```javascript

```

## GET /balance/:p1/:p2

#### Authentication



Clients must supply the following data


#### Captures:

- *p1*: the address of the first party in a credit relationship
- *p2*: the address of the second party in a credit relationship

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

-

```javascript
19
```

-

```javascript
19
```

## GET /balance/:user

#### Authentication



Clients must supply the following data


#### Captures:

- *user*: the address of the user whose friends will be returned

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

-

```javascript
19
```

-

```javascript
19
```

## POST /borrow

#### Authentication



Clients must supply the following data


#### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (): `application/json;charset=utf-8`

```javascript
{"creditor":"0x11edd217a875063583dd1b638d16810c5d34d54b","debtor":"0x6a362e5cee1cf5a5408ff1e12b0bc546618dffcb","amount":69,"memo":"test memo","submitter":"0x11edd217a875063583dd1b638d16810c5d34d54b","nonce":0,"hash":"0x4358c649de5746c91673378dd4c40a78feda715166913e09ded45343ff76841c","signature":"0x457b0db63b83199f305ef29ba2d7678820806d98abbe3f6aafe015957ecfc5892368b4432869830456c335ade4f561603499d0216cda3af7b6b6cadf6f273c101b"}
```

- Example (): `application/json`

```javascript
{"creditor":"0x11edd217a875063583dd1b638d16810c5d34d54b","debtor":"0x6a362e5cee1cf5a5408ff1e12b0bc546618dffcb","amount":69,"memo":"test memo","submitter":"0x11edd217a875063583dd1b638d16810c5d34d54b","nonce":0,"hash":"0x4358c649de5746c91673378dd4c40a78feda715166913e09ded45343ff76841c","signature":"0x457b0db63b83199f305ef29ba2d7678820806d98abbe3f6aafe015957ecfc5892368b4432869830456c335ade4f561603499d0216cda3af7b6b6cadf6f273c101b"}
```

#### Response:

- Status code 204
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

-

```javascript

```

-

```javascript

```

## GET /config

#### Authentication



Clients must supply the following data


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

-

```javascript
{"lndrAddress":"0x11edd217a875063583dd1b638d16810c5d34d54b","creditProtocolAddress":"0x6a362e5cee1cf5a5408ff1e12b0bc546618dffcb"}
```

-

```javascript
{"lndrAddress":"0x11edd217a875063583dd1b638d16810c5d34d54b","creditProtocolAddress":"0x6a362e5cee1cf5a5408ff1e12b0bc546618dffcb"}
```

## GET /counterparties/:user

#### Authentication



Clients must supply the following data


#### Captures:

- *user*: the address of the user whose friends will be returned

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

-

```javascript
[]
```

-

```javascript
[]
```

-

```javascript
["0x11edd217a875063583dd1b638d16810c5d34d54b"]
```

-

```javascript
["0x11edd217a875063583dd1b638d16810c5d34d54b"]
```

-

```javascript
["0x11edd217a875063583dd1b638d16810c5d34d54b","0x11edd217a875063583dd1b638d16810c5d34d54b"]
```

## GET /docs

#### Authentication



Clients must supply the following data


#### Response:

- Status code 200
- Headers: []

- No response body

## GET /friends/:user

#### Authentication



Clients must supply the following data


#### Captures:

- *user*: the address of the user whose friends will be returned

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

-

```javascript
[]
```

-

```javascript
[]
```

-

```javascript
[{"addr":"0x11edd217a875063583dd1b638d16810c5d34d54b","nick":"aupiff"}]
```

-

```javascript
[{"addr":"0x11edd217a875063583dd1b638d16810c5d34d54b","nick":"aupiff"}]
```

-

```javascript
[{"addr":"0x11edd217a875063583dd1b638d16810c5d34d54b","nick":"aupiff"},{"addr":"0x11edd217a875063583dd1b638d16810c5d34d54b","nick":"aupiff"}]
```

## GET /gas_price

#### Authentication



Clients must supply the following data


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

-

```javascript
19
```

-

```javascript
19
```

## PUT /gas_price

#### Authentication



Clients must supply the following data


#### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (): `application/json;charset=utf-8`

```javascript
19
```

- Example (): `application/json`

```javascript
19
```

#### Response:

- Status code 204
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

-

```javascript

```

-

```javascript

```

## POST /lend

#### Authentication



Clients must supply the following data


#### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (): `application/json;charset=utf-8`

```javascript
{"creditor":"0x11edd217a875063583dd1b638d16810c5d34d54b","debtor":"0x6a362e5cee1cf5a5408ff1e12b0bc546618dffcb","amount":69,"memo":"test memo","submitter":"0x11edd217a875063583dd1b638d16810c5d34d54b","nonce":0,"hash":"0x4358c649de5746c91673378dd4c40a78feda715166913e09ded45343ff76841c","signature":"0x457b0db63b83199f305ef29ba2d7678820806d98abbe3f6aafe015957ecfc5892368b4432869830456c335ade4f561603499d0216cda3af7b6b6cadf6f273c101b"}
```

- Example (): `application/json`

```javascript
{"creditor":"0x11edd217a875063583dd1b638d16810c5d34d54b","debtor":"0x6a362e5cee1cf5a5408ff1e12b0bc546618dffcb","amount":69,"memo":"test memo","submitter":"0x11edd217a875063583dd1b638d16810c5d34d54b","nonce":0,"hash":"0x4358c649de5746c91673378dd4c40a78feda715166913e09ded45343ff76841c","signature":"0x457b0db63b83199f305ef29ba2d7678820806d98abbe3f6aafe015957ecfc5892368b4432869830456c335ade4f561603499d0216cda3af7b6b6cadf6f273c101b"}
```

#### Response:

- Status code 204
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

-

```javascript

```

-

```javascript

```

## POST /nick

#### Authentication



Clients must supply the following data


#### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (): `application/json;charset=utf-8`

```javascript
{"addr":"0x11edd217a875063583dd1b638d16810c5d34d54b","nick":"aupiff","signature":"0x457b0db63b83199f305ef29ba2d7678820806d98abbe3f6aafe015957ecfc5892368b4432869830456c335ade4f561603499d0216cda3af7b6b6cadf6f273c101b"}
```

- Example (): `application/json`

```javascript
{"addr":"0x11edd217a875063583dd1b638d16810c5d34d54b","nick":"aupiff","signature":"0x457b0db63b83199f305ef29ba2d7678820806d98abbe3f6aafe015957ecfc5892368b4432869830456c335ade4f561603499d0216cda3af7b6b6cadf6f273c101b"}
```

#### Response:

- Status code 204
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

-

```javascript

```

-

```javascript

```

## GET /nick/:user

#### Authentication



Clients must supply the following data


#### Captures:

- *user*: the address of the user whose friends will be returned

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

-

```javascript
"aupiff"
```

-

```javascript
"aupiff"
```

## GET /nonce/:p1/:p2

#### Authentication



Clients must supply the following data


#### Captures:

- *p1*: the address of the first party in a credit relationship
- *p2*: the address of the second party in a credit relationship

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

-

```javascript
1
```

-

```javascript
1
```

## GET /pending/:user

#### Authentication



Clients must supply the following data


#### Captures:

- *user*: the address of the user whose friends will be returned

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

-

```javascript
[]
```

-

```javascript
[]
```

-

```javascript
[{"creditor":"0x11edd217a875063583dd1b638d16810c5d34d54b","debtor":"0x6a362e5cee1cf5a5408ff1e12b0bc546618dffcb","amount":69,"memo":"test memo","submitter":"0x11edd217a875063583dd1b638d16810c5d34d54b","nonce":0,"hash":"0x4358c649de5746c91673378dd4c40a78feda715166913e09ded45343ff76841c","signature":"0x457b0db63b83199f305ef29ba2d7678820806d98abbe3f6aafe015957ecfc5892368b4432869830456c335ade4f561603499d0216cda3af7b6b6cadf6f273c101b"}]
```

-

```javascript
[{"creditor":"0x11edd217a875063583dd1b638d16810c5d34d54b","debtor":"0x6a362e5cee1cf5a5408ff1e12b0bc546618dffcb","amount":69,"memo":"test memo","submitter":"0x11edd217a875063583dd1b638d16810c5d34d54b","nonce":0,"hash":"0x4358c649de5746c91673378dd4c40a78feda715166913e09ded45343ff76841c","signature":"0x457b0db63b83199f305ef29ba2d7678820806d98abbe3f6aafe015957ecfc5892368b4432869830456c335ade4f561603499d0216cda3af7b6b6cadf6f273c101b"}]
```

-

```javascript
[{"creditor":"0x11edd217a875063583dd1b638d16810c5d34d54b","debtor":"0x6a362e5cee1cf5a5408ff1e12b0bc546618dffcb","amount":69,"memo":"test memo","submitter":"0x11edd217a875063583dd1b638d16810c5d34d54b","nonce":0,"hash":"0x4358c649de5746c91673378dd4c40a78feda715166913e09ded45343ff76841c","signature":"0x457b0db63b83199f305ef29ba2d7678820806d98abbe3f6aafe015957ecfc5892368b4432869830456c335ade4f561603499d0216cda3af7b6b6cadf6f273c101b"},{"creditor":"0x11edd217a875063583dd1b638d16810c5d34d54b","debtor":"0x6a362e5cee1cf5a5408ff1e12b0bc546618dffcb","amount":69,"memo":"test memo","submitter":"0x11edd217a875063583dd1b638d16810c5d34d54b","nonce":0,"hash":"0x4358c649de5746c91673378dd4c40a78feda715166913e09ded45343ff76841c","signature":"0x457b0db63b83199f305ef29ba2d7678820806d98abbe3f6aafe015957ecfc5892368b4432869830456c335ade4f561603499d0216cda3af7b6b6cadf6f273c101b"}]
```

## GET /pending_settlements/:user

#### Authentication



Clients must supply the following data


#### Captures:

- *user*: the address of the user whose friends will be returned

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

-

```javascript
{"unilateralSettlements":[{"creditor":"0x11edd217a875063583dd1b638d16810c5d34d54b","debtor":"0x6a362e5cee1cf5a5408ff1e12b0bc546618dffcb","amount":69,"memo":"test memo","submitter":"0x11edd217a875063583dd1b638d16810c5d34d54b","nonce":0,"hash":"0x4358c649de5746c91673378dd4c40a78feda715166913e09ded45343ff76841c","signature":"0x457b0db63b83199f305ef29ba2d7678820806d98abbe3f6aafe015957ecfc5892368b4432869830456c335ade4f561603499d0216cda3af7b6b6cadf6f273c101b"}],"bilateralSettlements":[{"creditor":"0x11edd217a875063583dd1b638d16810c5d34d54b","debtor":"0x6a362e5cee1cf5a5408ff1e12b0bc546618dffcb","amount":69,"memo":"test memo","submitter":"0x11edd217a875063583dd1b638d16810c5d34d54b","nonce":0,"hash":"0x4358c649de5746c91673378dd4c40a78feda715166913e09ded45343ff76841c","signature":"0x457b0db63b83199f305ef29ba2d7678820806d98abbe3f6aafe015957ecfc5892368b4432869830456c335ade4f561603499d0216cda3af7b6b6cadf6f273c101b"}]}
```

-

```javascript
{"unilateralSettlements":[{"creditor":"0x11edd217a875063583dd1b638d16810c5d34d54b","debtor":"0x6a362e5cee1cf5a5408ff1e12b0bc546618dffcb","amount":69,"memo":"test memo","submitter":"0x11edd217a875063583dd1b638d16810c5d34d54b","nonce":0,"hash":"0x4358c649de5746c91673378dd4c40a78feda715166913e09ded45343ff76841c","signature":"0x457b0db63b83199f305ef29ba2d7678820806d98abbe3f6aafe015957ecfc5892368b4432869830456c335ade4f561603499d0216cda3af7b6b6cadf6f273c101b"}],"bilateralSettlements":[{"creditor":"0x11edd217a875063583dd1b638d16810c5d34d54b","debtor":"0x6a362e5cee1cf5a5408ff1e12b0bc546618dffcb","amount":69,"memo":"test memo","submitter":"0x11edd217a875063583dd1b638d16810c5d34d54b","nonce":0,"hash":"0x4358c649de5746c91673378dd4c40a78feda715166913e09ded45343ff76841c","signature":"0x457b0db63b83199f305ef29ba2d7678820806d98abbe3f6aafe015957ecfc5892368b4432869830456c335ade4f561603499d0216cda3af7b6b6cadf6f273c101b"}]}
```

## POST /register_push

#### Authentication



Clients must supply the following data


#### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (): `application/json;charset=utf-8`

```javascript
{"channelID":"31279004-103e-4ba8-b4bf-65eb3eb81859","platform":"ios","address":"0x11edd217a875063583dd1b638d16810c5d34d54b","signature":""}
```

- Example (): `application/json`

```javascript
{"channelID":"31279004-103e-4ba8-b4bf-65eb3eb81859","platform":"ios","address":"0x11edd217a875063583dd1b638d16810c5d34d54b","signature":""}
```

#### Response:

- Status code 204
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

-

```javascript

```

-

```javascript

```

## POST /reject

#### Authentication



Clients must supply the following data


#### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (): `application/json;charset=utf-8`

```javascript
{"hash":"0x457b0db63b83199f305ef29ba2d7678820806d98abbe3f6aafe015957ecfc5892368b4432869830456c335ade4f561603499d0216cda3af7b6b6cadf6f273c101b","signature":"0x4358c649de5746c91673378dd4c40a78feda715166913e09ded45343ff76841c"}
```

- Example (): `application/json`

```javascript
{"hash":"0x457b0db63b83199f305ef29ba2d7678820806d98abbe3f6aafe015957ecfc5892368b4432869830456c335ade4f561603499d0216cda3af7b6b6cadf6f273c101b","signature":"0x4358c649de5746c91673378dd4c40a78feda715166913e09ded45343ff76841c"}
```

#### Response:

- Status code 204
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

-

```javascript

```

-

```javascript

```

## POST /remove_friends/:user

#### Authentication



Clients must supply the following data


#### Captures:

- *user*: the address of the user whose friends will be returned

#### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (): `application/json;charset=utf-8`

```javascript
[]
```

- Example (): `application/json`

```javascript
[]
```

- Example (): `application/json;charset=utf-8`

```javascript
["0x11edd217a875063583dd1b638d16810c5d34d54b"]
```

- Example (): `application/json`

```javascript
["0x11edd217a875063583dd1b638d16810c5d34d54b"]
```

- Example (): `application/json;charset=utf-8`

```javascript
["0x11edd217a875063583dd1b638d16810c5d34d54b","0x11edd217a875063583dd1b638d16810c5d34d54b"]
```

#### Response:

- Status code 204
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

-

```javascript

```

-

```javascript

```

## POST /resubmit/:hash

#### Authentication



Clients must supply the following data


#### Captures:

- *hash*: the hash by which to identify a credit record among candidates for resubmission

#### Response:

- Status code 204
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

-

```javascript

```

-

```javascript

```

## GET /search_nick/:nick

#### Authentication



Clients must supply the following data


#### Captures:

- *nick*: the nickname to be associated with a particular address

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

-

```javascript
[]
```

-

```javascript
[]
```

-

```javascript
[{"addr":"0x11edd217a875063583dd1b638d16810c5d34d54b","nick":"aupiff"}]
```

-

```javascript
[{"addr":"0x11edd217a875063583dd1b638d16810c5d34d54b","nick":"aupiff"}]
```

-

```javascript
[{"addr":"0x11edd217a875063583dd1b638d16810c5d34d54b","nick":"aupiff"},{"addr":"0x11edd217a875063583dd1b638d16810c5d34d54b","nick":"aupiff"}]
```

## GET /taken_nick/:nick

#### Authentication



Clients must supply the following data


#### Captures:

- *nick*: the nickname to be associated with a particular address

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

-

```javascript
false
```

-

```javascript
false
```

-

```javascript
true
```

-

```javascript
true
```

## GET /transactions

#### Authentication



Clients must supply the following data


#### GET Parameters:

- user
     - **Values**: *0x11edd217a875063583dd1b638d16810c5d34d54b, 0x6a362e5cee1cf5a5408ff1e12b0bc546618dffcb*
     - **Description**: address of user whose records to display


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

-

```javascript
[]
```

-

```javascript
[]
```

-

```javascript
[{"ucac":"0xd5ec73eac35fc9dd6c3f440bce314779fed09f60","creditor":"0x11edd217a875063583dd1b638d16810c5d34d54b","debtor":"0x6a362e5cee1cf5a5408ff1e12b0bc546618dffcb","amount":69,"nonce":0,"memo":"simple memo"}]
```

-

```javascript
[{"ucac":"0xd5ec73eac35fc9dd6c3f440bce314779fed09f60","creditor":"0x11edd217a875063583dd1b638d16810c5d34d54b","debtor":"0x6a362e5cee1cf5a5408ff1e12b0bc546618dffcb","amount":69,"nonce":0,"memo":"simple memo"}]
```

-

```javascript
[{"ucac":"0xd5ec73eac35fc9dd6c3f440bce314779fed09f60","creditor":"0x11edd217a875063583dd1b638d16810c5d34d54b","debtor":"0x6a362e5cee1cf5a5408ff1e12b0bc546618dffcb","amount":69,"nonce":0,"memo":"simple memo"},{"ucac":"0xd5ec73eac35fc9dd6c3f440bce314779fed09f60","creditor":"0x11edd217a875063583dd1b638d16810c5d34d54b","debtor":"0x6a362e5cee1cf5a5408ff1e12b0bc546618dffcb","amount":69,"nonce":0,"memo":"simple memo"}]
```

## GET /unsubmitted

#### Authentication



Clients must supply the following data


#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

-

```javascript
[]
```

-

```javascript
[]
```

-

```javascript
[{"ucac":"0xd5ec73eac35fc9dd6c3f440bce314779fed09f60","creditor":"0x11edd217a875063583dd1b638d16810c5d34d54b","debtor":"0x6a362e5cee1cf5a5408ff1e12b0bc546618dffcb","amount":69,"nonce":0,"memo":"simple memo"}]
```

-

```javascript
[{"ucac":"0xd5ec73eac35fc9dd6c3f440bce314779fed09f60","creditor":"0x11edd217a875063583dd1b638d16810c5d34d54b","debtor":"0x6a362e5cee1cf5a5408ff1e12b0bc546618dffcb","amount":69,"nonce":0,"memo":"simple memo"}]
```

-

```javascript
[{"ucac":"0xd5ec73eac35fc9dd6c3f440bce314779fed09f60","creditor":"0x11edd217a875063583dd1b638d16810c5d34d54b","debtor":"0x6a362e5cee1cf5a5408ff1e12b0bc546618dffcb","amount":69,"nonce":0,"memo":"simple memo"},{"ucac":"0xd5ec73eac35fc9dd6c3f440bce314779fed09f60","creditor":"0x11edd217a875063583dd1b638d16810c5d34d54b","debtor":"0x6a362e5cee1cf5a5408ff1e12b0bc546618dffcb","amount":69,"nonce":0,"memo":"simple memo"}]
```

## POST /verify_settlement

#### Authentication



Clients must supply the following data


#### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (): `application/json;charset=utf-8`

```javascript
{"creditHash":"0x4358c649de5746c91673378dd4c40a78feda715166913e09ded45343ff76841c","txHash":"0xf357c689de57464713697787d4c40a78feda913162911e191e545343ff769999","creditorAddress":"0x6a362e5cee1cf5a5408ff1e12b0bc546618dffcb","signature":"0x457b0db63b83199f305ef29ba2d7678820806d98abbe3f6aafe015957ecfc5892368b4432869830456c335ade4f561603499d0216cda3af7b6b6cadf6f273c101b"}
```

- Example (): `application/json`

```javascript
{"creditHash":"0x4358c649de5746c91673378dd4c40a78feda715166913e09ded45343ff76841c","txHash":"0xf357c689de57464713697787d4c40a78feda913162911e191e545343ff769999","creditorAddress":"0x6a362e5cee1cf5a5408ff1e12b0bc546618dffcb","signature":"0x457b0db63b83199f305ef29ba2d7678820806d98abbe3f6aafe015957ecfc5892368b4432869830456c335ade4f561603499d0216cda3af7b6b6cadf6f273c101b"}
```

#### Response:

- Status code 204
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

-

```javascript

```

-

```javascript

```
