# lndr-backend

## LNDR Server

Web service API

## POST /borrow

#### Authentication



Clients must supply the following data


#### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (): `application/json;charset=utf-8`

```javascript
{"creditor":"0x11edd217a875063583dd1b638d16810c5d34d54b","debtor":"0x6a362e5cee1cf5a5408ff1e12b0bc546618dffcb","amount":69,"memo":"test memo","signature":"0x457b0db63b83199f305ef29ba2d7678820806d98abbe3f6aafe015957ecfc5892368b4432869830456c335ade4f561603499d0216cda3af7b6b6cadf6f273c101b"}
```

- Example (): `application/json`

```javascript
{"creditor":"0x11edd217a875063583dd1b638d16810c5d34d54b","debtor":"0x6a362e5cee1cf5a5408ff1e12b0bc546618dffcb","amount":69,"memo":"test memo","signature":"0x457b0db63b83199f305ef29ba2d7678820806d98abbe3f6aafe015957ecfc5892368b4432869830456c335ade4f561603499d0216cda3af7b6b6cadf6f273c101b"}
```

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

-

```javascript
{"hash":"0x4358c649de5746c91673378dd4c40a78feda715166913e09ded45343ff76841c","nonce":1}
```

-

```javascript
{"hash":"0x4358c649de5746c91673378dd4c40a78feda715166913e09ded45343ff76841c","nonce":1}
```

## GET /docs

#### Authentication



Clients must supply the following data


#### Response:

- Status code 200
- Headers: []

- No response body

## POST /lend

#### Authentication



Clients must supply the following data


#### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- Example (): `application/json;charset=utf-8`

```javascript
{"creditor":"0x11edd217a875063583dd1b638d16810c5d34d54b","debtor":"0x6a362e5cee1cf5a5408ff1e12b0bc546618dffcb","amount":69,"memo":"test memo","signature":"0x457b0db63b83199f305ef29ba2d7678820806d98abbe3f6aafe015957ecfc5892368b4432869830456c335ade4f561603499d0216cda3af7b6b6cadf6f273c101b"}
```

- Example (): `application/json`

```javascript
{"creditor":"0x11edd217a875063583dd1b638d16810c5d34d54b","debtor":"0x6a362e5cee1cf5a5408ff1e12b0bc546618dffcb","amount":69,"memo":"test memo","signature":"0x457b0db63b83199f305ef29ba2d7678820806d98abbe3f6aafe015957ecfc5892368b4432869830456c335ade4f561603499d0216cda3af7b6b6cadf6f273c101b"}
```

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

-

```javascript
{"hash":"0x4358c649de5746c91673378dd4c40a78feda715166913e09ded45343ff76841c","nonce":1}
```

-

```javascript
{"hash":"0x4358c649de5746c91673378dd4c40a78feda715166913e09ded45343ff76841c","nonce":1}
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
19
```

-

```javascript
19
```

## GET /pending

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
[{"creditRecord":{"creditor":"0x11edd217a875063583dd1b638d16810c5d34d54b","debtor":"0x6a362e5cee1cf5a5408ff1e12b0bc546618dffcb","amount":69,"memo":"test memo","signature":"0x457b0db63b83199f305ef29ba2d7678820806d98abbe3f6aafe015957ecfc5892368b4432869830456c335ade4f561603499d0216cda3af7b6b6cadf6f273c101b"},"submitter":"0x11edd217a875063583dd1b638d16810c5d34d54b"}]
```

-

```javascript
[{"creditRecord":{"creditor":"0x11edd217a875063583dd1b638d16810c5d34d54b","debtor":"0x6a362e5cee1cf5a5408ff1e12b0bc546618dffcb","amount":69,"memo":"test memo","signature":"0x457b0db63b83199f305ef29ba2d7678820806d98abbe3f6aafe015957ecfc5892368b4432869830456c335ade4f561603499d0216cda3af7b6b6cadf6f273c101b"},"submitter":"0x11edd217a875063583dd1b638d16810c5d34d54b"}]
```

-

```javascript
[{"creditRecord":{"creditor":"0x11edd217a875063583dd1b638d16810c5d34d54b","debtor":"0x6a362e5cee1cf5a5408ff1e12b0bc546618dffcb","amount":69,"memo":"test memo","signature":"0x457b0db63b83199f305ef29ba2d7678820806d98abbe3f6aafe015957ecfc5892368b4432869830456c335ade4f561603499d0216cda3af7b6b6cadf6f273c101b"},"submitter":"0x11edd217a875063583dd1b638d16810c5d34d54b"},{"creditRecord":{"creditor":"0x11edd217a875063583dd1b638d16810c5d34d54b","debtor":"0x6a362e5cee1cf5a5408ff1e12b0bc546618dffcb","amount":69,"memo":"test memo","signature":"0x457b0db63b83199f305ef29ba2d7678820806d98abbe3f6aafe015957ecfc5892368b4432869830456c335ade4f561603499d0216cda3af7b6b6cadf6f273c101b"},"submitter":"0x11edd217a875063583dd1b638d16810c5d34d54b"}]
```

## GET /transactions

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
[{"ucac":"0xd5ec73eac35fc9dd6c3f440bce314779fed09f60","creditor":"0x11edd217a875063583dd1b638d16810c5d34d54b","debtor":"0x6a362e5cee1cf5a5408ff1e12b0bc546618dffcb","amount":69,"memo":"simple memo"}]
```

-

```javascript
[{"ucac":"0xd5ec73eac35fc9dd6c3f440bce314779fed09f60","creditor":"0x11edd217a875063583dd1b638d16810c5d34d54b","debtor":"0x6a362e5cee1cf5a5408ff1e12b0bc546618dffcb","amount":69,"memo":"simple memo"}]
```

-

```javascript
[{"ucac":"0xd5ec73eac35fc9dd6c3f440bce314779fed09f60","creditor":"0x11edd217a875063583dd1b638d16810c5d34d54b","debtor":"0x6a362e5cee1cf5a5408ff1e12b0bc546618dffcb","amount":69,"memo":"simple memo"},{"ucac":"0xd5ec73eac35fc9dd6c3f440bce314779fed09f60","creditor":"0x11edd217a875063583dd1b638d16810c5d34d54b","debtor":"0x6a362e5cee1cf5a5408ff1e12b0bc546618dffcb","amount":69,"memo":"simple memo"}]
```

## TODO

- logging
- better error handling
    + validate all input data
- configuration file to eliminate hard-coded values
- friends lists

- between tx submission to blockchain & inclusion in a block,
  users can create a pending tx that will never be deleted
    + this logic will change soon, so don't worry about this now,
      though similar problems will present themselves in the future
