Cleanup conducted at Sun Jan 28 14:56:39 EST 2018.

Our goal is to reconcile any discrepencies between the `verified_credits` table and the ethereum blockchain.

First, we'll check what entries exist in the `verified_credits` table that do no exist on the blockchain.

```
$ curl localhost/unsubmitted
[ {"ucac":"0x869a8f2c3d22be392618ed06c8f548d1d5b5aed6","creditor":"0x3a1ea286e419130d894c9fa0cf49898bc81f9a5a","debtor":"0x754952bfa2097104a07f4f347e513a1da576ac7a","amount":265,"nonce":2,"memo":"Starbucks                       "}
, {"ucac":"0x869a8f2c3d22be392618ed06c8f548d1d5b5aed6","creditor":"0x754952bfa2097104a07f4f347e513a1da576ac7a","debtor":"0x3a1ea286e419130d894c9fa0cf49898bc81f9a5a","amount":326,"nonce":2,"memo":"Uber                            "}
, {"ucac":"0x869a8f2c3d22be392618ed06c8f548d1d5b5aed6","creditor":"0x754952bfa2097104a07f4f347e513a1da576ac7a","debtor":"0x3a1ea286e419130d894c9fa0cf49898bc81f9a5a","amount":2300,"nonce":2,"memo":"breakfast huckleberry           "}
, {"ucac":"0x869a8f2c3d22be392618ed06c8f548d1d5b5aed6","creditor":"0x3a1ea286e419130d894c9fa0cf49898bc81f9a5a","debtor":"0x754952bfa2097104a07f4f347e513a1da576ac7a","amount":400,"nonce":2,"memo":"coffee                          "}
, {"ucac":"0x869a8f2c3d22be392618ed06c8f548d1d5b5aed6","creditor":"0x754952bfa2097104a07f4f347e513a1da576ac7a","debtor":"0x3a1ea286e419130d894c9fa0cf49898bc81f9a5a","amount":30000,"nonce":2,"memo":"airbnb Santa Monica Dec         "}
, {"ucac":"0x869a8f2c3d22be392618ed06c8f548d1d5b5aed6","creditor":"0x2db907998b6688d0806ae91e2d45635e36b083ed","debtor":"0xdb33699e3b8c96a2bae4568954be4445bc17d15b","amount":2500,"nonce":0,"memo":"Business                        "}
, {"ucac":"0x869a8f2c3d22be392618ed06c8f548d1d5b5aed6","creditor":"0xdb33699e3b8c96a2bae4568954be4445bc17d15b","debtor":"0x2db907998b6688d0806ae91e2d45635e36b083ed","amount":2300,"nonce":0,"memo":"suppppp                         "}
, {"ucac":"0x869a8f2c3d22be392618ed06c8f548d1d5b5aed6","creditor":"0x2db907998b6688d0806ae91e2d45635e36b083ed","debtor":"0xdb33699e3b8c96a2bae4568954be4445bc17d15b","amount":4800,"nonce":0,"memo":"Ndbdidnd                        "}
, {"ucac":"0x869a8f2c3d22be392618ed06c8f548d1d5b5aed6","creditor":"0x941b7a5ae81b1ae7fd8a5ab8d3648722aab82532","debtor":"0xe0a335a0a907a1ef7432a722472dfdc8ee9b93b0","amount":10000,"nonce":0,"memo":"thanks for the 225              "}
, {"ucac":"0x869a8f2c3d22be392618ed06c8f548d1d5b5aed6","creditor":"0x941b7a5ae81b1ae7fd8a5ab8d3648722aab82532","debtor":"0xe0a335a0a907a1ef7432a722472dfdc8ee9b93b0","amount":250,"nonce":0,"memo":"gas money                       "}
, {"ucac":"0x869a8f2c3d22be392618ed06c8f548d1d5b5aed6","creditor":"0x941b7a5ae81b1ae7fd8a5ab8d3648722aab82532","debtor":"0xe0a335a0a907a1ef7432a722472dfdc8ee9b93b0","amount":2000,"nonce":0,"memo":"lost bet                        "}
, {"ucac":"0x869a8f2c3d22be392618ed06c8f548d1d5b5aed6","creditor":"0x5bd1e17ca43589adc011e63304283ba7c8b5bee4","debtor":"0x1b5fec5060e51886184d30b3d211d50836087b83","amount":100,"nonce":1,"memo":"fries                           "}
, {"ucac":"0x869a8f2c3d22be392618ed06c8f548d1d5b5aed6","creditor":"0x291cd9499fbcae5e3366902ebed395d14a10054a","debtor":"0x4a67f308939573b62a777b05867d46f72b0f7612","amount":500,"nonce":0,"memo":"mcD                             "}
]
```

I `delete from verified_credits where debtor = 'e0a335a0a907a1ef7432a722472dfdc8ee9b93b0';` since those records all have nonces equal to 0, which would be impossible to submit successfully to the CP contract.

```
{"ucac":"0x869a8f2c3d22be392618ed06c8f548d1d5b5aed6","creditor":"0x941b7a5ae81b1ae7fd8a5ab8d3648722aab82532","debtor":"0xe0a335a0a907a1ef7432a722472dfdc8ee9b93b0","amount":10000,"nonce":0,"memo":"thanks for the 225              "}
{"ucac":"0x869a8f2c3d22be392618ed06c8f548d1d5b5aed6","creditor":"0x941b7a5ae81b1ae7fd8a5ab8d3648722aab82532","debtor":"0xe0a335a0a907a1ef7432a722472dfdc8ee9b93b0","amount":250,"nonce":0,"memo":"gas money                       "}
{"ucac":"0x869a8f2c3d22be392618ed06c8f548d1d5b5aed6","creditor":"0x941b7a5ae81b1ae7fd8a5ab8d3648722aab82532","debtor":"0xe0a335a0a907a1ef7432a722472dfdc8ee9b93b0","amount":2000,"nonce":0,"memo":"lost bet                        "}
```

If we look in the `verified_credits` table, we'll also discover that between this same debtor and creditor, there already exists a credit with nonce 0, so none of the above three records can be vaild. I don't know how this error occurred, but this is the only course of action we can now take.

```
                               hash                               | nonce |                 creditor                 |                  debtor                  | amount |               memo               |                                                         creditor_signature                                                         |                                                          debtor_signature                                                          
------------------------------------------------------------------+-------+------------------------------------------+------------------------------------------+--------+----------------------------------+------------------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------------------
 07640e5214f45c74956d66ae0d3373ddc42d6d06b2bb28df46268fd8ac379354 |     0 | e0a335a0a907a1ef7432a722472dfdc8ee9b93b0 | 941b7a5ae81b1ae7fd8a5ab8d3648722aab82532 |  22500 | thanks for the ps4               | 20e89f10f4ccd13774700917f24f1b7eb902979a3ab190b51c74e8d188b86b0656cef9af4902ae0e9c88867a881a2f6f9b6c9ffaef2f8b20612d73ff533070801c | eccd81f1ad9a2686ebe28212825b06f48ff91ca741f5b61d3a6b569caf0622c7081eeba4b16b34e408d8393e91a75c5a87ab7b3b522d53abb2d5cbb34614b1591c
```

--------------

We continue trying to reconcile `verified_credits` and the blockchain.

```
$ curl localhost/unsubmitted
[{"ucac":"0x869a8f2c3d22be392618ed06c8f548d1d5b5aed6","creditor":"0x3a1ea286e419130d894c9fa0cf49898bc81f9a5a","debtor":"0x754952bfa2097104a07f4f347e513a1da576ac7a","amount":265,"nonce":2,"memo":"Starbucks                       "}
, {"ucac":"0x869a8f2c3d22be392618ed06c8f548d1d5b5aed6","creditor":"0x754952bfa2097104a07f4f347e513a1da576ac7a","debtor":"0x3a1ea286e419130d894c9fa0cf49898bc81f9a5a","amount":326,"nonce":2,"memo":"Uber                            "}
, {"ucac":"0x869a8f2c3d22be392618ed06c8f548d1d5b5aed6","creditor":"0x754952bfa2097104a07f4f347e513a1da576ac7a","debtor":"0x3a1ea286e419130d894c9fa0cf49898bc81f9a5a","amount":2300,"nonce":2,"memo":"breakfast huckleberry           "}
, {"ucac":"0x869a8f2c3d22be392618ed06c8f548d1d5b5aed6","creditor":"0x3a1ea286e419130d894c9fa0cf49898bc81f9a5a","debtor":"0x754952bfa2097104a07f4f347e513a1da576ac7a","amount":400,"nonce":2,"memo":"coffee                          "}
, {"ucac":"0x869a8f2c3d22be392618ed06c8f548d1d5b5aed6","creditor":"0x754952bfa2097104a07f4f347e513a1da576ac7a","debtor":"0x3a1ea286e419130d894c9fa0cf49898bc81f9a5a","amount":30000,"nonce":2,"memo":"airbnb Santa Monica Dec         "}
, {"ucac":"0x869a8f2c3d22be392618ed06c8f548d1d5b5aed6","creditor":"0x2db907998b6688d0806ae91e2d45635e36b083ed","debtor":"0xdb33699e3b8c96a2bae4568954be4445bc17d15b","amount":2500,"nonce":0,"memo":"Business                        "}
, {"ucac":"0x869a8f2c3d22be392618ed06c8f548d1d5b5aed6","creditor":"0xdb33699e3b8c96a2bae4568954be4445bc17d15b","debtor":"0x2db907998b6688d0806ae91e2d45635e36b083ed","amount":2300,"nonce":0,"memo":"suppppp                         "}
, {"ucac":"0x869a8f2c3d22be392618ed06c8f548d1d5b5aed6","creditor":"0x2db907998b6688d0806ae91e2d45635e36b083ed","debtor":"0xdb33699e3b8c96a2bae4568954be4445bc17d15b","amount":4800,"nonce":0,"memo":"Ndbdidnd                        "}
, {"ucac":"0x869a8f2c3d22be392618ed06c8f548d1d5b5aed6","creditor":"0x5bd1e17ca43589adc011e63304283ba7c8b5bee4","debtor":"0x1b5fec5060e51886184d30b3d211d50836087b83","amount":100,"nonce":1,"memo":"fries                           "}
, {"ucac":"0x869a8f2c3d22be392618ed06c8f548d1d5b5aed6","creditor":"0x291cd9499fbcae5e3366902ebed395d14a10054a","debtor":"0x4a67f308939573b62a777b05867d46f72b0f7612","amount":500,"nonce":0,"memo":"mcD                             "}
]
```

Solved some more nonce issues in the credits between 0x3a1ea286e419130d894c9fa0cf49898bc81f9a5a and 0x754952bfa2097104a07f4f347e513a1da576ac7a.

-----

```
[
  {
    "ucac": "0x869a8f2c3d22be392618ed06c8f548d1d5b5aed6",
    "creditor": "0x3a1ea286e419130d894c9fa0cf49898bc81f9a5a",
    "debtor": "0x754952bfa2097104a07f4f347e513a1da576ac7a",
    "amount": 400,
    "nonce": 2,
    "memo": "coffee                          "
  },
  {
    "ucac": "0x869a8f2c3d22be392618ed06c8f548d1d5b5aed6",
    "creditor": "0x2db907998b6688d0806ae91e2d45635e36b083ed",
    "debtor": "0xdb33699e3b8c96a2bae4568954be4445bc17d15b",
    "amount": 2500,
    "nonce": 0,
    "memo": "Business                        "
  },
  {
    "ucac": "0x869a8f2c3d22be392618ed06c8f548d1d5b5aed6",
    "creditor": "0xdb33699e3b8c96a2bae4568954be4445bc17d15b",
    "debtor": "0x2db907998b6688d0806ae91e2d45635e36b083ed",
    "amount": 2300,
    "nonce": 0,
    "memo": "suppppp                         "
  },
  {
    "ucac": "0x869a8f2c3d22be392618ed06c8f548d1d5b5aed6",
    "creditor": "0x2db907998b6688d0806ae91e2d45635e36b083ed",
    "debtor": "0xdb33699e3b8c96a2bae4568954be4445bc17d15b",
    "amount": 4800,
    "nonce": 0,
    "memo": "Ndbdidnd                        "
  },
  {
    "ucac": "0x869a8f2c3d22be392618ed06c8f548d1d5b5aed6",
    "creditor": "0x5bd1e17ca43589adc011e63304283ba7c8b5bee4",
    "debtor": "0x1b5fec5060e51886184d30b3d211d50836087b83",
    "amount": 100,
    "nonce": 1,
    "memo": "fries                           "
  },
  {
    "ucac": "0x869a8f2c3d22be392618ed06c8f548d1d5b5aed6",
    "creditor": "0x291cd9499fbcae5e3366902ebed395d14a10054a",
    "debtor": "0x4a67f308939573b62a777b05867d46f72b0f7612",
    "amount": 500,
    "nonce": 0,
    "memo": "mcD                             "
  },
  {
    "ucac": "0x869a8f2c3d22be392618ed06c8f548d1d5b5aed6",
    "creditor": "0x1b5fec5060e51886184d30b3d211d50836087b83",
    "debtor": "0x5bd1e17ca43589adc011e63304283ba7c8b5bee4",
    "amount": 100,
    "nonce": 2,
    "memo": "Settling up for $1              "
  }
]
```

Resubmitting credit with "mcD" memo: `curl -XPOST 34.238.20.130/resubmit/6b98af8c14cae991bb54a4be343ae8dd73bdee114c3f45d45445b2270ff86234`

Resubmitting credit with "fries" memo: `curl -XPOST 34.238.20.130/resubmit/7733d461c04848cac04c63341e661fbb7f5d9986ae5bdc37888e26fcee2f1de0`.

Failed to resubmit credits with "Nbd..." and "supppp" because of memo issue ("Business Card"-memo'd credit also has nonce 0).

--------------------

```
[
  {
    "ucac": "0x869a8f2c3d22be392618ed06c8f548d1d5b5aed6",
    "creditor": "0x3a1ea286e419130d894c9fa0cf49898bc81f9a5a",
    "debtor": "0x754952bfa2097104a07f4f347e513a1da576ac7a",
    "amount": 400,
    "nonce": 2,
    "memo": "coffee                          "
  },
  {
    "ucac": "0x869a8f2c3d22be392618ed06c8f548d1d5b5aed6",
    "creditor": "0x2db907998b6688d0806ae91e2d45635e36b083ed",
    "debtor": "0xdb33699e3b8c96a2bae4568954be4445bc17d15b",
    "amount": 2500,
    "nonce": 0,
    "memo": "Business                        "
  }
]
```

Both of these also have nonce issues....

All done!
