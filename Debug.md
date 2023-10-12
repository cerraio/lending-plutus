# Debugging Plutus
https://gist.github.com/bwbush/623543faa27c75b9ae0c8c302011b110

This example illustrates manually passing serialized data from a Plutus validator and then deserializing it. It uses [`Plutus.Debug`](https://github.com/input-output-hk/marlowe-cardano/blob/07538266e1a4ef747da7e0263f69eac645f6904c/marlowe/src/Plutus/Debug.hs).


## Instrument the validator

Change `traceIfFalse` to [`Plutus.Debug.debugIfFalse`](https://github.com/input-output-hk/marlowe-cardano/blob/07538266e1a4ef747da7e0263f69eac645f6904c/marlowe/src/Plutus/Debug.hs#L61) with added arguments containing the data to be passed out of the script. Here we change
```Haskell
traceIfFalse "R" $ any (checkScriptOutput addr hsh value) allOutputs
```
to
```Haskell
debugIfFalse "R" (addr, hsh, value, allOutputs) $ any (checkScriptOutput addr hsh value) allOutputs
```
, which only works if the data being serialized has `IsData` instances.


## Run the script

In this example, we tried to submit a failing script and received the following output:
```console
TxBodyScriptExecutionError [(ScriptWitnessIndexTxIn 2,ScriptErrorEvaluationFailed (CekError An error has occurred:  User error:
The provided Plutus code called 'error'.                                                                                             
Caused by: (unBData #d8799f4152d8799fd8799fd87a9f581c571dc6f3488905883a346746540f94254be3b5151a3845fc83fc1c9effd87a80ffd87a80a140a1401a05f5e1009fd8799fd8799fd8799f581ce1ce601c08286179ae7ae6ef3a38262d4112cab0663a68dfb8e9edc5ffd87a80ffa140a14000d87a80ffd8799fd8799fd87a
9f581cb81ade746787c130f13f0cc28d72d19a755c49684fccc3d5abdd6f62ffd87a80ffa140a1401a002dc6c0d8799f58206d1de149d42e49c15e33104441c1298ce8b11b0eef7d48994bf452c3dea7b427ffffd8799fd8799fd8799f581c90304fe1d67fbcad6ce67e6a32d37f0d75f159701b7328bee5912dc9ffd87a80ffa240a1401a0
5f5e100581c8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338da14f54686f6d61734d6964646c65746f6e01d87a80ffd8799fd8799fd8799f581ce1ce601c08286179ae7ae6ef3a38262d4112cab0663a68dfb8e9edc5ffd87a80ffa240a1401a002dc6c0581c8bb3b343d8e404472337966a722150048c768d0a92a981
3596c5338da14c4a6f686e466c65746368657201d87a80ffffffff)) [])]                                                                        
```

## Deserialize the data

Start a cabal REPL session and import the necessary functions:
```Haskell
import Cerra.Lending.Utils.Debug
import Plutus.V2.Ledger.Api
import Prettyprinter.Extras (pretty)
```

### Raw Plutus Data

One can see the Plutus `Data` using the `recoverPlutusData` function:
```Haskell
message = "d8799f4152d8799fd8799fd87a9f581c571dc6f3488905883a346746540f94254be3b5151a3845fc83fc1c9effd87a80ffd87a80a140a1401a05f5e1009fd8799fd8799fd8799f581ce1ce601c08286179ae7ae6ef3a38262d4112cab0663a68dfb8e9edc5ffd87a80ffa140a14000d87a80ffd8799fd8799fd87a9f581cb81ade746787c130f13f0cc28d72d19a755c49684fccc3d5abdd6f62ffd87a80ffa140a1401a002dc6c0d8799f58206d1de149d42e49c15e33104441c1298ce8b11b0eef7d48994bf452c3dea7b427ffffd8799fd8799fd8799f581c90304fe1d67fbcad6ce67e6a32d37f0d75f159701b7328bee5912dc9ffd87a80ffa240a1401a05f5e100581c8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338da14f54686f6d61734d6964646c65746f6e01d87a80ffd8799fd8799fd8799f581ce1ce601c08286179ae7ae6ef3a38262d4112cab0663a68dfb8e9edc5ffd87a80ffa240a1401a002dc6c0581c8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338da14c4a6f686e466c65746368657201d87a80ffffffff"
pretty $ recoverPlutusData message
```
```console
(Right <"R",
<<<"W\GS\198\243H\137\ENQ\136:4gFT\SI\148%K\227\181\NAK\SUB8E\252\131\252\FS\158">,
<>>,
<>,
{"": {"": 100000000}},
[<<<"\225\206`\FS\b(ay\174z\230\239:8&-A\DC2\202\176f:h\223\184\233\237\197">,
<>>,
{"": {"": 0}},
<>>,
<<<"\184\SUB\222tg\135\193\&0\241?\f\194\141r\209\154u\\IhO\204\195\213\171\221ob">,
<>>,
{"": {"": 3000000}},
<"m\GS\225I\212.I\193^3\DLEDA\193)\140\232\177\ESC\SO\239}H\153K\244R\195\222\167\180'">>,
<<<"\144\&0O\225\214\DEL\188\173l\230~j2\211\DEL\ru\241Yp\ESCs(\190\229\145-\201">,
<>>,
{"": {"": 100000000},
"\139\179\179C\216\228\EOTG#7\150jr!P\EOT\140v\141\n\146\169\129\&5\150\197\&3\141": {"ThomasMiddleton": 1}},
<>>,
<<<"\225\206`\FS\b(ay\174z\230\239:8&-A\DC2\202\176f:h\223\184\233\237\197">,
<>>,
{"": {"": 3000000},
"\139\179\179C\216\228\EOTG#7\150jr!P\EOT\140v\141\n\146\169\129\&5\150\197\&3\141": {"JohnFletcher": 1}},
<>>]>>)
```

### Deserialization via `FromData`

The raw plutus data is tedious to read, so we deserialize it with `fromData`.
```Haskell
recoverFromData message :: Either String (String, (Address, Maybe DatumHash, Value, [TxOut]))
recoverFromData message :: Either String (String, (CurrencySymbol))
recoverFromData message :: Either String (String, (ValidatorHash, ValidatorHash))
recoverFromData message :: Either String (String, (ValidatorHash, ValidatorHash, TxOut))
recoverFromData message :: Either String (String, (CurrencySymbol, Value))
```
```console
Right (
  "R",
  (
    Address {addressCredential = ScriptCredential 571dc6f3488905883a346746540f94254be3b5151a3845fc83fc1c9e, addressStakingCredential = Nothing},
    Nothing,
    Value (Map [(,Map [("",100000000)])]),
    [
      TxOut {
        txOutAddress = Address {addressCredential = PubKeyCredential e1ce601c08286179ae7ae6ef3a38262d4112cab0663a68dfb8e9edc5, addressStakingCredential = Nothing},
        txOutValue = Value (Map []),
        txOutDatumHash = Nothing
      },
      TxOut {
        txOutAddress = Address {addressCredential = ScriptCredential b81ade746787c130f13f0cc28d72d19a755c49684fccc3d5abdd6f62, addressStakingCredential = Nothing},
        txOutValue = Value (Map [(,Map [("",3000000)])]),
        txOutDatumHash = Just 6d1de149d42e49c15e33104441c1298ce8b11b0eef7d48994bf452c3dea7b427
      },
      TxOut {
        txOutAddress = Address {addressCredential = PubKeyCredential 90304fe1d67fbcad6ce67e6a32d37f0d75f159701b7328bee5912dc9, addressStakingCredential = Nothing},
        txOutValue = Value (Map [(,Map [("",100000000)]),(8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d,Map [("ThomasMiddleton",1)])]),
        txOutDatumHash = Nothing
      },
      TxOut {
        txOutAddress = Address {addressCredential = PubKeyCredential e1ce601c08286179ae7ae6ef3a38262d4112cab0663a68dfb8e9edc5, addressStakingCredential = Nothing},
        txOutValue = Value (Map [(,Map [("",3000000)]),(8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d,Map [("JohnFletcher",1)])]),
        txOutDatumHash = Nothing
      }
    ]
  )
)
```
which gives a clearer view of the on-chain data.

## Possible next steps

This manual process could be automated (for Marlowe), by having `marlowe-cli` interpret the log message when it tried to submit the transaction: it would then appropriately deserialize and pretty-print it.
