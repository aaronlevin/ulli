infinisink
==========

The idea behind **infinisink** is to have a sink to send simple data to. The sink can be accessed in a variety of ways:

- email
- twitter
- REST Api
- SMS
- facebook (mention)
- bookmarking

## Build Instructions

    cabal sandbox init
    cabal configure
    cabal install
    cabal build
    cabal run

## Medium endpoints

1. **Twitter**: `@ulli` or `@infinisink`
2. **Facebook**: mention `ulli` or `infinisink`
3. **Rest API**: `https://api.ulli.co/1/infinisink`
4. **SMS**: `1234567`
5. **Bookmarking**: (chrome, firefox, and IE app)

## Payload

The payload of each event will be:

    {
        "sink": "ulli",
        "medium": "twitter",
        "msg": "empty inbox",
        "payload": "(optional) i meant the email inbox",
        "user": "aaronmblevin"
    }

### Medium Strings

1. **Twitter**: `twitter`
2. **Facebook**: `facebook`
3. **Email**: `email`
4. **SMS**: `sms`
5. **REST Api**: `rest`
6. **Bookmarking**: `bookmark`

## REST Api

- `POST /:version/sink`
- `POST /:version/sink/:sink/:medium/:user`
