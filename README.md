TransmartUploader
=================

A R package that interfaces the **tMdataloader** (https://github.com/ThomsonReuters-LSPS/tMDataLoader)
ETL tool to upload data into **transmart**.
Specifically developed for the www.precisesads.eu project.

The current version of **tMdataloader** is 1.2.4-116.

The package now also makes use of
https://github.com/Clarivate-LSPS/tMDataLoader-samples for testing purposes.

## Usage

### documentation

`make doc`  will generate the package reference manual.
`make dev.pdf` will also add the internal functions.


## Development

Most of the tests require a Transmart test DB.
The easiest way to set-up one is to use the dockerized transmart
https://github.com/dennyverbeeck/transmart-docker.

### run the tests (no DB)

`make tests`

### run the tests on the Test DB instance

Define the TRANSMART_DB env var and make it point to your test instance:
TRANSMART_DB=host@port, e.g TRANSMART_DB=localhost@5432
If not defined the corresponding tests will be skipped.

If you run your tests also in a docker, make sure you use your computer hostname
as the DB host, and that you set the port binding to listen to all interfaces
in docker-compose.yml, e.g.:
```
   tmdb:
    image: dennyverbeeck/transmart-db:etriks-v4.0
    restart: unless-stopped
    ports:
      - "5432:5432"
 ```


It is automated via the Makefile:
```
make tests DB=localhost@5432

or in case of localhost@5432:

make tests-on-local-db

```





