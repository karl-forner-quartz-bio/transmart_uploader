TransmartUploader
=================

A R package that interfaces the **tMdataloader** (https://github.com/ThomsonReuters-LSPS/tMDataLoader)
ETL tool to upload data into **transmart**.
Specifically developed for the www.precisesads.eu project.

The current version of **tMdataloader** is 1.2.4-116.

## Usage

TODO

## Development

Most of the steps require a Transmart test DB.
The easiest way to set-up one is to use the dockerized transmart
https://github.com/dennyverbeeck/transmart-docker.

### run the tests on the Test DB instance

Define the TRANSMART_DB env var and make it point to your test instance:
TRANSMART_DB=host@port, e.g TRANSMART_DB=localhost@5432
If not defined the corresponding tests will be skipped.


It is automated via the Makefile:
```
make tests DB=localhost@5432

or in case of localhost@5432:

make tests-on-local-db

```





