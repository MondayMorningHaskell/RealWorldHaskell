# RealWorldHaskell

## Requirements

### Postgresql

Building this code requires that you have Postgres installed on your system. If you run `stack build` and see the following error message, this indicates that you do not have Postgres:

```bash
>> stack build
setup: The program 'pg_config' is required but it could not be found
```

On Linux, you'll want at least the following packages:

```bash
>> sudo apt install postgresql postgresql-contrib libpq-dev
```

On Windows and MacOS, you should be able to use the [downloads here](https://postgresql.org/download).
