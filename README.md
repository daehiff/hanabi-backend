[![Build Status](https://travis-ci.org/daehiff/hanabi-backend.svg?branch=master)](https://travis-ci.org/daehiff/hanabi-backend)

# Hanabi Backend

This is a Backend for playing Hanabi written in Haskell.

# Setup

## Local MongoDB
You need a local mongoDB instance. Docker (install MacOS: https://docs.docker.com/docker-for-mac/install/) can help to keep your environment clean:

To pull the MongoDB Container, run:
```bash
docker pull mongo:4.0.4
```
To run it:
```bash
docker run -d -p 27017-27019:27017-27019 mongo:4.0.4
```
In case you are using vsc, there is a fancy docker extension which allows you to restart and manage your containers
(https://marketplace.visualstudio.com/items?itemName=ms-azuretools.vscode-docker)

Otherwise you can restart/start your container by: 
```bash
docker restart $(docker ps -a | grep mongo:4.0.4 | cut -d ' ' -f 1)
```

(A nice graphical client for MongoDB is Robo3T)
## Project Setup

As global dependency's you need: make (should be preinstalled), ghcid and stack (should be preinstalled).

For ghcid, run:
```bash
cd ~ && stack install ghcid # Note you will be in ~after execution
```


To launch the project, you can simply run: 
```bash
make run
```

To run the unit tests, you can run:
```bash
make ptest
```

To acesss a project wide ghci, you can run:

```bash
make ghci
```

## Documentation

For creating documentation apidoc.js is used (https://apidocjs.com/).

Install it in case you want to create documentation.

To create documentation, use `make doc`

## ENVIROMENT

The backend needs to be configured, by a .env file before running create a file `.env`.
An example for a configuration using your local environment is: 

```bash
export DB_ADDR="localhost"
export DB_NAME="dev_test"
export DB_USE_REPLICA="false"
export DB_USER=""
export DB_PW=""
export PORT="8080"
"

```
## Hints

Since we are using ghcid in this project, you can test code inline, by adding the following comment:
```haskell
-- $> <code to execute>
```