# Hanabi Bckend

This is a Backend for playing Hanabi written in Haskell.


# Setup

## Local MongoDB
You need a local mongoDB instance. Docker (install MacOS: https://docs.docker.com/docker-for-mac/install/) can help to keep your environment clean:

To pull the MongoDB Container, run:
```bash
docker pull mongo:4.0.4
```
To run it:
```
docker run -d -p 27017-27019:27017-27019 -n mongodb mongo:4.0.4
```bash
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

## ENVIROMENT

The backend needs to be configured, by a .env file before running create a file `.env`:

```bash
export DB_ADDR="127.0.0.1"
export DB_NAME="test"
export DB_PW=""
export DB_UNAME=""
```
## Hints

Since we are using ghcid in this project, you can test code inline, by adding the following comment:
```haskell
-- $> <code to execute>
```